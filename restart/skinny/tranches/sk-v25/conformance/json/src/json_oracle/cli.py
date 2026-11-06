"""Command-line observations for the JSON assay."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import sys

from .parser import Budgets, ConfigurationError, OracleError, _observe, parse
from .projection import (
    ProjectionError,
    ScalarValidityError,
    _canonicalize,
    _integrity,
    _require_unicode_scalars,
)

_MAX_CLI_BUDGET = 9_223_372_036_854_775_806


class _ArgumentParser(argparse.ArgumentParser):
    def error(self, message: str) -> None:
        raise ConfigurationError("arguments", message)


def _budget(field: str, value: int | str) -> int:
    if type(value) is int:
        return value
    if (
        not value
        or any(character not in "0123456789" for character in value)
        or len(value) > 19
        or (len(value) == 19 and value > str(_MAX_CLI_BUDGET))
    ):
        raise ConfigurationError(field, value)
    return int(value)


def _read(path: Path, limit: int) -> bytes:
    source = bytearray()
    with path.open("rb") as stream:
        while len(source) <= limit:
            chunk = stream.read(min(65_536, limit + 1 - len(source)))
            if not chunk:
                break
            source.extend(chunk)
    if len(source) > limit:
        raise OracleError(
            "resource",
            "input_byte_budget",
            limit,
            {
                "input_bytes": limit,
                "nodes": 0,
                "aggregate_entries": 0,
                "decoded_utf16_units": 0,
                "max_depth": 0,
            },
        )
    return bytes(source)


def main() -> int:
    parser = _ArgumentParser()
    parser.add_argument("path", type=Path)
    parser.add_argument("--jcs", action="store_true")
    parser.add_argument("--require-scalars", action="store_true")
    parser.add_argument("--max-input-bytes", default=Budgets.max_input_bytes)
    parser.add_argument("--max-depth", default=Budgets.max_depth)
    parser.add_argument("--max-nodes", default=Budgets.max_nodes)
    parser.add_argument(
        "--max-aggregate-entries", default=Budgets.max_aggregate_entries
    )
    parser.add_argument(
        "--max-decoded-utf16-units", default=Budgets.max_decoded_utf16_units
    )
    try:
        arguments = parser.parse_args()
        budgets = Budgets(
            _budget("max_input_bytes", arguments.max_input_bytes),
            _budget("max_depth", arguments.max_depth),
            _budget("max_nodes", arguments.max_nodes),
            _budget("max_aggregate_entries", arguments.max_aggregate_entries),
            _budget("max_decoded_utf16_units", arguments.max_decoded_utf16_units),
        )
        document = _integrity(
            parse(_read(arguments.path, budgets.max_input_bytes), budgets)
        )
        if arguments.require_scalars:
            _require_unicode_scalars(document)
        jcs = None
        if arguments.jcs:
            canonical = _canonicalize(document)
            jcs = {
                "status": "accepted",
                "utf8": canonical.decode("utf-8"),
                "sha256": hashlib.sha256(canonical).hexdigest(),
            }
        output = _observe(document, jcs=jcs)
        accepted = True
    except (ConfigurationError, OracleError, ProjectionError, ScalarValidityError) as error:
        result = error.observation()
        output = json.dumps(
            result, ensure_ascii=True, sort_keys=True, separators=(",", ":")
        ).encode("ascii")
        accepted = False
    except OSError:
        result = {"status": "rejected", "category": "io", "code": "read_failed"}
        output = json.dumps(
            result, ensure_ascii=True, sort_keys=True, separators=(",", ":")
        ).encode("ascii")
        accepted = False
    sys.stdout.buffer.write(output)
    sys.stdout.buffer.write(b"\n")
    return 0 if accepted else 1


if __name__ == "__main__":
    raise SystemExit(main())
