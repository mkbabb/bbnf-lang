#!/usr/bin/env python3
"""Verify the closed runtime before importing any epoch-5 assay subject."""

from __future__ import annotations

import hashlib
import json
import os
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parents[1]
CONTRACT = ROOT / "inputs" / "contract.json"


def early_fail(code: str, **context: object) -> "NoReturn":
    print(json.dumps({"code": code, "context": context}, sort_keys=True), file=sys.stderr)
    raise SystemExit(2)


def main() -> int:
    try:
        contract = json.loads(CONTRACT.read_text(encoding="utf-8"))["python"]
    except (OSError, json.JSONDecodeError, KeyError) as error:
        early_fail("RUNTIME_CONTRACT_UNREADABLE", detail=str(error))
    executable = pathlib.Path(sys.executable).resolve()
    if hashlib.sha256(executable.read_bytes()).hexdigest() != contract["binary_sha256"]:
        early_fail("PYTHON_BINARY_DRIFT")
    flags = {
        "dont_write_bytecode": sys.flags.dont_write_bytecode,
        "isolated": sys.flags.isolated,
        "no_user_site": sys.flags.no_user_site,
        "optimize": sys.flags.optimize,
        "safe_path": sys.flags.safe_path,
        "utf8_mode": sys.flags.utf8_mode,
        "warn_default_encoding": sys.flags.warn_default_encoding,
    }
    if flags != contract["startup_flags"]:
        early_fail("STARTUP_FLAGS_DRIFT", actual=flags)
    if dict(os.environ) != contract["environment"]:
        early_fail("ENVIRONMENT_NOT_ALLOWLISTED", keys=sorted(os.environ))
    if len(sys.argv) < 2 or sys.argv[1] not in {"denominator", "generate", "manifest", "proof", "test"}:
        early_fail("LAUNCH_TARGET_INVALID", target=sys.argv[1:] or None)
    sys.path.insert(0, str(ROOT / "src"))
    from common import verify_runtime

    verify_runtime()
    target = sys.argv[1]
    sys.argv = [sys.argv[0], *sys.argv[2:]]
    if target == "denominator":
        from denominator_probe import main as callback
    elif target == "generate":
        from generate import main as callback
    elif target == "manifest":
        from manifest import main as callback
    else:
        sys.path.insert(0, str(ROOT / "tests"))
        if target == "proof":
            from proof import main as callback
        else:
            from offline_runner import main as callback
    return callback()


if __name__ == "__main__":
    raise SystemExit(main())
