#!/usr/bin/env python3
"""External future-candidate allowlist/crater contract; no candidate is fabricated here."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

from common import INPUTS, fail, governing_receipts, load


TEXT_SUFFIXES = {".rs", ".ts", ".tsx", ".js", ".mjs", ".cjs", ".json", ".toml", ".bbnf", ".py"}
FORBIDDEN_TERMS = (
    "semantic-matrix", "module-closure", "grammar.json", "matrix.json",
    "profile.json", "prelude", "bbnf_expectation", "owner table", "owner_table",
)


def capture() -> dict[str, Any]:
    contract = load(INPUTS / "future-candidate-contract.json")
    if contract.get("candidate_present") is not False:
        fail("CANDIDATE_STATE_INVALID", "epoch-3 must not fabricate a current VT candidate")
    allowed = contract.get("allowed_external_inputs")
    forbidden = contract.get("forbidden_authority_classes")
    if not isinstance(allowed, list) or not allowed or len(allowed) != len(set(allowed)):
        fail("BOUNDARY_ALLOWLIST_INVALID", "future-candidate input allowlist must be a unique nonempty list")
    if not isinstance(forbidden, list) or not forbidden:
        fail("BOUNDARY_FORBIDDEN_CLASSES_INVALID", "future-candidate forbidden authority classes are absent")
    return {
        "schema_version": 3,
        "stage": "candidate-boundary",
        "status": "NO_CANDIDATE_PRESENT",
        "observation_only": True,
        "future_enforcement": "REQUIRED_IN_EACH_FUTURE_CANDIDATE_BUILD_AND_PACKAGE_CRATER",
        "allowed_external_inputs": allowed,
        "forbidden_authority_classes": forbidden,
        "governing_documents": governing_receipts(),
        "root_blockers": [],
    }


def inspect_candidate(root: Path, contract: dict[str, Any]) -> dict[str, Any]:
    if not root.is_dir():
        fail("CANDIDATE_ROOT_MISSING", "future candidate crater root does not exist", root=str(root))
    allow = {str(Path(path)) for path in contract["allowed_external_inputs"]}
    violations = []
    scanned = 0
    for path in sorted(item for item in root.rglob("*") if item.is_file() and item.suffix.lower() in TEXT_SUFFIXES):
        scanned += 1
        try:
            text = path.read_text(errors="strict")
        except UnicodeDecodeError:
            violations.append({"code": "NON_UTF8_BUILD_INPUT", "path": path.relative_to(root).as_posix()})
            continue
        lowered = text.lower()
        for term in FORBIDDEN_TERMS:
            if term.lower() in lowered:
                violations.append({"code": "FORBIDDEN_ORACLE_AUTHORITY_IMPORT", "path": path.relative_to(root).as_posix(), "term": term})
        for line_number, line in enumerate(text.splitlines(), start=1):
            if "conformance/css-l4" not in line:
                continue
            if not any(item in line for item in allow):
                violations.append({"code": "INPUT_NOT_ALLOWLISTED", "path": path.relative_to(root).as_posix(), "line": line_number})
    return {"status": "PASS" if not violations else "FAIL", "scanned_files": scanned, "violations": violations}


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--candidate-root", type=Path)
    parser.add_argument("--contract", type=Path, default=INPUTS / "future-candidate-contract.json")
    options = parser.parse_args()
    contract = load(options.contract)
    if options.candidate_root is None:
        print(json.dumps({"status": "NO_CANDIDATE_PRESENT", "contract": str(options.contract), "future_enforcement": "REQUIRED_PER_CANDIDATE"}, sort_keys=True))
        return 0
    result = inspect_candidate(options.candidate_root, contract)
    print(json.dumps(result, sort_keys=True))
    return 0 if result["status"] == "PASS" else 2


if __name__ == "__main__":
    raise SystemExit(main())
