#!/usr/bin/env python3
"""Regenerate every deterministic epoch-5 artifact into one empty root."""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
import tempfile
from pathlib import Path

import assay
from common import GateError, ROOT, encode, enforce_offline, load, load_contract, sha256, store, verify_runtime


ORDER = (
    "genesis.json",
    "denominator.json",
    "wpt-prefix-blobs.json",
    "candidate-absence.json",
    "runtime.json",
    "observations.json",
    "mutation-results.json",
    "generation-receipt.json",
)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--check-against", type=Path)
    options = parser.parse_args()
    enforce_offline()
    if options.output.exists() and (not options.output.is_dir() or any(options.output.iterdir())):
        print(json.dumps({"code": "OUTPUT_NOT_EMPTY"}, sort_keys=True), file=sys.stderr)
        return 2
    options.output.mkdir(parents=True, exist_ok=True)
    try:
        inputs, values = assay.build_base()
        runtime_contract = load_contract()["python"]
        with tempfile.TemporaryDirectory(prefix="css-epoch5-denominator-") as temporary:
            probe = Path(temporary) / "denominator.json"
            command = [sys.executable, "-I", "-B", "-X", "utf8", "-X", "warn_default_encoding", str(ROOT / "src" / "launcher.py"), "denominator", "--output", str(probe)]
            rederived = subprocess.run(command, capture_output=True, env=runtime_contract["environment"])
            if rederived.returncode:
                raise GateError("DENOMINATOR_REDERIVATION_FAILED", "isolated denominator child failed", {"stderr": rederived.stderr.decode("utf-8", errors="replace")})
            inputs["_independent_denominator"] = load(probe)
        assay.validate_denominator(values["denominator.json"], inputs)
        values["runtime.json"] = verify_runtime()
        artifact_bytes = {name: encode(value) for name, value in values.items()}
        values["observations.json"] = assay.observations(artifact_bytes, inputs)
        resolved = {
            "inputs/contract.json": (ROOT / "inputs" / "contract.json").read_bytes(),
            "epoch4/inputs/manifest.json": (ROOT.parent / "epoch-4" / "inputs" / "manifest.json").read_bytes(),
            **artifact_bytes,
        }
        assay.validate_document(values["observations.json"], resolved, values, inputs)
        sys.path.insert(0, str(ROOT / "tests"))
        from mutations import run_all

        values["mutation-results.json"] = run_all(values["observations.json"], values, artifact_bytes, inputs)
        for name in ORDER[:-1]:
            store(options.output / name, values[name])
        artifact_receipts = [
            {"path": name, "bytes": (options.output / name).stat().st_size, "sha256": sha256((options.output / name).read_bytes())}
            for name in ORDER[:-1]
        ]
        values["generation-receipt.json"] = {
            "schema_version": 1,
            "status": "BYTE_REPLAY_VALID_LOCAL_AND_DOWNSTREAM_RED",
            "offline": True,
            "artifact_count": len(artifact_receipts),
            "artifacts": artifact_receipts,
            "unit_count": values["denominator.json"]["unit_count"],
            "unit_digest": values["denominator.json"]["unit_digest"],
            "denominator_rederivation": "FRESH_ISOLATED_CHILD_SAME_SEALED_EXTRACTOR_NOT_AN_ALGORITHMIC_ORACLE",
            "path_blob_count": values["wpt-prefix-blobs.json"]["path_blob_count"],
            "counterexample_count": values["mutation-results.json"]["executed_count"],
            "candidate_count": 0,
            "complete_candidate_count": 0,
            "credit": "NONE",
        }
        store(options.output / "generation-receipt.json", values["generation-receipt.json"])
    except (GateError, OSError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 2
    if options.check_against:
        mismatches = [name for name in ORDER if not (options.check_against / name).is_file() or (options.check_against / name).read_bytes() != (options.output / name).read_bytes()]
        if mismatches:
            print(json.dumps({"code": "STORED_ARTIFACT_DRIFT", "files": mismatches}, sort_keys=True), file=sys.stderr)
            return 1
    print(json.dumps({"status": values["generation-receipt.json"]["status"], "artifacts": len(ORDER), "units": values["denominator.json"]["unit_count"], "counterexamples": values["mutation-results.json"]["executed_count"]}, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
