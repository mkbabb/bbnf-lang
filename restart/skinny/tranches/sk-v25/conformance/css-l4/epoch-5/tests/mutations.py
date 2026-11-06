"""External historical counterexamples executed through owning epoch-5 gates."""

from __future__ import annotations

import copy
import json
import tempfile
from pathlib import Path
from typing import Any

from assay import candidate_absence, validate_denominator, validate_document, validate_tree_catalog, validate_wpt
from common import GateError, encode, fail, load, sha256
from parsers import UnsupportedDialect, parse_css_vds, parse_lex, parse_webidl, parse_yacc, project_markup
from source import scan_markup


def _resolved(document: dict[str, Any], artifact_bytes: dict[str, bytes]) -> dict[str, bytes]:
    fixed = {
        "inputs/contract.json": (Path(__file__).resolve().parents[1] / "inputs" / "contract.json").read_bytes(),
        "epoch4/inputs/manifest.json": (Path(__file__).resolve().parents[2] / "epoch-4" / "inputs" / "manifest.json").read_bytes(),
    }
    return {**fixed, **artifact_bytes}


def _document_mutation(name: str, document: dict[str, Any]) -> None:
    if name == "drop_positive_evidence":
        document["claims"]["input_bytes"]["evidence"] = []
    elif name == "uppercase_evidence_digest":
        document["evidence"][0]["sha256"] = document["evidence"][0]["sha256"].upper()
    elif name == "launder_blocker_counts":
        document["claims"]["local_semantics"]["blocked_units"] = 0
    else:
        fail("MUTATION_UNKNOWN", "document mutation is unknown", mutation=name)


def execute(row: dict[str, Any], document: dict[str, Any], values: dict[str, Any], artifact_bytes: dict[str, bytes], inputs: dict[str, Any]) -> None:
    gate = row["gate"]
    payload = row["payload"]
    if gate == "document":
        mutated = copy.deepcopy(document)
        _document_mutation(payload["mutation"], mutated)
        validate_document(mutated, _resolved(mutated, artifact_bytes), values, inputs)
    elif gate == "span-map":
        raw = payload["source"].encode()
        projection = project_markup(raw)
        if projection.text != payload["text"]:
            fail("SPAN_TEXT_CHANGED", "source-preserving projection changed CSS dialect text")
        fragments = projection.fragments(0, len(projection.text))
        digest = sha256(b"".join(raw[left:right] for left, right in fragments))
        if digest != payload["claimed_sha256"]:
            fail("SPAN_MAP_MISMATCH", "claimed exact source span digest differs")
    elif gate == "css-vds":
        parse_css_vds(payload["source"])
    elif gate == "yacc":
        parse_yacc(payload["source"])
    elif gate == "lex":
        parse_lex(payload["source"])
    elif gate == "webidl":
        parse_webidl(payload["source"])
    elif gate == "denominator":
        mutated = copy.deepcopy(values["denominator.json"])
        if payload["mutation"] == "coordinated_unit_deletion":
            removed = mutated["units"].pop()
            mutated["unit_count"] -= 1
            mutated["family_counts"][removed["family"]] -= 1
            if mutated["family_counts"][removed["family"]] == 0:
                del mutated["family_counts"][removed["family"]]
            mutated["disposition_counts"][removed["disposition"]] -= 1
            if mutated["disposition_counts"][removed["disposition"]] == 0:
                del mutated["disposition_counts"][removed["disposition"]]
            mutated["reverse_link_count"] -= len(removed["reverse_links"])
            digest_rows = [[item["id"], item["family"], item["fragments"], item["source_sha256"], item["disposition"], item["reverse_links"]] for item in mutated["units"]]
            mutated["unit_digest"] = sha256(encode(digest_rows))
        elif payload["mutation"] == "erase_one_unit_reverse_links":
            target = next(item for item in mutated["units"] if item["reverse_links"])
            mutated["reverse_link_count"] -= len(target["reverse_links"])
            target["reverse_links"] = []
        elif payload["mutation"] == "forge_unit_digest":
            mutated["unit_digest"] = "0" * 64
        validate_denominator(mutated, inputs)
    elif gate == "source-detector":
        failures = scan_markup(payload["source"].encode()).failures
        if failures:
            fail("BLOCKED_DETECTOR_FAILURE", "source detector preserved a typed failed occurrence", failures=failures)
        fail("DETECTOR_FAILED_OPEN", "malformed source disappeared")
    elif gate == "wpt":
        mutated = copy.deepcopy(values["wpt-prefix-blobs.json"])
        if payload["mutation"] == "promote_prefix_blob_to_test":
            mutated["wpt_test_count"] = 1
        elif payload["mutation"] == "classify_metadata_as_test":
            target = next(item for item in mutated["rows"] if item["classification"] == "METADATA")
            target["classification"] = "TEST"
        elif payload["mutation"] == "drop_declared_route":
            mutated["rows"].pop()
            mutated["path_blob_count"] -= 1
        elif payload["mutation"] == "duplicate_declared_route":
            mutated["rows"].append(copy.deepcopy(mutated["rows"][0]))
            mutated["path_blob_count"] += 1
        elif payload["mutation"] == "append_unknown_route":
            unknown = copy.deepcopy(mutated["rows"][0])
            unknown["path"] = "not-in-pinned-tree/unknown.html"
            mutated["rows"].append(unknown)
            mutated["path_blob_count"] += 1
        else:
            fail("MUTATION_UNKNOWN", "WPT mutation is unknown", mutation=payload["mutation"])
        validate_wpt(mutated, inputs)
    elif gate == "candidate":
        with tempfile.TemporaryDirectory(prefix="css-epoch5-candidate-") as temporary:
            root = Path(temporary)
            (root / payload["root"]).mkdir()
            candidate_absence(root)
    elif gate == "tree":
        validate_tree_catalog(payload["entries"], payload["plain_sort_root"])
    elif gate == "receipt":
        from manifest import validate_receipt

        subject = {"subject_sha256": "1" * 64, "file_count": 1, "total_bytes": 1}
        receipt = {"subject_definition": "file-manifest subject; manifest and this verification receipt excluded by declared self-reference rule", "manifest_sha256": "2" * 64, "subject_sha256": "0" * 64, "file_count": 1, "total_bytes": 1}
        validate_receipt(receipt, subject, "2" * 64)
    else:
        fail("COUNTEREXAMPLE_GATE_UNKNOWN", "counterexample gate is unknown", gate=gate)


def run_all(document: dict[str, Any], values: dict[str, Any], artifact_bytes: dict[str, bytes], inputs: dict[str, Any]) -> dict[str, Any]:
    fixture = load(Path(__file__).resolve().parents[1] / "fixtures" / "counterexamples.json")
    rows = fixture.get("counterexamples", [])
    identities = [row["id"] for row in rows]
    historical = {*(f"A-{index:02d}" for index in range(1, 11)), *(f"B-F{index:02d}" for index in range(1, 11))}
    supplemental = {"E5-WPT-DROPPED", "E5-WPT-DUPLICATED", "E5-WPT-UNKNOWN"}
    if len(rows) != 23 or len(set(identities)) != 23 or set(identities) != historical | supplemental:
        fail("COUNTEREXAMPLE_REGISTRY_INVALID", "registry must contain all twenty historical IDs and three WPT closure adversaries")
    results = []
    for row in rows:
        actual = None
        detail = None
        try:
            execute(row, document, values, artifact_bytes, inputs)
        except GateError as error:
            actual, detail = error.code, error.message
        except UnsupportedDialect as error:
            actual, detail = error.code, error.detail
        if actual != row["expected"]:
            fail("COUNTEREXAMPLE_RESULT_MISMATCH", "external adversary did not receive expected typed rejection", id=row["id"], expected=row["expected"], actual=actual)
        results.append({
            "id": row["id"],
            "gate": row["gate"],
            "expected": row["expected"],
            "actual": actual,
            "status": "REJECTED_AS_EXPECTED",
            "input_sha256": sha256((json.dumps(row, indent=2, sort_keys=True, ensure_ascii=True) + "\n").encode()),
            "detail": detail,
        })
    return {
        "schema_version": 1,
        "status": "ALL_EXTERNAL_COUNTEREXAMPLES_REJECTED",
        "registered_count": len(rows),
        "historical_count": sum(row["id"] in historical for row in results),
        "supplemental_count": sum(row["id"] in supplemental for row in results),
        "executed_count": len(results),
        "rejected_count": sum(row["status"] == "REJECTED_AS_EXPECTED" for row in results),
        "result_digest": sha256(encode(results)),
        "results": results,
    }
