"""Source-exact semantic obligations with closed statuses and terminal VT.W1 routing."""

from __future__ import annotations

import re
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any

from common import INPUTS, ROOT, encode, fail, governing_receipts, load, require_clean, sha256
from extract import source_range


ENUMS = {
    "bbnf": {"MAPPED", "UNMAPPED"},
    "fixtures": {"MAPPED", "UNMAPPED"},
    "wpt": {"PASS", "FAIL", "UNRUN", "UNMAPPED", "NO_DIRECT_TESTS"},
    "recovery": {"OBSERVED", "NOT_APPLICABLE", "UNPROVEN"},
    "diagnostics": {"OBSERVED", "NOT_APPLICABLE", "UNPROVEN"},
    "serialization": {"OBSERVED", "NOT_APPLICABLE", "UNPROVEN"},
    "owner": {"ASSIGNED", "UNMAPPED"},
}
EVIDENCED = {
    "bbnf": {"MAPPED"},
    "fixtures": {"MAPPED"},
    "wpt": {"PASS", "FAIL", "NO_DIRECT_TESTS"},
    "recovery": {"OBSERVED", "NOT_APPLICABLE"},
    "diagnostics": {"OBSERVED", "NOT_APPLICABLE"},
    "serialization": {"OBSERVED", "NOT_APPLICABLE"},
    "owner": {"ASSIGNED"},
}
PLANES = tuple(ENUMS)
RULE = re.compile(r"(?m)^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=")
CONTROL_STATUSES = {"correctness_status": "UNPROVEN", "performance_status": "UNRUN", "selection_status": "UNRUN"}


def bbnf_rules(repository: Path) -> dict[str, list[str]]:
    result: dict[str, list[str]] = defaultdict(list)
    root = repository / "grammar" / "css" / "l4"
    for path in sorted(root.glob("*.bbnf")):
        for name in RULE.findall(path.read_text()):
            result[name].append(path.relative_to(repository).as_posix())
    return dict(result)


def semantic_state(row: dict[str, Any]) -> str:
    return "EVIDENCED" if all(row["planes"][plane] in EVIDENCED[plane] for plane in PLANES) else "OBLIGATION"


def downstream(row: dict[str, Any]) -> dict[str, Any]:
    reasons = [f"{plane}:{row['planes'][plane]}" for plane in PLANES if row["planes"][plane] not in EVIDENCED[plane]]
    return {"status": "ROUTED_VT_W1" if reasons else "OBSERVATION_COMPLETE", "reasons": reasons}


def downstream_id(decision: dict[str, Any]) -> str:
    return "downstream::" + sha256(encode(decision))


def downstream_controls() -> dict[str, Any]:
    document = load(INPUTS / "downstream-controls.json")
    wave = next(row for row in governing_receipts() if row["role"] == "governing-wave")
    if document.get("governing_wave_sha256") != wave["sha256"]:
        fail("DOWNSTREAM_CONTROL_GOVERNANCE_DRIFT", "control gate manifest is not bound to the current governing wave")
    controls = document.get("controls")
    if document.get("schema_version") != 1 or not isinstance(controls, list) or len(controls) != 2:
        fail("DOWNSTREAM_CONTROLS_INVALID", "Lightning CSS and sonic-rs control rows must be explicit")
    expected = {"Lightning CSS", "sonic-rs"}
    if {row.get("control") for row in controls} != expected:
        fail("DOWNSTREAM_CONTROL_SET_INVALID", "mandatory downstream control set differs from the governing wave")
    for row in controls:
        for field, status in CONTROL_STATUSES.items():
            if row.get(field) != status:
                fail("DOWNSTREAM_CONTROL_STATUS_INVALID", "unexecuted control gate must stay explicitly open", control=row.get("control"), field=field, status=row.get(field))
        for channel in ("stable", "head"):
            commit = row.get(channel, {}).get("commit")
            if not isinstance(commit, str) or re.fullmatch(r"[0-9a-f]{40}", commit) is None:
                fail("DOWNSTREAM_CONTROL_PIN_INVALID", "control discovery anchor lacks an exact commit", control=row.get("control"), channel=channel)
    return document


def semantic_inventory(modules: dict[str, Any]) -> list[dict[str, Any]]:
    semantic: list[dict[str, Any]] = []
    for definition in modules["definitions"]:
        semantic.append({"id": f"row::{definition['id']}", "family": "production", "name": definition["name"], "module": definition["module"], "carrier_id": definition["carrier_id"], "semantic_id": definition["id"], "source_object": definition})
    for entity in modules["entities"]:
        for ordinal, field in enumerate(entity["field_occurrences"], start=1):
            semantic.append({"id": f"row::{entity['id']}::field::{ordinal}", "family": "entity-field", "name": entity["name"], "module": entity["module"], "carrier_id": entity["carrier_id"], "semantic_id": entity["id"], "field": field["label"], "field_ordinal": ordinal, "source_object": field})
    entities = {row["id"]: row for row in modules["entities"]}
    for extension in modules["extensions"]:
        entity = entities[extension["entity_id"]]
        semantic.append({"id": f"row::{extension['id']}", "family": "extension", "name": entity["name"], "module": entity["module"], "carrier_id": entity["carrier_id"], "semantic_id": extension["id"], "decision": extension["decision"], "source_object": extension})
    for observation in modules["observations"]:
        semantic.append({"id": f"row::{observation['id']}", "family": observation["family"], "name": observation["id"], "module": observation["module"], "carrier_id": observation["carrier_id"], "semantic_id": observation["id"], "source_object": observation})
    definitions = {row["id"]: row for row in modules["definitions"]}
    for ordinal, conflict in enumerate(modules["conflicts"], start=1):
        first = definitions[conflict["definition_ids"][0]]
        semantic.append({"id": f"row::conflict::{ordinal}::{conflict['name']}", "family": "conflict", "name": conflict["name"], "module": first["module"], "carrier_id": first["carrier_id"], "semantic_id": f"conflict::{ordinal}", "decision": conflict["decision"], "source_object": first})
    return semantic


def _span_from_text(carrier: dict[str, Any], data: bytes, start: int, end: int) -> dict[str, Any]:
    span = source_range(data, carrier["source_map"], start, end)
    if span["start"] is None:
        fail("SEMANTIC_SOURCE_UNMAPPED", "semantic text range has no exact source-byte projection", carrier_id=carrier["id"], text_start=start, text_end=end)
    return span


def _exact_source(item: dict[str, Any], carrier: dict[str, Any], data: bytes) -> dict[str, Any]:
    value = item["source_object"]
    family = item["family"]
    if family == "entity-field":
        span = value["source"]
    elif family == "extension":
        span = value["source"]
    elif family == "algorithm-step":
        span = value["step"]["source"]
    elif family == "idl-declaration":
        declaration = value["declaration"]
        span = _span_from_text(carrier, data, declaration["text_start"], declaration["text_end"])
    elif family in {"expression", "railroad", "legacy-grammar"}:
        source = value["source"]
        span = _span_from_text(carrier, data, source.get("text_start", 0), source.get("text_end", len(carrier["text"])))
    else:
        source = value["source"]
        starts = [source[key] for key in ("lhs_text_start", "text_start") if source.get(key) is not None]
        ends = [source[key] for key in ("lhs_text_end", "text_end") if source.get(key) is not None]
        starts.extend(part["start"] for part in source.get("rhs_source_parts", []))
        ends.extend(part["end"] for part in source.get("rhs_source_parts", []))
        span = _span_from_text(carrier, data, min(starts), max(ends)) if starts and ends else {"start": carrier["start"], "end": carrier["end"], "sha256": carrier["raw_sha256"]}
    if span.get("start") is None or span.get("end") is None or span.get("sha256") is None:
        fail("SEMANTIC_SOURCE_INCOMPLETE", "semantic row lacks an exact source identity and span", row_id=item["id"])
    return {"spec_id": carrier["spec_id"], "path": carrier["path"], "start": span["start"], "end": span["end"], "sha256": span["sha256"], "anchor": carrier.get("anchor"), "source_form": family}


def _source_id(source: dict[str, Any]) -> str:
    return "source::" + sha256(encode(source))


def wpt_owner_identity(carrier: dict[str, Any]) -> str:
    return f"wpt::{carrier['spec_id']}::{carrier.get('anchor') or '@document'}"


def validate(document: dict[str, Any], modules: dict[str, Any] | None = None, csswg: Path | None = None) -> None:
    sources = {row.get("id"): row for row in document.get("sources", [])}
    bbnf = {row.get("id"): row for row in document.get("bbnf_expectations", [])}
    wpt = {row.get("id"): row for row in document.get("wpt_ownership", [])}
    decisions = {row.get("id"): row for row in document.get("downstream_decisions", [])}
    if len(sources) != len(document.get("sources", [])) or len(bbnf) != len(document.get("bbnf_expectations", [])) or len(wpt) != len(document.get("wpt_ownership", [])) or len(decisions) != len(document.get("downstream_decisions", [])):
        fail("MATRIX_COMPACT_ID_DUPLICATE", "compact matrix source or expectation identity is duplicated")
    if csswg is not None:
        files: dict[str, bytes] = {}
        for source in sources.values():
            path = source.get("path")
            if path not in files:
                files[path] = (csswg / path).read_bytes()
            data = files[path]
            start, end = source.get("start"), source.get("end")
            if not isinstance(start, int) or not isinstance(end, int) or sha256(data[start:end]) != source.get("sha256"):
                fail("MATRIX_SOURCE_DRIFT", "semantic matrix source does not rehash against pinned CSSWG bytes", source_id=source.get("id"))
    seen: set[str] = set()
    for row in document.get("rows", []):
        if row.get("id") in seen:
            fail("MATRIX_ROW_DUPLICATE", "semantic matrix row identity is duplicated", row_id=row.get("id"))
        seen.add(row.get("id"))
        for plane, allowed in ENUMS.items():
            status = row.get("planes", {}).get(plane)
            if status not in allowed:
                fail("MATRIX_STATUS_UNKNOWN", "matrix plane status is outside its closed enum", row_id=row.get("id"), plane=plane, status=status)
        source = sources.get(row.get("source_id"))
        expectation = bbnf.get(row.get("bbnf_expectation_id"))
        ownership = wpt.get(row.get("wpt_ownership_id"))
        if source is None or not isinstance(source.get("start"), int) or not isinstance(source.get("end"), int) or source["end"] <= source["start"] or not re.fullmatch(r"[0-9a-f]{64}", str(source.get("sha256"))):
            fail("MATRIX_SOURCE_MISSING", "matrix row points to a missing or inexact semantic source", row_id=row.get("id"))
        if expectation is None or ownership is None:
            fail("MATRIX_EXPECTATION_MISSING", "matrix row points to a missing BBNF expectation or WPT ownership decision", row_id=row.get("id"))
        if row["planes"]["bbnf"] != expectation["status"] or row["planes"]["wpt"] != ownership["status"]:
            fail("MATRIX_EXPECTATION_STATUS_DRIFT", "matrix plane status differs from its compact expectation", row_id=row.get("id"))
        owner_status = row["planes"]["owner"]
        if owner_status == "UNMAPPED" and row.get("responsible_module") is not None:
            fail("MATRIX_OWNER_FABRICATED", "unmapped owner plane must not name a responsible BBNF module", row_id=row.get("id"))
        if owner_status == "ASSIGNED" and not row.get("responsible_module"):
            fail("MATRIX_RESPONSIBILITY_MISSING", "assigned owner plane lacks a proved responsible BBNF module", row_id=row.get("id"))
        expected_downstream = downstream(row)
        decision = decisions.get(row.get("terminal_downstream_id"))
        if row.get("semantic_state") != semantic_state(row) or decision != {"id": downstream_id(expected_downstream), **expected_downstream}:
            fail("MATRIX_TERMINAL_NOT_DERIVED", "semantic/downstream terminal state differs from closed plane statuses", row_id=row.get("id"))
    if modules is not None:
        expected = Counter(row["id"] for row in semantic_inventory(modules))
        actual = Counter(row["id"] for row in document.get("rows", []))
        if expected != actual:
            fail("MATRIX_ROWS_NOT_TOTAL", "matrix rows differ from the exact normative semantic-unit inventory", expected=sum(expected.values()), actual=sum(actual.values()))


def capture(scope: dict[str, Any], extraction: dict[str, Any], modules: dict[str, Any], repository: Path, csswg: Path) -> dict[str, Any]:
    require_clean(modules, "semantic-matrix")
    carriers = {row["id"]: row for row in extraction["records"]}
    rules = bbnf_rules(repository)
    routes: dict[tuple[str, str | None], list[dict[str, Any]]] = defaultdict(list)
    for route in scope["wpt"]["routes"]:
        routes[(route["spec_id"], route.get("anchor"))].append(route)

    rows: list[dict[str, Any]] = []
    sources: dict[str, dict[str, Any]] = {}
    expectations: dict[str, dict[str, Any]] = {}
    ownerships: dict[str, dict[str, Any]] = {}
    downstream_decisions: dict[str, dict[str, Any]] = {}
    used_routes: dict[str, str] = {}
    for item in semantic_inventory(modules):
        carrier = carriers[item["carrier_id"]]
        data = (csswg / carrier["path"]).read_bytes()
        exact = _exact_source(item, carrier, data)
        source_id = _source_id(exact)
        sources.setdefault(source_id, {"id": source_id, **exact})

        owned = routes.get((carrier["spec_id"], carrier.get("anchor")), [])
        no_tests = [route for route in owned if route["status"] == "NO_DIRECT_TESTS"]
        mapped = [route for route in owned if route["status"] == "PINNED"]
        unmapped = [route for route in owned if route["status"] == "UNMAPPED"]
        ownership_id = wpt_owner_identity(carrier)
        if no_tests and not mapped and not unmapped:
            wpt_status = "NO_DIRECT_TESTS"
            wpt_evidence = {"declarations": [{"reason": route["reason"], "start": route["start"]} for route in no_tests]}
        elif mapped:
            for route in mapped:
                used_routes[route["route"]] = route["blob_oid"]
            wpt_status = "UNRUN"
            wpt_evidence = {"routes": sorted({(route["route"], route["blob_oid"]) for route in mapped}), "unmapped_declarations": sorted({route.get("route") for route in unmapped if route.get("route")})}
        else:
            wpt_status = "UNMAPPED"
            wpt_evidence = {"unmapped_declarations": sorted({route.get("route") for route in unmapped if route.get("route")})}
        ownerships.setdefault(ownership_id, {"id": ownership_id, "spec_id": carrier["spec_id"], "anchor": carrier.get("anchor"), "relation": "EXACT_SPEC_SECTION_ANCHOR", "status": wpt_status, **wpt_evidence})

        matched = rules.get(item["name"], [])
        expectation_id = f"bbnf-expectation::{item['name']}"
        expectations.setdefault(expectation_id, {"id": expectation_id, "production": item["name"], "status": "UNMAPPED", "observed_rule_name_matches": matched, "authority": "NAME_MATCH_OBSERVATION_ONLY_NOT_A_SEMANTIC_BINDING_TYPE_FACT_OR_OWNER"})
        public_item = {key: value for key, value in item.items() if key not in {"source_object", "semantic_id"}}
        row = {
            **public_item,
            "source_id": source_id,
            "bbnf_expectation_id": expectation_id,
            "wpt_ownership_id": ownership_id,
            "responsible_module": None,
            "planes": {"bbnf": "UNMAPPED", "fixtures": "UNMAPPED", "wpt": wpt_status, "recovery": "UNPROVEN", "diagnostics": "UNPROVEN", "serialization": "UNPROVEN", "owner": "UNMAPPED"},
        }
        row["semantic_state"] = semantic_state(row)
        decision = downstream(row)
        decision_id = downstream_id(decision)
        downstream_decisions.setdefault(decision_id, {"id": decision_id, **decision})
        row["terminal_downstream_id"] = decision_id
        rows.append(row)

    rows.sort(key=lambda row: row["id"])
    semantic_counts = Counter(row["semantic_state"] for row in rows)
    family_counts = Counter(row["family"] for row in rows)
    plane_open = {plane: sum(row["planes"][plane] not in EVIDENCED[plane] for row in rows) for plane in PLANES}
    fixture = ROOT / "fixtures" / "mutations.json"
    result = {
        "schema_version": 3,
        "stage": "semantic-matrix",
        "author_gate": "PASS",
        "semantic_portfolio": "RED" if any(plane_open.values()) else "EVIDENCED",
        "status_enums": {key: sorted(value) for key, value in ENUMS.items()},
        "sources": [sources[key] for key in sorted(sources)],
        "bbnf_expectations": [expectations[key] for key in sorted(expectations)],
        "wpt_ownership": [ownerships[key] for key in sorted(ownerships)],
        "downstream_decisions": [downstream_decisions[key] for key in sorted(downstream_decisions)],
        "fixture_authority": {"path": fixture.relative_to(ROOT).as_posix(), "bytes": fixture.stat().st_size, "sha256": sha256(fixture.read_bytes()), "semantic_routes": 0},
        "wpt_retention_receipt": {"mapped_route_count": len(used_routes), "path_oid_sha256": sha256("".join(f"{path}\0{used_routes[path]}\0" for path in sorted(used_routes)).encode()) if used_routes else None, "actual_outcomes": 0},
        "rows": rows,
        "row_counts": dict(sorted(family_counts.items())),
        "semantic_counts": dict(sorted(semantic_counts.items())),
        "open_obligations": [
            {"code": "VT_W1_BBNF_FIXTURE_AND_BEHAVIOR_EVIDENCE_REQUIRED", "row_count": len(rows), "plane_open_counts": plane_open},
            {"code": "VT_W1_WPT_EXECUTION_REQUIRED", "exact_owned_route_count": len(used_routes), "actual_outcomes": 0},
            {"code": "VT_W0_EXTERNAL_CONTROL_FREEZE_AND_MEASUREMENT_REQUIRED", "controls": [row["control"] for row in downstream_controls()["controls"]], "selection_status": "UNRUN"},
        ],
        "downstream_control_gates": downstream_controls(),
        "derived_affected": {"rows": len(rows), "routed_vt_w1": semantic_counts["OBLIGATION"]},
        "root_blockers": [],
    }
    validate(result, modules, csswg)
    return result
