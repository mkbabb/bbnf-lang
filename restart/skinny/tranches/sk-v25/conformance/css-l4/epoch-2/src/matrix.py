"""Stage 4: external observation matrix with explicit incomplete states."""

from __future__ import annotations

from typing import Any

from common import encoded_json, reject_candidate_schema_use, sha256, terminal


REQUIRED_PLANES = (
    "bbnf_expectation",
    "fixtures",
    "wpt",
    "recovery",
    "diagnostics",
    "serialization",
    "owner",
)


def nearest_routes(spec: dict[str, Any], offset: int) -> list[dict[str, Any]]:
    routes = [route for route in spec.get("wpt_routes", []) if "route" in route]
    if not routes:
        return []
    distance = min(abs(route["source_offset"] - offset) for route in routes)
    return [route for route in routes if abs(route["source_offset"] - offset) == distance]


def observation_planes(spec: dict[str, Any], source: dict[str, Any], name: str) -> dict[str, Any]:
    routes = nearest_routes(spec, source["start"])
    return {
        "bbnf_expectation": {
            "module": f"css/{spec['id']}.bbnf",
            "production": name,
            "status": "UNMAPPED",
            "candidate_input": False,
        },
        "fixtures": {"positive": [], "negative": [], "status": "UNMAPPED"},
        "wpt": {
            "routes": [route["route"] for route in routes],
            "blob_oids": [route["blob_oid"] for route in routes if route.get("blob_oid")],
            "status": "UNRUN" if routes and all(route["terminal_state"] == "PINNED" for route in routes) else "UNMAPPED",
        },
        "recovery": {"expectation": None, "status": "UNPROVEN"},
        "diagnostics": {"expectation": None, "status": "UNPROVEN"},
        "serialization": {
            "canonical": None,
            "source_preserving": None,
            "status": "UNPROVEN",
        },
        "owner": {"module": f"css-l4-w0::{spec['id']}", "status": "ASSIGNED"},
    }


def row_state(row: dict[str, Any]) -> str:
    statuses = [row[plane]["status"] for plane in REQUIRED_PLANES]
    return "MAPPED" if all(terminal(status) for status in statuses) else "UNPROVEN"


def validate(document: dict[str, Any]) -> None:
    reject_candidate_schema_use(document)
    if document.get("schema_or_type_input_allowed") is not False:
        raise ValueError("matrix must explicitly reject candidate schema/type input")
    seen = set()
    for row in document["rows"]:
        if row["id"] in seen:
            raise ValueError(f"duplicate matrix row: {row['id']}")
        seen.add(row["id"])
        for plane in REQUIRED_PLANES:
            if plane not in row or "status" not in row[plane]:
                raise ValueError(f"matrix row {row['id']} lacks {plane} status")
        expected = row_state(row)
        if row["terminal_state"] != expected:
            raise ValueError(f"matrix row {row['id']} has non-derived terminal state")


def capture(scope: dict[str, Any], extraction: dict[str, Any], grammar: dict[str, Any]) -> dict[str, Any]:
    reject_candidate_schema_use(grammar)
    specs = {spec["id"]: spec for spec in scope["specs"]}
    carriers = {record["id"]: record for record in extraction["records"]}
    rows = []

    for definition in grammar["definitions"]:
        source = carriers[definition["carrier_id"]]
        spec = specs[definition["spec_id"]]
        row = {
            "id": f"production::{definition['id']}",
            "family": "production",
            "source": {
                "spec_id": source["spec_id"],
                "path": source["path"],
                "start": source["start"],
                "end": source["end"],
                "line": source["line"],
                "sha256": source["raw_sha256"],
            },
            "grammar_definition_id": definition["id"],
            "profile": definition["profile"],
            **observation_planes(spec, source, definition["name"]),
        }
        row["terminal_state"] = row_state(row)
        rows.append(row)

    for entity in grammar["entities"]:
        source = carriers[entity["carrier_id"]]
        spec = specs[entity["spec_id"]]
        row = {
            "id": f"entity::{entity['id']}",
            "family": entity["kind"],
            "source": {
                "spec_id": source["spec_id"],
                "path": source["path"],
                "start": source["start"],
                "end": source["end"],
                "line": source["line"],
                "sha256": source["raw_sha256"],
            },
            "grammar_entity_id": entity["id"],
            **observation_planes(spec, source, entity["name"]),
        }
        row["terminal_state"] = row_state(row)
        rows.append(row)

    for expression in grammar["expressions"]:
        source = carriers[expression["carrier_id"]]
        spec = specs[expression["spec_id"]]
        row = {
            "id": f"expression::{expression['id']}",
            "family": expression["dialect"],
            "source": {
                "spec_id": source["spec_id"],
                "path": source["path"],
                "start": source["start"],
                "end": source["end"],
                "line": source["line"],
                "sha256": source["raw_sha256"],
            },
            "grammar_expression_id": expression["id"],
            **observation_planes(spec, source, expression["id"]),
        }
        row["terminal_state"] = row_state(row)
        rows.append(row)

    for source in extraction["records"]:
        if source["normativity"] != "NORMATIVE" or source["kind"] not in {"algorithm", "idl"}:
            continue
        spec = specs[source["spec_id"]]
        row = {
            "id": f"{source['kind']}::{source['id']}",
            "family": source["kind"],
            "source": {
                "spec_id": source["spec_id"],
                "path": source["path"],
                "start": source["start"],
                "end": source["end"],
                "line": source["line"],
                "sha256": source["raw_sha256"],
            },
            **observation_planes(spec, source, source["id"]),
        }
        row["terminal_state"] = row_state(row)
        rows.append(row)

    represented_specs = {row["source"]["spec_id"] for row in rows}
    for spec in scope["specs"]:
        if spec["id"] in represented_specs:
            continue
        source = {
            "start": 0,
            "end": 0,
            "line": 1,
            "path": spec["path"],
            "spec_id": spec["id"],
            "raw_sha256": spec["sha256"],
        }
        row = {
            "id": f"coverage::{spec['id']}",
            "family": "coverage-decision",
            "source": {
                "spec_id": spec["id"],
                "path": spec["path"],
                "start": 0,
                "end": 0,
                "line": 1,
                "sha256": spec["sha256"],
            },
            **observation_planes(spec, source, "no-grammar-decision"),
        }
        row["terminal_state"] = row_state(row)
        rows.append(row)

    rows.sort(key=lambda row: row["id"])
    blockers = [
        {"code": "MATRIX_ROW_INCOMPLETE", "row_id": row["id"]}
        for row in rows
        if row["terminal_state"] != "MAPPED"
    ]
    result = {
        "schema_version": 2,
        "stage": "external-oracle-matrix",
        "candidate_input_policy": "OBSERVATION_ONLY_FORBID_SCHEMA_OR_TYPE_INPUT",
        "schema_or_type_input_allowed": False,
        "grammar_sha256": sha256(encoded_json(grammar)),
        "rows": rows,
        "status_counts": {
            "MAPPED": sum(row["terminal_state"] == "MAPPED" for row in rows),
            "UNPROVEN": sum(row["terminal_state"] == "UNPROVEN" for row in rows),
        },
        "blockers": blockers,
    }
    validate(result)
    return result
