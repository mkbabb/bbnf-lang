"""Source-qualified modules, extension/profile evidence, and total reference closure."""

from __future__ import annotations

import re
from collections import Counter, defaultdict
from typing import Any

from common import fail, require_clean
from dialects import references


CLOSURE_DECISIONS = {"RESOLVED_LOCAL", "AMBIGUOUS_LOCAL", "RESOLVED_IMPORT", "AMBIGUOUS_IMPORT", "UNMAPPED"}


def _name(lhs: str) -> str:
    value = " ".join(lhs.split())
    if value.startswith("<<") and value.endswith(">>"):
        return value[2:-2].strip()
    if value.startswith("<") and value.endswith(">"):
        return value[1:-1].strip()
    return value


def _entity_names(fields: list[dict[str, Any]], kind: str, carrier_id: str) -> list[tuple[str, dict[str, Any]]]:
    names = []
    for field in fields:
        if field["label"] != "name":
            continue
        for value in field["value"].split(","):
            if value.strip():
                names.append((value.strip(" ‘’'\""), field))
    if not names and kind == "legacy-row":
        names.append((f"media-feature@{carrier_id}", {"label": "name", "value": carrier_id, "source": {"start": None, "end": None, "sha256": None}}))
    return names


def _walk_units(dialects: dict[str, Any], extraction: dict[str, Any]) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]]]:
    carriers = {row["id"]: row for row in extraction["records"]}
    definitions: list[dict[str, Any]] = []
    entities: list[dict[str, Any]] = []
    observations: list[dict[str, Any]] = []
    for unit in dialects["units"]:
        carrier = carriers[unit["carrier_id"]]
        parsed = unit["parsed"]
        if isinstance(parsed, list):
            for ordinal, declaration in enumerate(parsed, start=1):
                observations.append({"id": f"idl::{carrier['spec_id']}::{carrier['id']}::{ordinal}", "family": "idl-declaration", "module": carrier["spec_id"], "carrier_id": carrier["id"], "declaration": declaration, "source": {"path": carrier["path"], "start": carrier["start"], "end": carrier["end"], "sha256": carrier["raw_sha256"]}})
        elif "definitions" in parsed:
            for ordinal, definition in enumerate(parsed["definitions"], start=1):
                name = _name(definition["lhs"])
                definitions.append({"id": f"definition::{carrier['spec_id']}::{carrier['id']}::{ordinal}", "module": carrier["spec_id"], "carrier_id": carrier["id"], "dialect": unit["dialect"], "name": name, "lhs": definition["lhs"], "rhs": definition["rhs"], "ast": definition["ast"], "source": {"path": carrier["path"], "carrier_start": carrier["start"], "carrier_end": carrier["end"], "carrier_sha256": carrier["raw_sha256"], "lhs_text_start": definition["lhs_start"], "lhs_text_end": definition["lhs_end"], "rhs_source_parts": definition["rhs_source_parts"]}})
            if parsed.get("expression"):
                expression = parsed["expression"]
                observations.append({"id": f"expression::{carrier['id']}", "family": "expression", "module": carrier["spec_id"], "carrier_id": carrier["id"], "ast": expression["ast"], "source": {"path": carrier["path"], "carrier_start": carrier["start"], "carrier_end": carrier["end"], "carrier_sha256": carrier["raw_sha256"], "text_start": expression["text_start"], "text_end": expression["text_end"]}})
        elif parsed.get("kind") == "legacy-yacc":
            for ordinal, production in enumerate(parsed["productions"], start=1):
                ast = {"kind": "legacy-yacc-rhs", "references": [{"kind": "reference", "name": name, "text_start": production["text_start"], "text_end": production["text_end"]} for name in production["references"]]}
                definitions.append({"id": f"definition::{carrier['spec_id']}::{carrier['id']}::{ordinal}", "module": carrier["spec_id"], "carrier_id": carrier["id"], "dialect": unit["dialect"], "name": production["name"], "lhs": production["name"], "rhs": production["rhs"], "ast": ast, "source": {"path": carrier["path"], "carrier_start": carrier["start"], "carrier_end": carrier["end"], "carrier_sha256": carrier["raw_sha256"], "text_start": production["text_start"], "text_end": production["text_end"]}})
        elif parsed.get("kind") == "legacy-lex":
            for ordinal, macro in enumerate(parsed["macros"], start=1):
                definitions.append({"id": f"scanner::{carrier['spec_id']}::{carrier['id']}::{ordinal}", "module": carrier["spec_id"], "carrier_id": carrier["id"], "dialect": unit["dialect"], "name": macro["name"], "lhs": macro["name"], "rhs": macro["pattern"], "ast": {"kind": "lex-pattern", "value": macro["pattern"]}, "source": {"path": carrier["path"], "carrier_start": carrier["start"], "carrier_end": carrier["end"], "carrier_sha256": carrier["raw_sha256"], "line": macro["line"], "text_start": macro["text_start"], "text_end": macro["text_end"]}})
        elif parsed.get("kind") == "entity-fields":
            for name, name_field in _entity_names(parsed["fields"], unit["kind"], carrier["id"]):
                entities.append({"id": f"entity::{carrier['spec_id']}::{carrier['id']}::{name}", "module": carrier["spec_id"], "carrier_id": carrier["id"], "entity_kind": unit["kind"], "name": name, "name_source": name_field["source"], "field_occurrences": parsed["fields"], "value_asts": parsed["value_asts"], "source": {"path": carrier["path"], "start": carrier["start"], "end": carrier["end"], "sha256": carrier["raw_sha256"]}})
        elif parsed.get("kind") == "algorithm":
            for step in parsed["steps"]:
                observations.append({"id": f"algorithm::{carrier['spec_id']}::{carrier['id']}::{step['ordinal']}", "family": "algorithm-step", "module": carrier["spec_id"], "carrier_id": carrier["id"], "step": step, "source": {"path": carrier["path"], "start": carrier["start"], "end": carrier["end"], "sha256": carrier["raw_sha256"]}})
        elif parsed.get("kind") in {"railroad", "legacy-lex"}:
            observations.append({"id": f"expression::{carrier['id']}", "family": unit["kind"], "module": carrier["spec_id"], "carrier_id": carrier["id"], "ast": parsed, "source": {"path": carrier["path"], "carrier_start": carrier["start"], "carrier_end": carrier["end"], "carrier_sha256": carrier["raw_sha256"], "text_start": parsed.get("text_start", 0), "text_end": parsed.get("text_end", len(carrier["text"]))}})
    return definitions, entities, observations


def validate(document: dict[str, Any]) -> None:
    expected = []
    for definition in document.get("definitions", []):
        expected.extend((definition["id"], "definition", definition["module"], row["name"], row.get("text_start"), row.get("text_end")) for row in references(definition["ast"]))
    for observation in document.get("observations", []):
        if observation.get("ast"):
            expected.extend((observation["id"], "expression", observation["module"], row["name"], row.get("text_start"), row.get("text_end")) for row in references(observation["ast"]))
    for entity in document.get("entities", []):
        for value in entity.get("value_asts", []):
            expected.extend((entity["id"], "entity-field", entity["module"], row["name"], row.get("text_start"), row.get("text_end")) for row in references(value["ast"]))
    closure = document.get("reference_closure", [])
    actual = [(row.get("source_id"), row.get("source_kind"), row.get("module"), row.get("reference"), row.get("text_start"), row.get("text_end")) for row in closure]
    if Counter(expected) != Counter(actual):
        fail("CLOSURE_NOT_TOTAL", "reference closure differs from the exact AST occurrence multiset", expected=len(expected), actual=len(actual))
    if len({row.get("id") for row in closure}) != len(closure):
        fail("CLOSURE_ID_DUPLICATE", "reference closure identity is not unique")
    unknown = [row for row in closure if row.get("decision") not in CLOSURE_DECISIONS]
    if unknown:
        fail("CLOSURE_DECISION_UNKNOWN", "reference closure contains an unknown decision", count=len(unknown))
    definition_ids = {row["id"] for row in document.get("definitions", [])}
    for row in closure:
        targets = row.get("target_ids", [])
        decision = row["decision"]
        expected_arity = 0 if decision == "UNMAPPED" else (1 if decision.startswith("RESOLVED_") else 2)
        if (expected_arity < 2 and len(targets) != expected_arity) or (expected_arity == 2 and len(targets) < 2) or any(target not in definition_ids for target in targets):
            fail("CLOSURE_TARGET_INVALID", "reference closure decision has invalid source-qualified targets", closure_id=row.get("id"), decision=decision)


def capture(scope: dict[str, Any], extraction: dict[str, Any], dialects: dict[str, Any]) -> dict[str, Any]:
    require_clean(dialects, "module-closure")
    definitions, entities, observations = _walk_units(dialects, extraction)
    specs = {row["id"]: row for row in scope["specs"]}
    imports: dict[str, set[str]] = defaultdict(set)
    for edge in scope["dependencies"]:
        if edge["classification"] == "NORMATIVE_CSS_SOURCE" and edge.get("target_spec"):
            imports[edge["from_spec"]].add(edge["target_spec"])
    by_qualified: dict[tuple[str, str], list[dict[str, Any]]] = defaultdict(list)
    by_name: dict[str, list[dict[str, Any]]] = defaultdict(list)
    for row in definitions:
        by_qualified[(row["module"], row["name"])].append(row)
        by_name[row["name"]].append(row)

    closure: list[dict[str, Any]] = []

    def close(source_id: str, module: str, occurrence: dict[str, Any], source_kind: str) -> None:
        name = occurrence["name"]
        local = by_qualified.get((module, name), [])
        imported = [row for target in sorted(imports[module]) for row in by_qualified.get((target, name), [])]
        if len(local) == 1:
            decision, targets = "RESOLVED_LOCAL", [local[0]["id"]]
        elif len(local) > 1:
            decision, targets = "AMBIGUOUS_LOCAL", [row["id"] for row in local]
        elif len(imported) == 1:
            decision, targets = "RESOLVED_IMPORT", [imported[0]["id"]]
        elif len(imported) > 1:
            decision, targets = "AMBIGUOUS_IMPORT", [row["id"] for row in imported]
        else:
            decision, targets = "UNMAPPED", []
        closure.append({"id": f"closure::{len(closure)+1}", "source_id": source_id, "source_kind": source_kind, "module": module, "reference": name, "text_start": occurrence.get("text_start"), "text_end": occurrence.get("text_end"), "decision": decision, "target_ids": targets})

    for definition in definitions:
        for occurrence in references(definition["ast"]):
            close(definition["id"], definition["module"], occurrence, "definition")
    for observation in observations:
        if observation.get("ast"):
            for occurrence in references(observation["ast"]):
                close(observation["id"], observation["module"], occurrence, "expression")
    for entity in entities:
        for value in entity["value_asts"]:
            for occurrence in references(value["ast"]):
                close(entity["id"], entity["module"], occurrence, "entity-field")

    extensions: list[dict[str, Any]] = []
    entity_by_name: dict[str, list[dict[str, Any]]] = defaultdict(list)
    for entity in entities:
        entity_by_name[entity["name"]].append(entity)
    for entity in entities:
        for ordinal, field in enumerate(entity["field_occurrences"], start=1):
            if field["label"] != "new_values":
                continue
            candidates = [row for row in entity_by_name[entity["name"]] if row["module"] in imports[entity["module"]] and row["id"] != entity["id"]]
            if len(candidates) == 1:
                decision, targets = "EXTENDS_ENTITY", [candidates[0]["id"]]
            elif candidates:
                decision, targets = "AMBIGUOUS_EXTENSION_BASE", [row["id"] for row in candidates]
            else:
                decision, targets = "UNMAPPED_BASE", []
            extensions.append({"id": f"extension::{entity['id']}::{ordinal}", "module": entity["module"], "entity_id": entity["id"], "field_ordinal": ordinal, "source": field["source"], "decision": decision, "target_entity_ids": targets})

    modules = []
    for identifier, spec in sorted(specs.items()):
        own_definitions = [row["id"] for row in definitions if row["module"] == identifier]
        info = spec.get("metadata", {})
        modules.append({"id": identifier, "source": {"path": spec["path"], "sha256": spec["sha256"]}, "imports": sorted(imports[identifier]), "exports": own_definitions, "shortname": info.get("shortname"), "level": info.get("level"), "profile_scopes": spec["scopes"], "selection": "INCLUDED_SOURCE_QUALIFIED"})

    # No bare-name winner exists. Repetitions are explicit conflict/extension observations.
    conflicts = []
    for name, rows in sorted(by_name.items()):
        if len(rows) > 1:
            conflicts.append({"name": name, "definition_ids": [row["id"] for row in rows], "modules": sorted({row["module"] for row in rows}), "decision": "COEXIST_SOURCE_QUALIFIED"})

    result = {
        "schema_version": 3,
        "stage": "module-closure",
        "profile": {"name": "CSS_SNAPSHOT_2026_CURRENT_WORK_L4_UNION", "decision": "COMPOSE_SOURCE_QUALIFIED_MODULES", "module_ids": sorted(specs), "scalar_priority": False},
        "aliases": scope["selection"].get("aliases", []),
        "supersession": {"decision": "NO_IMPLICIT_LEVEL_SUPERSESSION", "reason": "CSS levels coexist as source-qualified modules unless a retained authority says otherwise"},
        "modules": modules,
        "definitions": definitions,
        "entities": entities,
        "observations": observations,
        "extensions": extensions,
        "conflicts": conflicts,
        "reference_closure": closure,
        "root_blockers": [],
    }
    validate(result)
    return result
