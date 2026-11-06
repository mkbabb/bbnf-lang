"""Compact epoch-4 replay, occurrence closure, source census, and RED ledger."""

from __future__ import annotations

import gzip
import html
import re
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any

from common import INPUTS, ROOT, encode, evidence, fail, load, loads, object_bytes, positive, sha256
from languages import DialectError, parse_css_vds, parse_lex, parse_railroad, parse_webidl, parse_yacc
from source import Token, classes, dependency_occurrences, element_body, inherited_state, scan, wpt_directives


METADATA = re.compile(rb"<pre\s+class\s*=\s*['\"]?metadata['\"]?[^>]*>(.*?)</pre>", re.I | re.S)
SOURCE_KINDS = {"propdef", "descdef", "prod", "railroad", "idl"}
FIELD_VALUE = re.compile(r"(?im)(?:^|\n)\s*(?:value|new values)\s*:\s*(.*?)(?=\n\s*[A-Za-z][A-Za-z ]{0,30}\s*:|\Z)", re.S)
VALUE_DEFINITION = re.compile(r"(?m)^\s*(?:<<[^<>]+>>|<[^<>]+>|@?[-*A-Za-z][\w-]*(?:\(\))?)\s*=\s*")
CONTROL_STATE = {"correctness_status": "UNRUN", "selection_status": "UNRUN", "performance_status": "UNRUN"}


def _metadata(data: bytes) -> dict[str, str]:
    found = METADATA.search(data)
    if found is None:
        return {}
    result: dict[str, str] = {}
    for line in found.group(1).decode("utf-8", errors="replace").splitlines():
        if ":" in line:
            key, value = line.split(":", 1)
            result.setdefault("_".join(key.lower().split()), " ".join(value.split()))
    return result


def _plain(data: bytes) -> str:
    return " ".join(html.unescape(re.sub(rb"<[^>]*>", b" ", data).decode("utf-8", errors="strict")).split())


def _decode_object(record: dict[str, Any], *, label: str) -> bytes:
    compressed = object_bytes(record)
    try:
        return gzip.decompress(compressed)
    except (OSError, EOFError) as error:
        fail("BUNDLE_INVALID", "compressed input object cannot be decoded", label=label, detail=str(error))


def load_inputs() -> dict[str, Any]:
    manifest = load(INPUTS / "manifest.json")
    policy = load(INPUTS / "policy.json")
    governing = load(INPUTS / "governing.json")
    controls = load(INPUTS / "downstream-controls.json")
    if manifest.get("schema_version") != 1 or len(manifest.get("csswg", {}).get("sources", [])) != 96:
        fail("INPUT_MANIFEST_INVALID", "input manifest lacks 96 exact CSSWG source records")
    if manifest["inherited_obligations"].get("count") != 8_796:
        fail("OBLIGATION_COUNT_DRIFT", "inherited open obligation count differs")
    for row in controls.get("controls", []):
        for key, status in CONTROL_STATE.items():
            if row.get(key) != status:
                fail("CONTROL_STATE_DRIFT", "unexecuted control state must remain UNRUN", control=row.get("control"), field=key)
    for row in governing.get("documents", []):
        path = ROOT.parents[2] / row["path"]
        if sha256(path.read_bytes()) != row["sha256"]:
            fail("GOVERNING_DRIFT", "governing document differs from captured identity", path=row["path"])

    source_bytes: dict[str, bytes] = {}
    for row in manifest["csswg"]["sources"]:
        data = object_bytes(row)
        source_bytes[row["path"]] = data
    if set(source_bytes) != set(policy.get("source_paths", {}).values()):
        fail("SOURCE_SET_DRIFT", "policy and exact source object sets differ")
    catalog_record = manifest["csswg"]["full_source_key_catalog"]
    catalog_bytes = _decode_object(catalog_record["object"], label="csswg-source-key-catalog")
    if len(catalog_bytes) != catalog_record["uncompressed_bytes"] or sha256(catalog_bytes) != catalog_record["uncompressed_sha256"]:
        fail("SOURCE_KEY_CATALOG_DRIFT", "full CSSWG source-key catalog differs")
    source_key_catalog = loads(catalog_bytes, source="csswg-source-key-catalog")
    if len(source_key_catalog.get("entries", [])) != catalog_record["entry_count"]:
        fail("SOURCE_KEY_CATALOG_COUNT", "full CSSWG source-key catalog count differs")

    wpt_record = manifest["wpt"]
    catalog = _decode_object(wpt_record["catalog_object"], label="wpt-catalog")
    if len(catalog) != wpt_record["catalog_uncompressed_bytes"] or sha256(catalog) != wpt_record["catalog_uncompressed_sha256"]:
        fail("WPT_CATALOG_DRIFT", "WPT catalog content differs from exact tree receipt")
    pieces = catalog.split(b"\0")
    if pieces[-1] or (len(pieces) - 1) % 2:
        fail("WPT_CATALOG_SHAPE", "WPT path/blob-OID catalog shape is invalid")
    wpt_paths: dict[str, str] = {}
    for index in range(0, len(pieces) - 1, 2):
        path = pieces[index].decode("utf-8", errors="strict")
        oid = pieces[index + 1].decode("ascii")
        if path in wpt_paths or re.fullmatch(r"[0-9a-f]{40}", oid) is None:
            fail("WPT_CATALOG_ENTRY_INVALID", "WPT path/blob identity is invalid", path=path)
        wpt_paths[path] = oid
    if len(wpt_paths) != wpt_record["entry_count"]:
        fail("WPT_CATALOG_COUNT_DRIFT", "WPT entry count differs")

    obligation_record = manifest["inherited_obligations"]
    obligation_bytes = _decode_object(obligation_record["object"], label="obligations")
    if len(obligation_bytes) != obligation_record["uncompressed_bytes"] or sha256(obligation_bytes) != obligation_record["uncompressed_sha256"]:
        fail("OBLIGATION_BUNDLE_DRIFT", "inherited obligation bundle differs")
    obligations = loads(obligation_bytes, source="inherited-obligations")
    if len(obligations.get("rows", [])) != 8_796:
        fail("OBLIGATION_ROWS_DRIFT", "inherited atomic obligation ledger differs")
    return {
        "manifest": manifest,
        "policy": policy,
        "governing": governing,
        "controls": controls,
        "source_bytes": source_bytes,
        "source_key_catalog": source_key_catalog,
        "wpt_paths": wpt_paths,
        "obligations": obligations,
    }


def source_multimap(inputs: dict[str, Any]) -> tuple[dict[str, list[str]], dict[str, dict[str, str]]]:
    index: dict[str, set[str]] = defaultdict(set)
    metadata: dict[str, dict[str, str]] = {}
    by_directory: dict[str, str] = {}
    selected_by_path = {relative: spec_id for spec_id, relative in inputs["policy"]["source_paths"].items()}
    for spec_id, relative in sorted(inputs["policy"]["source_paths"].items()):
        data = inputs["source_bytes"][relative]
        info = _metadata(data)
        metadata[spec_id] = info
        directory = Path(relative).parent.name
        by_directory[directory.lower()] = spec_id
    for directory, relative, _bytes, _digest, keys in inputs["source_key_catalog"]["entries"]:
        identity = selected_by_path.get(relative, f"unsealed::{directory}")
        for key in keys:
            index[key].add(identity)
    for alias, directory in inputs["policy"].get("aliases", {}).items():
        target = by_directory.get(directory.lower())
        if target:
            index[alias.upper()].add(target)
    return {key: sorted(value) for key, value in index.items()}, metadata


def dependency_ledger(inputs: dict[str, Any], index: dict[str, list[str]]) -> tuple[list[list[Any]], list[dict[str, Any]], dict[str, int]]:
    retained_external = {
        row["authority_key"]: row
        for row in inputs["manifest"]["primary_authorities"]
        if row["role"] == "external-parsing-authority"
    }
    mandatory_unsealed = set(inputs["policy"]["mandatory_unsealed_parsing_keys"])
    ledger: list[list[Any]] = []
    blockers: list[dict[str, Any]] = []
    counters: Counter[str] = Counter()
    for spec_id, relative in sorted(inputs["policy"]["source_paths"].items()):
        data = inputs["source_bytes"][relative]
        for row in dependency_occurrences(spec_id, relative, data, scan(data)):
            if row["source_state"] == "COMMENT":
                decision, target, receipts = "EXCLUDED_COMMENT", None, []
            elif row["source_state"] == "DELETED":
                decision, target, receipts = "EXCLUDED_DELETED", None, []
            elif row["source_state"] == "NON_NORMATIVE" or not row["normative"]:
                decision, target, receipts = "INFORMATIVE_NO_EDGE", None, []
            else:
                candidates = index.get(row["key"], [])
                if len(candidates) == 1:
                    target = candidates[0]
                    if target.startswith("unsealed::"):
                        decision, receipts = "BLOCKED_CSS_SOURCE_BYTES_UNSEALED", []
                        blockers.append({"code": "CSS_SOURCE_BYTES_UNSEALED", "occurrence_id": row["id"], "key": row["key"], "target": target})
                    else:
                        target_record = next(item for item in inputs["manifest"]["csswg"]["sources"] if item["spec_id"] == target)
                        decision = "NORMATIVE_CSS_SOURCE"
                        receipts = [evidence("csswg-source", target_record["path"], target_record["sha256"])]
                elif len(candidates) > 1:
                    decision, target, receipts = "BLOCKED_COLLISION", candidates, []
                    blockers.append({"code": "DEPENDENCY_KEY_COLLISION", "occurrence_id": row["id"], "key": row["key"], "candidates": candidates})
                elif row["key"] in retained_external:
                    authority = retained_external[row["key"]]
                    decision, target = "EXTERNAL_PARSING_AUTHORITY", row["key"]
                    receipts = [evidence("external-authority", authority["url"], authority["sha256"])]
                elif row["key"] in mandatory_unsealed:
                    decision, target, receipts = "BLOCKED_EXTERNAL_AUTHORITY_UNSEALED", row["key"], []
                    blockers.append({"code": "EXTERNAL_PARSING_AUTHORITY_UNSEALED", "occurrence_id": row["id"], "key": row["key"]})
                else:
                    decision, target, receipts = "BLOCKED_OCCURRENCE_REVIEW_REQUIRED", None, []
                    blockers.append({"code": "DEPENDENCY_OCCURRENCE_UNREVIEWED", "occurrence_id": row["id"], "key": row["key"]})
            counters[decision] += 1
            # Every occurrence retains its own decision; shared vectors are interned
            # later by deterministic tuple identity rather than global key policy.
            ledger.append([row["id"], spec_id, relative, row["start"], row["end"], row["key"], row["syntax"], row["source_state"], row["normative"], decision, target, receipts])
    return ledger, blockers, dict(sorted(counters.items()))


def _carrier_kind(opening: Token, policy: dict[str, Any], path: str, raw: bytes) -> tuple[str | None, str | None]:
    own = classes(opening)
    direct = next((kind for kind in SOURCE_KINDS if kind in own), None)
    if direct:
        return direct, "SOURCE_CLASS"
    if "lang-webidl" in own and "def" in own:
        return "idl", "WEBIDL_CLASS"
    if "media-feature" in own:
        return "legacy-row", "MEDIA_FEATURE_CLASS"
    if "algorithm" in own or opening.attributes and "algorithm" in opening.attributes:
        return "algorithm", "ALGORITHM_CLASS"
    if opening.name == "pre":
        exact = next((row for row in policy["legacy_pre_sites"] if row["path"] == path and row["start"] == opening.start and row["sha256"] == sha256(raw)), None)
        if exact and exact["decision"].startswith("LEGACY_"):
            return "legacy-grammar", exact["decision"]
    return None, None


def _parse_carrier(kind: str, dialect: str, body: bytes) -> tuple[str, list[dict[str, Any]]]:
    text = body.decode("utf-8", errors="strict")
    plain = html.unescape(re.sub(r"<[^>]*>", "", text)) if kind != "railroad" else html.unescape(text)
    units: list[dict[str, Any]] = []
    if kind == "railroad":
        parsed = parse_railroad(plain)
        units.append({"family": "railroad", "start": parsed["text_start"], "end": parsed["text_end"]})
    elif dialect == "LEGACY_YACC":
        parsed = parse_yacc(plain)
        units.extend({"family": "yacc-rule", "start": row["text_start"], "end": row["text_end"], "name": row["name"]} for row in parsed["productions"])
    elif dialect == "LEGACY_LEX":
        parsed = parse_lex(plain)
        units.extend({"family": "lex-macro", "start": row["text_start"], "end": row["text_end"], "name": row["name"], "references": row["macro_references"]} for row in parsed["macros"])
        units.extend({"family": "lex-rule", "start": row["text_start"], "end": row["text_end"], "references": row["macro_references"]} for row in parsed["rules"])
    elif kind == "idl":
        parsed = parse_webidl(plain)
        units.extend({"family": "webidl-declaration", "start": row["text_start"], "end": row["text_end"], "member_count": len(row["members"])} for row in parsed["declarations"])
    elif kind in {"prod", "legacy-grammar"}:
        boundaries = list(VALUE_DEFINITION.finditer(plain))
        if boundaries:
            for index, match in enumerate(boundaries):
                end = boundaries[index + 1].start() if index + 1 < len(boundaries) else len(plain)
                rhs = plain[match.end() : end].strip()
                if not rhs: raise DialectError("CSS VDS definition has empty RHS")
                parse_css_vds(rhs)
                units.append({"family": "production", "start": match.start(), "end": end})
        elif plain.strip():
            parse_css_vds(plain.strip())
            units.append({"family": "expression", "start": 0, "end": len(plain)})
        else:
            raise DialectError("CSS VDS carrier is empty")
    elif kind in {"propdef", "descdef", "legacy-row"}:
        values = [match.group(1).strip() for match in FIELD_VALUE.finditer(plain) if match.group(1).strip()]
        for value in values:
            parse_css_vds(value)
        units.append({"family": "entity-table", "start": 0, "end": len(plain), "value_fields": len(values)})
    elif kind == "algorithm":
        units.append({"family": "structural-algorithm", "start": 0, "end": len(plain)})
    return plain, units


def semantic_census(inputs: dict[str, Any]) -> tuple[list[list[Any]], list[dict[str, Any]], dict[str, Any]]:
    sources = inputs["obligations"]["sources"]
    rows = inputs["obligations"]["rows"]
    by_path: dict[str, list[tuple[int, list[Any]]]] = defaultdict(list)
    for row_index, row in enumerate(rows):
        source = sources[row[3]]
        by_path[source[2]].append((row_index, source))

    def links(path: str, start: int, end: int) -> list[int]:
        return [index for index, source in by_path[path] if source[3] < end and start < source[4]]

    ledger: list[list[Any]] = []
    blockers: list[dict[str, Any]] = []
    linked_rows: set[int] = set()
    family_counts: Counter[str] = Counter()
    dispositions: Counter[str] = Counter()
    for spec_id, path in sorted(inputs["policy"]["source_paths"].items()):
        data = inputs["source_bytes"][path]
        tokens = scan(data)
        starts = [token for token in tokens if token.kind == "start_tag"]
        carrier_spans: set[tuple[int, int]] = set()
        for opening in starts:
            if opening.name not in {"pre", "table", "xmp", "div", "ol", "dl"} and not classes(opening) & (SOURCE_KINDS | {"lang-webidl", "media-feature", "algorithm"}):
                continue
            try:
                body, end = element_body(opening, tokens, data)
            except Exception:
                continue
            raw = data[opening.start:end]
            kind, dialect = _carrier_kind(opening, inputs["policy"], path, raw)
            if kind is None:
                if opening.name == "pre":
                    occurrence_id = "semantic::" + sha256(f"{path}\0{opening.start}\0{end}\0excluded-pre".encode())
                    ledger.append([occurrence_id, "explicit-pre-exclusion", spec_id, path, opening.start, end, "EXCLUDED_NON_SEMANTIC_PRE", []])
                    dispositions["EXCLUDED_NON_SEMANTIC_PRE"] += 1
                    family_counts["explicit-pre-exclusion"] += 1
                continue
            carrier_spans.add((opening.start, end))
            state, reason = inherited_state(opening)
            occurrence_id = "semantic::" + sha256(f"{path}\0{opening.start}\0{end}\0{kind}".encode())
            row_links = links(path, opening.start, end)
            if state != "NORMATIVE":
                disposition = "EXCLUDED_INHERITED_NORMATIVITY"
                # Preserve reverse links to inherited epoch-3 obligations as
                # casualty evidence, but do not count them as mapped semantics.
                ledger.append([occurrence_id, kind, spec_id, path, opening.start, end, disposition, row_links, reason])
                dispositions[disposition] += 1
                family_counts[kind] += 1
                continue
            try:
                _plain_text, units = _parse_carrier(kind, dialect or "", body)
                disposition = "MAPPED_BASELINE_OBLIGATIONS" if row_links else "BLOCKED_NO_BASELINE_MAPPING"
                if not row_links:
                    blockers.append({"code": "SOURCE_OCCURRENCE_UNMAPPED", "occurrence_id": occurrence_id, "family": kind})
                else:
                    linked_rows.update(row_links)
                ledger.append([occurrence_id, kind, spec_id, path, opening.start, end, disposition, row_links, {"dialect": dialect, "unit_count": len(units), "units": units}])
            except (DialectError, UnicodeDecodeError) as error:
                disposition = "BLOCKED_TYPED_DIALECT"
                blockers.append({"code": "DIALECT_TYPED_BLOCKER", "occurrence_id": occurrence_id, "family": kind, "dialect": dialect, "detail": str(error)})
                ledger.append([occurrence_id, kind, spec_id, path, opening.start, end, disposition, row_links, {"dialect": dialect, "detail": str(error)}])
            dispositions[disposition] += 1
            family_counts[kind] += 1

        # Independent denominator: normative source-native value additions and
        # named algorithms exist even when no legacy carrier recognized them.
        for opening in starts:
            state, reason = inherited_state(opening)
            if state != "NORMATIVE" or opening.name != "dfn":
                continue
            try:
                body, end = element_body(opening, tokens, data)
            except Exception:
                continue
            text = _plain(body)
            attributes = opening.attributes or {}
            algorithm_near = "algorithm" in data[end : end + 120].decode("utf-8", errors="ignore").lower()
            if "algorithm" in text.lower() or algorithm_near:
                family = "named-algorithm"
                if algorithm_near and "algorithm" not in text.lower():
                    text += " algorithm"
            elif attributes.get("data-dfn-for") or attributes.get("for"):
                family = "normative-dfn-value-addition"
            else:
                continue
            occurrence_id = "semantic::" + sha256(f"{path}\0{opening.start}\0{end}\0{family}".encode())
            row_links = links(path, opening.start, end)
            disposition = "MAPPED_BASELINE_OBLIGATIONS" if row_links else "BLOCKED_NO_BASELINE_MAPPING"
            if row_links:
                linked_rows.update(row_links)
            else:
                blockers.append({"code": "SOURCE_OCCURRENCE_UNMAPPED", "occurrence_id": occurrence_id, "family": family, "text": text[:160]})
            ledger.append([occurrence_id, family, spec_id, path, opening.start, end, disposition, row_links, text])
            dispositions[disposition] += 1
            family_counts[family] += 1
        for opening in starts:
            if opening.name not in {"h1", "h2", "h3", "h4", "h5", "h6"} or inherited_state(opening)[0] != "NORMATIVE":
                continue
            try:
                body, end = element_body(opening, tokens, data)
            except Exception:
                continue
            text = _plain(body)
            if "algorithm" not in text.lower():
                continue
            occurrence_id = "semantic::" + sha256(f"{path}\0{opening.start}\0{end}\0named-algorithm-heading".encode())
            row_links = links(path, opening.start, end)
            disposition = "MAPPED_BASELINE_OBLIGATIONS" if row_links else "BLOCKED_NO_BASELINE_MAPPING"
            if row_links: linked_rows.update(row_links)
            else: blockers.append({"code": "SOURCE_OCCURRENCE_UNMAPPED", "occurrence_id": occurrence_id, "family": "named-algorithm", "text": text[:160]})
            ledger.append([occurrence_id, "named-algorithm", spec_id, path, opening.start, end, disposition, row_links, text])
            dispositions[disposition] += 1
            family_counts["named-algorithm"] += 1

    missing = sorted(set(range(len(rows))) - linked_rows)
    if missing:
        blockers.append({"code": "BASELINE_OBLIGATIONS_NOT_REACHED_BY_INDEPENDENT_CENSUS", "count": len(missing), "index_sha256": sha256(encode(missing))})
    summary = {
        "occurrence_count": len(ledger),
        "family_counts": dict(sorted(family_counts.items())),
        "disposition_counts": dict(sorted(dispositions.items())),
        "linked_inherited_obligations": len(linked_rows),
        "unlinked_inherited_obligations": len(missing),
        "unlinked_index_sha256": sha256(encode(missing)),
    }
    return ledger, blockers, summary


def wpt_membership(inputs: dict[str, Any]) -> tuple[dict[str, Any], list[dict[str, Any]]]:
    tree = inputs["wpt_paths"]
    routes: dict[str, str] = {}
    unmapped: set[str] = set()
    directive_counts: Counter[str] = Counter()
    blockers: list[dict[str, Any]] = []
    directive_digest_input: list[Any] = []
    for spec_id, path in sorted(inputs["policy"]["source_paths"].items()):
        data = inputs["source_bytes"][path]
        info = _metadata(data)
        spec = {"id": spec_id, "wpt_path_prefix": info.get("wpt_path_prefix")}
        for directive in wpt_directives(spec, data, scan(data)):
            directive_counts[directive["kind"] + ":" + directive["status"]] += 1
            directive_digest_input.append([spec_id, path, directive])
            if directive["status"] == "BLOCKED_UNKNOWN_DIRECTIVE":
                blockers.append({"code": "WPT_DIRECTIVE_UNKNOWN", "spec_id": spec_id, "path": path, "tag": directive["tag"], "start": directive["start"]})
            elif directive["status"] != "DECLARED":
                continue
            elif directive["kind"] == "WPT_REST":
                matched = {route: oid for route, oid in tree.items() if route.startswith(directive["prefix"])}
                if not matched:
                    blockers.append({"code": "WPT_REST_PREFIX_EMPTY", "spec_id": spec_id, "prefix": directive["prefix"]})
                routes.update(matched)
            else:
                for name in directive["names"]:
                    route = name.lstrip("/") if name.startswith(("/", "css/", "html/", "svg/", "dom/", "url/", "webidl/")) else directive["prefix"] + name
                    if route in tree:
                        routes[route] = tree[route]
                    else:
                        unmapped.add(route)
    # Blob OIDs prove membership, not feature ownership or outcomes. Test blob
    # contents were not available as sealed inputs, so exact relations block.
    blockers.append({"code": "WPT_BLOB_CONTENT_UNRETAINED", "route_count": len(routes)})
    blockers.append({"code": "WPT_EXACT_FEATURE_RELATION_UNPROVEN", "inherited_obligation_count": 8_796})
    route_bytes = "".join(f"{path}\0{routes[path]}\0" for path in sorted(routes)).encode()
    return {
        "directive_counts": dict(sorted(directive_counts.items())),
        "directive_ledger_sha256": sha256(encode(directive_digest_input)),
        "resolved_route_count": len(routes),
        "resolved_route_blob_oid_sha256": sha256(route_bytes),
        "unmapped_route_count": len(unmapped),
        "unmapped_routes_sha256": sha256(encode(sorted(unmapped))),
        "membership_status": "REPRODUCED_WITH_TYPED_BLOB_BLOCKER",
        "ownership_status": "UNMAPPED",
        "ownership_relations": 0,
        "cartesian_fanout_relations": 0,
        "execution_status": "UNRUN",
        "actual_outcomes": 0,
    }, blockers


def validate_claim_boundary(document: dict[str, Any]) -> None:
    boundary = document.get("claim_boundary", {})
    required = {"byte_replay", "local_semantic", "downstream", "candidate_product"}
    if set(boundary) != required:
        fail("CLAIM_BOUNDARY_SHAPE", "claim boundary states are not disjoint and complete")
    if boundary["byte_replay"].get("status") == "VALID" and not boundary["byte_replay"].get("evidence"):
        fail("BYTE_REPLAY_EVIDENCE_MISSING", "valid byte replay has no exact evidence")
    if boundary["local_semantic"].get("status") == "VALID" and boundary["local_semantic"].get("blockers"):
        fail("LOCAL_SEMANTIC_LAUNDERED", "local semantic validity cannot coexist with known detector loss")
    if boundary["downstream"].get("semantic_obligation_count") != 8_796:
        fail("DOWNSTREAM_OBLIGATION_DRIFT", "downstream semantic obligations must remain atomic and open")
    if boundary["candidate_product"].get("candidate_count") != 0 or boundary["candidate_product"].get("complete_candidate_count") != 0:
        fail("CANDIDATE_FABRICATED", "input assay cannot fabricate a product candidate")


def validate_positive_state(status: str, receipts: list[dict[str, Any]]) -> None:
    if status in {"VALID", "MAPPED", "OBSERVED", "ASSIGNED", "PASS"} and not receipts:
        fail("POSITIVE_STATE_WITHOUT_EVIDENCE", "positive status spelling is not evidence", status=status)


def validate_document(document: dict[str, Any]) -> None:
    validate_claim_boundary(document)
    dependencies = document.get("dependencies", {})
    decisions = dependencies.get("decisions", [])
    if dependencies.get("occurrence_count") != len(decisions) or len({row[0] for row in decisions}) != len(decisions):
        fail("DEPENDENCY_LEDGER_NOT_TOTAL", "occurrence-qualified dependency ledger differs from its census")
    if any(len(row) != 12 or not row[9] for row in decisions):
        fail("DEPENDENCY_DECISION_MISSING", "a dependency occurrence lacks one explicit decision")
    semantics = document.get("source_semantics", {})
    census = semantics.get("census", [])
    if semantics.get("occurrence_count") != len(census) or len({row[0] for row in census}) != len(census):
        fail("SOURCE_CENSUS_NOT_TOTAL", "independent source-semantic occurrence ledger differs from its census")
    if any(len(row) < 8 or not row[6] for row in census):
        fail("SOURCE_DISPOSITION_MISSING", "source-semantic occurrence lacks exactly one disposition")
    wpt = document.get("wpt", {})
    if wpt.get("cartesian_fanout_relations") != 0 or wpt.get("ownership_relations") != 0 or wpt.get("execution_status") != "UNRUN":
        fail("WPT_OWNERSHIP_FABRICATED", "WPT membership was converted into broad ownership or an invented outcome")
    normalization = document.get("normalization", {})
    if normalization.get("atomic_obligation_count") != 8_796 or normalization.get("plane_vector_count") != 1 or normalization.get("terminal_decision_count") != 1 or normalization.get("expanded_row_payloads") != 0:
        fail("NORMALIZATION_DRIFT", "normalized evidence expanded or lost atomic obligation identity")
    blockers = {row["id"] for row in document.get("evidence_intern", {}).get("blockers", [])}
    refs = document["claim_boundary"]["local_semantic"].get("blockers", [])
    if any(ref not in blockers for ref in refs):
        fail("BLOCKER_REFERENCE_MISSING", "local semantic state references missing typed blocker evidence")


def build() -> dict[str, Any]:
    inputs = load_inputs()
    index, _metadata_rows = source_multimap(inputs)
    dependency_rows, dependency_blockers, dependency_counts = dependency_ledger(inputs, index)
    census_rows, census_blockers, census_summary = semantic_census(inputs)
    wpt, wpt_blockers = wpt_membership(inputs)
    collisions = {key: value for key, value in index.items() if len(value) > 1}
    local_blockers = dependency_blockers + census_blockers + wpt_blockers
    # Every blocker instance is interned by exact content; counts preserve
    # multiplicity without repeating immutable descriptions.
    blocker_vectors: dict[str, dict[str, Any]] = {}
    blocker_refs: list[str] = []
    for blocker in local_blockers:
        identity = "blocker::" + sha256(encode(blocker))
        blocker_vectors.setdefault(identity, {"id": identity, **blocker})
        blocker_refs.append(identity)
    manifest_bytes = (INPUTS / "manifest.json").read_bytes()
    policy_bytes = (INPUTS / "policy.json").read_bytes()
    source_set_digest = sha256(encode([[row["path"], row["bytes"], row["sha256"]] for row in inputs["manifest"]["csswg"]["sources"]]))
    byte_receipts = [
        evidence("input-manifest", "inputs/manifest.json", sha256(manifest_bytes)),
        evidence("policy", "inputs/policy.json", sha256(policy_bytes)),
        evidence("csswg-source-set", inputs["manifest"]["csswg"]["revision"], source_set_digest, count=96),
        evidence("wpt-path-blob-oid-catalog", inputs["manifest"]["wpt"]["tree_oid"], inputs["manifest"]["wpt"]["catalog_uncompressed_sha256"], count=inputs["manifest"]["wpt"]["entry_count"]),
        evidence("inherited-open-obligations", "epoch-3-adjudicated-open-rows", inputs["manifest"]["inherited_obligations"]["uncompressed_sha256"], count=8_796),
    ]
    downstream_vector = {
        "semantic_obligation_count": 8_796,
        "default_planes": {
            "bbnf": "UNMAPPED", "fixtures": "UNMAPPED", "owner": "UNMAPPED",
            "wpt_membership": wpt["ownership_status"], "wpt_execution": "UNRUN",
            "recovery": "UNPROVEN", "diagnostics": "UNPROVEN", "serialization": "UNPROVEN",
        },
        "terminal": "RED_OPEN",
        "controls": inputs["controls"],
    }
    document = {
        "schema_version": 4,
        "stage": "css-l4-observation-assay",
        "claim_boundary": {
            "byte_replay": positive("VALID", byte_receipts),
            "local_semantic": {"status": "BLOCKED", "blocker_count": len(blocker_refs), "unique_blocker_count": len(blocker_vectors), "blockers": blocker_refs},
            "downstream": downstream_vector,
            "candidate_product": {"status": "ABSENT_RED", "candidate_count": 0, "complete_candidate_count": 0, "product_credit": "NONE"},
        },
        "inputs": {
            "csswg_revision": inputs["manifest"]["csswg"]["revision"],
            "csswg_sources": 96,
            "csswg_source_bytes": sum(row["bytes"] for row in inputs["manifest"]["csswg"]["sources"]),
            "wpt_revision": inputs["manifest"]["wpt"]["revision"],
            "wpt_tree": inputs["manifest"]["wpt"]["tree_oid"],
            "wpt_catalog_entries": len(inputs["wpt_paths"]),
            "private_paths": 0,
        },
        "dependencies": {
            "occurrence_count": len(dependency_rows),
            "decisions": dependency_rows,
            "decision_counts": dependency_counts,
            "source_key_collisions": collisions,
            "collision_count": len(collisions),
            "decision_rule": inputs["policy"]["dependency_decision_policy"],
        },
        "source_semantics": {"census": census_rows, **census_summary},
        "wpt": wpt,
        "evidence_intern": {
            "open_plane_vector": downstream_vector["default_planes"],
            "terminal_decision": {"status": "RED_OPEN", "route": "VT.W1"},
            "blockers": [blocker_vectors[key] for key in sorted(blocker_vectors)],
        },
        "normalization": {
            "atomic_obligation_count": 8_796,
            "source_identity_count": len(inputs["obligations"]["sources"]),
            "plane_vector_count": 1,
            "terminal_decision_count": 1,
            "expanded_row_payloads": 0,
            "reverse_links": sum(len(row[7]) for row in census_rows),
        },
        "broader_vt_w0_remainder": [
            {"id": "VT.W0.JSON_ORACLE", "status": "RED_PLACEHOLDER", "evidence": None},
            {"id": "VT.W0.INCUMBENT_PROFILES_HOT_LEAVES", "status": "RED_PLACEHOLDER", "evidence": None},
            {"id": "VT.W0.FOUR_PRIVATE_COMPILING_PLACEHOLDERS", "status": "RED_PLACEHOLDER", "candidate_count": 0},
            {"id": "VT.W0.LIGHTNING_CSS_CONTROL_FREEZE", "status": "UNRUN", "evidence": None},
            {"id": "VT.W0.SONIC_RS_CONTROL_FREEZE", "status": "UNRUN", "evidence": None},
        ],
    }
    validate_document(document)
    return document
