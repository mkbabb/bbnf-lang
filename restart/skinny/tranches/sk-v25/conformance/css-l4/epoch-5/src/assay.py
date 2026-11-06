"""Eight direct epoch-5 owning mechanisms and their integrated validators."""

from __future__ import annotations

import copy
import gzip
import re
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any

from common import CSS_ROOT, EPOCH4, INPUTS, OBJECTS, ROOT, GateError, encode, evidence_index, exact_file, fail, git_blob_oid, git_tree_oid, gzip_object, load, load_epoch4_inputs, loads, object_bytes, receipt, sha256
from parsers import UnsupportedDialect, parse_css_vds, parse_lex, parse_webidl, parse_yacc, project_markup
from source import denominator as extract_denominator, scan_markup


HEX40 = re.compile(r"[0-9a-f]{40}")
POSITIVE = {"VALID"}
PLANE_STATUS = {
    "input_bytes": {"VALID", "BLOCKED"},
    "local_semantics": {"BLOCKED"},
    "wpt_test_membership": {"BLOCKED"},
    "downstream": {"RED_OPEN"},
    "candidate_product": {"ABSENT_RED"},
    "package": {"RED_UNBUILT"},
    "production": {"RED_UNCHANGED"},
}


def _manifest_records(value: Any) -> list[dict[str, Any]]:
    rows: list[dict[str, Any]] = []
    if isinstance(value, dict):
        if set(value) >= {"bytes", "sha256"} and isinstance(value.get("bytes"), int) and isinstance(value.get("sha256"), str):
            rows.append(value)
        for nested in value.values():
            rows.extend(_manifest_records(nested))
    elif isinstance(value, list):
        for nested in value:
            rows.extend(_manifest_records(nested))
    return rows


def load_inputs() -> dict[str, Any]:
    inputs = load_epoch4_inputs()
    manifest = inputs["manifest"]
    records = {row["sha256"]: row for row in _manifest_records(manifest)}
    for row in records.values():
        object_bytes(row)
    physical = {
        path.parent.name + path.name
        for path in (EPOCH4 / "inputs" / "objects" / "sha256").glob("*/*")
        if path.is_file()
    }
    if set(records) != physical:
        fail("OBJECT_STORE_NOT_CLOSED", "epoch-4 immutable object store has missing or extra objects", missing=sorted(set(records) - physical), extra=sorted(physical - set(records)))
    sources = {row["path"]: object_bytes(row) for row in manifest["csswg"]["sources"]}
    obligation_record = manifest["inherited_obligations"]
    obligation_bytes = gzip_object(obligation_record["object"])
    if len(obligation_bytes) != obligation_record["uncompressed_bytes"] or sha256(obligation_bytes) != obligation_record["uncompressed_sha256"]:
        fail("OBLIGATION_BUNDLE_DRIFT", "inherited obligation bundle differs")
    obligations = loads(obligation_bytes, source="inherited-obligations")
    if len(obligations.get("rows", [])) != 8_796:
        fail("OBLIGATION_COUNT_DRIFT", "inherited obligation identity count differs")
    inputs.update({"records": records, "sources": sources, "obligations": obligations, "obligation_bytes": obligation_bytes})
    return inputs


def _tree_entries(contract: dict[str, Any]) -> list[dict[str, str]]:
    tree = contract["csswg_tree"]
    compressed = object_bytes({"bytes": tree["bytes"], "sha256": tree["object_sha256"]}, epoch4=False)
    try:
        raw = gzip.decompress(compressed)
    except OSError as error:
        fail("CSSWG_TREE_GZIP_INVALID", "CSSWG tree receipt cannot be decoded", detail=str(error))
    if len(raw) != tree["uncompressed_bytes"] or sha256(raw) != tree["uncompressed_sha256"]:
        fail("CSSWG_TREE_RECEIPT_DRIFT", "CSSWG tree receipt differs after decoding")
    value = loads(raw, source="csswg-pinned-tree")
    if value.get("commit") != tree["commit"] or value.get("root_tree") != tree["root_tree"] or value.get("truncated") is not False:
        fail("CSSWG_TREE_RECEIPT_INCOMPLETE", "pinned tree receipt is truncated or bound to another commit")
    entries = value.get("entries")
    if not isinstance(entries, list) or len(entries) != tree["entry_count"]:
        fail("CSSWG_TREE_ENTRY_COUNT", "pinned tree receipt entry count differs")
    return entries


def validate_tree_catalog(entries: list[dict[str, str]], expected_root: str) -> dict[str, Any]:
    by_path: dict[str, dict[str, str]] = {}
    allowed_modes = {"040000": "tree", "100644": "blob", "100755": "blob", "120000": "blob"}
    for row in entries:
        path = row.get("path")
        mode = row.get("mode")
        kind = row.get("type")
        oid = row.get("sha")
        if not isinstance(path, str):
            fail("TREE_PATH_NOT_UTF8", "tree path is not a UTF-8 string")
        try:
            encoded = path.encode("utf-8", errors="strict")
        except UnicodeEncodeError as error:
            fail("TREE_PATH_NOT_UTF8", "tree path contains a surrogate", path=repr(path), detail=str(error))
        if not encoded or b"\0" in encoded or path.startswith("/") or "//" in path or any(part in {"", ".", ".."} for part in path.split("/")):
            fail("TREE_PATH_INVALID", "tree path is not canonical", path=path)
        if mode not in allowed_modes or allowed_modes[mode] != kind or not isinstance(oid, str) or HEX40.fullmatch(oid) is None:
            fail("TREE_ENTRY_INVALID", "tree entry mode/type/OID is invalid", path=path)
        if path in by_path:
            fail("TREE_PATH_DUPLICATE", "tree path is duplicated", path=path)
        by_path[path] = row
    tree_paths = {path for path, row in by_path.items() if row["type"] == "tree"}
    for path in by_path:
        parts = path.split("/")
        for index in range(1, len(parts)):
            if "/".join(parts[:index]) not in tree_paths:
                fail("TREE_PARENT_MISSING", "tree entry lacks a retained parent", path=path)
    children: dict[str, list[tuple[str, str, str]]] = defaultdict(list)
    for path, row in by_path.items():
        parent, _, name = path.rpartition("/")
        children[parent].append((row["mode"], name, row["sha"]))
    for path in sorted(tree_paths, key=lambda item: item.count("/"), reverse=True):
        actual = git_tree_oid(children[path])
        if actual != by_path[path]["sha"]:
            fail("TREE_OID_MISMATCH", "retained subtree does not reconstruct", path=path, expected=by_path[path]["sha"], actual=actual)
    actual_root = git_tree_oid(children[""])
    if actual_root != expected_root:
        fail("TREE_ROOT_MISMATCH", "retained tree does not reconstruct the commit root", expected=expected_root, actual=actual_root)
    return {"by_path": by_path, "root_tree": actual_root, "entry_count": len(by_path), "blob_count": sum(row["type"] == "blob" for row in by_path.values()), "tree_count": len(tree_paths)}


def genesis(inputs: dict[str, Any]) -> dict[str, Any]:
    contract = inputs["contract"]
    checked = validate_tree_catalog(_tree_entries(contract), contract["csswg_tree"]["root_tree"])
    selected: list[dict[str, Any]] = []
    for row in inputs["manifest"]["csswg"]["sources"]:
        tree_row = checked["by_path"].get(row["path"])
        data = inputs["sources"][row["path"]]
        oid = git_blob_oid(data)
        if tree_row is None or tree_row["type"] != "blob" or tree_row["sha"] != oid:
            fail("SELECTED_SOURCE_NOT_IN_PIN", "selected CSSWG source is not the exact pinned blob", path=row["path"], actual_oid=oid)
        selected.append({"path": row["path"], "bytes": len(data), "sha256": sha256(data), "git_blob_oid": oid})
    return {
        "schema_version": 1,
        "status": "VALID_PINNED_TREE_SELECTION",
        "commit": contract["csswg_tree"]["commit"],
        "root_tree": checked["root_tree"],
        "entry_count": checked["entry_count"],
        "blob_count": checked["blob_count"],
        "tree_count": checked["tree_count"],
        "selection_source": "COMMIT_TREE_OBJECTS_NOT_WORKTREE",
        "worktree_state_authority": "NONE_REQUIRED_OR_CLAIMED",
        "untracked_ignored_staged_submodule_state": "OUTSIDE_DERIVATION_BY_CONSTRUCTION",
        "selected_count": len(selected),
        "selected": selected,
    }


def validate_genesis(value: dict[str, Any], inputs: dict[str, Any]) -> None:
    expected = genesis(inputs)
    if encode(value) != encode(expected):
        fail("GENESIS_NOT_PINNED_TREE_EXACT", "genesis differs from independently reconstructed pinned-tree selection")


def _metadata(data: bytes) -> dict[str, str]:
    found = re.search(rb"<pre\s+class\s*=\s*['\"]?metadata['\"]?[^>]*>(.*?)</pre>", data, re.I | re.S)
    result: dict[str, str] = {}
    if found:
        for line in found.group(1).decode("utf-8", errors="replace").splitlines():
            if ":" in line:
                key, value = line.split(":", 1)
                result.setdefault("_".join(key.lower().split()), " ".join(value.split()))
    return result


def _wpt_paths(inputs: dict[str, Any]) -> dict[str, str]:
    record = inputs["manifest"]["wpt"]
    raw = gzip_object(record["catalog_object"])
    if len(raw) != record["catalog_uncompressed_bytes"] or sha256(raw) != record["catalog_uncompressed_sha256"]:
        fail("WPT_CATALOG_DRIFT", "retained WPT path/blob catalog differs")
    pieces = raw.split(b"\0")
    if pieces[-1] or (len(pieces) - 1) % 2:
        fail("WPT_CATALOG_SHAPE", "retained WPT catalog shape is invalid")
    result: dict[str, str] = {}
    for index in range(0, len(pieces) - 1, 2):
        path = pieces[index].decode("utf-8", errors="strict")
        oid = pieces[index + 1].decode("ascii")
        if path in result or HEX40.fullmatch(oid) is None:
            fail("WPT_CATALOG_ENTRY", "WPT path/blob entry is invalid", path=path)
        result[path] = oid
    if len(result) != record["entry_count"]:
        fail("WPT_CATALOG_COUNT", "WPT catalog entry count differs")
    return result


def _classification(path: str) -> tuple[str, str]:
    lower = path.lower()
    name = Path(path).name.lower()
    if name in {"meta.yml", "web_features.yml", ".gitignore", "readme.md"}:
        return "METADATA", "path-name classification"
    if "/support/" in lower or name.startswith("support") or Path(name).suffix in {".css", ".js", ".json", ".png", ".jpg", ".svg", ".woff", ".woff2", ".ttf"}:
        return "SUPPORT_OR_RESOURCE", "path-only conservative classification"
    if "-ref." in name or ".ref." in name or "/reference/" in lower:
        return "REFERENCE", "path-only reference convention"
    return "UNCLASSIFIED_TEST_MANIFEST_REQUIRED", "no retained WPT manifest or blob content"


def wpt_prefix_blobs(inputs: dict[str, Any]) -> dict[str, Any]:
    tree = _wpt_paths(inputs)
    directives: list[dict[str, Any]] = []
    matched: dict[str, dict[str, Any]] = {}
    rest_matched: dict[str, str] = {}
    unmapped: set[str] = set()

    def add(route: str, oid: str, provenance: list[Any]) -> None:
        target = matched.setdefault(route, {"blob_oid": oid, "source_directives": []})
        if target["blob_oid"] != oid:
            fail("WPT_PATH_OID_COLLISION", "one pinned WPT path resolved to multiple blob OIDs", path=route)
        if provenance not in target["source_directives"]:
            target["source_directives"].append(provenance)

    for spec_id, path in sorted(inputs["policy"]["source_paths"].items()):
        data = inputs["sources"][path]
        prefix_default = _metadata(data).get("wpt_path_prefix") or f"css/{spec_id}"
        scan = scan_markup(data)
        by_serial = {tag.serial: tag for tag in scan.tags}
        for tag in [item for item in scan.tags if item.name in {"wpt", "wpt-rest"}]:
            lineage = [by_serial[index] for index in tag.ancestors] + [tag]
            classes = set().union(*(item.classes for item in lineage))
            excluded = any(item.name == "del" for item in lineage) or bool(classes & {"example", "informative", "non-normative", "note"})
            prefix = ((tag.attributes.get("pathprefix") or prefix_default).strip("/") + "/")
            if excluded:
                directives.append({"kind": tag.name.upper().replace("-", "_"), "spec_id": spec_id, "source_path": path, "source_span": [tag.start, tag.close_end or tag.open_end], "prefix": prefix, "status": "EXCLUDED"})
                continue
            if tag.name == "wpt-rest":
                expanded = {key: value for key, value in tree.items() if key.startswith(prefix)}
                directives.append({"kind": "WPT_REST", "spec_id": spec_id, "source_path": path, "source_span": [tag.start, tag.close_end or tag.open_end], "prefix": prefix, "status": "DECLARED", "blob_count": len(expanded)})
                rest_matched.update(expanded)
                for route, oid in expanded.items():
                    right = tag.close_end or tag.open_end
                    add(route, oid, [spec_id, path, [[tag.start, right]], sha256(data[tag.start:right]), "WPT_REST"])
                continue
            if tag.close_start is None:
                directives.append({"kind": "WPT", "spec_id": spec_id, "source_path": path, "source_span": [tag.start, tag.open_end], "prefix": prefix, "status": "BLOCKED_UNCLOSED"})
                continue
            projection = project_markup(data[tag.open_end : tag.close_start], base=tag.open_end)
            declared: list[tuple[str, list[list[int]]]] = []
            line_offset = 0
            for raw_line in projection.text.splitlines(keepends=True):
                line = raw_line.rstrip("\r\n")
                if line.split() and not line.lstrip().startswith("#"):
                    left = len(line) - len(line.lstrip())
                    right = len(line.rstrip())
                    declared.append((" ".join(line.split()), projection.fragments(line_offset + left, line_offset + right)))
                line_offset += len(raw_line)
            if not declared:
                directives.append({"kind": "WPT", "spec_id": spec_id, "source_path": path, "source_span": [tag.start, tag.close_end or tag.close_start], "prefix": prefix, "status": "NO_DIRECT_TESTS", "declared_path_count": 0})
                continue
            resolved = 0
            for name, fragments in declared:
                route = name.lstrip("/") if name.startswith(("/", "css/", "html/", "svg/", "dom/", "url/", "webidl/")) else prefix + name
                if route in tree:
                    source = b"".join(data[left:right] for left, right in fragments)
                    add(route, tree[route], [spec_id, path, fragments, sha256(source), "WPT"])
                    resolved += 1
                else:
                    unmapped.add(route)
            directives.append({"kind": "WPT", "spec_id": spec_id, "source_path": path, "source_span": [tag.start, tag.close_end or tag.close_start], "prefix": prefix, "status": "DECLARED", "declared_path_count": len(declared), "resolved_path_count": resolved})
    rows = []
    for path, target in sorted(matched.items()):
        classification, basis = _classification(path)
        rows.append({"path": path, "blob_oid": target["blob_oid"], "classification": classification, "classification_basis": basis, "source_directives": sorted(target["source_directives"])})
    counts = dict(sorted(Counter(row["classification"] for row in rows).items()))
    return {
        "schema_version": 1,
        "relation": "DECLARED_DIRECT_PATH_OR_PREFIX_TO_PINNED_PATH_BLOB_OID_MEMBERSHIP_ONLY",
        "status": "BLOCKED_NO_WPT_TEST_MEMBERSHIP",
        "wpt_revision": inputs["manifest"]["wpt"]["revision"],
        "wpt_tree": inputs["manifest"]["wpt"]["tree_oid"],
        "directives": directives,
        "path_blob_count": len(rows),
        "wpt_rest_prefix_blob_count": len(rest_matched),
        "unmapped_declared_path_count": len(unmapped),
        "unmapped_declared_path_digest": sha256(encode(sorted(unmapped))),
        "classification_counts": counts,
        "wpt_test_count": 0,
        "wpt_execution_status": "UNRUN",
        "actual_outcome_count": 0,
        "rows": rows,
    }


def validate_wpt(value: dict[str, Any], inputs: dict[str, Any]) -> None:
    expected = wpt_prefix_blobs(inputs)
    if encode(value) != encode(expected):
        fail("WPT_PREFIX_CLASSIFICATION_DRIFT", "WPT prefix/blob classifications differ from retained pinned catalog")
    if value["wpt_test_count"] != 0 or value["status"] != "BLOCKED_NO_WPT_TEST_MEMBERSHIP":
        fail("WPT_TEST_MEMBERSHIP_UNPROVED", "prefix blobs cannot be promoted to WPT tests")


def candidate_absence(root: Path = ROOT) -> dict[str, Any]:
    contract = load(INPUTS / "contract.json")
    rows = []
    for relative in contract["candidate_roots"]:
        path = root / relative
        if path.exists() or path.is_symlink():
            fail("CANDIDATE_PRESENT", "enumerated candidate root is present", root=relative)
        rows.append({"root": relative, "status": "ABSENT", "lstat": "ENOENT"})
    return {
        "schema_version": 1,
        "status": "ABSENT_RED",
        "enumeration_closed": True,
        "candidate_count": 0,
        "complete_candidate_count": 0,
        "roots": rows,
        "future_candidate_authority": None,
        "credit": "NONE",
    }


def validate_candidate(value: dict[str, Any], root: Path = ROOT) -> None:
    expected = candidate_absence(root)
    if encode(value) != encode(expected):
        fail("CANDIDATE_ROOT_ENUMERATION_DRIFT", "candidate absence does not match the closed root enumeration")


def validate_denominator(value: dict[str, Any], inputs: dict[str, Any]) -> None:
    units = value.get("units")
    if not isinstance(units, list) or value.get("unit_count") != len(units):
        fail("DENOMINATOR_COUNT_DRIFT", "unit count differs from unit ledger")
    identities = [row.get("id") for row in units]
    if len(identities) != len(set(identities)):
        fail("DENOMINATOR_ID_DUPLICATE", "unit identity is duplicated")
    by_path: dict[str, list[tuple[int, int, int]]] = defaultdict(list)
    for index, row in enumerate(inputs["obligations"]["rows"]):
        source = inputs["obligations"]["sources"][row[3]]
        by_path[source[2]].append((index, source[3], source[4]))
    digest_rows = []
    for row in units:
        path = row.get("path")
        if path not in inputs["sources"] or not isinstance(row.get("fragments"), list) or not row["fragments"]:
            fail("UNIT_SOURCE_INVALID", "unit lacks an exact retained source and fragments", unit=row.get("id"))
        data = inputs["sources"][path]
        prior = -1
        links: set[int] = set()
        payload = bytearray()
        for fragment in row["fragments"]:
            if not isinstance(fragment, list) or len(fragment) != 2 or not all(isinstance(item, int) for item in fragment):
                fail("UNIT_FRAGMENT_INVALID", "unit fragment is malformed", unit=row.get("id"))
            left, right = fragment
            if left < 0 or left >= right or right > len(data) or left < prior:
                fail("UNIT_FRAGMENT_RANGE", "unit fragment is outside exact source bytes", unit=row.get("id"))
            prior = right
            payload.extend(data[left:right])
            links.update(index for index, start, end in by_path[path] if start < right and left < end)
        if row.get("source_bytes") != len(payload) or row.get("source_sha256") != sha256(bytes(payload)):
            fail("UNIT_SOURCE_DIGEST_DRIFT", "unit source bytes differ", unit=row.get("id"))
        if row.get("reverse_links") != sorted(links):
            fail("UNIT_REVERSE_LINK_DRIFT", "unit reverse links are not independently derived", unit=row.get("id"))
        if row.get("disposition") not in {"PARSED_WITH_REVERSE_LINKS_RED", "BLOCKED_NO_REVERSE_LINK", "BLOCKED_TYPED_UNSUPPORTED", "BLOCKED_DETECTOR_FAILURE", "EXCLUDED_DELETED", "EXCLUDED_NON_NORMATIVE"}:
            fail("UNIT_DISPOSITION_INVALID", "unit disposition is not closed", unit=row.get("id"))
        digest_rows.append([row["id"], row["family"], row["fragments"], row["source_sha256"], row["disposition"], row["reverse_links"]])
    expected_digest = sha256(encode(digest_rows))
    expected_families = dict(sorted(Counter(row["family"] for row in units).items()))
    expected_dispositions = dict(sorted(Counter(row["disposition"] for row in units).items()))
    if value.get("unit_digest") != expected_digest or value.get("family_counts") != expected_families or value.get("disposition_counts") != expected_dispositions:
        fail("DENOMINATOR_DIGEST_DRIFT", "ledger digest or totals differ from its canonical unit rows")
    if value.get("reverse_link_count") != sum(len(row["reverse_links"]) for row in units):
        fail("DENOMINATOR_REVERSE_LINK_COUNT", "unit reverse-link count differs")
    independently_derived = inputs.get("_independent_denominator")
    if independently_derived is not None and encode(value) != encode(independently_derived):
        fail("DENOMINATOR_NOT_SOURCE_TOTAL", "unit ledger differs from a fresh sealed-extractor source rederivation")


def observations(artifacts: dict[str, bytes], inputs: dict[str, Any]) -> dict[str, Any]:
    evidence = [
        receipt("input-contract", "inputs/contract.json", (INPUTS / "contract.json").read_bytes()),
        receipt("epoch4-input-manifest", "epoch4/inputs/manifest.json", (EPOCH4 / "inputs" / "manifest.json").read_bytes()),
    ]
    for name, data in sorted(artifacts.items()):
        evidence.append(receipt("generated-artifact", name, data))
    denominator_value = loads(artifacts["denominator.json"], source="generated-denominator")
    candidate_value = loads(artifacts["candidate-absence.json"], source="generated-candidate")
    return {
        "schema_version": 1,
        "stage": "CSS_L4_W0_INPUT_ASSAY_ONLY",
        "claims": {
            "input_bytes": {"status": "VALID", "evidence": ["inputs/contract.json", "epoch4/inputs/manifest.json", "genesis.json"]},
            "local_semantics": {"status": "BLOCKED", "unit_count": denominator_value["unit_count"], "unit_digest": denominator_value["unit_digest"], "blocked_units": sum(count for status, count in denominator_value["disposition_counts"].items() if status.startswith("BLOCKED")), "evidence": ["denominator.json"]},
            "wpt_test_membership": {"status": "BLOCKED", "test_count": 0, "execution": "UNRUN", "outcomes": 0, "evidence": ["wpt-prefix-blobs.json"]},
            "downstream": {"status": "RED_OPEN", "evidence": []},
            "candidate_product": {"status": "ABSENT_RED", "candidate_count": candidate_value["candidate_count"], "complete_candidate_count": candidate_value["complete_candidate_count"], "credit": "NONE", "evidence": ["candidate-absence.json"]},
            "package": {"status": "RED_UNBUILT", "evidence": []},
            "production": {"status": "RED_UNCHANGED", "evidence": []},
        },
        "evidence": evidence,
        "inherited_obligations": {"count": 8_796, "status": "OPEN_RED", "sha256": inputs["manifest"]["inherited_obligations"]["uncompressed_sha256"]},
        "controls": inputs["controls"],
        "routed_remainder": [
            {"id": "VT.W0.JSON_ORACLE", "status": "RED_UNRUN"},
            {"id": "VT.W0.INCUMBENT_PROFILES_HOT_LEAVES", "status": "RED_UNRUN"},
            {"id": "VT.W0.FOUR_PRIVATE_COMPILING_PLACEHOLDERS", "status": "RED_ABSENT"},
            {"id": "VT.W0.LIGHTNING_CSS_CONTROL", "status": "UNRUN"},
            {"id": "VT.W0.SONIC_RS_CONTROL", "status": "UNRUN"},
        ],
        "credit": {"css": "NONE", "css_l4": "NONE", "value_tape": "NONE", "lock": "NONE", "candidate": "NONE", "product": "NONE", "production": "NONE"},
    }


def validate_document(document: dict[str, Any], resolved: dict[str, bytes], artifacts: dict[str, Any], inputs: dict[str, Any]) -> None:
    if document.get("schema_version") != 1 or document.get("stage") != "CSS_L4_W0_INPUT_ASSAY_ONLY":
        fail("DOCUMENT_SCHEMA_INVALID", "integrated observation document schema/stage differs")
    claims = document.get("claims")
    if not isinstance(claims, dict) or set(claims) != set(PLANE_STATUS):
        fail("CLAIM_PLANES_NOT_CLOSED", "claim plane set differs", actual=sorted(claims) if isinstance(claims, dict) else None)
    index = evidence_index(document.get("evidence", []))
    if set(index) != set(resolved):
        fail("EVIDENCE_REFERENTS_NOT_CLOSED", "evidence ledger and resolved referents differ", ledger=sorted(index), resolved=sorted(resolved))
    for identity, data in resolved.items():
        if index[identity] != (len(data), sha256(data)):
            fail("EVIDENCE_REFERENT_DRIFT", "evidence receipt does not resolve to exact bytes", identity=identity)
    for plane, allowed in PLANE_STATUS.items():
        claim = claims[plane]
        status = claim.get("status")
        if status not in allowed:
            fail("CLAIM_STATUS_INVALID", "claim status is outside closed enum", plane=plane, status=status)
        refs = claim.get("evidence")
        if not isinstance(refs, list) or any(ref not in index for ref in refs):
            fail("CLAIM_EVIDENCE_UNRESOLVED", "claim evidence reference is missing", plane=plane)
        if status in POSITIVE and not refs:
            fail("POSITIVE_STATE_WITHOUT_EVIDENCE", "positive claim lacks resolved evidence", plane=plane)
    denominator_value = artifacts["denominator.json"]
    validate_denominator(denominator_value, inputs)
    local = claims["local_semantics"]
    blocked = sum(count for status, count in denominator_value["disposition_counts"].items() if status.startswith("BLOCKED"))
    if local.get("unit_count") != denominator_value["unit_count"] or local.get("unit_digest") != denominator_value["unit_digest"] or local.get("blocked_units") != blocked or blocked == 0:
        fail("LOCAL_SEMANTIC_LAUNDERED", "local claim differs from blocked atomic denominator")
    validate_genesis(artifacts["genesis.json"], inputs)
    validate_wpt(artifacts["wpt-prefix-blobs.json"], inputs)
    validate_candidate(artifacts["candidate-absence.json"])
    candidate = claims["candidate_product"]
    if candidate.get("candidate_count") != 0 or candidate.get("complete_candidate_count") != 0 or candidate.get("credit") != "NONE":
        fail("CANDIDATE_CREDIT_FABRICATED", "candidate/product claim is not zero-credit absence")
    if claims["wpt_test_membership"].get("test_count") != 0 or claims["wpt_test_membership"].get("execution") != "UNRUN" or claims["wpt_test_membership"].get("outcomes") != 0:
        fail("WPT_TEST_MEMBERSHIP_UNPROVED", "WPT test/outcome state was promoted")
    expected_credit = {"css": "NONE", "css_l4": "NONE", "value_tape": "NONE", "lock": "NONE", "candidate": "NONE", "product": "NONE", "production": "NONE"}
    if document.get("credit") != expected_credit:
        fail("CREDIT_NOT_ZERO", "assay carries unauthorized credit")


def build_base() -> tuple[dict[str, Any], dict[str, Any]]:
    inputs = load_inputs()
    values = {
        "genesis.json": genesis(inputs),
        "denominator.json": extract_denominator(inputs["sources"], inputs["policy"], inputs["obligations"]),
        "wpt-prefix-blobs.json": wpt_prefix_blobs(inputs),
        "candidate-absence.json": candidate_absence(),
    }
    validate_genesis(values["genesis.json"], inputs)
    validate_denominator(values["denominator.json"], inputs)
    validate_wpt(values["wpt-prefix-blobs.json"], inputs)
    validate_candidate(values["candidate-absence.json"])
    return inputs, values
