from __future__ import annotations

import copy
import json
import tempfile
from pathlib import Path

from _support import DATA_ROOT, ROOT, mutations, observations
import crater
import manifest
import pipeline
from common import GateError, object_bytes, sha256
from languages import DialectError, parse_css_vds, parse_lex, parse_railroad, parse_webidl, parse_yacc
from source import inherited_state, dependency_occurrences, scan


def _code(callback):
    try:
        callback()
    except GateError as error:
        return error.code
    raise AssertionError("mutation was accepted")


def positive_state_requires_evidence():
    return _code(lambda: pipeline.validate_positive_state("MAPPED", []))


def rendered_dependencies_and_collisions_are_total():
    document = observations()
    assert any(row[6] == "RENDERED_BIBLIOGRAPHY" for row in document["dependencies"]["decisions"])
    assert document["dependencies"]["collision_count"] > 0
    assert any(row[9] == "BLOCKED_COLLISION" for row in document["dependencies"]["decisions"])
    return "BLOCKED_COLLISION"


def malformed_dialects_are_rejected():
    probes = (
        lambda: parse_css_vds("a )"),
        lambda: parse_yacc("a: ??? ;"),
        lambda: parse_yacc("a: b | | c ;"),
        lambda: parse_yacc("a: b [ c | d ;"),
        lambda: parse_lex("%option noyywrap\nA [unterminated\n%%\n{A} return A;\n"),
        lambda: parse_webidl("garbage\ninterface X {};\nmore garbage"),
    )
    for probe in probes:
        try:
            probe()
        except DialectError:
            continue
        raise AssertionError("malformed dialect was accepted")
    return "DialectError"


def railroad_envelope_covers_descendants():
    parsed = parse_railroad("Choice: 1\n  Seq:\n    T: descendant\n    T: tail\n  T: other\n")
    root = parsed["items"][0]
    descendant_end = max(child["text_end"] for child in root["children"])
    assert parsed["text_end"] == descendant_end and root["text_end"] == descendant_end
    assert root["text_end"] > root["line_end"]
    return "DESCENDANT_MAX"


def wpt_membership_is_not_cartesian_ownership():
    wpt = observations()["wpt"]
    assert wpt["ownership_relations"] == 0 and wpt["cartesian_fanout_relations"] == 0
    assert wpt["ownership_status"] == "UNMAPPED" and wpt["execution_status"] == "UNRUN"
    return "ZERO_FANOUT"


def independent_census_deletion_fails():
    mutated = copy.deepcopy(observations())
    mutated["source_semantics"]["census"].pop()
    return _code(lambda: pipeline.validate_document(mutated))


def crater_rejects_split_and_binary_oracles():
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        (root / "resolved-inputs.json").write_text('{"allowed_external_inputs":{},"runtime_reads":[]}')
        (root / "main.rs").write_text('const X: &[u8] = include_bytes!(concat!("conformance/", "css-l4/epoch-4/data/", "ma", "trix.json"));')
        (root / "oracle.bin").write_bytes(b"prefix semantic-matrix suffix")
        result = crater.inspect_candidate(root)
    assert any(row["code"] in {"FORBIDDEN_ORACLE_AUTHORITY", "OBSERVATION_ARTIFACT_FORBIDDEN"} for row in result["violations"])
    return "FORBIDDEN_ORACLE_AUTHORITY"


def content_object_drift_fails():
    row = dict(json.loads((ROOT / "inputs" / "manifest.json").read_text())["csswg"]["sources"][0])
    row["bytes"] += 1
    return _code(lambda: object_bytes(row))


def mutation_registry_is_executable():
    rows = mutations()["mutations"]
    assert len(rows) == 20 and len({row["id"] for row in rows}) == 20
    assert {row["test"] for row in rows} == set(FUNCTIONS)
    return "EXECUTED_20"


def normalized_artifact_beats_expanded_control():
    current = (DATA_ROOT / "observations.json").stat().st_size
    assert current < 11_481_625
    assert observations()["normalization"]["expanded_row_payloads"] == 0
    return "SMALLER"


def lex_rules_and_macro_edges_are_consumed():
    parsed = parse_lex("%option noyywrap\nA a\nB {A}+\n%%\n{B} return TOKEN;\n. return *yytext;\n")
    assert len(parsed["macros"]) == 2 and len(parsed["rules"]) == 2
    assert parsed["macros"][1]["macro_references"] == ["A"]
    assert parsed["rules"][0]["macro_references"] == ["B"]
    return "RULES_AND_EDGES"


def normativity_is_inherited_once():
    data = b'<div class="note"><pre class="propdef informative">Name: x</pre><p>[[!CSS21]]</p></div>'
    tokens = scan(data)
    opening = next(row for row in tokens if row.kind == "start_tag" and row.name == "pre")
    assert inherited_state(opening)[0] == "NON_NORMATIVE"
    dependencies = dependency_occurrences("x", "x.bs", data, tokens)
    assert dependencies and dependencies[0]["source_state"] == "NON_NORMATIVE"
    stored_dependencies = observations()["dependencies"]["decisions"]
    assert sum(row[7] == "NON_NORMATIVE" and row[8] for row in stored_dependencies) >= 39
    casualties = [row for row in observations()["source_semantics"]["census"] if row[6] == "EXCLUDED_INHERITED_NORMATIVITY"]
    assert sum(len(row[7]) for row in casualties) >= 5
    return "NON_NORMATIVE"


def named_algorithms_enter_denominator():
    rows = observations()["source_semantics"]["census"]
    names = [row[8] for row in rows if row[1] == "named-algorithm" and len(row) > 8 and isinstance(row[8], str)]
    assert any("Grid Item Placement Algorithm".lower() in value.lower() for value in names)
    assert any("CSS property to IDL attribute".lower() in value.lower() for value in names)
    return "NAMED_ALGORITHM"


def wpt_rest_is_expanded():
    wpt = observations()["wpt"]
    assert wpt["directive_counts"].get("WPT_REST:DECLARED", 0) >= 1
    assert wpt["resolved_route_count"] > 0
    return "WPT_REST"


def unicode_authorities_block_per_occurrence():
    rows = [row for row in observations()["dependencies"]["decisions"] if row[5] in {"UNICODE", "ISO10646"}]
    assert rows and len({row[0] for row in rows}) == len(rows)
    assert any(row[9] == "BLOCKED_EXTERNAL_AUTHORITY_UNSEALED" for row in rows)
    return "BLOCKED_EXTERNAL_AUTHORITY_UNSEALED"


def local_blocker_cannot_be_laundered():
    mutated = copy.deepcopy(observations())
    mutated["claim_boundary"]["local_semantic"]["status"] = "VALID"
    return _code(lambda: pipeline.validate_claim_boundary(mutated))


def empty_root_replay_uses_no_private_path():
    document = observations()
    assert document["inputs"]["private_paths"] == 0
    for path in (ROOT / "inputs").glob("*.json"):
        assert b"/private/" not in path.read_bytes() and b"/Users/" not in path.read_bytes()
    return "NO_PRIVATE_PATH"


def crater_requires_resolved_graph_and_digests():
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        (root / "main.rs").write_text("fn main() {}")
        result = crater.inspect_candidate(root)
    assert any(row["code"] == "RESOLVED_GRAPH_MISSING" for row in result["violations"])
    return "RESOLVED_GRAPH_MISSING"


def final_receipt_must_match_manifest():
    subject = {"subject_sha256": "1" * 64, "file_count": 1, "total_bytes": 1}
    receipt = {
        "subject_definition": "file-manifest subject; manifest and this verification receipt excluded by declared self-reference rule",
        "manifest_sha256": "2" * 64,
        "subject_sha256": "0" * 64,
        "file_count": 1,
        "total_bytes": 1,
    }
    return _code(lambda: manifest.validate_receipt(receipt, subject, "2" * 64))


def intern_tables_retain_reverse_links():
    normalization = observations()["normalization"]
    assert normalization["plane_vector_count"] == 1 and normalization["terminal_decision_count"] == 1
    assert normalization["atomic_obligation_count"] == 8_796 and normalization["reverse_links"] > 0
    assert observations()["evidence_intern"]["blockers"]
    return "INTERNED"


FUNCTIONS = {
    name: value
    for name, value in globals().copy().items()
    if callable(value) and name in {row["test"] for row in mutations()["mutations"]}
}


def run_all() -> list[dict[str, str]]:
    results = []
    for row in mutations()["mutations"]:
        actual = FUNCTIONS[row["test"]]()
        if actual != row["expected"]:
            raise AssertionError(f"{row['id']} expected {row['expected']}, got {actual}")
        results.append({"id": row["id"], "test": row["test"], "result": actual})
    return results
