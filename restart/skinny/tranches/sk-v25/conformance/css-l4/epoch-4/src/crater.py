"""Resolved-input crater for future candidates; current candidate count is zero."""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any

from common import INPUTS, encode, fail, load, sha256


FORBIDDEN_TERMS = (
    "semantic-matrix", "module-closure", "matrix.json", "modules.json",
    "bbnf-expectation", "owner-table", "owner_table", "generated-schema",
    "conformance/css-l4/epoch-",
)
SOURCE_SUFFIXES = {".rs", ".ts", ".tsx", ".js", ".mjs", ".cjs", ".py", ".toml", ".json", ".bbnf", ".sh", ".yaml", ".yml"}
PATH_LITERAL = re.compile(r"['\"]([^'\"\n]*(?:/|\\)[^'\"\n]*)['\"]")
CONCAT = re.compile(r"concat!\s*\((.*?)\)", re.S)
STRING_LITERAL = re.compile(r"['\"]([^'\"]*)['\"]")


def capture() -> dict[str, Any]:
    policy = load(INPUTS / "policy.json")
    return {
        "schema_version": 4,
        "stage": "candidate-product-boundary",
        "status": "NO_CANDIDATE_PRESENT",
        "candidate_count": 0,
        "complete_candidate_count": 0,
        "allowed_external_prefixes": policy["candidate_allowed_prefixes"],
        "allowed_observation_artifact": None,
        "future_requirement": "RESOLVED_SOURCE_MACRO_BUILD_PACKAGE_GENERATED_BINARY_SYMLINK_RUNTIME_GRAPH_WITH_EXACT_DIGESTS",
    }


def _strings(text: str) -> list[str]:
    values = [match.group(1) for match in PATH_LITERAL.finditer(text)]
    for match in CONCAT.finditer(text):
        pieces = STRING_LITERAL.findall(match.group(1))
        if pieces:
            values.append("".join(pieces))
    return values


def inspect_candidate(root: Path) -> dict[str, Any]:
    if not root.is_dir():
        fail("CANDIDATE_ROOT_MISSING", "candidate crater root is absent", root=str(root))
    policy = load(INPUTS / "policy.json")
    allowed_prefixes = tuple(policy["candidate_allowed_prefixes"])
    trace_path = root / "resolved-inputs.json"
    trace: dict[str, Any] = {}
    violations: list[dict[str, Any]] = []
    if trace_path.is_file():
        try:
            trace = json.loads(trace_path.read_text())
        except json.JSONDecodeError as error:
            violations.append({"code": "RESOLVED_GRAPH_INVALID", "detail": str(error)})
    else:
        violations.append({"code": "RESOLVED_GRAPH_MISSING"})
    allowed_digests = trace.get("allowed_external_inputs", {}) if isinstance(trace, dict) else {}
    runtime_reads = trace.get("runtime_reads", []) if isinstance(trace, dict) else []
    if not isinstance(allowed_digests, dict) or not isinstance(runtime_reads, list):
        violations.append({"code": "RESOLVED_GRAPH_SHAPE"})
        allowed_digests, runtime_reads = {}, []

    nodes: list[list[Any]] = []
    edges: list[list[str]] = []
    for path in sorted(root.rglob("*"), key=lambda item: item.relative_to(root).as_posix()):
        relative = path.relative_to(root).as_posix()
        if path.is_symlink():
            target = path.resolve(strict=False)
            nodes.append([relative, "symlink", str(path.readlink())])
            edges.append([relative, "symlink", str(target)])
            try:
                target.relative_to(root.resolve())
            except ValueError:
                violations.append({"code": "SYMLINK_ESCAPES_CANDIDATE", "path": relative, "target": str(path.readlink())})
            continue
        if not path.is_file():
            continue
        data = path.read_bytes()
        digest = sha256(data)
        kind = "source" if path.suffix.lower() in SOURCE_SUFFIXES else "binary-or-generated"
        nodes.append([relative, kind, len(data), digest])
        lowered_bytes = data.lower()
        for term in FORBIDDEN_TERMS:
            if term.encode() in lowered_bytes:
                violations.append({"code": "FORBIDDEN_ORACLE_AUTHORITY", "path": relative, "term": term})
        try:
            text = data.decode("utf-8")
        except UnicodeDecodeError:
            text = ""
        if text:
            lowered = text.lower()
            for term in FORBIDDEN_TERMS:
                if term in lowered:
                    violations.append({"code": "FORBIDDEN_ORACLE_AUTHORITY", "path": relative, "term": term})
            for literal in _strings(text):
                normalized = literal.replace("\\", "/").lstrip("./")
                edges.append([relative, "literal-input", normalized])
                if "conformance/css-l4" in normalized:
                    violations.append({"code": "OBSERVATION_ARTIFACT_FORBIDDEN", "path": relative, "input": normalized})
                elif normalized.startswith(allowed_prefixes):
                    expected = allowed_digests.get(normalized)
                    candidate = root / normalized
                    if not isinstance(expected, str) or re.fullmatch(r"[0-9a-f]{64}", expected) is None:
                        violations.append({"code": "ALLOWED_INPUT_DIGEST_MISSING", "path": relative, "input": normalized})
                    elif not candidate.is_file() or sha256(candidate.read_bytes()) != expected:
                        violations.append({"code": "ALLOWED_INPUT_DIGEST_DRIFT", "path": relative, "input": normalized})
    for value in runtime_reads:
        if not isinstance(value, dict) or not isinstance(value.get("path"), str) or re.fullmatch(r"[0-9a-f]{64}", str(value.get("sha256"))) is None:
            violations.append({"code": "RUNTIME_READ_RECEIPT_INVALID"})
            continue
        edges.append(["@runtime", value.get("kind", "read"), value["path"]])
        if "conformance/css-l4" in value["path"]:
            violations.append({"code": "RUNTIME_ORACLE_READ_FORBIDDEN", "input": value["path"]})
    graph = {"nodes": nodes, "edges": sorted(edges)}
    return {
        "status": "FAIL" if violations else "VALID",
        "graph_sha256": sha256(encode(graph)),
        "node_count": len(nodes),
        "edge_count": len(edges),
        "violations": violations,
    }
