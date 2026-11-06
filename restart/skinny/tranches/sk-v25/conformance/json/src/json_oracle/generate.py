"""Deterministic manifests and fixtures for the pinned external corpus."""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
from pathlib import Path
from typing import Any

from .governance import GIT, NODE, child_environment, node_argv
from .parser import Budgets, OracleError, observe, parse
from .projection import ProjectionError, canonicalize

HEAD = "1ef36fa01286573e846ac449e8683f8833c5b26a"
TREE = "b936f9acdd24b9f5fefe68b90b9beab2c681137a"
ROOT = Path(__file__).resolve().parents[2]
NUMBER_AUTHORITY_SCOPE = (
    "Independent behavioral corpus only; it does not cache or replace the delegated "
    "ECMA-262, IEEE 754, or Unicode publications."
)
NUMBER_FOCUS = (
    "Five adjacent binary64 encodings on both signs at JCS fixed/scientific "
    "boundaries, range endpoints, and RFC 8785 rounding examples."
)
NUMBER_PROGRAM = "data/producers/ecmascript-numbers.mjs"
NUMBER_DIFFERENTIAL_SEED = "0x878520260719"
NUMBER_DIFFERENTIAL_COUNT = 100_000
NUMBER_UNIQUENESS = "distinct finite binary64 bit patterns"


def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def git(repository: Path, *arguments: str) -> str:
    return subprocess.run(
        [GIT, "-C", str(repository), *arguments],
        check=True,
        capture_output=True,
        text=True,
        env=child_environment(),
    ).stdout.strip()


def dump(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(value, ensure_ascii=True, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


def _base(source: bytes) -> tuple[dict[str, Any], Any | None]:
    try:
        document = parse(source)
    except OracleError as error:
        return error.observation(), None
    return json.loads(observe(document)), document


def _jcs(document: Any | None) -> dict[str, Any]:
    if document is None:
        return {"status": "not_applicable", "reason": "base_rejected"}
    try:
        canonical = canonicalize(document)
    except ProjectionError as error:
        return error.observation()
    return {
        "status": "accepted",
        "sha256": digest(canonical),
        "utf8": canonical.decode("utf-8"),
    }


def generate_suite(suite: Path, output: Path) -> None:
    if git(suite, "rev-parse", "HEAD") != HEAD:
        raise SystemExit("JSONTestSuite HEAD does not match the frozen pin")
    if git(suite, "rev-parse", "HEAD:test_parsing") != TREE:
        raise SystemExit("JSONTestSuite test_parsing tree does not match the frozen pin")
    if git(suite, "status", "--short"):
        raise SystemExit("JSONTestSuite worktree is not clean")
    files = sorted((suite / "test_parsing").glob("*.json"))
    if len(files) != 318:
        raise SystemExit(f"expected 318 test_parsing files, found {len(files)}")
    policies = {
        row["file"]: row
        for row in json.loads((ROOT / "data/i-case-policy.json").read_text())["cases"]
    }
    if set(policies) != {path.name for path in files if path.name.startswith("i_")}:
        raise SystemExit("i-case policy names do not exactly match the pinned i-files")
    manifest_files = []
    expected_rows = []
    labels = {"i": 0, "n": 0, "y": 0}
    for path in files:
        source = path.read_bytes()
        label = path.name[0]
        labels[label] += 1
        base, document = _base(source)
        contract = (
            base["status"] == "accepted"
            if label == "y"
            else base["status"] == "rejected"
            if label == "n"
            else base["status"] == policies[path.name]["base"]
        )
        if not contract:
            raise SystemExit(f"declared result mismatch for {path.name}: {base}")
        manifest_files.append(
            {"label": label, "path": f"test_parsing/{path.name}", "sha256": digest(source), "size": len(source)}
        )
        row = {
            "base": base,
            "file": path.name,
            "input_sha256": digest(source),
            "jcs": _jcs(document),
            "suite_label": label,
            "suite_or_declared_contract_met": contract,
        }
        if label == "i":
            row["i_policy"] = policies[path.name]["policy"]
        expected_rows.append(row)
    suite_manifest = {
        "file_count": len(files),
        "files": manifest_files,
        "head": HEAD,
        "labels": labels,
        "license": {
            "path": "LICENSE",
            "sha256": digest((suite / "LICENSE").read_bytes()),
            "spdx": "MIT",
        },
        "origin": "https://github.com/nst/JSONTestSuite",
        "readme_sha256": digest((suite / "README.md").read_bytes()),
        "test_parsing_tree": TREE,
    }
    dump(output / "json-test-suite-manifest.json", suite_manifest)
    (output / "expected-results.jsonl").write_text(
        "".join(
            json.dumps(row, ensure_ascii=True, sort_keys=True, separators=(",", ":")) + "\n"
            for row in expected_rows
        ),
        encoding="utf-8",
    )


def generate_fixtures(output: Path) -> None:
    fixtures = {
        "positive/source-facts.json": b' { "a" : 1e+02, "a":"\\uD834\\uDD1E", "z":[true,null] }\n',
        "positive/jcs-sort.json": '{"\\u20ac":1,"\\r":2,"\\ud83d\\ude00":3,"1":4}'.encode(),
        "negative/bom.json": b"\xef\xbb\xbf{}",
        "negative/invalid-utf8.json": b'["\x80"]',
        "negative/trailing.json": b"true false",
        "negative/bad-escape.json": b'"\\x"',
        "resource/depth-500.json": b"[" * 500 + b"]" * 500,
        "resource/depth-501.json": b"[" * 501 + b"]" * 501,
        "resource/node-budget.json": b"[0,1,2,3,4,5]",
        "resource/input-budget.json": b'"1234567890"',
        "mutation/duplicate-escape.json": b'{"a":1,"\\u0061":2}',
        "mutation/exact-number.json": b"1e400",
        "mutation/lone-surrogate.json": b'"\\uDEAD"',
        "mutation/utf8-span.json": ' ["é",{"𝄞":0}] '.encode(),
        "query/nested.json": b'{"items":[{"id":1},{"id":2}],"items":[]}',
    }
    rows = []
    for relative, source in sorted(fixtures.items()):
        path = output / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(source)
        base, document = _base(source)
        rows.append(
            {
                "base": base,
                "jcs": _jcs(document),
                "path": relative,
                "sha256": digest(source),
                "size": len(source),
            }
        )
    custom = [
        ("resource/node-budget.json", Budgets(max_nodes=5)),
        ("resource/input-budget.json", Budgets(max_input_bytes=8)),
    ]
    for relative, budgets in custom:
        try:
            parse(fixtures[relative], budgets)
            result = {"status": "accepted"}
        except OracleError as error:
            result = error.observation()
        next(row for row in rows if row["path"] == relative)["custom_budget"] = result
    dump(output / "manifest.json", {"fixtures": rows, "revision": 1})


def generate_numbers(output: Path) -> None:
    runtime = json.loads((ROOT / "data/runtime-closure.json").read_text())
    expected_identity = runtime["node"]["identity"]
    executable = Path(runtime["node"]["closure"]["root"]) / "bin/node"
    if str(executable) != NODE:
        raise SystemExit("ECMAScript number producer executable mismatch")
    actual_identity = json.loads(
        subprocess.run(
            node_argv(
                "-p",
                "JSON.stringify({node:process.version,v8:process.versions.v8,arch:process.arch,platform:process.platform})",
            ),
            check=True,
            capture_output=True,
            text=True,
            env=child_environment(),
        ).stdout
    )
    if actual_identity != expected_identity:
        raise SystemExit("ECMAScript number producer runtime mismatch")
    program = ROOT / NUMBER_PROGRAM
    base = node_argv("--no-warnings", str(program))
    generated = subprocess.run(
        base + ["corpus"],
        check=True,
        capture_output=True,
        env=child_environment(),
    ).stdout
    differential = subprocess.run(
        base
        + [
            "differential",
            NUMBER_DIFFERENTIAL_SEED,
            str(NUMBER_DIFFERENTIAL_COUNT),
        ],
        check=True,
        capture_output=True,
        env=child_environment(),
    ).stdout
    document = {
        "authority_scope": NUMBER_AUTHORITY_SCOPE,
        "cases": [json.loads(line) for line in generated.splitlines()],
        "focus": NUMBER_FOCUS,
        "producer": {
            "architecture": actual_identity["arch"],
            "binary_sha256": digest(executable.read_bytes()),
            "command": f"node --no-warnings {NUMBER_PROGRAM} corpus",
            "corpus_output_sha256": digest(generated),
            "differential": {
                "command": (
                    f"node --no-warnings {NUMBER_PROGRAM} differential "
                    f"{NUMBER_DIFFERENTIAL_SEED} {NUMBER_DIFFERENTIAL_COUNT}"
                ),
                "count": NUMBER_DIFFERENTIAL_COUNT,
                "output_sha256": digest(differential),
                "seed": NUMBER_DIFFERENTIAL_SEED,
                "uniqueness": NUMBER_UNIQUENESS,
            },
            "executable": str(executable),
            "node": actual_identity["node"],
            "platform": actual_identity["platform"],
            "program": NUMBER_PROGRAM,
            "program_sha256": digest(program.read_bytes()),
            "v8": actual_identity["v8"],
        },
        "revision": 2,
    }
    dump(output / "ecmascript-number-corpus.json", document)


def _record(root: Path, path: Path) -> dict[str, Any]:
    data = path.read_bytes()
    return {"path": path.relative_to(root).as_posix(), "sha256": digest(data), "size": len(data)}


def generate_manifests(root: Path) -> None:
    caches = [
        path
        for path in root.rglob("*")
        if (path.is_dir() and path.name == "__pycache__") or path.suffix == ".pyc"
    ]
    if caches:
        raise SystemExit("bytecode cache exists in assay root")
    source_paths = sorted((root / "src").rglob("*.py"))
    test_paths = sorted((root / "tests").rglob("*.py"))
    source = {
        "revision": 3,
        "source": [_record(root, path) for path in source_paths],
        "tests": [_record(root, path) for path in test_paths],
        "verification": [
            _record(root, root / "verify"),
            _record(root, root / "verify.py"),
        ],
    }
    dump(root / "data/source-manifest.json", source)
    excluded = {"data/file-manifest.json"}
    paths = sorted(
        path
        for path in root.rglob("*")
        if path.is_file()
        and path.relative_to(root).as_posix() not in excluded
        and not (path.parent == root and path.match("GESTALT-*.md"))
    )
    dump(
        root / "data/file-manifest.json",
        {
            "excluded": [
                "data/file-manifest.json (self-reference)",
                "GESTALT-*.md (root review namespace; externally anchored per pass)",
            ],
            "file_count": len(paths),
            "files": [_record(root, path) for path in paths],
            "revision": 3,
        },
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--suite", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--fixtures", action="store_true")
    parser.add_argument("--manifests", action="store_true")
    parser.add_argument("--numbers", action="store_true")
    arguments = parser.parse_args()
    if arguments.manifests:
        generate_manifests(arguments.output.resolve())
    elif arguments.numbers:
        generate_numbers(arguments.output.resolve())
    elif arguments.fixtures:
        generate_fixtures(arguments.output)
    elif arguments.suite:
        generate_suite(arguments.suite.resolve(), arguments.output.resolve())
    else:
        parser.error("--suite is required unless --fixtures is selected")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
