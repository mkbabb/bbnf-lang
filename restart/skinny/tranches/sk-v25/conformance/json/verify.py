"""Cache-clean offline verifier using two disposable subject roots."""

from __future__ import annotations

# This check deliberately precedes every assay or general-library import. The
# outer launcher supplies the full exact mode; direct optimized/bypass execution
# dies before governed subject bytes can load.
import sys as _early_sys

_PYTHON = (
    "/opt/homebrew/Cellar/python@3.14/3.14.5/Frameworks/Python.framework/"
    "Versions/3.14/Resources/Python.app/Contents/MacOS/Python"
)
if (
    _early_sys.executable != _PYTHON
    or _early_sys.flags.optimize != 0
    or _early_sys.flags.isolated != 1
    or not _early_sys.flags.safe_path
    or _early_sys.flags.no_user_site != 1
    or _early_sys.flags.dont_write_bytecode != 1
    or _early_sys.flags.utf8_mode != 1
    or not __debug__
):
    raise SystemExit("governed Python startup mismatch")

_early_sys.path.insert(0, __file__.rsplit("/", 1)[0] + "/src")

from json_oracle.governance import (  # noqa: E402
    BASE_ENVIRONMENT,
    ENV,
    GIT,
    NODE,
    OTOOL,
    PYTHON,
    SH,
    SW_VERS,
    child_environment,
    command_schema,
    enforce_verifier_startup,
    node_argv,
    python_module_argv,
    startup_description,
)

import argparse  # noqa: E402
import hashlib  # noqa: E402
import json  # noqa: E402
import os  # noqa: E402
from pathlib import Path  # noqa: E402
import platform  # noqa: E402
import shutil  # noqa: E402
import subprocess  # noqa: E402
import sys  # noqa: E402
import tempfile  # noqa: E402
import time  # noqa: E402


ROOT = Path(__file__).resolve().parent
enforce_verifier_startup(ROOT / "verify.py")
GOVERNANCE = {
    "constitution": "b45d1e6aeff51539b8a6320a2b5fa96d0df9adc932c683c29d81078e04bb09c5",
    "wave": "745b6e821842f843a9c0485111f1dfa79f1834473d4aab78ca7ac1c959756562",
}


def sha(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _closure(root: Path, entries: list[Path]) -> dict[str, object]:
    records: list[tuple[str, str, int]] = []
    byte_count = 0
    candidates: list[Path] = list(entries)
    for path in sorted(root.rglob("*")):
        relative = path.relative_to(root).as_posix()
        if path.is_symlink():
            records.append((f"link:{relative}", os.readlink(path), 0))
        elif path.is_file():
            size = path.stat().st_size
            records.append((f"file:{relative}", sha(path), size))
            byte_count += size
            if path.suffix in (".so", ".dylib") or os.access(path, os.X_OK):
                candidates.append(path)

    dependencies: dict[str, Path] = {}
    examined: set[Path] = set()
    while candidates:
        candidate = candidates.pop().resolve()
        if candidate in examined:
            continue
        examined.add(candidate)
        result = subprocess.run(
            [OTOOL, "-L", str(candidate)],
            capture_output=True,
            text=True,
            env=child_environment(),
        )
        if result.returncode:
            continue
        for line in result.stdout.splitlines()[1:]:
            dependency = line.strip().split(" (", 1)[0]
            if not dependency.startswith("/opt/homebrew/"):
                continue
            logical = Path(dependency)
            if not logical.exists():
                raise SystemExit(f"runtime dependency is missing: {logical}")
            resolved = logical.resolve()
            if not resolved.is_relative_to(root):
                dependencies[dependency] = resolved
                candidates.append(resolved)
    for logical, resolved in sorted(dependencies.items()):
        size = resolved.stat().st_size
        records.append((f"dependency:{logical}->{resolved}", sha(resolved), size))
        byte_count += size

    digest = hashlib.sha256()
    for name, identity, size in records:
        digest.update(f"{name}\0{identity}\0{size}\n".encode())
    return {
        "byte_count": byte_count,
        "dependency_count": len(dependencies),
        "record_count": len(records),
        "root": str(root),
        "root_sha256": digest.hexdigest(),
    }


def _executable_identity(path_text: str) -> dict[str, object]:
    path = Path(path_text)
    dynamic = subprocess.run(
        [OTOOL, "-L", str(path)],
        check=True,
        capture_output=True,
        env=child_environment(),
    ).stdout
    return {
        "dynamic_dependencies_sha256": hashlib.sha256(dynamic).hexdigest(),
        "path": str(path),
        "resolved": str(path.resolve()),
        "sha256": sha(path),
        "size": path.stat().st_size,
    }


def runtime_description() -> dict[str, object]:
    python = Path(PYTHON)
    python_root = python.parents[4]
    node = Path(NODE)
    node_root = node.parents[1]
    node_versions = json.loads(
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
    startup = startup_description()
    startup.pop("orig_argv")
    return {
        "commands": command_schema(),
        "executables": {
            name: _executable_identity(path)
            for name, path in (
                ("env", ENV),
                ("git", GIT),
                ("node", NODE),
                ("otool", OTOOL),
                ("python", PYTHON),
                ("sh", SH),
                ("sw_vers", SW_VERS),
            )
        },
        "macos": {
            "build": subprocess.run(
                [SW_VERS, "-buildVersion"],
                check=True,
                capture_output=True,
                text=True,
                env=child_environment(),
            ).stdout.strip(),
            "kernel": platform.release(),
            "version": subprocess.run(
                [SW_VERS, "-productVersion"],
                check=True,
                capture_output=True,
                text=True,
                env=child_environment(),
            ).stdout.strip(),
        },
        "node": {
            "closure": _closure(node_root, [node]),
            "identity": node_versions,
        },
        "python": {
            "closure": _closure(python_root, [python]),
            "identity": {
                "architecture": platform.machine(),
                "cache_tag": sys.implementation.cache_tag,
                "implementation": platform.python_implementation(),
                "version": platform.python_version(),
            },
            "startup": startup,
        },
        "revision": 2,
    }


def enforce_runtime() -> dict[str, object]:
    expected = json.loads((ROOT / "data/runtime-closure.json").read_text())
    actual = runtime_description()
    if actual != expected:
        raise SystemExit(
            "authoritative execution closure mismatch: "
            + json.dumps(actual, sort_keys=True, separators=(",", ":"))
        )
    return actual


def enforce_governance(constitution: Path, wave: Path) -> None:
    supplied = {"constitution": sha(constitution), "wave": sha(wave)}
    if supplied != GOVERNANCE:
        raise SystemExit(
            "governing input mismatch: "
            + json.dumps(supplied, sort_keys=True, separators=(",", ":"))
        )


def reject_caches(root: Path) -> None:
    caches = sorted(
        path.relative_to(root).as_posix()
        for path in root.rglob("*")
        if (path.is_dir() and path.name == "__pycache__") or path.suffix == ".pyc"
    )
    if caches:
        raise SystemExit(f"bytecode cache is forbidden: {caches[0]}")


def verify_records(root: Path, records: list[dict[str, object]]) -> None:
    for record in records:
        path = root / str(record["path"])
        if not path.is_file():
            raise SystemExit(f"manifest path missing: {path}")
        if path.stat().st_size != record["size"] or sha(path) != record["sha256"]:
            raise SystemExit(f"manifest mismatch: {path}")


def sealed_paths(root: Path) -> set[str]:
    return {
        path.relative_to(root).as_posix()
        for path in root.rglob("*")
        if path.is_file()
        and path.relative_to(root).as_posix() != "data/file-manifest.json"
        and not (path.parent == root and path.match("GESTALT-*.md"))
    }


def verify_live_manifests() -> int:
    specs = json.loads((ROOT / "data/spec-manifest.json").read_text())
    for document in specs["documents"]:
        path = ROOT / document["local_path"]
        if path.stat().st_size != document["size"] or sha(path) != document["sha256"]:
            raise SystemExit(f"cached specification mismatch: {path}")
    corpus = specs["independent_behavioral_corpus"]
    corpus_path = ROOT / corpus["local_path"]
    if corpus_path.stat().st_size != corpus["size"] or sha(corpus_path) != corpus["sha256"]:
        raise SystemExit("independent ECMAScript number corpus mismatch")

    source_manifest = json.loads((ROOT / "data/source-manifest.json").read_text())
    verify_records(
        ROOT,
        source_manifest["source"]
        + source_manifest["tests"]
        + source_manifest["verification"],
    )
    declared_source = {
        str(record["path"])
        for group in ("source", "tests", "verification")
        for record in source_manifest[group]
    }
    actual_source = {
        path.relative_to(ROOT).as_posix()
        for base in (ROOT / "src", ROOT / "tests")
        for path in base.rglob("*.py")
    } | {"verify", "verify.py"}
    if declared_source != actual_source:
        raise SystemExit("source manifest path set is stale")

    file_manifest = json.loads((ROOT / "data/file-manifest.json").read_text())
    verify_records(ROOT, file_manifest["files"])
    declared_files = {str(record["path"]) for record in file_manifest["files"]}
    actual_files = sealed_paths(ROOT)
    if declared_files != actual_files or file_manifest["file_count"] != len(actual_files):
        raise SystemExit("file manifest path set is stale")

    actual_reviews = {
        path.name for path in ROOT.glob("GESTALT-*.md") if path.is_file()
    }
    return len(actual_reviews)


def verify_number_producer() -> dict[str, object]:
    import struct

    from json_oracle.projection import _number

    corpus_path = ROOT / "data/ecmascript-number-corpus.json"
    corpus = json.loads(corpus_path.read_text())
    producer = corpus["producer"]
    program = ROOT / producer["program"]
    if sha(program) != producer["program_sha256"]:
        raise SystemExit("ECMAScript producer program mismatch")
    if producer["executable"] != NODE:
        raise SystemExit("ECMAScript producer executable mismatch")
    base = node_argv("--no-warnings", str(program))
    corpus_output = subprocess.run(
        base + ["corpus"],
        check=True,
        capture_output=True,
        env=child_environment(),
    ).stdout
    if hashlib.sha256(corpus_output).hexdigest() != producer["corpus_output_sha256"]:
        raise SystemExit("ECMAScript corpus producer digest mismatch")
    rows = [json.loads(line) for line in corpus_output.splitlines()]
    if rows != corpus["cases"]:
        raise SystemExit("ECMAScript corpus producer rows differ")

    differential = producer["differential"]
    output = subprocess.run(
        base
        + [
            "differential",
            differential["seed"],
            str(differential["count"]),
        ],
        check=True,
        capture_output=True,
        env=child_environment(),
    ).stdout
    if hashlib.sha256(output).hexdigest() != differential["output_sha256"]:
        raise SystemExit("ECMAScript differential producer digest mismatch")
    seen: set[str] = set()
    for line in output.splitlines():
        row = json.loads(line)
        bits = row["binary64_hex"]
        if bits in seen:
            raise SystemExit("ECMAScript differential contains duplicate binary64")
        seen.add(bits)
        value = struct.unpack(">d", bytes.fromhex(bits))[0]
        if _number(repr(value), 0) != row["json_stringify"]:
            raise SystemExit(f"ECMAScript differential mismatch at {bits}")
    if len(seen) != differential["count"]:
        raise SystemExit("ECMAScript differential count mismatch")
    return {
        "corpus_rows": len(rows),
        "differential_rows": len(seen),
        "differential_sha256": hashlib.sha256(output).hexdigest(),
    }


def command(arguments: list[str], root: Path, environment: dict[str, str]) -> float:
    started = time.monotonic()
    subprocess.run(arguments, cwd=root, env=environment, check=True)
    return time.monotonic() - started


def compare_trees(first: Path, second: Path, label: str) -> None:
    first_paths = {
        path.relative_to(first).as_posix() for path in first.rglob("*") if path.is_file()
    }
    second_paths = {
        path.relative_to(second).as_posix() for path in second.rglob("*") if path.is_file()
    }
    if first_paths != second_paths:
        raise SystemExit(f"{label} path sets differ")
    for relative in first_paths:
        if (first / relative).read_bytes() != (second / relative).read_bytes():
            raise SystemExit(f"{label} bytes differ: {relative}")


def copy_subject(destination: Path) -> None:
    def ignore(directory: str, names: list[str]) -> set[str]:
        ignored = {
            name for name in names if name == "__pycache__" or name.endswith(".pyc")
        }
        if Path(directory).resolve() == ROOT:
            ignored.update(name for name in names if Path(name).match("GESTALT-*.md"))
        return ignored

    shutil.copytree(ROOT, destination, ignore=ignore)
    reject_caches(destination)


def replay(
    root: Path,
    suite: Path,
    generated: Path,
    fixtures: Path,
    pass_number: int,
    timings: list[dict[str, object]],
) -> None:
    environment = child_environment({"JSON_TEST_SUITE": str(suite)})
    tests = python_module_argv(
        root, "unittest", "discover", "-s", "tests", "-t", ".", "-v"
    )
    timings.append(
        {"phase": f"tests-{pass_number}", "seconds": command(tests, root, environment)}
    )
    suite_command = python_module_argv(
        root,
        "json_oracle.generate",
        "--suite",
        str(suite),
        "--output",
        str(generated),
    )
    timings.append(
        {
            "phase": f"suite-regeneration-{pass_number}",
            "seconds": command(suite_command, root, environment),
        }
    )
    for name in ("json-test-suite-manifest.json", "expected-results.jsonl"):
        if (generated / name).read_bytes() != (root / "data" / name).read_bytes():
            raise SystemExit(f"checked-in suite data is stale: {name}")

    number_command = python_module_argv(
        root,
        "json_oracle.generate",
        "--numbers",
        "--output",
        str(generated),
    )
    timings.append(
        {
            "phase": f"number-regeneration-{pass_number}",
            "seconds": command(number_command, root, environment),
        }
    )
    if (generated / "ecmascript-number-corpus.json").read_bytes() != (
        root / "data/ecmascript-number-corpus.json"
    ).read_bytes():
        raise SystemExit("checked-in ECMAScript number corpus is stale")

    fixture_command = python_module_argv(
        root,
        "json_oracle.generate",
        "--fixtures",
        "--output",
        str(fixtures),
    )
    timings.append(
        {
            "phase": f"fixture-regeneration-{pass_number}",
            "seconds": command(fixture_command, root, environment),
        }
    )
    compare_trees(fixtures, root / "fixtures", "fixture regeneration")

    source_before = (root / "data/source-manifest.json").read_bytes()
    files_before = (root / "data/file-manifest.json").read_bytes()
    manifest_command = python_module_argv(
        root,
        "json_oracle.generate",
        "--manifests",
        "--output",
        str(root),
    )
    timings.append(
        {
            "phase": f"manifest-regeneration-{pass_number}",
            "seconds": command(manifest_command, root, environment),
        }
    )
    if (root / "data/source-manifest.json").read_bytes() != source_before:
        raise SystemExit("source manifest regeneration is stale")
    if (root / "data/file-manifest.json").read_bytes() != files_before:
        raise SystemExit("file manifest regeneration is stale")
    reject_caches(root)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--suite", type=Path)
    parser.add_argument("--constitution", type=Path)
    parser.add_argument("--wave", type=Path)
    parser.add_argument("--describe-runtime", action="store_true")
    arguments = parser.parse_args()
    if arguments.describe_runtime:
        print(json.dumps(runtime_description(), indent=2, sort_keys=True))
        return 0
    if not arguments.suite or not arguments.constitution or not arguments.wave:
        parser.error("--suite, --constitution, and --wave are required")
    enforce_governance(arguments.constitution.resolve(), arguments.wave.resolve())
    runtime = enforce_runtime()
    reject_caches(ROOT)
    review_count = verify_live_manifests()
    number_proof = verify_number_producer()
    suite = arguments.suite.resolve()
    timings: list[dict[str, object]] = []
    with tempfile.TemporaryDirectory(prefix="json-w0-verify-") as directory:
        temporary = Path(directory)
        roots = [temporary / "root-a", temporary / "root-b"]
        generated = [temporary / "suite-a", temporary / "suite-b"]
        fixtures = [temporary / "fixtures-a", temporary / "fixtures-b"]
        for root in roots:
            copy_subject(root)
        for index in range(2):
            replay(roots[index], suite, generated[index], fixtures[index], index + 1, timings)
        compare_trees(generated[0], generated[1], "suite regenerations")
        compare_trees(fixtures[0], fixtures[1], "fixture regenerations")
    reject_caches(ROOT)
    summary = {
        "expected_rows": 318,
        "invocation": {
            "argv": list(sys.argv),
            "environment": dict(sorted(BASE_ENVIRONMENT.items())),
            "executable": PYTHON,
            "launcher_sha256": sha(ROOT / "verify"),
            "orig_argv": list(sys.orig_argv),
            "runtime_manifest_sha256": sha(ROOT / "data/runtime-closure.json"),
            "verify_sha256": sha(ROOT / "verify.py"),
        },
        "network": "unused",
        "number_proof": number_proof,
        "passes": 2,
        "runtime_closure_sha256": {
            name: runtime[name]["closure"]["root_sha256"]
            for name in ("python", "node")
        },
        "review_reports_observed": review_count,
        "sealed_root_sha256": sha(ROOT / "data/file-manifest.json"),
        "source_manifest_sha256": sha(ROOT / "data/source-manifest.json"),
        "timings": timings,
    }
    print(json.dumps(summary, sort_keys=True, separators=(",", ":")))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
