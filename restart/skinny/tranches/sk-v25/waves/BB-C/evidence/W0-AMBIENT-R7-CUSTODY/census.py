#!/usr/bin/env python3
"""Deterministic BB-C.W0 R7 custody census; no build or code execution."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import stat
import tomllib


FAMILIES = ("bbnf", "json", "css_l4", "css_pretty", "google_sheets", "ebnf", "bnf", "csv", "math")
DRIFT_FAMILIES = ("json", "css_l4", "css_pretty", "google_sheets")
ROOT_INPUTS = ("Cargo.toml", "Cargo.lock", "rust-toolchain.toml", ".cargo/config.toml")
EXTERNAL_PACKAGES = (
    "parse-that/rust/parse_that",
    "parse-that/rust/regex",
    "pprint/rust",
    "pprint/rust/derive",
)
PROJECTION_OWNERS = (
    "xtask/src/lib.rs",
    "xtask/src/main.rs",
    "xtask/src/regen_simple_runtime.rs",
    "xtask/src/regen_css.rs",
)


def digest(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as stream:
        while chunk := stream.read(1024 * 1024):
            h.update(chunk)
    return h.hexdigest()


def require_regular(path: Path) -> Path:
    if path.is_symlink() or not path.is_file():
        raise SystemExit(f"RED: required regular file missing or symlinked: {path}")
    return path.resolve()


def recursive_regular(root: Path, suffixes: tuple[str, ...] | None = None) -> list[Path]:
    if not root.is_dir():
        raise SystemExit(f"RED: required directory missing: {root}")
    files = []
    for path in root.rglob("*"):
        if path.is_symlink():
            raise SystemExit(f"RED: symlink in governed tree: {path}")
        if path.is_file() and (suffixes is None or path.suffix in suffixes):
            files.append(path.resolve())
    return files


def label(path: Path, workspace: Path, programming: Path) -> str:
    try:
        rel = path.relative_to(workspace)
        value = Path("bbnf-lang") / rel
    except ValueError:
        value = path.relative_to(programming)
    text = value.as_posix()
    if "\t" in text or "\n" in text or "\r" in text:
        raise SystemExit(f"RED: non-ledger-safe path: {text!r}")
    return text


def rows(paths: list[Path], workspace: Path, programming: Path) -> list[dict[str, object]]:
    unique = {path.resolve() for path in paths}
    result = []
    for path in unique:
        info = path.stat()
        if not stat.S_ISREG(info.st_mode):
            raise SystemExit(f"RED: non-regular census member: {path}")
        result.append(
            {
                "path": label(path, workspace, programming),
                "mode": f"{stat.S_IMODE(info.st_mode):04o}",
                "bytes": info.st_size,
                "sha256": digest(path),
                "source": path,
            }
        )
    result.sort(key=lambda row: str(row["path"]).encode("utf-8"))
    return result


def write_ledger(path: Path, members: list[dict[str, object]]) -> dict[str, object]:
    data = "path\tmode\tbytes\tsha256\n" + "".join(
        f'{row["path"]}\t{row["mode"]}\t{row["bytes"]}\t{row["sha256"]}\n'
        for row in members
    )
    path.write_text(data, encoding="utf-8", newline="\n")
    path_data = "".join(f'{row["path"]}\n' for row in members)
    path_file = path.with_name(path.stem + "-PATHS.txt")
    path_file.write_text(path_data, encoding="utf-8", newline="\n")
    return {
        "members": len(members),
        "bytes": sum(int(row["bytes"]) for row in members),
        "path_set_sha256": hashlib.sha256(path_data.encode()).hexdigest(),
        "ledger_sha256": hashlib.sha256(data.encode()).hexdigest(),
        "ledger_file_sha256": digest(path),
        "path_file_sha256": digest(path_file),
    }


def collect_build_inputs(workspace: Path, programming: Path) -> list[Path]:
    paths = [require_regular(workspace / item) for item in ROOT_INPUTS]
    manifest = tomllib.loads((workspace / "Cargo.toml").read_text(encoding="utf-8"))
    for member in manifest["workspace"]["members"]:
        root = workspace / member
        paths.append(require_regular(root / "Cargo.toml"))
        build = root / "build.rs"
        if build.exists():
            paths.append(require_regular(build))
        paths.extend(recursive_regular(root / "src"))

    # All grammar modules and rewrite rules are runtime inputs to the generator.
    paths.extend(recursive_regular(workspace / "grammar", (".bbnf", ".ron")))

    # Runtime projection descriptions are inputs to the R7 atomic projection cone.
    paths.extend(recursive_regular(workspace / "xtask/runtime-projections", (".toml",)))

    # Path-patched packages selected by .cargo/config.toml.
    paths.append(require_regular(programming / "parse-that/rust/Cargo.toml"))
    for rel in EXTERNAL_PACKAGES:
        root = programming / rel
        package_manifest = require_regular(root / "Cargo.toml")
        paths.append(package_manifest)
        package = tomllib.loads(package_manifest.read_text(encoding="utf-8"))
        source_root = root / "src"
        if source_root.is_dir():
            paths.extend(recursive_regular(source_root))
        build = root / "build.rs"
        if build.exists():
            paths.append(require_regular(build))
        explicit_lib = package.get("lib", {}).get("path")
        if explicit_lib:
            paths.append(require_regular(root / explicit_lib))
    return paths


def collect_projection_files(workspace: Path) -> list[Path]:
    paths = [require_regular(workspace / rel) for rel in PROJECTION_OWNERS]
    for family in DRIFT_FAMILIES:
        paths.append(require_regular(workspace / f"xtask/runtime-projections/{family}.toml"))
        paths.append(require_regular(workspace / f"crates/core/src/grammar/generated/{family}.registry.json"))
        paths.extend(recursive_regular(workspace / f"crates/core/src/runtime/{family}"))
    return paths


def fleet_paths(root: Path) -> list[Path]:
    expected = {f"{family}{suffix}" for family in FAMILIES for suffix in (".rs", ".registry.json")}
    actual = {path.name for path in root.iterdir() if path.is_file() and not path.is_symlink()}
    if actual != expected:
        missing = sorted(expected - actual)
        extra = sorted(actual - expected)
        raise SystemExit(f"RED: fleet set mismatch: missing={missing}, extra={extra}")
    return [require_regular(root / name) for name in sorted(expected, key=lambda item: item.encode())]


def local_rows(paths: list[Path], root: Path) -> list[dict[str, object]]:
    result = []
    for path in paths:
        info = path.stat()
        result.append(
            {
                "path": path.relative_to(root).as_posix(),
                "mode": f"{stat.S_IMODE(info.st_mode):04o}",
                "bytes": info.st_size,
                "sha256": digest(path),
                "source": path,
            }
        )
    result.sort(key=lambda row: str(row["path"]).encode())
    return result


def copy_snapshot(members: list[dict[str, object]], destination: Path) -> None:
    if destination.exists():
        shutil.rmtree(destination)
    for row in members:
        target = destination / str(row["path"])
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(Path(row["source"]), target)


def public(summary: dict[str, object]) -> dict[str, object]:
    return {key: value for key, value in summary.items() if key != "source"}


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--workspace", type=Path, required=True)
    parser.add_argument("--sealed-fleet", type=Path, required=True)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()

    workspace = args.workspace.resolve()
    programming = workspace.parent
    out = args.out.resolve()
    out.mkdir(parents=True, exist_ok=True)

    build = rows(collect_build_inputs(workspace, programming), workspace, programming)
    build_summary = write_ledger(out / "BUILD-INPUTS.tsv", build)

    projection = rows(collect_projection_files(workspace), workspace, programming)
    projection_summary = write_ledger(out / "RUNTIME-PROJECTION.tsv", projection)
    copy_snapshot(projection, out / "runtime-projection-snapshot")

    source_fleet = local_rows(fleet_paths(args.sealed_fleet.resolve()), args.sealed_fleet.resolve())
    custody_root = out / "sealed-fleet"
    custody_fleet = local_rows(fleet_paths(custody_root), custody_root)
    candidate_root = args.candidate.resolve()
    candidate_fleet = local_rows(fleet_paths(candidate_root), candidate_root)
    source_public = [{key: value for key, value in row.items() if key != "source"} for row in source_fleet]
    custody_public = [{key: value for key, value in row.items() if key != "source"} for row in custody_fleet]
    candidate_public = [{key: value for key, value in row.items() if key != "source"} for row in candidate_fleet]
    if source_public != custody_public or source_public != candidate_public:
        raise SystemExit("RED: source, custody, and candidate fleet ledgers differ")
    fleet_summary = write_ledger(out / "SEALED-FLEET.tsv", source_fleet)
    candidate_summary = write_ledger(out / "ATOMIC-CANDIDATE.tsv", candidate_fleet)

    receipt = {
        "schema": "bb-c.w0.r7-custody.v1",
        "algorithm": "census.py",
        "algorithm_sha256": digest(Path(__file__).resolve()),
        "workspace": str(workspace),
        "sealed_fleet_source": str(args.sealed_fleet.resolve()),
        "atomic_candidate_root": str(candidate_root),
        "build_inputs": build_summary,
        "runtime_projection": projection_summary,
        "sealed_fleet": fleet_summary,
        "atomic_candidate": candidate_summary,
        "source_custody_candidate_equal": True,
    }
    (out / "CENSUS.json").write_text(
        json.dumps(receipt, indent=2, sort_keys=True) + "\n", encoding="utf-8", newline="\n"
    )


if __name__ == "__main__":
    main()
