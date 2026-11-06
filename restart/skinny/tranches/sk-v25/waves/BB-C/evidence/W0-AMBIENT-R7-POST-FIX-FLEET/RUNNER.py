#!/usr/bin/env python3
"""R7 post-fix Apple-ARM fleet and runtime evidence runner.

The runner is deliberately split into explicit phases.  Each phase records raw
stdout/stderr, duration, exit status, and content identities; a failing phase
stops without retry.  Product/source files in the live worktree are read only.
"""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import tempfile
import time


WORKSPACE = Path("/Users/mkbabb/Programming/bbnf-lang")
PROGRAMMING = WORKSPACE.parent
EVIDENCE = Path(__file__).resolve().parent
CENSUS_SOURCE = (
    WORKSPACE
    / "restart/skinny/tranches/sk-v25/waves/BB-C/evidence"
    / "W0-AMBIENT-R7-CUSTODY/census.py"
)
FAMILIES = (
    "bbnf",
    "bnf",
    "css_l4",
    "css_pretty",
    "csv",
    "ebnf",
    "google_sheets",
    "json",
    "math",
)
RUNTIME_FAMILIES = ("json", "css_pretty", "google_sheets", "css_l4")


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        while chunk := stream.read(1024 * 1024):
            digest.update(chunk)
    return digest.hexdigest()


def write_bytes(path: Path, data: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(data)


def write_json(path: Path, value: object) -> None:
    write_bytes(
        path,
        (json.dumps(value, indent=2, sort_keys=True) + "\n").encode("utf-8"),
    )


def load_census_module():
    spec = importlib.util.spec_from_file_location("r7_census", CENSUS_SOURCE)
    if spec is None or spec.loader is None:
        raise RuntimeError("cannot load R7 census module")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def public_rows(rows: list[dict[str, object]]) -> list[dict[str, object]]:
    return [{k: v for k, v in row.items() if k != "source"} for row in rows]


def ledger_bytes(rows: list[dict[str, object]]) -> bytes:
    text = "path\tmode\tbytes\tsha256\n" + "".join(
        f'{row["path"]}\t{row["mode"]}\t{row["bytes"]}\t{row["sha256"]}\n'
        for row in rows
    )
    return text.encode("utf-8")


def ledger_summary(rows: list[dict[str, object]], data: bytes) -> dict[str, object]:
    paths = "".join(f'{row["path"]}\n' for row in rows).encode("utf-8")
    return {
        "members": len(rows),
        "bytes": sum(int(row["bytes"]) for row in rows),
        "path_set_sha256": sha256_bytes(paths),
        "ledger_sha256": sha256_bytes(data),
    }


def candidate_rows(source_rows: list[dict[str, object]], candidate: Path) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for source in source_rows:
        path = candidate / str(source["path"])
        info = path.stat()
        rows.append(
            {
                "path": source["path"],
                "mode": f"{stat.S_IMODE(info.st_mode):04o}",
                "bytes": info.st_size,
                "sha256": sha256_file(path),
            }
        )
    return rows


def run_command(name: str, argv: list[str], cwd: Path, env: dict[str, str] | None = None) -> dict[str, object]:
    started_unix_ns = time.time_ns()
    started_monotonic_ns = time.monotonic_ns()
    completed = subprocess.run(
        argv,
        cwd=cwd,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    ended_monotonic_ns = time.monotonic_ns()
    ended_unix_ns = time.time_ns()
    stdout_path = EVIDENCE / f"{name}.stdout"
    stderr_path = EVIDENCE / f"{name}.stderr"
    write_bytes(stdout_path, completed.stdout)
    write_bytes(stderr_path, completed.stderr)
    receipt = {
        "argv": argv,
        "cwd": str(cwd),
        "started_unix_ns": started_unix_ns,
        "ended_unix_ns": ended_unix_ns,
        "duration_ns": ended_monotonic_ns - started_monotonic_ns,
        "status": completed.returncode,
        "stdout": {
            "path": stdout_path.name,
            "bytes": len(completed.stdout),
            "sha256": sha256_bytes(completed.stdout),
        },
        "stderr": {
            "path": stderr_path.name,
            "bytes": len(completed.stderr),
            "sha256": sha256_bytes(completed.stderr),
        },
    }
    write_json(EVIDENCE / f"{name}.json", receipt)
    return receipt


def local_tree_rows(root: Path, paths: list[Path]) -> list[dict[str, object]]:
    rows: list[dict[str, object]] = []
    for path in paths:
        info = path.stat()
        rows.append(
            {
                "path": path.relative_to(root).as_posix(),
                "mode": f"{stat.S_IMODE(info.st_mode):04o}",
                "bytes": info.st_size,
                "sha256": sha256_file(path),
            }
        )
    rows.sort(key=lambda row: str(row["path"]).encode("utf-8"))
    return rows


def fleet_rows(workspace: Path) -> list[dict[str, object]]:
    root = workspace / "crates/core/src/grammar/generated"
    paths = [root / f"{family}{suffix}" for family in FAMILIES for suffix in (".rs", ".registry.json")]
    if any(path.is_symlink() or not path.is_file() for path in paths):
        raise RuntimeError("fleet member absent, non-regular, or symlinked")
    return local_tree_rows(workspace, paths)


def runtime_rows(workspace: Path) -> list[dict[str, object]]:
    paths: list[Path] = []
    for family in RUNTIME_FAMILIES:
        root = workspace / "crates/core/src/runtime" / family
        if not root.is_dir():
            raise RuntimeError(f"missing runtime root: {root}")
        for path in root.rglob("*"):
            if path.is_symlink():
                raise RuntimeError(f"runtime symlink: {path}")
            if path.is_file():
                paths.append(path)
    return local_tree_rows(workspace, paths)


def record_ledger(name: str, rows: list[dict[str, object]]) -> dict[str, object]:
    data = ledger_bytes(rows)
    write_bytes(EVIDENCE / name, data)
    return ledger_summary(rows, data)


def load_state() -> dict[str, object]:
    return json.loads((EVIDENCE / "STATE.json").read_text(encoding="utf-8"))


def abort_recensus() -> None:
    """Seal the governed live operand when a hostile pre-run stop fires."""
    EVIDENCE.mkdir(parents=True, exist_ok=True)
    census = load_census_module()
    members = census.rows(
        census.collect_build_inputs(WORKSPACE, PROGRAMMING), WORKSPACE, PROGRAMMING
    )
    data = ledger_bytes(members)
    write_bytes(EVIDENCE / "BUILD-INPUTS-ABORT.tsv", data)
    status = subprocess.run(
        ["git", "status", "--porcelain=v1", "-z", "--untracked-files=all"],
        cwd=WORKSPACE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if status.returncode != 0:
        raise RuntimeError(status.stderr.decode("utf-8", errors="replace"))
    write_bytes(EVIDENCE / "PORCELAIN-ABORT-v1-z.bin", status.stdout)
    state = {
        "schema": "bb-c.w0.r7-post-fix-fleet-abort.v1",
        "workspace": str(WORKSPACE),
        "head": subprocess.run(
            ["git", "rev-parse", "HEAD"],
            cwd=WORKSPACE,
            stdout=subprocess.PIPE,
            check=True,
        ).stdout.decode().strip(),
        "build_inputs": ledger_summary(members, data),
        "porcelain_v1_z": {
            "bytes": len(status.stdout),
            "sha256": sha256_bytes(status.stdout),
        },
        "grammar_facts_sha256": sha256_file(
            WORKSPACE / "crates/ir/src/passes/recognizers/grammar_facts.rs"
        ),
        "pratt_const_fold_sha256": sha256_file(
            WORKSPACE / "crates/core/tests/pratt_const_fold.rs"
        ),
        "terminal_disposition": "RED_HOSTILE_PRE_RUN_STOP",
        "build_started": False,
        "regen_started": False,
        "runtime_started": False,
    }
    write_json(EVIDENCE / "ABORT-STATE.json", state)


def prepare() -> None:
    EVIDENCE.mkdir(parents=True, exist_ok=False)
    # This script already exists because apply_patch created its parent.  The
    # expected first invocation therefore sees one evidence member.


def snapshot_and_build() -> None:
    EVIDENCE.mkdir(parents=True, exist_ok=True)
    census = load_census_module()
    source_paths = census.collect_build_inputs(WORKSPACE, PROGRAMMING)
    pre = census.rows(source_paths, WORKSPACE, PROGRAMMING)
    pre_data = ledger_bytes(pre)
    write_bytes(EVIDENCE / "BUILD-INPUTS-PRE.tsv", pre_data)

    candidate = Path(tempfile.mkdtemp(prefix="bbcw0-r7-post-fix-fleet-candidate.", dir="/private/tmp"))
    for row in pre:
        source = Path(row["source"])
        target = candidate / str(row["path"])
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)

    # Fail closed if any governed live byte moved during the copy.
    post = census.rows(census.collect_build_inputs(WORKSPACE, PROGRAMMING), WORKSPACE, PROGRAMMING)
    post_data = ledger_bytes(post)
    write_bytes(EVIDENCE / "BUILD-INPUTS-POSTCOPY.tsv", post_data)
    copied = candidate_rows(pre, candidate)
    copied_data = ledger_bytes(copied)
    write_bytes(EVIDENCE / "CANDIDATE-BUILD-INPUTS.tsv", copied_data)
    if public_rows(pre) != public_rows(post) or public_rows(pre) != copied:
        raise RuntimeError("governed build-input bytes moved or candidate copy differs")

    target = Path(tempfile.mkdtemp(prefix="bbcw0-r7-post-fix-fleet-build.", dir="/private/tmp"))
    candidate_workspace = candidate / "bbnf-lang"
    git_status = subprocess.run(
        ["git", "status", "--porcelain=v1", "-z", "--untracked-files=all"],
        cwd=WORKSPACE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if git_status.returncode != 0:
        raise RuntimeError(git_status.stderr.decode("utf-8", errors="replace"))
    write_bytes(EVIDENCE / "PORCELAIN-v1-z.bin", git_status.stdout)
    head = subprocess.run(
        ["git", "rev-parse", "HEAD"], cwd=WORKSPACE, stdout=subprocess.PIPE, check=True
    ).stdout.decode().strip()

    state = {
        "schema": "bb-c.w0.r7-post-fix-fleet.v1",
        "workspace": str(WORKSPACE),
        "head": head,
        "candidate_root": str(candidate),
        "candidate_workspace": str(candidate_workspace),
        "target_root": str(target),
        "build_inputs": ledger_summary(pre, pre_data),
        "source_postcopy_equal": True,
        "candidate_copy_equal": True,
        "porcelain_v1_z": {
            "bytes": len(git_status.stdout),
            "sha256": sha256_bytes(git_status.stdout),
        },
        "grammar_facts_sha256": sha256_file(WORKSPACE / "crates/ir/src/passes/recognizers/grammar_facts.rs"),
        "pratt_const_fold_sha256": sha256_file(WORKSPACE / "crates/core/tests/pratt_const_fold.rs"),
    }
    write_json(EVIDENCE / "STATE.json", state)

    env = os.environ.copy()
    env["CARGO_BUILD_JOBS"] = "1"
    build = run_command(
        "00-build",
        [
            "/usr/bin/arch",
            "-arm64",
            "cargo",
            "build",
            "--locked",
            "--target",
            "aarch64-apple-darwin",
            "--profile",
            "ax-iter",
            "-p",
            "xtask",
            "--features",
            "grammar-regen",
            "--target-dir",
            str(target),
        ],
        candidate_workspace,
        env,
    )
    if build["status"] != 0:
        raise SystemExit("RED: fresh ARM xtask build failed")

    binary = target / "aarch64-apple-darwin/ax-iter/xtask"
    if binary.is_symlink() or not binary.is_file():
        raise RuntimeError("fresh xtask binary missing or symlinked")
    file_probe = run_command("00-binary-file", ["/usr/bin/file", str(binary)], candidate_workspace)
    lipo_probe = run_command("00-binary-lipo", ["/usr/bin/lipo", "-archs", str(binary)], candidate_workspace)
    if file_probe["status"] != 0 or lipo_probe["status"] != 0:
        raise RuntimeError("binary architecture probe failed")
    file_text = (EVIDENCE / "00-binary-file.stdout").read_text(encoding="utf-8")
    lipo_text = (EVIDENCE / "00-binary-lipo.stdout").read_text(encoding="utf-8").strip()
    if "arm64" not in file_text or lipo_text != "arm64":
        raise RuntimeError("RED: binary is not arm64-only")
    state["binary"] = {
        "path": str(binary),
        "mode": f"{stat.S_IMODE(binary.stat().st_mode):04o}",
        "bytes": binary.stat().st_size,
        "sha256": sha256_file(binary),
        "file_arm64": True,
        "lipo_archs": lipo_text,
    }
    state["fleet_pre"] = record_ledger("GENERATED-PRE.tsv", fleet_rows(candidate_workspace))
    state["runtime_pre"] = record_ledger("RUNTIME-PRE.tsv", runtime_rows(candidate_workspace))
    write_json(EVIDENCE / "STATE.json", state)


def parse_pratt_mapping(parser_source: str) -> dict[str, int]:
    marker = "pub const PRECEDENCE_ENTRIES_comparison_expr: &[PrattEntry]"
    start = parser_source.index(marker)
    end = parser_source.index("];", start)
    section = parser_source[start:end]
    entry_re = re.compile(
        r"PrattEntry\s*\{\s*byte:\s*(\d+)u8,\s*"
        r"second_byte:\s*::core::option::Option::(Some\((\d+)u8\)|None),\s*"
        r"op_discriminant:\s*(\d+)u8,\s*\}",
        re.S,
    )
    mapping: dict[str, int] = {}
    for match in entry_re.finditer(section):
        first = int(match.group(1))
        second = int(match.group(3)) if match.group(2).startswith("Some") else None
        literal = bytes([first] + ([] if second is None else [second])).decode("ascii")
        mapping[literal] = int(match.group(4))
    return mapping


def parse_direct_mapping(parser_source: str) -> dict[str, int]:
    marker = "pub fn parse_keyword_GoogleSheetsParser_compare_op"
    start = parser_source.index(marker)
    next_fn = parser_source.index("pub fn ", start + len(marker))
    section = parser_source[start:next_fn]
    branch_re = re.compile(
        r"input\[\*p\.\.\*p \+ \d+usize\] == \[([^]]+)\]"
        r"(?:(?!input\[\*p\.\.\*p \+).)*?builder\.push_branch_tag\((\d+)u32\)",
        re.S,
    )
    mapping: dict[str, int] = {}
    for match in branch_re.finditer(section):
        octets = [int(value) for value in re.findall(r"(\d+)u8", match.group(1))]
        literal = bytes(octets).decode("ascii")
        mapping[literal] = int(match.group(2))
    return mapping


def semantic_gate(workspace: Path) -> dict[str, object]:
    sidecar_path = workspace / "crates/core/src/grammar/generated/google_sheets.registry.json"
    parser_path = workspace / "crates/core/src/grammar/generated/google_sheets.rs"
    sidecar_text = sidecar_path.read_text(encoding="utf-8")
    sidecar = json.loads(sidecar_text)
    layouts = list(sidecar["registry"]["layouts"].values())
    by_name = {layout["rule_name"]: layout for layout in layouts}
    compare = by_name["compare_op"]
    error = by_name["error_literal"]
    expected_names = [f"branch_{index}" for index in range(6)]
    expected_mapping = {"<>": 0, "<=": 1, ">=": 2, "=": 3, "<": 4, ">": 5}

    def u8_layout(layout: dict[str, object], arity: int | None = None) -> dict[str, bool]:
        fields = layout["fields"]
        result = {
            "rule_type_u8": layout["rule_type"] == "U8",
            "field_types_u8": all(field["type_desc"] == "U8" for field in fields),
            "no_span_or_tuple": "Span" not in json.dumps(layout) and "Tuple" not in json.dumps(layout),
        }
        if arity is not None:
            result["arity"] = len(fields) == arity
        return result

    compare_checks = u8_layout(compare, 6)
    compare_fields = compare["fields"]
    compare_checks.update(
        {
            "unique_names": len({field["name"] for field in compare_fields}) == len(compare_fields),
            "logical_names": [field["name"] for field in compare_fields] == expected_names,
            "branch_sources": all(
                field["source"] == {"BranchTag": {"branch_index": index}}
                for index, field in enumerate(compare_fields)
            ),
        }
    )
    error_checks = u8_layout(error)
    duplicate_layout_names = [
        layout["rule_name"]
        for layout in layouts
        if len({field["name"] for field in layout["fields"]}) != len(layout["fields"])
    ]
    parser_source = parser_path.read_text(encoding="utf-8")
    direct = parse_direct_mapping(parser_source)
    pratt = parse_pratt_mapping(parser_source)
    result = {
        "schema": "bb-c.w0.r7-post-fix-sheets-semantic-gate.v1",
        "sidecar_sha256": sha256_file(sidecar_path),
        "parser_sha256": sha256_file(parser_path),
        "compare_op_sidecar": compare_checks,
        "error_literal_sidecar": error_checks,
        "duplicate_layout_field_names": duplicate_layout_names,
        "expected_mapping": expected_mapping,
        "direct_mapping": direct,
        "direct_mapping_pass": direct == expected_mapping,
        "pratt_mapping": pratt,
        "pratt_mapping_pass": pratt == expected_mapping,
    }
    result["pass"] = (
        all(compare_checks.values())
        and all(error_checks.values())
        and not duplicate_layout_names
        and result["direct_mapping_pass"]
        and result["pratt_mapping_pass"]
    )
    return result


def fleet() -> None:
    state = load_state()
    workspace = Path(str(state["candidate_workspace"]))
    binary = Path(str(state["binary"]["path"]))
    regen = run_command("01-regen", ["/usr/bin/arch", "-arm64", str(binary), "regen"], workspace)
    if regen["status"] != 0:
        raise SystemExit("RED: atomic fleet regen failed")
    after_regen = fleet_rows(workspace)
    state["fleet_after_regen"] = record_ledger("GENERATED-AFTER-REGEN.tsv", after_regen)
    check = run_command(
        "02-regen-check",
        ["/usr/bin/arch", "-arm64", str(binary), "regen", "--check"],
        workspace,
    )
    after_check = fleet_rows(workspace)
    state["fleet_after_check"] = record_ledger("GENERATED-AFTER-CHECK.tsv", after_check)
    fixed_point = after_regen == after_check
    state["fleet_fixed_point"] = fixed_point
    write_json(EVIDENCE / "STATE.json", state)
    if check["status"] != 0 or not fixed_point:
        raise SystemExit("RED: fleet check or exact fixed point failed")
    semantic = semantic_gate(workspace)
    write_json(EVIDENCE / "SEMANTIC-GATE.json", semantic)
    state["semantic_gate"] = {
        "pass": semantic["pass"],
        "sha256": sha256_file(EVIDENCE / "SEMANTIC-GATE.json"),
    }
    write_json(EVIDENCE / "STATE.json", state)
    if not semantic["pass"]:
        raise SystemExit("RED: mandatory pre-runtime semantic gate failed")


def runtime() -> None:
    state = load_state()
    if not state.get("fleet_fixed_point") or not state.get("semantic_gate", {}).get("pass"):
        raise SystemExit("RED: runtime phase is not authorized")
    workspace = Path(str(state["candidate_workspace"]))
    binary = Path(str(state["binary"]["path"]))
    commands = (
        ("03-runtime-json", "regen-json"),
        ("04-runtime-css-pretty", "regen-css-pretty"),
        ("05-runtime-google-sheets", "regen-google-sheets"),
        ("06-runtime-css-l4", "regen-css"),
    )
    for name, command in commands:
        receipt = run_command(name, ["/usr/bin/arch", "-arm64", str(binary), command], workspace)
        state[f"{name}_status"] = receipt["status"]
        state["runtime_current"] = record_ledger(f"RUNTIME-AFTER-{name}.tsv", runtime_rows(workspace))
        write_json(EVIDENCE / "STATE.json", state)
        if receipt["status"] != 0:
            raise SystemExit(f"RED: first runtime generator failure at {command}")
    check = run_command(
        "07-check-runtime",
        ["/usr/bin/arch", "-arm64", str(binary), "check-runtime"],
        workspace,
    )
    state["check_runtime_status"] = check["status"]
    state["runtime_after_check"] = record_ledger("RUNTIME-AFTER-CHECK.tsv", runtime_rows(workspace))
    write_json(EVIDENCE / "STATE.json", state)
    if check["status"] != 0:
        raise SystemExit("RED: runtime projection fixed-point check failed")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "phase", choices=("abort-recensus", "snapshot-build", "fleet", "runtime")
    )
    args = parser.parse_args()
    if args.phase == "abort-recensus":
        abort_recensus()
    elif args.phase == "snapshot-build":
        snapshot_and_build()
    elif args.phase == "fleet":
        fleet()
    else:
        runtime()


if __name__ == "__main__":
    main()
