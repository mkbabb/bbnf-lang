#!/usr/bin/env python3
"""CB-B0 — the contest's measurement harness (sk-v27 LW1.CON-1).

One rep is one fresh process. Every rep records the load before and after,
the thermal state, the child's rusage, and the kernel's per-QoS CPU-time
accounting for that very process (proc_pid_rusage on the unreaped child),
so the QoS a rep actually ran at is measured, never assumed.

Records are keyed by the tree hash of the working tree (tracked plus
untracked, ignored files excluded) with `docs/benchmarks/records/` removed,
plus the commit sha: writing a record never changes the key of the next.

A record is QUIET only when every rep started at a 1-minute load below the
quiet bar (8), the thermal state recorded no warning, the machine-wide
`guarded --exclusive bench` lock was held, and at least 95 % of every rep's
CPU time landed in the user-interactive QoS bucket. Anything else is NOISY,
with each reason listed. NOISY numbers are never certifying.

Usage:
  cbb0.py run --label L --matrix PORTABLE|NATIVE --reps N --out DIR
              [--metric NAME=REGEX ...] [--build-note TEXT] [--toolchain TEXT]
              [--repo PATH] -- CMD ...
  cbb0.py dry-run --out DIR [--repo PATH]
  cbb0.py tree-hash [--repo PATH]
  cbb0.py validate FILE ...
"""
from __future__ import annotations

import argparse
import ctypes
import datetime as dt
import fcntl
import hashlib
import json
import os
import platform
import re
import statistics
import subprocess
import sys
import tempfile
import time

SCHEMA_ID = "cb-b0/1"
RECORDS_PREFIX = "docs/benchmarks/records"
QUIET_LOAD = 8.0
QOS_UI_MIN_FRACTION = 0.95
BENCH_LOCK = os.path.expanduser("~/.claude/guard/slots/ex-bench")
HERE = os.path.dirname(os.path.abspath(__file__))
SCHEMA_PATH = os.path.join(HERE, "manifest.schema.json")

# ── macOS process plumbing ────────────────────────────────────────────────────

_libc = ctypes.CDLL("/usr/lib/libSystem.B.dylib", use_errno=True) if sys.platform == "darwin" else None

QOS_CLASSES = {  # <sys/qos.h>
    "user_interactive": 0x21,
    "user_initiated": 0x19,
    "default": 0x15,
    "utility": 0x11,
    "background": 0x09,
}


class _RusageInfoV4(ctypes.Structure):  # <sys/resource.h> struct rusage_info_v4
    _fields_ = [("ri_uuid", ctypes.c_uint8 * 16)] + [
        (name, ctypes.c_uint64)
        for name in (
            "user_time system_time pkg_idle_wkups interrupt_wkups pageins wired_size "
            "resident_size phys_footprint proc_start_abstime proc_exit_abstime "
            "child_user_time child_system_time child_pkg_idle_wkups child_interrupt_wkups "
            "child_pageins child_elapsed_abstime diskio_bytesread diskio_byteswritten "
            "qos_default qos_maintenance qos_background qos_utility qos_legacy "
            "qos_user_initiated qos_user_interactive billed_system_time serviced_system_time "
            "logical_writes lifetime_max_phys_footprint instructions cycles billed_energy "
            "serviced_energy interval_max_phys_footprint runnable_time"
        ).split()
    ]


_QOS_BUCKETS = ("default", "maintenance", "background", "utility", "legacy",
                "user_initiated", "user_interactive")


def _cstrs(items):
    arr = (ctypes.c_char_p * (len(items) + 1))()
    for i, s in enumerate(items):
        arr[i] = s.encode() if isinstance(s, str) else s
    arr[len(items)] = None
    return arr


def spawn_rep(cmd: list[str], stdout_path: str, clamp: str | None = None) -> dict:
    """Spawn one fresh process, wait without reaping, read its QoS accounting, reap."""
    if _libc is None:
        raise SystemExit("CB-B0 requires darwin (proc_pid_rusage); see the plan's x86 policy X-4")
    attr = ctypes.c_void_p()
    fa = ctypes.c_void_p()
    _libc.posix_spawnattr_init(ctypes.byref(attr))
    _libc.posix_spawn_file_actions_init(ctypes.byref(fa))
    if clamp is not None:  # the kernel only accepts clamps downward (utility, background)
        rc = _libc.posix_spawnattr_set_qos_class_np(ctypes.byref(attr), QOS_CLASSES[clamp])
        if rc != 0:
            raise OSError(rc, f"posix_spawnattr_set_qos_class_np({clamp}) refused")
    out_fd = os.open(stdout_path, os.O_CREAT | os.O_WRONLY | os.O_TRUNC, 0o644)
    _libc.posix_spawn_file_actions_adddup2(ctypes.byref(fa), out_fd, 1)
    argv = _cstrs(cmd)
    envp = _cstrs([f"{k}={v}" for k, v in os.environ.items()])
    pid = ctypes.c_int()
    t0 = time.perf_counter_ns()
    rc = _libc.posix_spawnp(ctypes.byref(pid), argv[0], ctypes.byref(fa), ctypes.byref(attr), argv, envp)
    os.close(out_fd)
    _libc.posix_spawn_file_actions_destroy(ctypes.byref(fa))
    _libc.posix_spawnattr_destroy(ctypes.byref(attr))
    if rc != 0:
        raise OSError(rc, f"posix_spawnp failed for {cmd[0]}")
    os.waitid(os.P_PID, pid.value, os.WEXITED | os.WNOWAIT)
    wall_ns = time.perf_counter_ns() - t0
    ri = _RusageInfoV4()
    ri_rc = _libc.proc_pid_rusage(pid.value, 4, ctypes.byref(ri))
    _, status, ru = os.wait4(pid.value, 0)
    qos = {b: int(getattr(ri, "qos_" + b)) for b in _QOS_BUCKETS} if ri_rc == 0 else {}
    total = sum(qos.values())
    dominant = max(qos, key=qos.get) if total else "unknown"
    return {
        "wall_ns": wall_ns,
        "exit": os.waitstatus_to_exitcode(status),
        "user_us": int(ru.ru_utime * 1e6),
        "sys_us": int(ru.ru_stime * 1e6),
        "maxrss_bytes": int(ru.ru_maxrss),
        "instructions": int(ri.instructions) if ri_rc == 0 else None,
        "cycles": int(ri.cycles) if ri_rc == 0 else None,
        "qos_cpu": qos,
        "qos_dominant": dominant,
        "qos_ui_fraction": (qos.get("user_interactive", 0) / total) if total else 0.0,
    }


# ── host conditions ───────────────────────────────────────────────────────────

def load1() -> float:
    return os.getloadavg()[0]


def therm() -> dict:
    try:
        raw = subprocess.run(["pmset", "-g", "therm"], capture_output=True, text=True, timeout=10).stdout
    except (OSError, subprocess.TimeoutExpired) as e:
        return {"raw": f"unavailable: {e}", "warning": True}
    limit = re.search(r"CPU_Speed_Limit\s*=\s*(\d+)", raw)
    warning = not re.search(r"No thermal warning level has been recorded", raw)
    if limit and int(limit.group(1)) < 100:
        warning = True
    return {"raw": raw.strip(), "warning": warning}


def bench_lock_held() -> bool:
    """True iff some process holds guarded's machine-wide 'bench' exclusive lock."""
    if not os.path.exists(BENCH_LOCK):
        return False
    fd = os.open(BENCH_LOCK, os.O_RDWR)
    try:
        fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        return True
    else:
        fcntl.flock(fd, fcntl.LOCK_UN)
        return False
    finally:
        os.close(fd)


def _sysctl(name: str) -> str:
    try:
        return subprocess.run(["sysctl", "-n", name], capture_output=True, text=True).stdout.strip()
    except OSError:
        return ""


def host() -> dict:
    return {
        "model": _sysctl("hw.model"),
        "cpu": _sysctl("machdep.cpu.brand_string"),
        "ncpu": int(_sysctl("hw.ncpu") or 0),
        "perflevel0_cpus": int(_sysctl("hw.perflevel0.logicalcpu") or 0),
        "os": f"{platform.system()} {platform.release()}",
        "mem_bytes": int(_sysctl("hw.memsize") or 0),
    }


# ── tree identity ─────────────────────────────────────────────────────────────

def _git(repo: str, *args: str, env: dict | None = None) -> str:
    return subprocess.run(["git", "-C", repo, *args], capture_output=True, text=True, check=True,
                          env=env).stdout.strip()


def tree_hash(repo: str) -> dict:
    """Hash of the working tree (tracked + untracked, ignores honoured) minus RECORDS_PREFIX."""
    with tempfile.TemporaryDirectory(prefix="cbb0-index-") as d:
        env = dict(os.environ, GIT_INDEX_FILE=os.path.join(d, "index"))
        _git(repo, "read-tree", "HEAD", env=env)
        _git(repo, "add", "-A", "--", ".", f":(exclude){RECORDS_PREFIX}", env=env)
        _git(repo, "rm", "-r", "--cached", "-q", "--ignore-unmatch", "--", RECORDS_PREFIX, env=env)
        tree = _git(repo, "write-tree", env=env)
    head_tree = _git(repo, "rev-parse", "HEAD^{tree}")
    with tempfile.TemporaryDirectory(prefix="cbb0-index-") as d:
        env = dict(os.environ, GIT_INDEX_FILE=os.path.join(d, "index"))
        _git(repo, "read-tree", "HEAD", env=env)
        _git(repo, "rm", "-r", "--cached", "-q", "--ignore-unmatch", "--", RECORDS_PREFIX, env=env)
        head_minus = _git(repo, "write-tree", env=env)
    return {
        "commit": _git(repo, "rev-parse", "HEAD"),
        "tree_hash": tree,
        "tree_hash_excludes": [RECORDS_PREFIX],
        "head_tree": head_tree,
        "dirty": tree != head_minus,
    }


# ── statistics ────────────────────────────────────────────────────────────────

def summarize(xs: list[float]) -> dict:
    xs = sorted(xs)
    n = len(xs)
    if n == 0:
        return {"n": 0}
    q = statistics.quantiles(xs, n=4, method="inclusive") if n >= 2 else [xs[0]] * 3
    med = statistics.median(xs)
    return {
        "n": n,
        "median": med,
        "p25": q[0],
        "p75": q[2],
        "iqr": q[2] - q[0],
        "mad": statistics.median([abs(x - med) for x in xs]),
        "min": xs[0],
        "max": xs[-1],
        "rel_iqr": ((q[2] - q[0]) / med) if med else None,
    }


# ── run ───────────────────────────────────────────────────────────────────────

def _parse_metrics(specs: list[str]) -> list[tuple[str, re.Pattern]]:
    out = []
    for s in specs:
        name, _, rx = s.partition("=")
        if not name or not rx:
            raise SystemExit(f"--metric wants NAME=REGEX, got {s!r}")
        out.append((name, re.compile(rx, re.M)))
    return out


def _file_sha256(path: str) -> str:
    with open(path, "rb") as f:
        return hashlib.sha256(f.read()).hexdigest()


def record_key(ident: dict, label: str, matrix: str) -> str:
    safe = re.sub(r"[^A-Za-z0-9_.-]+", "_", label)
    return f"{ident['tree_hash'][:16]}-{ident['commit'][:12]}-{safe}-{matrix}"


def run(args) -> str:
    repo = os.path.abspath(args.repo)
    metrics = _parse_metrics(args.metric or [])
    ident = tree_hash(repo)
    os.makedirs(args.out, exist_ok=True)
    key = record_key(ident, args.label, args.matrix)
    reps = []
    loads_before, loads_after = [], []
    therm_before = therm()
    lock = bench_lock_held()
    with tempfile.TemporaryDirectory(prefix="cbb0-rep-") as d:
        for i in range(args.reps):
            lb = load1()
            out_path = os.path.join(d, f"rep{i}.out")
            r = spawn_rep(args.cmd, out_path)
            la = load1()
            with open(out_path, encoding="utf-8", errors="replace") as f:
                text = f.read()
            m = {}
            for name, rx in metrics:
                hit = rx.search(text)
                m[name] = float(hit.group(1)) if hit else None
            r.update({"i": i, "load_before": lb, "load_after": la, "metrics": m,
                      "stdout_sha256": hashlib.sha256(text.encode()).hexdigest()})
            loads_before.append(lb)
            loads_after.append(la)
            reps.append(r)
    therm_after = therm()
    reasons = []
    if any(lb >= QUIET_LOAD for lb in loads_before):
        reasons.append(f"load1 >= {QUIET_LOAD:g} before {sum(lb >= QUIET_LOAD for lb in loads_before)}/{len(reps)} reps "
                       f"(max {max(loads_before):.2f})")
    if therm_before["warning"] or therm_after["warning"]:
        reasons.append("thermal warning recorded")
    if not lock:
        reasons.append("guarded --exclusive bench lock not held")
    low_qos = [r["i"] for r in reps if r["qos_ui_fraction"] < QOS_UI_MIN_FRACTION]
    if low_qos:
        doms = sorted({r["qos_dominant"] for r in reps if r["i"] in low_qos})
        reasons.append(f"user-interactive QoS < {QOS_UI_MIN_FRACTION:g} of CPU on {len(low_qos)}/{len(reps)} reps "
                       f"(dominant bucket: {','.join(doms)})")
    if len(reps) < 10:
        reasons.append(f"{len(reps)} reps, below the programme floor of 10")
    bad_exit = [r["i"] for r in reps if r["exit"] != 0]
    if bad_exit:
        reasons.append(f"non-zero exit on reps {bad_exit}")
    summary = {"wall_ms": summarize([r["wall_ns"] / 1e6 for r in reps])}
    for name, _ in metrics:
        vals = [r["metrics"][name] for r in reps if r["metrics"][name] is not None]
        summary[name] = summarize(vals)
    rec = {
        "schema": SCHEMA_ID,
        "record_key": key,
        "label": args.label,
        "matrix": args.matrix,
        "created_at": dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
        "repo": ident,
        "host": host(),
        "toolchain": args.toolchain or "",
        "build_note": args.build_note or "",
        "command": args.cmd,
        "harness": {"guarded_run": os.environ.get("GUARDED_RUN", ""), "bench_lock_held": lock,
                    "qos_requested": "user_interactive (set by the measured binary; the kernel rejects a "
                                     "spawn-attribute raise and exec resets thread QoS)",
                    "quiet_load": QUIET_LOAD, "qos_ui_min_fraction": QOS_UI_MIN_FRACTION,
                    "harness_sha256": _file_sha256(__file__)},
        "conditions": {"load_before": loads_before, "load_after": loads_after,
                       "therm_before": therm_before, "therm_after": therm_after},
        "reps": reps,
        "summary": summary,
        "status": "NOISY" if reasons else "QUIET",
        "status_reasons": reasons,
    }
    errs = validate_obj(rec, load_schema())
    if errs:
        raise SystemExit("record fails its own schema:\n  " + "\n  ".join(errs))
    path = os.path.join(args.out, key + ".json")
    with open(path, "w") as f:
        json.dump(rec, f, indent=1, sort_keys=False)
        f.write("\n")
    return path


def dry_run(args) -> str:
    """A schema-valid record from a trivial workload (`/usr/bin/true`, 3 reps)."""
    ns = argparse.Namespace(repo=args.repo, out=args.out, label="dry-run", matrix="PORTABLE", reps=3,
                            metric=[], build_note="dry run: /usr/bin/true", toolchain="", cmd=["/usr/bin/true"])
    return run(ns)


# ── schema validation (the subset of JSON Schema the manifest uses) ───────────

def load_schema() -> dict:
    with open(SCHEMA_PATH) as f:
        return json.load(f)


_TYPES = {"object": dict, "array": list, "string": str, "boolean": bool, "null": type(None)}


def _type_ok(v, t) -> bool:
    if isinstance(t, list):
        return any(_type_ok(v, x) for x in t)
    if t == "integer":
        return isinstance(v, int) and not isinstance(v, bool)
    if t == "number":
        return isinstance(v, (int, float)) and not isinstance(v, bool)
    return isinstance(v, _TYPES[t])


def validate_obj(v, s, path="$") -> list[str]:
    errs = []
    if "type" in s and not _type_ok(v, s["type"]):
        return [f"{path}: expected {s['type']}, got {type(v).__name__}"]
    if "enum" in s and v not in s["enum"]:
        errs.append(f"{path}: {v!r} not in {s['enum']}")
    if "pattern" in s and isinstance(v, str) and not re.search(s["pattern"], v):
        errs.append(f"{path}: {v!r} !~ /{s['pattern']}/")
    if "minimum" in s and isinstance(v, (int, float)) and v < s["minimum"]:
        errs.append(f"{path}: {v} < {s['minimum']}")
    if "minItems" in s and isinstance(v, list) and len(v) < s["minItems"]:
        errs.append(f"{path}: {len(v)} items < {s['minItems']}")
    if isinstance(v, dict):
        for k in s.get("required", []):
            if k not in v:
                errs.append(f"{path}: missing {k}")
        props = s.get("properties", {})
        for k, sub in props.items():
            if k in v:
                errs += validate_obj(v[k], sub, f"{path}.{k}")
        if s.get("additionalProperties") is False:
            for k in v:
                if k not in props:
                    errs.append(f"{path}: unexpected key {k}")
        elif isinstance(s.get("additionalProperties"), dict):
            for k in v:
                if k not in props:
                    errs += validate_obj(v[k], s["additionalProperties"], f"{path}.{k}")
    if isinstance(v, list) and "items" in s:
        for i, x in enumerate(v):
            errs += validate_obj(x, s["items"], f"{path}[{i}]")
    return errs


def validate_files(paths: list[str]) -> int:
    schema = load_schema()
    rc = 0
    for p in paths:
        with open(p) as f:
            errs = validate_obj(json.load(f), schema)
        print(f"{'OK  ' if not errs else 'FAIL'} {p}")
        for e in errs:
            print("     " + e)
        rc |= bool(errs)
    return rc


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="verb", required=True)
    r = sub.add_parser("run")
    r.add_argument("--label", required=True)
    r.add_argument("--matrix", required=True, choices=["PORTABLE", "NATIVE"])
    r.add_argument("--reps", type=int, default=10)
    r.add_argument("--out", required=True)
    r.add_argument("--metric", action="append")
    r.add_argument("--build-note")
    r.add_argument("--toolchain")
    r.add_argument("--repo", default=os.getcwd())
    r.add_argument("cmd", nargs=argparse.REMAINDER)
    d = sub.add_parser("dry-run")
    d.add_argument("--out", required=True)
    d.add_argument("--repo", default=os.getcwd())
    t = sub.add_parser("tree-hash")
    t.add_argument("--repo", default=os.getcwd())
    v = sub.add_parser("validate")
    v.add_argument("files", nargs="+")
    a = ap.parse_args(argv)
    if a.verb == "run":
        a.cmd = a.cmd[1:] if a.cmd[:1] == ["--"] else a.cmd
        if not a.cmd:
            ap.error("run needs a command after --")
        if a.reps < 10:
            print(f"cbb0: {a.reps} reps is below the programme floor of 10; the record says so", file=sys.stderr)
        print(run(a))
    elif a.verb == "dry-run":
        print(dry_run(a))
    elif a.verb == "tree-hash":
        print(json.dumps(tree_hash(os.path.abspath(a.repo)), indent=1))
    elif a.verb == "validate":
        return validate_files(a.files)
    return 0


if __name__ == "__main__":
    sys.exit(main())
