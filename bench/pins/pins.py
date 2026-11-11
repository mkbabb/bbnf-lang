#!/usr/bin/env python3
"""Contest pins (sk-v27 LW1.CON-2): resolve and check `bench/W0-BENCH-PINS.tsv` v2.

Rule: each contestant is pinned to its newest published, non-yanked release that is at least
14 days old on the resolution date (pre-releases only where the project publishes nothing
else, e.g. the lightningcss crate). Registry rows carry the registry checksum; upstream rows
carry the tag's commit (the content address). Every row records what the workspaces lock
today, so a departure from the pin is visible as a diff, never silent.

  pins.py resolve [--date YYYY-MM-DD] [--out bench/W0-BENCH-PINS.tsv]
  pins.py check FILE        # rc 0 iff every row has a checksum and is >= 14 days old
"""
from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import re
import subprocess
import sys
import urllib.parse
import urllib.request

MIN_AGE_DAYS = 14
UA = "bbnf-lang sk-v27 bench pins (github.com/mkbabb/bbnf-lang)"
REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
V1 = os.path.join(REPO, "bench", "pins", "W0-BENCH-PINS.v1-sk-v26.tsv")  # sk-v26 47d51a190, verbatim
LOCKS = {"root": os.path.join(REPO, "Cargo.lock"), "skinny": os.path.join(REPO, "skinny", "Cargo.lock")}

# role: what the contestant is in the contest (plan §3 CON lane; the competition design's opponent set).
CRATES = [
    ("simd-json", "JSON P1/P4 opponent"), ("sonic-rs", "JSON P1/P4 opponent"),
    ("serde_json", "JSON P1 opponent"), ("jiter", "JSON P1 opponent"),
    ("serde_json_borrow", "JSON P1 opponent (borrowed)"),
    ("winnow", "combinator opponent"), ("nom", "combinator opponent"), ("chumsky", "combinator opponent"),
    ("pest", "PEG generator opponent"), ("pest_derive", "PEG generator opponent"),
    ("tree-sitter", "CST opponent (P4 only)"), ("tree-sitter-json", "CST opponent grammar"),
    ("tree-sitter-css", "CST opponent grammar"),
    ("cssparser", "CSS P2 opponent (G-C1a)"), ("lightningcss", "CSS P2 opponent (G-C1b)"),
    ("regex", "regex opponent"), ("regex-automata", "regex opponent / DFA reference"),
    ("logos", "lexer-generator opponent"),
]
NPM = [
    ("@lezer/lr", "JS incremental-LR opponent"), ("@lezer/json", "JS CST opponent grammar"),
    ("@lezer/css", "JS CST opponent grammar"),
    ("@csstools/css-tokenizer", "JS CSS tokenizer opponent"),
    ("@csstools/css-parser-algorithms", "JS CSS parser opponent"),
    ("css-tree", "JS CSS parser opponent"), ("postcss", "JS CSS parser opponent"),
    ("peggy", "JS PEG generator opponent"), ("chevrotain", "JS parser-toolkit opponent"),
    ("ohm-js", "JS PEG opponent"), ("parsimmon", "JS combinator opponent"),
]
# (name, host, project, tag regex with one version group, crate to align with or None, role)
UPSTREAM = [
    ("simdjson", "github", "simdjson/simdjson", r"^v(\d+\.\d+\.\d+)$", None, "JSON reference (C++)"),
    ("yyjson", "github", "ibireme/yyjson", r"^(\d+\.\d+\.\d+)$", None, "JSON reference (C)"),
    ("simd-json", "github", "simd-lite/simd-json", r"^v(\d+\.\d+\.\d+)$", "simd-json", "JSON opponent source"),
    ("sonic-rs", "github", "cloudwego/sonic-rs", r"^v(\d+\.\d+\.\d+)$", "sonic-rs", "JSON opponent source"),
    ("cssparser", "github", "servo/rust-cssparser", r"^v?(\d+\.\d+\.\d+)$", "cssparser", "CSS opponent source"),
    ("lightningcss", "github", "parcel-bundler/lightningcss", r"^v(\d+\.\d+\.\d+)$", None, "CSS opponent source"),
    ("tree-sitter", "github", "tree-sitter/tree-sitter", r"^v(\d+\.\d+\.\d+)$", "tree-sitter", "CST opponent source"),
    ("regex", "github", "rust-lang/regex", r"^(\d+\.\d+\.\d+)$", "regex", "regex opponent source"),
    ("re2c", "github", "skvadrik/re2c", r"^(\d+(?:\.\d+)+)$", None, "lexer-generator reference"),
    ("dav1d", "gitlab", "code.videolan.org/videolan/dav1d", r"^(\d+\.\d+\.\d+)$", None,
     "SIMD/asm dispatch reference (DAV1D-style kernels)"),
]
TOOLCHAINS = [("rust-stable", "github", "rust-lang/rust", r"^(\d+\.\d+\.\d+)$", "toolchain for the cargo +stable matrix")]

COLUMNS = ["kind", "name", "pin", "checksum", "published_at", "age_days", "resolved_on", "locked_root",
           "locked_skinny", "v1_pin", "departure", "source", "role"]


def _get(url: str):
    req = urllib.request.Request(url, headers={"User-Agent": UA, "Accept": "application/json"})
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.load(r)


def _ts(s: str) -> dt.datetime:
    return dt.datetime.fromisoformat(s.replace("Z", "+00:00"))


def _vkey(v: str):
    core, _, pre = v.partition("-")
    nums = [int(x) for x in re.findall(r"\d+", core)]
    pre_nums = [int(x) for x in re.findall(r"\d+", pre)]
    return (nums, 0 if pre else 1, pre_nums)


def locked(name: str) -> dict:
    out = {}
    for k, path in LOCKS.items():
        try:
            text = open(path).read()
        except OSError:
            out[k] = "n/a"
            continue
        vs = re.findall(r'\[\[package\]\]\nname = "%s"\nversion = "([^"]+)"' % re.escape(name), text)
        out[k] = ",".join(sorted(set(vs), key=_vkey)) or "-"
    return out


def v1_pins() -> dict:
    """(kind, name) -> version from the sk-v26 v1 table (crate rows read from the BBNF lock rows)."""
    out = {}
    try:
        lines = open(V1).read().splitlines()[1:]
    except OSError:
        return out
    for line in lines:
        f = line.split("\t")
        if len(f) < 3:
            continue
        kind = {"crate": "crate", "upstream_commit": "upstream_commit"}.get(f[0])
        if kind:
            name = {"Lightning CSS": "lightningcss"}.get(f[1], f[1])
            out[(kind, name)] = f[2].replace(" and ", ",")
    return out


def resolve_crate(name: str, cutoff: dt.datetime):
    vs = _get(f"https://crates.io/api/v1/crates/{name}/versions?per_page=100")["versions"]
    ok = [v for v in vs if not v["yanked"] and _ts(v["created_at"]) <= cutoff]
    stable = [v for v in ok if "-" not in v["num"]]
    pool = stable or ok
    if not pool:
        raise LookupError(f"crate {name}: no release >= {MIN_AGE_DAYS} days old")
    best = max(pool, key=lambda v: _vkey(v["num"]))
    newest = max((v for v in vs if not v["yanked"]), key=lambda v: _vkey(v["num"]))
    return best["num"], best["checksum"], best["created_at"], f"crates.io (newest {newest['num']} {newest['created_at'][:10]})"


def resolve_npm(name: str, cutoff: dt.datetime):
    d = _get("https://registry.npmjs.org/" + urllib.parse.quote(name, safe="@"))
    times = d["time"]
    cands = [v for v in d["versions"] if v in times and "-" not in v and _ts(times[v]) <= cutoff
             and not d["versions"][v].get("deprecated")]
    if not cands:
        raise LookupError(f"npm {name}: no release >= {MIN_AGE_DAYS} days old")
    best = max(cands, key=_vkey)
    latest = d["dist-tags"].get("latest", "?")
    return best, d["versions"][best]["dist"]["integrity"], times[best], f"npm (latest {latest} {times.get(latest, '?')[:10]})"


def _gh(path: str):
    return json.loads(subprocess.run(["gh", "api", path], capture_output=True, text=True, check=True).stdout)


def _peeled_tags(url: str) -> dict:
    out = subprocess.run(["git", "ls-remote", "--tags", url], capture_output=True, text=True, check=True).stdout
    tags = {}
    for line in out.splitlines():
        sha, ref = line.split("\t")
        tag = ref[len("refs/tags/"):]
        if tag.endswith("^{}"):
            tags[tag[:-3]] = sha  # the peeled commit wins over the tag object
        else:
            tags.setdefault(tag, sha)
    return tags


def resolve_upstream(host, project, rx, align_version, cutoff):
    rx = re.compile(rx)
    if host == "github":
        tags = _peeled_tags(f"https://github.com/{project}.git")
        cands = []
        for t, sha in tags.items():
            m = rx.match(t)
            if m:
                cands.append((m.group(1), t, sha))
        cands.sort(key=lambda c: _vkey(c[0]), reverse=True)
        if align_version:
            cands = [c for c in cands if c[0] == align_version] or cands
        for ver, tag, sha in cands:
            date = _gh(f"repos/{project}/commits/{sha}")["commit"]["committer"]["date"]
            if align_version and ver == align_version:
                return ver, sha, date, f"github.com/{project} tag {tag} (aligned to the crate pin)"
            if _ts(date) <= cutoff:
                return ver, sha, date, f"github.com/{project} tag {tag}"
        raise LookupError(f"{project}: no tag >= {MIN_AGE_DAYS} days old")
    base, _, proj = project.partition("/")
    tags = _get(f"https://{base}/api/v4/projects/{urllib.parse.quote(proj, safe='')}/repository/tags?per_page=100")
    cands = [(rx.match(t["name"]).group(1), t) for t in tags if rx.match(t["name"])]
    cands.sort(key=lambda c: _vkey(c[0]), reverse=True)
    for ver, t in cands:
        if _ts(t["commit"]["committed_date"]) <= cutoff:
            return ver, t["commit"]["id"], t["commit"]["committed_date"], f"{project} tag {t['name']}"
    raise LookupError(f"{project}: no tag >= {MIN_AGE_DAYS} days old")


def _row(kind, name, pin, checksum, published, resolved_on, lock, source, role, v1=None):
    age = (dt.datetime.combine(resolved_on, dt.time(tzinfo=dt.timezone.utc)) - _ts(published)).days
    locked_vs = {v for k in ("root", "skinny") for v in lock[k].split(",") if v not in ("-", "n/a")}
    v1 = v1 or "-"
    if not locked_vs and v1 != "-":
        departure = "none" if v1 == pin else f"v1 {v1} -> pin {pin}"
    elif not locked_vs:
        departure = "not locked (introduce at pin)"
    elif locked_vs == {pin}:
        departure = "none"
    else:
        departure = "lock " + ",".join(sorted(locked_vs, key=_vkey)) + " -> pin " + pin
    return {"kind": kind, "name": name, "pin": pin, "checksum": checksum, "published_at": published,
            "age_days": str(age), "resolved_on": resolved_on.isoformat(), "locked_root": lock["root"],
            "locked_skinny": lock["skinny"], "v1_pin": v1, "departure": departure, "source": source, "role": role}


def resolve(on: dt.date) -> list[dict]:
    cutoff = dt.datetime.combine(on, dt.time(tzinfo=dt.timezone.utc)) - dt.timedelta(days=MIN_AGE_DAYS)
    rows, crate_pins, v1 = [], {}, v1_pins()
    nolock = {"root": "-", "skinny": "-"}
    for name, role in CRATES:
        try:
            pin, ck, pub, src = resolve_crate(name, cutoff)
            crate_pins[name] = pin
            rows.append(_row("crate", name, pin, ck, pub, on, locked(name), src, role, v1.get(("crate", name))))
        except Exception as e:  # recorded, never dropped
            rows.append({c: "" for c in COLUMNS} | {"kind": "crate", "name": name, "departure": f"UNRESOLVED: {e}",
                                                    "role": role, "resolved_on": on.isoformat()})
    for name, role in NPM:
        try:
            pin, ck, pub, src = resolve_npm(name, cutoff)
            rows.append(_row("npm", name, pin, ck, pub, on, nolock, src, role))
        except Exception as e:
            rows.append({c: "" for c in COLUMNS} | {"kind": "npm", "name": name, "departure": f"UNRESOLVED: {e}",
                                                    "role": role, "resolved_on": on.isoformat()})
    for name, hostk, project, rx, align, role in UPSTREAM:
        try:
            pin, sha, pub, src = resolve_upstream(hostk, project, rx, crate_pins.get(align) if align else None, cutoff)
            rows.append(_row("upstream_commit", name, pin, sha, pub, on, nolock, src, role,
                            v1.get(("upstream_commit", name))))
        except Exception as e:
            rows.append({c: "" for c in COLUMNS} | {"kind": "upstream_commit", "name": name,
                                                    "departure": f"UNRESOLVED: {e}", "role": role,
                                                    "resolved_on": on.isoformat()})
    for name, hostk, project, rx, role in TOOLCHAINS:
        pin, sha, pub, src = resolve_upstream(hostk, project, rx, None, cutoff)
        rows.append(_row("toolchain", name, pin, sha, pub, on, nolock, src, role))
    return rows


CHECKSUM = re.compile(r"^(?:[0-9a-f]{40}|[0-9a-f]{64}|sha512-[A-Za-z0-9+/]{86}==)$")


def check(path: str) -> list[str]:
    errs = []
    with open(path) as f:
        lines = [l.rstrip("\n") for l in f if l.strip() and not l.startswith("#")]
    head = lines[0].split("\t")
    for c in ("checksum", "published_at", "resolved_on", "pin"):
        if c not in head:
            errs.append(f"header lacks column {c!r} (v1 file?)")
    if errs:
        return errs
    ix = {c: head.index(c) for c in head}
    for n, line in enumerate(lines[1:], 2):
        f = line.split("\t")
        name = f"{f[ix['kind']] if 'kind' in ix else '?'}:{f[ix['name']] if 'name' in ix else '?'}"
        if len(f) != len(head):
            errs.append(f"line {n} {name}: {len(f)} fields, header has {len(head)}")
            continue
        if not CHECKSUM.match(f[ix["checksum"]]):
            errs.append(f"line {n} {name}: checksum {f[ix['checksum']]!r} is not a sha1/sha256/sha512 address")
        try:
            age = dt.datetime.combine(dt.date.fromisoformat(f[ix["resolved_on"]]),
                                      dt.time(tzinfo=dt.timezone.utc)) - _ts(f[ix["published_at"]])
            if age < dt.timedelta(days=MIN_AGE_DAYS):
                errs.append(f"line {n} {name}: published {f[ix['published_at']]} is {age.days} d before "
                            f"resolution, under {MIN_AGE_DAYS}")
        except ValueError as e:
            errs.append(f"line {n} {name}: dates unreadable ({e})")
    return errs


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="verb", required=True)
    r = sub.add_parser("resolve")
    r.add_argument("--date", default=dt.date.today().isoformat())
    r.add_argument("--out", default=os.path.join(REPO, "bench", "W0-BENCH-PINS.tsv"))
    c = sub.add_parser("check")
    c.add_argument("file")
    a = ap.parse_args(argv)
    if a.verb == "resolve":
        on = dt.date.fromisoformat(a.date)
        rows = resolve(on)
        with open(a.out, "w") as f:
            f.write(f"# W0-BENCH-PINS v2 — sk-v27 LW1.CON-2; resolved {on.isoformat()} by bench/pins/pins.py; "
                    f"rule: newest non-yanked release >= {MIN_AGE_DAYS} d old; check: pins.py check\n")
            f.write("\t".join(COLUMNS) + "\n")
            for row in rows:
                f.write("\t".join(row[c] for c in COLUMNS) + "\n")
        print(a.out)
        bad = [r for r in rows if r["departure"].startswith("UNRESOLVED")]
        for b in bad:
            print("UNRESOLVED", b["kind"], b["name"], b["departure"], file=sys.stderr)
        return 0
    errs = check(a.file)
    for e in errs:
        print("FAIL", e)
    print(f"{'OK' if not errs else 'RED'} {a.file}: {len(errs)} violation(s)")
    return 1 if errs else 0


if __name__ == "__main__":
    sys.exit(main())
