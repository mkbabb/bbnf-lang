#!/usr/bin/env python3
"""LW3.TOPO-0: re-read the entry facts of LW3-ORDER §0 (and the three facts
amendment 1 adds) at the heads in heads-LW3.json, by light reads only (git
object reads, the GitHub API, crates.io and npm metadata, `rustc --version`).

A fact whose read differs from the order's value is DRIFT and gets its own
LEDGER row. A fact that only a build or a run can read (for example "about 4 of
the 63 MANIFEST grammars compile under the spine") is UNIT-RED: it is not read
here, and the row names the unit whose born-RED read takes it.

usage: topo0_lw3_facts.py <heads-LW3.json> <out.tsv>"""
import json, pathlib, re, subprocess, sys, urllib.request

P = pathlib.Path.home() / "Programming"
heads = json.loads(pathlib.Path(sys.argv[1]).read_text())["repos"]
out = pathlib.Path(sys.argv[2])
BL, PT, PP = (heads[r]["remote"]["refs/heads/sk-v27-lw3"] for r in ("bbnf-lang", "parse-that", "pprint"))
MASTER = {r: heads[r]["remote"]["refs/heads/master"] for r in ("bbnf-lang", "parse-that", "pprint")}


def sh(*a, repo=None, ok=(0,)):
    p = subprocess.run((["git", "-C", str(P / repo)] if repo else []) + list(a),
                       capture_output=True, text=True)
    if p.returncode not in ok:
        raise RuntimeError(f"{a}: rc {p.returncode}: {p.stderr.strip()[:200]}")
    return p.stdout


def git(repo, *a, ok=(0,)):
    return sh(*a, repo=repo, ok=ok)


def blob(repo, rev, path):
    return git(repo, "show", f"{rev}:{path}")


def grep_count(repo, rev, pattern, *paths, flags="-nP"):
    o = git(repo, "grep", flags, pattern, rev, "--", *paths, ok=(0, 1))
    return len([l for l in o.splitlines() if l.strip()])


def conflicts(repo, a, b):
    o = git(repo, "merge-tree", "--write-tree", "--name-only", "--no-messages", a, b, ok=(0, 1))
    return len(o.splitlines()[1:]) if o else 0


def http_json(url):
    req = urllib.request.Request(url, headers={"User-Agent": "lockstep-entry-read"})
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.load(r)


rows = []


def fact(fid, claim, order_value, read):
    try:
        val, verdict = read()
    except Exception as e:  # a read that cannot be taken is RED, never HOLDS
        val, verdict = f"read failed: {e}", "UNREAD"
    rows.append((fid, claim, str(order_value), str(val), verdict))


def eq(read_value, want):
    return read_value, ("HOLDS" if read_value == want else "DRIFT")


# ── Masters ────────────────────────────────────────────────────────────────
for repo, want in (("bbnf-lang", "29d4fd5ef"), ("parse-that", "f7ac459"), ("pprint", "dccbd63")):
    fact(f"F0.master.{repo}", f"{repo} origin/master", want,
         lambda repo=repo, want=want: (MASTER[repo][:9], "HOLDS" if MASTER[repo].startswith(want) else "DRIFT"))

# ── The spine ──────────────────────────────────────────────────────────────
SP = "b5fd0ca98"
fact("F1.spine.ref", "origin codex/sk-v26-bbnf", SP,
     lambda: (lambda s: (s[:9], "HOLDS" if s.startswith(SP) else "DRIFT"))(
         heads["bbnf-lang"]["remote"].get("refs/heads/codex/sk-v26-bbnf", "absent")))
fact("F1.spine.count", "commits on af15f63e0", 24,
     lambda: eq(int(git("bbnf-lang", "rev-list", "--count", f"af15f63e0..{SP}").strip()), 24))
fact("F1.spine.not-ancestor", "spine is not an ancestor of master", "not ancestor",
     lambda: eq("not ancestor" if subprocess.run(["git", "-C", str(P / "bbnf-lang"), "merge-base",
                "--is-ancestor", SP, MASTER["bbnf-lang"]]).returncode else "ancestor", "not ancestor"))
fact("F1.integration-backend", "integration/backend on origin", "absent",
     lambda: eq("present" if "refs/heads/integration/backend" in heads["bbnf-lang"]["remote"] else "absent", "absent"))
fact("F1.merge-tree", "conflicted paths, merge-tree master x spine", 15,
     lambda: eq(conflicts("bbnf-lang", MASTER["bbnf-lang"], SP), 15))

# ── The silent hazard ──────────────────────────────────────────────────────
fact("F2.lib-rs", "spine crates/core/src/lib.rs `pub mod` lines", "pub mod compiler; only",
     lambda: eq("; ".join(l.strip() for l in blob("bbnf-lang", SP, "crates/core/src/lib.rs").splitlines()
                          if re.match(r"\s*pub mod", l)), "pub mod compiler;"))
fact("F2.xtask", "spine deletes xtask/", "deleted",
     lambda: eq("deleted" if not git("bbnf-lang", "ls-tree", SP, "xtask").strip() else "present", "deleted"))
fact("F2.root-modules", "master crates/core/src/lib.rs root modules exported",
     "types grammar generate backend graph imports lower path pipeline runtime",
     lambda: (lambda mods: (" ".join(sorted(mods)), "HOLDS" if {"types", "grammar", "generate", "backend",
              "graph", "imports", "lower", "path", "pipeline", "runtime"} <= mods else "DRIFT"))(
         set(re.findall(r"^\s*pub mod (\w+)", blob("bbnf-lang", BL, "crates/core/src/lib.rs"), re.M))))

# ── The spine's runtime ────────────────────────────────────────────────────
K = "e31fbfedc"
fact("F3.kernel-abi.sk-v26", "KERNEL_ABI defined at PT e31fbfedc", "present",
     lambda: eq("present" if grep_count("parse-that", K, r"const KERNEL_ABI", "rust") else "absent", "present"))
fact("F3.kernel-abi.master", "KERNEL_ABI at PT master", "absent",
     lambda: eq("present" if grep_count("parse-that", PT, r"KERNEL_ABI", "rust") else "absent", "absent"))
fact("F3.spine-asserts", "spine asserts KERNEL_ABI == 3", "== 3",
     lambda: eq("== 3" if grep_count("bbnf-lang", SP, r"KERNEL_ABI.{0,12}3", "crates") else "absent", "== 3"))
fact("F3.pt-conflicts", "conflicted paths, merge-tree PT master x e31fbfedc", 96,
     lambda: eq(conflicts("parse-that", PT, K), 96))
fact("F3.pt-diffstat", "e31fbfedc against its merge-base with PT master (+ / -)", "+5749 / -26812",
     lambda: (lambda mb: (lambda s: (s, "HOLDS" if s == "+5749 / -26812" else "DRIFT"))(
         (lambda t: f"+{sum(int(x.split()[0]) for x in t if x.split()[0] != '-')} / "
                    f"-{sum(int(x.split()[1]) for x in t if x.split()[1] != '-')}")(
             git("parse-that", "diff", "--numstat", mb, K).splitlines())))(
         git("parse-that", "merge-base", PT, K).strip()))

# ── The spine's front end ──────────────────────────────────────────────────
fact("F4.frontend", "spine front end is hand-written (compiler/frontend.rs)", "hand-written",
     lambda: eq("hand-written" if git("bbnf-lang", "ls-tree", SP, "crates/core/src/compiler/frontend.rs").strip()
                else "absent", "hand-written"))
fact("F4.manifest-compile", "about 4 of 63 MANIFEST grammars compile under the spine", "~4 / 63",
     lambda: ("not read here (needs a build)", "UNIT-RED: LW3.COMP-1c spine_manifest"))
fact("F4.manifest-entries", "grammar/MANIFEST entries", 63,
     lambda: eq(len([l for l in blob("bbnf-lang", BL, "grammar/MANIFEST").splitlines()
                     if l.strip() and not l.startswith("#")]), 63))

# ── What the spine lacks ───────────────────────────────────────────────────
fact("F5.recursive-rule", "spine rejects left recursion as RecursiveRule", "present",
     lambda: eq("present" if grep_count("bbnf-lang", SP, r"RecursiveRule", "crates/core/src/compiler") else "absent", "present"))
fact("F5.organs", "spine compiler names egraph / csp / OptIR / plan.json / KernelId", 0,
     lambda: eq(grep_count("bbnf-lang", SP, r"egraph|EGraph|csp_solver|OptIr|plan\.json|KernelId",
                           "crates/core/src/compiler", flags="-nP"), 0))

# ── LW2 trial winners ──────────────────────────────────────────────────────
fact("F6.trials", "F-TRIAL-EGRAPH / F-TRIAL-CSP records", 0,
     lambda: eq(grep_count("bbnf-lang", BL, r"F-TRIAL-(EGRAPH|CSP)", "docs/benchmarks/records"), 0))

# ── obs() and the failure contract ─────────────────────────────────────────
fact("F7.obs", "`fn obs` / `ObsClass` in the three repos' code (records excluded)", 0,
     lambda: eq(sum(grep_count(r, h, r"fn obs\b|ObsClass", ".", ":!restart", ":!*.md") for r, h in
                    (("bbnf-lang", BL), ("parse-that", PT), ("pprint", PP))), 0))

# ── Oracles ────────────────────────────────────────────────────────────────
fact("F8.jsontestsuite", "JSONTestSuite in corpus/MANIFEST.sha256", "absent",
     lambda: eq("present" if grep_count("bbnf-lang", BL, r"(?i)jsontestsuite|/y_|/n_", "corpus", flags="-nP") else "absent", "absent"))
fact("F8.oracle-crate", "crates/oracle", "absent",
     lambda: eq("present" if git("bbnf-lang", "ls-tree", BL, "crates/oracle").strip() else "absent", "absent"))

# ── Held-out grammars ──────────────────────────────────────────────────────
fact("F9.toml", "a TOML grammar under grammar/", "absent",
     lambda: eq("present" if [l for l in git("bbnf-lang", "ls-tree", "-r", "--name-only", BL, "grammar").splitlines()
                              if "toml" in l.lower()] else "absent", "absent"))
fact("F9.csv-training", "grammar/misc/csv.bbnf is one of the 9 (regen entries)", "training",
     lambda: eq("training" if "csv" in blob("bbnf-lang", BL, "grammar/MANIFEST") else "absent", "training"))
fact("F9.prereg-heldout", "PREREG held-out section", "absent",
     lambda: eq("present" if grep_count("bbnf-lang", BL, r"(?i)^#+ .*held.?out", "bench") else "absent", "absent"))

# ── Regex ──────────────────────────────────────────────────────────────────
fact("F10.hir-fingerprint", "Hir ABI v1 fingerprint", "0x0c587e007f6ccf7d",
     lambda: eq("0x0c587e007f6ccf7d" if grep_count("parse-that", PT, r"0x0c587e007f6ccf7d", "rust/regex") else "absent",
                "0x0c587e007f6ccf7d"))
fact("F10.dfa-longest", "rust/regex/src/automata/dfa.rs:115 is leftmost-longest", "longest at :115",
     lambda: (lambda ls: (ls[114].strip()[:90], "HOLDS" if re.search(r"(?i)longest", "\n".join(ls[110:120])) else "DRIFT"))(
         blob("parse-that", PT, "rust/regex/src/automata/dfa.rs").splitlines()))
fact("F10.route-a", "spike/or7-route-a", "5999f9c",
     lambda: (lambda s: (s[:9], "HOLDS" if s.startswith("5999f9c") else "DRIFT"))(
         heads["parse-that"]["remote"].get("refs/heads/spike/or7-route-a", "absent")))
fact("F10.parser-lines", "rust/regex/src/hir/parser/*.rs lines", 799,
     lambda: eq(sum(len(blob("parse-that", PT, f).splitlines()) for f in
                    git("parse-that", "ls-tree", "-r", "--name-only", PT, "rust/regex/src/hir/parser").split()), 799))
fact("F10.generality-regex", "lines naming bbnf under PT rust/regex (amendment 1)", 17,
     lambda: eq(grep_count("parse-that", PT, "bbnf", "rust/regex", flags="-niP"), 17))
fact("F10.generality-bootstrap", "lines naming bbnf under PT rust/bootstrap (amendment 1)", 13,
     lambda: eq(grep_count("parse-that", PT, "bbnf", "rust/bootstrap", flags="-niP"), 13))
fact("F10.born-red-ignores", "#[ignore] attributes in PT rust/regex/tests/born_red_lw1.rs", 9,
     lambda: eq(len(re.findall(r"^\s*#\[ignore", blob("parse-that", PT, "rust/regex/tests/born_red_lw1.rs"), re.M)), 9))

# ── pprint ─────────────────────────────────────────────────────────────────
fact("F11.unsafe", "`unsafe` lines in pprint rust/src", 1,
     lambda: eq(grep_count("pprint", PP, r"unsafe", "rust/src"), 1))
fact("F11.width-rs", "pprint rust/src/width.rs", "absent",
     lambda: eq("present" if git("pprint", "ls-tree", PP, "rust/src/width.rs").strip() else "absent", "absent"))
fact("F11.version", "pprint master version line", "0.5.0-alpha.0",
     lambda: eq(re.search(r'^version\s*=\s*"([^"]+)"', blob("pprint", PP, "rust/Cargo.toml"), re.M).group(1), "0.5.0-alpha.0"))
fact("F11.published", "pprint newest on crates.io", "0.4.0",
     lambda: eq(http_json("https://crates.io/api/v1/crates/pprint")["crate"]["max_version"], "0.4.0"))
fact("F11.pt-depends-pp", "PT depends on pprint (PP-1's reverse-edge fact)", "depends",
     lambda: eq("depends" if grep_count("parse-that", PT, r"^pprint\s*=", "rust", flags="-nP") else "no edge", "depends"))

# ── The contest ────────────────────────────────────────────────────────────
fact("F12.decision-plan", "sha256 of bench/DECISION-PLAN.lw2.json", "9882d5a1e004…",
     lambda: (lambda h: (h[:12], "HOLDS" if h.startswith("9882d5a1e0048c34") else "DRIFT"))(
         __import__("hashlib").sha256(subprocess.run(["git", "-C", str(P / "bbnf-lang"), "show",
             f"{BL}:bench/DECISION-PLAN.lw2.json"], capture_output=True).stdout).hexdigest()))
fact("F12.arms", "contest arms on disk (node_json_parse.mjs is the one)", "1 arm",
     lambda: (lambda fs: (", ".join(fs) or "none", "HOLDS" if fs == ["node_json_parse.mjs"] else "DRIFT"))(
         sorted(pathlib.PurePath(f).name for f in git("bbnf-lang", "ls-tree", "-r", "--name-only", BL, "bench/contest").split()
                if "/arms/" in f)))

# ── Carried rows ───────────────────────────────────────────────────────────
fact("F13.iai-apt", "bench-iai.yml installs valgrind from the live archive", "apt-get",
     lambda: eq("apt-get" if re.search(r"apt-get install -y valgrind", blob("bbnf-lang", BL, ".github/workflows/bench-iai.yml")) else "absent", "apt-get"))
fact("F13.iai-stub", "docs/benchmarks/iai-baselines/json.json valgrind_instrs", "null",
     lambda: eq("null" if re.search(r'"valgrind_instrs"\s*:\s*null', blob("bbnf-lang", BL, "docs/benchmarks/iai-baselines/json.json")) else "set", "null"))
fact("F13.iai-trigger", "bench-iai.yml triggers (amendment)", "pull_request only",
     lambda: (lambda y: eq("pull_request only" if "pull_request" in y and "workflow_dispatch" not in y and "push:" not in y else "other", "pull_request only"))(
         blob("bbnf-lang", BL, ".github/workflows/bench-iai.yml")))

# ── value.js ───────────────────────────────────────────────────────────────
fact("F14.npm-latest", "npm @mkbabb/value.js latest", "4.3.0",
     lambda: eq(http_json("https://registry.npmjs.org/@mkbabb/value.js")["dist-tags"]["latest"], "4.3.0"))
fact("F14.files", "value.js package.json `files` at the latest tag omits the six .bbnf", "omits",
     lambda: (lambda m: eq("omits" if not any("bbnf" in f for f in m["files"]) else "carries", "omits"))(
         json.loads(blob("value.js", "v" + http_json("https://registry.npmjs.org/@mkbabb/value.js")["dist-tags"]["latest"], "package.json"))))

# ── Amendment-1 facts ──────────────────────────────────────────────────────
fact("A1.stable", "rustc +stable on this machine", "1.97.0",
     lambda: (lambda v: (v, "HOLDS" if v.startswith("rustc 1.97.0") else "DRIFT"))(
         sh("rustc", "+stable", "--version").strip()))
fact("A1.pin", "rust-toolchain.toml channel", "1.99.1",
     lambda: eq(re.search(r'channel\s*=\s*"([^"]+)"', blob("bbnf-lang", BL, "rust-toolchain.toml")).group(1), "1.99.1"))
fact("A1.divergence", "grammar/DIVERGENCE.tsv absent; SK/grammar/DIVERGENCE.tsv present", "absent / present",
     lambda: eq(("present" if git("bbnf-lang", "ls-tree", BL, "grammar/DIVERGENCE.tsv").strip() else "absent") + " / " +
                ("present" if git("bbnf-lang", "ls-tree", BL, "restart/skinny/tranches/sk-v27/grammar/DIVERGENCE.tsv").strip() else "absent"),
                "absent / present"))
fact("A1.path-edges", "in-repo `path = \"../` edges in master *.toml", 49,
     lambda: eq(grep_count("bbnf-lang", BL, r'path = "\.\./', "*.toml"), 49))
fact("A1.spine-manifest", "spine crates/core/Cargo.toml autotests / version", "autotests = false, 0.4.0",
     lambda: (lambda t: eq(("autotests = false" if re.search(r"autotests\s*=\s*false", t) else "autotests default") + ", " +
                           re.search(r'^version\s*=\s*"([^"]+)"', t, re.M).group(1), "autotests = false, 0.4.0"))(
         blob("bbnf-lang", SP, "crates/core/Cargo.toml")))

out.write_text("fact\tclaim\torder\tread\tverdict\n" +
               "".join("\t".join(r) + "\n" for r in rows))
n = {v: sum(1 for r in rows if r[4].split(":")[0] == v) for v in ("HOLDS", "DRIFT", "UNIT-RED", "UNREAD")}
print(f"{len(rows)} facts: " + ", ".join(f"{k} {v}" for k, v in n.items()))
for r in rows:
    if r[4] != "HOLDS":
        print("  " + " | ".join(r))
