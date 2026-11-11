#!/usr/bin/env python3
"""LW1.TOPO-2: ownership record. Read-only over git and the workflow journals.
usage: topo2_ownership.py <out-dir> <claude-worktrees.tsv>"""
import datetime, fnmatch, json, os, pathlib, subprocess, sys

P = pathlib.Path.home() / "Programming"
PROJ = pathlib.Path.home() / ".claude" / "projects"
out, wt_tsv = pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2])

def git(repo, *a):
    return subprocess.run(["git", "-C", str(P / repo), *a], capture_output=True, text=True).stdout

def ls(repo, ref, glob):
    return [p for p in git(repo, "ls-tree", "-r", "--name-only", ref).splitlines()
            if fnmatch.fnmatchcase(p, glob)]  # root-anchored; '*' spans '/'


def journal(wf_glob):
    j = next(PROJ.glob(f"*/*/subagents/workflows/{wf_glob}/journal.jsonl"))
    L = [json.loads(l) for l in j.open()]
    st = {o["key"] for o in L if o["type"] == "started"}
    rs = {o["key"] for o in L if o["type"] == "result"}
    return {"journal": str(j).replace(str(pathlib.Path.home()), "~"), "seats_started": len(st),
            "seats_resolved": len(st & rs), "last_write": datetime.datetime.fromtimestamp(
                j.stat().st_mtime).astimezone().isoformat(timespec="seconds")}

CLOSE = "4ad2ca653"
close = {"workflow": "wf_d837f96c", "label": "xp-w9-w10-continue", "repo": "value.js",
         "commit": git("value.js", "rev-parse", CLOSE).strip(),
         "subject": git("value.js", "log", "-1", "--format=%s", CLOSE).strip(),
         "on_origin": sorted(r.strip() for r in git("value.js", "branch", "-r", "--contains", CLOSE).splitlines()),
         "file": "docs/tranches/X/COHESION.md",
         "line": "**The xp-w9-w10-continue workflow is DONE.** bbnf-lang 0.3.0 and value.js 4.2.1 are on npm, both with SLSA provenance; parse-that and bbnf-lang now have their own publish workflows.",
         **journal("wf_d837f96c-*")}
REL = f"released by the wf_d837f96c close ({CLOSE}); no contest/ci route needed"
VJH = "value.js app-work hold (megatranche-realization / owner-1105-eve); value.js makes and releases the edit (LW1.USE-5)"
holds = [
    ("bbnf-lang", "c9b93adb4", "bbnf-lang/typescript/**", "typescript/**", REL),
    ("bbnf-lang", "c9b93adb4", "bbnf-lang/.github/workflows/*release*", ".github/workflows/*release*", REL),
    ("parse-that", "origin/master", "parse-that/.github/workflows/**", ".github/workflows/**", REL),
    ("parse-that", "origin/master", "parse-that/typescript/package.json", "typescript/package.json", REL),
    ("parse-that", "origin/master", "parse-that/CHANGELOG.md", "CHANGELOG.md", REL),
    ("value.js", "origin/tranche-u", "package.json", "package.json", VJH),
    ("value.js", "origin/tranche-u", "pnpm-lock.yaml", "package-lock.json", VJH),
    ("value.js", "origin/tranche-u", "gen-grammar.mjs", "scripts/gen-grammar.mjs", VJH),
    ("value.js", "origin/tranche-u", "generated grammar modules", "src/css/bbnf/generated/**", VJH),
]
rec_holds = []
for repo, ref, plan_glob, glob, status in holds:
    m = ls(repo, ref, glob)
    note = None
    if plan_glob == "pnpm-lock.yaml": note = "value.js has no pnpm-lock.yaml; its lockfile is package-lock.json (the PLAN name is corrected here)"
    if not m: note = f"no path matches at {ref} (the hold is vacuous today; it binds a future file)"
    rec_holds.append({"repo": repo, "plan_glob": plan_glob, "glob": glob, "read_at": ref,
                      "holder_until_close": "wf_d837f96c", "status": status, "matches": m, "note": note})
rows = [l.split("\t") for l in wt_tsv.read_text().splitlines()]
real = [r for r in rows if not os.path.islink(P / "bbnf-lang/.claude/worktrees" / r[0])]
links = [{"name": r[0], "target": os.readlink(P / "bbnf-lang/.claude/worktrees" / r[0])} for r in rows if r not in real]
cw = {"entries": len(rows), "registered_worktrees": len(real), "symlinks": links,
      "dirty": sum(int(r[3]) > 0 for r in real),
      "head_in_local_master": sum(r[5] == "yes" for r in real),
      "commits_on_no_origin_ref_and_not_in_local_master": [
          {"worktree": r[0], "branch": r[1], "head": r[2], "commits_off_origin": int(r[4]), "dirty": int(r[3]), "date": r[6]}
          for r in real if r[4] and int(r[4]) > 0 and r[5] == "no"],
      "fate": "untouched (read-only); per-entry fate at LW7 with the dirty tree (X-6)", "table": "claude-worktrees.tsv"}
rec = {"taken": datetime.datetime.now().astimezone().isoformat(timespec="seconds"),
       "close_record": close, "holds": rec_holds,
       "app_work_holds": {"value.js": ["megatranche-realization", "x-0hr-residuals-2", "owner-1105-eve"],
                          "daraksha": ["daraksha-v3-repair", "owner-1105-eve"],
                          "keyframes.js": ["owner-1105-eve"], "fourier-analysis": ["owner-1105-eve"],
                          "never": ["value.js scripts/dev/dev.sh"]},
       "workflows": {"wf_d837f96c": "CLOSED (close_record)",
                     "wf_fce2ec2b": {**journal("wf_fce2ec2b-*"), "state": "DORMANT: no journal write since 2026-07-16; 25 seats never resolved; holds nothing in this programme"}},
       "bbnf_lang_claude_worktrees": cw}
(out / "ownership-W0.json").write_text(json.dumps(rec, indent=1) + "\n")
print("ok", len(rec_holds), "holds;", cw["registered_worktrees"], "worktrees +", len(links), "symlinks")
