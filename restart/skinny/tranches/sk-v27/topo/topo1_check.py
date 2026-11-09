#!/usr/bin/env python3
"""LW1.TOPO-1 falsifier: every commit SHA cited in the sk-v27 records resolves
against the heads receipt, i.e. it is a commit in one of the receipted repos
and is reachable from a head that heads-W0.json records for that repo.

usage: topo1_check.py <heads-W0.json> [record.md ...]  (default: PLAN, LEDGER, RULINGS)

LW2.TOPO-0 adds two verified resolutions for tokens that are not programme
commits (both are lookups that can fail, not exemptions):
- an all-decimal token is a GitHub Actions run id, resolved by
  `gh api repos/<owner>/<repo>/actions/runs/<id>` against a receipted origin;
- a third-party pin is resolved by `git ls-remote <url> <ref>` through
  upstream-pins.tsv (token, url, ref), and must equal the token's prefix."""
import json, pathlib, re, subprocess, sys

HOME = pathlib.Path.home() / "Programming"
TR = pathlib.Path(__file__).resolve().parent.parent
receipt_path = pathlib.Path(sys.argv[1])
records = [pathlib.Path(p) for p in sys.argv[2:]] or [TR / n for n in ("PLAN.md", "LEDGER.md", "RULINGS.md")]
if not receipt_path.is_file():
    print(f"RED\n  no heads receipt at {receipt_path}"); sys.exit(1)
receipt = json.loads(receipt_path.read_text())["repos"]
TOK = re.compile(r"`([0-9a-f]{7,40})(?=[`:])")

def git(repo, *a, stdin=None):
    return subprocess.run(["git", "-C", str(HOME / repo), *a], input=stdin,
                          capture_output=True, text=True)

heads = {}
for repo, r in receipt.items():
    hs = set(r["remote"].values()) | set(r["local"].values()) | {w["head"] for w in r["worktrees"] if w["head"]}
    heads[repo] = sorted(h for h in hs if git(repo, "cat-file", "-e", h + "^{commit}").returncode == 0)

cited = sorted({t for p in records for t in TOK.findall(p.read_text())})
pins = {}
pin_file = pathlib.Path(__file__).resolve().parent / "upstream-pins.tsv"
if pin_file.is_file():
    for line in pin_file.read_text().splitlines()[1:]:
        if line.strip():
            t, url, ref = line.split("\t")
            pins[t] = (url, ref)
slugs = sorted({re.sub(r"(\.git)?$", "", r["origin"]).split("github.com")[-1].lstrip(":/")
                for r in receipt.values()})

def ci_run(tok):
    for slug in slugs:
        p = subprocess.run(["gh", "api", f"repos/{slug}/actions/runs/{tok}", "--jq", ".id"],
                           capture_output=True, text=True)
        if p.returncode == 0 and p.stdout.strip() == tok:
            return slug

def upstream(tok):
    if tok not in pins: return None
    url, ref = pins[tok]
    out = subprocess.run(["git", "ls-remote", url, ref], capture_output=True, text=True).stdout
    return url if any(l.split("\t")[0].startswith(tok) for l in out.splitlines()) else None

bad, ok, objs, runs, ups = [], 0, 0, 0, 0
for tok in cited:
    home = [repo for repo in receipt if git(repo, "rev-parse", "--verify", "-q", tok + "^{commit}").returncode == 0]
    if not home:
        if any(git(repo, "cat-file", "-e", tok).returncode == 0 for repo in receipt):
            objs += 1; continue  # a tree or blob id, not a commit
        if tok.isdigit() and ci_run(tok):
            runs += 1; continue  # a GitHub Actions run id, verified by the API
        if upstream(tok):
            ups += 1; continue  # a third-party pin, verified by ls-remote
        bad.append(f"{tok}: not a commit or object in any receipted repo"); continue
    reach = [repo for repo in home if git(repo, "rev-list", "--count", tok, "--stdin",
                                          stdin="".join(f"^{h}\n" for h in heads[repo])).stdout.strip() == "0"]
    if reach: ok += 1
    else: bad.append(f"{tok}: commit in {home} but reachable from no receipted head")
print("RED" if bad else "GREEN", f"{ok}/{len(cited) - objs - runs - ups} cited commits resolve ({objs} non-commit object ids, {runs} CI run ids, {ups} upstream pins verified)", *bad, sep="\n  ")
sys.exit(1 if bad else 0)
