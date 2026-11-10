"""S14 P4 (S12-P4-4): every publishable package in bbnf-lang names its
repository (check-repo-dag.sh maps registry packages by that field), and no
skinny lab package is publishable (X-1: the lab is admitted by census, never
published). usage: python3 -I publish-scope.py <worktree> [held...]; rc 1 = RED.
A held package is printed as HELD, never as ok: it names a census MOVE row
whose cure is the crate leaving the repository (simd-scan, PERF-1)."""
import json, subprocess, sys
root = sys.argv[1]; held = set(sys.argv[2:]); bad = 0
for ws in (".", "skinny"):
    meta = json.loads(subprocess.run(
        ["cargo", "metadata", "--no-deps", "--offline", "--format-version", "1",
         "--manifest-path", f"{root}/{ws}/Cargo.toml"],
        check=True, capture_output=True, text=True).stdout)
    for p in sorted(meta["packages"], key=lambda p: p["name"]):
        if p["publish"] == []:
            continue
        if ws == "skinny":
            print(f"RED {ws} {p['name']}: lab package is publishable"); bad += 1
        elif not p.get("repository") and p["name"] in held:
            print(f"HELD {ws} {p['name']}: no repository; leaves with its census MOVE row")
        elif not p.get("repository"):
            print(f"RED {ws} {p['name']}: publishable with no repository"); bad += 1
        else:
            print(f"ok  {ws} {p['name']}: {p['repository']}")
print("RED" if bad else "GREEN", f"({bad})")
sys.exit(1 if bad else 0)
