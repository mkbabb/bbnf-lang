#!/usr/bin/env python3
"""mutant.py <rev> <path> <python-regex> <replacement>: print a commit sha whose
tree is <rev>'s with one substitution in <path>; no ref is moved."""
import os, re, subprocess, sys, tempfile
rev, path, pat, rep = sys.argv[1:5]
src = subprocess.check_output(["git", "show", f"{rev}:{path}"]).decode()
new, n = re.subn(pat, rep, src, count=1, flags=re.M)
assert n == 1, "mutation did not apply"
blob = subprocess.run(["git", "hash-object", "-w", "--stdin"], input=new.encode(), capture_output=True, check=True).stdout.decode().strip()
idx = tempfile.mktemp(dir=os.path.expanduser("~/.dev-logs/bbnf-lockstep/lw3/r0-fold"))
env = dict(os.environ, GIT_INDEX_FILE=idx)
subprocess.run(["git", "read-tree", rev], env=env, check=True)
subprocess.run(["git", "update-index", "--cacheinfo", f"100644,{blob},{path}"], env=env, check=True)
tree = subprocess.check_output(["git", "write-tree"], env=env).decode().strip()
os.unlink(idx)
c = subprocess.check_output(["git", "commit-tree", tree, "-p", rev, "-m", "mutant (R0-FOLD falsifier; unreferenced)"]).decode().strip()
print(c)
