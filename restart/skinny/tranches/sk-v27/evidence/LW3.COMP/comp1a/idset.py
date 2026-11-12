#!/usr/bin/env python3
"""idset.py <list-log>: print `target<TAB>id` for every test a
`cargo test -- --list --format terse` log names (targets by source path;
doc-tests by crate)."""
import re, sys
target = None
for line in open(sys.argv[1], errors="replace"):
    line = line.rstrip("\n")
    m = re.match(r"\s+Running (\S+) \(.*/deps/(.+)-[0-9a-f]{16}\)", line)
    if m:
        target = f"{m.group(2)}:{m.group(1)}"; continue
    m = re.match(r"\s+Doc-tests (\S+)", line)
    if m:
        target = "doc:" + m.group(1); continue
    m = re.match(r"(.+): (test|bench)$", line)
    if m and target:
        print(f"{target}\t{m.group(1)}")
