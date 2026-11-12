#!/usr/bin/env bash
# The step runner the falsifiers use inside one outer `guarded` run: drops
# the guard options up to `--` and runs the command in the outer run's tree.
while [ $# -gt 0 ] && [ "$1" != "--" ]; do shift; done
[ "${1:-}" = "--" ] && shift
exec "$@"
