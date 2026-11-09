#!/bin/zsh
# usage: topo6-falsifier.sh <repo> <sha> <cargo-dir-rel> <shared-target>
# Clean checkout of <sha>; corpus never fetched -> verify RED x2; tampered asset refused x2;
# fetch from the public release URL with no credentials -> GREEN; verify GREEN x2.
set -u
repo=$1 sha=$2 sub=$3 tgt=$4
L=~/.dev-logs/bbnf-lockstep/lw2/s1-custody/topo6
W=~/.dev-logs/bbnf-lockstep/lw2/s1-custody/topo6/clean-$repo
rm -rf $W
git clone -q --no-local --filter=blob:none https://github.com/mkbabb/$repo $W || exit 90
cd $W && git -c advice.detachedHead=false checkout -q $sha || exit 91
echo "== clean checkout $repo $(git rev-parse HEAD); data/ present: $(test -e data && echo yes || echo no)"
cd $W/$sub
export CARGO_TARGET_DIR=$tgt GIT_TERMINAL_PROMPT=0
unset BBNF_CORPUS_DIR PARSE_THAT_CORPUS_DIR
cargo build -q -p xtask --profile ax-iter 2>&1 | tail -3
X=$tgt/ax-iter/xtask
for i in 1 2; do echo "== RED verify #$i (corpus not fetched)"; $X corpus verify; echo "rc=$?"; done
asset=$(grep -v '^#' $W/corpus/MANIFEST.sha256 | awk '$2 !~ /\//{print $2}')
url="https://github.com/mkbabb/$repo/releases/download/${asset%.tar.gz}/$asset"
mkdir -p $L/tamper-$repo && curl -fsSL -o $L/tamper-$repo/$asset $url
cp $L/tamper-$repo/$asset $L/tamper-$repo/tampered.tar.gz
python3 -c "import sys;p=sys.argv[1];b=bytearray(open(p,'rb').read());b[len(b)//2]^=1;open(p,'wb').write(b)" $L/tamper-$repo/tampered.tar.gz
for i in 1 2; do echo "== RED tamper #$i (one byte flipped)"; $X corpus fetch --asset $L/tamper-$repo/tampered.tar.gz; echo "rc=$?"; echo "data/ files after refusal: $(find $W/data -type f 2>/dev/null | wc -l)"; done
echo "== GREEN fetch (public URL, no token, no credentials)"; env -u GH_TOKEN -u GITHUB_TOKEN $X corpus fetch; echo "rc=$?"
for i in 1 2; do echo "== GREEN verify #$i"; $X corpus verify; echo "rc=$?"; done
echo "== twitter.json: $(shasum -a 256 $W/data/json/twitter.json | cut -c1-64)"
cd ~ && rm -rf $W
