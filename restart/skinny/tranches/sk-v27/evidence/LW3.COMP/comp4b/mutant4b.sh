#!/bin/sh
export CARGO_TARGET_DIR=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/target-parse-that
W=/Users/mkbabb/Programming/parse-that-wt/lw3-s6-kernel; L=/Users/mkbabb/.dev-logs/bbnf-lockstep/lw3/s6-kernel/logs
cd $W || exit 2
mut() { # name file python-old python-new cmd...
  name=$1; f=$2; old=$3; new=$4; shift 4
  for n in 1 2; do
    o=$L/c4b-mutant-$name-$n.log
    python3 -c "import sys;p,o,n=sys.argv[1:];s=open(p).read();assert s.count(o)==1,'mutation';open(p,'w').write(s.replace(o,n))" "$f" "$old" "$new" || exit 3
    { echo "# head $(git rev-parse HEAD) + mutant $name"; git diff -- $f; } > $o
    (eval "$@") >> $o 2>&1; echo "# rc $?" >> $o
    git checkout -- $f; git diff --quiet -- $f && echo "# restored byte-equal" >> $o
  done
}
mut kernel-insertion-order rust/parse_that/src/kernel/outcome.rs \
 'core::cmp::Ordering::Equal => Self {
                at: self.at,
                expected: self.expected.union(other.expected),
            },' 'core::cmp::Ordering::Equal => self,' \
 'cd rust && cargo test --locked -p parse_that --test kernel failure_contract'
mut legacy-insertion-order rust/parse_that/src/state.rs \
 'if let Err(at) = self.expected.binary_search(&_label) {
                        self.expected.insert(at, _label);
                    }' 'if !self.expected.contains(&_label) {
                        self.expected.push(_label);
                    }' \
 'cd rust && cargo test --locked -p parse_that --features diagnostics --test expected_sorted_test'
mut ts-insertion-order typescript/src/parse/state.ts \
 'export function insertExpected(set: string[], label: string): void {' 'export function insertExpected(set: string[], label: string): void {
    if (!set.includes(label)) { set.push(label); }
    return;' \
 'cd typescript && npx vitest run --maxWorkers=2 test/expected-set.test.ts'
mut abi-three rust/parse_that/src/kernel/mod.rs 'pub const KERNEL_ABI: u32 = 4;' 'pub const KERNEL_ABI: u32 = 3;' \
 'cd rust && cargo test --locked -p parse_that --test kernel --test kernel_abi'
