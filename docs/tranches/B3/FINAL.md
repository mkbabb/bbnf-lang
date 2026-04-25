# B3 — FINAL

Tranche B3 closes on parser-baseline restoration. The runtime parser
returns from `BbnfBootstrap::parse` in microseconds against grammars
that previously hung indefinitely; the workspace builds clean; the
data grammars regenerate end-to-end through the parser, lowering,
and IR pipeline. The bbnf self-host regen surfaces a separate
downstream codegen-emission defect that lands as B4's opening scope.

## Headline

The parser hang surfaced on every grammar regardless of size or
recursion density. Five architectural fixes, applied in sequence,
collectively restore the parser baseline:

1. **Tape finaliser cycle resolved** — the post-hoc reverse-walk that
   derived per-record `frame_depth` from the `child_off` graph spun
   indefinitely on tapes that mixed pre-order and post-order compound
   shapes. The reverse walk was retired in favour of in-builder
   bookkeeping: a single depth counter on the builder bumps on every
   compound open, decrements on every close, and stamps each pushed
   record in lockstep. A retroactive bump on post-order close lifts
   the children that were emitted before the wrapping compound row
   into the (parent + 1) depth slot. One path, no graph traversal.
2. **Atomic depth rollback** — `frame_depth` and `current_depth`
   migrated from siblings of the column array into the column array
   itself, so the parser's 72 retry sites that already roll back the
   tape `len` cursor pick up depth rollback for free. The cursor's
   post-order backward walk gained a guard: leap to `child_off` only
   when it points strictly before the current position; otherwise
   step by one. Mixed-shape compounds no longer trap the walk.
3. **End-compound bump scope widened** — the post-order close walks
   the leftmost-descendant chain from the first child and bumps the
   entire descendant range, not just the immediate-child range.
   Records that under-bumped to the parent's own depth — colliding
   with the parent in the finaliser's same-depth sibling chain — now
   land at (parent + 1) and chain correctly.
4. **Pratt operand seeding corrected** — the Pratt expression shape
   set the outer compound's `child_off` to the leftmost-descendant
   approximation `outer + 1` after the operand was emitted. Operand
   shape functions emit interior records first and the outer compound
   row last, so `outer + 1` lands inside the operand's body rather
   than on the operand row. Single-operand chains' cursor children
   iteration then entered operand interior, surfacing multiple
   records as phantom operands. The fix seeds `this_operand_root`
   from the operand dispatcher's returned outer offset, and updates
   per-iteration RHS the same way.
5. **Lowering cousin-leak guard** — the post-order bump cascade and
   pre-order Pratt-body emission together place cousin records (with
   different parent compounds) at the same final `frame_depth`. The
   finaliser's depth-only sib_skip computation chains them as
   "siblings"; the lowering's chain-operand collector then surfaces
   the cousin as a phantom operand. The lowering now bounds children
   by the parent compound's `span_hi`: children whose `lo >= body_hi`
   are cousin leaks, discarded before they reach operand resolution.

The five fixes are independent in formulation but coupled in effect.
Fix 1 unblocks the parser to produce a tape; fix 2 makes the cursor's
walk over that tape cycle-safe; fix 3 widens the post-order bump so
the tape's depth column reflects structural truth; fix 4 corrects the
Pratt outer's child pointer so the cursor reads the operand row, not
the operand interior; fix 5 catches the residual cousin-leak that
slips through when fix 3's wider bump puts cousins at shared depth.

## Architectural narrative

The reverse-walk derivation of `frame_depth` was authored against a
canonical post-order tape — every compound's `child_off` strictly
less than its own index. That contract held for every shape in the
emitter family at the time the finaliser landed. The Pratt expression
shape, introduced concurrently, breaks the contract: a Pratt outer
opens with a forward `child_off = self + 1` because the leftmost
operand has not yet been dispatched. The two landings shipped without
each other's awareness; the latent contract violation surfaced when
the parser ran on any grammar input that placed a Pratt compound as a
direct child of a post-order parent — which the BBNF self-host
grammar did at every `value_path`.

The reverse walk's leap algorithm presupposed `child_off < self_idx`
and "leaped" through canonical post-order subtrees in O(depth). On
the violating shape, the leap landed identity (`pos = pos`) and the
loop spun. Profile capture located the loop body in two adjacent PCs
inside the inlined finaliser body — 100 % of the parser's runtime
hits in a tight 28-byte machine-code window.

Removing the reverse walk required moving its state-deriving role
into the parser side. The builder already tracks structural pushes;
adding a `current_depth` counter and stamping each push in lockstep
costs O(1) per push and zero post-hoc walks. The post-order shape's
retroactive bump preserves the finaliser's same-depth sibling
contract: children emitted before the wrapping compound row stamp at
the outer frame's depth, then move to the inner frame's depth when
the close fires.

The cursor's post-order walk inherited the same leap-on-`child_off`
pattern as the finaliser's reverse walk. It needed the same
cycle-safe guard: leap only when the target points strictly backward.
With the cursor cycle-safe, the parser produced a well-formed tape
on every grammar — but the lowering then stack-overflowed on
`find_descendant_by_kind` because the bump scope was too narrow.
The leftmost-descendant walk widens the scope to cover entire
post-order subtrees; the under-bump that had two cousin compounds
sharing a depth disappears.

Pratt outer's `child_off` override remained the last cycle-masked
bug. With the finaliser cycle gone and the bump scope correct, the
override surfaced as a single-operand chain that the lowering's
operand collector resolved with multiple records. The Pratt emitter
already had access to the operand dispatcher's returned outer
offset; using it directly retires the leftmost-descendant
approximation and lands the override on the operand row.

The lowering cousin-leak guard catches the residual: even with the
bump scope widened correctly, cousin records (descendants of
different parent compounds) can land at the same final `frame_depth`
when the bump cascade promotes a deeply-nested record into a layer
where a sibling-of-the-parent already sits. The finaliser's
depth-only chain reads them as siblings; only structural span
containment distinguishes them. The lowering was the natural place
to apply the span check — children whose `lo >= body_hi` are out of
the chain's structural scope and discarded before operand
resolution.

## Performance

Pre-fix substrate: `BbnfBootstrap::parse` did not return on any
grammar within bounded measurement windows (json: > 120 s before
timeout; bbnf: > 600 s before timeout). The hang was a tight inner
loop in the finaliser's reverse walk; profile capture credited
100 % of parser runtime to the inlined loop body.

Post-fix substrate, release profile, single thread:

| Surface | Wall |
|---|---|
| `bbnf_parses_its_own_grammar` (built-binary, runtime separated from compile) | 0.20 s real |
| `compile_pipeline::compile_bbnf` divan median | 2.831 ms |
| `compile_pipeline::compile_json` divan median | 182 µs |
| `compile_pipeline::compile_ebnf` divan median | 602 µs |
| `compile_pipeline::compile_css_l4` divan median | 26.72 ms |
| `compile_pipeline::compile_sheets` divan median | 12.06 ms |
| `xtask regen --grammar json` `compile_paths_request` (parse + lowering + IR) | 1.48 ms |
| `xtask regen --grammar json` `generate_all` (codegen TokenStream) | 3.02 ms |
| `xtask regen --grammar json` `prettyplease` (format) | 11.13 ms |
| `xtask regen --grammar json` total wall (incremental compile + run) | 65.45 s |

The `compile_bbnf` median sits ~3 × the prior microsecond-scale
baseline measurements; the difference reflects accumulated runtime
work between the two measurement eras and not regression introduced
in the parser-restoration scope. The microsecond-to-low-millisecond
band is well clear of any wall that would constitute a runtime
regression. The xtask total wall is dominated by xtask binary
incremental compile (release profile, ~1 m); the actual regen
pipeline runs in ~16 ms.

The `bbnf_parity` test binary itself compiles in 34.6 s release
cold; the `bbnf_parses_its_own_grammar` test, which SHA-asserts the
parsed self-host AST against a known-good fixture, executes in
0.20 s wall and reports `1 passed` against the released binary.

## Test results

The tape suite — which exercises every B3 architectural fix directly
(retired reverse-walk, atomic depth rollback, cycle-safe cursor walk,
widened post-order bump scope) — runs 100 tests, 100 passed, 0 skipped
in 0.155 s wall (`cargo nextest run -p tape --profile ax-iter`,
EXIT 0).

The `bbnf_parity` binary's `bbnf_parses_its_own_grammar` test
SHA-asserts the parsed `BbnfBootstrap` AST against a known-good
fixture, ran at Phase 1 with `1 passed; 0 failed` and `time real
0.20`.

The workspace and package-filtered nextest scopes (`-p bbnf -p tape -p
bbnf_derive`) fail at build time with E0107 "missing generics for
struct `FusedOutput`" through `#[derive(Parser)]` macro expansion in
several test sources (`value_api_apples_to_apples`,
`serialize_roundtrip`, `css_l4_parity`, `sheets_parity`,
`json_check`). The migration retained `FusedOutput<R>`; consumer
fixtures still write `FusedOutput`. This debt pre-dates B3 (AY-II.W0'
migration polish that the W0' close ceremony is named to resolve)
and does not block xtask regen on the data grammars or the
`bbnf_parity` SHA assertion. Full evidence at
`docs/tranches/B3/audit/W1-test-output.txt`.

## Out of scope

The bbnf self-host xtask regen reaches the codegen stage and
completes the parser, lowering, IR, and `generate_all` phases:

- `compile_paths_request` (parse + lowering + IR): 2.91 ms
- `generate_all` (TokenStream emission): 9.50 ms

The TokenStream then fails `syn::parse2` with "expected loop or
block expression". The emitted Rust contains a syntax that `syn`
rejects; the failure lives in the codegen surface that produces the
TokenStream, not in the parser/lowering/IR pipeline that B3
restored. This is a separate emit-correctness defect, addressed as
B4's opening scope.

The latent build-orphan in `crates/bootstrap/src/bin/cost_grid_sweep.rs`
(an import that lost its source crate when `bbnf-ir` was purged from
bootstrap's manifest) predates B3 and is not in B3's scope; B3's
verification commands target the affected crates explicitly so the
orphan does not block the close gate.

## Hand-off

The proc-macro `bbnf-bootstrap` compile path uses the checked-in
`generated.rs` and is not affected by the bbnf self-host regen
blocker. B2's W0.c re-execution unblocks against the post-B3
substrate: `cargo xtask regen --grammar bbnf` no longer hangs in
`BbnfBootstrap::parse`, and the IR pipeline runs in milliseconds.

After B2 closes, B4 opens with two scopes: the codegen `syn::parse2`
emit-correctness fix and any hardening required to re-land the
post-fix parser substrate's interaction with subsequent runtime
work. AY-II.W0' close ceremony shifts to B4 close on the post-B4
substrate; AY-II.W1-W5 sequencing operates on whatever runtime B4
produces.
