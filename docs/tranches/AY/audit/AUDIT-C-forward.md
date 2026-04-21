# AUDIT-C — Architectural Path Forward

Audit agent C of the AY pause triumvirate. Angle: the idiomatic,
gestalt path to close AY with (a) total viability of the
direct-to-object projection, (b) zero grammar overfitting, (c) AU
bench parity under the same profile discipline.

Primary evidence: `target/expand/ay-json.rs` (6,186 lines,
regenerated 2026-04-20 via `make ay-expand-json`), current builder
in `crates/tape/src/builder.rs`, the draft W7 fix diff in
`/Users/mkbabb/Programming/bbnf-wt-ay-w7/`, and bench artefacts
`post-AY-W{4,5,6}-bench.txt` + `post-AU.json`.

Status: open_stack breaks under `columns_mut().truncate`; Shape-2
held back from open/close; `note_push` fires on every structural
push without the promised short-circuit; 71 projections emit but
only the 2 resolver-backed ones carry a `materialize_projection_*`
helper on JSON.

---

## 1. Direct-to-object projection — elegance audit

**Finding.** The admission walk
(`crates/core/src/backend/rust/emitter/grammar.rs:158-212`
`collect_projection_admissions`) is grammar-derived and uniformly
shaped. Two arms — `ResolverNamed { binding_name }` via
`RustNamedTypes::resolve_named` on `TypeDesc::Named(sid)`, and
`GrammarLayout { layout }` via `ir.payload_layouts.get(&rule.id)`.
Neither carries a grammar-name branch. The emitted consts
(`PROJECTION_DIRECT_TO_STRUCT`), structs
(`<Grammar><RuleCamel>Projection` —
`ay-json.rs:4575-4620`), and shims
(`__named_type_shim_*`, `__grammar_projection_*` —
`ay-json.rs:4643-4657`) all derive from IR state alone. Good.

The **materialize side is half-delivered.**
`emit_projection_fns` at
`crates/core/src/backend/rust/emitter/shapes/value_materialize.rs:381-435`
mirrors `collect_projection_admissions` Arm-2 only — the
`ResolverNamed` branch emits no helper. The emitted JSON expand
confirms this: `PROJECTION_DIRECT_TO_STRUCT` has two entries
(`bool`, `string` —
`ay-json.rs:4633-4636`), only **one** `materialize_projection_*`
helper is present (`materialize_projection_bool_JsonParser` —
`ay-json.rs:5028-5042`). The `("string", "String")` admission points
at the resolver's `String` type but no helper reads the packed bytes
back into it; the consumer stays on the `Vec<JsonParserValue<'p>>`
compound path (see `JsonParserValue::string(::std::vec::Vec<…>)` at
`ay-json.rs:4668`). AY.W6's "71 admissions" counts admissions; the
consumer coverage number is "1 of 2 on JSON, 69 of 71 across all
four grammars".

The helper body itself is idiomatic
(`ay-json.rs:5028-5042`): a `payload_bytes` slice + `from_le_bytes`
per field, `Option<_>` return with `None` on short buffer or
missing payload. `#[inline]` is correct — LLVM will collapse at the
caller.

**Proposal.** Unify the admission walk and the materialiser walk
behind one `ProjectionAdmission` iterator owned by a new
sub-module `emitter/projection/` that both `grammar.rs` and
`value_materialize.rs` consume:

```
crates/core/src/backend/rust/emitter/projection/
├── mod.rs        // pub use admissions, emit_struct, emit_helper
├── admission.rs  // ProjectionAdmission + collect_projection_admissions
├── struct_emit.rs// per-admission struct definition
└── helper_emit.rs// per-admission materialize_projection_* helper
```

For `ResolverNamed` admissions, the helper emits a
`read_<binding_name>_from_bytes(payload_bytes) -> Option<Name>`
dispatch that picks up whatever reader the backend's
`RustNamedTypes` already exposes (today: `String`, `Color`). For
`GrammarLayout`, the current per-field helper stays.
`materialize_projection_*` becomes **total** across admissions —
every entry in `PROJECTION_DIRECT_TO_STRUCT` resolves to one
runnable helper, and the wire-contract test at
`admitted_projection_surfaces` extends to assert
`PROJECTION_DIRECT_TO_STRUCT.len() ==
count(materialize_projection_* fns)`.

**Risk.** Resolver-backed types (`String`, `Color`) may have
backend-specific reader shape that the byte-slice reader can't
produce without redesigning `RustNamedTypes::resolve_named` to
expose a `from_payload_bytes` method alongside the type
declaration. This is the substrate completion that W6.b stopped
short of; it lands in ~200 LOC inside the new `projection/`
module without adding grammar dispatch anywhere.

**Score 4/5.** Not load-bearing for the bench gate, but the
invariant "every admission has a consumer" is load-bearing for
tranche close per AY §13 + README §Activation-gate rule.

---

## 2. TapeBuilder rollback — root-cause architecture

**Finding.** The current substrate violates a substrate invariant
that isn't stated anywhere: **the `open_stack` and the `columns`
must agree on which rows exist.** The emitter's retry patterns
(`wrap.rs:557`, `keyword.rs:316, 412, 431`, `inline.rs:384, 476,
622`, `alt_dispatch.rs:592`, `flat.rs:517, 571, 576`) call
`builder.columns_mut().truncate(attempt_len)` directly — that
entry point at `crates/tape/src/columns.rs:195-199` truncates
`records` + `sib_skip` + invalidates `packed_cache`, but knows
nothing about `open_stack`. `OpenFrame::last_child` becomes a
dangling index into a row that no longer exists. The next
`note_push` reads `frame.last_child != u32::MAX`, takes the
stamp path, calls `set_sib_skip_at(prev, new_idx - prev)` on a
row whose bits now belong to some completely unrelated later push,
and corrupts the stamp invariant. The `prev < new_idx` guard in
the W7 draft
(`/Users/mkbabb/Programming/bbnf-wt-ay-w7` diff on `builder.rs`
lines 235-267) *hides the corruption* by preferring the rollback
branch when the ordering looks wrong, but it preserves the broken
invariant: stale `OpenFrame` state sits on the stack across a
truncation boundary.

This is why Shape-2
(`array.rs::emit_parse_array_list`) can't move to open/close —
its per-iter IIFE can fail AFTER frames have been pushed, leaving
interstitial frames the outer loop doesn't know about. Same for
Pratt operand failure. Same for Alt trial backtrack across every
shape with `truncate` calls listed above.

**The invariant to declare:** *column length is the single source
of truth for which rows exist, and `open_stack` entries whose
`compound_offset` sits at-or-above the current column length are
no longer live frames.*

**Proposal — architectural transposition.** Move `open_stack`
inside `Columns` (`crates/tape/src/columns.rs`) and make `truncate`
frame-aware:

```rust
// crates/tape/src/columns.rs
pub struct Columns {
    records: Vec<TapeRec>,
    sib_skip: Vec<u32>,
    // …pay_* columns unchanged…
    /// AY.W7 — open-frame stack colocated with the column it
    /// indexes into. `truncate` drops frames whose
    /// `compound_offset` no longer exists, preserving the
    /// invariant that every `OpenFrame` references a live row.
    open_stack: Vec<OpenFrame>,
}

impl Columns {
    #[inline]
    pub fn truncate(&mut self, new_len: usize) {
        self.records.truncate(new_len);
        self.sib_skip.truncate(new_len);
        while let Some(frame) = self.open_stack.last() {
            if (frame.compound_offset as usize) >= new_len {
                self.open_stack.pop();
            } else if (frame.last_child as usize) >= new_len {
                // Live frame, stale child pointer. Reset child
                // window to "no children yet".
                self.open_stack.last_mut().unwrap().last_child
                    = u32::MAX;
                self.open_stack.last_mut().unwrap().first_child
                    = u32::MAX;
                break;
            } else {
                break;
            }
        }
        self.invalidate_packed();
    }
}
```

`TapeBuilder` keeps the `open_compound` / `close_compound` /
`note_push` surface unchanged (they delegate into `columns`).
`OpenFrame` becomes a column-module type. Per
feedback_directory_modules, the split lands as
`crates/tape/src/columns/{mod.rs, open_stack.rs, truncate.rs}`
when `columns.rs` exceeds the natural module boundary.

**Risk.** Every caller of `columns_mut().truncate(N)` now pays
a stack-walk on truncate. The stack is empty for grammars that
never open-compound, and bounded by parse depth (~66 on twitter)
when it's used; the walk is negligible compared with the
`records.truncate` + `sib_skip.truncate` that already fire.

**Score 5/5.** This is the invariant AY.W5→W7 has been avoiding.
Every "recorded miss" downstream of the truncate/open_stack
drift dissolves once it is declared and enforced.

---

## 3. Emitter + substrate cohesion

**Finding.** Today:

- Shape-1 wrapped arrays + JSON objects: `open_compound` /
  `close_compound`
  (`emitter/shapes/array.rs::emit_parse_array_wrapped`,
  `emitter/shapes/object.rs`). Confirmed by expand at
  `ay-json.rs:1405-1448` (array) + `1657-1708` (object) — 17
  `open_compound`, 1 `push_compound` on JSON.
- Shape-2 list arrays: `push_compound`
  (`emitter/shapes/array.rs::emit_parse_array_list:596, 693, 706,
  773`) because the per-iter IIFE at `array.rs:671-690` truncates
  columns on iter failure (actually it only rewinds `*p`, but the
  retry convention across other shapes IS truncate-based and the
  emitter flagged it as architecturally impossible under the
  current open_stack).
- Pratt: outer compound on open/close
  (`ay-json.rs` doesn't exercise Pratt); reducer inner compounds
  still `push_compound`
  (`shapes/pratt.rs:346`, `shapes/pratt.rs:442`). Originally Pratt
  was prototyped fully on open/close and reverted exactly because
  of the same rollback issue.

This is **architectural drift** dressed as pragmatism. With §2
landed the "frame-rollback breaks open_stack" rationale
evaporates.

**Proposal — unification.** Pick (a): every compound emission
uses `open_compound` / `close_compound`. Retire
`TapeBuilder::push_compound` from the generated-parser surface.

Concrete changes:

- `emit_parse_array_list` at
  `emitter/shapes/array.rs:466-791`: replace `push_compound(Rule,
  …)` + `push_compound(Seq, …)` with `open_compound` +
  `close_compound`, matching the wrapped variant.
- `emit_pratt_tape` at `emitter/shapes/pratt.rs:346, 442`:
  reducer inner compounds switch to open/close.
- Every other `push_compound` in `emitter/shapes/*.rs`:
  migration audit. Per feedback_no_workarounds_arch, the
  architectural transposition is mandatory; no dual API.
- `TapeBuilder::push_compound` becomes a test-only helper on a
  feature gate or is deleted wholesale. Preferred: delete.
  Tests that construct tapes directly go through
  `open_compound` + `close_compound` too.

**Close-stamping then lives in one place:** the
`note_push` + `close_compound` pair — no finaliser
`derive_sib_skip` post-pass at all. Today the finaliser still
runs
(`crates/tape/src/finaliser.rs`) because the legacy
`push_compound` path needs it. Retire `push_compound`, retire the
finaliser's `derive_sib_skip` step. `derive_frame_depth` stays
only for the DTA-less legacy gated behind
`has_inline_frame_depth: false` — already dead post-W1 but left as
dead code per feedback_no_workarounds.

Alternative (b) — make `close-stamping` a substrate-side
post-pass independent of the emitter API — is architecturally
inferior because it reintroduces the finaliser-derived sib_skip
pass that AY.W5 was built explicitly to eliminate (per AY.W5.a
rationale). The whole premise of W5 was write-time stamping; (b)
is backing out of that premise.

**Risk.** The Pratt reducer inner compound is a nested structure
where frames open inside `?`-bearing per-op bodies. Same rollback
invariant as §2; once §2 lands, this is a rote migration.

**Score 5/5.** One codegen path. One stamping path. Cleanup of
~800 LOC of emitter duplication + a finaliser sub-pass.

---

## 4. Perf recovery path — idiomatic, not patchwork

**Finding.** Cumulative W4→W6 twitter regression is `746 → 548
MB/s = -26.6%` on the same `bench` profile. The chief suspect is
`note_push` — it fires on every `push_leaf` + `push_leaf_with` +
`push_leaf_borrowed_string` + `push_leaf_with_arena_frame` +
`push_leaf_with_arena_payload` + `push_leaf_with_f64_direct` +
`open_compound` + `push_compound` via inlined stamp of the
innermost `OpenFrame` (`builder.rs:236-248`). JSON expand shows
17 `open_compound` + 17 `push_leaf*` + 1 `push_compound` on a
single parse of a nested object — every one of those 35 push
sites runs `note_push`, and JSON twitter uses `open_compound`
throughout so `open_stack` is NEVER empty on a JSON parse. The
"one predicted-not-taken branch" comment at `builder.rs:225-226`
is incorrect for JSON: the branch goes to the stamp body every
time.

**Options.**

(a) *Zero-cost short-circuit.* The emitter statically knows
whether a given parse fn uses `open_compound`. Emit a thin
wrapper `push_leaf_no_frame(…)` that skips `note_push`, and use
it in the paths where the emitter proves no enclosing
`open_compound` is live. Problems: requires emitter-side
reachability analysis to know which leaves can sit inside an open
frame; in a nested grammar the analysis is non-local; the two
surfaces proliferate.

(b) *Full-post-pass `sib_skip` stamp.* Delete `note_push`.
`finaliser::finalise` walks the tape once, derives every
`sib_skip`. This was the pre-W5 regime; the AY.W5 rationale
claims (correctly) that a single post-pass is cheaper than a
per-push stamp when the parser is going to do a pass anyway for
`frame_depth`. But W5's read-side gain was supposed to be: "no
post-pass needed because `sib_skip` is already stamped." That
gain has never materialised — the finaliser still runs.

(c) *Hybrid via emitter decision.* Codegen inspects
`ir.payload_layouts` + per-rule reachability and picks
open/close vs push_compound per grammar. Reintroduces the dual
API §3 deletes.

**Recommendation.** (b). The post-pass was strictly faster
pre-W5; the W5 evidence
(`post-AY-W5-bench.txt`: twitter -17%) confirms write-time
stamping did not pay for itself on JSON. Combined with §3's
retirement of `push_compound`, the recipe becomes:

1. Emitter emits `open_compound` / `close_compound` everywhere.
   These write the compound row at open time with
   `child_off = NONE` and back-patch `span_hi` + `child_off` +
   `HAS_CHILDREN_BIT` at close.
2. Leaves push through the existing zero-hook fast paths; no
   `note_push`.
3. `close_compound` does NOT stamp `sib_skip` — no per-push
   bookkeeping at all.
4. `finaliser::finalise` derives every `sib_skip` in one
   sequential post-pass (the existing
   `finaliser::derive_frame_depth` + `finaliser::finalise` combo,
   with the `SIB_SKIP_STAMPED_BIT` check-and-skip removed since
   everything is derived).

Net effect: 17 × 35 = ~600 dynamic `note_push` calls on twitter
disappear. The one extra sequential pass over the
`frame_depth` + `sib_skip` arrays is the same loop the finaliser
already runs.

**Risk.** `TapeRec::SIB_SKIP_STAMPED_BIT` (`tape.rs:0x0020`
extra-bit) becomes dead. Delete it and the read-side fast path
that inspects it. Per feedback_no_workarounds, not a cost — a
simplification.

**Score 5/5.** This is the AU-parity unlock.

---

## 5. Bench profile normalization

AU's `post-AU.json` used `[profile.bench]` (fat LTO, codegen-units
= 1, debug = true). AY's bench artefacts
(`post-AY-W5-bench.txt`, `post-AY-W6-bench.txt`) use the same
profile per `Cargo.toml:profile.bench`. Apples-to-apples is
already established; the -27% twitter figure is not a profile
artefact.

**Discipline.** Final-proof and tranche-close numbers run on
`[profile.bench]` (fat LTO). Wave-mid sanity runs ride
`[profile.profiling-prep]` (inherits release / thin LTO, debug =
true, strip = false) via `cargo prep-bench` — the existing
three-tier command surface
(`docs/instructions/tranche/SPEC.md:§Three-tier command
surface`). Dev iteration stays on `[profile.ax-iter]` via
`make iter-test-*`.

One paragraph to add to `AY.md` §Operational posture:
*"Wave-close + tranche-close bench numbers ride
`[profile.bench]` only. Wave-mid sanity and spot
regressions ride `[profile.profiling-prep]` to preserve fat-LTO
capture headroom for final-proof runs."* No new profiles. No
per-wave surface proliferation.

**Score 2/5.** Cosmetic given the apples-to-apples is already
established; a plan-doc annotation prevents future drift.

---

## 6. No-grammar-overfitting gate

Grepped the emitter + runtime + tape crates for
`JsonParser|CssL4Parser|BbnfParser|GoogleSheetsParser|CssParser`
as identifier tokens:

- `crates/core/src/backend/rust/emitter/**`: **zero matches**.
- `crates/core/src/runtime/**`: **zero matches**.
- `crates/tape/**`: **zero matches**.

The generated `materialize_projection_bool_JsonParser` identifier
is *composed* at emission time from `grammar_name` (a runtime
string argument to `emit_projection_fn` at
`value_materialize.rs:446-498`) — not from a hardcoded
`"JsonParser"` branch. The walked IR state
(`ir.payload_layouts`, `ir.types`, `RustNamedTypes::from_ir`) is
grammar-derived in the purest sense: no codepath has a
`match grammar_name.as_str() { "JsonParser" => … }` switch.

**Finding.** Zero grammar-name dispatch in the emitter, runtime,
or tape crates. Invariant holds.

The one cautionary note: the **test harness** at
`crates/core/tests/*` calls
`<JsonParser as Parser>::PROJECTION_DIRECT_TO_STRUCT` by name —
that's the test's job, not production routing, and is fine.

**Score 1/5.** No remediation required. The invariant is
preserved by construction; the W6.b admission mechanism is
grammar-agnostic.

---

## 7. The "recorded miss" pattern — gestalt

**Finding.** W2, W3, W4, W5, W6 closed "with recorded misses" —
five consecutive waves. Each time the next wave's plan named
owner responsibility for the deferral:

- W2 DtS magnitude miss → W3/W5 owner.
- W3 BEAT-sonic gate miss → W5-W7 owner.
- W4 regex + canada gate miss → W7 owner.
- W5 twitter -17% regression → W6 owner.
- W6 twitter -27% regression → W7 owner.
- W7 stalled on `note_push` rollback at the time of this audit.

This is not scope-revelation absorbed forward — it is
**structural under-scoping**. Each wave's nominal hard gate
assumed the previous wave's gate closed. When it didn't, the
current wave inherited the deferred work AND its own work AND a
regression. W6 carried W5's -17% + its own -11% = -27%; W7 was
planned to reclaim both, plus land the shared-fact optimizer,
plus land three deferred W3/W4/W5 gates, plus regen the
bootstrap.

Per `docs/instructions/tranche/SPEC.md:§Scope-reveal protocol`
the defensible move at W5-close would have been one of:

1. **Absorb** — the revealed work fits the existing schedule.
   This requires evidence the revealed work IS absorbable. By
   W5-close the W2/W3/W4 misses had already established that it
   was not.
2. **New letter** — close AY on what landed; open BA with a
   fresh wave schedule.

The cascade does not end at W8. W8 is "close ceremony + FINAL
authoring" — it cannot close gates, only record them. Running
W7 → W8 under the current plan silently produces a FINAL.md with
a near-parity hard-gate table that is mostly MISSED. That
failure mode is precisely the anti-pattern
`docs/instructions/README.md:§Code discipline` flags as "silently
declaring incomplete at execution time to dodge completion
requirements."

**Proposal.** Combine W5-W8 into **one architectural workstream
AY.W-close** of five parallel sub-phases:

1. **AY.W-close.1** — the §2 `open_stack` transposition
   (Columns-owned, truncate-aware).
2. **AY.W-close.2** — the §3 emitter unification (delete
   `push_compound` from emitter surface; Shape-2 + Pratt
   reducer on open/close).
3. **AY.W-close.3** — the §4 perf recovery (delete
   `note_push`; run finaliser post-pass; delete
   `SIB_SKIP_STAMPED_BIT` read-side).
4. **AY.W-close.4** — the §1 materialise-side unification
   (every `PROJECTION_DIRECT_TO_STRUCT` entry carries a
   `materialize_projection_*` helper).
5. **AY.W-close.5** — bench close + FINAL.md (numbers against
   AU's `post-AU.json` baseline; routed deferrals to BA).

All five sub-phases close on evidence that is already
well-scoped: cargo expand, samply, bench artefacts, wire-contract
test. Five agents can run in parallel under the existing
six-agent-per-wave cap. `W7.md` + `W8.md` retire; PROGRESS.md
records the replan per SPEC §Absorb vs New-letter.

**Risk.** Combining five phases into one workstream with
parallel agents requires disjoint file bounds. Current bounds:

- §2 owns `crates/tape/src/columns.rs` +
  `crates/tape/src/builder.rs`.
- §3 owns `crates/core/src/backend/rust/emitter/shapes/*.rs`.
- §4 owns `crates/tape/src/finaliser.rs` +
  `crates/tape/src/builder.rs` (cross-bound with §2).
- §1 owns
  `crates/core/src/backend/rust/emitter/shapes/value_materialize.rs`
  + `crates/core/src/backend/rust/view/named_types.rs`
  (plus the new `emitter/projection/` module).
- §5 owns `docs/benchmarks/` + `docs/tranches/AY/`.

§2 and §4 share `builder.rs` — sequence §2 before §4 (the §4
deletions depend on §2's open_stack invariant). Everything else
runs in parallel.

**Score 5/5.** Tranche-close structural.

---

## Forward-path recommendation (≤500 words)

Close AY by landing the three architectural transpositions below
in priority order. Replan W7 + W8 as a single five-sub-phase
workstream under §7's `AY.W-close` umbrella; retire the
individual W7 and W8 docs into PROGRESS.md entries.

### Priority 1 — Move `open_stack` into `Columns`, make `truncate` frame-aware (§2)

This is the rollback invariant the W7 draft fix band-aids in the
wrong place. Colocating the frame stack with the columns it
indexes means `columns_mut().truncate(N)` becomes the single
authority for row liveness; live frames whose `last_child` index
no longer exists reset their child window to "none" rather than
dangle. The draft `prev < new_idx` guard at
`/Users/mkbabb/Programming/bbnf-wt-ay-w7` builder.rs diff lines
235-267 deletes; the `while let Some(top)` orphan-drain at
builder.rs diff lines 493-502 deletes; their work moves into
`Columns::truncate`. File bounds:
`crates/tape/src/columns.rs` + `crates/tape/src/builder.rs`.

### Priority 2 — Retire `push_compound` from the emitter surface (§3)

With Priority 1 landed, the rationale for keeping Shape-2 arrays
and Pratt reducer inner compounds on `push_compound` evaporates.
Migrate `emit_parse_array_list`
(`emitter/shapes/array.rs:466-791`) and the Pratt reducer path
(`emitter/shapes/pratt.rs:346, 442`) onto `open_compound` /
`close_compound`. Delete `TapeBuilder::push_compound`. One
emission API. One stamping path. ~800 LOC of emitter drift
disappears. File bounds: `crates/core/src/backend/rust/emitter/
shapes/*.rs` (array, pratt, wrap, keyword, inline, alt_dispatch,
flat) + `crates/tape/src/builder.rs`.

### Priority 3 — Delete `note_push`; post-pass `sib_skip` finalise (§4)

`note_push` fires on every structural push including every leaf
on every grammar that uses `open_compound` — 35 dynamic calls
per JSON object parse on twitter. The claimed
"predicted-not-taken" short-circuit
(`builder.rs:225-226`) does not apply because `open_stack` is
never empty on JSON. Delete `note_push` and `OpenFrame::
{first_child, last_child}` tracking. `open_compound` records
compound-open; `close_compound` back-patches span_hi + child_off
+ HAS_CHILDREN_BIT; `sib_skip` falls out of one post-pass in
`finaliser::finalise`. `TapeRec::SIB_SKIP_STAMPED_BIT` deletes.
The pre-W5 post-pass was strictly faster per the AU baseline
(twitter 1967 MB/s); restoring it plus keeping the AY.W1 flat-AoS
substrate + AY.W4 SIMD unescape + AY.W6.b direct-to-struct is the
AU-parity recipe. File bounds: `crates/tape/src/builder.rs` +
`crates/tape/src/finaliser.rs` + `crates/tape/src/tape.rs`
(SIB_SKIP_STAMPED_BIT retirement).

**Concurrently** with Priorities 2-3, Audit §1's materialise-side
unification (one helper per admission) lands in its own sub-phase
on disjoint file bounds. After all three priorities land, W6.b's
71 admissions become 71 helpers; W5/W6/W7's "recorded miss"
cascade closes; twitter recovers to AU parity under the same
`[profile.bench]` discipline. AY closes on W-close.5 evidence.
