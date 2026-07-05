# Archaeological Dig — Tranches AZ-I, AZ-II, AZ-III

The AZ arc is the "direct-to-struct + tape dissolution" era: the campaign's
final three-tranche push before the restart. Its single governing thesis was
to abolish the generic intermediate parse substrate (the columnar "tape" record
stream) and replace it, for every grammar, with a grammar-derived native Rust
struct graph — "one materialised parse form." The arc succeeded at the deletion
and failed at everything the deletion was supposed to buy: throughput collapsed
30–80%, never recovered, and the recovery was deferred to a tranche (BB.close)
that never ran. This dig reconstructs what was attempted, what actually shipped,
where divergence began, and what is worth carrying forward.

Source corpus: `docs/tranches/AZ-I/`, `docs/tranches/AZ-II/`,
`docs/tranches/AZ-III/` (charters, FINALs, PROGRESS, RESEARCH, audit/, waves/).
Git cross-checks against the current tree at repo root.

---

## (a) What each tranche attempted — charters and waves

### AZ-I — direct-to-struct on JSON, CSS L4, Sheets

Charter thesis (`docs/tranches/AZ-I/AZ-I.md:19-33`): a typed `->` in a JSON,
CSS L4, or Sheets rule *is* the declaration of a field in an emitted struct; the
IR type-inference pass (`project_types`) composes these into a closed struct
graph (`StructRegistry`); the emitter writes straight into the graph "without
traversing an intermediate tape record stream." The tape crate was to survive at
AZ-I close scoped to exactly one consumer — BBNF's own bootstrap — with AZ-II
migrating and deleting it (`AZ-I.md:5-17`).

Declared waves (`AZ-I.md:153-160`): W0 research + classifier-unification study +
audit baseline; W1 `StructRegistry`/`project_types` closure; W2 direct-to-struct
emission substrate; W3 CSS L4 emission; W4 FINAL. The plan carried hard
throughput gates anchored to the AU baseline (`AZ-I.md:99-107`): JSON twitter
≥ 1967 MB/s, CSS bootstrap ≥ 600 MB/s, Sheets parse_simple ≥ 95 MB/s, etc., and
an explicit reversal discipline — a "wave-local 20% rule" (`AZ-I.md:310-314`)
that reverts a wave's substrate if it misses its own throughput gate by >20%.

### AZ-II — BBNF self-host cutover + tape deletion

Charter thesis (`docs/tranches/AZ-II/AZ-II.md:78-117`): BBNF's own grammar moves
to the struct path via a two-stage byte-equal bootstrap cutover (Stage A: old
tape compiler builds the struct compiler; Stage B: the struct compiler rebuilds
itself; close gate: Stage B output byte-equal to Stage A across the full BBNF
fixture corpus). Once BBNF parses into a struct, the tape has no consumers, and
`crates/tape/` is deleted (`AZ-II.md:384-408`). Declared as a single collapsed
`cutover` wave (`AZ-II.md:243-245`).

### AZ-III — AZ-II continuation close

Charter (`docs/tranches/AZ-III/AZ-III.md:1-34`): AZ-II did not actually close;
AZ-III is its continuation. Two duties: finish the AZ-II terminal-close work
"that was not honestly green," and land a grammar-general "authority substrate"
(durable egraph/node/projection facts, type obligations, consumed CSP
constraints) that the audits showed was needed to stop recurring local fixes.
Nine waves W0–W5 (`AZ-III.md:92-104`), split heavily under audit contact.

---

## (b) What shipped vs. what died on the vine

### AZ-I — struct-direct shipped; performance died

The substrate landed and activated. All three data grammars (plus, by the close
ceremony's collapse, all nine grammars) route through struct-direct emission via
`EmitStrategy::for_grammar` StructDirect arms; generated parsers carry zero tape
symbols (`docs/tranches/AZ-I/FINAL.md:166-172, 156`). The RuntimeView trait
unified view access across JSON/CSS/Sheets; parity harnesses were recoded
struct-vs-external (`FINAL.md:98-137`).

But the throughput gate **MISSED** and was routed forward, and the samply
attribution fleet was **WAIVED** (`FINAL.md:159-160`). The recorded regression
(`FINAL.md:191-205`) is the load-bearing negative result of the entire arc:

| Grammar/fixture | AZ-I MB/s | AU baseline | Δ vs AU |
|---|---:|---:|---:|
| JSON canada | 547 | 1231 | **-55.6%** |
| JSON citm | 1476 | 2438 | -39.5% |
| JSON twitter | 1402 | 1967 | -28.7% |
| JSON data_xl | 747 | 1179 | -36.6% |
| BBNF self_parse | 87 | 394 | **-77.9%** |
| BBNF css_l4_grammar | 111 | 496 | -77.6% |
| BBNF ebnf | 42 | 223 | **-81.2%** |
| BBNF json | 66 | 283 | -76.7% |

FINAL's own diagnosis (`FINAL.md:206-211`): "The JSON struct-direct path's
per-leaf bookkeeping (Object frame `pending_key` state machine, per-field arena
writes, compound-frame push/pop) costs more than the legacy tape-write path."
Additionally, CSS L4 bootstrap and Sheets parse both **SIGABRT** (stack overflow
under fat-LTO recursive descent), leaving those bench rows unmeasured
(`FINAL.md:227-241`).

The wave plan itself did not survive contact: the 17 declared downstream waves
were collapsed to 5 by a six-agent audit (`FINAL.md:16-30`), and the original
W2/W3/W4 were folded into a single "W2-act.close" ceremony.

### AZ-II — the crate died; the close did not

Physical deletion shipped: `crates/tape/` is absent from the current tree (git
verified). But the *cutover* — the thing that was supposed to be "two regen
invocations plus mechanical deletion" (`AZ-II.md:234-239`) — exploded into 14
sequential sub-stages (cutover.A through cutover.N) and then a further O0–O7 plus
five O3a child specs (`AZ-II/FINAL.md:52-53`; `AZ-II.md:245`). cutover.N was
"dispatched at master `43f0795b` and halted at organizational usage limit before
any commits landed" (`AZ-II/FINAL.md:53`; `AZ-II.md:50-51`). O5 (`crates/tape`
deletion) never went green: the crate was absent but "the no-default-features
build and regen check are not green" (`AZ-II.md:58-62`), so AZ-II was declared
**"CLOSED AS CONTINUATION HANDOFF … not a terminal green close"**
(`AZ-II/FINAL.md:1-11`, `AZ-II.md:19-42`).

What genuinely landed: the resolver-arm activation for all nine grammars, the
`serialize_compact_doc` typed walker over `BbnfDocument`, and — the tell — a
**hand-written 900-LOC `bootstrap_parser.rs`** authored at cutover.G "to break
the chicken-and-egg between regen and the broken on-disk `BbnfBootstrap::parse`"
(`AZ-II/FINAL.md:72`). This is a workaround (a shim carried across an entire
tranche) explicitly forbidden by the arc's own no-workarounds precepts.

### AZ-III — deleted the shim; declared terminal-with-carries anyway

Genuine wins (`AZ-III/FINAL.md:67-112`): `bootstrap_parser.rs` **DELETED (1505
LOC)** with the canonical generated path now passing 95/95 BBNF parity (commit
`954d166b`); a durable `FactAuthority` surface (`d8f43633`); two silent
`BoxedEnum` type-inference fallbacks replaced by named obligations
(`UnresolvedCompoundRef` at `reference.rs:74`, `HeterogeneousAltJoin` at
`revise.rs:123`); CSP shape/layout/dispatch installers globalized with the no-op
`shape_dict::install` deleted; 301 LOC of dead code retired.

But the close is honest only in its qualifier. `cargo xtask regen --check`
(strict mode) is **RED — 7/9 grammars drift** at AZ-III close, "KNOWN-CARRIED"
to a BB tranche that never ran (`AZ-III/FINAL.md:50, 57, 240-242`). Workspace
nextest is 1407/1527 = 92.1% with 118 failures (`FINAL.md:235-238`). And the
17-entry matrix was re-measured under `bench-iter` (no-LTO, codegen-units=16) —
which FINAL explicitly states is **"not apples-to-apples"** with the fat-LTO AU
baseline (`FINAL.md:163-167, 244-250`), with three `WATCHDOG_HALT` entries
(json.data_xl 2.4s, css.tailwind >120s, compile_css_l4) routed to BB.close
(`FINAL.md:126, 267-269`). AZ-III closed `TERMINAL_WITH_CARRIES`; the carries
were routed to BA and BB.

### The carries evaporated — BA/BB never ran

Git cross-check is decisive: `docs/tranches/BA/` and `docs/tranches/BB/` contain
only plan docs (`BA.md`, `BB.md`) plus `audit/` and `waves/` — **no `FINAL.md`
for either**, and `git log --all` returns **zero `feat(ba/`, `fix(bb/`-scoped
implementation commits**. BA has 18 wave-spec files (W0–W6 with sub-letters); BB
has 23. Every single performance carry from AZ-I → AZ-II → AZ-III was routed to
"BB.close," and BB.close never happened before the restart. **The entire AZ
arc's throughput recovery was an infinite deferral.**

---

## (c) Root causes of divergence

### RC1 — Eager struct materialisation is slower than the tape it replaced (the founding error)

The AZ thesis positioned direct-to-struct as "the eager dual of [simdjson]
ondemand — eager materialisation of the same shape ondemand materialises lazily"
(`AZ-I/RESEARCH.md:246-249`). This is precisely backwards. simdjson and sonic-rs
adopted **lazy** ondemand *because* eager materialisation is the expensive part;
sonic-rs's `pointer!` skips off-path structure via structural offsets and never
materialises a tree (`RESEARCH.md:250-258`). AZ-I built eager where the SOTA it
cited had deliberately gone lazy. The measured cost was exactly the predicted
one — per-leaf bookkeeping, per-field arena writes, compound push/pop
(`AZ-I/FINAL.md:206-211`) — a 30–80% regression with no recovery path inside the
architecture.

### RC2 — A purity rule (`no-orthogonal-codepaths`) forbade the reversal the profiler demanded

AZ-I's own reversal discipline (the wave-local 20% rule) should have reverted the
substrate on a -55% miss. It was **waived**, with the stated reason that
"Reversal would re-introduce dual codegen paths (`feedback_no-orthogonal-codepaths`
violation). AZ-I closes WITH RECORDED MISSES on perf" (`AZ-I/FINAL.md:217-223`).
The ideological commitment to a single materialised form overrode a factor-of-two
performance loss. This is the archetypal local minimum: the project could not
climb out of the slow architecture because the exit — keeping the fast tape path
alongside the struct path — was defined as heresy. The reversal budget the plan
authored as a "health signal" (`AZ-I.md:325-331`) was never once exercised across
three tranches (`AZ-III/FINAL.md:271-280`).

### RC3 — Perpetual deferral to a tranche that could not run

Perf recovery was deferred at every close: AZ-I → "routed to BB.close"
(`FINAL.md:159, 211`); AZ-II → "re-bench gated on cutover.O; no optimization
tranche may consume these stale values" (`AZ-II/FINAL.md:111, 122`); AZ-III →
"the perf optimization route is deferred to BB.close, not absorbed in W4"
(`AZ-III/FINAL.md:267-269`). BB.close structurally blocked on BA close, which
blocked on AZ-III terminal evidence. BA/BB never opened. The deferral chain was a
closed loop with no exit — a documented instance of the very "substrate-first,
consumer-later, reversal-never" Era V anti-pattern the arc claimed to preclude
(`AZ-I.md:328-331`; `RESEARCH.md:46-53`).

### RC4 — Benchmark dishonesty by profile switch

AZ-III converted a measurable regression into an unmeasurable "carry" by changing
the benchmark profile. The AU/AZ-I baselines were fat-LTO; AZ-III measured under
`bench-iter` (no-LTO) and declared cross-profile comparison "not apples-to-apples"
(`AZ-III/FINAL.md:163-167, 244-250`). The regression did not close — it was made
invisible. This is the exact pathology the process retro flagged in the AV/AW era:
"bench-omission until V10 hid 2.5–4.5× regression"
(`AZ-I/audit/AUDIT-1-PROCESS-RETRO.md:17-19`). The arc re-committed the sin it had
catalogued.

### RC5 — The bootstrap chicken-and-egg, papered over with a shim then a per-shape whack-a-mole

Self-hosting BBNF into struct-direct requires a working struct-direct parser to
regen the parser. cutover.G's answer was a 900-LOC hand-written
`bootstrap_parser.rs` (`AZ-II/FINAL.md:72`) — a workaround carried an entire
tranche. Deleting it in AZ-III required four rounds of per-shape emitter surgery
(W2.4.r flat-shape Span synthesis, W2.4.s lower_term activation, W2.4.t
lower_factor modifier recovery, W2.4.u keyword-shape Span push;
`AZ-III/FINAL.md:46, 195-203`), and **still** left four substrate divergences
(keyword Unit→Span, entry-rule classifier, HRegex payload i64→str, PHF table
generation) that keep `regen --check` red (`AZ-III/FINAL.md:57, 121`).

### RC6 — "Grammar-general" claimed; per-grammar/per-shape patching delivered

Every AZ charter carried a "grammar-general fixes only" invariant
(`AZ-III.md:44-46`). The actual work was a cascade of grammar-specific emitter
arms: array Shape-2 dispatch, flat Alt/Repeat/Regex/Negate inline emission
(`AZ-II/FINAL.md:71`), keyword-shape Alt-of-Ref for CSS L4 pseudo-classes
(`AZ-II.md:274`), alt_dispatch Alt-of-Literal/Regex/Seq surgery for BBNF
`type_name` and CSS pseudo-classes (`AZ-II.md:78`), named_color alt_dispatch
emission for CSS (`AZ-III/FINAL.md:143`). The generality was aspirational; the
codegen surface fractured per grammar per shape.

### RC7 — Wave proliferation and plan miscalibration as a chronic condition

AUDIT-1 (`AZ-I/audit/AUDIT-1-PROCESS-RETRO.md:32-79`) catalogues the process
organism's failure modes with evidence: substrate-without-consumer (≥3 in the
last 4 tranches), wave proliferation (AX carried 19 wave docs; AW-I.W4 had six
sub-waves W4α–W4ζ), mid-tranche pivots multiplying tranche letters (AW →
AW-I/II/III/IV/V — five FINALs), and plan miscalibrations falsifiable at plan
time (B6.W1 budget 660s, measured 17s — 38× stale). The AZ arc reproduced all of
these: a 3-stage cutover became 14 + O0–O7 + O3a children; BA/BB accumulated 41
wave-spec docs before a line of code.

### RC8 — God modules and dead substrate accreted under the churn

The architecture audit (`AZ-I/audit/AUDIT-6-ARCHITECTURE.md:93-111`) and decay
inventory (`AZ-I/audit/AUDIT-3-DECAY-INVENTORY.md:114-163`) record: `dta.rs` 1625
LOC (>50% kernel-dead per its own comment, ~900 LOC reclaimable),
`emitter/grammar.rs` 1432 LOC, `csp_strategy/mod.rs` 1273 LOC, `view/value.rs`
1272 LOC — each a tangle of independent concerns. `StructBuilder` shipped as an
orphan abstraction: one implementor, zero generic consumers
(`AUDIT-6:151-163`), the trait's own doc admitting the generated code takes the
concrete type so LLVM can monomorphise.

---

## (d) Deferred / abandoned / lost work worth resurrecting

1. **The grammar-derived struct-shape target designs** (`AZ-I/RESEARCH.md:68-165`)
   — concrete `JsonValue`/`JsonNumber`, `Cell`/`CellValue`/`Formula`,
   `StyleSheet`/`Declaration`/`TypedValue`/`Length`/`Color` enum sketches. These
   are the correct *output* IR shapes for a typed parser and remain valid targets
   regardless of eager-vs-lazy materialisation. Resurrect as the type surface; do
   **not** resurrect eager materialisation into them (RC1).

2. **The parent-pointer vs. root-traversal analysis** (`AZ-I/RESEARCH.md:172-235`)
   — a costed tradeoff (8 bytes/node vs. O(depth) rewalk) that was explicitly
   handed to BA.W0 to decide by measurement. BA never ran; the decision is still
   open and the analysis is clean.

3. **The SOTA technique survey and its core lesson** (`AZ-I/RESEARCH.md:237-285`):
   simdjson ondemand (lazy, struct-direct-on-bytes), sonic-rs `LazyValue` +
   `pointer!` (path-compiled skip via structural offsets), yyjson's frontier —
   **"SIMD is not where the next 10% lives; key dispatch and in-place payload
   allocation are"** (`RESEARCH.md:269-276`). This is a load-bearing directional
   result for any perf-first re-founding, and it argues directly against the eager
   thesis the same document adopted.

4. **The classifier locked-split disposition + its revisit condition**
   (`AZ-I/CLASSIFIER-UNIFICATION.md`). The negative result: the regex-HIR,
   structural-alphabet, and payload-kind classifiers consume incommensurable input
   substrates (bytes, node-shape categories, projected `TypeDesc` tuples);
   unification is nominal only (`CLASSIFIER-UNIFICATION.md:1-23, 210-247`). The
   resurrectable *positive*: unification becomes real only when all three share a
   fourth substrate — "a unified type lattice … the egraph substrate is the
   candidate" (`:249-269`). SK-V21's e-graph work should test exactly this
   condition rather than re-deriving the negative.

5. **AZ-III's genuine substrate wins** (`AZ-III/FINAL.md:79-98`): the durable
   `FactAuthority` surface (`facts/authority.rs`), the type-obligation mechanism
   replacing silent `BoxedEnum` fallbacks (named `UnresolvedCompoundRef` /
   `HeterogeneousAltJoin` obligations), and the globalized CSP shape/layout/dispatch
   installers with disconnect tests. These are the "no silent fallback" substrate
   the arc was right to build; they should survive the restart intact.

6. **The B-series process template** (`AUDIT-1:82-110`) — the one unambiguous
   process success in the corpus: single narrow architectural lever per tranche,
   evidence-grounded gates, FINALs of 200–350 LOC, 8 closures in 5 calendar days.
   The measured contrast (AV/AW consumed weeks for less durable gain than B5 alone
   delivered in three days) is the template SK-V21's wave design should copy.

7. **The dead-substrate reclaim inventory** (`AUDIT-3:302`, `AUDIT-6:355-495`):
   `dta.rs` ~900 LOC dead, `tape::visitor` family, orphan `StructBuilder`, the
   god-module split proposals (dta/, grammar/, view/value/). Free reclaim if the
   restart carries any of this code forward.

---

## (e) Meta-process pathologies

1. **"Continuation close" as a euphemism for not closing.** AZ-II "closed as a
   continuation handoff" (`AZ-II/FINAL.md:1-11`) — i.e. it did not close, and the
   goalposts moved to AZ-III. AZ-III then closed `TERMINAL_WITH_CARRIES` with
   `regen --check` red and 118 tests failing. The vocabulary of closure
   (MET-WITH-CARRIES, TERMINAL_WITH_CARRIES, complete_with_misses) grew precisely
   as green closure receded.

2. **Named-carry laundering.** Every miss was "routed to BA/BB with an evidence
   path that resolves" (`AZ-III/FINAL.md:114-127, 284-285`). The routing ritual
   manufactured accountability while the destinations (BA/BB) never opened. Debt
   dressed as scheduling is still debt; here it was total — the restart inherited
   every unpaid carry.

3. **Doc/code inversion.** FINALs assert gates MET while the underlying commands
   fail: hard gate 1 "NAMED-BLOCKER → MET-AT-CARRY" reconciled a red `regen
   --check` to "MET" via an xtask "content-equality skip" (`AZ-III/FINAL.md:57`) —
   softening the gate rather than passing it. The byte-equal reproducibility claim
   rested on the same skip (`AZ-II/FINAL.md:88`).

4. **Compute-wall halts mid-wave.** "Halted at organizational usage limit" recurs
   as a first-class trajectory event (`AZ-II/FINAL.md:53`; `AZ-II.md:50-51, 245`),
   with entire dispatched waves (cutover.N) landing zero commits. The process had
   no graceful degradation for hitting the wall — work simply vanished.

5. **Planning outran execution by an order of magnitude.** BA/BB accreted 41
   wave-spec documents and full plan charters with zero implementation. The
   process retro named this ("wave proliferation," "ceremony around deferral
   consumes more LOC than the deferred work"; `AUDIT-1:112-183`) and the arc did it
   anyway, harder.

6. **The one thing the process did well** was the read-only audit triumvirate:
   the six-agent audits (`AZ-I/audit/W2-CLOSE-AUDIT.md`,
   `AZ-III/audit/SIX-AGENT-SYNTHESIS-2026-04-30.md`) produced convergent, honest
   findings that correctly diagnosed the arc's own failures — the collapse of 17
   waves to 5, the substrate-without-consumer pattern, the fact/type/CSP authority
   gap. The audits were more truthful than the FINALs they fed. SK-V21's own
   audit-substrate pass is heir to this, and it works.

---

## Bottom line for the totality plan

The AZ arc proves, at cost, three things SK-V21 must not re-learn: (1) eager
whole-tree struct materialisation loses to a flat intermediate on the parse hot
path — SOTA went lazy for a reason; (2) a "one materialised form / no orthogonal
codepaths" purity rule that forbids keeping a fast path is a performance trap that
locks out the reversal the profiler demands; and (3) deferring perf recovery to a
downstream tranche is deferring it to never. The tape deletion was real and the
type/fact/CSP-authority substrate was real and good; the eager materialisation
that motivated the deletion was the mistake. Carry the struct *shapes*, the SOTA
lazy-ondemand direction, the classifier/egraph revisit condition, the
FactAuthority/obligation substrate, and the B-series single-lever cadence. Leave
the eager-materialisation thesis, the profile-switch bench accounting, and the
named-carry-to-a-tranche-that-never-runs deferral pattern in the ground.
