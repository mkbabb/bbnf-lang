# CRIT-P6 — the HM-core spike and the type plane (SPEC §6 / TW5)

## Verdict in one line

The prototype is an honest, well-grounded instrument whose own two corrections
expose three structural gaps in SPEC §6 that the spec has not yet absorbed. The
instrument is nearly settled; the spec section it tests is not.

## The prototype's evidence holds up

The method is legitimate and grounded, not contrived. The spike compiles both
grammars through the real frontend (`crates/core/tests/p6_hm_spike.rs:52`,
`:366`, `:419`, `:430` all call `bbnf::pipeline::compile_grammar`) and types the
actual `FnDescriptor::Expr` dump, not a hand-built AST. The reported pass counts
are backed by real assertions: `p6_hm_spike.rs:401` asserts `span_default == 0`,
`:403` asserts `failed == 0`, `:425/:454/:461` assert the three negative
injections error. The FnDescriptor census (PROTO-P6.md:41-52) is internally
consistent with the results line (`:128-133`): json 4 checked + 1 synth, gs 7
checked + 21 synth, total 11 + 22.

Honesty is the prototype's strongest feature. It does not hide the two things
that hurt it. Correction 1 (PROTO-P6.md:24-26) states plainly that the union-find
unifier — 82 of the 190 LOC, the one genuinely novel artifact — is **not
exercised by the corpus** and is "proven correct only synthetically." The spike
code confirms this against itself: the only union-find exercise is the synthetic
test at `p6_hm_spike.rs:464-494`, which hand-builds `Ty::IntVar` and never touches
a corpus descriptor. Correction 2 (`:27-32`, `:182-183`) states that correct-width
typing is falsified for 22 of 33 descriptors — synth recovers `I64`, not the
`U8` the grammar wrote — because the width is destroyed upstream at lowering.

Loadavg is labelled (`:125`, 6.15 — loud), but P6 makes **zero throughput
claims**; every measurement is a deterministic pass/fail or type count, so the
loud load is immaterial to the conclusion. The labelling discipline is correct
even where moot.

## Where the prototype is mildly generous

The headline verdict word is "SUPPORTS" (`:13`). For a spike whose central novel
machinery is unexercised and whose named capability (correct-width end-to-end
typing) is falsified for two-thirds of the descriptors, "PARTIALLY SUPPORTS —
reveals an upstream blocker" would be the honest register. The body corrects the
headline thoroughly, so a reader is not misled; but the one-word verdict, lifted
into a synthesis table, would over-read. Confidence 80% (`:34`) is fair.

One number-hygiene slip: `:131` labels the gs synth range `fn4..12,16..27` as
"(22 tags)", but that range is 21 and the line's own arithmetic says `synth=21`.
The 22 is the cross-grammar total (1 json + 21 gs, correctly stated at `:62`).
Non-load-bearing, but the "22" is reused with two different denotations.

Integration risk is un-priced. The load-bearing replacement for
`generate.rs:371/:399` (`:116-123`) is a code sketch; the spike runs its own
synth/check in a separate test file and never swaps into the real generate path.
The algorithm is validated against real descriptors; the integration is not.

## Where the SPEC over-claims beyond this evidence

**1. "P6 de-risks 1-3 over two grammars" (SPEC §6, line 228) is not supported.**
By P6's own accounting: step 1 (synth, kill `unwrap_or(Span)`) is de-risked but
only to `I64`, not to correct width; step 2 (HostSigTable + unifier) is de-risked
**synthetically only** — the corpus has no `@host fn` and drops call args, so the
param edge is unreachable (`:162-166`); step 3 (`Record`/`Sum`) is **absent from
the corpus entirely** (`:173`), untested. The spec should read "de-risks step 1
over two grammars; steps 2-3 validated synthetically / not present in the corpus."

**2. The TW5 close gate has a correctness hole.** SPEC line 356 gates TW5 on
"9 grammars type end-to-end with zero `unwrap_or(Span)`." P6 proves that exact
condition is satisfiable while 22 discriminant tags land at `I64` instead of
`U8` (`:30-32`, `:182-183`) — the gate passes on an outcome P6 flags as wrong.
"Zero Span defaults" is necessary, not sufficient; the gate must also assert
correct width. And §6 never schedules the prerequisite P6 identifies as step 0
of any type tranche: the upstream lowering fix that preserves the suffix and the
host-call args (`value_expr/literals.rs:7,17`; `expression/wrap.rs:589-606`;
dropped `FnCall` args — `:60-66`, `:154-159`). That fix is unnamed in §6 and in
the TW5 manifest bullet. A deferral without disposition.

**3. Step 2 is substrate-without-consumer — the spec's own §1 pathology.** The
HostSigTable + unifier land in TW5, but the `@host` surface that would feed and
exercise them lands only in TW6 ("land-or-delete the closure/@host surface",
line 359). P6 finding #3 (`:162-166`) confirms the table is unreachable on the
real corpus today. This is precisely the "no substrate lands without its consumer
in the same wave" law (§1.2, lines 50-51). Either @host declarations move into
TW5, or the unifier is explicitly deferred-with-disposition to TW6 and its
landing gated on the consumer — the spec currently does neither.

## Corpus-representativeness caveat (not a fault, but a bound)

The two grammars are well-chosen against JSON/CSS overfitting (google-sheets is a
third surface). But neither contains a `-> f1 -> f2` chain, a generic rule, a
record, a sum, or a typed host argument (`:24-26`, `:173`) — i.e. none of the
features that justify the unifier and none that steps 3-4 are built for. The
spike therefore validates the parts of §6 the corpus does not actually need
(bidirectional check/synth + one coercion + integer defaulting) and leaves the
parts it is built for (the HM machinery) to synthetic proof. This is disclosed,
but it bounds how much "de-risking" TW5 can claim from P6.

## Convergence

64/100. The instrument itself is ~80-settled — method honest, grounded in the
real frontend, faithful to its own code, self-correcting. But the item is the
prototype *and* the §6 section it tests, and §6 carries three unreconciled gaps
that P6's evidence directly surfaces: the "de-risks 1-3" over-claim, the
width-blind TW5 gate plus the unscheduled lowering prerequisite, and the
step-2 substrate-without-consumer ordering. Each needs a spec revision, not more
prototyping. Another loop pass is required on the spec side; the prototype does
not need to re-run.
