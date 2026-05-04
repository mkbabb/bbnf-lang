# Hardening — Pass A (Parse Front)

Date: 2026-05-03
Target: `restart/audit/passes/PASS-A.md` — Pass A synthesis (829 lines)
Auditor: hardening agent under `restart/prompts/HARDENING.md`
Hard cap: 45 minutes; this report committed before the cap.

---

## §1 — Target identification

Pass A synthesis at `/Users/mkbabb/Programming/bbnf-lang/restart/audit/passes/PASS-A.md`,
829 lines, commit `6e74a4b1` per dispatch. Six per-agent reports under
`restart/audit/per-agent/pass-a-agent-{1..6}-*.md` (2 351 lines aggregate)
form the substrate. The synthesis adjudicates inter-agent disagreement
and produces the verdict ledger, transposition ratifications, new-facility
ledger, cross-cut ledger, lock + precept verdicts, punch list (W0–W8 of
46 surgeries), and greenfield commitments.

The hardening adversary holds these in front of itself and walks the
nine lanes from `restart/prompts/HARDENING.md` §Lanes. The locks at
`restart/locks/14-LOCKS.md` are settled; the precepts are settled; the
greenfield mandate is settled; **Amendment 01** at
`restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md`
retracts per-grammar declaration crates as a default and supersedes
prior plan text — Pass A pre-dates that amendment and must be checked
against it.

---

## §2 — Cohort verdict

| Lane | Verdict | Faults | Recommendation |
|---|---|---:|---|
| 1 Lock-Adherence | partial | 4 | reconcile per-grammar-crate references with Amendment 01; surface Lock 12 silence; resolve Lock 9 adjudication; tighten Lock 11 alternative |
| 2 Sequencing Discipline | N/A | 0 | single-pass synthesis; Pass A names only intra-pass dependencies |
| 3 Cohesion | partial | 5 | LOC budgets for SPLIT obligations missing; eight new-facility receivers carry no gate; cohort-template generator origin-tranche silent |
| 4 SOTA Anchoring | honoured | 0 | Pass A is pre-codegen; Lock 8 honoured-as-silent (n/a) is correct |
| 5 Grammar-Authoritative | violated | 7 | per-grammar declaration crates as a default contradicts Amendment 01; per-X table for "all 7 Lock 14 sites" missing; future-grammar onboarding test absent; bbnf-host-prims absent |
| 6 Generated-Code Budget | partial | 3 | only one wave (W7) carries explicit LOC; aggregate budget elides Pass B carry; per-grammar generated LOC delta projection missing |
| 7 Friction Forecast | violated | 6 | no `pointer!` doc page; no `parse_in` lifetime cookbook; no Lock 9 adjudication user-side mental model; no layout-lowering error verbatim; no Pratt+SIMD misfire diagnostic; no migration page for 15-crate fragmentation |
| 8 Carry & Deferral | partial | 4 | "synthesizer adjudicates" appears 5 times without receiver/blocker/gate triple; Pass B carries enumerated but not gated |
| 9 Greenfield Discipline | partial | 3 | per-grammar-crate-as-escape-hatch elevates the safety valve; cohort-template route requires re-anchoring under amendment; `Box::leak` unresolved |

**Final decision: requires amendments.** Pass A's substantive findings
(Lock 14 retirement plan, Lock 13 SPLIT obligations, Lock 7 path
triplet, Lock 11 path-dep promotion, Lock 2 vocabulary rename) survive
the lanes. The amendments needed are mechanical: re-anchor every
per-grammar-crate reference to Amendment 01's `bbnf-host-prims` +
template-emitted-subdirectory shape; gate the eight new facilities;
specify the friction surfaces; resolve the synthesizer-adjudication
triumvirate. Pass A does not require re-draft; it requires a punch-list
patch under the amendment. The synthesizer must pick this up.

---

## §3 — Lane 1 — Lock-Adherence

Per-lock walk against the 14 locks at `restart/locks/14-LOCKS.md`.

### Lock 1 — Tape and columnar dead

`restart/audit/passes/PASS-A.md:415` reads "substantively-honoured; ~9
narrative-residue scrubs". The 9 residue sites enumerate at
`restart/audit/per-agent/pass-a-agent-3-lock-adherence.md:30-38`. No
live tape code in Pass A scope. Verification command at
`pass-a-agent-3-lock-adherence.md:43-46` returns zero non-comment hits.

| Site | Substance | Verdict |
|---|---|---|
| `restart/audit/passes/PASS-A.md:415` | Lock 1 verdict ledger row cites 9 narrative residues; surgery is comment scrub at W0 | honoured |
| `restart/audit/passes/PASS-A.md:466-471` | Punch-list item 4 enumerates the 15 sites for tape narrative scrub | honoured |

**Lane 1 / Lock 1 verdict: honoured.**

### Lock 2 — Layout lowering canonical

`restart/audit/passes/PASS-A.md:416` reads "violated; one rename pass".
The synthesis identifies `TypeDesc`, `StructLayout`, `TypeMap` as
Lock-2-retired vocabulary. Surgery batched into W2 punch-list items
12–14 + 16. New facility 8 (`LayoutSink`) at line 324 is the
Lock-2-named consumer trait, ratified.

| Site | Substance | Verdict |
|---|---|---|
| `restart/audit/passes/PASS-A.md:416` | Lock 2 verdict ledger row | honoured |
| `restart/audit/passes/PASS-A.md:524-545` | W2 batch | honoured |
| `restart/audit/passes/PASS-A.md:324` | new facility 8 (`LayoutSink`) | honoured |

**Lane 1 / Lock 2 verdict: honoured.**

### Lock 3 — Cursor-parse + byte-skip unified

`restart/audit/passes/PASS-A.md:417` reads "honoured". Pass A scope is
the lowering side; the consult-site is in `path/cursor.rs` (Pass A) and
the eager-empty-path elision is in `generated/json.rs` (Pass B). Pass A
verifies the consult-site shape; the cost-on-eager-path is a Pass B
concern. Correctly scoped.

**Lane 1 / Lock 3 verdict: honoured.**

### Lock 4 — Per-domain orthogonal optimisation

`restart/audit/passes/PASS-A.md:418` reads "honoured". The structural
decomposition (csp_strategy + egraph + recognizers) keeps the four
optimisation domains separate. Pass A's Proposal 2 fracture preserves
the boundary by routing each into `bbnf-passes/`'s sub-tree, never a
unified solver. Correct.

**Lane 1 / Lock 4 verdict: honoured.**

### Lock 5 — IR + per-backend lower

`restart/audit/passes/PASS-A.md:419` reads "substantively honoured; one
redress". The redress is the relocation of Rust-specific path strings
from `crates/ir/src/registry/strategy.rs:130-185` into
`bbnf-codegen/src/rust/` per Pass A punch-list item 9 at line 502.
This honours the Lock 5 contract.

| Site | Substance | Verdict |
|---|---|---|
| `restart/audit/passes/PASS-A.md:419` | Lock 5 verdict | honoured |
| `restart/audit/passes/PASS-A.md:502-508` | punch-list 9 surgery | honoured |

**Lane 1 / Lock 5 verdict: honoured.**

### Lock 6 — xtask emits committed source artefacts

`restart/audit/passes/PASS-A.md:420` reads "honoured". Pass A is
pre-codegen; the workspace metadata and the committed `generated/`
artefact survive untouched. Correct scoping.

**Lane 1 / Lock 6 verdict: honoured.**

### Lock 7 — Consolidated path crate

`restart/audit/passes/PASS-A.md:421` reads "violated". Pass A's
Proposal 3 (`restart/audit/passes/PASS-A.md:265-280`) names the
post-restart triplet `path-core/`, `path/`, `path-ts/` and routes the
`crates/core/src/path/` runtime executor INTO `path-core/src/runtime/`.
W3 punch-list items 24–26 (line 572-591) execute. The proposal
matches Lock 7's named footnote shape.

**Lane 1 / Lock 7 verdict: honoured.**

### Lock 8 — Surpass sonic-rs / simdjson / lightning-css

`restart/audit/passes/PASS-A.md:422` reads "honoured (Pass A is
pre-codegen)". The lock binds Pass B's runtime measurement; Pass A's
IR shape does not block surpassing SOTA. Correct.

**Lane 1 / Lock 8 verdict: honoured (n/a per scope).**

### Lock 9 — Slice-borrow primary

`restart/audit/passes/PASS-A.md:423` reads "violated at one site". The
site is `crates/core/src/grammar/mod.rs:57`'s `Box::leak`. Pass A
defers adjudication: punch-list item 45 at line 694 reads "Synthesizer
chooses: (a) public-API change forcing `&'static`-able input, OR (b)
introduce `parse_grammar_in(input, &bump)` arena variant per Lock 9".

**Fault.** Per `restart/prompts/HARDENING.md` Lane 8 (Carry & Deferral):
every "user adjudicates" / "synthesizer adjudicates" must name receiver
+ blocker + gate. This adjudication has receiver (synthesizer) but no
blocker named (why can't Pass A pick?) and no receiving-gate named
(which master-plan §receives the verdict?). Pass A should pick (b) —
the `parse_grammar_in(input, &bump)` arena variant — because Lock 9
explicitly names `parse_in(input, &bump)` as the bumpalo escape hatch
(`restart/locks/14-LOCKS.md:50`); option (a) would force a public-API
break with no compensating ergonomic value, and the ergonomics are why
the leak existed.

**Surgery.** Pass A line 39 (verdict ledger row for `grammar/mod.rs`)
and line 405-406 (residue 7) and line 694-698 (punch-list 45) all
re-write to: "introduce `parse_grammar_in(input, &bump)` arena variant
per Lock 9, deprecate the leaking entry to `parse_grammar_owned`. Lock
9 honours-by-construction; Lock 9 ledger row updates from violated to
honoured-with-W8-surgery."

**Lane 1 / Lock 9 verdict: violated (deferral fault).**

### Lock 10 — Pratt + SIMD auto-detected

`restart/audit/passes/PASS-A.md:424` reads "honoured". The miners
(`pratt.rs`, `operator_chain.rs`, `pattern_alphabet.rs`) carry the
detection; no `@pratt` / `@simd` directive is observed. Correct.

**Lane 1 / Lock 10 verdict: honoured.**

### Lock 11 — Path-deps for incubating sister crates

`restart/audit/passes/PASS-A.md:425` reads "violated". W0 punch-list
items 1–3 (lines 452-464) bring `parse-that`, `bbnf-regex` into the
workspace and switch `csp-solver` from versioned to path-dep. Proposal
4 (line 282-291) accepts EITHER workspace member relocation OR
git-submodule pinning, deferring the mechanism to the synthesizer.

**Fault.** The synthesizer-adjudicates pattern recurs here without a
receiver-blocker-gate triple. Pass A's Proposal 4 line 287 reads
"Sibling-repo path-dep submodule pinning … is acceptable
ALTERNATIVELY to workspace member relocation; the synthesizer adjudicates
the mechanism." But Lock 11 settles the substance — path-deps until
stable — without specifying the mechanism. This is fine, but the gate
is missing: **how does the master plan verify the choice landed?**
Either path-dep declaration in `Cargo.toml` (workspace member) or
`.gitmodules` entry (submodule) — both are greppable. Surgery: amend
Pass A line 287 to: "Sibling-repo submodule pinning is acceptable
ALTERNATIVELY; gate at master-plan close: `cargo metadata | jq
'.workspace_members[] | select(.name | startswith(\"parse-that\"))'`
returns the dep regardless of mechanism."

**Lane 1 / Lock 11 verdict: honoured-with-mechanism-vagueness.**

### Lock 12 — ser + gorgeous archive

`restart/audit/passes/PASS-A.md:426` reads "silent (Pass C scope)".
This is correct — the archive ceremony is Pass C's. But Pass A's
silence here flags the cross-pass dependency: BA W0 cannot begin until
the archive is complete (Lock 12 mandates "ser + gorgeous archive
BEFORE BA.W0"). Pass A residue ledger §5 enumerates 7 cross-pass
concerns; Lock 12's pre-condition does NOT appear in §5
(`restart/audit/passes/PASS-A.md:374-407`).

**Fault.** Lock 12 is a Pass C deliverable that gates Pass A's first
wave. Pass A's silence on this is correct in scope (Pass C owns the
ceremony) but incorrect in residue ledger (the dependency must land
in §5 for synthesizer pickup).

**Surgery.** Add to `restart/audit/passes/PASS-A.md:407` (after residue
7): "**Lock 12 archive ceremony** is a Pass C deliverable but a Pass A
W0 pre-condition. Synthesizer sequences Pass C archive BEFORE BA W0
opens. Receiving gate: `find archive/ser archive/gorgeous` returns
non-empty after Pass C archive lands."

**Lane 1 / Lock 12 verdict: silent (must-add to residue ledger).**

### Lock 13 — No god directories

`restart/audit/passes/PASS-A.md:427` reads "violated extensively (~13
SPLIT obligations + 1 god directory at `crates/core/src/`)". The 13
SPLITs enumerate at punch-list items 15–22 (W2; ~6 in IR
`passes/`) + items 34–37 (W6; ~4 in `bbnf-parse/`) + item 39 (W6;
path-core verification). The god directory is `crates/core/src/`
(11 immediate children mixing concerns), retired by Proposal 1
fracture. Both surgeries land at clear waves.

| Site | Substance | Verdict |
|---|---|---|
| `restart/audit/passes/PASS-A.md:427` | Lock 13 verdict | honoured |
| `restart/audit/passes/PASS-A.md:538-568` | W2 IR splits | honoured |
| `restart/audit/passes/PASS-A.md:642-664` | W6 splits in core / sister crates | honoured |
| `restart/audit/passes/PASS-A.md:618-622` | core fracture (W4) retires the god directory | honoured |

**Lane 1 / Lock 13 verdict: honoured.**

### Lock 14 — Full grammar generalisation

`restart/audit/passes/PASS-A.md:428` reads "violated at 7 sites". The
7 sites enumerate at line 332-343 (cross-cut ratification 4.1). Surgery
at W1 punch-list items 5–11 (line 473-519). The 7 retirement steps
land in one coordinated wave.

**Fault — Amendment 01 conflict.** Pass A line 33 (verdict ledger row
for `css_types.rs`) names the successor `crates/bbnf-grammar-css-l4/src/host.rs`,
"per-grammar declaration crate per Lock 14 footnote." This is the
overfitting Amendment 01 retracts. Per
`restart/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-24`,
the post-restart workspace carries **zero per-grammar crates**; the
host-fn surface is the generic `bbnf-host-prims` crate consumed via
metadata-declared composition or extended-BBNF directives.

Pass A's seven Lock 14 retirement steps that depend on
"per-grammar declaration crates" are:

| Pass A line | Statement | Amendment 01 redress |
|---|---|---|
| 33 | `css_types.rs` → `bbnf-grammar-css-l4/src/host.rs` | re-anchor: `parse_hex_color` becomes a composition over `bbnf-host-prims` primitives (`parse_hex_pair × 3 + Color::Rgb`); composition lives in CSS L4 grammar metadata or extended-BBNF directive; no `bbnf-grammar-css-l4` crate |
| 85 | `path/markers.rs` ZSTs → "per-grammar declaration crates emit `pub struct <G>;`" | re-anchor: ZSTs emit at `crates/bbnf-runtime/src/grammars/<g>/marker.rs` (template-emitted subdirectory under `bbnf-runtime`), not in a per-grammar crate |
| 241 | Proposal 1 box reads "bbnf-grammar-css-l4/ ← per-grammar declaration crate (host fns)" + "(other per-grammar declaration crates as needed)" | retract: replace with "bbnf-host-prims/ ← generic primitive library; per-grammar host-fn composition declared in workspace metadata" |
| 322 | new facility 6: "Per-grammar declaration crate template … `crates/bbnf-grammar-<g>/` (per grammar that needs host fns) … CSS L4 is the first instantiation" | retract entirely; replace with "bbnf-host-prims primitive library + workspace-metadata-declared composition (or extended-BBNF `@host` directives)" |
| 336 | cross-cut 4.1 step 1: "Move `css_types.rs` to `crates/bbnf-grammar-css-l4/src/host.rs`" | re-anchor: "Reformulate `parse_hex_color` as `bbnf-host-prims::compose(regex_capture, parse_hex_pair × 3, Color::Rgb)`; the composition lives in `[workspace.metadata.bbnf.grammars.css_l4.host_fns]`" |
| 342 | cross-cut 4.1 step 7: "Relocate `path/markers.rs` ZSTs to per-grammar declaration crates" | re-anchor: "Relocate `path/markers.rs` ZSTs to template-emitted `crates/bbnf-runtime/src/grammars/<g>/marker.rs`" |
| 478-481 | punch-list 5: "Create `crates/bbnf-grammar-css-l4/`" | retract: replace with "Create `crates/bbnf-host-prims/` (the primitive library) + add CSS L4 host-fn composition to workspace metadata" |
| 519 | punch-list 11: "Move per-grammar ZSTs … to per-grammar declaration crates (or to `crates/bbnf-codegen/src/generated/<g>/marker.rs` as xtask emit output)" | re-anchor first option to "to template-emitted `crates/bbnf-runtime/src/grammars/<g>/marker.rs`"; second option already amendment-conformant |
| 722 | new-crate creation list row: `crates/bbnf-grammar-css-l4/` | retract; replace with `crates/bbnf-host-prims/` |
| 759 | file-migration row: `crates/core/src/css_types.rs` → `bbnf-grammar-css-l4/src/host.rs` | re-anchor: the migration target is `bbnf-host-prims/src/composition_table.rs` + `[workspace.metadata.bbnf.grammars.css_l4.host_fns]` (where the composition is declared) |

**Surgery.** Apply amendment-driven re-anchoring at the 9 sites enumerated.
The cohort of sites is greppable as
`rg -nE 'bbnf-grammar-css-l4|per-grammar declaration crate|crates/<grammar>'`
in PASS-A.md.

**Future-grammar onboarding test.** Pass A does NOT contain the future-grammar
onboarding test (a hypothetical `yaml.bbnf` adding via 2 declarative
surfaces — Amendment 01 contracts the test from Lock 14's "3 surfaces"
to "2 surfaces"). The test must land. Surgery: add §6.5 to PASS-A.md:

> **Future-grammar onboarding test.** A hypothetical `yaml.bbnf`
> grammar adds via:
> 1. Drop `grammar/yaml/yaml.bbnf` into the source tree
> 2. Add `[workspace.metadata.bbnf.grammars.yaml]` block declaring
>    strategy + host-fn composition
>
> Verification: `cargo xtask regen` produces
> `crates/bbnf-runtime/src/grammars/yaml/{generated.rs, runtime.rs}`
> from the template; no edit in any other crate; `cargo nextest run -p
> bbnf-runtime` passes.

**Per-X table for "all grammars" claims.** Pass A's lock-adherence row
verdicts at line 415-441 do not table the seven Lock 14 violations
per-grammar; they list per-site. The substance is correct; the table
shape is silent. Surgery: the synthesizer's master plan should table
per-grammar (Json | CssL4 | Sheets | BBNF | csv | math | ebnf | bnf |
css_pretty) which sites apply; Pass A's per-site tabulation feeds it.

**Lane 1 / Lock 14 verdict: violated (per-grammar-crate references contradict
Amendment 01; future-grammar onboarding test absent).**

### Lock 1-14 cohort verdict

| Lock | Pass A verdict (claimed) | Hardening verdict |
|---|---|---|
| 1 | substantively-honoured | honoured |
| 2 | violated; one rename | honoured (surgery scheduled) |
| 3 | honoured | honoured |
| 4 | honoured | honoured |
| 5 | substantively honoured | honoured |
| 6 | honoured | honoured |
| 7 | violated; restructure | honoured (surgery scheduled) |
| 8 | honoured (pre-codegen) | honoured |
| 9 | violated at 1 site | violated (deferral fault) |
| 10 | honoured | honoured |
| 11 | violated | honoured-with-mechanism-vagueness |
| 12 | silent (Pass C) | silent (must-add to residue ledger) |
| 13 | violated extensively | honoured (surgery scheduled) |
| 14 | violated at 7 sites | violated (Amendment 01 conflict) |

**Lane 1 verdict: partial — 4 faults (Locks 9, 11, 12, 14).**

---

## §4 — Lane 2 — Sequencing Discipline

Per `restart/prompts/HARDENING.md` §Lane 2: "if target is a multi-wave
plan". Pass A is a single-pass synthesis, not a multi-wave plan. Pass A
DOES propose W0–W8 wave structure for the receiving BA tranche, but
this is a recommendation to the synthesizer, not authoritative wave
sequencing.

That said, Pass A's W0–W8 recommendation contains intra-pass
sequencing claims worth verifying:

| Wave | Pass A claim | Verifiable? |
|---|---|---|
| W0 | foundational, zero-risk; Lock 11 path-deps + tape narrative scrub | yes — Cargo.toml edits + comment scrubs are independent |
| W1 | Lock 14 retirement (7 sites in one wave) | yes — the 7 sites enumerate; the wave is self-contained |
| W2 | Lock 2 rename + Lock 13 IR splits | partially — the Lock 2 rename produces ~50 sites of consumer rename; the SPLITs introduce new file paths; sequencing of "rename first, then SPLIT" is the right order but Pass A does not specify |
| W3 | Lock 7 path triplet + path runtime relocation | yes — the triplet is internally cohesive |
| W4 | core fracture (depends on W3 path triplet) | yes — Pass A line 359-362 ratifies "the largest Pass A surgery depends on Lock 11 promotion + Lock 14 retirement + Lock 2 rename + path triplet landing first" |
| W5 | grammar source tree + bootstrap dev binaries | yes — the grammar tree rename is independent |
| W6 | god-module SPLITs in core / sister crates | yes — the SPLITs are within already-relocated files |
| W7 | new facilities (audit, error, validators, cohort-template, sink) | yes — additive |
| W8 | Lock 9 + BBNF aggregator | yes — terminal cleanup |

**Fault — W2 internal sequencing silent.** Pass A's W2 batch (lines
522-568) lists items 12–23 (Lock 2 rename + Lock 13 SPLITs) as a
single wave. The Lock 2 rename produces ~50 sites of consumer rename
across the IR crate (per agent-3 line 87). The Lock 13 SPLITs change
file paths within `passes/types/` → `passes/layout/`. If the rename
lands first (without SPLIT), the consumer-rename surface is identical;
if the SPLIT lands first (without rename), the consumer-rename surface
is ~50 sites + ~12 file-path renames. The interleaving matters.

**Surgery.** Add to Pass A line 521-523 (W2 wave header): "Sub-sequence:
W2.M1 = Lock 2 rename of `TypeDesc/StructLayout/TypeMap` → `Layout` at
the type level; W2.M2 = `passes/types/` → `passes/layout/` directory
rename; W2.M3-M9 = Lock 13 SPLITs against the renamed tree. The Lock 2
rename pass is mechanical; the SPLIT pass operates on the renamed
substrate."

**Lane 2 verdict: N/A (single-pass) — but Pass A's W2 sub-sequencing should
amend.**

---

## §5 — Lane 3 — Cohesion

Every claim in Pass A must be verifiable from artefacts Pass A produces
or cites. Walk for orphan claims and orphan deliverables.

### Orphan claims (claim has no supporting evidence)

| Pass A line | Claim | Evidence trail |
|---|---|---|
| 731 | "Total new crates: 15" | sums to 15 from the table at line 716-731 — verifiable |
| 790 | "Aggregate Pass A LOC delta: ~−210" | sums approximately from the per-wave delta table at line 778-789; the W3 −640 + W7 +700 dominates; ~−210 is in range. Verifiable. |
| 791-793 | "Hand-written-source LOC retired: ~3000 LOC (per CENSUS §10.5)" | citation: `restart/corpora/CENSUS.md` §10.5. Verifiable (citation present). |
| 791-793 | "Hand-written-source LOC added: ~700 LOC (new facilities)" | matches W7 +700; verifiable |

| Pass A line | Claim | Evidence missing |
|---|---|---|
| 685 | "~250 LOC new + ~−1500 LOC retired hand-written cohort runtime (Pass B's ~1500 LOC of mechanical instantiation)" | the ~1500 LOC retirement is a Pass B carry; Pass A claims the LOC delta in its own ledger but the substance lives in Pass B scope. **Fault** — the LOC delta belongs in Pass B's ledger, not Pass A's. Surgery: line 685 should read "~250 LOC new (Pass A's xtask command + spec); ~−1500 LOC retired hand-written cohort runtime (Pass B carry; cited at residue §5.3)." |
| 805 | "`inverse-layout-audit` build gate passes for every grammar in workspace metadata" | "every grammar" — Lock 14 sensitivity. The gate must enumerate the per-grammar table; today the audit runs against the workspace-metadata-declared grammar set. Per Lock 14, the grammar set IS the workspace metadata (pluggable). Verifiable; honoured. |
| 681 | "CI gate: `cargo xtask validate-metadata --check`" | the validator's substance is the schema; Pass A's new facility 3 line 319 names the surface but no schema example appears in PASS-A.md (it does in agent-5 line 38). Surgery: cite agent-5's schema example or fold it into Pass A. |

### Orphan deliverables (deliverable has no consuming wave)

| Deliverable | Pass A wave | Consumer | Verdict |
|---|---|---|---|
| `bbnf-error` crate (new facility 4) | W7 (item 41) | "Per-crate error types impl `BbnfError`" — the consumers are every crate; line 723 names per-crate impl as part of the surgery | **partial fault** — Pass A creates the crate but does not enumerate which per-crate error types impl which categories. The Lock 14 retirement at W1 is gated on `GrammarAuditTag` but doesn't reference `bbnf-error`. Surgery: add to PASS-A.md line 723: "Per-crate impl gate: `bbnf-parse::Error`, `bbnf-codegen::Error`, `bbnf-runtime::Error`, `path-core::PathError`, `bbnf-passes::LayoutError` (post Lock 2) all implement `BbnfError`; gate at W7 close: `rg -n 'impl BbnfError for' crates/{bbnf-parse,bbnf-codegen,bbnf-runtime,path-core,bbnf-passes}/src/` returns ≥5 hits." |
| Cohort-template generator (new facility 5) | W7 (item 43) | Pass B's runtime side; "~−1500 LOC retired hand-written cohort runtime (Pass B carry)" at line 685 | **partial fault** — the carry is named but no Pass B receiving wave is cited. Pass A residue §5.3 line 387-389 names "Pass B owns the runtime side" but doesn't name a wave. Surgery: residue §5.3 amends to "Pass B's wave receives at the synthesizer-determined receiving locus; gate: `find crates/bbnf-runtime/src/grammars -mindepth 1 -maxdepth 1 -type d \| wc -l` returns ≥9". |
| `LayoutSink` trait (new facility 8) | W7 (item 44) | Pass B's emitters; line 324 "the Rust emitter, TS emitter, WASM emitter each impl `LayoutSink`" | **fault** — TS + WASM emitters are BD+ scope per Lock 5; Pass A claims a deliverable whose consumer is post-BA. Surgery: amend PASS-A.md line 324: "Rust emitter implements `LayoutSink` at BA; TS + WASM emitters implement at BD (Pass C carry — see residue §5.7)." Add the Pass C carry to residue ledger. |
| Workspace-metadata schema validator (new facility 3) | W7 (item 42) | CI; the gate at line 804 cites it | honoured (consumer is CI; gate exists) |
| Inverse-layout-audit pass (new facility 1) | W7 (item 40) | xtask regen; the gate at line 805 cites it | honoured |

**Lane 3 verdict: partial — 5 fault rows (cohort-template Pass B carry, LayoutSink BD carry, bbnf-error per-crate gate, validate-metadata schema citation, ~250 LOC misattribution).**

---

## §6 — Lane 4 — SOTA Anchoring

Per `restart/prompts/HARDENING.md` §Lane 4: every parse-throughput
gate cites a competitor + dataset + platform. Non-throughput
engineering gates must NOT claim Lock 8 honour.

Pass A is pre-codegen. The Lock 8 verdict at line 422 reads "honoured
(Pass A is pre-codegen)" with citation "(n/a)". This is correct: Pass
A's hard-gate table (lines 798-807) names ZERO parse-throughput gates.
Every gate is engineering (rg returns 0; cargo check passes; find
returns 0; cargo xtask validate-metadata --check; cargo nextest run).

| Pass A gate | Substance | Lock 8 claim? | Verdict |
|---|---|---|---|
| `rg -nE 'TapeRec\|...' returns 0` | engineering | no | correct (engineering only) |
| `rg -nE 'match \w+ {[^}]*Json => ...' returns 0` | engineering | no | correct |
| `rg -nE 'TypeDesc\|...' returns 0` | engineering | no | correct |
| `find … -name "*.rs" \| xargs wc -l \| sort -n` | engineering (LOC budget) | no | correct |
| `cargo check --workspace` | engineering | no | correct |
| `cargo xtask validate-metadata --check` | engineering | no | correct |
| `inverse-layout-audit` | engineering (build gate) | no | correct |
| `find -name "*.rs" \| awk '$1 > 500'` returns 0 | engineering (LOC budget) | no | correct |
| `cargo nextest run --workspace` | engineering | no | correct |

Zero parse-throughput gates in Pass A is correct per scope. The Lock 8
"honoured" claim with "(n/a)" citation is correct.

**Lane 4 verdict: honoured.**

---

## §7 — Lane 5 — Grammar-Authoritative Discipline (Lock 14 deep dive)

This is the most consequential lane and where Pass A's primary fault
lives.

### Verification 1: Zero `match grammar { Json => …, CssL4 => …, … }` arms in proposed generic crates

Grep PASS-A.md for proposed generic-crate code containing such match
arms.

```
rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|Bbnf\w*\s*=>|GoogleSheets\w*\s*=>' restart/audit/passes/PASS-A.md
```

Two hits:

- Line 169: in the verdict-ledger ROW for `bbnf-path/src/registry.rs`,
  describing the EXISTING violation, not proposed code: "retire `match
  grammar` at L132-135"
- Line 511: in punch-list item 10's surgery description: "Replace the
  `match grammar { 'json' => ..., ... }`"

Both are descriptions of CODE TO RETIRE, not code Pass A proposes to
land. **Verification 1 passes.**

### Verification 2: Per-X tables for "all grammars" / "every grammar" / "all backends" claims

Pass A makes "all grammars" / "every grammar" claims at:

- Line 805: "passes for every grammar in workspace metadata" — pluggable per workspace metadata (correct; the table IS the metadata)
- Line 717-731: per-crate creation list — uses a tabular shape (per-X table)
- Line 740-760: file-migration list — tabular per-source-file

Pass A's claims are tabularised. **Verification 2 passes.**

### Verification 3: Future-grammar onboarding test

Pass A does NOT contain the future-grammar onboarding test. The
yaml.bbnf gedanken test from Lock 14 / Amendment 01 is absent from
PASS-A.md.

**Surgery (already noted in §3 / Lock 14).** Add §6.5 to Pass A
verdicting the 2-surface ceremony per Amendment 01:

```
6.5 Future-grammar onboarding test (Lock 14 verification)
A hypothetical `yaml.bbnf` grammar adds via TWO surfaces:
  1. Drop grammar/yaml/yaml.bbnf into the source tree
  2. Add [workspace.metadata.bbnf.grammars.yaml] declaring strategy +
     host-fn composition (over bbnf-host-prims)
Verification: `cargo xtask regen` produces template-emitted
crates/bbnf-runtime/src/grammars/yaml/{generated.rs, runtime.rs};
zero crate creation; zero edit in any other generic crate;
`cargo nextest run -p bbnf-runtime --features yaml` passes.
```

**Verification 3 fails.**

### Verification 4: Per-grammar code lives only in workspace metadata or in optional declaration crates

Per Amendment 01, **per-grammar code lives only in workspace metadata
or template-emitted runtime subdirectories** — declaration crates are
retracted. Pass A's nine sites (enumerated in §3 / Lock 14) reference
declaration crates as a *default*, not an *optional* surface. This
contradicts Amendment 01 line 9-13:

> The master plan committed at `a9a85f45` declares 33 workspace
> members, 9 of which are per-grammar declaration crates… This is
> overfitting. Lock 14 names per-grammar declaration crates as an
> *optional* escape hatch, not a default.

**Surgery (consolidated).** Apply amendment-driven re-anchoring at 9
PASS-A.md sites:

| # | Site | Pass A reads | Re-anchor to |
|---|---|---|---|
| 1 | line 33 successor for `css_types.rs` | `crates/bbnf-grammar-css-l4/src/host.rs` | `bbnf-host-prims/src/composition_table.rs` + `[workspace.metadata.bbnf.grammars.css_l4.host_fns]` |
| 2 | line 85 successor for `path/markers.rs` | "per-grammar declaration crates" | `crates/bbnf-runtime/src/grammars/<g>/marker.rs` (template-emitted) |
| 3 | line 241 Proposal 1 box | `bbnf-grammar-css-l4/ … (other per-grammar declaration crates as needed)` | strike; replace with `bbnf-host-prims/ ← generic primitive library; per-grammar host-fn composition declared in workspace metadata` |
| 4 | line 322 new-facility 6 | "Per-grammar declaration crate template" | retract; replace with "`bbnf-host-prims` primitive library + workspace-metadata-declared composition" |
| 5 | line 336 cross-cut 4.1 step 1 | "Move `css_types.rs` to `crates/bbnf-grammar-css-l4/src/host.rs`" | "Reformulate `parse_hex_color` as composition over `bbnf-host-prims` primitives in workspace metadata" |
| 6 | line 342 cross-cut 4.1 step 7 | "Relocate to per-grammar declaration crates" | "Relocate to template-emitted subdirectories under `crates/bbnf-runtime/src/grammars/<g>/`" |
| 7 | line 478-481 punch-list 5 | "Create `crates/bbnf-grammar-css-l4/`" | "Create `crates/bbnf-host-prims/`; add CSS L4 host-fn composition to workspace metadata" |
| 8 | line 519 punch-list 11 | "to per-grammar declaration crates" | "to template-emitted `crates/bbnf-runtime/src/grammars/<g>/marker.rs`" |
| 9 | line 722 new-crate creation list row | `crates/bbnf-grammar-css-l4/` | `crates/bbnf-host-prims/` |
| 10 | line 759 file-migration row | `bbnf-grammar-css-l4/src/host.rs` | `bbnf-host-prims/src/composition_table.rs` (mechanism) + workspace metadata block (data) |

Pass A's "Total new crates: 15" claim at line 731 changes to "Total
new crates: ~15 modulo amendment-01 compliance — `bbnf-grammar-css-l4`
retracts; `bbnf-host-prims` adds; `bbnf-runtime-template` adds;
template-emitted subdirs under `bbnf-runtime/src/grammars/` are NOT
new crates."

**Verification 4 fails on 10 sites.**

**Lane 5 verdict: violated.**

---

## §8 — Lane 6 — Generated-Code + LOC Budget

Per `restart/prompts/HARDENING.md` §Lane 6: per-wave LOC budget +
xtask regen-cycle budget + per-grammar LOC delta projection.

### Per-wave LOC budget

Pass A line 776-789 carries a per-wave LOC delta ledger. Eight rows
(W0–W8). Substance:

| Wave | Net delta | Reason |
|---|---|---|
| W0 | ~−20 | tape narrative scrub |
| W1 | ~−250 | shape_dict_bbnf + GrammarAuditTag + manifest table + markers |
| W2 | ~+0 | Lock 2 rename + Lock 13 SPLITs (redistribute) |
| W3 | ~−640 | path triplet eliminates ~500 mirror + ~163 legacy |
| W4 | ~+0 | crate fracture (relocations) |
| W5 | ~+0 | dir renames |
| W6 | ~+0 | god-module SPLITs (redistribute) |
| W7 | ~+700 | new facilities; offset by ~−1500 LOC Pass B carry |
| W8 | ~+0 | API change |

The ledger is per-wave. Honoured per Lane 6 §Per-wave.

### xtask regen-cycle budget

Pass A is pre-codegen. The xtask regen-cycle budget binds Pass B; Pass
A's silence here is correct in scope.

But Pass A DOES carry an xtask command surface in W7 (validate-metadata,
cohort-template). The xtask invocation cost (binary size, wall time)
goes silent. **Fault** — even pre-codegen, Pass A introduces ~250 LOC
of new xtask code (cohort-template generator) and ~150 LOC validator.
The build-time cost should land somewhere. Surgery: add to PASS-A.md
line 798-807 (hard-gates table): "`cargo xtask validate-metadata --check`
wall ≤ 100 ms" + "`cargo xtask regen` wall ≤ 5 s on the in-tree
9-grammar set (Pass B will tighten)."

### Per-grammar LOC delta projection

Pass A scope is pre-codegen; per-grammar LOC delta is a Pass B
concern. Pass A's silence here is correct.

But Pass A's residue §5.6 names the relocation `crates/core/src/grammar/generated/` → `crates/bbnf-codegen/src/generated/` (~169 K LOC). This is a Pass B surgery, but Pass A claims it as a sequencing prerequisite. **Fault** — the per-grammar generated-LOC delta projection (post-relocation invariance: bbnf.rs ≤ 22 K, css_l4.rs ≤ 110 K, json.rs ≤ 3.7 K) does NOT appear in Pass A's residue ledger nor in §8.4 budget ledger. Surgery: add to PASS-A.md line 793 (after aggregate Pass A LOC delta): "**Pre-Pass-B sequencing invariant**: post-relocation, `find crates/bbnf-codegen/src/generated -name "*.rs" \| xargs wc -l \| awk '$1 > 110000 || ($1 > 22000 && $0 !~ /css_l4/)'` returns 0; bytes pre/post relocation are byte-identical (regen invariant)."

### LOC budget vs Pass A SPLIT obligations

Pass A line 642-664 (W6) lists 5 SPLIT obligations. Each SPLIT
redistributes a >500 LOC file into a directory module of ≤ 200 LOC
files. The redistribute LOC totals (584 + 539 + 731 + 590 + 639 = 3083
LOC) appear as "~+0" in the W6 budget. This is correct (LOC moves,
doesn't grow).

**Lane 6 verdict: partial — 3 faults (xtask wall budget; per-grammar
generated-LOC invariant; W6 SPLIT verification gate).**

---

## §9 — Lane 7 — Friction Forecast

Per `restart/prompts/HARDENING.md` §Lane 7: forecast user/grammar-author
friction at proposed APIs.

### `pointer!` macro syntax

Pass A line 322 ratifies new facility 6 (per-grammar declaration crate
template) — per Amendment 01 this retracts. The `pointer!` macro lives
in `crates/path/` (proc-macro shell post Proposal 3). Friction
candidates:

| Friction surface | Pass A coverage | Required artefact |
|---|---|---|
| `pointer!["a", "b", 1]` syntax | silent | `docs/cookbook/path-macro.md` (HARDENING-PLAN-SYNTHESIS punch-list 34 names this) |
| Compile-time path validation error: rule-name typo | silent | error verbatim: `error: rule 'foo' not found in grammar Json; suggestions: 'food', 'bar'` |
| Wildcard typed-iterator output | silent | doc page covering `pointer![..., *]` returns `impl Iterator<Item = TypedPath<G, T>>` |

**Fault.** Pass A's punch list at W7 adds the inverse-layout-audit, the
bbnf-error, the validate-metadata, the cohort-template, the LayoutSink
trait (line 666-690). It does NOT add the pointer-macro cookbook. Pass
A's residue §5 does not flag the cookbook either.

**Surgery.** Add to PASS-A.md residue ledger §5: "**8. Pointer-macro
cookbook** (`docs/cookbook/path-macro.md`) is gated by Pass A's path
triplet at W3 close but the cookbook surface itself is Pass C scope.
Synthesizer cross-references at master-plan close: BA W3 close ⇒ Pass
C cookbook receiving wave."

### `parse / parse_in / parse_owned` lifetime API

Pass A line 423 / 694-698 ratifies the W8 adjudication of `Box::leak`.
The Lock 9 escape hatches are `parse(input) -> &'i ...`,
`parse_in(input, &bump)`, `parse_owned(input)`.

Pass A's Lock 9 surgery is at one site (`grammar/mod.rs:57`). Friction
candidates:

| Friction | Pass A coverage | Required artefact |
|---|---|---|
| When to use parse vs parse_in vs parse_owned | silent | `docs/cookbook/lifetime-surfaces.md` |
| Bumpalo bump-wrap ergonomics | silent | error verbatim: `error: parse_in requires a &Bump; allocate one with: let bump = bumpalo::Bump::new(); parser.parse_in(input, &bump)` |

**Fault.** Pass A's W8 punch-list 45 names the API change but no
cookbook surface, no error verbatim. Surgery as above.

### Layout lowering errors

Pass A's new facility 1 (inverse-layout-audit at line 317) names the
build-failing pass. Friction candidates:

| Friction | Pass A coverage | Required artefact |
|---|---|---|
| What does the rule-name + span tell me about the missing layout? | partial — the agent-5 sketch at line 109-138 names the error structure | `docs/errors/layout-lowering.md` with verbatim examples |
| How do I add a typed `->` to a rule that doesn't have one? | silent | cookbook entry |

**Fault.** The audit pass exists but no error-message cookbook is
gated. Surgery: add to PASS-A.md line 671 (W7 item 40 surgery): "+ a
`docs/errors/layout-lowering.md` cookbook page emitted alongside the
audit pass; gate at W7 close: `test -f docs/errors/layout-lowering.md`
returns 0."

### Pratt + SIMD auto-detection misfire diagnostics

Pass A's Lock 10 verdict at line 424 reads "honoured". The miners are
the auto-detection. Friction candidates:

| Friction | Pass A coverage | Required artefact |
|---|---|---|
| Optimizer classified my rule as Pratt; I want it not Pratt | silent | escape directive (e.g. `@no-pratt rule_name`) — but Lock 10 forbids `@pratt`/`@simd` directives. The misfire diagnostic must be observational, not author-overridable. |
| Optimizer classified my rule as SIMD-eligible; performance regressed | silent | `docs/optimizer/pratt-simd-detection.md` with verbatim example showing the cost-model decision trace |

**Fault.** Pass A's silence on Pratt/SIMD misfire diagnostics is
inherent to its scope (Pass A is the IR side; the misfire surface is at
codegen-time). But residue §5 should flag it for Pass B. Surgery: add
to residue ledger.

### Crate split (15-crate fragmentation) migration path

Pass A line 731 declares "Total new crates: 15. Existing crates that
retire wholesale". Per Amendment 01 the count tightens to 24 total
workspace members (per AMENDMENT-01 line 102). Friction candidates:

| Friction | Pass A coverage | Required artefact |
|---|---|---|
| Consumer code that imports `bbnf::path::*` after the split | silent | `docs/migration/bc-core-split.md` (HARDENING-PLAN-SYNTHESIS punch-list 34) |
| Cargo.toml dep updates for sister-tranche consumers | silent | migration script |
| Public-API breakage: `bbnf::Path` becomes `path::Path` | mentioned at line 290 (Proposal 3 migration cost: "the move to `path::*` re-exported from `bbnf::path` keeps the consumer surface") | the re-export keeps surface; verbatim doc page absent |

**Fault.** Pass A names the re-export pattern but does not gate the
migration page. Surgery: add to W4 punch-list (post-fracture): "+
emit `docs/migration/bc-core-split.md` covering import paths and
Cargo.toml updates; gate at W4 close: `test -f docs/migration/bc-core-split.md`."

### Adding a new grammar (Lock 14 onboarding test from user perspective)

Already covered in §7 / Lane 5 verification 3. The future-grammar
onboarding test is absent. Friction surface = the test itself.

**Lane 7 verdict: violated — 6 friction surfaces uncovered (pointer!
macro doc, lifetime cookbook, layout-lowering error doc, Pratt+SIMD
misfire residue, migration page, future-grammar test).**

---

## §10 — Lane 8 — Carry & Deferral Audit

Per `restart/prompts/HARDENING.md` §Lane 8: every "deferred to" /
"carries to" / "future" / "TBD" / "user adjudicates" must name (a)
receiver, (b) blocker, (c) receiving gate.

Pass A's deferrals:

| Pass A line | Deferral | Receiver | Blocker | Gate | Verdict |
|---|---|---|---|---|---|
| 39 | "synthesizer adjudicates" between (a) public-API change vs (b) `parse_in(input, &bump)` for `Box::leak` | synthesizer | none cited | none cited | **fault** — see §3 / Lock 9 |
| 185 | `crates/bootstrap/src/lib.rs` either retires or shim-survives | "Synthesizer adjudicates" | resolved at line 191 ("KEEP-MODIFY") | line 191 adjudication | honoured (resolved in-pass) |
| 287 | submodule-pinning vs in-tree-relocation for parse-that | synthesizer | none cited | none cited | **fault** — see §3 / Lock 11 |
| 305 | bootstrap retirement partially ratified | synthesizer adjudication | line 305 names "name-stability outweighs the 28-LOC cost" | resolved | honoured |
| 374-407 | residue ledger — 7 cross-pass concerns | synthesizer | each row names the cross-pass concern; receiving wave silent on most | none cited | **fault** — see below |
| 405-406 | "Synthesizer adjudicates between elegance (force the lifetime) and ergonomics (provide both surfaces)" | synthesizer | the trade-off is named | the receiving gate is W8 (line 694) | honoured (terminus is W8) |
| 685 | "Pass B's ~1500 LOC of mechanical instantiation" | Pass B | the cohort-template generator; ratified at W7 | Pass B receiving wave silent | **fault** — Pass B receiving wave not named |
| 701-704 | "post-W4 relocation; current location is `crates/core/src/grammar/generated/mod.rs:35`" | W4 (W8 punch-list 46) | the relocation is W4 | gate cited | honoured |

### Residue ledger faults (§5)

| § | Substance | Receiver named? | Blocker named? | Gate named? |
|---|---|---|---|---|
| 5.1 | bbnf-error consolidation spans all three passes | "Synthesizer reconciles" | each pass's adoption | none |
| 5.2 | bbnf-ir → bbnf-codegen boundary | Pass B's verdict | shared with Pass B | none |
| 5.3 | per-grammar runtime template (cohort generator) | shared with Pass B | metadata schema authored at A; consumed at B | none |
| 5.4 | core fracture pulls Pass B content | "Synthesizer must sequence" | the relocations are interlocked | none |
| 5.5 | parse-that workspace promotion intersects Pass C (xtask + workspace metadata + dep policy) | Pass C | Pass A names; Pass C operationalises | none |
| 5.6 | Generated/ tree relocation | Pass B surgery | sequencing prerequisite | none |
| 5.7 | Box::leak — synthesizer adjudicates | synthesizer | (a) vs (b) | terminus W8 |

Most residue rows name the receiver but not the gate. **Fault** — every
residue must terminate at a gate.

**Surgery.** Amend each residue row in §5 to carry a "receiving gate"
column. Example for §5.1:

> 1. **`bbnf-error` consolidation** — Receiver: synthesizer +
>    every-pass-adoption. Blocker: per-crate error type ergonomics
>    must be agreed before consolidation. Receiving gate: at master-plan
>    close, `rg -n 'impl BbnfError for' crates/{bbnf-parse,bbnf-codegen,bbnf-runtime,path-core,bbnf-passes}/src/`
>    returns ≥5 hits.

Apply equivalent gate-citation to §5.2-§5.7 (and add §5.8 for the
pointer! cookbook per §9).

**Lane 8 verdict: partial — 4 fault rows (Lock 9 deferral, Lock 11
mechanism, residue 5.1-5.6 missing gates, Pass B receiving wave for
cohort-template).**

---

## §11 — Lane 9 — Greenfield Discipline

Per `restart/prompts/HARDENING.md` §Lane 9 + the user's mandate:

- No quick solutions
- No workarounds
- No legacy code survives uncontested
- Idiomatic, gestalt approaches
- Architectural transpositions for elegance / simplicity / performance

### No quick solutions

Pass A's surgeries are substrate-respecting. The Lock 14 retirement
(W1) is a coordinated 7-site wave, not a per-site patch. The Lock 13
SPLITs (W2 + W6) restructure rather than annotate. The Lock 7 path
triplet (W3) absorbs the runtime executor rather than dual-maintains.
Honoured.

**Fault.** The per-grammar declaration crate as a default (per §3 /
Lock 14) IS a quick solution — Lock 14 names per-grammar declaration
crates as an *optional* escape hatch; Pass A elevates the safety valve
to a default. Amendment 01 retracts. Greenfield discipline is honoured
by Amendment 01, not by Pass A's pre-amendment text. **Surgery already
enumerated in §3 / Lock 14 / 9 sites.**

### No workarounds

Pass A surfaces 3 workaround sites:
- `Box::leak` at `grammar/mod.rs:57` — synthesizer adjudicates (W8)
- Wildcard `@debug` strip-prefix at `grammar/host.rs:387` — FAIL-EXPLICIT (W6 punch-list 34)
- Defensive fallback at `lower/value_expr/simple_kinds.rs:185` — `unreachable!()` or fix upstream (W6 punch-list 38)

All three are surgical at root, not patched. Honoured.

### No legacy code survives uncontested

Pass A's verdict ledger §1 covers ~200 hand-written files. Each carries
a bucket: KEEP-OUTRIGHT / KEEP-MODIFY / ABROGATE-DELETE / ABROGATE-MOVE
/ ABROGATE-REPLACE. No file survives without justification. Honoured.

### Idiomatic, gestalt approaches

Pass A's Proposals 1-6 are gestalt (whole-system rewrites). The Lock 13
SPLITs are systemic (~13 obligations across the codebase). Honoured.

### Architectural transpositions for elegance / simplicity / performance

Pass A's six proposals are architectural transpositions:
- Proposal 1: `crates/core/` god directory → 3 sibling crates + thin aggregator
- Proposal 2: `crates/ir/` → 3 sibling crates (IR types / passes / VM)
- Proposal 3: path crate triplet (Lock 7's named shape)
- Proposal 4: workspace promotion of sister crates (Lock 11)
- Proposal 5: grammar source tree uniform layout (Lock 13/14)
- Proposal 6: bootstrap retirement (Lock 13 + KISS)

All six honour the mandate.

**Lane 9 verdict: partial — 3 faults (per-grammar-crate default vs Amendment
01; Box::leak adjudication unresolved; cohort-template route requires
re-anchoring under Amendment 01).**

---

## §12 — Punch list

Ordered for the synthesizer + amendment agent. Each entry: target /
surgery / owner / scope / lanes producing.

### A — Lock 14 / Amendment 01 reconciliation (10 sites)

| # | Target | Surgery | Owner | Scope | Lanes |
|---|---|---|---|---|---|
| 1 | `restart/audit/passes/PASS-A.md:33` | re-anchor successor for `css_types.rs` from `bbnf-grammar-css-l4/src/host.rs` to `bbnf-host-prims/src/composition_table.rs` + `[workspace.metadata.bbnf.grammars.css_l4.host_fns]` | amendment agent | single-line | 1, 5 |
| 2 | `restart/audit/passes/PASS-A.md:85` | re-anchor "per-grammar declaration crates" to "template-emitted `crates/bbnf-runtime/src/grammars/<g>/marker.rs`" | amendment agent | single-line | 1, 5 |
| 3 | `restart/audit/passes/PASS-A.md:241` | strike `bbnf-grammar-css-l4/` row from Proposal 1 box; replace with `bbnf-host-prims/` row | amendment agent | paragraph | 1, 5 |
| 4 | `restart/audit/passes/PASS-A.md:322` | retract new-facility 6 (per-grammar declaration crate template); replace with "`bbnf-host-prims` primitive library + workspace-metadata-declared composition" | amendment agent | row replacement | 1, 5, 9 |
| 5 | `restart/audit/passes/PASS-A.md:336` | re-anchor cross-cut 4.1 step 1 from "Move `css_types.rs` to `crates/bbnf-grammar-css-l4/src/host.rs`" to "Reformulate `parse_hex_color` as composition over `bbnf-host-prims` primitives in workspace metadata" | amendment agent | single-line | 1, 5 |
| 6 | `restart/audit/passes/PASS-A.md:342` | re-anchor cross-cut 4.1 step 7 from "to per-grammar declaration crates" to "to template-emitted subdirectories under `crates/bbnf-runtime/src/grammars/<g>/`" | amendment agent | single-line | 1, 5 |
| 7 | `restart/audit/passes/PASS-A.md:478-481` | retract punch-list 5 ("Create `crates/bbnf-grammar-css-l4/`"); replace with "Create `crates/bbnf-host-prims/`; add CSS L4 host-fn composition to workspace metadata" | amendment agent | paragraph | 1, 5 |
| 8 | `restart/audit/passes/PASS-A.md:519` | re-anchor "to per-grammar declaration crates" to "to template-emitted `crates/bbnf-runtime/src/grammars/<g>/marker.rs`" | amendment agent | single-line | 1, 5 |
| 9 | `restart/audit/passes/PASS-A.md:722` | retract `crates/bbnf-grammar-css-l4/` row; add `crates/bbnf-host-prims/` row + `crates/bbnf-runtime-template/` row | amendment agent | row replacement | 1, 5 |
| 10 | `restart/audit/passes/PASS-A.md:759` | re-anchor file-migration target from `bbnf-grammar-css-l4/src/host.rs` to `bbnf-host-prims/src/composition_table.rs` (mechanism) + workspace metadata block (data) | amendment agent | single-line | 1, 5 |

### B — Future-grammar onboarding test

| # | Target | Surgery | Owner | Scope | Lanes |
|---|---|---|---|---|---|
| 11 | `restart/audit/passes/PASS-A.md` (after §6 close) | add §6.5 "Future-grammar onboarding test" with the 2-surface ceremony per Amendment 01: drop yaml.bbnf + metadata block; verification via `cargo xtask regen` + `cargo nextest run -p bbnf-runtime` | amendment agent | new section | 5 |

### C — Carry & deferral fixes

| # | Target | Surgery | Owner | Scope | Lanes |
|---|---|---|---|---|---|
| 12 | `restart/audit/passes/PASS-A.md:39, 405-407, 694-698` | resolve `Box::leak` adjudication — Pass A picks (b) `parse_grammar_in(input, &bump)` per Lock 9 escape hatch; deprecate the leaking entry to `parse_grammar_owned`; receiving gate: BA W8 close | amendment agent | multi-line | 1, 8 |
| 13 | `restart/audit/passes/PASS-A.md:287` | tighten Lock 11 mechanism: "Sibling-repo submodule pinning is acceptable ALTERNATIVELY; gate at master-plan close: `cargo metadata` returns the dep regardless of mechanism" | amendment agent | paragraph | 1, 8 |
| 14 | `restart/audit/passes/PASS-A.md` (residue ledger §5) | add receiving-gate column to each residue row; specify gate at master-plan or wave-close granularity | amendment agent | multi-row | 1, 3, 8 |
| 15 | `restart/audit/passes/PASS-A.md` (residue ledger §5) | add §5.8: "Lock 12 archive ceremony is a Pass C deliverable but a Pass A W0 pre-condition; receiving gate: `find archive/ser archive/gorgeous` returns non-empty after Pass C archive lands" | amendment agent | new row | 1, 8 |
| 16 | `restart/audit/passes/PASS-A.md:685` | re-attribute LOC delta: "~250 LOC new (Pass A's xtask command + spec); ~−1500 LOC retired hand-written cohort runtime (Pass B carry; cited at residue §5.3)" | amendment agent | single-line | 3, 6 |

### D — Friction surfaces (cookbooks + error verbatims)

| # | Target | Surgery | Owner | Scope | Lanes |
|---|---|---|---|---|---|
| 17 | `restart/audit/passes/PASS-A.md` (residue ledger §5) | add §5.9: "Pointer-macro cookbook (`docs/cookbook/path-macro.md`); receiving gate: BA W3 close ⇒ Pass C cookbook receiving wave; verbatim error gate: `error: rule '<X>' not found in grammar <G>; suggestions: …`" | amendment agent | new row | 7, 8 |
| 18 | `restart/audit/passes/PASS-A.md` (residue ledger §5) | add §5.10: "Lifetime-surfaces cookbook (`docs/cookbook/lifetime-surfaces.md`); receiving gate: BA W8 close + verbatim error gate" | amendment agent | new row | 7, 8 |
| 19 | `restart/audit/passes/PASS-A.md:671` | append to W7 item 40 surgery: "+ `docs/errors/layout-lowering.md` cookbook page; gate at W7 close: `test -f docs/errors/layout-lowering.md`" | amendment agent | single-line | 7 |
| 20 | `restart/audit/passes/PASS-A.md` (residue ledger §5) | add §5.11: "Pratt/SIMD misfire diagnostics — Pass B receiving wave; gate cites `docs/optimizer/pratt-simd-detection.md`" | amendment agent | new row | 7, 8 |
| 21 | `restart/audit/passes/PASS-A.md` (W4 punch-list section) | append: "Emit `docs/migration/bc-core-split.md` covering import paths + Cargo.toml updates; gate at W4 close: `test -f docs/migration/bc-core-split.md`" | amendment agent | single-line | 7 |

### E — Cohesion + budget gaps

| # | Target | Surgery | Owner | Scope | Lanes |
|---|---|---|---|---|---|
| 22 | `restart/audit/passes/PASS-A.md:319, 678, 723` | cite agent-5 schema example (or fold it into PASS-A.md) for `validate-metadata` validator; specify per-crate impl gate for `bbnf-error` (≥5 impls); confirm cohort-template generator's Pass B receiving wave | amendment agent | multi-line | 3 |
| 23 | `restart/audit/passes/PASS-A.md:798-807` (hard-gates table) | add: "`cargo xtask validate-metadata --check` wall ≤ 100 ms"; "`cargo xtask regen` wall ≤ 5 s on the in-tree 9-grammar set"; "post-relocation, `find crates/bbnf-codegen/src/generated -name "*.rs" \| xargs wc -l \| awk '$1 > 110000'` returns 0" | amendment agent | new rows | 6 |
| 24 | `restart/audit/passes/PASS-A.md:521-523` | add W2 sub-sequencing: "W2.M1 = Lock 2 rename at type level; W2.M2 = `passes/types/` → `passes/layout/` directory rename; W2.M3-M9 = Lock 13 SPLITs against the renamed tree" | amendment agent | paragraph | 2 |
| 25 | `restart/audit/passes/PASS-A.md:324` | qualify `LayoutSink` claim: "Rust emitter implements `LayoutSink` at BA; TS + WASM emitters implement at BD (Pass C carry — see residue §5.7)" | amendment agent | single-line | 3, 8 |

---

## §13 — Final readiness

> **Decision: amendment-required.**
>
> Pass A's substantive findings — the Lock 14 7-site retirement, the
> 13 Lock 13 SPLIT obligations, the Lock 7 path triplet, the Lock 11
> path-dep promotion, the Lock 2 vocabulary rename, the 6 macro
> proposals, the 8 new facilities — survive the lanes. The faults are
> mechanical: 10 sites where Pass A's per-grammar declaration crates
> contradict Amendment 01; the Lock 9 / Lock 11 deferrals lacking
> receiver-blocker-gate triples; the residue ledger's missing
> receiving-gates; the silent friction surfaces (pointer! cookbook,
> lifetime cookbook, layout-lowering errors, Pratt/SIMD misfire,
> migration page); the future-grammar onboarding test absent; the
> per-wave xtask wall + per-grammar generated-LOC invariant gates
> silent. The 25-item punch list is mechanical and fits within a single
> amendment agent's pass.
>
> Hereupon the synthesizer reads PASS-A.md, this hardening report, and
> Amendment 01 together; applies the punch list as an in-place patch
> to PASS-A.md or as a separate `AMENDMENT-02-PASS-A-RECONCILIATION.md`
> per the orchestrator's preference; and proceeds to master-plan V2
> issuance with the reconciled Pass A as substrate. After the master
> plan re-issues, tranche-drafting agents A through J consume the
> reconciled artefacts.
