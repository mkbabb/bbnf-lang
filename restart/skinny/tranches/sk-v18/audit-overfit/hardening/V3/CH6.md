# S-P0 audit-overfit hardening V3 — CH6 ANTI-PAPER-CLOSE (independent re-grep, 2nd confirm)

CH6 lens = ANTI-PAPER-CLOSE. Subject: the SK-V18 S-P0 audit-overfit artefacts
(`a0`/`a1`/`a2`/`a3` + `SYNTHESIS-AUDIT-OVERFIT.md`). Mandate (PASS-0-OVERFIT-AUDIT §3 +
ORCHESTRATOR §3W): verify (1) the residual-overfit audit is COMPLETE, (2) the 6 addenda are
EXECUTABLE + correctly catch the V3 failure modes, (3) the PRUNE-sequencing is SOUND — disposition
each ACCEPT/REVISE/REJECT with path:line.

CH6's specific charge is paper-close: a gate closed with PROSE rather than SUBSTANCE; a relocated
seam a regex cannot fire on; a decision DEFERRED onto a RED-by-design gate; an escape hatch that
re-admits the very overfit being pruned under a relabel. I re-grepped every dispositive witness
INDEPENDENTLY at live HEAD `83b66db42` (confirmed `git rev-parse` = `83b66db42`, the commit each
artefact cites) — I do not inherit the prior cycles' greps. All seven dispositions below are
disk-grounded, not artefact-trusting.

## §A — Disk re-verification (CH6 does not paper-close its own review)

| Witness | Artefact claim | Disk verdict (`83b66db42`) |
|---|---|---|
| `CSS_GENERATED_RS` verbatim `&str` const | `runtime_generator.rs:701`, body `:701`→`:1611` = 910 LOC | CONFIRMED — `const CSS_GENERATED_RS: &str = r#"` at `:701`; closing `"#;` at `:1611` |
| const `_RS` courier count in codegen | 8 (a1 §L1) | CONFIRMED — `rg 'const \w*_RS\s*:\s*&str\s*=\s*r#"' codegen/src` = 8 |
| 7 css_l4 `generated.rs` byte-identical | md5 `b654562c…` ×7 | CONFIRMED — all 7 share `b654562ccff46ed62dd48e9ace325830` |
| `RuntimeEmitterKind` fork | `grammar_provider.rs:40-42`, dispatched `:110` | CONFIRMED — `enum RuntimeEmitterKind {CompiledLowering, RequestFacts}`; also field-typed in `regen.rs:15` (xtask-referenced, L3 residual real) |
| phantom `<G>` | `tape/mod.rs:175` | CONFIRMED — `ValueRef<…K = AnyKind, G: EventGrammar = AnyGrammar>` |
| L5 warm path live; canon kept | `nonjson_css_l4.rs:3091` + `css_canon_bench.rs` present | CONFIRMED — `measure_mbps` at `:3091`; `css_canon_bench.rs` present |
| L6 CSS NEON dead at admission | `find_css_significant` only `lib.rs:574` cfg(test); 0 in generated | CONFIRMED — sole caller `lib.rs:574` inside the `#[cfg(test)]` opened `:51`; 0 hits in `grammars/*/generated.rs`; only cold `count_top_level_commas` at `generated.rs:157/:809` |
| R16 nested-struct recipe | `RuntimeTarget` 12 fields, derives `Clone,Copy,Debug` only; both nests `PartialEq,Eq` | CONFIRMED — `regen.rs:5` `#[derive(Clone, Copy, Debug)]`; 12 fields; `frontend_requirements`(#11)+`output_labels`(#12) nested; `grammar_provider.rs:45/:91` both `PartialEq, Eq` |
| R-A0-2 collapse-to-one disk evidence | one `stylesheet.bbnf`, 7 profiles are metadata | CONFIRMED — `regen_css.rs:24` `CSS_L4_ROOTS = &["grammar/css/l4/stylesheet.bbnf"]`; `entry_rule:"stylesheet"` ×7; 7 DISTINCT `fact_schema` `:49…:157` riding nested `output_labels` |
| P1 x86 / P5 leak | 24+4 files, nasm-rs:19, 9 checkasm sites; leak ×7 | CONFIRMED — 24 `src/x86_64/` + 4 `ext/x86/`; `nasm-rs="0.3"` `Cargo.toml:19`; 9 `bbnf_simd::x86_64::` sites in `checkasm_parity.rs`; `parse_w11_1_number` ×7 in `json/generated.rs` |

Every dispositive witness verifies. No artefact claim is overstated against disk; no gate keys on a
descriptive figure (the 910-LOC body is descriptive, the binding gate is `verbatim_blob_present==false`
+ the `.bbnf`-mutation test — `a1:88`). The R-A0-2 disk grounding is the strongest single check
against paper-close: the 7 css_l4 rows demonstrably derive from ONE `stylesheet.bbnf` (`regen_css.rs:24`)
with one `entry_rule`, so "collapse-to-one" is disk-true and "differentiate into 7 roots" would be the
manufacture-overfit the addendum forbids. The artefacts carry this answer UP, not deferred.

## §B — The seven CH6 dispositions

**S1 — R-A0-1 "beats CSSOM" qualifier is a REJECT clause, not a preference (ACCEPT).** The lazy-vs-eager
asymmetry is the canonical paper-close seam: a real number (cold, N≥200, real-corpus `css_canon_bench`)
that smuggles asymmetric work behind an OR that lets the cheaper re-label branch close the honesty gate
with no symmetric comparator. The artefacts close the seam STRUCTURALLY: `SYNTHESIS:101` (R-A0-1 row) +
`a0:334-339` (§4 binding item 1) bind "an unqualified 'beats CSSOM'/'equal-work' close-report claim
behind a re-label is a **REJECT**, per a0 §4" — explicitly "a REJECT clause, not a preference." The OR
survives (the disclosure branch is honest IF stated); the dishonesty (bare re-label + unqualified ">beats")
does not. ACCEPT — `SYNTHESIS:101`, `a0:334-339`, `a1:430-431`.

**S2 — R-A0-2 collapse-to-one is carried UP, not deferred onto a RED gate (ACCEPT).** A deferred
decision on a RED-by-design gate is the second classic paper-close (S-P3 could bind "differentiate" and
manufacture 7 fake `.bbnf` roots to false-green a distinctness gate). The artefacts foreclose it with the
disk answer, not a deferral: `SYNTHESIS:102` carries "DISK EVIDENCE is collapse-to-one (one
`stylesheet.bbnf`, byte-identical output — `generator_grammar_count == 3` = json+css+sheets, NOT
json+7-css+sheets); manufacturing 7 fake `.bbnf` roots … is the EXACT overfit the addendum forbids";
`a0:388-399` (§5 item 1) states the same disk-grounded answer. I independently confirmed the disk
evidence (`regen_css.rs:24` one root, 7 distinct `fact_schema`) — the answer is TRUE, and binding it
up removes the seam where S-P3 re-derives and possibly picks the wrong branch. ACCEPT — `SYNTHESIS:102`,
`a0:388-399`, `a2:376-380`.

**S3 — R-A0-3 honest-finding escape is machine-checked, closing the largest prose seam (ACCEPT).** The
contract's OWN admission of its largest paper-close surface (HANDOFF §6 "named primitive" escape) is the
sharpest test: a verbatim blob re-entering under a "primitive" label past a PROSE-reviewed gate is SK-V13's
exact failure mode at the escape hatch. The artefacts machine-check the (a)-(c) gate: `SYNTHESIS:103`
binds "(a) grammar-invoked + (b) grammar-derived-data + (c) `verbatim_blob_present==false` machine-checked";
`a0:429-441` (§6) + `a1:134-149` restate (b) as a per-primitive MACHINE mutate-falsifier — the primitive's
EMITTED OUTPUT must vary under a `.bbnf` mutation, NOT merely "accepts a grammar-derived argument" (a fixed
body keyed off a decorative argument fails (b) exactly as a const courier fails the whole-path test). This
converts the one previously-prose predicate into a machine check — the decisive anti-paper-close fold.
ACCEPT — `SYNTHESIS:103`, `a0:429-441`, `a1:134-149`.

**S4 — the relocated-overfit-seam is closed at the FULL-EXPANDED-ROW altitude, both nested structs (ACCEPT).**
The deepest paper-close risk in the whole goalset is the relocated seam: a per-grammar branch moved out of a
`Json =>` match arm into a neutral-identifier `RuntimeTarget` data-table, which the arm-census regex is
SYNTACTICALLY INCAPABLE of firing on. The artefacts catch it structurally, not by regex: addendum 2 is a
3-co-gate conjunction with `runtime_target_rows_collapsed` (`SYNTHESIS:59-63`, `a1:190-194`). The R1-CH5
fold (R16) is the load-bearing sharpening — the recipe must inline BOTH nested structs
(`frontend_requirements` #11 AND `output_labels` #12), not just the prose's 3 named pseudo-fields, or a
future seam riding `frontend_requirements` slips a one-nested-struct recipe (the shallow-compare false-green
displaced one field over). `SYNTHESIS:211-221` + `a3:117-214` + `a2:382-400` bind a full-row
`RuntimeTarget: PartialEq` collapse (covers both nests automatically, cannot couple to a hand-rolled list).
I confirmed the cost is real and stated: `regen.rs:5` derives only `Clone, Copy, Debug`, both nests already
`PartialEq, Eq` (`grammar_provider.rs:45/:91`) — the one-line derive is viable and is named as the pin's
cost. The seam is closed at the invariant's altitude, mechanism-agnostic. ACCEPT — `SYNTHESIS:211-221`,
`a3:175-214`, `a2:382-400`.

**S5 — the 6 addenda are executable + V3-catching; none decorative (ACCEPT).** Every addendum fires on a
live surface I re-grepped: L1 `CSS_GENERATED_RS:701` + 8 couriers; L2 7× `b654562c…`; L3
`RuntimeEmitterKind:40-42` (the neutral-named fork the L2 arm-census CANNOT catch — exactly why L3 is a
distinct lens, `a1:234-238`); L4 `ValueRef<G=AnyGrammar>:175` (G axis, NOT the real K axis, with
test-exclusion + `json_rich_navigation_preserved` so a ≥2 impl-count cannot LCD-flatten, `a1:336-354`);
L5 `measure_mbps:3091` warm micro-fixture (the triple-violation: warm + micro-fixture + more-work
competitor); L6 `find_css_significant` cfg(test)-only with the retire branch gated on a samply MEASUREMENT
not an assertion (`a1:506-515`). Each is bound THREE ways (close-gate + §0.4 pre-block + telemetry the
gate-json consumer REJECTs on, `a0:56`). None is decorative — each catches a V3-found failure mode STILL
LIVE at HEAD. ACCEPT — `a1:99-528`, `SYNTHESIS:48-71`.

**S6 — the PRUNE-sequencing is sound; PRUNE-before-GENERALIZE-before-PROVE binds with no march-over (ACCEPT).**
The sequencing is the structural defense against admitting a generalize wave on top of an un-pruned
contrivance. The entry-gate chain `PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1` (`SYNTHESIS:186-195`)
is binding with the load-bearing edges grounded: P4-before-G2/G3 (the Lock-14 gate must be meaningful AS
the un-forked emitter is built, `SYNTHESIS:204`, `a2:235-239`); the P1↔`checkasm_parity.rs` build-soundness
coupling (the 9 active `bbnf_simd::x86_64::` sites I confirmed BREAK the build if `src/x86_64/` is deleted
without decoupling — so P1's exit gate is `cargo test --no-run` clean in the SAME wave, `a2:254-294`); G2's
DUAL entry-gate on BOTH G1 AND P3 (`SYNTHESIS:207`, the R1-CH3 directional fold); G3-failure-blocks-PROVE
stated as a FORWARD arrow, never a backward "G3 gates G1/G2" (`a2:298-307`). The directional-arrow fold is
the anti-paper-close correction (a loosely-inverted arrow would have let a downstream reader mis-sequence).
ACCEPT — `SYNTHESIS:181-224`, `a2:215-408`.

**S7 — the residual census is COMPLETE; no silent wave-past; the honest-finding §6 path + SK-V19 tee-up
carried (ACCEPT).** Completeness is the CH6 charge: every live overfit surface mapped to a named wave +
machine gate, with no surface waved past silently. The R1–R16 + R-A0-1/2/3 census (`SYNTHESIS:83-107`) maps
each residual to a disposition and a telemetry column; the per-axis A1–A6 verdict (`SYNTHESIS:134-148`)
reconciles to PASS-0's axis set; the CLEAN/KEEP inventory (`SYNTHESIS:109-113`) prevents throwing the
aarch64 hardening out with the x86 bathwater. The ONE NEW finding (R16, MEDIUM, the gate-recipe precision
pin) is the audit EARNING its keep beyond confirming the goalset — a precision hazard the Alpha CHALLENGE
did not fully surface, pinned to S-P3 (`a3:248-252`). Crucially for CH6: the honest-finding §6 path is
carried as a GENUINE outcome (a real "generator cannot lower Pratt" finding becomes a pluggable
`.bbnf`-invoked primitive, gated (a)-(c)), NEVER a silent blob (`SYNTHESIS:276-282`), and PROVE-Sheets
emits THROUGH the un-forked generator only (do-not-stub-prove). No paper-close seam survives.
ACCEPT — `SYNTHESIS:72-282`, `a3:248-252`.

## §C — Verdict

The audit converged the three CH6 framing-completion seams STRUCTURALLY rather than rhetorically: the
"beats" qualifier is a REJECT clause (S1), the collapse-to-one answer is disk-grounded and carried up
(S2), the honest-finding escape is machine-checked (S3), the relocated seam is closed at the
full-expanded-row altitude inlining BOTH nested structs (S4). The 6 addenda are executable and each fires
on a live V3-found surface (S5); the PRUNE-sequencing binds with no march-over and the directional arrows
are correct (S6); the census is complete with the NEW R16 earning the pass and the §6 honest-finding path
carried as a genuine-not-silent outcome (S7). I re-grepped every witness independently at HEAD `83b66db42`
and found no overstated claim, no gate keyed on a descriptive figure, no deferred decision on a RED gate,
and no escape hatch re-admitting a relabeled blob. No new anti-paper-close defect.

## Tally
ACCEPT 7 · REVISE 0 · REJECT 0 — **100%**.

TALLY accept=7 revise=0 reject=0
