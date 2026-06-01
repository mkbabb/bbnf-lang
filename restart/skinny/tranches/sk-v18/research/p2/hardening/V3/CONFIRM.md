# SK-V18 S-P2 CHALLENGE — confirm pass (cycle V3)

Fold-confirmation only (not a fresh review). Read target: `SYNTHESIS-RESEARCH.md`.

1. ACCEPT — §intro line 17: `grammar_provider.rs:110-111` is cited as "the JSON-only `first_unsupported` gate" (the live fork dispatch is separately `runtime_generator.rs:16`). Gate is NOT mis-attributed to runtime_generator.rs:110. Present + coherent.
2. ACCEPT — §intro lines 28-30: `_proof_compiles` census names the `…_witness.rs` `EventGrammar` impls in `grammars/json/` + `grammars/sheets_witness/` as "DEFINITIONS but... consumed ONLY by the `_tests.rs`... defined, never animated". Present + coherent.
3. ACCEPT — §1 R-E cell line 42: "the **precedence tower is the SOLE Sheets-distinctive construct**"; Nu8-tagged-alt family "demoted from the litmus" with CSS 295× vs Sheets 21×, named a SHARED construct, not dual-novelty. Present + coherent.
4. ACCEPT — §2 coupling 5 lines 96-101: "Sheets emits THROUGH the un-forked G3 generator (its precedence tower...)"; "`Nu8`-tagged-alt family is a SHARED construct, not part of the litmus". Present + coherent.
5. ACCEPT — §3 G2 Exit lines 169-174: "an EXPLICIT >SOTA-regression gate distinct from parity — `track1_rich/lightningcss >= the S-P1 ratio` on `css_canon_bench` (cold, corpus-in-timer)"; explicitly distinct from the 9-field cssparser oracle CORRECTNESS parity. Present + coherent.
6. ACCEPT — §3 G3 Exit lines 179-184: fourth conjunct **`emit_shape_source == lowered_program`** with falsifier "grep the `render(program)` body for any read of `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract` == 0". Reads no target.* field. Present + coherent.
7. ACCEPT — §3 G5/G6 Exit lines 193-198: "Timed-plane binding (addendum 5): the checkasm differential is a CORRECTNESS gate only; G6 may report only its PASS/FAIL pre-H1, and any Mbps/speedup FIGURE... MUST come from the corpus-in-timer symmetric harness". Present + coherent.
8. ACCEPT — §3 ascii lines 137-140: PROVE and G5/G6 are sibling `├─`/`└─` branches both under G4; PROVE annotated "PARALLEL to G5/G6 (Sheets does not use the CSS NEON)". Branches off G4, parallel to (not nested under) G5/G6. Present + coherent.
9. ACCEPT — §4 R-B bullet lines 230-236: "NEUTRALITY-PROOF obligation (CH6)... the balanced-recognizer SHELL must be PROVEN neutral by at least one NON-CSS invocation... the JSON object/array balanced `{}`/`[]`... OR the Sheets `paren_expr` balancing must invoke the SAME primitive — ELSE it is demoted to an honestly CSS-scoped name (`css_balanced_component_scan`)". Present + coherent.
10. ACCEPT — §4 R-C bullet lines 243-247: "The (b) falsifier for each leaf kernel is specifically the BYTE-SET / numeric-class mutation: widen the `number` rule's digit class in the `.bbnf` → the `b'0'..=b'9'` literal in the emitted kernel widens". Present + coherent.
11. ACCEPT — §4 closing lines 259-265: "(d) **PROFILE-PROVEN-NARROW-LEAF**: the primitive covers a SINGLE hot leaf attributable to a named S-P1-profile hot leaf... and the surrounding structural SKELETON MUST be walk-derived... (d) bounds its SIZE". Skeleton-walk-derived, size-bounding. Present + coherent.

NEW reject-level defect scan: NONE.
- Ordinal systems consistent at both occurrences (§intro lines 13-15 and §2 R16 lines 114-116): `frontend_requirements`/`output_labels` = `RuntimeTarget` #11/#12 = `RuntimeProfileContract` #3/#4. No conflation.
- ASCII vs per-wave entry-gates consistent for G5/G6 (P1∧P3∧G3∧S-P1) and substantively for PROVE. Minor framing variance only: ASCII line 139 lists G4 as a direct conjunct of PROVE's gate while §2/per-wave list (lines 101, 199) list G4 as transitive; both require G4 closed before PROVE and the ASCII child-of-G4 placement is consistent — sub-reject, no contradiction.
- No fabricated mechanism, no addenda violation. All six S-P0 addenda + S-P1 profile-ground-truth references hold; addendum 5 timed-plane binding correctly placed at G6 (not deferred a wave late).

Verdict: all 13 folds present and coherent; no new reject-level defect.

TALLY accept=11 revise=0 reject=0
