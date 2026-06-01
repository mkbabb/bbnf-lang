---
cycle: V7
lens: CH6 (V7) — ANTI-PAPER-CLOSE
pass: T-P1-TOTALITY-EXCAVATION
reviewer_role: adversarial CHALLENGE
generated_at: 2026-06-01
targets: [1A-substrate-evidence, 1B-codegen-evidence, 1C-runtime-evidence, 1D-skinny-lessons, 1E-locks-evidence, 1F-coherence-scan, 1F-anti-pattern, 1F-past-corpora]
verification_head: dirty tree at master (3ac131c45 + uncommitted generated.rs)
prior_cycle: V6/CH6.md (ACCEPT — accept=6 revise=0 reject=0)
disposition: ACCEPT
---

# CH6 (V7) — ANTI-PAPER-CLOSE Verdict

## Lens

No inventory may self-report a divergence "resolved/wired" without a live-evidence
citation; no divergence may be deferred to "a later inventory"; every UNKNOWN must
carry a verify_action. Spot-verify the most load-bearing cited path:line rows
against the V1 spec (ARCHITECTURE / MASTER-PLAN / LOCKS) and live code. PROPORTIONATE:
a nit is a REVISE only if it would mislead a T-P2 reader. REJECT only when an
inventory STATES SOMETHING FALSE ON DISK + the live falsifying path:line.

## Method

This is the V7 convergence-seeking re-run of the ANTI-PAPER-CLOSE lens, targeting the
second of two consecutive clean cycles. Read all eight inventories end-to-end. Did
NOT trust the V6/CH6 clean verdict — independently re-grounded 40+ of the highest-
leverage path:line rows on disk and against the V1 LOCKS / ARCHITECTURE / SPEC
surfaces. Ran the literal LOCKS:349 self-gate command at BOTH the narrowed ir+analysis
scope AND the full literal 13-crate scope. Scanned all eight files for paper-close
closure words and deferral-to-later-inventory patterns. Confirmed every UNKNOWN row
carries a populated verify_action column.

## Load-Bearing Citations Spot-Verified (all resolve EXACTLY on disk this cycle)

| claim | inventory | live result |
|---|---|---|
| `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>` `tape/mod.rs:175`; `_kind` PhantomData `:178`; `_grammar` `:179`; `Tape` `:94`; `flag_values` `:98`; `id()` `:170` | 1A,1C,1D,1E,1F | verbatim match |
| tape modules = `assembler/event_grammar/event_grammar_tests/mod/offsets` (no token/builder/span/payload/view/trace) | 1A,1C | exact 5, no spec-named extras |
| `BackendShape` 5-variant enum `ir/src/lib.rs:340-346` | all | exact 5 variants |
| `ExprKind` 8 variants (Seq/Alt/Repeat/Optional/Literal/Regex/Ref/Annotation), no Predicate/Lookbehind/Call/Layout/Error | 1A | exact 8, no extras |
| `select_lowering` 5-arm `match cost.chosen` `lower/mod.rs:18-26`, zero grammar names | 1B,1D,1E | exact 5 arms |
| `RuntimeEmitterKind{CompiledLowering,RequestFacts}` `grammar_provider.rs:40-42`; `emitter` `:33`; `:110` CSS-exempt | 1B,1C,1D,1E,1F | verbatim |
| `runtime_generator.rs:16-26` match fork; `:91` `normalize(CSS_GENERATED_RS)`; `:701` const open / `:1611` close | 1B,1C,1D,1E,1F | verbatim |
| **LOCKS:349 literal verification command = `rg ... crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/` "returns ZERO"** | 1E,1F | text verbatim at LOCKS:349 |
| **LOCKS:349 self-gate live = 13 sites (11 ir + 2 analysis) at BOTH the narrowed ir+analysis scope AND the full literal 13-crate scope** | 1C,1E,1F | exact 13/11/2 both scopes |
| 9 idents rows `strategy.rs:137,143,149,155,161,167,173,179,185`; narrow 4-name regex catches exactly `:137,143,149,155` (+ `:315` doc-comment), 5 escape; consumer `for_grammar_with_manifest(...PRODUCTION_MANIFEST_TABLE)` `:216` | 1E,1F | verbatim |
| LOCKS:620 "The `G:EventGrammar` type parameter is the generality vehicle" | 1A,1E,1F | verbatim (amend candidate grounded) |
| CSS config.rs: zero `W7_/BackendShape/substrate_target`; JSON config triad `:22-30` (`SinkOnly`/`direct_sink`/`generated_function`) | 1A | 0 vs full triad |
| 7 css_l4 `generated.rs` md5 `b654562ccff46ed62dd48e9ace325830` ×7 | 1C,1D,1F | identical |
| `parse_w11_1_number` count in json `generated.rs` | 1C,1D,1E | exactly 7 |
| `css_cold_harness.rs` `track1_full` at `:131`, `// ---- track1_full` comment at `:130` | 1D,1F-anti | verbatim |
| css_types.rs 66 LOC, `:1` host-shim line; builder.rs 817 LOC; sheets_witness 24+1 LOC; x86 28 files; simd-scan index.rs+lib.rs 217 LOC | 1C,1D,1E,1F | all exact |
| 8-of-9 OnceCell breadth: `ensure_structural_index` = math 0; all 8 others 2-3 (google_sheets 3) | 1E,1F-anti | exact |
| OnceCell probe `json.rs:701`; emitter "The probe substrate (OnceCell + helper)" `support.rs:67`; `simd-scan/src/lib.rs:68` exports `{StructuralIndex, next_structural_at_or_after}`; skinny `next_structural_at_or_after` = 0 | 1E,1F | verbatim |
| `runtime_target_rows_collapsed` = 0 live in skinny code; SPEC:247 carries it as a PLANNED bool gate | 1B,1E,1F | exact (planned-only) |
| `regen.rs:5` `#[derive(Clone, Copy, Debug)]` over `pub(crate) struct RuntimeTarget` `:6` | 1B,1D,1F-past | verbatim |
| DirectParser `cursor: usize` field `:671`, struct text `:668`; `measure_mbps` resolves to `src/nonjson_css_l4.rs:3091` not the 318-LOC `benches/` sibling | 1A,1D,1F | verbatim |
| 1B D2 loc_delta provenance: prior `+400..+1200` figure grep of SPEC = 0 (correctly NO source); SPEC:440 ≤450 G3 band / ARCH:1280-1282 intrinsic-blocked band both ground | 1B,1C | exact |
| 1C D8 re-key: SPEC:443 PROVE-wave Sheets row, SPEC:440 G3 band | 1C | exact line anchors |

## ANTI-PAPER-CLOSE Findings (enumerated)

### F1 — Closure-word scan: all occurrences correctly handled, ZERO genuine paper-close (ACCEPT)

A full-corpus scan for `resolved|wired|closed|fixed|done|complete` returned a
handful of hits; each was triaged against the lens contract (a closure word is a
paper-close ONLY when it self-reports a divergence resolved WITHOUT a live citation):

- **1F-past-corpora.md:76** — "RESOLVED" appears in quotes with an explicit downgrade:
  "'RESOLVED' downgraded per CH6-V3-F2, the closure word is not carried into the SK-V18
  inheritance predicate." Catching a paper-close, not committing one. ACCEPT.
- **1D-skinny-lessons.md:66** — "Phantom `<G>` resolved by DELETE" is the SPEC-CLAIM
  column (SK-V18 goal phrasing); the verdict column reads **unimplemented** with live
  citations `tape/mod.rs:175`,`:179`,`:197` showing `<G>` still present (disk-confirmed:
  `_grammar: PhantomData` IS at `:197`). The "resolved" is planned, the verdict is
  NOT-done. ACCEPT.
- **1A-substrate-evidence.md:108 (1A-DIV-005)** — "Prior mislabel resolved" names the
  REMOVAL of the W7 mislabel (disk-confirmed: `rg W7_|BackendShape|substrate_target
  css_l4_declaration_values/config.rs` = 0), and the row immediately states the removal
  OPENED an opposite-direction gap (no `substrate_target` row replaced it). Removal-fact
  plus a freshly-opened gap, NOT a closure. ACCEPT.
- **1A-substrate-evidence.md:90 (1A-SUB-018)** — "template-emitted spec claim is VIOLATED
  ... Downgraded from `impl_exceeds_spec` per CH6-F3." Explicitly NOT a closure; an
  opened-gap downgrade with live citation `json/sink.rs:1`/`generated.rs:748`. ACCEPT.
- **1B:63** — "One named rewrite set wired" carries the live citation
  `backend_egraph.rs:9` and the verdict IMPL_EXCEEDS_SPEC with the gap stated (3-pool
  separation not realized). A cited exceedance, not a paper-close. ACCEPT.
- **1B:53** "fail-closed" / **1B:38,59,93,100,137; 1F-anti:62; 1F-past:47** "fixed-literal"
  — technical terms (W7 validation / a body that literally IS a fixed literal), each with
  a live citation; describe divergences, not closures. ACCEPT.
- **1F-coherence:104** "crate attribution fixed per CH5-V4-010" — a self-correction of the
  inventory's OWN prior attribution (the OnceCell lives in the `crates/core` CONSUMER, not
  `simd-scan`), accompanied by the live falsifier `simd-scan/src/lib.rs:68`. A correction
  with evidence, not a divergence-resolved-without-citation. ACCEPT.

No closure word self-reports a divergence resolved without a live citation. ZERO
genuine paper-close.

### F2 — Deferral-to-later-inventory scan: ZERO hits (ACCEPT)

No "covered later / deferred to 1X / addressed in 1X" patterns (live grep = 0). Every
cross-inventory reference is a CITATION to a CO-EQUAL sibling row that EXISTS and carries
a verify_action (e.g. 1C C12 routes the generator-provenance closure to 1A-UNK-003 +
1D U-1, both present; 1E D-1E-V5-14 cross-links 1F COH18-005 + 1C D9; COH18-015 cross-
links 1E:159 + 1F-anti OnceCell row). The 1A-SUB-018 / C12 downgrades carry the live
falsifying citation, never punted. ACCEPT.

### F3 — Every UNKNOWN carries a verify_action (ACCEPT)

- 1A: UNK-001..005 — verify_action column populated on all 5.
- 1B: U1/U2/U3 — each carries an inline `VERIFY:` clause.
- 1C: U1..U4 — verify_action prose on each.
- 1D: U-1..U-5 — `**verify_action:**` prose on each.
- 1E: 1E-V5-U1..U3 — verify_action column populated (T-P3 disposition + re-run/re-grep
  command on each).
- 1F-coherence: U-COH18-001/002 — verify_action column populated (the early `rg -c`
  undercount was a token-phrasing artefact; both rows carry a populated 3rd column with a
  concrete `rg`/`Confirm` command).
No bare UNKNOWN exists in any inventory. ACCEPT.

### F4 — LOCKS:349 self-gate falsification is REAL, exactly stated, and admissibly caught (ACCEPT)

1E D-1E-V5-14 / 1F COH18-012 assert Lock 14's OWN verification command "returns ZERO"
but live returns 13 sites. This cycle I read the LITERAL command text at LOCKS:349
(`rg ... crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,
parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/` "returns ZERO") and ran it at
the FULL literal 13-crate scope — it returns exactly 13 (11 ir + 2 analysis; the other
named crates either are absent under `crates/` or add zero hits), IDENTICAL to the
narrowed ir+analysis scope. The narrow 4-name regex catches exactly the 4 idents rows at
`:137,143,149,155` (+ a `:315` doc-comment), and the other 5 idents rows + 2 analysis
doc-comments + the `ir` recognizers siblings make up the 13. This is an inventory
correctly catching a FALSE assertion in the V1 spec (a self-gate that is RED), which is
an admissible divergence finding — under the corrected REJECT convention this is an
ACCEPT of the inventory, NOT an inventory error. ACCEPT.

### F5 — Amendment candidates grounded in live spec text (ACCEPT)

1A-LOCK1-AMEND-001 / 1E LAC-1E-V5-01..07 / 1F COH18-008 rest on LOCKS:620 reading
verbatim "The `G:EventGrammar` type parameter is the generality vehicle" (disk-confirmed
at `:620`, full clause read this cycle including the "config-breadth ... 8 of 9 generated
grammars ... a SEPARATE axis" sentence the amendment re-anchors onto) and the certified
SK-V18 DELETE of `<G>` (SPEC:99-102 lineage; disk shows `<G>` still present, test-only).
Both poles verified; the contradiction is real, the candidates are disposition-routed to
T-P3 / SK-V19 / Pass Omega (never amended in-pass). LAC-1E-V5-02's `runtime_target_rows_
collapsed` is correctly flagged a PLANNED-not-live symbol (live grep = 0; SPEC:247 carries
it as a planned bool gate). ACCEPT.

### F6 — Prior-cycle re-anchor corrections re-verify clean (ACCEPT)

Spot-checked the error-prone re-anchors from V1-V6: track1_full `:131` (not `:130`
comment) — correct; DirectParser cursor field `:671`, struct text `:668` — correct;
`measure_mbps` resolves to `src/nonjson_css_l4.rs:3091` not the 318-LOC `benches/`
sibling (confirmed `benches/nonjson_css_l4.rs` = 318 LOC) — consistent; REDRESS item-51
`742-768` / item-53 `784-813` with item-52 `769-783` carve-out — internally consistent
across 1B/1D/1E; 8-of-9 OnceCell breadth (math=0, google_sheets=3) — disk-exact;
Tape::id `:170` — disk-exact; `_grammar: PhantomData` `:197` — disk-exact; regen.rs:5
derive over `:6` struct — disk-exact; 1B D2 prior-figure provenance (SPEC grep = 0,
two-way band re-source) — disk-exact. The 7th-cycle citation discipline is at fixed
point.

## Disposition

This is a 7th-cycle inventory set hardened through 6 prior cycles (5 consolidated +
V6). Under the ANTI-PAPER-CLOSE lens, the inventories are SOUND: every divergence
claiming a removal/fact carries a live citation; no divergence is deferred to a later
inventory (only co-equal cross-references, every target row present with a verify_action);
every UNKNOWN carries a populated verify_action; every load-bearing path:line resolves
exactly on disk and against the V1 LOCKS / ARCHITECTURE / SPEC surfaces; the one
genuinely FALSE on-disk statement surfaced (the LOCKS:349 self-gate asserting "returns
ZERO" while the literal 13-crate command returns 13) is correctly caught BY the
inventories as an admissible spec-defect divergence, not committed by them.

No residual REVISE rises to the "would mislead a T-P2 reader" bar — every closure-word
occurrence is explicitly quoted/downgraded/gap-opened/cited in situ, which is exactly
what a careful T-P2 reader needs. No GENUINE reject: no inventory states anything false
on disk (per the corrected convention, catching the spec's own false self-gate is an
ACCEPT of the inventory, not a reject). This lens reaches a clean fixed point for V7 —
the SECOND consecutive clean cycle, matching V6/CH6.

TALLY accept=6 revise=0 reject=0
