---
cycle: V8
lens: CH6 (V8) — ANTI-PAPER-CLOSE
pass: T-P1-TOTALITY-EXCAVATION
reviewer_role: adversarial CHALLENGE
generated_at: 2026-06-01
targets: [1A-substrate-evidence, 1B-codegen-evidence, 1C-runtime-evidence, 1D-skinny-lessons, 1E-locks-evidence, 1F-coherence-scan, 1F-anti-pattern, 1F-past-corpora]
verification_head: dirty tree at master (097c4dd90 + uncommitted generated.rs)
prior_cycle: V7/CH6.md (ACCEPT — accept=6 revise=0 reject=0); V6/CH6.md (ACCEPT — accept=6 revise=0 reject=0)
disposition: ACCEPT
---

# CH6 (V8) — ANTI-PAPER-CLOSE Verdict

## Lens

No inventory may self-report a divergence "resolved/wired" without a live-evidence
citation; no divergence may be deferred to "a later inventory"; every UNKNOWN must
carry a verify_action. Spot-verify the most load-bearing cited path:line rows
against the V1 spec (ARCHITECTURE / MASTER-PLAN / LOCKS) and live code.
PROPORTIONATE: a nit is a REVISE only if it would mislead a T-P2 reader. REJECT
only when an inventory STATES SOMETHING FALSE ON DISK + the live falsifying
path:line proving the inventory wrong. A self-falsified suspicion is an ACCEPT.

## Method

This is the V8 convergence-confirming re-run of the ANTI-PAPER-CLOSE lens — the
THIRD cycle on this lens after two consecutive clean (V6, V7). Did NOT trust the
V6/V7 clean verdicts: read all eight inventories end-to-end and independently
re-grounded the highest-leverage path:line rows on disk and against the V1
LOCKS / ARCHITECTURE / SPEC surfaces. Ran the literal LOCKS:349 self-gate
command at BOTH the narrowed ir+analysis scope AND the full literal 13-crate
scope. Ran a full-corpus closure-word scan (`resolved|wired|closed|fixed|done|
complete`) and a deferral-to-later-inventory scan across all eight files.
Confirmed every UNKNOWN row carries a populated verify_action and every
cross-inventory reference resolves to an EXISTING co-equal sibling row.

## Load-Bearing Citations Spot-Verified (all resolve EXACTLY on disk this cycle)

| claim | inventory | live result |
|---|---|---|
| `ValueRef<'doc,'input:'doc,K=AnyKind,G:EventGrammar=AnyGrammar>` `tape/mod.rs:175`; `_kind` PhantomData `:178`; `_grammar` `:179`; `:197`; `Tape` `:94`; `id()` `:170` | 1A,1C,1D,1E,1F | verbatim match |
| `BackendShape` 5-variant enum `ir/src/lib.rs:340-346` (EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage) | all | exact 5 variants |
| LOCKS:620 "The `G:EventGrammar` type parameter is the generality vehicle" + "config-breadth … 8 of 9 generated grammars … a SEPARATE axis" sentence | 1A,1E,1F | verbatim (amend candidate grounded) |
| ARCHITECTURE §9.2 :1997 "The `G:EventGrammar` type parameter is the generality vehicle" companion prose | 1A | verbatim |
| **LOCKS:349 literal verification command** = `rg ... crates/{ir,parse,codegen,runtime,path,path-core,egraph,csp-solver,parse-that-regex,parse-that,bbnf-simd,analysis,lsp}/src/` "returns ZERO" | 1E,1F | text verbatim at LOCKS:349 |
| **LOCKS:349 self-gate live = 13 sites** at BOTH narrowed ir+analysis scope AND full literal 13-crate scope | 1C,1E,1F | exact 13 both scopes (9 strategy.rs + 2 analysis + 1 grammar_facts + 1 scalar.rs) |
| 9 idents rows `strategy.rs:137,143,149,155,161,167,173,179,185`; consumer `for_grammar_with_manifest(...PRODUCTION_MANIFEST_TABLE)` `:216` | 1E,1F | verbatim, exact 9 |
| `attach_structural_index` NO-OP `json/generated.rs:12-15` (`debug_assert_eq!` + `let _ = state;`); `scan_structurals` `scan.rs:22`; `structural_capacity_for` `:47` | 1A | verbatim |
| CSS config.rs: zero `W7_/BackendShape/substrate_target` (`rg -c` = empty); JSON config triad `:22-30` (`W7_DIRECT_BACKEND_SHAPE/SUBSTRATE_TARGET/RETENTION_LIFETIME/POLICY_OWNER/SAME_SUBSTRATE_UNION`) | 1A | 0 vs full triad |
| @generated markers in `crates/core/src/runtime/` = 67; ARCH §9 :1932 asserts "0/9" + "67 hand-written files" | 1C,1D,1F | exact 67; STALE 0/9 confirmed |
| Pattern-H census: full = 71, per-grammar = 67, +4 = `tape/{mod,cursor,arena,record}.rs` | 1E,1F | exact 71/67/+4 |
| `css_types.rs` 66 LOC, `:1` "Host shims for the CSS L4 grammar's `-> parse_hex_color(...)` map" | 1C,1E,1F | exact 66 LOC, verbatim line 1 |
| `backend_egraph.rs:9` `const REWRITE_SET: &str = "sk-v15-w7-direct-sink-normalization-v1"` | 1B | verbatim |

## ANTI-PAPER-CLOSE Findings (enumerated)

### F1 — Closure-word scan: every occurrence correctly handled, ZERO genuine paper-close (ACCEPT)

A full-corpus scan for `resolved|wired|closed|fixed|done|complete` returned a
handful of hits; each was triaged against the lens contract (a closure word is a
paper-close ONLY when it self-reports a divergence resolved WITHOUT a live citation):

- **1F-past-corpora.md:76** — "RESOLVED" appears in quotes with an explicit
  downgrade: "'RESOLVED' downgraded per CH6-V3-F2, the closure word is not carried
  into the SK-V18 inheritance predicate." Catching a paper-close, not committing one. ACCEPT.
- **1D-skinny-lessons.md:66** — "Phantom `<G>` resolved by DELETE" is the SPEC-CLAIM
  column (SK-V18 goal phrasing); the verdict column reads **unimplemented** with live
  citations `tape/mod.rs:175`,`:179`,`:197` showing `<G>` still present (disk-confirmed:
  `_grammar: PhantomData` IS at `:197`). The "resolved" is planned, the verdict is
  NOT-done. ACCEPT.
- **1A-substrate-evidence.md:108 (1A-DIV-005)** — "Prior mislabel resolved" names the
  REMOVAL of the W7 mislabel (disk-confirmed: `rg -c W7_|BackendShape|substrate_target
  css_l4_declaration_values/config.rs` = empty), and the row immediately states the removal
  OPENED an opposite-direction gap (no `substrate_target` row replaced it). Removal-fact
  plus a freshly-opened gap, NOT a closure. ACCEPT.
- **1A-substrate-evidence.md:90 (1A-SUB-018)** — "template-emitted spec claim is VIOLATED
  … Downgraded from `impl_exceeds_spec` per CH6-F3." Explicitly NOT a closure; an
  opened-gap downgrade with live citation `json/sink.rs:1`/`generated.rs:748`. ACCEPT.
- **1A-substrate-evidence.md:110 (1A-DIV-007)** — "Generated-runtime claim NOT closed."
  A negated closure word naming a standing gap, with live citations. ACCEPT.
- **1B:63** — "One named rewrite set wired" carries the live citation
  `backend_egraph.rs:9` (verbatim-verified) and verdict IMPL_EXCEEDS_SPEC with the gap
  stated (3-pool separation not realized). A cited exceedance, not a paper-close. ACCEPT.
- **1B:53,72** "fail-closed" — W7 validation term, each with a live citation; describes a
  divergence, not a closure. ACCEPT.

No closure word self-reports a divergence resolved without a live citation. ZERO
genuine paper-close.

### F2 — Deferral-to-later-inventory scan: ZERO hits (ACCEPT)

`rg -ni 'deferred to (1[A-F]|a later)|covered (in|later)|addressed in 1[A-F]|punt'`
across all eight files = 0. Every cross-inventory reference is a CITATION to a
CO-EQUAL sibling row that EXISTS and carries a verify_action — each spot-confirmed
this cycle: 1C C12 routes the generator-provenance closure to 1A-UNK-003 (present)
+ 1D U-1 (present); 1A-LOCK1-AMEND-001 cross-links 1F COH18-008 (present);
COH18-015 cross-links 1E:159 (the "New substrate / public substrate API / retained
sidecar: NONE … TOTALITY-TREE CARRY" line, present). The 1A-SUB-018 / 1C-C12
downgrades carry the live falsifying citation, never punted. ACCEPT.

### F3 — Every UNKNOWN carries a verify_action (ACCEPT)

- 1A: 1A-UNK-001..005 — verify_action column populated on all 5 (each carries an `rg`/
  `Inspect`/`Compare` command).
- 1B: U1/U2/U3 — each carries an inline `VERIFY:` clause (`:153`/`:158`/`:163`).
- 1C: U1..U4 — `verify_action:` prose on each (4/4, spot-read).
- 1D: U-1..U-5 — `**verify_action:**` prose on each (5/5).
- 1E: 1E-V5-U1..U3 — verify_action column populated (T-P3 disposition + a concrete
  re-run/re-grep command on each).
- 1F-coherence: U-COH18-001/002 — verify_action column populated with concrete
  `rg`/`Confirm` commands.
No bare UNKNOWN exists in any inventory. ACCEPT.

### F4 — LOCKS:349 self-gate falsification is REAL, exactly stated, admissibly caught (ACCEPT)

1E D-1E-V5-14 / 1F COH18-012 assert Lock 14's OWN verification command "returns ZERO"
but live returns 13 sites. This cycle I read the LITERAL command text at LOCKS:349
and ran it at the FULL literal 13-crate scope — it returns exactly 13, IDENTICAL to
the narrowed ir+analysis scope (9 `strategy.rs` idents rows + 2 `analysis`
doc-comments + 1 `grammar_facts.rs` + 1 `shape_dispatch/scalar.rs`). The narrow
4-name regex catches exactly the 4 idents rows at `:137,143,149,155`; the other 5
idents rows (`:161,167,173,179,185`) + the analysis/recognizers siblings make up the
13. This is an inventory correctly catching a FALSE assertion in the V1 spec (a
self-gate that is RED) — under the corrected REJECT convention this is an ACCEPT of
the inventory (it states the truth, the spec is wrong), NOT an inventory error. ACCEPT.

### F5 — Amendment candidates grounded in live spec text (ACCEPT)

1A-LOCK1-AMEND-001 / 1E LAC-1E-V5-01..07 / 1F COH18-008 rest on LOCKS:620 reading
verbatim "The `G:EventGrammar` type parameter is the generality vehicle" (disk-confirmed
at `:620`, full clause read this cycle including the "config-breadth … 8 of 9 generated
grammars … a SEPARATE axis" sentence the amendment re-anchors onto) and the certified
SK-V18 DELETE of `<G>` (disk shows `<G>` still present, test-only at `tape/mod.rs:175`).
Both poles verified; the contradiction is real, the candidates are disposition-routed to
T-P3 / SK-V19 / Pass Omega (never amended in-pass). LAC-1E-V5-02's
`runtime_target_rows_collapsed` is correctly flagged a PLANNED-not-live symbol. ACCEPT.

### F6 — The 1C-C12 provenance-fallacy self-catch is the strongest anti-paper-close row (ACCEPT)

1C C12 would naturally read IMPLEMENTED / IMPL-EXCEEDS-SPEC (9 grammars "genuinely
generated"); instead it is DOWNGRADED to `partial / provenance UNKNOWN` precisely
because the only live witness is the `@generated`/`AUTO-GENERATED` header comment (no
regen round-trip, no manifest, no byte-equivalence proof), and the row credits that a
closure verdict resting on the header alone is the EXACT provenance fallacy C2/D4 reject
("markers do not equal generator output"). The round-trip closure is routed OPEN to
1A-UNK-003 + 1D U-1 (both present). This is the lens working as designed: a near-closure
actively retired, the gap kept open and routed to a co-equal verify_action. ACCEPT.

### F7 — Prior-cycle re-anchor corrections re-verify clean (ACCEPT)

Spot-checked the error-prone re-anchors: `Tape::id` `:170` — disk-exact; `_grammar:
PhantomData` `:197` — disk-exact; `attach_structural_index` NO-OP `:12-15` with `let _ =
state;` — disk-exact; CSS config zero-W7 vs JSON config triad — disk-exact;
`REWRITE_SET` `:9` — disk-exact; Pattern-H 71/67/+4 — disk-exact; @generated = 67 with
ARCH §9 STALE 0/9 — disk-exact; LOCKS:349 13-site self-gate — disk-exact both scopes.
The 8th-cycle citation discipline is at fixed point.

## Disposition

This is an 8th-cycle inventory set hardened through 7 prior cycles (5 consolidated +
V6 + V7). Under the ANTI-PAPER-CLOSE lens, the inventories are SOUND: every divergence
claiming a removal/fact carries a live citation; no divergence is deferred to a later
inventory (only co-equal cross-references, every target row present with a verify_action,
all spot-confirmed); every UNKNOWN carries a populated verify_action; every load-bearing
path:line resolves exactly on disk and against the V1 LOCKS / ARCHITECTURE / SPEC
surfaces; the one genuinely FALSE on-disk statement surfaced (the LOCKS:349 self-gate
asserting "returns ZERO" while the literal 13-crate command returns 13) is correctly
caught BY the inventories as an admissible spec-defect divergence, not committed by them.

No residual REVISE rises to the "would mislead a T-P2 reader" bar — every closure-word
occurrence is explicitly quoted/downgraded/gap-opened/cited in situ, which is exactly
what a careful T-P2 reader needs. No GENUINE reject: no inventory states anything false
on disk (per the corrected convention, catching the spec's own false self-gate is an
ACCEPT of the inventory, not a reject). This lens reaches a clean fixed point for V8 —
the THIRD consecutive clean cycle, exceeding the 2-consecutive-clean target (V6, V7, V8).

TALLY accept=6 revise=0 reject=0
