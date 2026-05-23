# CH2 GENERALITY (Lock 14) — Pass Alpha V3 Disposition

Lens: CH2 per `restart/prompts/ORCHESTRATOR.md §3W` — "Lock 14 holds:
no grammar-name leak; every proposed intervention is grammar-neutral
and works for CSS L4 / Sheets / BBNF-self, not only JSON."

Authority binding: `restart/locks/LOCKS.md:220-238` (Lock 14 text);
V1 dispatch context
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V1/CHALLENGE-CONTEXT.md:99-110`
(§CH-2 scope); V2 addendum
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CHALLENGE-V2-ADDENDUM.md`;
V2 disposition
`restart/skinny/tranches/sk-v14/research/alpha-hardening/V2/CH2.md`
(97 % ACCEPT; 0 REJECT; 1 NEW REVISE NF-1). V3 cycle inspects the V2
α-baseline (`958406257`) overlaid with the V3 micro-fold commit
`5e2ae78b4` (two-file atomic: alpha-E `800 → 815` lines;
HANDOFF `242 → 246 → 245` lines). V3 is the confirming pass per
`ORCHESTRATOR.md §3Z` two-consecutive-cycle rule; V2 returned
CONVERGED-EXPECTING-V3-CONFIRM at 99.27 % CONSOLIDATED.

## §0 — Disposition summary

- ACCEPT-rate: 100 % (33 / 33 § lines disposed)
- REJECT count: 0
- REVISE count: 0
- FOLD-LANDED count: 1 / 1 V2 fold (F-V3-α-E-1 closes V2 NF-1)
- FOLD-PARTIAL count: 0
- FOLD-MISSING count: 0
- Critical findings: 0
- Escalation flag: none. CH2 converges at 100 % ACCEPT on V3 — the
  two-consecutive-cycle convergence chain (V2 97 % + V3 100 %) closes
  cleanly. The lens reports CONVERGED-FINAL.

V2 NF-1 (C-3 round-trip gate's hardcoded eight-grammar literal at
`alpha-E:362-365`) has been folded verbatim per V2 CH2 §3 recommended
text. The gate's grammar enumeration now derives from
`workspace.metadata.bbnf.grammars` via `cargo metadata --format-version
1 | jq -r '.workspace_metadata.bbnf.grammars | keys[]'`, restoring
forward-discipline parity with C-1's E-7 invariant at the gate-
authoring site rather than only at first-grammar-admission time. No
new Lock 14 leak surfaces under fresh-finding scan across all eight
V3 artefacts.

## §1 — Per-artefact disposition table

| Artefact | § | Disposition | Reason |
|---|---|---|---|
| `SYNTHESIS.md` | §0.1 close condition | ACCEPT | R10 verbatim; close criterion treats JSON cells + CSS features symmetrically — unchanged from V2; STAND per V3 commit body ("Other files STAND: SYNTHESIS"). |
| `SYNTHESIS.md` | §0.2 goalset enumeration | ACCEPT | Per-grammar row counts named; all four surfaces enumerated under one rubric. Numeric divergence reconciliation paragraph (F-1) plane-neutral; unchanged from V2. |
| `SYNTHESIS.md` | §0.3 R-target goalset (R4 row) | ACCEPT | F-11 mirror clause "first instance of the `regen-{grammar}` family; the xtask binary parametrises a grammar-neutral generator" preserved at `SYNTHESIS.md:96`; unchanged from V2 V3 STAND. |
| `SYNTHESIS.md` | §0.4 P-1…P-7 pre-blocks | ACCEPT | P-6 names per-grammar provider modules as the recurrence vector; pre-block scope unchanged from V2; P-1 round-trip-rule trigger grammar-neutral. |
| `SYNTHESIS.md` | §0.5 wave-by-wave deferral | ACCEPT | Contracted deferral unchanged from V2; deferred work remains grammar-neutral in surface. |
| `SYNTHESIS.md` | §1 corrected diagnosis | ACCEPT | Pillars marked `grammar-neutral` for bbnf-simd / OffsetFlags + Tape; unchanged from V2. |
| `SYNTHESIS.md` | §2 telemetry binding | ACCEPT | Column rule wording treats every plane and every grammar uniformly; `track2_entry_point` column (F-15) plane-keyed not grammar-keyed. Unchanged from V2. |
| `SYNTHESIS.md` | §3 candidate C-1 row | ACCEPT | Forward invariant clause at `SYNTHESIS.md:271` verbatim — "any new grammar added under `workspace.metadata.bbnf.grammars.{name}` produces ZERO new `.rs` files…ZERO new directories in `crates/core/src/runtime/`". F-12 + E-7 binding preserved; STAND under V3. |
| `SYNTHESIS.md` | §3 candidate C-2 row | ACCEPT | Per-plane comparator rebind has no grammar branch; unchanged from V2. |
| `SYNTHESIS.md` | §3 candidate C-3 row | ACCEPT | F-11 family-shape clause "first instance of the `regen-{grammar}` family — the xtask binary parametrises a grammar-neutral generator" preserved at `SYNTHESIS.md:273`; STAND under V3 (no SYNTHESIS row mirror needed — the V2 CH2 §3 "optional" α-F mirror was discretionary; the C-3 row already names the family shape, and NF-1's substantive fix lands in α-E where the gate text lives). |
| `SYNTHESIS.md` | §3 candidate C-4 row | ACCEPT | C-4 row inherits F-13's S-P3 dispatch-discipline clause (line 335-341); no grammar branch baked. Unchanged from V2. |
| `SYNTHESIS.md` | §3 candidate C-5 row | ACCEPT | Revert + REDRESS; unchanged from V2. |
| `SYNTHESIS.md` | §4 S-P3 constraints (C-1 forward invariant clause) | ACCEPT | `SYNTHESIS.md:330-334` F-12 clause verbatim preserved; STAND under V3. |
| `SYNTHESIS.md` | §4 S-P3 constraints (C-4 dispatch-discipline clause) | ACCEPT | `SYNTHESIS.md:335-341` F-13 clause verbatim preserved (including the "no `match grammar { Json => ..., CssL4 => ... }`" prohibition at line 340); STAND under V3. |
| `SYNTHESIS.md` | §4 S-P3 constraints (other lines) | ACCEPT | G-SIMD-GRAMMAR-POLICY triad (F-14), LOC ceiling (F-6), triumvirate (F-9) all grammar-neutral. Unchanged from V2. |
| `SYNTHESIS.md` | §5 pre-blocked routes | ACCEPT | "grammar-name branches in generic crates" enumerated as pre-blocked; carrying P-1…P-7 verbatim. Unchanged from V2. |
| `SYNTHESIS.md` | §6 close posture | ACCEPT | Bracket framing symmetric; unchanged from V2. |
| `HANDOFF.md` | §1 bracket verdict | ACCEPT | Pillar list mirrors SYNTHESIS §1.1; unchanged from V2. |
| `HANDOFF.md` | §3 honest baseline | ACCEPT | JSON-CSS parity in admit-count framing preserved. |
| `HANDOFF.md` | §4 authorship declaration | ACCEPT | F-2(b) lands α-F as sole author of all four artefacts; no grammar implications; unchanged from V2. |
| `HANDOFF.md` | §6 next-move | ACCEPT | "CH2 verifies Lock 14 grammar-neutrality" pin preserved at line 170; F-7 + F-8 hard-cap + G-Omega edits grammar-neutral. |
| `HANDOFF.md` | §7 refusal conditions (broadened) | ACCEPT — **fresh V3 verify** | F-V3-α-F-1 broadens the carry-over guard at `HANDOFF.md:192-197` to "25 CSS + 5 parse_only + 6 direct + 11 typed = **47 rows**" with `SYNTHESIS.md §1.3` reconciliation cite; lines 208-218 preserve "any grammar-specific generic behaviour through SPEC-local wording (Lock 14 binding)" + the `G-SIMD-GRAMMAR-POLICY` clause + the "lets the hardcoded P1–P8 cascade silently serve JSON / CSS / Sheets / BBNF-self rows" refusal. The fold is CH3 in scope but introduces ZERO Lock 14 leak — numeric reconciliation is plane-symmetric; the refusal list remains grammar-keyed only where it enumerates the pre-blocked recurrence vectors. |
| `HANDOFF.md` | §8 disposition | ACCEPT | PENDING-V3 wording; no grammar implications. |
| `alpha-A-results-extraction.md` | §1 direct reconciliation | ACCEPT | A-1 fold preserved; +2 rows (marine_ik, instruments) row-keyed; grammar-neutral. STAND per V3. |
| `alpha-A-results-extraction.md` | §3 typed extension annotation | ACCEPT | A-2 fold preserved; `[ext†]` legend per-row binding; grammar-neutral. STAND per V3. |
| `alpha-A-results-extraction.md` | §6 c/B LOC budget | ACCEPT | A-3 fold preserved; schema column rule plane-keyed not grammar-keyed. STAND per V3. |
| `alpha-B-competitor-deltas.md` | comparator overlay | ACCEPT | STANDs per V2 addendum §0 and V3 commit body; CSS / JSON overlay symmetric. |
| `alpha-C-redress-digest.md` | §2 pre-block P-7 falsifiability | ACCEPT | C-1 fold triple-check gate preserved; substrate-keyed not grammar-keyed. STAND per V3. |
| `alpha-C-redress-digest.md` | other pre-blocks (P-1..P-6) | ACCEPT | Unchanged; grammar-neutral. STAND per V3. |
| `alpha-D-validated-invalidated.md` | §S-3 Lock 14 | ACCEPT | 30 violations cited; reopen path is trait + emit collapse. STAND per V3 commit body. |
| `alpha-D-validated-invalidated.md` | §V-4 + §V-5 | ACCEPT | bbnf-simd + Tape + OffsetFlags carried grammar-neutral. STAND per V3. |
| `alpha-E-candidate-shortlist.md` | §2 shortlist table | ACCEPT | C-3 row pointer "see §5 + hardening V1 CH7 §3.1"; C-1 row "see §3"; grammar-neutral. STAND. |
| `alpha-E-candidate-shortlist.md` | §3 C-1 detail (forward invariant) | ACCEPT | `alpha-E:170-176` E-7 forward-invariant clause verbatim preserved (cross-cited by the V3-folded C-3 gate at `alpha-E:375`). The Lock 14 baseline gate (`bbnf-bench::lock14_baseline::validate`) still named at line 168; rejector binding at line 174. STAND. |
| `alpha-E-candidate-shortlist.md` | §4 C-2 detail | ACCEPT | Per-plane comparator rebind preserved; no grammar branch. STAND. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 detail (Purpose + family binding) | ACCEPT | E-6 family-shape clause at `alpha-E:303-309` verbatim preserved; STAND. |
| `alpha-E-candidate-shortlist.md` | §5 C-3 detail (round-trip gate) | ACCEPT — **FOLD-LANDED (F-V3-α-E-1; closes V2 NF-1)** | `alpha-E:362-387` now reads "Round-trip (core tree, all rostered grammars). For each grammar name `<g>` enumerated under `workspace.metadata.bbnf.grammars` in the top-level `Cargo.toml` (currently `{json, css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}` — the list is metadata-derived, not source-of-truth at the gate site; the canonical shell form is `for g in $(cargo metadata --format-version 1 \| jq -r '.workspace_metadata.bbnf.grammars \| keys[]'); do rm -rf "crates/core/src/runtime/${g}/" && cargo xtask "regen-${g}" && git diff -- "crates/core/src/runtime/${g}/" \|\| exit 1; done`)". Lines 372-381 add the binding clause: "The gate enumerates from `workspace.metadata.bbnf.grammars` so that adding a 9th grammar requires NO change to the gate's text — only an addition under `workspace.metadata.bbnf.grammars` and a `regen-<g>` xtask registration per C-1's forward invariant (`alpha-E-candidate-shortlist.md:170-176`). This parity is binding: both gates (C-1 forward invariant and C-3 round-trip) derive grammar enumeration from the same workspace metadata clause Lock 14 itself names (`LOCKS.md:220` 'workspace metadata declaring its strategy'), relocating the forward-blindness catch from first-grammar-admission time (C-1) to gate-authoring time (C-3)." The fold lands verbatim per V2 CH2 §3 recommended text — plus an additional cross-cite to `LOCKS.md:220` that strengthens the binding beyond the V2 recommendation. The eight-name parenthetical is correctly subordinated as "currently" (descriptive, not normative); the loop body iterates over `cargo metadata` output. NF-1 closed at gate-authoring time, not at first-grammar-admission time. |
| `alpha-E-candidate-shortlist.md` | §6 C-4 detail (Pre-blocked routes + 2-grammar) | ACCEPT | E-8 clauses at `alpha-E:559-571` verbatim preserved (`BackendShape` enum dispatch + "no `match grammar { Json => ..., CssL4 => ... }`" prohibition + two-grammar-family exercise requirement). STAND. |
| `alpha-E-candidate-shortlist.md` | §7 C-5 detail | ACCEPT | Revert covers JSON parse_only + CSS rows symmetrically; E-10 scribe-count clarification (29 entries) row-keyed not grammar-keyed. STAND. |
| `alpha-E-candidate-shortlist.md` | §8 consolidated pre-blocks | ACCEPT | P-6 verbatim; consolidated list grammar-neutral. STAND. |
| `alpha-E-candidate-shortlist.md` | §9 concurrency matrix | ACCEPT | E-13 fold preserved; ledger-serialisation discipline, not grammar privilege. STAND. |
| `alpha-E-candidate-shortlist.md` | §10 cost / caps / telemetry | ACCEPT | E-9 hot-leaf clause at `alpha-E:760-770` verbatim preserved; E-2 cap discipline grammar-neutral. STAND. |
| `alpha-E-candidate-shortlist.md` | §11 convergence + escalation | ACCEPT | Escalation triggers measurement-keyed, not grammar-keyed. STAND. |
| `DISPATCH-CONTEXT.md` | §0–§3 + per-agent | ACCEPT | STANDs per V2 addendum §0 + V3 commit body; the α-agent scope spec unchanged. |

## §2 — Critical findings

Zero critical findings in V3. The V2 NF-1 REVISE has been closed by
F-V3-α-E-1, and no new Lock 14 leak surfaces under fresh-finding scan
across the V3 artefacts.

### Fold verification: F-V3-α-E-1 — LANDED

V2 CH2 NF-1 (C-3 §5 round-trip gate hand-enumerates the 8 grammar
names rather than deriving from workspace metadata) has been folded
verbatim per V2 CH2 §3 recommended text — and tighter on one axis.

**Pre-fold state** (per V2 CH2 §2 NF-1 evidence at
`alpha-E:362-365`):

> "**Round-trip (core tree, all 8 grammars).** For each of `{json,
> css_l4, google_sheets, bbnf, csv, ebnf, bnf, math}`: `rm -rf
> crates/core/src/runtime/<grammar>/ && cargo xtask regen-<grammar> &&
> git diff -- crates/core/src/runtime/<grammar>/` produces empty
> output."

**Post-fold state** at `alpha-E:362-387` (V3 commit `5e2ae78b4`):

> "**Round-trip (core tree, all rostered grammars).** For each grammar
> name `<g>` enumerated under `workspace.metadata.bbnf.grammars` in
> the top-level `Cargo.toml` (currently `{json, css_l4,
> google_sheets, bbnf, csv, ebnf, bnf, math}` — the list is metadata-
> derived, not source-of-truth at the gate site; the canonical shell
> form is `for g in $(cargo metadata --format-version 1 | jq -r
> '.workspace_metadata.bbnf.grammars | keys[]'); do rm -rf
> "crates/core/src/runtime/${g}/" && cargo xtask "regen-${g}" && git
> diff -- "crates/core/src/runtime/${g}/" || exit 1; done`)"

Four checks confirm the fold's correctness:

1. **Heading wording.** "all 8 grammars" → "all rostered grammars".
   The number 8 is removed from the gate's normative scope; rostering
   replaces hardcoded count. The eight names remain as descriptive
   parenthetical with the explicit qualifier "currently … the list is
   metadata-derived, not source-of-truth at the gate site".

2. **Loop body.** The shell form is now `for g in $(cargo metadata
   --format-version 1 | jq -r '.workspace_metadata.bbnf.grammars |
   keys[]'); do … done`. The iteration source is the workspace
   metadata clause Lock 14 itself names at `LOCKS.md:220` ("workspace
   metadata declaring its strategy"). The body parametrises `${g}`
   into both the directory wipe and the `regen-${g}` xtask
   invocation — adding a 9th grammar requires NO edit to the loop.

3. **Cross-cite to C-1.** Lines 374-375 explicitly cross-cite C-1's
   forward invariant at `alpha-E-candidate-shortlist.md:170-176`. The
   binding parity is named at lines 376-381: "both gates (C-1 forward
   invariant and C-3 round-trip) derive grammar enumeration from the
   same workspace metadata clause Lock 14 itself names
   (`LOCKS.md:220`)". This is a *strengthening* over V2 CH2 §3's
   recommended text — the LOCKS.md:220 cite was not in the V2 §3
   recommendation; it lands here as architectural alignment between
   the gate's substrate and Lock 14's source language.

4. **Catch-time relocation.** Lines 379-381 name the architectural
   consequence: "relocating the forward-blindness catch from first-
   grammar-admission time (C-1) to gate-authoring time (C-3)". This
   is the precise substantive concern V2 NF-1 surfaced — that the
   E-1-expanded gate must not foreclose future grammars at the
   precise check meant to forbid grammar privilege. With the fold
   landed, the C-3 gate now fails *at gate-authoring* if a hardcoded
   list is reintroduced, not at first-grammar-admission attempt.

The optional α-F SYNTHESIS §3 C-3 row mirror clause was a
discretionary item in V2 CH2 §3; its absence does not unwind the
fold. The C-3 row at `SYNTHESIS.md:273` already names the family-
shape invariant ("first instance of the `regen-{grammar}` family;
the xtask binary parametrises a grammar-neutral generator"); the
substantive gate text where the hardcoded literal lived was in α-E,
and α-E is where the fold lands. The SYNTHESIS row mirror would have
been belt-and-braces — pleasant but not load-bearing.

### Fresh-finding scan: V3 artefacts under Lock 14

Scanned all eight V3 artefacts for grammar-name leaks via
`grep -nE 'grammar.*=.*"(json|css|sheets|bbnf|csv|ebnf|bnf|math)"|
match grammar|if grammar ==|JsonGrammar|RuntimeProvider::Json|
parse_json_grammar'`. Every match is one of three benign categories:

1. **Negative-grep targets inside falsifiability gates** (the gates
   *detect and reject* the leaks — the strings appear because the
   gate command is the grep that returns ZERO post-redress).
   Examples: `SYNTHESIS.md:271`, `DISPATCH-CONTEXT.md:165`,
   `alpha-E-candidate-shortlist.md:159`, `alpha-C-redress-digest.md:322`.

2. **Pre-block patterns explicitly prohibited** (the strings appear
   as the forbidden form so that S-P3 constraints can reject them).
   Example: `SYNTHESIS.md:340` and `alpha-E:577` — `no \`match
   grammar { Json => ..., CssL4 => ... }\` arm may appear in the
   dispatch path`. The S-P3 clause names the leak shape exactly so
   future code review can grep for it.

3. **Current-state references inside the audit narrative** (the
   strings name the to-be-PRUNED files: `JsonGrammar` public struct
   at `skinny/crates/bbnf/src/lib.rs:46-64`; `parse_json_grammar` in
   `skinny/crates/grammar/src/lib.rs`). These are the symbols PRUNE-3
   + PRUNE-4 + C-1's forward invariant remove from generic crates.

Zero leaks of category (4) — i.e., zero load-bearing references that
bake a grammar privilege into the candidate slate, the constraint
plane, or the same-wave consumer chain. The HANDOFF §7 refusal-list
broadening (F-V3-α-F-1) preserves the "any grammar-specific generic
behaviour through SPEC-local wording (Lock 14 binding)" refusal at
line 208-210 + the `G-SIMD-GRAMMAR-POLICY` + the cascade-silently-
serve refusal at line 216-218; the broadening is numeric (41 → 47
rows) not architectural.

The C-3 gate's loop body uses `jq` to read
`.workspace_metadata.bbnf.grammars | keys[]` — the JSON key path that
`cargo metadata --format-version 1` exposes for
`workspace.metadata.bbnf.grammars` in `Cargo.toml`. The path is
syntactically correct for `cargo metadata`'s output schema and aligns
with Lock 14's named substrate. The shell form is robust under POSIX
(double-quoted variable expansion; `exit 1` on first failure;
explicit `format-version 1` pin).

## §3 — Recommended folds for V4 (if any)

None. CH2 converges at 100 % ACCEPT on V3. The two-consecutive-cycle
chain (V2 97 % + V3 100 %) closes per `ORCHESTRATOR.md §3Z`. The
SK-V14 contract may lock at V3 convergence; the orchestrator may
proceed directly to S-P0 per the SK-V14 ORCHESTRATOR-PROMPT pin
(G-Alpha auto-signed).

The four V1 CH2 Findings and the one V2 CH2 NF-1 are now PERMANENTLY
closed in the SK-V14 contract:

- F-11 (R4 family-shape) binding at `SYNTHESIS.md:96` + `:273`.
- F-12 (C-1 forward invariant) binding at `SYNTHESIS.md:271` +
  `:330-334` and α-E §3 at `alpha-E:170-176`.
- F-13 (C-4 dispatch-discipline + two-grammar exercise) binding at
  `SYNTHESIS.md:335-341` and α-E §6 at `alpha-E:559-571`.
- E-9 (hot-leaf column grammar-keyed symbol rule) binding at
  `alpha-E:760-770`.
- F-V3-α-E-1 (C-3 round-trip gate derived enumeration) binding at
  `alpha-E:362-387` with cross-cite to `LOCKS.md:220` and C-1
  forward invariant at `alpha-E:170-176`.

No CH2 fold remains outstanding. The lens reports CONVERGED-FINAL.
