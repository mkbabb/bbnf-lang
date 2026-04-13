# Prior-Tranche Retrospective (AR-audit-prior)

Audit scope: the five most recent landed tranches — AP, AO, AN, AM, AK
(AL is prototype-only: see §2). Each section records the plan scope,
the commits that landed it, the items that were deferred or reverted,
and a verdict. Two codebase-wide inventories (TODO markers, `#[allow]`
attributes) follow. A three-paragraph trend analysis closes.

---

## 1. Five-tranche commit-level status

### AP — Correctness, enrichment, structural activation (partial)

**Scope:**
1. Activate structural pre-scan dispatch across JSON and CSS L4 (AP.1)
2. Land Bool / U8 payload projection alongside the existing F64 path
   (AP.2) plus SIMD whitespace bitmap (AP.3.1) and SIMD string scanner
   (AP.3.3)
3. Key-dispatch for CSS L4 `__declaration` (+35-50% bootstrap)
   (AP.4), scanner kernel surgery (AP.5.1-5.3)

**Landed commits (git log --grep="AP\\.")** — 15 entries:
- `2f7c1bd` structural dispatch deletion (reassigned to AQ.5)
- `95842dc` CSS L4 declaration key dispatch (AP.4)
- `4417f8a`, `2fa3172` peek-only structural dispatch + WS-elision gate
- `a603df9` NibbleLut DFA + SIMD delimiter scan (AP.5.1-5.2)
- `06a2083` `Tape::get_unchecked` in cursor hot paths (AP.5.3)
- `2a8af08` structural dispatch disabled + WS trim restored (AP.1
  RESCINDED)
- `02018a4` `scan_json_number_f64` wiring (AP.0.2)
- `4af16a1` `@ws` pattern threading (AP.ws)
- `b354189` bootstrap rule kind stale ref fix (AP.0.1)
- `30f01ee` Bool / U8 payload wiring (AP.2)

**Deferrals recorded in AP.md / AQ-audit.md:**
- AP.1 structural activation FAILED / DISABLED (two successive attempts
  rolled back, then resurfaced in AP.1b as peek-only before AQ.5
  deleted the entire infrastructure)
- AP.3.2 trim elision, AP.3.4 SIMD `filter_quote_parity`
- AP.4.2 pattern hoist, AP.4.3 CSS L4 type errors, AP.4.4 CSS L4
  structural investigation
- AP.5.4 UTF-8 defer, AP.5.5 TapeBuilder default prealloc
- AP.6.4 cost sweep, AP.6.5 global CSP solve

**Verdict — PARTIAL.** The headline AP.1 activation never shipped; its
infrastructure was deleted in AQ.5. Supporting items (AP.3.1 WS bitmap,
AP.3.3 SIMD string, AP.4 key dispatch) landed and drove the measured
+AP throughput. Six of nine plan items deferred to AQ.

---

### AO — Structural dispatch primary lever + scanner generalization

**Scope:**
1. Pre-scan infrastructure (IR pass, ParserState cursor, quote-parity
   filter, codegen pre-scan emission) — Phase 0
2. Padded-buffer mode (Phase 1), parse-that generalization (Phase 2),
   SIMD widening (Phase 3), cost-model calibration (Phase 4), CSS L4
   correctness + self-hosting + polish (Phase 5)

**Landed commits (git log --grep="AO\\.")** — 4 entries:
- `7198c97` structural pre-scan dispatch + WS elision (AO.0.4-0.6)
- `4114695` `compute_structural_bytes` IR pass (AO.0.1)
- `b2167e0` number scanner reference updates (AO.2.2)
- `89f9fe3` AP plan + AO status doc update (housekeeping)

**Deferrals, by AO.md's own header:**
> "Phase 0 (structural dispatch) infrastructure was implemented in the
> emitter codegen but never activated in any production grammar. The
> proc-macro derive path does not call the structural pre-scan entry
> point… Phases 1-5 remain plan-only with no code written."

All remaining AO items were absorbed into AP. Concretely:
AO.2.1 CSS re-export delete PARTIAL, AO.2.5 `sp_json/sp_css` delete
NOT LANDED, AO.2.6 nibble-LUT dedup NOT LANDED, AO.3 SIMD widening
NOT LANDED, AO.4 cost calibration NOT LANDED, AO.4.2 global CSP NOT
LANDED, AO.5.1 CSS tailwind grammar fix landed late in AP.

**Verdict — RUBBER-STAMPED.** Only AO.0.1 (IR pass) + AO.0.4-0.6
(codegen) + AO.2.2 (scanner rename) landed. The primary lever
(structural dispatch activation) never flipped to true. Five of six
phases were plan-only.

---

### AN — Correctness + f64 payload + `@ws` SIMD

**Scope:**
1. AN.0 correctness bugs (6 items: `__has_children`, `__branch_idx`,
   serialize namespace pollution, LSP regression, CSS L4 tailwind,
   `Debug` derive gap)
2. AN.1 CSS `@ws` SIMD routing
3. AN.2 scanner generalization
4. AN.3 single-pass string scan, AN.4 WS bitmap cache, AN.5 32-byte
   SIMD, AN.6 instrumentation

**Landed commits (git log --grep="AN\\.")** — 8 entries:
- `8012dac` `__has_children` + inner `branch_idx` (AN.0.1+0.2)
- `361e3c9` per-parser submodule wrapping (AN.0.3)
- `ad219bd` inlined tape compound AST walkers (AN.0.4)
- `fffd76f` value-keyword nonterminal-ref fallback (AN.0.4)
- `a4341c0` `@ws` SIMD routing (AN.3.1 — labelled AN.1 in plan)
- `bc8277a`, `dc7935b` post-AN bench docs
- `acaa189` plan doc
- `0c61dd5` three-tier f64 payload projection (AN Phase 0)

**Deferrals per AN.md's "What Landed" contract and AQ-audit.md:**
- AN.2 scanner generalization NOT LANDED (deferred to AO → AP → AQ.4)
- AN.3 single-pass string scan NOT LANDED
- AN.5 32-byte SIMD NOT LANDED
- AN.6 instrumentation NOT LANDED (finally shipped in AQ.9.3 / commit
  `a78c4a6`)
- AN.2 + AN.3 + AN.5 all absorbed by AO's Phase 2 / 3, which itself
  was then absorbed by AQ.4 / AQ.8

**Verdict — PARTIAL.** AN.0 correctness cluster fully landed and all
six bugs were fixed. AN.1 `@ws` SIMD routing shipped with clean
measurements (-29% total expanded CSS code). AN.4.2 WS bitmap cache
shipped. AN Phase 0 f64 payload projection shipped. But everything in
AN.2/3/5/6 deferred forward into a chain of three subsequent tranches.

---

### AM — Tape purity + SIMD parity

**Scope:**
1. AM.0 fix three regressions blocking workspace
2. AM.1 delete EmissionTier axis + BumpSlab (~2000 LOC)
3. AM.2 tape payload buffer
4. AM.3 per-branch tape surgery
5. AM.4 SIMD string scanner
6. AM.5 structural bitmap pre-scan infrastructure
7. AM.6 cost-model calibration

**Landed commits (git log --grep="AM\\.")** — 9 entries:
- `4d1afeb` AM.0 workspace regressions
- `7608530` AM.1 EmissionTier + BumpSlab delete (-2,306 LOC)
- `17f794e` AM.2 tape payload buffer
- `cffcb6b` AM.3 per-branch `push_leaf`/`push_compound`
- `f62a69b`, `c722a58`, `6984199` post-AM bench docs

**Deferrals per AM.md's "Remaining gap analysis":**
- AM.4 SIMD string scanner built (+425 LOC) but showed ~neutral
  impact vs LLVM-optimized inline HIR — DOCUMENTED AS NEUTRAL
- AM.5 structural bitmap infrastructure built (+300 LOC) but not
  integrated into codegen (AM.5.3); deferred to AN → AO → AP (where it
  was then deleted in AQ.5)
- AM.6 cost-model calibration NOT LANDED (deferred forward)

**Verdict — FULLY LANDED for declared work.** Every AM.0-3 phase
shipped with measured deltas; AM.4 built, integrated, measured,
documented as neutral. The deferrals are transparent in the plan: the
tranche correctly distinguishes "shipped" from "investigated but not
merged" at the tail.

---

### AK — Flat Vec tape substrate + per-branch variant_idx

**Scope (small tranche):**
1. Replace `ChunkedArena<TapeRec>` with flat `Vec<TapeRec>` (AK.0)
2. Thread `__branch_idx: u8` for per-branch discriminator (AK.1)
3. Fix variant_idx correctness (AK.2)

**Landed commits (git log --grep="AK\\.")** — 3 entries + 1 bench
doc:
- `0fc6beb` AK.0 flat Vec substrate
- `9658bb2` AK.1 + AK.2 per-branch `variant_idx` via `__branch_idx`
- `c62ad38` AK.3 post-AK bench baseline doc

**Deferrals:** None recorded. AK was a tight 3-item tranche and every
item shipped. Measured impact +10-14% across every JSON file.

**Verdict — FULLY LANDED.** Canonical example of a focused tranche
shipping complete.

---

## 2. AL — no execution tranche

The four `AL-prototype-*.md` files exist as discussion documents. There
is no `AL.md`. The entire AL plan was "subsumed into AM / AN / AO / AP"
per AQ-audit.md line 26. A single commit (`e6574a9 fix(serialize):
rewrite codegen for tape-first architecture (AL.1)`) bears the AL label,
and it covered the AN Phase 1.1 serialize rewrite work. AL-prototype-2's
key diagnosis (unified ABI with tape pass-through for direct-to-struct
projection) was explicitly acknowledged but never implemented — AM took
the opposite path (delete Tier B, use payload side-channel) and the full
direct-to-struct feature remains a standing audit item (see
`AR-audit-direct-struct.md`).

---

## 3. TODO / FIXME / HACK inventory

Command:
```
grep -rn -E "\b(TODO|FIXME|XXX|HACK)\b" crates/ parse-that/rust/
  | grep -v "\.md:" | exclude-dir=target
```

**Total: 1 match across the entire codebase.**

| Crate | Count | Kind | Line |
|-------|-------|------|------|
| `crates/ir` | 1 | `XXX` (placeholder token, not a marker) | `crates/ir/src/types/grammar.rs:247` |

The single hit is a comment template using `XXX` as a literal rendered
stand-in for a future method name; it is not a defect marker. Zero
TODO, zero FIXME, zero HACK. By this metric the codebase is spotless.

**Deferred / pending marker inventory** (stricter filter):
```
grep -rn -E "\b(deferred|Deferred|DEFERRED|pending|Pending|PENDING)\b"
  crates/ | 10 architectural comment hits
```

Notable chronic entries:
- `crates/core/src/grammar/generated.rs:12341` — `// that rewrites it is
  deferred to AG.4b` — AG landed 2026-03, still deferred at AQ close.
- `crates/core/src/grammar/schema/emit/ts.rs:3` — `//! Contract (fixed;
  implementation deferred)` — schema TS emit never implemented.
- `crates/core/src/grammar/schema/emit/runtime.rs:3` — same pattern.
- `crates/core/src/backend/rust/emitter/operator_chain.rs:29` — operator
  chain specialization deferred under tape-first.
- `crates/core/src/backend/rust/emitter/map_value.rs:4,154` — map-expr
  evaluation deferred to view layer (intentional architectural choice).

The remaining 4-5 hits are either legitimate variable names (`pending:
Vec<Id>` in the e-graph) or doc-string history (`Tranche Y.5 introduced
the UnionFind substrate as a deferred fix`).

---

## 4. `#[allow(...)]` attribute inventory

Command:
```
grep -rn "#\[allow(" crates/ | exclude-dir=target
```

**Total: 237 occurrences across 27 files.**

**Distribution by kind** (one `#[allow]` can contain multiple lints):

| Lint | Count | Notes |
|------|-------|-------|
| `dead_code` | 90 | |
| `non_snake_case` | 78 | |
| `non_camel_case_types` | 60 | |
| `clippy::zero_prefixed_literal` | 8 | |
| `non_upper_case_globals` | 3 | |
| `clippy::too_many_arguments` | 2 | |
| `unused_variables` | 1 | |
| `unused` | 1 | |
| `deprecated` | 1 | |
| `clippy::all` | 1 | |

**Distribution by file** (top 5):
| File | Count | % of total |
|------|-------|------------|
| `crates/core/src/grammar/generated.rs` | 190 | **80%** |
| `crates/core/src/backend/rust/view/*.rs` (6 files) | 12 | 5% |
| `crates/core/src/grammar/schema/emit/rust/*.rs` (2 files) | 8 | 3% |
| `crates/csp-solver/tests/solver.rs` | 7 | 3% |
| remaining 19 files | 20 | 9% |

**Reading of the distribution.** `generated.rs` dominates because the
self-hosted grammar codegen output requires codegen-level lint
suppressions for rule/variant names that clash with Rust naming
conventions. The 80% concentration in one hand-maintained generated
file is the single clearest signal of the open Phase 1.2 self-hosting
debt: the generated file cannot be fully regenerated, so its lint
profile stays on the disk. Outside `generated.rs`, 47 allows across the
remaining 237 - 190 = 47 lines is ~1.7 allows per touched file — well
within acceptable background noise for a codegen-heavy crate.

---

## 5. Trend analysis

**Paragraph 1 — Converging or accumulating.** The last three closed
tranches (AK → AM → AQ) show a clear convergence signature. AK was a
three-item, fully-landed tranche with +10-14% across every dataset.
AM added three fully-landed items with a headline +16% on canada
(beats sonic-rs) and explicitly documented its two "investigated
but not merged" items. AQ executed a hard-gated cleanup — delete
`PayloadKind`, delete structural dispatch infrastructure, deoverfit
`RegexClass` — verified by `grep` returning zero matches for all three
banned tokens. By contrast the middle pair (AN → AO) accumulated debt:
AN deferred four of six optimization phases, AO explicitly labelled
itself "Phase 0 code complete for Phase 0, never exercised end-to-end"
and rolled the remaining phases forward. The net is a V-shaped trend:
debt peaked at AO, then AP/AQ deliberately harvested and deleted the
stranded infrastructure. Zero TODO/FIXME/HACK markers in the final
codebase and only 10 `deferred` comments (four of which are
intentional architectural choices) confirm the convergence.

**Paragraph 2 — Chronic deferrals.** Four items show up in three or
more tranches as deferred or unimplemented:

1. **Clean bootstrap regeneration** — present in AC → AE → AF → AG →
   AI → AM → AN → AP → AQ → AR. Nine tranches and still open: AQ.1.2
   attempted regen, hit "sub-variant compound layout coupling," restored
   the span-text fallback at `5b06096`, and documented the deferral in
   `post-AQ.json` under `deferred_work[0]`. The concrete measure: 190
   of 237 workspace `#[allow]` attributes live in `generated.rs`.

2. **Global CSP solve** — AL-prototype-1, AL-prototype-2, AO.4.2,
   AP.6.5, AQ.9.5. Each proposes it, none ship it. AQ.9.5 explicitly
   marks it deferred with the rationale "per-component CSP solve is
   sufficient at current grammar scale."

3. **Cost-model grid sweep** — AM.6, AO.4.1, AP.6.4, AQ.9.4. Four
   successive tranches plan it; none run it. AQ.9.4 marks it
   "deferred" without resolution.

4. **BBNF_EGRAPH_REPORT / BBNF_CSP_REPORT release-build
   instrumentation** — AN.6, AO.4.3, AP.6.4, AQ.9.3. Shipped at AQ.9.3
   (commit `a78c4a6`, 2026-04-13) after four tranches of deferral.
   One of the four chronic items is now closed.

Secondary chronic items: scanner generalization (AN.2 → AO.2 → AQ.4
— shipped at AQ.4), structural dispatch activation (AM.5.3 → AN.5
→ AO Phase 0 → AP.1/1b — deleted at AQ.5).

**Paragraph 3 — Real state of Phase 1.2 (self-hosting closure) across
AE, AF, AG, AI, AM, AN, AP, AQ.** Phase 1.2 is the chronic deferral.
Each tranche either planned regen or acknowledged it was still
pending. Current status per `docs/benchmarks/post-AQ.json` and
`AR-audit-self-hosting.md`:

- AE-era: tape-first lowering landed, but clean regen deferred.
- AF / AG: EmissionTier axis built then deleted in AM.1; regen never
  attempted end-to-end.
- AI: `reconcile_cross_component_tiers` silently widens every Direct
  to Tape, so the premise that informed AE-AG's regen-block was
  strategically dead — the block was never lifted.
- AM / AN / AP: consumers of generated.rs were patched to work around
  the stale file (AM.0, AN.0.3 per-parser submodule wrapping, AP.0.1
  stale `rhs` / `value_expr` refs) without regenerating the file
  itself.
- AQ.1: the explicit attempt. `5b06096` restored the span-text
  fallback and `9f3f883` deleted the legacy bridge, but the underlying
  blocker (sub-variant compound layout vs typed view-accessor
  dispatch) was not resolved. Regen produces a divergent 78,307-line
  file that fails six grammar_roundtrip tests
  (per `AR-audit-self-hosting.md`).
- AR state: three open audit files (`AR-audit-self-hosting.md`,
  `AR-audit-scanners.md`, `AR-audit-direct-struct.md`). Self-hosting
  closure is the single largest open item in the roadmap, and it has
  been the single largest open item since Tranche AC.

---

## 6. Headline finding

Five of the last six tranches (AK, AM, AN, AP, AQ) are partial-to-full
lands of declared scope; AO is the one rubber-stamp ("code complete,
never exercised"). The codebase is technically clean — one `XXX`
placeholder, zero TODO/FIXME/HACK — but 80% of `#[allow]` attributes
(190 of 237) live in a single hand-maintained `generated.rs`, a direct
measure of the chronic Phase 1.2 self-hosting deferral that has
survived ten tranches (AC through AR). AQ.9.3 closed one of the four
chronic deferrals (instrumentation); the other three (clean regen,
global CSP solve, cost-model grid sweep) remain open heading into AR.
