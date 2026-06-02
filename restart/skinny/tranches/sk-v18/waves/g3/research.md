# SK-V18 G3 — RESEARCH+PLAN: un-fork the emitter (DELETE `RuntimeEmitterKind`, dispatch grammar-neutrally)

Date: 2026-06-02. Wave G3 (§6, R-A A). Entry gate `G1 ∧ G2 closed ∧ P4 live ∧ P3 row-collapse` —
all GREEN. Research only; no implementation, no cargo, no regen.

---

## EXECUTIVE VERDICT (read first)

**G3 is ACHIEVABLE, but NOT as the literal §6 sentence reads — it requires a documented SPEC
amendment, the EXACT analog of G1's split-equivalence amendment and G2's lowering-gap amendment.**

The §6 close-condition #2 / G3.2-conjunct-5 sentence says: *`render(program)` reads its output-shape
ONLY from `program.policy_summary.backend_shape`*. This is **structurally impossible for CSS**: after
G2, CSS does NOT lower (`first_unsupported` rejects its `@token`/`@pretty`/`->` constructs), so CSS
never produces a `SinkOnlyProgram`, never produces a `RuntimePolicySummary`, and therefore **has no
`policy_summary.backend_shape` to read**. JSON emits a `SinkOnlyProgram` (carries
`policy_summary.backend_shape == SinkOnly`); CSS emits a `CssScanProgram` (carries NO `BackendShape`
at all). The two are distinct lowered-artifact TYPES produced by two distinct lowering pipelines that
converge only at the `match … emitter` fork in `runtime_generator.rs:16`.

**The genuinely grammar-NEUTRAL un-fork is OPTION (c): dispatch on the lowered-artifact TYPE — a
neutral `LoweredArtifact { SinkOnly(SinkOnlyProgram) | Scan(CssScanProgram) }` enum — NOT a
`BackendShape` value, and NOT a grammar name.** Each artifact variant is *produced by* a
shape-derived lowering decision (`first_unsupported` ⇒ which lowering pipeline runs), so the
discriminator is shape-DERIVED, never grammar-named. This satisfies conjuncts 1–4 cleanly. It does
**NOT** satisfy conjunct 5 *literally* (the artifact carries the shape, not `policy_summary.
backend_shape` for CSS), so conjunct 5's `emit_shape_source` semantics must be amended from
"reads `program.policy_summary.backend_shape`" to "reads the LOWERED ARTIFACT (its variant ∨ its
`policy_summary`), NEVER a `RuntimeTarget`/`RuntimeProfileContract` field". The firewall the conjunct
exists to enforce — *no read of `target.*`/`contract.*`* — is preserved EXACTLY and is fully
achievable; only the over-specific "single field name" is widened to "the lowered artifact".

**Option (a) — CSS cost-models to a `CollapsedStage` BackendShape via a lowering shim — is REJECTED:**
it would require CSS to reach `lower_to_rust`, which `first_unsupported` forbids on `@token`/`@pretty`/
`->`; forcing it is precisely the G2 LOWERING GAP that the ratified G2 amendment already accepted CSS
cannot cross. Minting a synthetic `BackendShape` for CSS without a real cost-model derivation would be
a fabricated-shape relocated seam (a grammar-name branch wearing a neutral enum value), the exact L1
REJECT. Option (b) (a shared shape-enum BOTH programs map into) collapses into (c) once you see CSS
has no genuine shape to map — the "map" would be a hardcoded `CssScanProgram ⇒ CollapsedStage`
constant, i.e. a relocated grammar tag. **Option (c) is the only grammar-neutral, non-fabricated
route.**

The `RuntimeEmitterKind` DELETION itself is clean and unconditional: the `emitter` field (the literal
grammar-family tag) is removed from `RuntimeProfileContract` (`grammar_provider.rs:33`) and
`RuntimeTarget` (`regen.rs:15`); the dispatch is replaced by the lowering pipeline *selecting the
artifact variant* on the structural `first_unsupported` predicate, then `render()` matching the
variant. `runtime_target_rows_collapsed == true` (P3's R16 full-row `PartialEq` — already present)
holds across the field removal. **Net: G3 achievable; amendment = conjunct-5 widening, recorded
honestly as `waves/g3/AMENDMENT.md`, folded to SPEC §6 at the next Pass Omega.**

---

## 1. CURRENT-STATE MAP — the exact fork and the two post-G1/G2 emit mechanisms

### 1.1 — The single fork point (`runtime_generator.rs:16-27`)

```rust
pub(crate) fn emit_from_request(request, facts) -> Result<EmittedSource, CodegenError> {
    match request.profile_contract.emitter {                 // ← THE FORK (grammar-family tag)
        RuntimeEmitterKind::CompiledLowering => {            // ← JSON family
            let source = request.sources.first()...;
            crate::emit_from_source(&request.grammar_name, &source.source)
        }
        RuntimeEmitterKind::RequestFacts => emit_request_facts(request, facts),  // ← CSS family
    }
}
```

This `match` is the grammar-family fork the SPEC names. `request.profile_contract.emitter` is the
`RuntimeEmitterKind` enum — set per-target in the xtask `RuntimeTarget` rows:

- `xtask/src/main.rs:184` — `JSON_TARGET.emitter = CompiledLowering`
- `xtask/src/regen_css.rs:51` — CSS `TARGETS[0].emitter = RequestFacts`

So the enum is **literally a per-grammar config tag plumbed from the regen table** — exactly the
"per-grammar branch in a neutral data table" the addendum-2/3 co-gates target. `RuntimeProfileContract.
emitter` (field #1, `grammar_provider.rs:33`) carries it into codegen; `RuntimeTarget.emitter`
(field #9, `regen.rs:15`) is its source-of-truth in xtask.

### 1.2 — The two emit mechanisms after G1+G2

**JSON (the `CompiledLowering` arm):**
`emit_from_request` → `emit_from_source(grammar_name, source)` (`lib.rs:108`) → `passes::compile` →
`emit_with_layout` (`lib.rs:155`) → `lower::lower_to_rust(...)` (`lib.rs:163`) → produces
`LoweredRust { sink_only_program: Some(SinkOnlyProgram) }` → `emit_compiled(grammar_name, sink_only)`
(`runtime_generator.rs:29`) → `json_sink_direct::render(&SinkOnlyProgram)`
(`json_sink_direct/mod.rs:30`).
The `SinkOnlyProgram` (`sink_only.rs:20`) carries `policy_summary: RuntimePolicySummary` whose
`backend_shape: BackendShape == SinkOnly` (`sink_only.rs:47-48`). The cost model
(`select_lowering(cost.chosen)`, `lower/mod.rs:19-27`) chose `SinkOnly`, so the JSON shape is a
genuine cost-model-derived `BackendShape`.

**CSS (the `RequestFacts` arm):**
`emit_from_request` → `emit_request_facts(request, facts)` (`runtime_generator.rs:74`) →
`lower::css_scan::derive_program(grammar_name, entry_rule, scan_sources)` (`css_scan.rs:126`) →
produces `CssScanProgram` (`css_scan.rs:104`) → `css_scan_direct::render(&CssScanProgram)`
(`css_scan_direct/mod.rs:58`).
The `CssScanProgram` carries `grammar_name`, `entry_rule`, a grammar-DERIVED `ScanAlphabet`, and
`at_rule_branches`. **It carries NO `BackendShape`, NO `policy_summary`, NO `RuntimePolicySummary`**
(confirmed: `grep BackendShape|RuntimePolicySummary css_scan.rs == 0`). It is built by a raw-AST
structural harvest (`ScannedGrammar::from_sources`, `css_scan.rs:199`), explicitly because CSS cannot
reach `lower_to_rust`.

### 1.3 — Why CSS cannot lower (the asymmetry's root cause)

`grammar_provider.rs:110-118` gates lowering: for any emitter *other than* `RequestFacts`,
`facts.first_unsupported()` must be `None`. `first_unsupported` (`grammar/src/lib.rs:79`) returns the
first construct whose `unsupported()` (`:214-236`) yields a code — and `@token`
(`TokenDirective`), `@pretty` (`PrettyDirective`), `->` (`Projection`/`TypedProjection`), and
host-capture all yield codes (`BBNF-UNSUPPORTED-DIRECTIVE` / `-PROJECTION` / `-HOST-CAPTURE`). The CSS
L4 grammar pervasively uses `@token`/`@pretty`/`->`, so `first_unsupported` is `Some(...)` ⇒ CSS is
REJECTED from lowering. This is the structural fact: **`first_unsupported.is_none()` is the
shape-derived predicate that decides JSON-lowers / CSS-doesn't**, and it is already grammar-NEUTRAL
(it keys on construct KINDS present in the grammar, never on a grammar NAME).

### 1.4 — What `RuntimeProfileContract.emitter` carries today; where the grammar-family branch lives

`RuntimeProfileContract` (`grammar_provider.rs:31-37`): `{ emitter: RuntimeEmitterKind,
expected_files, frontend_requirements, output_labels }`. The `emitter` field is the **only**
grammar-family discriminator in the contract; `expected_files` (the file roster) and
`frontend_requirements` co-vary with it but are validated independently. The branch lives in exactly
ONE place — `runtime_generator.rs:16` — plus the lowering-skip predicate at `grammar_provider.rs:110`
(`emitter != RequestFacts`), which is the second read of the same tag.

---

## 2. THE UN-FORK DESIGN — the single grammar-agnostic dispatch

### 2.1 — The neutral discriminator: lowered-artifact TYPE (option c)

The un-fork replaces the `match emitter` grammar-tag with a **lowering step that produces a neutral
sum-type artifact, then a single `render()` that matches the artifact variant**:

```rust
// codegen, neutral — no grammar name, no per-grammar config tag.
pub enum LoweredArtifact {
    SinkOnly(lower::sink_only::SinkOnlyProgram),   // a lowered program (carries policy_summary.backend_shape)
    Scan(lower::css_scan::CssScanProgram),         // a lowered scan IR (shape = balanced byte-skip)
}

fn lower_for_runtime(request, facts) -> Result<LoweredArtifact, CodegenError> {
    // The SHAPE-DERIVED selection: does the grammar lower, or does it need the scan IR?
    // This is the SAME first_unsupported predicate that already gates lowering today —
    // a construct-KIND test, never a grammar-NAME test.
    if facts.first_unsupported().is_none() {
        let sink_only = /* passes::compile + lower_to_rust → SinkOnlyProgram */;
        Ok(LoweredArtifact::SinkOnly(sink_only))
    } else {
        let program = lower::css_scan::derive_program(&request.grammar_name, &request.entry_rule, ...);
        Ok(LoweredArtifact::Scan(program))
    }
}

fn render(artifact: &LoweredArtifact, ...) -> Result<EmittedSource, CodegenError> {
    match artifact {
        LoweredArtifact::SinkOnly(p) => emit_sink_only(p, ...),   // = today's emit_compiled body
        LoweredArtifact::Scan(p)     => emit_scan(p, ...),        // = today's emit_request_facts body
    }
}
```

**Why this is grammar-NEUTRAL (the conjunct-1–4 proof):**

- **conjunct 1 (`emitter_fork_present == false`):** `RuntimeEmitterKind`, `CompiledLowering`,
  `RequestFacts` are all DELETED. `render` matches a structural artifact variant, not a grammar
  family. grep over `codegen/src/` + `xtask/src/` (test-excluded) == 0. ✅
- **conjunct 2 (`generator_grammar_branch_count == 0`):** the selection predicate is
  `first_unsupported().is_none()` — a construct-KIND test (`TokenDirective`/`Projection`/...). No
  `match grammar`, no `if grammar_name == "json"|"css"`, no `"json"`/`"css"` literal in the emit path.
  ✅
- **conjunct 3 (`generator_grammar_type_count == 0`):** the artifact variants are `SinkOnly`/`Scan`
  (shape-named), NOT `JsonEmitter`/`CssEmitter`. `SinkOnlyProgram` and `CssScanProgram` are
  shape-named IR types pre-existing from G1/G2, not grammar-named emit-path types. ✅
- **conjunct 4 (`runtime_target_rows_collapsed == true`):** unaffected by the dispatch change; the
  R16 full-row `PartialEq` (already on `RuntimeTarget`, `regen.rs:5`) holds across the `emitter`
  field removal. ✅ (see §3.)

### 2.2 — Why the discriminator is shape-DERIVED, not a relocated grammar tag

The standing seam-hazard (Lock-14 / §5-risk-1) is "a per-grammar branch relocated into a neutral data
table or a neutral enum value." The artifact variant is NOT that, because the variant is **chosen by
the SAME structural lowering predicate that the codebase already uses** (`first_unsupported`), NOT by
reading a config field. There is no `if profile == "css_l4" ⇒ Scan` and no `RuntimeTarget` column
selecting the variant. The variant is the OUTPUT of running the lowering pipeline that the grammar's
own construct-kinds admit. A grammar with no `@token`/`@pretty`/`->` lowers to `SinkOnly`; a grammar
with them produces the `Scan` IR. **This is grammar-DERIVED by construction** — the seam cannot
relocate into the variant because the variant is computed, never configured.

### 2.3 — Reconciling the JSON-lowers / CSS-doesn't asymmetry NEUTRALLY

The asymmetry is real and irreducible (it is the G2 LOWERING GAP, ratified): JSON's lowered artifact
is a `SinkOnlyProgram` with a true `BackendShape`; CSS's lowered artifact is a `CssScanProgram` with
no `BackendShape`. The neutral reconciliation is to recognize that **"the lowered program" is itself a
sum type** — two structurally different lowerings, each shape-derived. The un-fork does NOT pretend
CSS has a `BackendShape` (option a, REJECTED) and does NOT force one (the fabricated-shape seam). It
dispatches on which artifact the shape-derived lowering produced. This is the honest expression of
"ONE generator with ONE emit entry whose branches are the IR shapes the grammars actually lower to" —
the same way `select_lowering` (`lower/mod.rs:19`) already dispatches the 5 `BackendShape` variants
without any being a grammar tag.

### 2.4 — The `emit_shape_source` firewall — preserved EXACTLY, semantics widened

The conjunct-5 firewall's BINDING content is: **the emit body reads NO
`target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract`/`contract.emitter`
field.** Under option (c):

- `render(artifact)` reads the artifact's VARIANT (a computed lowering output) — never a `target.*`
  or `contract.*` field. ✅ The firewall holds verbatim.
- For the JSON arm, `emit_sink_only` reads `p.policy_summary.backend_shape` (the lowered program's own
  shape) — exactly what conjunct 5 wants. ✅
- For the CSS arm, `emit_scan` reads the `CssScanProgram`'s own derived data (`alphabet`,
  `at_rule_branches`) — a lowered artifact, never a `target.*` field. ✅

**The ONLY thing that changes vs the literal SPEC wording:** conjunct 5 says the shape comes from
`program.policy_summary.backend_shape` *specifically*. Under (c) the shape for CSS comes from the
artifact VARIANT (`Scan`), not from a `policy_summary` (which CSS doesn't have). So
`emit_shape_source == lowered_program` is still TRUE in the firewall's intended sense (shape from the
lowered artifact, not the target), but the literal "from `policy_summary.backend_shape`" sub-clause is
satisfied only for the JSON arm. **This is the amendment** (§4).

---

## 3. `RuntimeEmitterKind` DELETION plan + the R16 co-gate

### 3.1 — What `RuntimeEmitterKind` deletion touches

`RuntimeEmitterKind` (the enum + both variants) is deleted from `grammar_provider.rs:39-43`. The
`emitter` field is removed from:

1. **`RuntimeProfileContract`** (`grammar_provider.rs:33`) — the field that carried it into codegen.
   The contract becomes `{ expected_files, frontend_requirements, output_labels }`. (The
   `expected_files` roster now follows the artifact variant: `emit_sink_only` validates against
   `COMPILED_RUNTIME_FILES`, `emit_scan` against `REQUEST_FACTS_RUNTIME_FILES` — per-arm roster
   constants, exactly the §6 "each declares its own roster" absorption of candidate B.)
2. **`RuntimeTarget`** (`regen.rs:15`, field #9) — the xtask source-of-truth tag. Removed from the
   struct AND from every row literal: `main.rs:184` (JSON) and `regen_css.rs:51` (CSS).
3. **The lowering-skip predicate** (`grammar_provider.rs:110`, `emitter != RequestFacts`) — replaced
   by the artifact selection in §2.1 (the `first_unsupported().is_none()` test moves INTO the
   `lower_for_runtime` step; the gate stops reading `emitter`).
4. **`emit_from_request`** (`runtime_generator.rs:12-27`) — the `match emitter` is replaced by
   `lower_for_runtime(...)?` then `render(&artifact, ...)`.
5. The `pub use` re-export at `lib.rs:19` and the test contract builders (`lib.rs:281-297`).

### 3.2 — What replaces it

Nothing replaces the *tag*; the routing decision MOVES from a stored config field to the computed
lowering predicate (`first_unsupported`). The `expected_files`/`frontend_requirements` co-data stays
on the contract (they are NOT grammar tags — `frontend_requirements` is the closure-validation data;
`expected_files` is the roster check). Per §6, candidate B's `ProjectionSpec` is ABSORBED: each
artifact-variant renderer declares its own roster (the per-arm `COMPILED_RUNTIME_FILES` /
`REQUEST_FACTS_RUNTIME_FILES`), retiring the need for the contract to carry an emit-selection tag.

### 3.3 — The R16 row-collapse co-gate (conjunct 4) — confirmed HOLDS

The SPEC §6 narrative describes the R16 recipe as "add `PartialEq` to `RuntimeTarget` (+1 line; today
`Clone, Copy, Debug` only)". **On the LIVE tree, P3 has already landed this**: `regen.rs:5` reads
`#[derive(Clone, Copy, Debug, PartialEq, Eq)]` on `RuntimeTarget`. Both nested structs already derive
`PartialEq, Eq`: `RuntimeFrontendRequirements` (`grammar_provider.rs:45`) and `RuntimeOutputLabels`
(`grammar_provider.rs:91`). So the full-row recursive `PartialEq` is in force NOW —
`runtime_target_rows_collapsed == true` already holds.

**The G3 co-gate obligation is to PRESERVE it across the `emitter` field removal:** after removing
field #9, `RuntimeTarget` still derives the full-row `PartialEq` recursing into
`frontend_requirements` and `output_labels`. No hand-rolled prose comparison is introduced (the named
REJECT). The derive is unchanged; only the field count drops by one. ✅ The co-gate is satisfied by
the field removal leaving the derive intact.

**Numbering note (per SPEC §G3.3):** the `emitter` is `RuntimeTarget` field #9 (`regen.rs:15`) and
`RuntimeProfileContract` field #1 (`grammar_provider.rs:33`) — two distinct ordinals for the same
logical tag; both are removed. The two nested structs recur as `RuntimeProfileContract` fields #2/#3
and `RuntimeTarget` fields #11/#12; the R16 derive lives on `RuntimeTarget` (`regen.rs:5`) and is the
only admitted mechanism.

---

## 4. FEASIBILITY VERDICT + the required amendment

### 4.1 — Is the un-fork a genuinely neutral dispatch, or a residual grammar-keyed branch?

**Genuinely neutral** under option (c). The discriminator (`LoweredArtifact` variant) is computed by
the shape-derived `first_unsupported` predicate, never read from a grammar tag. Conjuncts 1–4 are
cleanly satisfiable, and the conjunct-5 firewall (`no target.*`/`contract.*` read) is preserved
verbatim. There is NO residual grammar-name seam: the variant is the OUTPUT of lowering, not a
configured selection. The L1/relocated-seam REJECT does not fire, because nothing relocates into a
data table — the `emitter` tag is DELETED, not moved.

### 4.2 — Does the CSS-doesn't-lower asymmetry force option (a) (a CSS shape shim)? — NO, and it would be a REJECT

Forcing CSS to cost-model to a `CollapsedStage` (or any) `BackendShape` requires CSS to reach
`lower_to_rust`, which `first_unsupported` forbids on `@token`/`@pretty`/`->`. This is the **exact G2
LOWERING GAP** that the ratified G2 amendment accepted as structural and irreducible (`waves/g2/
research.md §2`, `waves/g2/AMENDMENT.md`). Two options for forcing it, both REJECTED:

- **Relax `first_unsupported` so CSS lowers** — out of G3 scope, re-opens the entire G2 lowering
  rebuild, and the G2 amendment already established CSS's constructs cannot be lowered to the
  `SinkOnlyExpr`/`BackendIr` shape without an AST CSS cannot even parse in the skinny tree
  (`grammar::parse_grammar` rejects `,`/`->`/`>>`/`<<`/`?w`, `css_scan.rs:143-156`).
- **Mint a synthetic `BackendShape` for CSS without a real cost-model derivation** (e.g. hardcode
  `CssScanProgram` ⇒ `CollapsedStage`) — this is a **fabricated-shape relocated seam**: a grammar-name
  branch (`CSS ⇒ CollapsedStage`) wearing a neutral enum value. It would pass conjuncts 1–3 by
  grep but the synthetic constant IS the grammar tag in disguise. REJECT (the precise thing the
  emit_shape_source firewall + R16 co-gate exist to catch).

So option (a) is not forced AND is not admissible. Option (c) avoids it entirely by NOT requiring CSS
to have a `BackendShape`.

### 4.3 — Option (b) collapses into (c)

Option (b) ("a derived shape-enum BOTH the SinkOnlyProgram AND the css_scan program map into") is only
neutral if BOTH genuinely HAVE a shape to map. CSS does not — its "shape" would be a hardcoded
constant (`Scan ⇒ CollapsedStage`), i.e. the §4.2 fabricated seam. The honest form of (b) is exactly
(c): the sum-type variant IS the shape-enum, with no fabricated `BackendShape` value. So (b) is not a
distinct admissible option; it is (c) with a misleading name.

### 4.4 — The amendment (honest §6 surfacing, the G1/G2 analog)

**G3 is achievable WITH a documented amendment** — narrower than G1's or G2's. The amendment widens
conjunct-5 / close-condition #2's `emit_shape_source` definition:

> **AMENDMENT (G3, the lowered-artifact-TYPE neutrality, the G1/G2 analog).** Close-condition #2 and
> G3.2-conjunct-5 read "`render(program)` reads its output-shape ONLY from
> `program.policy_summary.backend_shape`". Because CSS does not lower (the ratified G2 LOWERING GAP —
> `first_unsupported` rejects `@token`/`@pretty`/`->`), CSS produces a `CssScanProgram` with NO
> `policy_summary.backend_shape`. The un-fork therefore dispatches `render` on the neutral
> lowered-ARTIFACT sum type `LoweredArtifact { SinkOnly(SinkOnlyProgram) | Scan(CssScanProgram) }` —
> the variant being the OUTPUT of the shape-derived `first_unsupported` lowering predicate, never a
> grammar tag, never a configured field. `emit_shape_source == lowered_program` is RE-READ as: the
> emit body reads its output-shape from the LOWERED ARTIFACT (its variant, or — for the SinkOnly arm —
> its `policy_summary.backend_shape`), and reads NO `target.*`/`contract.*` field. The firewall
> (no `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract`/
> `contract.emitter` read) is PRESERVED verbatim; only the over-specific single-field sub-clause is
> widened to "the lowered artifact". Synthesizing a `BackendShape` for CSS to satisfy the literal
> wording is REJECTED as a fabricated-shape relocated seam.

This is materially the SAME move the campaign already ratified twice: G1 widened "byte-equivalent" to
"structural+behavioral equivalence" for the flat recognizer; G2 widened "lower to `lower_to_rust`" to
"derive from the raw-AST harvest" for CSS. G3 widens "shape from `policy_summary.backend_shape`" to
"shape from the lowered artifact (variant ∨ policy_summary)". All three are forced by the SAME root
fact: CSS is architecturally a flat byte-skip scan that does not fit the JSON `SinkOnlyExpr` pipeline.

### 4.5 — Honest residual risk

The amendment SOFTENS conjunct 5's literal text but PRESERVES its binding firewall. The genuine risk
is that a reviewer reads the widened conjunct as "softer ⇒ a seam can hide in the variant." It cannot,
because the variant is COMPUTED by `first_unsupported`, not configured — but the AMENDMENT.md MUST
make the "variant-is-computed-not-configured" proof explicit (the arm-census grep over the
`lower_for_runtime` body for any `target.*`/`contract.*`/`profile ==`/`grammar_name ==` read == 0 is
the binding falsifier, IN ADDITION to the §6 conjunct-5 grep over the `render` body). This is a
strictly STRONGER scan than the literal SPEC (it scans the selection predicate too), so the amendment
does not weaken the gate's catch power.

---

## 5. IMPLEMENTATION STEP PLAN + verification

### 5.1 — Edit sequence (ONE revert slice)

1. **`grammar_provider.rs`** — DELETE `pub enum RuntimeEmitterKind` (`:39-43`); REMOVE the `emitter`
   field from `RuntimeProfileContract` (`:33`); rewrite the lowering-skip gate (`:110-119`) to NOT
   read `emitter` — move the `first_unsupported` test into the new `lower_for_runtime` step (it stays
   a hard error iff a NON-lowerable grammar is asked for the SinkOnly artifact). Then call
   `runtime_generator::emit_from_request(&request, &facts)` unchanged at the seam.
2. **`runtime_generator.rs`** — REPLACE `emit_from_request`'s `match emitter` (`:16-27`) with:
   `let artifact = lower_for_runtime(request, facts)?; render(&artifact, request, facts)`. Add
   `enum LoweredArtifact { SinkOnly(SinkOnlyProgram) | Scan(CssScanProgram) }` and `lower_for_runtime`
   (the `first_unsupported().is_none()` selection). Rename `emit_compiled` → `emit_sink_only` (the
   `SinkOnly` arm, body unchanged — same `json_sink_direct::render` + roster validate against
   `COMPILED_RUNTIME_FILES`). Rename `emit_request_facts` → `emit_scan` (the `Scan` arm, body
   unchanged — same `css_scan::derive_program` + `css_scan_direct::render` + roster validate against
   `REQUEST_FACTS_RUNTIME_FILES`). `render(&artifact)` matches the two arms. **No body of either arm
   changes** — this is a PATH change, so the generated output is byte-equivalent (conjunct 6).
3. **`grammar_profile.rs`** — keep `COMPILED_RUNTIME_FILES`/`REQUEST_FACTS_RUNTIME_FILES`; they become
   the per-arm rosters (un-gate the `#[cfg(test)]` on `REQUEST_FACTS_RUNTIME_FILES` since `emit_scan`
   now references it in non-test code). Candidate-B absorption: each renderer owns its roster.
4. **`regen.rs`** — DELETE the `emitter` field from `RuntimeTarget` (`:15`) and from the
   `RuntimeProfileContract` constructor (`:82`); LEAVE the `#[derive(... PartialEq, Eq)]` (`:5`)
   intact (the R16 co-gate). Field count drops 12→11.
5. **`xtask/src/main.rs`** (`:184`) + **`xtask/src/regen_css.rs`** (`:51`) — DELETE the `emitter:` line
   from the JSON_TARGET and CSS TARGETS row literals.
6. **`lib.rs`** — DELETE the `RuntimeEmitterKind` re-export (`:19`); update the test contract builders
   (`:281-297`) to drop `emitter`.
7. **Regenerate** all grammar outputs (`cargo xtask regen-json` + `regen-css`); diff MUST be empty
   (PATH change, not OUTPUT change).

### 5.2 — Verification (the §6 exit-gate telemetry — the 5 conjuncts + 3 MUST-preserves)

| Conjunct | Telemetry column | Check |
|---|---|---|
| 1 | `emitter_fork_present` | `rg 'RuntimeEmitterKind\|CompiledLowering\|RequestFacts' codegen/src xtask/src` (test-excluded) == 0 ⇒ `false` |
| 2 | `generator_grammar_branch_count` | arm-census over `render`+`lower_for_runtime` body: `match grammar`/`grammar_name ==`/`"json"`/`"css"`/`"google_sheets"` == 0 ⇒ `0` |
| 3 | `generator_grammar_type_count` | no `*Emitter`-per-grammar type; only `LoweredArtifact{SinkOnly,Scan}` (shape-named) + pre-existing `SinkOnlyProgram`/`CssScanProgram` ⇒ `0` |
| 4 | `runtime_target_rows_collapsed` | `RuntimeTarget` still derives full-row `PartialEq` (`regen.rs:5`) after field removal, recursing into both nested structs ⇒ `true` |
| 5 | `emit_shape_source` | grep `render` body + `lower_for_runtime` body for `target.profile`/`target.emitter`/`target.output_labels`/`target.profile_contract`/`contract.emitter` == 0 ⇒ `lowered_program` (under the amended definition) |
| 6 | `dirty_generated_state` / `generated_md5_distinct` | `cargo xtask regen --check` exit 0; diff regenerated vs shipped == empty; no byte-identical pair across json/css_l4 ⇒ `clean` / `true` |
| 7 | `g3_json_hot_leaf_preserved` | `parse_object_value_at_direct`+`parse_array_element_at_direct` re-emitted with identical `inline(always)`+`sink.*` (byte-equivalent output proves this); JSON 51/51 strict-vs-sonic-rs held ⇒ `true` |
| 8 | `g3_css_sota_ratio_held` | same-run `track1_rich/lightningcss > 1.0×`, re-confirmed DIRECTIONALLY vs the G2-recorded `track1_rich_over_lcss_ratio_pre_g2` (NOT re-measured); byte-equivalent CSS output ⇒ ratio unchanged ⇒ `true` |

Because G3 is a PATH change with byte-equivalent OUTPUT, conjuncts 6/7/8 follow from the
byte-equivalence diff: if the generated files are identical, the JSON hot leaf and CSS scan bodies are
provably unchanged, so the >SOTA ratio and 51/51 guard are inherited from G1/G2-close. The HIGH risk
is conjunct 5's amendment defensibility, not the mechanical edit.

### 5.3 — Caps / reruns / revert / downstream

≤90 min wave wall, 30 min/redress (HIGH-risk: documented larger cap if §5-risk-1 seam fires). One full
gate refresh; a second requires a REDRESS cost note. Revert `runtime_generator.rs` +
`grammar_provider.rs` + `grammar_profile.rs` + `regen.rs` + `main.rs` + `regen_css.rs` + generated
output as ONE slice; REDRESS names WHICH conjunct failed + the relocated-seam witness if conjunct 5
fired. **G3 rejection BLOCKS G4, G6, PROVE.** Outcome `G`/`A`/`C` on close, `R` on miss.

### 5.4 — The §6 generality stress (deferred to PROVE, per G3.4)

G3's un-forked `render` must render recursive `CallRule`/`RepeatLoop` chains from `SinkOnlyExpr`
structure (`sink_only.rs:69-96`) with NO recursion-depth or rule-name special-case. Sheets does NOT
emit in G3; IF the un-forked path cannot render the Sheets precedence tower, the §6 finding surfaces
at PROVE (a named, `.bbnf`-invoked, (a)-(d)-gated precedence primitive), NOT here. G3's obligation is
only that the recursion comes from the neutral IR, never a special-case — which option (c) preserves
(the `SinkOnly` arm is the unchanged `emit_compiled` body; PROVE's Sheets will flow through the SAME
`SinkOnly` artifact arm, since Sheets, unlike CSS, has no `@token`/`@pretty`/`->` and CAN lower).

---

## APPENDIX — key evidence paths (all absolute)

- Fork: `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/runtime_generator.rs:16-27`
  (`emit_from_request` `match emitter`); `:29-72` (`emit_compiled`/JSON); `:74-122`
  (`emit_request_facts`/CSS).
- The tag: `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/grammar_provider.rs:33`
  (`RuntimeProfileContract.emitter`), `:39-43` (`enum RuntimeEmitterKind`), `:110-119` (the
  lowering-skip gate reading `emitter`).
- BackendShape: `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/ir/src/lib.rs:339-346` (the
  neutral 5-shape enum); `select_lowering` at
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/lower/mod.rs:19-27`.
- JSON artifact: `SinkOnlyProgram` + `RuntimePolicySummary.backend_shape` at
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/lower/sink_only.rs:20-53`.
- CSS artifact (NO BackendShape): `CssScanProgram` at
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/codegen/src/lower/css_scan.rs:104-140`; the
  raw-AST harvest rationale at `:1-26` and `:142-156`.
- CSS-doesn't-lower root: `first_unsupported` at
  `/Users/mkbabb/Programming/bbnf-lang/skinny/crates/grammar/src/lib.rs:79-83`; `unsupported` codes at
  `:214-236`; the lowering gate at `grammar_provider.rs:110`.
- R16 co-gate (already landed): `RuntimeTarget` derive at
  `/Users/mkbabb/Programming/bbnf-lang/skinny/xtask/src/regen.rs:5`; nested-struct derives at
  `grammar_provider.rs:45`/`:91`.
- xtask emitter tags to delete: `/Users/mkbabb/Programming/bbnf-lang/skinny/xtask/src/main.rs:184`
  (JSON), `/Users/mkbabb/Programming/bbnf-lang/skinny/xtask/src/regen_css.rs:51` (CSS).
- Two render entries: `json_sink_direct::render` at
  `.../crates/codegen/src/json_sink_direct/mod.rs:30`; `css_scan_direct::render` at
  `.../crates/codegen/src/css_scan_direct/mod.rs:58`.
- Amendment precedent: `restart/skinny/tranches/sk-v18/waves/g1/AMENDMENT.md`,
  `.../waves/g2/AMENDMENT.md`, `.../waves/g2/research.md §2` (the LOWERING GAP).
