# SK-V18 SPEC — PLAN PB: G1 (JSON projection) + G2 (CSS lowering) Wave Specs

Date: 2026-06-01. S-P3 synthesis-PLAN pass, packet PB. This is NOT an implementation dispatch;
it is the executable wave manifest for the two FIRST GENERALIZE waves of SK-V18 (the
backtrack-into-one-generator cycle). It folds the S-P2 converged candidate shortlist
(`research/p2/SYNTHESIS-RESEARCH.md` §1/§3/§4, rB §4, rC §4), the S-P0 audit-overfit addenda +
PRUNE-list (`audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1/§4/§5/§6), and the S-P1 profile
ground-truth (`research/p1/SYNTHESIS-PROFILE.md` §2/§3) into the per-wave entry/exit gates,
falsifiers, caps, telemetry columns, and named-primitive contracts for **G1** and **G2**. The
SK-V17 SPEC (`tranches/sk-v17/SPEC.md`) is the structure template. Host: aarch64 / Apple M5 Max
ONLY. No cargo run this pass.

Authority (binding inputs, re-grounded on disk this pass):
- `research/p2/SYNTHESIS-RESEARCH.md` §1 (R-C C1 → G1; R-B B⊃A → G2), §3 (the per-wave
  entry-gate predicates, lines 135-175), §4 (the §6 named-primitive (a)-(d) escape).
- `research/p2/rC-json-projection.md` §0-§6 (the G1 surface, the C1 recommendation, the
  diff-control gate, the leaf-scanner named-primitive discipline).
- `research/p2/rB-css-lowering.md` §0-§6 (the G2 surface, the B-wrapping-A recommendation, the
  `balanced_component_scan` primitive, the structural-alphabet-derivation gap).
- `audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md` §1 (the 6 addenda), §4 (P1-P5 PRUNE), §5
  (sequencing), §6 (the (a)-(c) escape + R16), §2.1 (the diff-control + witness-scan-root
  obligations).
- `research/p1/SYNTHESIS-PROFILE.md` §2 (JSON 91.5% hot leaf), §3 (CSS 94.1% scalar scan).

Disk re-verification this pass (every load-bearing surface confirmed at the cited line):
- `crates/codegen/src/json_sink_direct.rs:4` (`pub fn render(program: &SinkOnlyProgram)`),
  `:80/:97/:125/:252/:327/:368/:498` (the 7 fixed-literal `push_str` blocks, S-P0 R2),
  `:147/:187/:227` (the `parse_w11_1_number_{direct,object,array}_direct` metalang leak, P5),
  `:306` (the `b'-' | b'0'..=b'9'` digit fast-path split), `:336`
  (`match_tiny_plain_string_direct` tiny-string inline), `:457-461` (`render_number_emitter`'s
  `{prefix}` parameterization — the ONE proven-viable sink-prefix variation).
- `crates/codegen/src/runtime_generator.rs:17/:25` (the live `RuntimeEmitterKind`
  `CompiledLowering`/`RequestFacts` fork dispatch), `:91`
  (`("generated.rs", normalize(CSS_GENERATED_RS))` — the CSS const-`&str` courier splice),
  `:701` (`const CSS_GENERATED_RS: &str = r#"` — the ≈910-LOC verbatim blob, S-P0 R1).

Dispatch lock: this packet authorizes NO G1/G2 implementation from S-P3 itself. G1 is
conditionally gated on the P-cluster closing (P4 live); G2 dual-gates on G1 ∧ P3 closed ∧ P4
live. Each wave remains blocked until its predecessor closes AND the orchestrator/user dispatches
the wave triumvirate AND the entry gate below holds GREEN.

---

## §PB.0 — The two waves in the binding sequence (context)

Per `SYNTHESIS-RESEARCH.md` §3 and `SYNTHESIS-AUDIT-OVERFIT.md` §5, the standing order is
`PRUNE(P1..P5) → G1 → G2 → G3 → G4 → {G5/G6 ∥ PROVE} → H1`. PB covers the two FIRST GENERALIZE
waves:

```
P-cluster (P1..P5; P4 live BEFORE G2/G3)
  └─ G1  JSON projection (R-C C1)        entry: P-cluster closed (P4 live)
       └─ G2  CSS lowering (R-B B⊃A)     entry: G1 ∧ P3 closed ∧ P4 live   (DUAL gate)
            └─ (G3 un-fork — packet PC)
```

G1 is the FIRST generalize wave; its discipline (the `SinkOnlyExpr` AST-walk facts-bus + the
per-leaf (a)-(d) primitive escape) is INHERITED by G2 (`SYNTHESIS-RESEARCH.md` §2.2: "G2 reuses
G1's projecting-renderer pattern"). A G1 REDRESS BLOCKS G2/G3/G4/PROVE. A P3 REDRESS independently
BLOCKS G2 (the dual gate). Neither wave dispatches over a REDRESSed predecessor.

LOC budgets (per `[generated-size-budget]`; conjunctive with the hard cap):

| Wave | Section | Name | Initial dispatch status | Source/edit LOC budget | Impl/redress cap |
|---|---|---|---|---|---:|
| G1 | §PB.1 | JSON projection — `SinkOnlyExpr` AST-walk emitter (R-C C1) | Conditional on P-cluster close (P4 live) | ≤450 hand source/test/gate LOC; generated `json/generated.rs` named separately + diff-audited (must byte-match the pre-deletion oracle, so net generated delta ≈ 0; ±5% line delta SOFT tripwire only) | ≤90 min wave wall; 30 min per redress dispatch |
| G2 | §PB.2 | CSS lowering — `balanced_component_scan` primitive + fact-keyed projection (R-B B⊃A) | Conditional on G1 ∧ P3 close (P4 live) | ≤450 hand source/test/gate LOC; the new `lower/css_scan.rs` + `css_scan_direct.rs` + the `runtime_scan`/`runtime_simd` primitive shell + arg-derivation pass; generated `css_l4/generated.rs` named separately + diff-audited; the ≈910-LOC `CSS_GENERATED_RS` is DELETED (net generated delta accounted, not gate-keyed on the exact figure per §2.1.3) | ≤90 min wave wall; 30 min per redress dispatch |

Per `[dispatch-hard-cap]`: every dispatch carries "HARD CAP: N min. At 0.9N commit, at N halt"
(research 20 / plan 15 / redress 30). G2 is MED-HIGH (the structural-alphabet-derivation gap is
the campaign's single most-likely REDRESS, rB §5) and may carry a documented larger redress cap if
the arg-derivation pass under-delivers; the larger cap is recorded, not silent.

---

## §PB.1 — Wave G1: JSON projection (R-C C1 — `SinkOnlyExpr` AST-walk emitter)

### G1.0 — What G1 replaces (grounded)

`json/generated.rs` (1235 LOC) is the byte-concatenation of three sources assembled in
`runtime_generator.rs:29-37 emit_compiled` (rC §0):
1. `include_str!("json_templates/generated.rs")` — the tape recognizer oracle (391 LOC,
   `include_str!`-spliced, L1-clean AS a template).
2. `JSON_PARSE_ONLY_GENERATED_RS` const-`&str` courier (`runtime_generator.rs:195`) — a verbatim
   `_RS` blob (one of the 8 `_RS` couriers, L1 verbatim-blob hazard).
3. `json_sink_direct::render(sink_only)` — the SinkOnly direct-to-struct path whose 7
   fixed-literal `push_str` blocks (`render_header:80`, `render_entry:97`,
   `render_value_dispatch:125`, `render_container_rules:252`, `render_string_rule:327`,
   `render_number_rules:368`, `render_utility_rules:498`) carry the **91.52% hot leaf**
   (`parse_object_value_at_direct` 79.82% + `parse_array_element_at_direct` 11.70%, profile §2).
   The ONLY grammar-derived bytes today are the header comment interpolation (`:75`) and the
   number-emitter `{prefix}` (`:457-461`). Everything else is a hand-authored literal that merely
   HAPPENS to encode JSON's shape — the L1 "grammar-driven banner over hand-written body" REJECT.

G1's task (R-C C1, rC §4): make `json_sink_direct.rs` resemble the in-tree AST-walk emitters
`lower/tape_plan.rs::render_expr` and `json_typed_direct.rs` — a recursive emitter over the
`SinkOnlyExpr` tree (`lower/sink_only.rs:68-96`: Entry / Seq / Alt{mode,branches} /
RepeatLoop{body,min} / OptionalBranch / ByteLiteral(bytes) / RegexProgram{span_kind,pattern} /
CallRule{callee} / DirectBuild(DirectShape) / ValueProject / Return) — NOT a fixed-literal
stringifier. Fold the parse-only `_RS` courier (source #2) into the SAME walk so G1 retires BOTH
blob shapes, not one.

### G1.1 — Entry gate (the binding predicate; GREEN before dispatch)

Per `SYNTHESIS-RESEARCH.md` §3 line 160 + the lattice (§2.2):
- **P-cluster closed** (P1, P2, P3, P4, P5 all GREEN), **P4 live** specifically — the Lock-14
  green-by-exclusion fix MUST be live so the new emitter is neutrality-scanned AS authored.
- **P5 shares G1's exact surface** (rC §6): the `parse_w11_1_number_*` leak lives 7× inside
  `json_sink_direct.rs` (`:147/:187/:227` + the emitted defs). Either P5 lands first and G1
  re-emits the already-renamed names, OR G1's walk subsumes P5 by deriving the fn names from the
  `.bbnf` `number` rule (`parse_number_*`, not the hand-copied `w11_1` tag). The P5 falsifier
  (`grep -c parse_w11_1_number = 0`) MUST hold on the **G1-regenerated** `json_sink_direct.rs`, not
  a stale one. ENTRY REQUIRES P5 closed; EXIT re-asserts it on the regenerated file.
- **P4's `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_, _RS, EventGrammar, *EventGrammar}`** live, so the
  JSON `_RS` courier G1 retires is caught at its emit site.

### G1.2 — Exit gate (MEASURABLE; the binding proof, NOT a line delta)

Per `SYNTHESIS-RESEARCH.md` §3 lines 160-166 + rC §2:

1. **Byte-equivalence diff-control (the BINDING proof, §2.1.1 / CH7).** The same-wave regen MUST
   byte-match the regenerated `json/generated.rs` against (a) the `json_templates/` byte-for-byte
   oracle AND (b) the current shipped `generated.rs`, BEFORE the oracle is deleted. Mechanism: the
   existing `EmittedSource::check_dir` (`lib.rs:74`, exact `actual != *source` comparison) +
   the `emission_is_deterministic` / `direct_parser_is_authored_from_sink_only_lowering` tests
   (`lib.rs:481-621`). **The ±5% line-count delta is a SOFT tripwire ONLY** — a faithful
   projection may legitimately reorder/dedupe past it; a line-delta tripwire is advisory, NEVER a
   REJECT.
2. **The `.bbnf`-mutation falsifier (proves derivation, not re-stringification).** Mutate
   `grammar/json/json.bbnf` (canonical: drop the `bool` rule), regen, the emitted dispatch MUST
   lose the `b't'`/`b'f'` arms. A fixed body fails this (the arms persist) → REJECT. Revert the
   mutation after the falsifier fires.
3. **Hot-leaf preservation (the 91.5% MUST-preserve, mechanical).** The regenerated
   `generated.rs` still contains `fn parse_object_value_at_direct` (and
   `parse_array_element_at_direct`) with **identical `#[inline(always)]`/`#[inline(never)]` cfg
   shape** and the **same `sink.object_*`/`sink.array_*` call sites** (profile §2). The
   monomorphized-sink byte-dispatch shape is preserved — no devirtualization/indirection
   regression. **Do NOT LCD-collapse the value/object/array dispatch triple** (they differ only by
   sink prefix — parameterize per `render_number_emitter`'s `{prefix}`, do NOT unify; an LCD-unify
   erases the monomorphized-sink leaf and regresses the 91.5% — rC §5 secondary risk).
4. **`verbatim_blob_present == false`** (addendum 1). BOTH the 7 SinkOnly `push_str` literals AND
   the parse-only `_RS` courier (source #2) are folded into the derived walk. Falsifier: grep the
   regenerated codegen path for `r#"..."#` blob bodies in `json_sink_direct.rs` ⇒ 0; the
   `JSON_PARSE_ONLY_GENERATED_RS` const ⇒ deleted.
5. **P5 re-assertion (on the regenerated file).** `grep -c parse_w11_1_number` on the
   G1-regenerated `json_sink_direct.rs` == 0; no `w[0-9]+`/corpus/`sk_v` tag in the shipped JSON
   runtime; `regen --check` clean.

### G1.3 — The leaf-scanner named primitives (the §6 (a)-(d) gated escape)

Per `SYNTHESIS-RESEARCH.md` §4 (the SECONDARY §6 finding) + rC §3/§4/§5: the structural SKELETON
(dispatch match, container loops, literal arms, the 3 sink-prefix variants) is **walk-derived** by
C1. Only the proven-hot inner LEAF kernels stay byte-stable as named, `.bbnf`-INVOKED,
machine-gated primitives. The two G1 leaf primitives:

- **`decode_json_string_to_arena`** — invoked by the `.bbnf` `string` rule
  (`string = /.../ -> decode_json_string_to_arena`). Covers the `match_tiny_plain_string_direct`
  tiny-string inline fast-path (`json_sink_direct.rs:336`) + the `unescape_string` Cow-borrow
  (profile §2 leaf #3, keep the `\`-free borrow). PROFILE ANCHOR: leaf #3 (3.45%) + the inline in
  the 91.5% body.
- **`parse_number_*`** — invoked by the `.bbnf` `number` rule (`number = /.../ -> f64`). Covers
  the `b'-' | b'0'..=b'9'` digit fast-path array split (`:306`) + `materialize_u64`
  (profile §2 leaf #8). PROFILE ANCHOR: the digit fast-path inside the 91.5% body + leaf #8 (0.53%).

**Per-primitive contract (each primitive MUST pass ALL FOUR or it is a relabeled blob = REJECT):**

| gate | predicate | G1 leaf falsifier |
|---|---|---|
| **(a) grammar-INVOKED by name** | the primitive is reached via a `.bbnf` `->` invocation, not a hard-coded call | grep the `.bbnf`: `string`/`number` rule carries `-> decode_json_string_to_arena` / `-> parse_number_*`; the emitter reads the callee FROM the rule's `->`, not a literal |
| **(b) emitted-output VARIES under invoking-rule mutation (the BYTE-SET / NUMERIC-CLASS mutation)** | mutate the INVOKING rule's class in the `.bbnf` → the emitted kernel's byte/class literal changes | **WIDEN the `number` rule's digit class in `.bbnf` → the `b'0'..=b'9'` literal in the emitted `parse_number_*` kernel WIDENS.** A kernel that does NOT vary under its own rule's class mutation is a relabeled fixed courier even though the surrounding skeleton varies (the byte-equivalence gate alone is satisfiable by routing the SAME literal through the new walk — (b) is what distinguishes a derived leaf from a relabeled one, §4) |
| **(c) `verbatim_blob_present == false`** | the primitive body is emitted from grammar facts, not a `r#"..."#` splice | no `_RS`/raw-string body for the leaf in the regenerated codegen |
| **(d) PROFILE-PROVEN-NARROW-LEAF** | the primitive covers a SINGLE hot leaf attributable to a named S-P1 hot leaf; the surrounding skeleton is walk-derived | machine-check: primitive LOC ≤ the profiled hot-leaf extent (string-scan / digit-scan kernels only, NOT the dispatch/loop skeleton); a primitive spanning a rule's whole body or an unprofiled region is REJECT regardless of (a)-(c) |

Per `SYNTHESIS-RESEARCH.md` §4: (a)-(b)-(c) prove the primitive is grammar-coupled; (d) bounds its
SIZE so the escape cannot admit an arbitrarily large relabeled blob that merely varies under
mutation. **Do NOT LCD-unify the value/object/array dispatch triple** (rC §5) — that is structural,
NOT a leaf primitive, and unifying it regresses the monomorphized-sink leaf.

### G1.4 — Telemetry columns (emitted AND consumed by `gate-json` in the G1 slice)

Per `SYNTHESIS-AUDIT-OVERFIT.md` §6 (the `--skv18-generalization-report` consumer) +
`typed-materialization-invariant` (every emitted field consumed same-wave or the wave fails):

```text
g1_json_generated_byte_equivalent        (regenerated == oracle == shipped, pre-deletion; the BINDING proof)
g1_bbnf_mutation_falsifier_fires          (drop bool ⇒ b't'/b'f' arms vanish; revert)
g1_hot_leaf_preserved                     (parse_object_value_at_direct + parse_array_element_at_direct: same inline cfg + sink.* call sites)
g1_dispatch_triple_not_lcd_collapsed      (value/object/array remain 3 sink-prefix variants, not unified)
verbatim_blob_present                     (false; both SinkOnly literals AND parse-only _RS folded)
g1_leaf_primitive_count                   (2: decode_json_string_to_arena, parse_number_*)
g1_leaf_primitive_abcd_pass               (per-primitive (a)∧(b)∧(c)∧(d) all green)
g1_metalang_leak_count                    (parse_w11_1_number on REGENERATED file == 0)
g1_json_guard_rows_held                   (51/51 JSON rows A/GO strict same-plane; throughput within ±1.0% of baseline)
line_delta_vs_oracle                      (SOFT tripwire only; advisory, never REJECT)
```

The `gate-json` consumer REJECTS the G1 row if: `g1_json_generated_byte_equivalent != true`;
`g1_bbnf_mutation_falsifier_fires != true`; `g1_hot_leaf_preserved != true`;
`verbatim_blob_present != false`; any `g1_leaf_primitive_abcd_pass` arm false;
`g1_metalang_leak_count != 0`; `g1_json_guard_rows_held != true`. `line_delta_vs_oracle` does NOT
gate (SOFT tripwire).

### G1.5 — Caps, reruns, revert, downstream

- **Cap:** ≤90 min wave wall; 30 min per redress dispatch ("HARD CAP: 30 min. At 27 commit, at 30
  halt").
- **Rerun ceiling:** one full gate refresh (byte-equiv re-run + mutation-falsifier + hot-leaf grep +
  JSON 51/51 maintain); a second requires a REDRESS cost note. Extra reruns are REDRESS evidence,
  not retry room.
- **Pre-blocked routes:** any JSON throughput regression on the 91.5% leaf; an LCD-unify of the
  dispatch triple; a relabeled fixed fragment masquerading as a leaf primitive (fails (b) or (d));
  trading the SinkOnly blob for the `_RS` blob (only one folded); a courier-swap that passes
  byte-equivalence but fails the `.bbnf`-mutation falsifier.
- **Revert protocol:** revert the G1 codegen + regenerated-output commits together, restore the
  shipped `json/generated.rs` + the `json_templates/` oracle, record a G1 REDRESS rejection naming
  the failed gate (byte-equiv / mutation / hot-leaf / primitive-(b) / metalang). Hardening docs
  remain as history.
- **Downstream effect:** **G1 REJECTION BLOCKS G2, G3, G4, PROVE** (the un-forked emitter consumes
  G1's grammar-walk pattern; G2 inherits G1's facts-bus discipline; Sheets emits THROUGH the
  un-forked generator). No downstream wave dispatches over a REDRESSed G1.

---

## §PB.2 — Wave G2: CSS lowering (R-B B⊃A — `balanced_component_scan` primitive + fact-keyed projection)

### G2.0 — What G2 replaces (grounded)

The CSS provider is emitted by `emit_request_facts` (`runtime_generator.rs:76-103`); for
`generated.rs` it does `normalize(CSS_GENERATED_RS)` (`:91`) — a pure splice of
`const CSS_GENERATED_RS: &str = r#"..."#` (`:701`→ terminator, ≈910 LOC, S-P0 R1/L1 witness). The
`.bbnf` is **never consumed** by this path: `emit_from_request` (`:12-27`) routes
`RuntimeEmitterKind::RequestFacts` (`:25`) straight to `emit_request_facts`; only
`CompiledLowering` (JSON, `:17`) reaches `lower::lower_to_rust`. **CSS does not touch `lower/` at
all today** (rB §0).

The load-bearing parser inside the courier (rB §0):
- `CssFullParser` (`:1125-1467`): a flat balanced-delimiter recognizer, NOT a recursive-descent
  walk of the rule tree. Pushes structural offsets to the EXISTING offset tape; recovers node kind
  lazily from the source byte + sparse `AT_RULE_FLAG`.
- The **94.1% hot leaf**: `find_component_delim` (`:1357-1380`, 79.5% parser self-time) +
  `consume_balanced_at` (`:1393-1413`, 14.6%) — the scalar byte-at-a-time scan over the delimiter
  set `{ } ; :` plus string/comment skips and nested `()[]{}` balancing.
- The lazy rich projection (preserve-rich-ast, the >SOTA product): `CssNode`/`CssRule`/
  `CssDeclaration`/`CssTypedValue` (`:744-954`), `CssDocument::rich_summary` (`:958-1035`), the
  rich-plane classifier `CssTypedValue::classify` (`:929-953`).

**The deep finding (rB §1, §4, frames everything):** the grammar describes a RICH RECURSIVE CSSOM,
but the >SOTA recognizer is a STRUCTURE-RECOVERING DELIMITER SCAN that deliberately does NOT
recurse the rule tree. A naive grammar-walk lowering (one parse-fn per rule) produces the
combinator-shaped recursive descent (lightningcss's own architecture) that **categorically
regresses >SOTA** (CSS currently beats lightningcss 1.66-3.38×). So G2 is NOT "walk the grammar
IR"; it derives the delimiter-scan recognizer FROM grammar-supplied data (alphabet, structural-byte
set, branch tags, entry rule), emitting the scan SHAPE the profile attributes the win to — the JSON
analog (`json_sink_direct::render` is template-keyed by grammar facts, NOT an IR tree-walk). The
full grammar-IR tree-walk (R-B Candidate C) is REJECTED outright.

G2's recommendation (R-B B⊃A, rB §4): a **hybrid** — the delimiter-scan recognizer CORE lands as
the **`balanced_component_scan` grammar-parameterized NAMED PRIMITIVE** (the §6 honest-finding,
gated (a)-(d)); the drivers + lazy rich projection land as **grammar-fact-keyed `push_str` emit
blocks** (Candidate A, inheriting G1's facts-bus discipline).

### G2.1 — Entry gate (the DUAL binding predicate; GREEN before dispatch)

Per `SYNTHESIS-RESEARCH.md` §3 line 167 + the lattice (§2.2) + rB §6 — **G2 dual-gates; a P3
failure blocks G2 INDEPENDENT of G1**:

- **G1 closed.** G2 reuses G1's projecting-renderer discipline (the facts-bus + emit-block pattern
  that retires the fixed literals). "The projecting renderer must exist first"
  (`a2-prune-sequencing.md:511`). A G1 REDRESS BLOCKS G2.
- **P3 closed (the independent conjunct).** The 7 `css_l4_*` rows (all `grammar_name:"css_l4"`,
  `entry_rule:"stylesheet"`, `source_roots: CSS_L4_ROOTS`, differing ONLY in
  `profile`/`output_dir`/`output_labels`) must collapse to ONE CSS config + the `RuntimeTarget`
  row-collapse (`RuntimeTarget: PartialEq` full-row, R16) BEFORE G2 derives CSS — else G2
  re-derives the SAME scan into 7 byte-identical files and re-creates the replica overfit
  (addendum 2). The G6 WIRE then consumes the P3-collapsed SINGLE CSS scan; collapse-first is
  mandatory or the NEON re-forks 7 ways (`SYNTHESIS-PROFILE.md` §3).
- **P3 ∧ P4 conjunct:** `runtime_target_rows_collapsed == true` (R16 full-row `PartialEq`) AND
  P4's `FORBIDDEN_GENERIC_TOKENS ⊇ {CSS_, _RS}` live so the CSS const courier G2 retires is caught
  at its emit site. P4 MUST be live BEFORE G2 (the un-forked CSS emitter is neutrality-scanned AS
  authored).

### G2.2 — Exit gate (MEASURABLE)

Per `SYNTHESIS-RESEARCH.md` §3 lines 167-175 + rB §4/§5:

1. **CSS `generated.rs` grammar-DERIVED.** `verbatim_blob_present == false`; the
   `CSS_GENERATED_RS` grep == 0 (the const courier deleted, `runtime_generator.rs:701` gone). The
   ≈910-LOC courier LOC is cohort-carried, NOT gate-keyed on the exact figure (§2.1.3); the binding
   gate is `verbatim_blob_present == false` + the mutation test.
2. **The `balanced_component_scan` named primitive passes the per-primitive (a)-(c) mutate
   falsifier:** mutate the INVOKING `.bbnf` rule (the `stylesheet`/`ruleBlock`/`declaration`
   structural shape) → the emitted ARG byte sets change (the delimiter set, the open/close pairs,
   the comment/string skip flags). A primitive whose args do NOT vary under the `.bbnf` mutation is
   a relabeled blob = REJECT. **PLUS (d) PROFILE-PROVEN-NARROW-LEAF:** the primitive covers the
   single 94.1% scalar-scan hot leaf; its shell LOC ≤ the profiled extent; the drivers + projection
   are fact-keyed-emitted around it, NOT inside it.
3. **9-field cssparser oracle CORRECTNESS parity held** (gate-before-speed). The emitted CSS scan's
   typed summary equals the cssparser same-workload typed summary EXACTLY across the 4 benched
   corpora. An incomplete structural-alphabet derivation diverges from the oracle (parity REJECT) —
   this is the make-or-break (rB §5, the structural-alphabet-derivation gap).
4. **The EXPLICIT >SOTA-regression gate (distinct from parity).** `track1_rich/lightningcss >= the
   S-P1 ratio` on `css_canon_bench` — COLD, **corpus-in-timer**, the P2-survivor cold/real-corpus
   harness, absolute figures inheriting §5-risk-7's QUIET-recapture caveat. The S-P1 per-corpus
   ratios are the floor: bootstrap 2.190, tailwindcss 3.375, material-components-web 1.658 (min),
   animate 2.101 (`SYNTHESIS-PROFILE.md` §1). G2 RE-DERIVES the 94.1% scan, so oracle parity
   (correct output) does NOT prove throughput preservation — **the bench re-measurement is the
   binding regression falsifier.** The ratios are load-robust (invariant to uniform host slowdown,
   §5-risk-7); absolute Mbps is DIRECTIONAL and not re-locked.
5. **The `balanced_component_scan` NEUTRALITY-PROOF obligation (CH6, §4).** The primitive is named
   NEUTRALLY but is exercised ONLY by CSS in this campaign. Its inner alphabet-scan sub-kernel (the
   `bbnf-simd` eq-set member scan) is genuinely neutral (caller-supplied byte set), but the
   **balanced-recognizer SHELL** must be PROVEN neutral by at least one NON-CSS invocation in this
   campaign — the JSON object/array balanced `{}`/`[]` nesting OR the Sheets `paren_expr` balancing
   must invoke the SAME primitive. **ELSE it is demoted to an honestly CSS-scoped name
   (`css_balanced_component_scan`), not a false neutral** — a neutrally-named CSS-only primitive is
   an overfit-in-waiting. G2 declares which non-CSS invocation discharges the obligation OR adopts
   the CSS-scoped name; the gate REJECTS a neutral name with zero non-CSS caller in the campaign.

### G2.3 — The `balanced_component_scan` named primitive (the PRIMARY §6 finding; the (a)-(d) gated escape)

Per `SYNTHESIS-RESEARCH.md` §4 (the PRIMARY §6 finding) + rB §2/§4 + `SYNTHESIS-PROFILE.md` §3: the
94.1% hot leaf is a flat balanced-delimiter recognizer whose delimiter alphabet (`{}:;`) and
structural-byte dispatch (`' " / ( [ {`) are EMERGENT from the rule shapes, modeled by NO
`SinkOnlyExpr` node (`lower/sink_only.rs:62-103` has no balanced-component-scan node). The honest
path: the balanced scan lands as the grammar-parameterized `balanced_component_scan` named
primitive, grammar-INVOKED, taking grammar-DERIVED byte-set ARGS, with a per-primitive
mutate-falsifier. **This primitive is ALSO the G6 NEON-retarget call site — ONE seam for G2+G6**
(no orphan kernel, no per-grammar re-emit; the WIRE consumes the P3-collapsed SINGLE scan).

**The named-primitive contract (the same (a)-(d) machine gate as G1's leaves; ALL FOUR or REJECT):**

| gate | predicate | G2 primitive falsifier |
|---|---|---|
| **(a) grammar-INVOKED by name** | the emitted CSS scan reaches the primitive via a `.bbnf`-derived call, not a hard-coded body | the emitter reads the entry rule + structural-byte set FROM `stylesheet.bbnf`'s block/declaration/at-rule shape (the new `lower/css_scan.rs` arg-derivation pass), not a literal; the call site names `balanced_component_scan(...)` with grammar-derived args |
| **(b) emitted ARGS VARY under invoking-rule mutation** | mutate the `.bbnf` `stylesheet`/`ruleBlock`/`declaration` rule → the emitted ARG byte sets (delimiter set, open/close pairs, skip flags) change | **mutate the invoking rule's structural-byte shape in `stylesheet.bbnf` → the emitted delimiter byte-array constant changes.** An arg keyed only off a decorative parameter FAILS (b) and is a relabeled blob (rB §2 cons-a, the single largest paper-close surface, R-A0-3) |
| **(c) `verbatim_blob_present == false`** | the primitive call site + arg constants are emitted from grammar facts; the kernel BODY lives in the shared `runtime_scan`/`runtime_simd` surface, NOT a `r#"..."#` splice | `CSS_GENERATED_RS` grep == 0; the scan body is the shared primitive, the emitter authors only the call + arg constants |
| **(d) PROFILE-PROVEN-NARROW-LEAF** | the primitive covers the SINGLE 94.1% scalar-scan hot leaf; it stays a NARROW balanced-scan with grammar-supplied byte sets, NOT a CSS-aware mega-function (rB §2 cons-b, the god-kernel risk) | machine-check: primitive shell LOC ≤ the profiled 94.1% extent (`find_component_delim` + `consume_balanced_at`); the drivers (`parse_block`/`parse_declaration` shells ≤4.2%) + the lazy projection (`CssNode`/`CssRule`/`classify`) are fact-keyed-emitted AROUND it (Candidate A), NOT absorbed into it; a primitive spanning a rule's whole body is REJECT |

**The arg-derivation pass (the G2 make-or-break, rB §5).** G2 adds a new `lower/css_scan.rs`
analysis that DERIVES the structural alphabet (which bytes open/close blocks, which terminate
declarations, the comment/string skip set, the `:`-split, the at-rule-vs-qualified branch tags from
the `AT_RULE_FLAG` projection) FROM `stylesheet.bbnf`'s block/declaration/at-rule structure,
producing a `CssScanProgram` (the CSS analog of `SinkOnlyProgram`). `css_scan_direct::render`
(sibling of `json_sink_direct::render`) emits the recognizer + lazy projection as `push_str` blocks
PARAMETERIZED by those facts. **If the derivation is incomplete/wrong, the emitted scan either (a)
diverges from the 9-field oracle (parity REJECT) OR (b) is hand-patched back to match (collapses to
a verbatim blob, L1 REJECT).** The mitigation IS Candidate B: derive only the ARG byte sets (a far
smaller, more tractable derivation), keep the scan ALGORITHM in the named primitive — but then (b)
MUST prove the args vary, or it is a relabel. This is the single most likely place G2 REDRESSes.

### G2.4 — The lazy rich projection emit blocks (Candidate A, inheriting G1's discipline)

Per rB §4: the drivers (`parse_block`/`parse_declaration` shells) + the lazy rich projection
(`CssNode`/`CssRule`/`CssDeclaration`/`CssTypedValue::classify` derived from the grammar's
at-rule/declaration/value-class facts) land as grammar-fact-keyed `push_str` emit blocks — the same
template-keyed-by-grammar-facts pattern G1 lands, NOT a generic IR tree-walk. The
addendum-4 LCD-flatten co-gate (rB §5 secondary, deferred to G4 but tracked here): G2's projection
emit MUST keep CSS's `CssRule::selector_count`/`CssDeclaration::typed_value` rich API intact and
NOT flatten it toward a JSON common denominator (`json_rich_navigation_preserved == true` is the
G4 co-gate; G2 must not foreclose it).

### G2.5 — Telemetry columns (emitted AND consumed by `gate-json` in the G2 slice)

```text
g2_css_generated_grammar_derived          (verbatim_blob_present == false; CSS_GENERATED_RS grep == 0)
g2_balanced_scan_primitive_abcd_pass      ((a)∧(b)∧(c)∧(d) all green)
g2_balanced_scan_arg_mutation_fires       (mutate stylesheet.bbnf structural-byte shape ⇒ emitted delimiter byte-array changes; revert)
g2_balanced_scan_neutrality_discharged    (≥1 non-CSS invocation OR demoted to css_balanced_component_scan)
g2_cssparser_oracle_parity                (EXACT 9-field, 4/4 corpora; gate-before-speed)
g2_sota_ratio_held                        (track1_rich/lightningcss >= S-P1 ratio per corpus; cold, corpus-in-timer)
g2_sota_ratio_floor                       (bootstrap 2.190 | tailwind 3.375 | mcw 1.658 | animate 2.101)
g2_corpus_in_timer                        (true; the P2-survivor cold/real-corpus css_canon_bench plane)
g2_css_rich_projection_not_flattened      (CssRule/CssDeclaration rich API intact; G4 co-gate not foreclosed)
g2_css_replica_singular                   (P3-collapsed single CSS config; not re-derived 7×)
verbatim_blob_present                     (false)
runtime_target_rows_collapsed             (true; R16 full-row PartialEq, P3 conjunct re-asserted)
```

The `gate-json` consumer REJECTS the G2 row if: `g2_css_generated_grammar_derived != true`;
any `g2_balanced_scan_primitive_abcd_pass` arm false; `g2_balanced_scan_arg_mutation_fires != true`;
`g2_balanced_scan_neutrality_discharged != true`; `g2_cssparser_oracle_parity != true` (gate before
ANY speed admission); `g2_sota_ratio_held != true`; `g2_corpus_in_timer != true`;
`verbatim_blob_present != false`; `runtime_target_rows_collapsed != true`. The absolute-Mbps figures
carry the §5-risk-7 QUIET-recapture caveat (DIRECTIONAL until H1's quiet re-capture); the
load-robust RATIO is the binding gate.

### G2.6 — Caps, reruns, revert, downstream

- **Cap:** ≤90 min wave wall; 30 min per redress dispatch; G2 is MED-HIGH (the
  structural-alphabet-derivation gap) and may carry a documented larger redress cap if the
  arg-derivation pass under-delivers — the larger cap is RECORDED, not silent.
- **Rerun ceiling:** one full gate refresh (oracle parity + the >SOTA ratio re-measure + the
  arg-mutation falsifier + the neutrality check); a second requires a REDRESS cost note. Extra
  reruns are REDRESS evidence.
- **Pre-blocked routes:** the full grammar-IR tree-walk (R-B Candidate C — the combinator-shaped
  descent that regresses >SOTA); a hand-patched scan body that passes the oracle but fails the
  arg-mutation falsifier (the L1 relabel); a neutrally-named CSS-only primitive with zero non-CSS
  caller (the false-neutral overfit — demote or add a caller); a god-kernel that absorbs the
  drivers/projection into the primitive (fails (d)); re-deriving the scan into 7 byte-identical
  files (the P3 re-fork); any >SOTA admission on a non-corpus-in-timer or warm/micro-fixture plane;
  a `track1_rich/lightningcss` below the S-P1 ratio reported as a pass.
- **Revert protocol:** revert the G2 `lower/css_scan.rs` + `css_scan_direct.rs` + primitive-shell +
  regenerated-output commits together, restore the `CSS_GENERATED_RS` courier + the 7 (now
  P3-collapsed) css_l4 configs, record a G2 REDRESS rejection naming the failed gate
  (oracle-parity / >SOTA-ratio / arg-mutation / neutrality / verbatim-blob). If the
  arg-derivation pass is the failure, the REDRESS records the structural-alphabet-derivation gap as
  the named residual (rB §5) — do NOT paper-close with a hand-patched blob.
- **Downstream effect:** **G2 REJECTION BLOCKS G3, G4, G6, PROVE** (G3 un-forks the emitter G2
  feeds; G6 wires the NEON into the `balanced_component_scan` primitive G2 lands; G4 shares the
  trait over the CSS/JSON projections; Sheets emits THROUGH the un-forked generator). No downstream
  wave dispatches over a REDRESSed G2.

---

## §PB.3 — The shared Lock-14 / generality exit gate (both waves)

Per the SK-V17 template §2.1 + `SYNTHESIS-AUDIT-OVERFIT.md` §2.1.2 (the P4 witness-scan-root
coupling): both G1 and G2 carry this exit gate.

- **Public-API scan:** no new public JSON-named (G1) or CSS-named (G2) API in generic crates.
- **Grammar-branch scan:** no generic branch selects behavior by grammar name; `generator_grammar
  _branch_count == 0`, `generator_grammar_type_count == 0` (addendum 2's 3-co-gate conjunction).
- **Relocated-seam guard:** the emitter reads output-shape from the LOWERED PROGRAM
  (`program.policy_summary.backend_shape`), NEVER from a `RuntimeTarget` field — this is the G3
  exit conjunct (`emit_shape_source == lowered_program`) but G1/G2 must NOT introduce a
  `target.profile`-keyed branch that G3 would then have to un-fork. `runtime_target_rows_collapsed
  == true` (R16 full-row `PartialEq`) catches the relocated seam the arm-census grep cannot see
  (§5-risk-1).
- **Witness-emission scan-root coupling (P4):** if the G1/G2 emitter emits any grammar-named
  `EventGrammar` literal into the generated runtime, the `runtime_generator.rs`-scoped
  `FORBIDDEN_GENERIC_TOKENS` (extended by P4 with `CSS_`/`_RS`/`EventGrammar`/`*EventGrammar`)
  catches it at the emit site. (G1 retires the JSON `_RS` couriers; G2 retires the CSS courier.)
- **The (a)-(d) §6 escape is the ONLY admissible hand-shaping** — every named primitive (G1's 2
  leaves, G2's balanced scan) is gated (a) grammar-INVOKED + (b) emitted-output-VARIES-under-rule-
  mutation + (c) `verbatim_blob_present == false` + (d) PROFILE-PROVEN-NARROW-LEAF. A primitive
  failing any arm is a relabeled hand-written blob = REJECT.

---

## §PB.4 — Cross-wave residual-risk register (the S-P2 §5 risks G1/G2 plan around)

| risk (S-P2 §5) | wave | mitigation bound into the gate above |
|---|---|---|
| 3. Hot-leaf byte-equivalence under the AST-walk | G1 | byte-equivalence diff-control (G1.2.1) + per-leaf (a)-(b)-(c)-(d) machine gate; do NOT LCD-unify the dispatch triple (G1.2.3 / G1.3) |
| 2. The structural-alphabet-derivation gap (the G2 make-or-break) | G2 | Candidate B narrows derivation to the ARG byte sets; the (b) arg-mutation falsifier proves they vary; an incomplete derivation REDRESSes honestly, never hand-patches to a blob (G2.3) |
| 1. The relocated seam (sharpest risk) | G1+G2 | `runtime_target_rows_collapsed == true` (R16 full-row `PartialEq`) + emitter reads shape from the lowered program, not `target.profile` (§PB.3) |
| 7. Load-depressed absolute Mbps | G2 | the >SOTA gate keys on the load-robust RATIO (`track1_rich/lightningcss >= S-P1 ratio`), not absolute Mbps; absolute carries the QUIET-recapture caveat deferred to H1 (G2.2.4) |

---

**Deliverable status:** G1 + G2 wave specs complete — entry/exit gates, falsifiers, caps,
telemetry, and the named-primitive (a)-(d) contracts bound to the S-P2 sequencing
(`SYNTHESIS-RESEARCH.md` §3/§4) and the rB/rC recommendations. Next packet: PC (G3 un-fork emitter
+ R16 row-collapse). Ready-for-S-P3-consolidation.
