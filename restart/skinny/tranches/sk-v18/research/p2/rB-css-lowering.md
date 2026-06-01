# SK-V18 S-P2 / R-B — CSS Lowering from `.bbnf` (Class R-B, wave G2)

RESEARCH pass. No cargo run. Every claim grounded in code at the live tree
(`/Users/mkbabb/Programming/bbnf-lang/skinny/crates/...`, cwd worktree). Cited
`path:line` are the codegen-courier line numbers inside
`crates/codegen/src/runtime_generator.rs` and the runtime substrate. Bound by the
6 S-P0 addenda (`SYNTHESIS-AUDIT-OVERFIT.md §1`, `a1-six-addenda-lens-registry.md`)
and the S-P1 profile ground-truth (`SYNTHESIS-PROFILE.md §3`).

---

## 0. The exact surface G2 must replace (grounded)

The CSS provider is emitted by `emit_request_facts` (`runtime_generator.rs:76-103`).
For `generated.rs` it does **`normalize(CSS_GENERATED_RS)`** (`:91`) — a pure splice of
a `const CSS_GENERATED_RS: &str = r#"..."#` raw-string body running
`:701`→ the `"#;` terminator (≈910 LOC, L1 witness). The `.bbnf` is **never consumed**
by this path: `emit_from_request` (`:12-27`) routes `RuntimeEmitterKind::RequestFacts`
straight to `emit_request_facts`; only `RuntimeEmitterKind::CompiledLowering` (JSON)
reaches `crate::emit_from_source` → `passes::compile` → `lower::lower_to_rust`
(`lib.rs:107-118`, `:154-179`). **CSS does not touch `lower/` at all today.**

The body that must become grammar-derived (the load-bearing parser):

- `CssFullParser` (`:1125-1467`): a **flat balanced-delimiter recognizer**, NOT a
  recursive-descent walk of the grammar's rule tree. It pushes structural offsets into
  the EXISTING offset tape (`push_offset`/`push_plain_offset`, `:1144-1158`) and recovers
  node kind lazily from the source byte + sparse `AT_RULE_FLAG`
  (`CssNodeKind::at_cursor`, `:723-740`).
- The 94.1% hot leaf: `find_component_delim` (`:1357-1380`, 79.5% parser self-time) +
  `consume_balanced_at` (`:1393-1413`, 14.6%) — the same scalar byte-at-a-time scan over
  the delimiter set `{ } ; : ( [` plus string/comment skips. `parse_at_rule` (`:1179`),
  `parse_qualified_rule` (`:1210`), `parse_block` (`:1228`), `parse_block_item` (`:1248`),
  `parse_declaration` (`:1280`) are the thin drivers (≤4.2% combined).
- The lazy rich projection (preserve-rich-ast, the >SOTA product): `CssNode`/`CssRule`/
  `CssDeclaration`/`CssTypedValue` (`:744-954`), `CssDocument::nodes`/`summary`/`rich_summary`
  (`:958-1035`), and the rich-plane value-head classifier `CssTypedValue::classify`
  (`:929-953`) + `number_has_unit` (`:1557`) / `leading_ident_is_function` (`:1583`) /
  `scan_value_end` (`:1517`) — the S-P1-flagged second hot candidate once the scan is NEON'd.

**The deep finding (frames all candidates):** the grammar `stylesheet.bbnf` describes a
RICH RECURSIVE CSSOM (`qualifiedRule = selectorList , ruleBlock`; `mediaRule`; `keyframesRule`;
`genericAtRule`; `declaration` via `properties.bbnf:161-166`), whereas the >SOTA recognizer
is a STRUCTURE-RECOVERING DELIMITER SCAN that deliberately does NOT recurse the rule tree —
it scans to the next `{`/`}`/`;`/`:` and re-derives kind from bytes. A naive grammar-walk
lowering (one parse-fn per rule, à la `selectorList`→`complexSelector`→...) would produce a
**combinator-shaped recursive descent that is categorically slower** than the delimiter scan
and would regress >SOTA. So G2 is not "walk the grammar IR"; it is "derive the delimiter-scan
recognizer FROM grammar-supplied data (alphabet, structural-byte set, branch tags, entry rule),
emitting the scan SHAPE the profile attributes the win to." This is exactly the JSON analog:
`json_sink_direct::render` does NOT walk `SinkOnlyExpr` to emit bodies — it `push_str`s a fixed
fused byte-dispatch shape parameterized by `program.entry_rule`/`literals`/`direct_shapes`
(`json_sink_direct.rs:68-95, 124-249`). G2 must follow the same template-keyed-by-grammar-facts
pattern, not a generic IR tree-walk.

The JSON analog is itself **only partially derived** today (R2/G1's concern): the bodies of
`parse_object_direct`/`parse_string_direct`/etc. are verbatim raw strings; only the dispatch
arms (keyed off `config::*_LITERAL`), the header (entry/shapes), and the number emitter (sink
prefix) vary. G1 must make JSON fully projection-driven first; G2 inherits whatever facts-bus +
emit-block discipline G1 lands (sequencing: G2 entry-gates on G1, `a2-prune-sequencing.md:321`).

---

## 1. Candidate A — Grammar-fact-keyed scan emitter (the JSON-render analog, lifted to CSS)

**Shape.** Add a CSS lowering to `lower/` mirroring `sink_only`: extend `lower_to_rust`
(`lower/rust.rs:32-92`) to also produce a `CssScanProgram` (the CSS analog of
`SinkOnlyProgram`) carrying grammar-DERIVED facts — entry rule (`stylesheet`),
the structural-delimiter alphabet, the at-rule-vs-qualified branch tags (`BackendRule`
branch projection, the existing `AT_RULE_FLAG`), the comment/string skip set, the
declaration `:`-split, the keyframe/media/generic at-rule heads. A new
`css_scan_direct::render(program)` (sibling of `json_sink_direct::render`) emits the
recognizer + lazy projection as `push_str` blocks PARAMETERIZED by those facts (the
delimiter sets become emitted byte-array constants derived from the grammar's literal/
class leaves; the branch tags become the emitted flag-set calls). `emit_request_facts`
is replaced by routing CSS through `emit_with_layout`/`emit_compiled` (un-fork, G3).

**Pros.** (a) Isomorphic to the proven JSON path — ONE emitter discipline, satisfies
addendum 3 directly (no `RuntimeEmitterKind` fork survives). (b) The scan SHAPE is
preserved byte-for-byte from the current courier, so the 94.1% hot path does NOT regress
(preserve-rich-ast / >SOTA held). (c) Grammar-derived: mutating `stylesheet.bbnf`'s
at-rule head or delimiter shape changes the emitted byte-array constants → passes the L1
(a)-(c) mutate-falsifier. (d) Distinct output (addendum 2): JSON emits a sink-dispatch,
CSS emits a delimiter-scan + tape-push — categorically different bodies, md5-distinct by
construction.

**Cons / risk.** The hot scan's delimiter set + structural-byte dispatch are NOT trivially
recoverable from the existing `BackendIr`/`SinkOnlyExpr` (`lower/sink_only.rs:62-103`):
`SinkOnlyExpr` models Seq/Alt/RepeatLoop/CallRule/RegexProgram/ByteLiteral — it has no
"balanced-delimiter component scan" node. The CSS recognizer's `find_component_delim`
delimiter set (`{ } ; :`) is an EMERGENT property of the stylesheet/ruleBlock/declaration
rule shapes, not a literal in any one rule. So Candidate A needs a new IR analysis pass
that DERIVES the structural alphabet from the grammar (which bytes open/close blocks, which
terminate declarations) — non-trivial, and the largest single piece of G2 work.

---

## 2. Candidate B — Grammar-parameterized named primitive (the §6 honest-finding path)

**Shape.** Accept that the delimiter-scan recognizer cannot be SHAPE-derived from a generic
grammar walk without regressing >SOTA (the §1 deep finding), and instead register a
**`balanced_component_scan` grammar-parameterized PRIMITIVE** in the runtime (`runtime_simd`/
a new `runtime_scan` surface). The primitive takes grammar-derived ARGUMENTS — the delimiter
byte set, the structural open/close pairs, the comment/string skip flags — supplied by the
emitter from the `.bbnf`. The generated `generated.rs` INVOKES the primitive by name with
those args; the emitter authors the call site + the arg constants, never the kernel body.
The lazy projection (`CssNode`/`CssRule`/`CssDeclaration`/`classify`) is emitted as
grammar-keyed `push_str` blocks as in Candidate A.

**Pros.** (a) Honest about the irreducible hand-shaped core (the balanced scan IS a
genuine algorithm, not derivable line-by-line) — qualifies under the (a)-(c) escape
(`a1 §L1` REVISE criterion, `SYNTHESIS-AUDIT-OVERFIT.md §6`): grammar-INVOKED by name,
grammar-DERIVED args, `verbatim_blob_present == false`. (b) The primitive is grammar-NEUTRAL
and SHARED — it is the EXACT surface the G6 NEON retarget needs (`SYNTHESIS-PROFILE.md §3`:
"the retargeted NEON must land as a shared grammar-neutral runtime primitive the generated
scan CALLS"). So Candidate B co-locates the G2 lowering and the G6 acceleration target in one
seam — no orphan kernel, no per-grammar re-emit. (c) Smallest blast radius: the emitter never
needs the structural-alphabet-derivation pass to emit a full scanner body; it derives only the
ARG bytes.

**Cons / risk.** (a) The (a)-(c) escape is "the single largest paper-close surface in the
contract" (`SYNTHESIS-AUDIT-OVERFIT.md` R-A0-3) — it must pass the per-primitive mutate test
(mutate the invoking rule, regen, the emitted ARGS must change), or it is a relabeled blob
REJECT. (b) Risk of the primitive becoming a god-kernel if its arg surface grows to encode
all of CSS's structure — it must stay a NARROW balanced-scan with grammar-supplied byte sets,
not a CSS-aware mega-function. (c) Distinctness (addendum 2): if JSON also adopts a shared
scan primitive, the two `generated.rs` must still differ in their call sites / projection
bodies (they do — JSON is sink-dispatch, CSS is tape-push + lazy CSSOM).

---

## 3. Candidate C — Full grammar-IR tree-walk recursive descent (REJECTED baseline)

**Shape.** Lower `stylesheet.bbnf` literally: one emitted parse-fn per `BackendRule`
(`stylesheet`→`ruleList`→`ruleItem`→`qualifiedRule`→`selectorList`→...→`declaration`),
a true recursive-descent that matches the grammar's recursive structure, emitting tape
pushes at the typed `->` materialization points.

**Pros.** Maximally "grammar-driven" in the naive sense; trivially passes the L1 mutate
test (every rule shape change ripples); maximally distinct per grammar.

**Cons / risk (DISQUALIFYING).** This is the combinator-shaped descent the delimiter scan
was BUILT TO AVOID. It would replace the 94.1% flat scalar scan with deep per-rule call
chains over `selectorList`/`complexSelector`/`value` alternations — categorically more
branches, more stack, more dispatch per byte. It WILL regress >SOTA (CSS currently beats
lightningcss 1.66-3.38×; a tree-walk descent is the cssparser-class architecture lightningcss
itself uses). Violates `preserve-rich-ast`'s "never flatten for speed" inverse: here it would
inflate for purity. **Rejected** — listed only to bound the design space and to name the
genuine §6 tension: a fully tree-walk-derived CSS parser CANNOT preserve >SOTA without the
hand-shaped delimiter-scan core.

---

## 4. RECOMMENDATION — Candidate B (primitive) wrapping Candidate A (fact-keyed projection)

Recommend a **hybrid**: the **delimiter-scan recognizer core lands as Candidate B's
grammar-parameterized `balanced_component_scan` named primitive** (honest §6 finding, gated
(a)-(c)), and the **drivers + lazy rich projection land as Candidate A's grammar-fact-keyed
emit blocks** (`parse_block`/`parse_declaration` shells + `CssNode`/`CssRule`/`CssDeclaration`/
`CssTypedValue::classify` derived from the grammar's at-rule/declaration/value-class facts).

Rationale: the §1 deep finding is real and binding — the scalar scan is NOT line-derivable
from a generic IR walk, but it IS a narrow, namable, grammar-parameterizable algorithm. So
the honest action is exactly the contract's §6 path: name it, parameterize it by grammar-
derived byte sets, INVOKE it from the emitted scan, and machine-prove the args vary under a
`.bbnf` mutation. Everything ELSE (the structural drivers, the lazy projection, the typed
classifier) IS fact-keyed-emittable per Candidate A. This single decision also DISCHARGES the
G6 sequencing dependency: the shared primitive IS the NEON retarget call site
(`SYNTHESIS-PROFILE.md §3`), so G2 and G6 share one seam instead of forking it.

Pure Candidate A (no primitive) is the fallback IF the structural-alphabet-derivation pass
proves tractable enough to emit the full scan body from grammar facts without a hand-shaped
core — but on current evidence the balanced-scan recursion (string/comment skip + nested
`()[]{}`) is the irreducible piece, so B-wrapping-A is the grounded recommendation.

---

## 5. Key risk

**The structural-alphabet-derivation gap.** The hot scan's delimiter set (`{ } ; :`) and
structural-byte dispatch (`' " / ( [ {`) are EMERGENT from the rule shapes, not present as
literals in any single `BackendRule`; `SinkOnlyExpr` (`lower/sink_only.rs:62-103`) has no node
that models a balanced-component scan. G2 must add an IR analysis that derives these byte sets
from `stylesheet.bbnf`'s block/declaration/at-rule structure. If that derivation is incomplete
or wrong, the emitted scan either (a) silently diverges from the 9-field cssparser oracle
(parity REJECT) or (b) is hand-patched to match — which collapses back into a verbatim blob
(L1 REJECT). The mitigation is Candidate B: derive only the ARG byte sets (a far smaller, more
tractable derivation) and keep the scan ALGORITHM in the named primitive — but then the
(a)-(c) mutate-falsifier MUST prove those args genuinely vary with the grammar, or the whole
thing is a relabel. This is the make-or-break of G2 and the single most likely place it
REDRESSes.

Secondary risk: addendum-4 LCD-flatten. CSS's lazy projection (`CssDocument::nodes` →
`CssNode::value` → typed `CssRule`/`CssDeclaration`, `runtime_generator.rs:969/795`) and JSON's
(`JsonValue` via `JsonNodeKind::at_cursor`, `json/value.rs:146-167`) both navigate the SAME
tape but with DIFFERENT rich shapes. G2's shared trait (G4) must NOT flatten CSS's
`CssRule::selector_count`/`CssDeclaration::typed_value` or JSON's `get(key)`/typed-`Kind` to a
common denominator — `json_rich_navigation_preserved == true` is a co-gate. G2's projection
emit must keep the two grammar-specific rich APIs intact while sharing only the substrate +
the cursor primitive.

---

## 6. Prune / sequencing dependency

**G2 entry-gates on BOTH G1 AND P3** (dual gate, authoritative per
`SYNTHESIS-AUDIT-OVERFIT.md §5` graph; `a2-prune-sequencing.md:321,226`):

- **P3 MUST close before G2.** The 7 `css_l4_*` rows
  (`regen_css.rs:35-...`, all `grammar_name:"css_l4"`, `entry_rule:"stylesheet"`,
  `source_roots: CSS_L4_ROOTS`, differing ONLY in `profile`/`output_dir`/`output_labels`)
  must collapse to ONE CSS config + `RuntimeTarget` row-collapse (`RuntimeTarget: PartialEq`
  full-row, R16) BEFORE G2 derives CSS — else G2 re-derives the SAME scan into 7 byte-identical
  files and re-creates the replica overfit (addendum 2). The WIRE (G6) then consumes the
  P3-collapsed SINGLE CSS scan; collapse-first is mandatory or the NEON re-forks 7 ways.
- **G1 MUST close before G2.** G2 reuses G1's projecting-renderer discipline (the facts-bus +
  emit-block pattern that retires `json_sink_direct::render`'s fixed literals); "the projecting
  renderer must exist first" (`a2-prune-sequencing.md:511`). A G1 failure BLOCKS G2.
- **P4 (Lock-14 gate) MUST land before G2/G3.** The `FORBIDDEN_GENERIC_TOKENS` extension
  (`CSS_`/`_RS`/`EventGrammar`) + moving `runtime_generator.rs` into `GENERIC_SCAN_ROOTS` must
  be live so the un-forked CSS emitter is neutrality-scanned AS it is authored, not after
  (`a2-prune-sequencing.md:191-193`).

**G2 emits forward into:** G3 (un-fork — CSS routed through `emit_compiled`, retiring
`RuntimeEmitterKind::RequestFacts`), then G4 (shared trait over the CSS/JSON lazy projections),
then **G6 (NEON WIRE into the P3-collapsed shared scan primitive** Candidate B lands). A G2
failure blocks G3/G4/G6/PROVE (Sheets emits through the un-forked generator). Hard cap:
research 20 / plan 15 / redress 30 min (`SYNTHESIS-AUDIT-OVERFIT.md §5.6`).
