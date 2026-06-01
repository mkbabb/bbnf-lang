# SK-V18 S-P0 — The Six CHALLENGE Addenda Lens Registry (A1, cycle V3)

Date: 2026-05-31. Bracket HEAD: `83b66db42` (SK-V17 closed `f6a38445b`; the V3
PASS-IMPL audit committed `7dbe44c22`; the Alpha goalset bracket was `318d9c046`,
HEAD has since advanced to `83b66db42` — every grep below was re-run live at
`83b66db42`).

**Cycle V3 fold posture.** The V1 registry converged at the V2 hardening confirm:
all seven lenses CH1-CH7 closed at 100% (0 REVISE / 0 REJECT) for the V1→V2 pair,
two-consecutive ≥95% met (`hardening/V2/CH{1..7}.md`, each `TALLY accept=N
revise=0 reject=0`). Every V1 fold was discharged on disk at V2. This V3 pass (a)
re-anchors the registry to the live HEAD and re-confirms all six lenses STILL FIRE
on disk (L1 8 `_RS` couriers + `CSS_GENERATED_RS:701`; L2 7×`b654562c…`; L3
`RuntimeEmitterKind:40`; L4 `ValueRef<…G: EventGrammar = AnyGrammar>:175`; L5
`measure_mbps:3091` + `css_canon_bench` kept; L6 `find_css_significant` 0 hot-path
callers — all re-grepped this pass); (b) absorbs the ONE carried sub-REVISE the V2
CH7 disposition recorded for a later cycle (`hardening/V2/CH7.md:142-151`, the a3
§3 fenced-block field-vs-source-line numbering paraphrase) — now resolved in
`a3-arch-measurement-gate-residual.md:117-135` (the `NN:` prefixes are explicitly
labeled `regen.rs` source lines, with `frontend_requirements` pinned as field #11
and `output_labels` as field #12, foreclosing the "field #17/#18 vs 12 fields"
misread; no gate keys on either ordinal — the gate is field-NAMED). No REVISE or
REJECT remains open across the V2 CH set.
Pass: S-P0 Overfit Audit (`restart/prompts/skinny/PASS-0-OVERFIT-AUDIT.md`).
Authority seed: `restart/audit/skinny-impl-overfit/V3/CONSOLIDATED-AUDIT.md`
(the six addenda are PROPOSED at `CONSOLIDATED-AUDIT.md:88-94`); the binding
goalset that consumes them: `restart/skinny/tranches/sk-v18/SYNTHESIS.md`
(§0.1 gates, §0.4 pre-blocks, Section 2 telemetry).

## 0. Purpose and binding force

The V3 PASS-IMPL overfit audit found that SK-V17 closed >SOTA on JSON and CSS
but on **hand-written, forked, replicated** parsers, NOT on a grammar-driven
generator (`CONSOLIDATED-AUDIT.md:9-13`, D1-D4 + C1-C3). The six forward-lens
addenda the V3 agents proposed (`CONSOLIDATED-AUDIT.md:88-94`) are the precise
falsification tests that would have caught each fake-generalization surface
BEFORE it shipped. This registry **formalizes those six as binding S-P0 lenses**:
each entry pins (1) the failure mode it catches, citing the V3 finding by
path:line; (2) the executable check (grep / diff / md5 / samply) runnable from
`skinny/crates/`; (3) the REJECT criterion and the REVISE criterion, kept
distinct per the abrogate-before-patch discipline.

These six are **subordinate refinements of CH7 Overfit-Prune**
(`PASS-0-OVERFIT-AUDIT.md:62-87`), not new top-level lenses. CH7 already binds
"every new code is grammar-derived, never hand-written under a `// @generated`
header" and "no SCAFFOLD-ONLY landing counts as an admit." The six addenda are
the **executable decision procedures** that make CH7 machine-checkable for the
SK-V18 generalization surface specifically. A finding under any of the six is a
CH7 REJECT/REVISE and carries CH7's non-negotiable force: it CANNOT be carried as
"acknowledged but not blocking" (`PASS-0-OVERFIT-AUDIT.md:86-87`).

**Binding scope.** This registry binds into (a) the S-P0 audit-overfit dispatch
itself (each A0-A3 sub-agent applies the relevant lenses to the goalset surface);
(b) **every downstream SK-V18 wave's CHALLENGE phase** (the lens IDs below are
cited verbatim by the per-wave gate consumer). The gate consumer that mechanizes
them is the S-P3-bound `(cd skinny && cargo xtask gate-json --check-results
--skv18-generalization-report <path>)` (`SYNTHESIS.md:583-617`); each lens maps
to one or more of the Section-2 telemetry columns named in its entry.

**Severity convention.** REJECT = the surface is a relabeled overfit/contrivance
and the admit does NOT land (revert + REDRESS entry). REVISE = the direction is
correct but the obligation/check/scope is mismatched and must be corrected before
the admit lands (the abrogate-before-patch default still applies: ask "can we
delete?" before "can we patch?"). The two are NOT interchangeable — a REJECT that
gets softened to REVISE is itself a paper-close.

---

## Lens L1 — verbatim-blob

**Addendum text (verbatim, `CONSOLIDATED-AUDIT.md:89`):** "NEW-CH(verbatim-blob):
flag const-string-courier emitters (a `@generated` file that is a verbatim `&str`
literal in codegen = hand-written, not derived)." Also `SYNTHESIS.md:427-429`
(the verbatim-blob re-entry pre-block): "a `@generated` file that is a verbatim
`&str` literal in codegen is hand-written, NOT derived — REJECT as
'grammar-driven.' G1/G2 must not replace one const-string courier with another."

**Failure mode it catches (V3 finding D1, HIGH).** A "grammar-driven generator"
that is actually a hand-written recursive-descent scanner spliced into the output
verbatim. Per `CONSOLIDATED-AUDIT.md:30`: "CSS: `CSS_GENERATED_RS` is a
~646–910-LOC hand-written recursive-descent scanner emitted **verbatim as a Rust
`const &str`** (`runtime_generator.rs:701-1611`); the `.bbnf` grammar is **never
consumed** by the CSS emit path."
**LOC figure (disk-measured, not a seed estimate):** the raw-string body runs
`runtime_generator.rs:701`→`:1611` = **910 LOC** (the closing `"#;` is at `:1611`,
re-measured this pass). The V3-seed "646–910" range is the seed's PRE-MEASUREMENT
estimate; it is SUPERSEDED by the disk-measured 910-line body span. The figure is
DESCRIPTIVE only — no gate keys on the LOC; the binding gate is
`verbatim_blob_present == false` + the `.bbnf`-mutation test. The seed continues:
"the `.bbnf` grammar is **never
consumed** by the CSS emit path … This is the **identical SK-V16 finding,
UN-REMEDIATED**, now wearing a real `@generated` header (provenance-honest header
on hand-written content)." Same for JSON (`CONSOLIDATED-AUDIT.md:31`):
`json_sink_direct::render` "emits the hot parser as fixed Rust string literals …
the grammar only `validate()`-gates emission, does not shape it." The provenance
header LIES: the `@generated` banner asserts derivation while the body is
hand-authored. The lens defeats the lie by checking the BODY, not the banner.

**Live witness at `83b66db42` (the surface still present, un-remediated):**
- `skinny/crates/codegen/src/runtime_generator.rs:701` — `const CSS_GENERATED_RS:
  &str = r#"` (the verbatim CSS scanner blob), spliced at `:91`
  (`("generated.rs".to_string(), normalize(CSS_GENERATED_RS))`).
- Eight sibling const-`&str` couriers in the same file:
  `JSON_PARSE_ONLY_GENERATED_RS:195`, `JSON_PARSE_ONLY_PARSER_RS:550`,
  `JSON_MOD_RS:572`, `JSON_HOST_RS:594`, `CSS_MOD_RS:598`, `CSS_PARSER_RS:612`,
  `CSS_SINK_RS:665`, `CSS_GENERATED_RS:701`.

**Executable check.**
```sh
# (a) Any const-&str courier whose value is a raw-string body in the codegen crate:
rg -n 'const\s+\w*_RS\s*:\s*&str\s*=\s*r#"' skinny/crates/codegen/src/
# MUST return 0 for any const spliced into a @generated output path.

# (b) The specific CSS courier (G2 close gate, SYNTHESIS.md:332):
grep -c 'CSS_GENERATED_RS' skinny/crates/codegen/src/runtime_generator.rs   # MUST be 0

# (c) Every @generated runtime file must NOT be reproducible by a const lookup in codegen —
# confirm the emit path consumes the .bbnf AST, not a const. The header is necessary-not-sufficient:
rg -n '@generated' skinny/crates/runtime/src/grammars/*/generated.rs        # banner present
# AND the producing emit fn must take a parsed-grammar/SinkOnlyProgram argument, not splice a const.
```

**Telemetry column:** `verbatim_blob_present` (must be `false`,
`SYNTHESIS.md:563`).

**REJECT criterion.** Any `@generated` output file whose content is a verbatim
`&str`/raw-string literal in `skinny/crates/codegen/src/` (check (a)/(b) non-zero)
is **REJECT as "grammar-driven"** — it is hand-written content under a provenance
header. The `@generated` banner does NOT rehabilitate it. This includes the
relabel hazard: a courier renamed to look derived, or split into N smaller const
fragments concatenated at emit, is the same blob (check (a) catches the
`const … _RS: &str = r#"` syntax regardless of name).

**REVISE criterion.** If the emit path PROJECTS from the `.bbnf`/`SinkOnlyProgram`
AST but a single isolated sub-fragment is a named primitive (e.g. a registered
balanced-delimiter scanner the grammar invokes by name), it qualifies for the
honest-finding escape ONLY under the §0.1 PASS-IMPL gate (`SYNTHESIS.md:342`,
the (a)-(c) qualification): (a) the `.bbnf` INVOKES it by name (machine: `grep` the
primitive's name in the `.bbnf`); (b) the primitive's EMITTED OUTPUT VARIES
correspondingly under a `.bbnf` mutation of the invoking rule's shape — i.e. apply
the same per-primitive mutate-falsifier (the §L1 whole-path mutate test applied to
THIS primitive's output in isolation): mutate the invoking rule, regen, and the
primitive's emitted body MUST change; a fixed body keyed off a merely-decorative
grammar-derived argument FAILS (b) exactly as a const courier fails the whole-path
test, so "accepts a grammar-derived argument" is NOT sufficient; (c) it carries
`verbatim_blob_present == false`. All three are MACHINE predicates (grep · mutate +
regen-diff · telemetry column) — none is prose-reviewed-at-admission. A primitive
failing (a)-(c) is a relabeled blob — REJECT, not REVISE. Absent the escape, the
correct action is to make the emit projection-driven (G1/G2), not to patch the blob.

---

## Lens L2 — distinct-grammar-output

**Addendum text (verbatim, `CONSOLIDATED-AUDIT.md:90`):**
"NEW-CH(distinct-grammar-output): N claimed grammars must have N **non-identical**
`generated.rs` (diff-census)." Refined at `SYNTHESIS.md:433-435` (the
distinct-grammar-output re-entry): "N claimed grammars must have N non-identical
`generated.rs`; byte-identical replicas do NOT count" — and crucially
`SYNTHESIS.md:565` makes md5-distinctness **necessary-not-sufficient**: it is
co-gated by `generator_grammar_branch_count == 0` and the structural
`runtime_target_rows_collapsed` check.

**Failure mode it catches (V3 finding D1, HIGH).** Claiming N grammars while
shipping ONE parser replicated N times. Per `CONSOLIDATED-AUDIT.md:34`: "The **7
`css_l4_*/generated.rs` are byte-identical** (`diff` = 0) — ONE CSS parser
replicated 7×, materially overstating '7 grammars admitted' (all share
`stylesheet.bbnf` / `entry_rule: stylesheet`)." The inverse failure (the
necessary-not-sufficient half): N files that ARE md5-distinct but were produced
by ONE emitter body that branches on a grammar token, or by a relocated
per-grammar data-table — md5-distinctness alone would false-green that.

**Live witness at `83b66db42` (replicas still byte-identical):** all 7
`skinny/crates/runtime/src/grammars/css_l4_*/generated.rs` share one md5
`b654562ccff46ed62dd48e9ace325830` (re-verified live across at-rules /
declaration-values / declaration-values-extended / nested-layout /
stylesheet-selectors / vendor-and-custom-atrules / visual-functions).

**Executable check (the full three-part co-gate — md5 alone is insufficient).**
```sh
# (a) diff/md5 census — N claimed grammars => N DISTINCT md5s:
for d in skinny/crates/runtime/src/grammars/*/generated.rs; do md5 -q "$d"; done | sort | uniq -d
# A repeated md5 (uniq -d non-empty) over files claimed as distinct grammars = REJECT.

# (b) generator_grammar_branch_count == 0 — no grammar-token match-arm in the emitter body
#     (a distinct md5 can still come from a self-disclosing-token branch):
rg -n 'match\s+\w+\s*\{[^}]*Json\s*=>|CssL4\s*=>|(GoogleSheets|Sheets)\w*\s*=>|Bbnf\w*\s*=>' \
   skinny/crates/codegen/src skinny/xtask/src        # MUST be 0

# (c) runtime_target_rows_collapsed — no per-grammar branch RELOCATED into a neutral data-table
#     (a distinct md5 can still come from an xtask RuntimeTarget data-table; the regex in (b) is
#     syntactically incapable of firing on a neutral-identifier table, CH2 V3 §8.1). Structural:
#     all RuntimeTarget rows sharing one grammar_name byte-identical except (output_dir,expected_files):
#     count(distinct config-tuple-minus-path-columns) == 1 per grammar_name  (tiny xtask/awk/jq assert).
```

**Telemetry columns:** `generated_md5_distinct` (must be `true`,
`SYNTHESIS.md:572`); co-gated by `generator_grammar_branch_count == 0`
(`:565`), `generator_grammar_type_count == 0` (`:567`),
`runtime_target_rows_collapsed == true` (`:566`).

**REJECT criterion.** (i) Any "N grammars" claim over a set whose `generated.rs`
files are NOT all md5-distinct (check (a), `uniq -d` non-empty) — **REJECT** the
count. (ii) md5-distinct files whose distinctness is produced by a grammar-token
match-arm (check (b) non-zero) OR a relocated per-`grammar_name` data-table
(check (c) fails the per-`grammar_name` collapse) — **REJECT**: md5-distinctness
is necessary-not-sufficient, the distinctness rides a branch/table, not a distinct
`.bbnf`. The 7 CSS replicas are the canonical REJECT today.

**REVISE criterion.** If the surface is genuinely N distinct grammars but the
profiles have not yet collapsed to one config per grammar (the live P3 state: 7
css_l4 rows carry 7 distinct `profile` values — `SYNTHESIS.md:140-167`), the gate
is correctly RED pre-collapse; the action is to **collapse to one CSS config OR
differentiate by distinct `.bbnf` roots** (P3, `SYNTHESIS.md:328`), preserving
profile-distinctness where the profiles are genuinely distinct grammars. Do NOT
erase the `profile` discriminator to false-green check (c). REVISE = run the
collapse; do not paper over.

---

## Lens L3 — single-emitter-path

**Addendum text (verbatim, `CONSOLIDATED-AUDIT.md:91`):**
"NEW-CH(single-emitter-path): one grammar-agnostic emitter; flag grammar-family
forks." Bound at `SYNTHESIS.md:333` (G3): "ONE grammar-agnostic emitter path
emits every grammar per the canonical Lock-14 three-surface model … The
single-emitter-path CHALLENGE passes: no grammar-family flag fork."

**Failure mode it catches (V3 finding D1, HIGH).** A forked generator wearing an
abstract-enum costume — two grammar-family code paths behind a neutral-looking
discriminator. Per `CONSOLIDATED-AUDIT.md:32`: "The generator is FORKED:
`RuntimeEmitterKind = {CompiledLowering(JSON), RequestFacts(CSS)}`
(`grammar_provider.rs:40`) — a grammar-family fork behind an abstract enum." This
is the subtle case L1/L2 miss: the enum variants are NEUTRAL-named
(`CompiledLowering`/`RequestFacts`, not `Json`/`Css`), so the arm-census grep
(L2 check (b)) does NOT fire — yet the fork is real, JSON takes one variant and
CSS the other. The lens catches the fork by enumerating emitter-strategy
discriminators and verifying ONE path serves all grammars.

**Live witness at `83b66db42` (the fork still present):**
- `skinny/crates/codegen/src/grammar_provider.rs:40-42` — `pub enum
  RuntimeEmitterKind { CompiledLowering, RequestFacts }`.
- `:33` — `pub emitter: RuntimeEmitterKind` (the per-grammar selector field).
- `:110` — `if request.profile_contract.emitter != RuntimeEmitterKind::RequestFacts`
  (the live dispatch on the fork; JSON=`CompiledLowering`, CSS=`RequestFacts`).
- Note: the L2 arm-census grep is CLEAN today (`Json =>` does not appear) — the
  fork hides behind the neutral enum names, which is exactly why L3 is a distinct
  lens.

**Executable check.**
```sh
# (a) The named fork enum must be gone post-G3:
grep -rnE 'RuntimeEmitterKind|CompiledLowering|RequestFacts' skinny/crates/codegen/src/   # MUST be 0

# (b) No second emitter-strategy discriminator may replace it. Enumerate emitter-kind enums /
#     match on a per-grammar emitter field; ONE emit fn must serve JSON+CSS+Sheets:
rg -n 'enum\s+\w*Emitter\w*Kind|enum\s+\w*EmitKind|emitter\s*:\s*\w+Kind' skinny/crates/codegen/src/
#     Any per-grammar emitter-kind discriminator = a relocated fork.

# (c) The neutral-identifier relocation (a fork moved into the RuntimeTarget data-table) is the
#     same seam L2 check (c) covers — runtime_target_rows_collapsed per grammar_name. Reuse it.
```

**Telemetry column:** `emitter_fork_present` (must be `false` post-G3,
`SYNTHESIS.md:564`). Co-gated by `generator_grammar_branch_count == 0` (`:565`)
and `runtime_target_rows_collapsed == true` (`:566`) so a fork relocated into a
neutral data-table is structurally caught.

**REJECT criterion.** Any grammar-family fork — a `RuntimeEmitterKind`-style enum
whose variants partition by grammar family, or a per-grammar `emitter:` field that
selects a different emit body for JSON vs CSS vs Sheets — is **REJECT**, even
when the variant names are neutral (`CompiledLowering`/`RequestFacts`). The
neutral name is NOT a defense; the fork is in the dispatch, not the spelling. A
relocated fork (the strategy moved into a per-`grammar_name` data-table) is REJECT
via L2 check (c).

**REVISE criterion.** If a SINGLE emit fn takes grammar-derived DATA (alphabet,
entry rule, rule shapes from the `.bbnf`) and dispatches on that DATA — not on a
grammar-family tag — that is the target architecture, not a fork; verify check (c)
collapses to one config-tuple per `grammar_name`. If the emit fn currently forks
but the intended end-state is one data-driven path, REVISE = finish the un-fork
(G3), do not ship the enum behind a wider abstraction.

---

## Lens L4 — phantom-generic

**Addendum text (verbatim, `CONSOLIDATED-AUDIT.md:92`):**
"NEW-CH(phantom-generic): a generic param `<G>` never instantiated with a real
type is decorative — flag it." Bound at `SYNTHESIS.md:334` (G4) and
`:430-432` (the phantom-generic re-entry pre-block): "a generic `<G>` never
instantiated with a real type outside `#[cfg(test)]` is decorative; G4
instantiates-or-deletes, it does not add a second phantom."

**Failure mode it catches (V3 finding D2, HIGH).** A type parameter that exists
only to LOOK grammar-parametric — never instantiated with a real production type,
its trait methods having zero non-test call sites. Per `CONSOLIDATED-AUDIT.md:36`:
"`ValueRef<G: EventGrammar>` … Never instantiated with a real grammar (always
`AnyGrammar`); `EventGrammar`'s methods have **zero non-test call sites**;
`JsonEventGrammar`/`SheetsEventGrammar` are inert witnesses. The W2
'grammar-parametric projection' claim **is not load-bearing**." The decorative
`<G>` manufactures a false generalization claim ("grammar-parametric value
reference") over a parameter that production code never animates.

**Live witness at `83b66db42` (the phantom still present):**
- `skinny/crates/runtime/src/tape/mod.rs:175` — `pub struct ValueRef<'doc,
  'input: 'doc, K = AnyKind, G: EventGrammar = AnyGrammar>` — `G` DEFAULTS to
  `AnyGrammar`. (`K = AnyKind` is the SEPARATE, already-real axis — do NOT
  conflate; G4 targets the `G` axis ONLY, `SYNTHESIS.md:334`.)
- The ONLY `G` instantiations with a named grammar witness are test-only
  `_proof_compiles`:
  `skinny/crates/runtime/src/tape/event_grammar_tests.rs:18`
  (`_proof_compiles::<JsonEventGrammar>`), `:20`
  (`::<SheetsEventGrammar>`), `:21` (`::<AnyGrammar>`) — all inside a test module.
- `AnyGrammar::STRUCTURAL_CLASS_COUNT == 0` (asserted `event_grammar_tests.rs:36`)
  — the production default carries zero structural classes, i.e. the phantom is
  inert in production.

**Executable check.**
```sh
# (a) Find the <G> default (the phantom signature):
grep -n 'G: EventGrammar = AnyGrammar' skinny/crates/runtime/src/tape/mod.rs
# (b) Census G instantiations with a REAL grammar witness, EXCLUDING tests
#     (mirror the F6 phantom-grep test-exclusion, SYNTHESIS.md:569):
rg -n 'ValueRef::<[^>]*\b(Json|Sheets|Css)\w*EventGrammar\b|ValueRef<[^>]*\b(Json|Sheets|Css)\w*EventGrammar\b' \
   skinny/crates/runtime/src/ | grep -vE '_tests\.rs|tests\.rs|#\[cfg\(test\)\]|_proof_compiles'
#     ZERO non-test instantiations => the <G> axis is a phantom => instantiate-or-DELETE.
# (c) Confirm the only animator is the test proof:
grep -rn '_proof_compiles' skinny/crates/runtime/src/   # only in *_tests.rs
```

**Telemetry column:** `phantom_generic_resolved` (enum `instantiated`/`deleted`;
`phantom` i.e. `G` stays `AnyGrammar` is NO-GO, `SYNTHESIS.md:568`).

**REJECT criterion.** A `<G>` (or any type param) whose only instantiations with a
real type are inside `#[cfg(test)]` / `_proof_compiles` is **decorative** — and
keeping it as-is (`phantom_generic_resolved == phantom`) is a **REJECT**. The
inert-witness defense (`JsonEventGrammar`/`SheetsEventGrammar` exist as types) does
NOT count: a witness type with no production instantiation is still a phantom
animator.

**REVISE criterion (instantiate-or-DELETE; DELETE is the default).** Per
`SYNTHESIS.md:334`, **DELETE is the abrogate-before-patch DEFAULT** — "no
`CssEventGrammar` witness exists at HEAD, so 'instantiate' entails authoring a new
grammar-named type; the trait does NOT require it." REVISE = remove the `<G>`
parameter from the struct. INSTANTIATE is permissible ONLY if a production grammar
witness genuinely reaches the `G` axis at a non-test call site. **Critical
preserve-rich-ast guard (`SYNTHESIS.md:334`, `:570`):** resolving the phantom must
NOT couple to LCD-flattening JSON's richness — the shared trait's existence is
INDEPENDENT of `<G>`, deleting `<G>` and defining the trait are separable, and a
≥2-impl trait that flattens JSON's `get(key)`/typed-`Kind`/visitor
(`json_rich_navigation_preserved == false`) is itself a preserve-rich-ast REJECT
even at ≥2 impls. Do NOT "resolve" one phantom by manufacturing a thin LCD trait
that erases rich navigation.

---

## Lens L5 — timed-plane-symmetry + corpus-in-the-timer

**Addendum text (verbatim, `CONSOLIDATED-AUDIT.md:93`):**
"NEW-CH(timed-plane-symmetry + corpus-in-the-timer): the >SOTA comparator must do
equal work on the real corpus, cold." Bound at `SYNTHESIS.md:338` (H1) and
`:501-503` / `:580` (the strict comparator gate): "The timed plane must do equal
work on the real corpus, cold — no micro-fixtures … no more-work competitor."

**Failure mode it catches (V3 finding C3, HIGH; with C2 framing, MEDIUM).** A
>SOTA claim measured on a WARM loop over MICRO-FIXTURES where the timed competitor
does MORE work than the subject. Per `CONSOLIDATED-AUDIT.md:53`: "`nonjson_css_l4.rs:528`
`lightningcss_facts`/`measure_mbps`: warm (16+2000 iters), times 85–357-byte
SHA256-pinned micro-fixtures (not the real corpus), and the timed lightningcss
does MORE work (parse + SHA256 + a second cssparser re-parse). This is the SK-16
contrivance family, **still in the tree**." Three independent violations in one
harness: (1) WARM (not cold-per-parse, violating `no-warm-benches`); (2)
MICRO-FIXTURE (85-357 bytes, not the 71KB-495KB real corpus — corpus-out-of-the-
timer); (3) MORE-WORK COMPETITOR (the timed competitor pays SHA256 + a re-parse
the subject does not). The companion C2 finding
(`CONSOLIDATED-AUDIT.md:50`): even the CANONICAL harness is honestly
"lazy rich-summary beats eager full-CSSOM," not equal-work — so the lens also
binds the materialization-depth disclosure.

**Live witness at `83b66db42` (the contrived path still present, P2 target):**
- `skinny/crates/bbnf-bench/src/nonjson_css_l4.rs:3091` — `fn measure_mbps`: 16
  warm iters (`for _ in 0..16 { black_box(f(...)) }`) + a 2000-iter amortized loop
  (`let iterations = 2_000u64`; `bytes = input.len() * iterations`) — WARM and
  amortized, not cold-per-parse.
- `nonjson_css_l4.rs:528` `lightningcss_facts` + siblings `:551`/`:566`/`:583`
  (`*_lightningcss_facts`); the SHA256 work `:26` `use sha2::{Digest, Sha256}`,
  fixture hashing at `:925`/`:1054`/`:1205`.
- The HONEST harness that produced the headline numbers:
  `skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs` — cold per-parse, N≥200
  default, real corpus, single `parse(...)` black-boxed and dropped each sample
  (`:9-12` cold-discipline doc). This is KEPT (P2 deletes only the warm path).

**Executable check.**
```sh
# (a) The warm/more-work micro-fixture path must be DELETED (P2, SYNTHESIS.md:327):
grep -nE 'measure_mbps|lightningcss_facts' skinny/crates/bbnf-bench/src/nonjson_css_l4.rs   # MUST be 0
# (b) No warm/amortized loop in any >SOTA harness — flag multi-iter amortization in a timer:
rg -n 'for _ in 0\.\.(16|[0-9]{3,})|iterations\s*=\s*[0-9]{3,}|warmup|warm' \
   skinny/crates/bbnf-bench/src/   # any hit in a TIMED >SOTA path = warm-bench violation
# (c) The canonical honest harness present + cold + real-corpus + N>=200:
test -f skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs && \
   rg -n 'cold|black_box|median|default N=200|sample' skinny/crates/bbnf-bench/src/bin/css_canon_bench.rs
# (d) corpus-in-the-timer: the timed region parses the REAL corpus (71KB-495KB), not a micro-fixture;
#     and the competitor does NOT pay extra work (no SHA256 / re-parse inside the timed competitor call).
```

**Telemetry columns:** `corpus_in_timer` (must be `true`, real corpus inside the
timed region, cold, no micro-fixtures; `SYNTHESIS.md:580`);
`materialization_framing` (enum `lazy-rich-vs-eager-cssom` / `symmetric-comparator`;
the honest CSS framing, `:579`). The gate REJECTS `corpus_in_timer == false` and
any single-tuple broadcast (`sample_count == 1`).

**REJECT criterion.** A >SOTA claim is **REJECT** if ANY of: (i) the timed loop is
WARM/amortized (check (b) fires) rather than cold-per-parse; (ii) the timed region
parses a MICRO-FIXTURE rather than the real corpus (`corpus_in_timer == false`,
check (d)); (iii) the timed COMPETITOR does more work than the subject (SHA256,
re-parse, extra materialization inside the competitor's timed call). The
`nonjson_css_l4.rs measure_mbps` path is the canonical triple-REJECT and must be
DELETED (P2), not patched. A fabricated competitor column for an un-run engine
(yyjson/asmjson/RapidJSON honest-`None` on aarch64, `SYNTHESIS.md:495-497`) is
also a REJECT under this lens.

**REVISE criterion.** If the harness is genuinely cold + real-corpus but the
materialization DEPTH differs (the subject counts lazily while the competitor
builds an owned CSSOM — the canonical `css_canon_bench` C2 case), the action is to
**disclose the asymmetry explicitly OR add a symmetric materialization-depth
comparator** (H1, `SYNTHESIS.md:338`): set `materialization_framing ==
lazy-rich-vs-eager-cssom` or `symmetric-comparator`. REVISE = honest framing; the
numbers stand, the claim is re-worded. Do NOT silently re-label "lazy beats eager"
as "equal-work CSSOM beats CSSOM."

---

## Lens L6 — acceleration-wiring

**Addendum text (verbatim, `CONSOLIDATED-AUDIT.md:94`):**
"NEW-CH(acceleration-wiring): a 'NEON acceleration' claim must show the kernel
reached at admission, not only `#[cfg(test)]`." Bound at `SYNTHESIS.md:336` (G6)
and `:575` / `:596` (the strict gate): "any kernel claiming acceleration is
reached at admission (grep the generated hot path, not tests); scalar passthroughs
carry an honest label."

**Failure mode it catches (V3 finding C1, HIGH; with A4 ASM-backlog).** An
acceleration claim ("NEON structural-index acceleration") where the kernel is
checkasm-validated but UNWIRED from the hot path — reachable only from
`#[cfg(test)]` callers. Per `CONSOLIDATED-AUDIT.md:47`: "`find_css_significant`/
`find_comment_close` are **dead at admission** (only `#[cfg(test)]` callers); only
`count_top_level_commas` reaches a generated module, in the *cold* rich-summary.
**The hot CSS scan is scalar.** The W3 commit title ('NEON structural-index
acceleration') overstates what is wired." The lens catches the gap between
"kernel EXISTS and passes checkasm" and "kernel is REACHED on the benched hot
path." A checkasm-green kernel with only test callers accelerates nothing.

**Live witness at `83b66db42` (the unwiring still present):**
- The kernels exist + are checkasm-validated:
  `skinny/crates/runtime/src/runtime_simd.rs:29` `count_top_level_commas`, `:112`
  `find_comment_close`, `:169` `find_css_significant`.
- `find_css_significant` / `find_comment_close` have ONLY `#[cfg(test)]` callers:
  `skinny/crates/runtime/src/lib.rs:574` (inside `#[test] fn
  neon_significant_skip_matches_scalar`), `:598` + `:608` (inside `#[test] fn
  neon_comment_close_matches_scalar`). The sole `#[cfg(test)]` in lib.rs is at
  `:51`, so everything from `:51` onward — including these callers — is test-only.
- Only `count_top_level_commas` reaches a generated module, and only the COLD
  rich-summary: `css_l4_*/generated.rs:157` → `:809-810`
  (`crate::runtime_simd::count_top_level_commas`).
- JSON's bespoke holdout (G5 context):
  `skinny/crates/runtime/src/grammars/json/scan.rs:25` → `neon::scan(input)`, a
  JSON-private NEON path NOT routed through the neutral
  `bbnf-simd/src/dispatch.rs:42 select_classifier`.
- The 5 scalar-passthrough kernels + UDOT `digit_mac` orphan
  (`CONSOLIDATED-AUDIT.md:60`): `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
  `bulk_emit_positions_64`, `byte_class_from_table_64`, `eob_pad_clamp` are wired
  as "neon" but are scalar passthroughs — an honesty-of-label failure.

**Executable check.**
```sh
# (a) Census callers of each kernel claiming acceleration; EXCLUDE #[cfg(test)] / tests:
for k in find_css_significant find_comment_close count_top_level_commas; do
  echo "== $k =="
  rg -n "$k" skinny/crates/runtime/src/ | grep -vE 'runtime_simd\.rs|_tests\.rs|#\[cfg\(test\)\]'
done
#     A kernel with ZERO non-test callers reaching a generated/hot module = unwired = NO acceleration.
# (b) Confirm the hot path (generated.rs) actually calls the kernel, not only the cold rich-summary:
rg -n 'runtime_simd::(find_css_significant|find_comment_close)' skinny/crates/runtime/src/grammars/*/generated.rs
#     MUST be non-empty for a "hot CSS scan accelerated" claim.
# (c) JSON neutral-kernel migration (G5): json/scan.rs routes through select_classifier, not bespoke neon::scan:
grep -n 'neon::scan\|select_classifier' skinny/crates/runtime/src/grammars/json/scan.rs
# (d) The retire branch is gated on a MEASUREMENT, not an assertion (SYNTHESIS.md:382):
#     samply on the benched CSS hot path => the kernel's target leaf is non-top-N (an attribution row),
#     NOT "marked retired with zero acceleration wired."
```

**Telemetry column:** `acceleration_at_admission` (enum `admission` /
`cfg-test-only` / `scalar-passthrough-labeled` / `retired`; any "NEON/ASM" claim
must be `admission`; `cfg-test-only` is NO-GO for an acceleration claim,
`SYNTHESIS.md:575`/`:596`).

**REJECT criterion.** An acceleration claim ("NEON/ASM acceleration") whose kernel
is reached ONLY from `#[cfg(test)]` callers (check (a) returns only test callers;
check (b) empty) is **REJECT** — `acceleration_at_admission == cfg-test-only` is
NO-GO. A checkasm-green kernel is NOT an admitted acceleration; reaching the
benched hot path AT ADMISSION is. A scalar body wearing a "neon" label
(the 5 passthroughs) is REJECT as a mislabel unless re-labeled honestly.

**REVISE criterion (wire-or-retire honestly, `SYNTHESIS.md:336`/`:382`).** Three
honest dispositions are REVISE, not REJECT: (i) WIRE the kernel into the hot path
with a same-wave consumer in `generated.rs` (then `acceleration_at_admission ==
admission`); (ii) RETIRE/mark dead — but ONLY with a **samply attribution row
proving the kernel's target leaf is non-top-N on the benched CSS hot path** (the
retire branch is gated on a MEASUREMENT, not an assertion — it cannot close by
marking all NEON "retired" with zero acceleration wired); (iii) LABEL a scalar
passthrough honestly (`scalar-passthrough-labeled` — no "neon" label on a scalar
body). An orphan kernel with neither a hot-path consumer nor a non-top-N
measurement is NOT a valid REVISE — it is a REJECT pending one of (i)/(ii)/(iii).

---

## Registry summary table (bind verbatim into every downstream CHALLENGE)

| Lens | Catches (V3 finding) | Primary executable check | Telemetry column(s) | REJECT trigger |
|---|---|---|---|---|
| **L1 verbatim-blob** | D1 (`CONSOLIDATED-AUDIT.md:30-31`): hand-written scanner emitted as `const &str` under a `@generated` banner | `rg -n 'const \w*_RS\s*:\s*&str\s*=\s*r#"' skinny/crates/codegen/src/` = 0; `grep -c CSS_GENERATED_RS …runtime_generator.rs` = 0 | `verbatim_blob_present == false` | any `@generated` output reproducible from a const-`&str` in codegen |
| **L2 distinct-grammar-output** | D1 (`:34`): 7 byte-identical CSS replicas claimed as "7 grammars" | md5 census `uniq -d` empty; AND arm-census = 0; AND `runtime_target_rows_collapsed` per `grammar_name` | `generated_md5_distinct`, `generator_grammar_branch_count`, `runtime_target_rows_collapsed` | repeated md5 over claimed-distinct grammars; OR distinctness from a branch/data-table |
| **L3 single-emitter-path** | D1 (`:32`): `RuntimeEmitterKind={CompiledLowering,RequestFacts}` grammar-family fork behind a neutral enum | `grep -rnE 'RuntimeEmitterKind\|CompiledLowering\|RequestFacts' skinny/crates/codegen/src/` = 0 | `emitter_fork_present == false` | any grammar-family fork, even with neutral variant names |
| **L4 phantom-generic** | D2 (`:36`): `ValueRef<G: EventGrammar=AnyGrammar>` instantiated only in `_proof_compiles` tests | `grep 'G: EventGrammar = AnyGrammar' tape/mod.rs`; G-instantiation census `grep -vE 'tests\.rs\|#[cfg(test)]'` = 0 non-test | `phantom_generic_resolved ∈ {instantiated,deleted}`; `json_rich_navigation_preserved == true` | `<G>` instantiated only under `#[cfg(test)]` (DELETE default; no LCD-flatten resolve) |
| **L5 timed-plane-symmetry + corpus-in-timer** | C3 (`:53`) warm 16+2000-iter micro-fixture more-work bench; C2 (`:50`) lazy-vs-eager | `grep -nE 'measure_mbps\|lightningcss_facts' nonjson_css_l4.rs` = 0; warm-loop census = 0; `css_canon_bench` cold + real-corpus + N≥200 | `corpus_in_timer == true`; `materialization_framing` disclosed | warm/amortized loop, OR micro-fixture in timer, OR more-work competitor |
| **L6 acceleration-wiring** | C1 (`:47`): NEON kernels reached only from `#[cfg(test)]`; hot CSS scan scalar | kernel-caller census excluding tests = non-empty on a hot/generated module; `generated.rs` calls the kernel | `acceleration_at_admission ∈ {admission,scalar-passthrough-labeled,retired}` | acceleration claim with only `cfg(test)` callers; scalar body labeled "neon"; orphan kernel w/o non-top-N samply |

---

## Sequencing constraint the registry imposes (PRUNE → GENERALIZE → PROVE)

The six lenses are NOT independent gates applied in parallel — they have a
**precedence the standing order (`SYNTHESIS.md:186` "PRUNE first, then GENERALIZE,
then PROVE") imposes**, and S-P0's synthesis must encode it:

1. **L1 (verbatim-blob), L5 (timed-plane), L6 (acceleration-wiring)** bind the
   PRUNE/HONESTY surface — they catch contrivances that must be DELETED or
   honestly framed BEFORE the generalize waves rebuild on top (P1-P5, H1; the
   `css_canon_bench` honest harness is preserved). A generalize wave that lands on
   a surface still failing L1/L5/L6 inherits the contrivance.
2. **L2 (distinct-grammar-output), L3 (single-emitter-path)** bind the GENERALIZE
   emit-path surface (G1-G3) — they are the co-gate that makes "grammar-driven
   generator" a verified claim, not an asserted one (md5-distinct is
   necessary-not-sufficient; the arm-census + `runtime_target_rows_collapsed`
   complete it).
3. **L4 (phantom-generic)** binds the GENERALIZE value-API surface (G4) — and
   carries the preserve-rich-ast guard so resolving the phantom cannot LCD-flatten
   JSON.
4. All six are RE-APPLIED at PROVE (Sheets via the generator only): the Sheets
   `generated.rs` must be L1-clean (no `_GENERATED_RS` Sheets blob,
   `SYNTHESIS.md:337`), L2-distinct (md5 ≠ JSON ≠ CSS), L3-emitted by the un-forked
   path, and L4-instantiate the shared trait.

**Dependency chain (from the §3 revert protocol, `SYNTHESIS.md:662-669`):**
PRUNE → G1 → G2 → G3 → G4 → G5/G6 → PROVE → H1. A wave failing its lens-gated exit
BLOCKS every downstream wave that entry-gates on it. In particular: L1-failure on
G1/G2 blocks G3/G4/PROVE; L3-failure on G3 (un-fork) blocks PROVE (which emits
Sheets THROUGH the un-forked generator). No downstream wave dispatches over a
REDRESSed predecessor.

---

## Honest residual note (no new hardcoding introduced)

This registry adds ZERO new code or hardcoding — it is a binding lens
specification. Every check is a grep/diff/md5/samply over the existing benched
skinny tree (`skinny/crates/`). The six lenses, applied to the SK-V18 goalset
surface at `83b66db42`, currently find the V3 findings STILL PRESENT and
UN-REMEDIATED (L1: `CSS_GENERATED_RS:701` + 7 sibling couriers; L2: 7 ×
`b654562c…`; L3: `RuntimeEmitterKind:40-42`; L4: `ValueRef<G=AnyGrammar>:175` +
test-only `_proof_compiles`; L5: `measure_mbps:3091` warm micro-fixture path; L6:
`find_css_significant`/`find_comment_close` test-only callers) — which is correct:
S-P0 runs BEFORE the PRUNE/GENERALIZE waves, so the surfaces it lenses are the
surfaces those waves will course-correct. The lenses are the falsification tests
that keep the course-correction honest.
