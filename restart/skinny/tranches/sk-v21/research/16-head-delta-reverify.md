# HEAD-delta re-verification of the SK-V20 load-bearing adjudications

Verified at `734bfec51` (the SK-V21 charter commit; nothing else has landed since SK-V20's
close at `05302109d`). Every item below was re-derived from the file at current HEAD, not
carried forward from the prior report's prose.

## 1. Lock-14 gate scope — CONFIRMED, unchanged

The live gate is `crates/ir/tests/lock14_grammar_generalization.rs` (149 lines). Its scan
surface is hard-coded to exactly two roots: `generic_crate_src_roots()` at
`crates/ir/tests/lock14_grammar_generalization.rs:83-87` returns
`vec![ir_crate.join("src"), crates_dir.join("analysis").join("src")]` — i.e. `crates/ir/src`
and `crates/analysis/src` only. The token list is a hand-rostered 17-token `ROSTER` const
naming nine grammars' parser/grammar struct idents (`lock14_grammar_generalization.rs:26-44`:
`JsonParser`, `JsonGrammar`, `GoogleSheetsParser`, …, `CssPrettyGrammar`). `crates/core`,
`xtask`, and the entire `skinny/` tree (including its own codegen and its own generated
grammars) sit structurally outside the gate's reach — the gate cannot see a grammar-name
leak anywhere in those trees, and a new tenth grammar's idents would also silently escape it
until someone manually widens `ROSTER`. Nothing about this has changed since SK-V20.

## 2. perf_cost literal-0 — CORRECTED: 1 production site, not "~8"

`rg -n "perf_cost"` across `skinny/` and `crates/` returns 11 hits in three files:
`skinny/crates/passes/src/backend_egraph.rs`, `skinny/crates/passes/src/decision_csp.rs`,
`skinny/crates/passes/src/lib.rs`. Of these, 7 are the literal `perf_cost: 0`:
`lib.rs:590`, `lib.rs:2023`, `lib.rs:2035`, `lib.rs:2062`, `lib.rs:2083`,
`decision_csp.rs:242`, `decision_csp.rs:254`.

Checked against `#[cfg(test)]` boundaries (`lib.rs:1674`, `lib.rs:2012`, `lib.rs:2052`,
`decision_csp.rs:226` open `mod tests` blocks that run through EOF): **only `lib.rs:590` sits
in production code** — it is the one live `BackendCandidate` constructor
(`crate::backend_egraph::BackendCandidate { … perf_cost: 0, capacity_cost:
candidate_capacity_cost(...), static_size_cost: candidate_static_size_cost(...), … }`,
`lib.rs:580-593`). The other six literal-0 sites are test-fixture `BackendCandidate` literals
inside `mod tests`. `backend_egraph.rs`'s four `perf_cost` occurrences are the struct field
declaration and genuine field-to-field passthrough (`candidate.perf_cost` at
`backend_egraph.rs:170,284`) — never a literal.

So the finding survives but is narrower than SK-V20 stated: the decision engine's cost model
has exactly **one** production call site where `perf_cost` is a dead constant zero rather than
a computed value (`skinny/crates/passes/src/lib.rs:590`); `capacity_cost` and
`static_size_cost` beside it on the same struct literal ARE computed. The field itself
(`backend_egraph.rs:21`) is plumbed end-to-end and would carry real values the instant the one
production constructor stops hard-coding it — this is a one-line fix, not an eight-site one.

## 3. Live JSON NEON scanner is dead — CONFIRMED, unchanged, self-documented

`skinny/crates/runtime/src/grammars/json/scan.rs:1-13` carries its own disposition comment:
"the live default `CapacityPlan` is `GrowOnly` … which returns a constant 256 and touches NONE
of the structural scanners below." The dispatch is literal at `scan.rs:59-65`:

```
pub fn structural_capacity_for(plan: CapacityPlan, source: &[u8]) -> usize {
    match plan {
        CapacityPlan::Sampled => sampled_structural_capacity(source),
        CapacityPlan::Exact => exact_structural_count(source) + 8,
        CapacityPlan::OneShotSimd => scan_structurals(source).positions().len() + 8,
        CapacityPlan::GrowOnly => 256,
    }
}
```

`CapacityPlan::from_env` (`skinny/crates/runtime/src/tape/assembler.rs:21-28`) maps
`Some("D") | Some("grow") | Some("growth") | None => CapacityPlan::GrowOnly` — i.e. with
`BBNF_TAPE_CAPACITY` unset (the default, live path) the plan is `GrowOnly`, so
`scan_structurals` / the aarch64 `neon::scan` / `sampled_structural_capacity` /
`exact_structural_count` are reachable only via an explicit non-default env override or the
bench/parity harnesses. This is unchanged from SK-V20's characterization.

## 4. Three value substrates / duplicate path surface / dead ser::Deserializer — CONFIRMED

**Value substrates**, three distinct types with no shared trait:
- `crates/ir/src/vm/interpreter/value.rs:52` — `pub enum Value` (generic parse-tree node:
  Span/Tagged/Array/Nil), the interpreter's own substrate.
- `crates/core/src/runtime/{json,css_l4,math,google_sheets,bnf,csv,css_pretty,bbnf,ebnf}/value.rs`
  — nine per-grammar generated typed `Value` enums, a second substrate family.
- `serde_json::Value`, used directly at `skinny/crates/bbnf/src/lib.rs:83` inside
  `pub fn parse_owned(input: String) -> Result<serde_json::Value, Error>` — a third,
  wholly external substrate leaking into the skinny public API surface, still present.

**Path surface**: two structurally identical `Path<'a>` types are independently defined —
`crates/core/src/path/ir.rs:74` and `crates/core/src/runtime/path.rs:56` are both, verbatim,
`pub struct Path<'a>(pub &'a [PathSegment<'a>]);`. `crates/core/src/path/mod.rs:32-36`
documents this explicitly as deliberate ("The existing borrowed `runtime::path::Path<'a>`
stays in place per the W2 modify-carve rule; this module is the typed-path surface sibling")
— i.e. the duplication is a known, accepted-as-permanent fork, not an oversight in progress of
being resolved. Layered on top, `crates/core/src/path/ascent.rs` exports three
`AscentStrategy` impls (`DefaultAscent`, `HybridSidecar`, `InStructPointer`,
`path/mod.rs:49`) and Google Sheets carries its own fourth resolution surface at
`crates/core/src/runtime/google_sheets/document/path_query.rs`. The "four path APIs" framing
holds.

**Dead `ser::Deserializer`**: `crates/ser/src/traits.rs:113` defines `pub trait
Deserializer<'a>`; `crates/ser/src/slice.rs:6,11,21` implements `SliceDeserializer`. The only
consumer of `bbnf-ser` is `crates/core` (`crates/core/Cargo.toml:17`), and every reference to
the crate from `crates/core/src/generate/serialize/mod.rs` is to `bbnf_ser::Serializer` /
`bbnf_ser::StringSerializer` only (lines 5, 34, 47, 77, 81, 91, 95) — zero occurrences of
`Deserializer` or `SliceDeserializer` outside `crates/ser` itself. The deserialize half of the
crate is fully-implemented, fully dead code. Confirmed unchanged.

## 5. Inline `#[cfg(test)]` in src/ — RECOUNTED, and it is worse than previously carried

Repo-wide (`rg -c '^\s*#\[cfg\(test\)\]' --glob '**/src/**/*.rs'`): **42 files, 84 occurrences.**

`crates/core` (the audited "7 violations" set) is **unchanged, still exactly 7**:
`backend/rust/analysis/inline/mod.rs`, `path/cursor.rs`, `path/executor.rs`, `path/schema.rs`,
`runtime/bbnf/parse_with.rs`, `runtime/google_sheets/parse_with.rs`,
`runtime/json/parse_with.rs`.

SK-V20's `research/06-precepts-recap.md:22` reported "Two skinny violations:
`passes/src/decision_csp.rs:226`, `grammar/src/lib.rs:602`." Re-scanning the full `skinny/`
tree at HEAD finds far more than two, even excluding the bench (`bbnf-bench`, 13 files) and
tooling (`xtask`, 2 files) crates: `bbnf-regex/src/lib.rs:259`, `bbnf/src/lib.rs:91`,
`codegen/src/lib.rs:290,1161,1245`, `codegen/src/lower/css_scan.rs`,
`codegen/src/lower/sink_only.rs`, `grammar/src/lib.rs:1680,1728,1760`, `ir/src/lib.rs:602`,
`passes/src/decision_csp.rs:226`, `passes/src/lib.rs:1674` (plus two test-fixture-only blocks
at 2012/2052), `runtime/src/lib.rs:32`, `runtime/src/grammars/json/scan.rs` — 14 non-bench,
non-tooling production files. One of these, `ir/src/lib.rs:602`, sits at the exact line
number SK-V20 cited for "`grammar/src/lib.rs:602`" — that earlier citation appears to have
named the wrong crate for what is otherwise the same violation (a small transcription slip,
not a fixed-then-regressed file); `grammar/src/lib.rs`'s own three `#[cfg(test)]` blocks
(1680/1728/1760) are separate, additional violations SK-V20 did not list at all. Net: the
`no-inline-tests` precept (N12 in `restart/skinny/tranches/sk-v20/DEFERRED-LEDGER.md:48`) is
not merely "not trending down" as SK-V20 phrased it — a full-tree accounting puts it at 7
core + at least 14 skinny (21+), well above the "7 + 2" figure carried forward, whether because
the prior audit under-scoped its skinny search or the count has genuinely grown.

## 6. crates/simd-scan x86/wasm dead weight + emitter-stamped dead AVX2 — CONFIRMED

`crates/simd-scan/src/` still ships `avx2.rs` (370 lines), `avx512.rs` (149 lines), and
`wasm.rs` (433 lines) alongside the one live `neon.rs` (719 lines) — 952 lines of code for
architectures the charter's own benchmark constraint excludes ("aarch64/Apple-M only",
`PASS-ALPHA.md:62`; SK-V19's global gate likewise states "aarch64-only… x86/SVE remain
diagnostic", `restart/MASTER-PLAN.md:1017`).

Separately, the Rust backend emitter unconditionally stamps x86_64-gated AVX2 branches into
the shared struct-support template at
`crates/core/src/backend/rust/emitter/shapes/dispatcher/support.rs:541,595,644,683`
(`#[cfg(all(target_arch = "x86_64", target_feature = "avx2"))] … unsafe fn
nospace_bitmap_64_avx2` / `first_quote_or_backslash_avx2`). This propagates into every
generated grammar file: `rg -c avx2 crates/core/src/grammar/generated/*.rs` returns 92 total
hits across `csv.rs`, `google_sheets.rs`, `bnf.rs`, `json.rs` (and by the same emitter path,
the rest of the nine). On the project's only live target (aarch64) these branches compile out
entirely, but they are still authored, generated, and shipped as dead source in every one of
the nine grammar files — confirmed unchanged from SK-V20.

## 7. css_l4.rs generated line count — CONFIRMED, unchanged: 108,406 lines

`wc -l crates/core/src/grammar/generated/css_l4.rs` → **108,406**, matching SK-V20's
`research/16-generated-size.md:43` exactly. Current sibling sizes: `bbnf.rs` 21,557,
`google_sheets.rs` 13,861, `css_pretty.rs` 9,052, `ebnf.rs` 7,659, `json.rs` 3,505, `bnf.rs`
3,304, `csv.rs` 1,702, `math.rs` 875 — total generated corpus **169,956 lines**. `css_l4.rs`
alone is ~64% of all generated grammar code and roughly 5x the next-largest file. No change.

## 8. skinny json_templates false-generated couriers — CONFIRMED, unchanged

`skinny/crates/codegen/src/json_templates/{parser,value,view,generated}.rs` each open with
the literal header `// @generated by skinny bbnf-codegen; do not edit by hand.` (line 1 of
each). But these files are not codegen *output* — they are codegen *input*: they are pulled in
verbatim via `include_str!` by `skinny/crates/codegen/src/runtime_generator.rs:101,107,122,
126,130,281` (`include_str!("json_templates/generated.rs")`,
`include_str!("json_templates/parser.rs")`, etc.), string-normalized, and spliced into the
actual generated output the codegen crate produces for consumers. No tool in `skinny/xtask` or
`crates/codegen/src/lib.rs` writes to `json_templates/` — a repo-wide `grep -rln
json_templates` outside `codegen/src/{runtime_generator.rs,json_templates/}` and one
`bbnf-bench` consumer turns up nothing. These are hand-maintained template sources carrying a
provenance header that asserts the opposite of what is true — a "false-generated courier" in
the precise sense: the header claims machine authorship for a file no machine has ever
written.

## 9. MASTER-PLAN receiver-row drift on MP.SK19.W6 — CONFIRMED, and now precisely dated

`restart/MASTER-PLAN.md:1041` (the `§13.6` receiver table) declares `MP.SK19.W6` to be "Lock-2
`StructLayout` canonical-name reconcile" (F9 / LAC-2F-FOLD-05), sized against a stated baseline
of `grep StructLayout crates/`=960 vs `grep backend_shape|LayoutFacts crates/`=0. Both counts
are unchanged at HEAD (`rg -c StructLayout crates/` sums to 960; `rg -c
'backend_shape|LayoutFacts' crates/` matches nothing) — i.e. the literal W6 receiver as
written in the plan has had zero progress.

Meanwhile two commits have landed under the "W6" label carrying an entirely different scope:
`36ca10646` ("fold(sk-v19-w6abe): collapse the named-grammar leaks — metadata-driven manifest,
CSS host relocation, the live Lock-14 gate") and `f4633d468` ("fold(sk-v19-w6c): re-key the
BBNF shape recognizer on structure, not rule name"). Their content — the production
`strategy.rs` manifest becoming build-script-generated from workspace metadata, `css_types.rs`
deletion and host-module relocation, and the live Lock-14 self-gate audited in item 1 above —
is the "zero grammar code in generic crates" leak cluster, not the `StructLayout` rename the
MASTER-PLAN's `§13.6` table names for W6. `restart/MASTER-PLAN.md` was never updated after
either commit: the `§13.6` row at line 1041 and its companion citation at line 1363 both still
describe only the un-landed `StructLayout` rename, with no cross-reference to the sub-wave
labels (`w6abe`, `w6c`) that actually consumed the "W6" name. The drift is real and now
dateable: `w6abe` landed 2026-06-10, `w6c` landed 2026-06-07, and the MASTER-PLAN receiver-row
prose predates both and has not been reconciled with either since.

## Uncommitted playground/ changes — mixed: one is safe cleanup, one is pure junk

`git status --short` shows `M playground/vite.config.ts` and `?? playground/test-results/`.

`vite.config.ts`'s diff removes a manual `resolve.alias` entry for `"@mkbabb/keyframes.js"` →
`../node_modules/@mkbabb/keyframes.js/dist/keyframes.js`. The package is still declared in
`playground/package.json:14` (`"@mkbabb/keyframes.js": "^2.2.0"`) and is still imported by two
live composables — `playground/src/composables/useScrollMorph.ts:2` and
`playground/src/components/landing/useHeroSequence.ts:2`
(`import { ScrollTimeline } from "@mkbabb/keyframes.js"`). Checking the installed package
itself: `node_modules/@mkbabb/keyframes.js/package.json` declares proper `"main":
"./dist/keyframes.js"` and matching `"exports"` fields, meaning standard Node/Vite module
resolution should already resolve the bare specifier to the same `dist/keyframes.js` path the
removed alias pointed at — the alias reads as redundant, and its removal looks like legitimate,
low-risk cleanup rather than an accidental regression. This auditor did not run the dev
server or a build (out of brief), so this is a plausibility read, not a verified-green one; it
should be spot-checked with `vite build`/`vite dev` before committing.

`playground/test-results/.last-run.json` is a Playwright test-run artifact
(`{"status": "passed", "failedTests": []}`, 4.0K total directory size) that is untracked and
not covered by any `.gitignore` in the repo. This is pure junk — a local test-run byproduct,
not a deliverable — and should be added to `.gitignore` rather than committed or carried
forward as a pending diff.
