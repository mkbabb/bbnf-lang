# SK-V18 S-P2 / R-F — G6 Grammar-Neutral Scan Primitive (research digest)

Class R-F. Addendum 6 (acceleration-wiring, `a1-six-addenda-lens-registry.md` §L6).
Waves G5/G6. This pass is RESEARCH only — no code, no cargo. Every claim is grounded in
the live tree (paths/lines cited). Host: aarch64 / Apple M5 Max ONLY; x86 is a P1 prune
target and is out of scope for every candidate below.

## 0. The grounded problem (what the primitive must accelerate, and what it must NOT)

The profile ground-truth (`SYNTHESIS-PROFILE.md` §3, `a2-css-hot-leaves.md`) fixes the
target precisely, and it is NARROWER than the addenda prose implies:

- **CSS hot leaf = `CssFullParser::find_component_delim`** scalar byte-at-a-time scan,
  **79.5%** of parser self-time; together with its mutually-recursive inner half
  `consume_balanced_at` (14.6%) = **94.1%** of CSS parser self-time. Live body:
  `css_l4_declaration_values/generated.rs:657-680` (`find_component_delim`) and `:693-713`
  (`consume_balanced_at`), byte-identical across all 7 `css_l4_*/generated.rs`
  (md5 `b654562c…`, re-verified this pass).
- **JSON hot leaf = `parse_object_value_at_direct` / `parse_array_element_at_direct`**
  (`json/generated.rs:823` / `:863`), 91.5% combined — but these are **single-byte
  branch-on-first-byte dispatch**, NOT scans (`:832 match byte { b'{'… }`; the body looks at
  ONE byte and recurses; there is no skip-to-next-significant loop). **JSON has NO hot scan
  leaf.** `json/scan.rs` (`scan_structurals`→`neon::scan`) is **zero-sampled** (A1 §json/scan.rs);
  `skip_ascii_whitespace` (`parse-that-regex/src/lib.rs:119`, the only JSON skip-scan) does not
  clear the top-8 either. The JSON product path is scan-free.

This asymmetry is the crux of R-F. The "same primitive serves CSS AND JSON" neutrality
requirement (prompt; addendum 6) must be honored at the level of the **kernel contract being
alphabet-data**, NOT by manufacturing a JSON hot consumer that does not exist
(that would be an orphan-kernel inversion: wiring a consumer to justify a kernel). The honest
neutrality story is: the kernel is grammar-agnostic *by construction* (caller supplies the
significant byte-set); CSS is the hot consumer that lands it WITH it (no-orphan law); JSON is
served by the *same* `bbnf-simd` primitive surface it already rides for `scan_structurals`
(off the hot path today, but the SAME shared kernel, so no second substrate is created).

### The structural gap between the hot CSS scan and the existing dead NEON (R7)

`find_component_delim` is NOT a flat skip. Per byte it does (generated.rs:662-680):
(a) test `delimiters.contains(byte)` → return; (b) on `' "` → `consume_string_at`; (c) on
`/` followed by `*` → `consume_comment_at`; (d) on `( [ {` → **recurse** into
`consume_balanced_at` (nested bracket consumption, mutually recursive); (e) on `) ] }` →
error; (f) else advance one byte. The dead NEON `runtime_simd::find_css_significant`
(`runtime_simd.rs:169`) is a **flat stop-at-first-member-of `{delims}∪{fixed}`** with NO
recursion, NO string/comment consumption — it was written for a DIFFERENT, flatter function
(R7, confirmed by inspection in `a2` §"the dead NEON does not even cover the hot leaf").
Wiring it as-is accelerates only the inert run *between* structural bytes, not the
balanced-consume recursion. So R-F is a **retarget**, not a wire-as-is.

### What ALREADY exists (the neutral primitive surface — KEEP, do not re-author)

The `bbnf-simd` crate is already a grammar-neutral, caller-data, checkasm-gated primitive
library. The relevant existing kernels, all aarch64 NEON + scalar reference + 14-file checkasm
discipline:

- `byte_class_from_eq_set_64` (`aarch64/byte_class_from_eq_set_64.rs:33`) — 64-byte block →
  u64 mask of `byte ∈ set` (set ≤8), `vceqq_u8`-fan + `vorrq_u8`-reduce + movemask. Scalar
  ref `scalar/byte_class_from_eq_set_64.rs`; checkasm `checkasm_byte_class_from_eq_set_64.rs`.
- `find_ascii_set_member64` (`bbnf-simd/src/lib.rs:209`) — **block-loop already built atop the
  above**: `find first cursor≥from with bytes[cursor] ∈ set` (set ≤8), 64-byte NEON stride +
  scalar tail. This IS the un-recursive core of `find_component_delim`'s inner skip.
- `bracket_depth_mask_64` (`aarch64/bracket_depth_mask_64.rs:31`) — 64-byte block → bracket-
  interior (depth≥1) mask + i32 depth carry, over caller open/close sets ≤4. Already the
  vector engine behind `count_top_level_commas` (`runtime_simd.rs:29`).
- `comment_body_mask_64` (`prim` / `aarch64/comment_body_mask_64.rs`) — 64-byte comment-
  interior mask + `CommentCarry`, over caller open/close digraphs. Behind `find_comment_close`.
- `classify_tbl4` structural-terminator path (`aarch64/classify_tbl4.rs:89`) — table-driven
  structural+terminator masks; the JSON `scan_structurals` rides this (`json/scan.rs:219`).

**The neutrality precedent is already in the tree and is the model to follow:** the generated
CSS already calls a shared neutral primitive — `css_l4_*/generated.rs:809-810`
`fn count_top_level_commas(bytes) { crate::runtime_simd::count_top_level_commas(bytes) }` — a
thin generated wrapper over the `runtime_simd` surface over the `bbnf-simd` kernel. The
generator authors the CALL; the kernel is hand-authored ONCE in `bbnf-simd` and is alphabet-
data. R-F extends exactly this pattern to the 94%-share `find_component_delim` scan.

## 1. Candidate approaches (2-3, with concrete trade-offs)

### Candidate A — "inner-skip vectorize" (retarget `find_ascii_set_member64` to the scalar shell)

Keep the scalar `find_component_delim`/`consume_balanced_at` recursive CONTROL shell exactly
as generated; replace ONLY the per-byte `_ => pos + 1` inert advance with a vectorized
"advance to next byte ∈ significant-set" call, where significant-set = `delimiters ∪
{' " / ( [ { ) ] }`. This is `find_ascii_set_member64` (already in `bbnf-simd/src/lib.rs:209`),
lifted to a `runtime_simd` entry whose set is **caller data** (the generated grammar passes its
delimiter set; the fixed structural family is the recognizer's own constant). The recursion,
string-skip, comment-skip stay scalar (they are 1-2% of self-time each; not the hot leaf).

- **How it serves CSS:** directly — the 79.5% leaf IS the inert-run skip between structural
  bytes; the block-loop replaces the byte-at-a-time `delimiters.contains` + `pos+1` (generated.rs
  :663-676) with a 64-wide `vceqq` fan. The recursion fan-out (`consume_balanced_at`) reuses the
  SAME entry for its own inert advance (:709).
- **How it serves JSON (neutrality):** the same `bbnf-simd::find_ascii_set_member64` / the
  `byte_class_from_eq_set_64` kernel underneath is the SAME kernel JSON's `scan_structurals`
  already classifies with (`json/scan.rs` rides `classify_tbl4` + `byte_class_from_eq_set_64`
  in the checkasm corpus parity, `checkasm_byte_class_from_eq_set_64.rs:320 b"{}[],:"`). No
  JSON hot consumer is manufactured; the neutrality is in the shared kernel + shared eq-set
  contract, not a fabricated JSON caller.
- **Trade-offs.** + Smallest surface: the kernel ALREADY EXISTS and is checkasm-gated; R-F is
  a `runtime_simd` wrapper + a generated call-site swap + a dav1d scalar-reference/parity test.
  + Bit-identical semantics are trivial to prove (the scalar `find_component_delim` inner loop
  IS the reference). + No new NEON authored → no new checkasm kernel-file (reuses
  `checkasm_byte_class_from_eq_set_64`). − Does NOT accelerate the string/comment/balanced
  consumption itself (only the inert runs between them) — but those are the cold 14.6%/<2%
  tail, and `consume_balanced_at`'s OWN inert advance is covered by the same entry, so the
  realized share is most of the 94%. − Set may exceed 8 (delims ≤4 + fixed 9 = ≤13): handled
  exactly as `find_css_significant` already does (`runtime_simd.rs:180-199` — two ≤8 eq-set
  fans OR-reduced); this is a SOLVED problem in the dead kernel, salvaged.

### Candidate B — "balanced-consume bitmap" (depth-mask the whole component in one pass)

Replace BOTH `find_component_delim` and `consume_balanced_at` with a bitmap-driven scan:
per 64-byte block compute (i) significant-byte mask (`byte_class_from_eq_set_64`), (ii)
bracket-interior+depth mask (`bracket_depth_mask_64`, already proven), (iii) string-interior
mask + (iv) comment-interior mask (`comment_body_mask_64`, already proven), AND them to find
the first TOP-LEVEL delimiter (delimiter bit ∧ ¬bracket-interior ∧ ¬string ∧ ¬comment). This
replaces the recursion with depth-carry arithmetic — the same shape that made
`count_top_level_commas` (`runtime_simd.rs:29-78`) work.

- **How it serves CSS:** covers the FULL 94.1% (both leaves) — the balanced-consume recursion
  becomes a depth-carry, no scalar recursion at all on the common path.
- **How it serves JSON:** the four masks (eq-set, bracket-depth, string, escape) are EXACTLY
  the masks JSON's `neon::scan` already composes (`json/scan.rs:224-264`: quotes, escape_mask,
  prefix_xor string-body, punctuation). So Candidate B's mask-composition vocabulary is
  literally the shared JSON scan vocabulary — genuine neutrality, and it would let a future
  pass move the JSON direct path onto a structural pre-scan with the SAME composer.
- **Trade-offs.** + Highest ceiling (full 94%, including the recursion). + Maximally neutral
  (the mask-AND composer is the JSON scan's own vocabulary). − Largest semantic-parity risk:
  must reproduce `find_component_delim`'s EXACT error contract (unexpected-closing-delimiter at
  :673/:706, unclosed-component at :712, unclosed-comment/string) bit-for-bit from masks — the
  depth-underflow and error-position semantics are subtle and the checkasm differential must
  cover adversarial nesting/escape windows. − Composes 3-4 kernels per block where Candidate A
  composes 1; on the real corpora the inert runs dominate, so B's extra mask work may not pay
  off vs A's single fan until brackets are dense. − Bigger surface = bigger G6 risk under the
  hard cap.

### Candidate C — "table-classifier unify" (route CSS through the `classify_tbl4` JSON engine)

Generalize the JSON `classify_structural_terminator_block_from_table` path
(`classify_tbl4.rs:89`) into the single block classifier for BOTH grammars: build the lo6 class
table from the grammar's significant alphabet (`build_lo6_table`, `:8`), and have CSS consume
the `structural_mask` exactly as JSON consumes it, with the scalar shell handling
recursion/strings on the returned positions.

- **How it serves both:** one literal kernel (`classify_block_from_table`) for JSON structural
  scan AND CSS significant scan — the strongest single-emitter story.
- **Trade-offs.** + One kernel, maximal code unification. − The lo6 table is `admissible` only
  if no two alphabet members collide mod 64 (`dispatch.rs:101 lo6_table_admissible`); the CSS
  significant family `' " / ( ) [ ] { } : ; ,` must be checked for lo6 collisions, and adding
  caller delimiters at runtime can collide — a correctness hazard `byte_class_from_eq_set_64`
  (Candidate A) does NOT have (eq-set has no lo6 constraint). − Forces CSS onto a table-build
  per parse (or a cached table), heavier than A's two static fans. − Highest coupling: a JSON
  scan-path change now perturbs CSS. Rejected as the primary on KISS + the lo6-collision hazard,
  but noted as the eventual unification if both grammars move to a shared structural pre-scan.

## 2. RECOMMENDATION — Candidate A (inner-skip vectorize), with B's masks as the documented
upgrade path

**Recommend Candidate A.** Rationale, all grounded:

1. **No-orphan-kernel compliance is automatic and cheapest.** The kernel
   (`byte_class_from_eq_set_64` / `find_ascii_set_member64`) ALREADY EXISTS, is checkasm-gated,
   and is alphabet-data. R-F's deliverable is a `runtime_simd` retarget entry (salvaging the
   set-split logic already written in the dead `find_css_significant:180-204`) + the generated
   call-site swap, landing in the SAME commit as the consumer (`css_l4_*/generated.rs` inner
   loop) — satisfying the addendum-6 hard law ("land WITH the hot consumer, same commit") with
   the minimum new surface, under the G6 hard cap.
2. **It directly hits the measured 79.5% leaf** (the inert-run skip) and, via the same entry on
   `consume_balanced_at`'s advance, most of the 94.1%. It is the WIRE the profile mandates
   (94.1% ≫ ~8% threshold; RETIRE rejected per `a2` because deleting a kernel that targets a
   94% path is wrong).
3. **Grammar-neutral by the existing precedent.** The set is caller data (CSS passes its
   delimiter set; the structural family is the recognizer constant), exactly as the generated
   `count_top_level_commas` wrapper already calls the neutral `runtime_simd` surface
   (`generated.rs:809-810`). The generator emits a CALL; it does not author vector code per
   grammar — so it does NOT re-fork the shape G3 un-forks (the P3-collapse + single-emitter law).
4. **JSON neutrality is honest, not fabricated.** The same `bbnf-simd` eq-set kernel underlies
   JSON's `scan_structurals` corpus-parity (`checkasm_byte_class_from_eq_set_64.rs:320`). We do
   NOT manufacture a JSON hot consumer (none exists — JSON product is scan-free, A1). Neutrality
   = shared kernel + shared eq-set contract, the truthful claim addendum 6 admits.
5. **dav1d discipline is already seeded.** Scalar reference = the existing
   `find_component_delim` inner loop itself (and `significant_ref`, `lib.rs:506`); checkasm
   differential parity = extend `checkasm_byte_class_from_eq_set_64` + the runtime parity guard
   `neon_significant_skip_matches_scalar` (the `#[test] fn` at `lib.rs:562`, whose lone
   `find_css_significant` call site is `lib.rs:574` — the `#[cfg(test)]`-only caller the L6
   admission census flags today, a1 §L6) retargeted to the recursive shell over the real corpora;
   aarch64 NEON/dotprod only, no x86. Post-G6 the L6 census target moves: the admission-proof
   caller must be a `runtime_simd::find_…` call in a `grammars/*/generated.rs` hot loop, NOT this
   `lib.rs:574` test site.

Candidate B is the **documented upgrade path** (record in the G6 SPEC, do not build now): if a
later measurement shows the bracket-recursion tail (`consume_balanced_at` 14.6%) dominates after
A lands, lift to the bracket-depth-mask composer — its mask vocabulary is already the JSON scan
vocabulary, so B is the genuine convergence point, but it carries the higher parity risk and
larger surface that the hard cap and abrogate-before-patch discipline say to defer until
measured. Candidate C is rejected (lo6-collision hazard + JSON↔CSS coupling).

## 3. Key risk

**The dominant risk is parity-under-retarget, not the kernel.** Candidate A keeps the recursive
scalar shell, so the kernel parity is trivial — but the retarget must preserve
`find_component_delim`'s exact contract at three seams: (a) the significant set spans ≤13 bytes
(>8 eq-set cap) → must use the two-fan OR-reduce already in the dead `find_css_significant`
(`runtime_simd.rs:180-204`), and the salvage must be byte-exact; (b) the block-skip must NOT
skip PAST a `(`/`[`/`{`/`'`/`"`/`/` that requires recursion/consumption — i.e. the significant
set must INCLUDE the structural family, so the vector skip stops AT them and hands control back
to the scalar shell (this is why it is "find significant", not "find delimiter"); (c) the error
positions (:673 unexpected-close, :712 unclosed) must be reproduced from the scalar shell, not
the kernel. The checkasm differential + the `neon_significant_skip_matches_scalar` guard
(retargeted to the recursive shell + run over the real 71KB-495KB corpora, NOT the micro-cases
at `lib.rs:564-570`) is the gate. Secondary risk: realized speedup is bounded by inert-run
length on the corpora — if components are short (dense delimiters) the 64-wide fan amortizes
poorly; this is a MEASUREMENT to confirm post-wire, not a correctness risk.

## 4. Prune / sequencing dependency (binding)

**P1 → P3 → G6, in that order; G6 is a hard descendant of both.**

- **P1 (x86 prune, R8) BEFORE any kernel work.** The retarget is aarch64-only; the x86
  `byte_class_from_eq_set_64` surface (`prim` `#[cfg(avx512bw)]` arm) and `ext/x86` are pruned
  first so the kernel surface is single-arch when R-F retargets (`SYNTHESIS-AUDIT §4 PRUNE-1`).
- **P3 (collapse the 7 css_l4 replicas, R4) BEFORE G6 — the load-bearing sequencing fact.**
  `find_component_delim` is **replicated byte-identically across 7 generated.rs**
  (md5 `b654562c…`, verified). The retargeted call site must land into the **P3-COLLAPSED single
  CSS scan**, NOT be re-emitted 7 ways — re-emitting bespoke vector calls per-replica pre-collapse
  would re-fork the exact shape G3 un-forks and G6 polices (`SYNTHESIS-PROFILE §3` neutrality
  constraint; `a2` §1). The primitive itself stays singular in `bbnf-simd`/`runtime_simd`
  regardless; the GENERATED CALL SITE must be singular too, which only P3 guarantees.
- **S-P1 profile is the standing gate for the WIRE branch** (sequencing fact 4,
  `SYNTHESIS-AUDIT §5`): G6 may author the retarget ONLY because the 94.1% hot-leaf measurement
  exists (it does — `a2`). No orphan: the consumer (`generated.rs` inner loop) and the
  `runtime_simd` entry land in ONE commit; `acceleration_at_admission == admission`
  (`a1-six-addenda §L6`), proven by the generated-`generated.rs` caller census (the lens L6
  check (b): `rg runtime_simd::find_..._significant …/grammars/*/generated.rs` MUST be non-empty
  post-G6), not by a `#[cfg(test)]` caller.
- **G3 (un-fork emitter) BEFORE G6's call-site emission** in the dependency graph
  (`SYNTHESIS-AUDIT §5`: PRUNE→G1→G2→G3→G4→G5/G6), so the single grammar-agnostic emitter is the
  thing that emits the `runtime_simd` call, not a CSS-family fork. The dead
  `find_css_significant`/`find_comment_close` (R7, R10/R11 mislabels) are SALVAGED-or-deleted in
  the same wave: salvage the set-split logic into the retarget entry; retire `find_comment_close`
  ONLY if retargeting to the comment-consume proves unsafe (`a2` §verdict) — gated on the
  samply non-top-N measurement, never a bare assertion.

---
Bottom line: the neutral scan primitive ALREADY EXISTS as alphabet-data in `bbnf-simd`; R-F is a
RETARGET (salvage the dead set-split, swap the generated inner-skip call) onto the P3-collapsed
single CSS scan, landed with its consumer, neutral because the kernel is the same eq-set kernel
JSON already rides — not because a JSON hot consumer is fabricated (JSON product is scan-free).
