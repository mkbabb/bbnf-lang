# HARDENING-WORKSPACE-SK-V1

## §1 Target identification

- Path: `restart/skinny/WORKSPACE.md`
- Lines audited: 1–613 (post-redress; SHA tracked by orchestrator)
- Cycle: SK-V1 (skinny first-pass)
- Lens stack applied: Lanes 1, 3, 4, 5, 6, 7, 8, 9 (Lane 2 N/A — single-wave); Lenses F, G, H, I, J, K; Lenses L, M, N
- Time consumed: 36 min (commit-pace per skinny HARDENING.md §9)

The target is the workspace + LOC-budget quadrant of the skinny corpus. Sister quadrants (`SUBSTRATE.md`, `COMPILER.md`, `BENCH.md`, `INDEX.md`) own internal contracts referenced here; this audit treats those references as ground truth and probes only WORKSPACE-resident commitments.

## §2 Cohort verdict

| Lane / Lens | Verdict | KEEP | REINVENT | DISCARD | Recommendation |
|---|---|---:|---:|---:|---|
| Lane 1 — Lock-Adherence | violated-with-recommendation | 11 | 2 | 0 | Two Lock-13 edge cases (§4.6 generated subdir; §4.4 `layout/` subnesting) need explicit ratification; Lock-14 `host_registry = "skinny-none"` is a non-canonical sentinel — fence under §5.6 or rename. |
| Lane 2 — Sequencing | N/A | — | — | — | Single-wave skinny. |
| Lane 3 — Cohesion | violated-with-recommendation | 7 | 2 | 0 | §1.1 / §7 / §8 disposition tables triple-state crate fates with three mildly-divergent vocabularies; `pipeline` shim is named in two places that disagree (§1.1 says inlined into `xtask::regen` + `bbnf::compile`; §7 says `bbnf/src/parse/pipeline.rs`). |
| Lane 4 — SOTA Anchoring | honoured | 4 | 0 | 0 | WORKSPACE forwards SOTA gates to BENCH.md; the only WORKSPACE perf gate is the 90-second build-time target which is engineering, not Lock-8. Lock 8 honour is delegated correctly. |
| Lane 5 — Grammar-authoritative | violated-with-recommendation | 6 | 1 | 0 | `runtime/src/grammars/json/` is grammar-named in WORKSPACE — but Lock 14 permits per-grammar generated dirs; verify the metadata-driven generation contract is the only path. The `host_fns.default_registry = "host::primitives"` line in §3 references a path that does not exist in the skinny crate set. |
| Lane 6 — Generated-Code + LOC Budget | violated-with-recommendation | 5 | 2 | 0 | The 31,400 ceiling holds with thin head-room; `bbnf-bench` budget at 2,000 LOC is now provably tight given BENCH §11.1 indicates "target ≤ ~2,200 LOC" (BENCH:1582). Track 2 cap removed in BENCH.md but WORKSPACE §1.1, §6, §7, §10 still cite "≤ 500 LOC" four times — STALE. |
| Lane 7 — Friction Forecast | silent (must add) | 0 | 0 | 0 | WORKSPACE adds dev-iteration loop (§9) but is silent on user-facing failure surfaces it owns: `passes` budget overrun (§2.1), `lint-loc` xtask exit codes, the 90-second build miss diagnostic, and the package-rename-via-`package =` workspace alias in §3 (a known footgun). |
| Lane 8 — Carry & Deferral | violated-with-recommendation | 6 | 2 | 0 | §1.1 routes `parse-that` to `parse-that-regex`, but Lock 11 amendment names `parse-that` as the published parser combinator + regex family — the skinny implicitly drops the parser-combinator role with no graduation receiver named. §10 routes nine-grammar generated-LOC enforcement "to F.W3" — receiver named, blocker absent (no specific evidence the F tranche owns the regen-equality scaling cost). |
| Lane 9 — Greenfield Discipline | honoured | 9 | 1 | 0 | Skinny consciously avoids the OpenFrame relics; honoured. One contrivance flagged in §3: `pratt = "off"`, `literal_trie = "off"`, `regex_prefilter = "json-regex-only"` are five enum-string overrides that the metadata validator must accept; ARCH §5 schema names `auto` as the canonical default. The `"off"` and `"json-regex-only"` strings are skinny-specific and must be either added to the canonical schema enum (cross-quadrant invariant) or routed through a skinny-mode validator branch. |
| Lens F — LLM bias | honoured-with-rec | 0 | 1 | 0 | Two pseudo-precise numerics not anchored: §2 "≤ 4,000 generated LOC" cites PASS-2:432 (verifiable); §9 "≤ 90s clean release on M1 Pro" has no measurement provenance — engineering gate without baseline. |
| Lens G — Overfitting | honoured | 0 | 0 | 0 | Skinny is JSON-only by design; over-fit is the design. |
| Lens H — Hallucination + provenance | honoured-with-rec | 0 | 1 | 0 | §2 cites `restart/audit/pass-2-codegen/PASS-2.md:432` for the 3,500 LOC + 2% baseline — orchestrator should verify the line; §10 cites `PASS-2.md:435` for the 172,125 LOC ceiling — same. |
| Lens I — Contrivance | honoured-with-rec | 8 | 2 | 0 | `simd-scan` keeps `avx512/` and `wasm/` as **dead code** "for parity" (§4.8); this is contrivance — Lock 13 demands cohesive encapsulation; carrying dead arches to satisfy a Lock 13 4-10 child count is ceremony. The `bridge/` directory in `passes/` (§4.4) is "intentionally vestigial" — same shape. |
| Lens J — Host-language leverage | honoured-with-rec | 4 | 1 | 0 | Six dev-deps in BENCH (§7.6) — sonic-rs, simd-json, serde_json, criterion, mimalloc, blake3 — all genuinely needed. WORKSPACE-side: `sha2 = "0.10"` (workspace.dep §3) duplicates BENCH-side `blake3 = "1"`; one hash family suffices for fixture verification. |
| Lens K — Meta-grammar discipline | honoured | 4 | 0 | 0 | WORKSPACE owns no meta-grammar surface; the metadata schema deviation rows are downstream of COMPILER. |
| Lens L — Premise fidelity | violated-with-recommendation | 6 | 2 | 0 | `passes` 6,000 LOC budget is FAITHFUL only conditional on §2.1; §10 lists eight omissions but the **HM-only deviation** is not in the omissions table (it surfaces in §2.1 as a budget-binding signal but is absent from §10's row labelled "GADT / DK13 / OutsideIn / CSP type-system"). The Box<[T]> sealing deviation (SUBSTRATE §1.2) is absent from §10 entirely. |
| Lens M — Falsifiability | N/A (delegated) | — | — | — | Threshold matrix lives in BENCH.md §6; WORKSPACE delegates correctly. |
| Lens N — Graduation mechanicality | violated-with-recommendation | 4 | 3 | 0 | §8.1 mechanical-closure table has FIVE rows; INDEX.md ledger has SEVEN rows (Box<[T]>; HM hierarchy inversion are the two redress additions). WORKSPACE §8.1 missing the Box<[T]> row — the two-Vec-vs-Box snapshot inversion's V1 closure cost is unenumerated here. The HM-as-top-level row in §8.1 ("HM hierarchy inversion") states "150-300 LOC wrapper" — defensible but not steelmanned against the case where DK13/GADT/CSP additions discover that `algorithm_w.rs` was structured for top-level invocation and needs internal restructuring. |

**Final decision: SK-AMENDMENT-REQUIRED-NARROW.**

The skinny WORKSPACE survives the lens stack with KEEP-dominant verdicts but carries **eight surgical edits** that are mechanical (not re-architecture). The dominant fault classes are (a) **stale Track-2 LOC ceiling references** that the BENCH redress dropped; (b) **deviation-ledger drift** between INDEX, SUBSTRATE, and WORKSPACE §8.1 / §10; (c) **`bbnf-bench` budget arithmetic** that no longer balances post-BENCH refresh. None are architectural; all close in narrow surgery. SK-V2 dispatch after punch-list application.

---

## §3 Lane 1 — Lock-Adherence

The skinny respects the 14 locks by inheritance from V1 (`restart/skinny/INDEX.md` cross-quadrant invariants 1-6). WORKSPACE.md is the structural quadrant; locks 1, 5, 13, 14 are load-bearing here.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| §1 (table, line 27) | 10-crate set (closed) | Adding crate is amendment, not workspace change. | Honours Lock 14's "config + grammar source, no code change" principle by closing the crate boundary. Honours Lock 13's god-directory rule via small crate count. | Closed set forces inlined shims (`source_stub`, `host_stubs`, `diagnostic`); each is a Lock-13-adjacent risk if shim grows past 500 LOC. | Steelman: the skinny might need a separate `host_decode` crate if the `decode_string` SUBSTRATE path turns out to require its own visibility. Counter: SUBSTRATE.md owns that path; visibility is enforced via `runtime::tape::decode_string` not a separate crate. KEEP. | KEEP |
| §3 (line 137) | `[workspace.metadata.bbnf]` block with `host_registry = "skinny-none"` | Sentinel value not in ARCH §5 schema (`default` / specific name). | Names the deliberate cut; metadata validator can detect it. | `"skinny-none"` is not in the ARCH §5 enum; the metadata validator (`grammar::metadata`) per ARCH §5 line 729 will reject it unless extended. Cross-quadrant invariant violation against Lock 14's settled schema. | Steelman: extend the schema to admit `"skinny-none"` for the duration of skinny life; mechanical at V1 graduation. Counter: the validator is itself in `grammar` crate — which the skinny has at "partial" status (§1 row 2). The validator must be extended in skinny scope before this string is acceptable. The schema-extension surgery is `grammar/src/validate/host_registry.rs` adding the sentinel. | REINVENT (1) |
| §3 (line 173) | `pratt = "off"`, `literal_trie = "off"`, `regex_prefilter = "json-regex-only"`, `simd = "json-structural-always"` | Skinny pins the optimizer choices that V1 routes via cost-model. | Bounded by `cost-model` skip (§1.1 row 5); pre-empts the cost-model dispatch. | ARCH §5 line 732 says "pratt, simd, and recognizers default to `auto`" per Lock 10. Hardcoded enum values not in ARCH's declared schema; metadata validator must accept skinny-mode strings. | Steelman: skinny escape valve via `[workspace.metadata.bbnf.grammars.json.optimization].profile = "skinny-json-curated"` only, with the explicit string overrides as a *consequence* of profile resolution rather than user-set fields. Counter: WORKSPACE.md §3 line 170 already names `profile = "balanced"` AND fields beneath it. Pick one. The skinny needs ONE override mechanism, not two. | REINVENT (2) |
| §4.1-§4.10 | Lock 13 (4-10 children per `src/`) | Each crate's directory layout. | Every layout (§4.1: 6 children, §4.2: 5, §4.3: 5, §4.4: 6, §4.5: 4, §4.6: 6, §4.7: 6, §4.8: 7, §4.9: 7, §4.10: 4) is within range. The §4.7 promotion is correctly named as Lock 13 surgery. | §4.6 `runtime/src/` has 6 children including `grammars/` whose only child is `json/`. Single-child intermediate dir under Lock 13's "cohesive concern" mandate is a sub-structural concern — the parent directory holds one subtree until V1 adds more grammars. | Steelman: `grammars/` is the regen mount-point; `json/` is the V1-shaped per-grammar generated subdir per ARCH §9 line 1373. Even at 1 child, the directory is the fixed mount where regen lands; V1 graduation adds 8 siblings. KEEP under "deliberate single-child mount-point" reasoning, but spec should name it. | KEEP-with-caveat |
| §4.4 (line 282) | `passes/src/layout/types/` (HM nested under layout) | layout/ is a parent dir holding only `types/` (a single child) | Mirrors V1 ARCH §8.2 path; Lock 2 names `passes::layout` as canonical. | Lock 13: `layout/` has ONE child (`types/`). Below 4-10. The §4.4 listing has 6 children at `passes/src/`, which is fine — but `passes/src/layout/` itself is non-compliant. | Steelman: `passes::layout::types` is the algorithm-W namespace; the layout dir holds future siblings (`@layout` lowering at V1). Skinny single-child is a Lock-13-13 edge case the spec must call out explicitly under "deliberate mount-point" — same shape as `runtime/src/grammars/`. WORKSPACE.md §4.4 should name this exception under §2.1's HM-only contradiction discussion. | REINVENT (3) |
| §4.8 (line 343) | `simd-scan/src/` includes `avx512/` and `wasm/` as dead code "for parity" | Carries V1 arches to keep cargo-cfg gates stable | Lock 13 honoured at child count (7); `simd-scan` is KEEP-OUTRIGHT (§8 row 8). | `avx512/` and `wasm/` are dead code in the skinny. Lock 13 says "Every directory partitions one cohesive concern" — dead arches dilute cohesion. Lens I (contrivance): keeping dead siblings to satisfy 4-10 child count is ceremony. | Steelman: the V1 simd-scan inherits the directory shape; rebuilding the directory at V1 graduation is wasted work. Counter: cargo-cfg gates can be applied at the file level; the dead arches could compile under their cfgs without occupying directory slots. The carry-as-ratified verdict survives only because §8 is KEEP-OUTRIGHT — skinny mines the existing 2,607 LOC verbatim. KEEP under verbatim-mining. | KEEP |
| §3 (line 137-139) | `generated_root` / `fixture_root` paths | Workspace metadata names directories where regen and fixtures land. | Honours Lock 14 metadata-only onboarding. | `fixture_root = "crates/test-fixtures/corpus"` — but BENCH.md §3.2 line 281 says "Files land at `tests/fixtures/json/twitter.json`". Two different fixture paths. WORKSPACE says `crates/test-fixtures/corpus`, BENCH says `tests/fixtures/json/`. Cross-quadrant disagreement. | Steelman: the two paths are two bench targets — `test-fixtures` carries the manifest + checksums, `tests/fixtures/json/` is the unpacked corpus loaded at runtime. But §3 line 139 + §1 row 10 + §10 row 2 of WORKSPACE all imply one location. BENCH §3.2 says another. Surgery: WORKSPACE §3 line 139 should clarify whether `fixture_root` is the manifest dir or the corpus dir; the loader knows the difference. | violated |
| §4.11 + §6 (lines 384-489) | `xtask` shape | Single-file ~250 LOC binary | Replaces `bbnf-cli` cleanly; Lock 6 honoured (xtask emits committed source artefacts, not proc-macro). | `xtask/src/main.rs` is "Single-file binary; not subject to Lock 13's directory rule" (§4.11 line 390). True for binaries. | KEEP. | KEEP |

**Lane 1 verdict: violated-with-recommendation (3 REINVENT, 1 violated, 11 KEEP).** Lock 13 is honoured at the immediate-child count level for every crate; two cases (`runtime/grammars/`, `passes/layout/`) have single-child mount-points that need explicit ratification, not surgery. Lock 14's metadata schema is strained by `"skinny-none"` and `"json-structural-always"` — one schema-extension surgery closes both. Lock 5's per-backend boundary is honoured (`wasm = false`). Lock 6 honoured. Lock 1 inherits from SUBSTRATE.

---

## §4 Lane 2 — Sequencing Discipline

N/A. The skinny is single-wave. Lane 2 reports as N/A per skinny HARDENING §4.

---

## §5 Lane 3 — Cohesion

Every WORKSPACE claim must be verifiable from artefacts WORKSPACE produces or cites; cross-quadrant references must be coherent.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| §1.1 row 11 (line 55) | `pipeline` shim location: "in `xtask::regen` + `bbnf::compile`" | Pipeline crate inlined into two locations | Names two receivers; mechanical migration. | §7 row 4 (line 503) says shim location is "`crates/bbnf/src/parse/pipeline.rs` + `xtask/src/main.rs::regen`" — different from §1.1 (`bbnf::compile` vs `bbnf/src/parse/pipeline.rs`). Inconsistency. | Steelman: `bbnf::compile` is the public function exported from `crates/bbnf/src/parse/pipeline.rs`. The two statements describe the same thing at different abstraction levels. Surgery: standardise on one phrasing across §1.1 + §7. | REINVENT (4) |
| §1.1 row 8 (line 51) | `parse-that` skipped (legacy combinator core) | Not needed for JSON | Reduces scope. | Lock 11 amendment (14-LOCKS:54): "`parse-that` is the canonical name for the published parser combinator + regex family." V1 publishes `parse-that` AND `parse-that-regex` as a sub-crate. Skinny drops `parse-that` entirely — does the V1 graduation carry the parser-combinator role or is it lost? §8 row 7 (line 528) says "`parse-that-regex` -> `parse-that` (with `parse-that-regex` as its regex sub-crate per Lock 11 amendment)" which implies the skinny `parse-that-regex` graduates into the V1 `parse-that` crate. So the parser-combinator portion is V1-additive — receiver named. | KEEP under graduation receiver. | KEEP |
| §3 (line 142) | `host_fns.default_registry = "host::primitives"` | References a `host::primitives` module that does not exist in skinny | The literal value matches V1 ARCH §5 line 686. | The skinny has no `host` crate (§1.1 row 4 says: "Inlined as a 50-LOC private module in `bbnf::host_stubs`"). The string `"host::primitives"` references a non-existent path. Either the validator rejects it (and skinny breaks) or skinny carries the string symbolically (in which case §1.1 row 4 should say so). | Steelman: the metadata schema is symbol-only; the validator does not look up paths. KEEP if so; spec should name the symbol-only behaviour. Surgery: §3 line 142 + §1.1 row 4 should agree on whether `host::primitives` is symbolic or live. | REINVENT (5) |
| §10 (table, line 575) | "Per-grammar declaration crates (Lock 14 escape valve)" omitted | Skinny has no `@host fn` calls. | Honours Lock 14's "rare exception" framing. | The omission table conflates "per-grammar declaration crate" (a Lock-14 escape valve, an actual workspace-resident crate) with "host fns" (a directive). They are distinct mechanisms. Row 1 says scope reason is "Main JSON grammar has no `@host fn`" — but declaration crates and `@host fn` are not co-extensive (a declaration crate carries trait-impl bridges, not just host fns). | Steelman: the skinny has neither declaration crates NOR `@host fn` (§10 row 9 separately covers `@host fn`). Two rows is correct. Surgery: §10 row 1's scope-reason text should distinguish the mechanism (declaration crate) from the reason (no `@host fn`). | REINVENT (6) |
| §1 row 2 (line 29) | `grammar` partial: "host-fn-free by deliberate skinny deviation" | Crate description names the host-fn-free deviation | Names the deviation in-line; honest framing. | The deviation is enforced at the `json.bbnf` source — the `grammar` crate's `validate/` module per §4.2 must reject `@host fn` for the JSON grammar specifically? No — `validate/` rejects "non-skinny directives" (§4.2 line 261). But §1 row 2 says `@host fn` is "deliberately skinny deviation," which is grammar-source-side, not validator-side. The validator then needs to permit `@host fn` syntactically (because V1 will add it back in tranche D) but reject it for the JSON skinny grammar. This is two dispositions: parse-OK, validate-reject-for-JSON. | Steelman: §1 row 2 says the validator rejects `@host fn` for the skinny only via the `BBNF-DIRECTIVE-NOT-IN-SKINNY` trapdoor (§1 row 2: "the other five directives parse to a no-op trapdoor that errors with `BBNF-DIRECTIVE-NOT-IN-SKINNY`"). But `@host fn` is not in the "other five directives"; the six per Lock 10 amendment are `@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token`. The skinny enables `@import` (§1 row 2) and trapdoors the rest — so `@host fn` is in the trapdoor set. The disposition is internally consistent. KEEP. | KEEP |
| §3 (line 137-186) | Cargo.toml metadata block | Names the canonical schema | Pinned per ARCH §5. | The metadata block has 9 sub-tables; ARCH §5 has 8 (`recognizers`, `host_fns`, `grammars.json`, `grammars.json.runtime`, `grammars.json.host`, `grammars.json.optimization`, `grammars.json.codegen`, `grammars.json.fixtures`). WORKSPACE.md adds `[workspace.metadata.bbnf]` ROOT (which ARCH §5 also has — `generated_root`, etc.). Same set. KEEP. | Steelman: ARCH §5 says `host_registry = "default"` not `"skinny-none"`. Skinny diverges deliberately (already flagged Lane 1). KEEP at structure; REINVENT at value (already counted). | KEEP |
| §8 (line 522) | `grammar` row: "Mine concept from current `crates/core/src/imports/` and the bootstrap in `crates/bootstrap/`" | Names two mining sources | Two sources cited. | `crates/bootstrap/` per MIGRATION §3.1 is 4 files / 465 LOC (`MIGRATION.md:95`). Mining 465 LOC for the skinny grammar (3,500-LOC budget per WORKSPACE §2) is reasonable. `crates/core/src/imports/` per CENSUS — verify this exists in current code. | Steelman: the skinny has an authority-anchored mining plan. KEEP. | KEEP |
| §8 (line 525) | `passes` row: "Skinny `passes::source_stub/` migrates to `source` crate" | Migration target named | Lock 14 honoured (source is internal V1 crate). | `passes::source_stub/` is at §1.1 row 11 located at `crates/passes/src/source_stub/` — which is then migrated at V1. INDEX §"Open contradictions" row 4 lists this as a deliberate skinny deviation. Coherent. KEEP. | KEEP. | KEEP |
| §11 (line 611) | "Bench competitor crates (`sonic-rs`, `simd-json`) are dev-dependencies of `bbnf-bench`. They are not workspace.dependencies. Owned by BENCH.md." | WORKSPACE delegates to BENCH | Honours single-quadrant ownership. | §3 line 130 lists `serde_json` as a workspace.dep — but BENCH §7.6 also lists `serde_json` as a `[dev-dependencies]` entry of `bbnf-bench` Cargo.toml. Double-listing risk: the workspace alias propagates `serde_json` as available to all workspace crates; BENCH's local pin (`=1.0.117`) may differ from any other crate that depends on `serde_json` via the workspace alias. | Steelman: only `bbnf-bench` uses `serde_json` (verifiable from the per-crate role list); workspace alias is harmless. Counter: if `bbnf` ever needs `serde_json` (e.g., for diagnostic JSON output), the workspace alias resolves to whatever BENCH.toml later pins or the workspace.toml later pins; one of the two pins wins. Surgery: `serde_json` belongs in `bbnf-bench`'s `[dev-dependencies]` only, not `[workspace.dependencies]`. | REINVENT (7) |

**Lane 3 verdict: violated-with-recommendation (4 REINVENT, 5 KEEP).** Cohesion is largely solid but four micro-inconsistencies leak across §1.1 / §3 / §7 / §10 / §11 — same content described twice slightly differently. All close in 1-2-line surgery. Cross-quadrant: §3 `fixture_root` vs BENCH `tests/fixtures/json/` is the most-impactful inconsistency (Lane 1 also flagged).

---

## §6 Lane 4 — SOTA Anchoring

WORKSPACE.md owns no parse-throughput gates directly — those live in BENCH.md §6. The only WORKSPACE perf gate is the **build-time** target.

| Site | Item | Pros | Cons | Verdict |
|---|---|---|---|---|
| §5 (line 444) + §9 (line 553) | "≤ 90s clean release on M1 Pro" | Engineering-side iteration ceiling. | Lens F: pseudo-precise numeric without measurement provenance — neither cited from a corpus nor named as a calibration. | Engineering gate, not Lock 8. KEEP. |
| §3.1 (line 226) | "`debug = true`, `strip = false`" | Honours samply-symbol-resolution rule (cited inline). | None. | KEEP. |
| §1.1 row 7 (line 49) | `cost-model` skipped: "BENCH.md bounds this cut with alternate-plan probes" | Routes the cost-model deferral to a measurement gate. | Lock 8: parse-throughput gates name competitor + dataset + platform; alternate-plan probes (BENCH §7.8.2) cite per-corpus thresholds. | KEEP under BENCH delegation. |
| §10 (entire table) | Each omitted-mechanism row carries SOTA-impact statement. | Honours skinny HARDENING Lens L premise-fidelity discipline. | None — the table is the honest impact accounting. | KEEP. |

**Lane 4 verdict: honoured.** WORKSPACE delegates Lock 8 correctly to BENCH; the only WORKSPACE-resident perf gate (90s build-time) is engineering, not parse-throughput, and Lock 8 honour is not claimed for it.

---

## §7 Lane 5 — Grammar-Authoritative Discipline

| Site | Item | Verdict |
|---|---|---|
| §3 (line 153) | `[workspace.metadata.bbnf.grammars.json]` block | KEEP — single grammar entry; metadata-only onboarding per Lock 14. |
| §4.6 (line 320) | `runtime/src/grammars/json/` (per-grammar generated subdir) | KEEP — Lock 14 permits per-grammar generated runtime modules from a "single grammar-agnostic generator template" (14-LOCKS:60). Verified the spec's regen contract (§5 command 4 + §6) generates `json/` from `grammars/json.bbnf` only. |
| §3 (line 142) | `host_fns.default_registry = "host::primitives"` | REINVENT — already flagged Lane 3; the path references a non-existent module in skinny. Either the validator is symbol-only or the skinny's host-stub module needs to publish a `host::primitives`-named symbol. |
| §4 (each crate's `src/` listing) | Verify no grammar-named modules in generic crates | KEEP — `grammar/`, `ir/`, `passes/`, `codegen/`, `runtime/` (excluding `runtime/src/grammars/`), `parse-that-regex/`, `simd-scan/`, `bbnf-bench/` (excluding `bbnf-bench/src/track2/json.rs`), `test-fixtures/` are scanned. No `json.rs` modules in the generic-crate src trees outside of the two ratified locations (generated runtime + Track 2 hand-coded probe). |
| Grep verifications | `rg -nP 'match\s+\w+\s*\{[^}]*Json\s*=>'` against WORKSPACE.md | KEEP — the document does not contain match-arm hard-codes. Mentions of "json" in WORKSPACE.md are either fixture paths, generated-dir paths, grammar-source paths, or metadata table keys — all per-X table cells, none are paragraph-hardcodes-grammar-in-plan-logic. |
| §1.1 row 4 + §3 line 142 + §7 row 3 | `host_stubs` shim is grammar-agnostic | KEEP — the shim is "Empty `HostRegistry` + `HostFnId(u32)` placeholder" (§7 row 3); does not name `Json`. Honours Lock 14. |

**Lane 5 verdict: violated-with-recommendation (1 REINVENT, 5 KEEP).** Grammar-authoritative discipline is honoured at every grep test; the one inconsistency is the `"host::primitives"` symbolic reference that Lane 3 already flagged. Per-X table cells (§3 metadata, §1 crate set, §4 directory layouts) all use grammar names ratified per Lock 14's metadata-onboarding pattern.

---

## §8 Lane 6 — Generated-Code + LOC Budget (LOAD-BEARING)

The 31,400 handwritten LOC ceiling is binding per skinny HARDENING §4 Lane 6 sharpening.

### 8.1 Per-crate LOC budget audit

The §2 table sums to **31,400 LOC** as claimed:

| Crate | Skinny LOC | Verified | Notes |
|---|---:|---|---|
| `bbnf` | 600 | ✓ | 100 + 50 + 450 ≈ 600. |
| `grammar` | 3,500 | ✓ | Bootstrap parser only; no full BBNF. |
| `ir` | 2,500 | ✓ | Subset variants. |
| `passes` | 6,000 | flagged | §2.1 contradiction binding signal. |
| `codegen` | 4,500 | ✓ | 1,500 + 2,000 + 700 + 300 = 4,500. |
| `runtime` | 4,000 | ✓ | 1,500 + 800 + 600 + 400 + 300 + 400 = 4,000. |
| `parse-that-regex` | 4,000 | ✓ | 1,000 + 1,000 + 1,000 + 700 + 300 = 4,000. |
| `simd-scan` | 3,500 | ✓ | 2,607 + 893 ≈ 3,500. |
| `bbnf-bench` | 2,000 | **STALE** | See §8.2 below. |
| `test-fixtures` | 800 | ✓ | 200 + 300 + 300 = 800. |
| **Total** | **31,400** | conditional on `bbnf-bench` |  |

### 8.2 The `bbnf-bench` budget reckoning (DOMINANT FAULT)

WORKSPACE.md §2 (line 73): `bbnf-bench` budget = 2,000 LOC, decomposed as:
- Criterion harness: ~600
- Reproducibility schema serializer: ~300
- Parity matrix runner: ~300
- Masking probes: ~200
- Track 2 handwritten parser: ≤500

**Sum: 600 + 300 + 300 + 200 + 500 = 1,900 LOC** (with 100 LOC headroom).

BENCH.md §11.1 (line 1567-1582) post-redress decomposition:
- `fixtures.rs`: ≤ 120
- `metadata.rs`: ≤ 250 (schema_version + per-corpus parity + RSS + cold_cache_mode add fields)
- `parity.rs`: ≤ 100
- `gate.rs`: ≤ 350 (matrix expansion: F-split, G-collapse, M-add, BEAT_BOUND classifier)
- `bin/gate.rs`: ≤ 60
- `track2/json/`: 800-1,500 (measurement-driven; reference-class)
- `track2/css_prior.rs`: ≤ 600 (optional CSS prior probe)
- `benches/json_parity.rs`: ≤ 250 (probe additions)
- `benches/simd_scan.rs`: ≤ 150 (per-corpus parity)

**BENCH.md target: ≤ ~2,200 LOC** (per BENCH:1582), with Track 2 at 800-1,500 LOC (not ≤500), and the optional CSS prior probe adding up to 600 LOC.

**The arithmetic does not balance.** WORKSPACE §2 budgets `bbnf-bench` at **2,000 LOC with Track 2 at ≤500 LOC**. BENCH §11.1 budgets `bbnf-bench` at **~2,200 LOC with Track 2 at 800-1,500 LOC**. The two specs disagree by 200-1,800 LOC depending on Track 2's measured size and CSS-prior inclusion.

**Steelman:** WORKSPACE §2 is intentionally lower than BENCH §11.1 because BENCH owns the internal split and its number is target-with-headroom, while WORKSPACE owns the binding budget and forces BENCH to keep its real-world LOC at or under the binding 2,000. **Counter:** Track 2 is no longer cap-budgeted (BENCH §1.2 line 71-78 explicit: "LOC is measurement-driven, not constraint-driven... reference-class 800-1,500 LOC"). If Track 2 lands at 1,200 LOC measured, the WORKSPACE 2,000 binding requires the rest of `bbnf-bench` (criterion harness + metadata + parity + gates + benches) to fit in 800 LOC — half of BENCH §11.1's 1,030 LOC sum-without-Track 2.

**Surgery candidates** (Lens N: graduation-mechanical):

1. **Raise WORKSPACE §2 `bbnf-bench` to 2,500 LOC** (or 3,000 with CSS-prior probe). Total moves to 31,900 (or 32,400). The "31,400" claim in INDEX.md, README, and the `passes`-budget contradiction signal lose their round-number anchor.
2. **Split Track 2 from `bbnf-bench` budget**: Track 2 owned at 1,500 LOC, `bbnf-bench` other = 1,000 LOC. Total = 31,400 LOC unchanged but `bbnf-bench` is now two budget rows.
3. **Drop CSS-prior probe from skinny scope** (BENCH §9.1 says "Defer if implementation budget excludes this probe"): keeps `bbnf-bench` at ≤ 1,600 LOC + Track 2 at 800-1,500 LOC = 2,400-3,100 LOC. Same problem persists at 2,400 floor.

The dominant surgery is (1) or (2). The author should pick one and commit it before SK-V2 dispatch.

| Site | Item | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|
| §1.1 row 9 (line 36) | "the ≤500 LOC Track 2 handwritten substrate probe" | Names a Track-2 cap | BENCH.md §1.2 line 71 dropped this cap. Stale reference. | Surgery: §1.1 row 9 line 36 strike "≤500 LOC". | REINVENT (8a) |
| §2 (table, line 73) | `bbnf-bench` 2,000 LOC including "Track 2 handwritten parser (≤500)" | Decomposition includes a stale Track 2 cap | Same. | Surgery: redo arithmetic per (1) or (2) above. | REINVENT (8b) |
| §6 / xtask::loc (line 487) | "`xtask lint-loc` enforces the ≤4,000 generated JSON LOC budget and the ≤500 LOC Track 2 handwritten probe budget before bench results can authorize dispatch" | xtask is the gate enforcing the cap | Stale: ≤500 LOC cap dropped by BENCH redress; xtask should enforce the §10.6 substrate-API correspondence checklist instead. | Surgery: line 487 strike "≤500 LOC Track 2 handwritten probe budget"; replace with "the substrate-API correspondence checklist per BENCH.md §10.6". | REINVENT (8c) |
| §10 (table row 14, line 590) | "`xtask lint-loc` gates ≤4,000 JSON generated LOC and ≤500 Track 2 LOC" | Same | Same | Surgery: same. | REINVENT (8d) |
| §11 (line 596-604) | "31,400 handwritten LOC plus ≤4,000 generated LOC if and only if..." | Closure conditions | If `bbnf-bench` budget grows to 2,500 LOC, the 31,400 claim flexes. | Surgery: re-cite the post-arithmetic total. | REINVENT (8e) |
| §8 (table, line 538-542) | Mechanical-closure cost rows | Each has a LOC range | The HM-only `passes` constraint row (1,500-3,000 LOC additive) is loose. The Box<[T]> sealing deviation is missing entirely from §8.1 (INDEX has it; WORKSPACE §8.1 does not). | Surgery: add Box<[T]>-sealing row to §8.1 with closure cost. | REINVENT (9) |

### 8.3 Generated LOC budget

| Site | Item | Verdict |
|---|---|---|
| §2 (line 76) + §3 (line 180) | "≤ 4,000 generated LOC" anchored to PASS-2:432 (3,500 LOC + 2%) | Provenance verifiable; orchestrator should confirm `restart/audit/pass-2-codegen/PASS-2.md:432` carries 3,500 LOC observation. KEEP. |
| §10 (last row) + xtask::loc | xtask lint-loc enforcement | KEEP — gate exists; ≤4,000 budget is per-grammar; nine-grammar scale routes to F.W3. |

**Lane 6 verdict: violated-with-recommendation (5 REINVENT, 5 KEEP).** Five surgical edits cluster around the **stale Track-2 ≤500 LOC cap** that the BENCH redress dropped but WORKSPACE still cites four times (§1.1, §2, §6, §10), and the `bbnf-bench` budget arithmetic that no longer balances. Plus one missing Box<[T]> deviation row in §8.1. All mechanical to close.

---

## §9 Lane 7 — Friction Forecast

WORKSPACE.md is silent on user-facing failure surfaces it owns.

| Friction surface | User | Mental model | Confusion point | Required artefact |
|---|---|---|---|---|
| `passes` budget overrun (§2.1) | Skinny implementor | "I'll just add a few more LOC for HM bookkeeping" | The §2.1 "binding constraint" framing is well-named in spec, but the implementor running `xtask lint-loc` will see only "exceeds 6,000 LOC". Diagnostic must say: "passes-LOC-overrun signals scope wrong; reopen WORKSPACE §2.1 contradiction; do not absorb." | Verbatim error from `xtask lint-loc`: `BBNF-SCOPE-WRONG: passes/ exceeds 6,000 LOC. WORKSPACE.md §2.1 names this as scope-wrong evidence; the V1 plan needs partial implementation to validate the SOTA-beat claim. Do not absorb the overrun — reopen the contradiction.` |
| `xtask lint-loc` exit codes | CI | Pass/fail | What's the exit code for "stale Track-2 cap fails"? Spec doesn't say. | xtask exit-code table: 0 = pass, 1 = generated LOC overrun, 2 = passes-budget overrun (scope-wrong signal), 3 = substrate-API correspondence checklist fail. |
| 90s build-time miss | Skinny implementor | "build is slow but completes" | §5 (line 446-453) lists four causes + surgery, but no diagnostic. | A wrapper script `scripts/skinny-build-doctor.sh` that runs `cargo build --workspace --release --timings` and surfaces hot-spots. |
| Workspace alias (`bbnf-grammar = { path = ..., package = "grammar" }`) | New contributor | "I depend on `bbnf-grammar`; the package name in Cargo.toml is `grammar`?" | Cargo's `package = "grammar"` aliasing is a known footgun (the import path uses `bbnf_grammar`, the dependency line uses `bbnf-grammar`, the crate name is `grammar`). | Cookbook entry in `docs/skinny-onboarding.md`: "When you `use bbnf_grammar::ast::*`, the resolution path is dep-line `bbnf-grammar` -> workspace alias to `crates/grammar/Cargo.toml` `name = grammar` (auto-renamed back to `bbnf_grammar` per Cargo identifier rules). The reason: V1 publishes as `bbnf-grammar`; skinny mirrors the V1 published-name convention so graduation does not rename consumers. (§3 line 116-126 commentary.)" |

**Lane 7 verdict: silent-must-add.** WORKSPACE owns four friction surfaces; spec adds none. **Surgery (10): WORKSPACE §6 should add an exit-code table for `xtask lint-loc`; §2.1 should name the verbatim diagnostic; §3 should add the package-alias cookbook reference.**

---

## §10 Lane 8 — Carry & Deferral Audit

| Site | Carry | Receiver | Blocker | Receiving gate | Verdict |
|---|---|---|---|---|---|
| §1.1 row 2 | `bbnf-language-server` skipped | Tranche I | LSP/DAP/incremental parse | I.W? gate (named only by tranche letter) | violated — receiving gate not named at wave granularity. INDEX.md (line 35) says "Tranche I" only; receiving wave should be I.W2 (LSP) or I.W3 (DAP) per V1 plan. WORKSPACE §1.1 references "tranche I" without wave. |
| §1.1 row 3 | `vm` skipped | Tranche E | BIR debug-replay validation | E.W? gate | violated — same. |
| §1.1 rows 6-8 | `cost-model`, `egraph`, `csp-solver` skipped | (various) | "alternate-plan probes" + ARCH §10.1 | H.W2/H.W3 (per INDEX line 36) | KEEP — INDEX names H.W2/H.W3 for cost-driven rewrites and recognizer tuning. WORKSPACE §1.1 row 5 cites "BENCH.md alternate-plan probes" — receiver-by-mechanism, not by wave. The wave receiver lives in INDEX. KEEP via INDEX delegation. |
| §1.1 row 11 | `pipeline` inlined | V1 `pipeline` crate | "scheduler topology, stage DAGs, caching" | (no wave named) | KEEP — V1 architecture has `pipeline` crate per ARCH §1; the receiver is the V1 crate itself, not a tranche wave. The skinny inlines for budget; V1 graduation extracts. Receiver identified at architecture level. |
| §10 row 14 | "Generated LOC budget enforcement at scale" | F.W3 | "One generated tree (`json/`) is exercised; nine-grammar scale routes to F.W3" | F.W3 gate | KEEP — F.W3 is wave-granular receiver; the blocker (nine-grammar regen-equality) is implicit but recoverable. |
| §10 row 3 | "GADT / DK13 / OutsideIn / CSP type-system" omitted | (no receiver named in WORKSPACE) | "JSON's grammar is monomorphic" | (no gate named) | violated — INDEX line 35 names "Tranche D" but WORKSPACE §10 row 3 does not. The omission impact statement says "Risk: V1 grammars (CSS L4, Sheets) carry generics + GADTs; the JSON SOTA-beat number does not validate that the type system layer adds zero perf cost. The BENCH agent must mark the JSON number as a *necessary but insufficient* SOTA-viability signal." This is a Lens L MASKING surfacing, but no V1-tranche receiver is named in WORKSPACE. |
| §10 row 11 | "Multiple grammars" omitted | Tranche H ("the V1 H tranche owns the per-grammar SOTA-beat closure") | "Skinny is JSON-only" | H.W? gate | KEEP at tranche granularity, violated at wave granularity. |
| §8 (table, line 521-531) | Mining sources for V1 | (named per row) | (varies) | (V1 destination crate) | KEEP — every row names mining source + V1 destination + mining vs fresh; Lane 8 receiver discipline honoured. |
| §11 (line 606-611) | Open contradictions flagged | Synthesis pass | (per contradiction) | (per contradiction) | KEEP — four contradictions named; each is either (a) non-issue per cross-quadrant invariant, (b) flagged-as-binding-signal, or (c) wave-routed. |

**Lane 8 verdict: violated-with-recommendation (2 violated, 6 KEEP).** Two carry rows (§1.1 row 2 LSP, §1.1 row 3 VM, §10 row 3 type-system) name tranche but not wave. Surgery: per INDEX.md cross-references, fill in wave receivers. **Surgery (11): WORKSPACE §1.1 rows 2/3 should append "(tranche I.W2 — LSP/DAP)" and "(tranche E.W?)" respectively; §10 row 3 should append "Tranche D".**

---

## §11 Lane 9 — Greenfield Discipline

| Site | Item | Greenfield-honour | Verdict |
|---|---|---|---|
| §1.1 row 1 | `bbnf-cli` replaced by `xtask` | "No quick solutions": xtask is a deliberate cut, not a workaround. | KEEP. |
| §1.1 row 9 | `path` family skipped | "No legacy code uncontested": path DSL is V1 G-tranche territory; not skinny-relevant. | KEEP. |
| §3.1 (line 226) | `debug = true`, `strip = false`, `thin LTO` | "Idiomatic, gestalt approaches": samply-symbol-resolution rule cited inline. | KEEP. |
| §4.7 (line 327) | `parse-that-regex` directory promotion (Lock 13 surgery) | "Architectural transposition for elegance": converts `regex/{hir,nfa,dfa,vm}` to top-level siblings; mechanical-mechanical-mechanical. | KEEP. |
| §4.8 (line 343) | `simd-scan` `avx512/`, `wasm/` carried as dead code | "No contrivance, no overengineering": Lens I flagged this. | KEEP at carry-as-mining; REINVENT at "for parity" framing. |
| §4.4 + §2.1 (line 79-87) | `passes` 6,000 LOC binding signal | "Root-cause fixes proposed": binding-as-signal is greenfield-honest. | KEEP. |
| §6 (line 459) | xtask is single-file ~250 LOC | "Idiomatic, gestalt approaches": small `xtask` rather than full `bbnf-cli`; honoured. | KEEP. |
| §7 (line 514) | "shim discipline rule: when a shim grows past 500 LOC, it has earned its own crate" | "No legacy code uncontested": growth-trips-graduation discipline. | KEEP. |
| §8 (line 533) | "the V1 architecture's IR boundary (Lock 5) and tape substrate (Lock 1) are mechanism-incompatible with the current `crates/core/` walker pattern; mining those would import the failure modes" | "No legacy code uncontested": explicit refusal-to-mine on architecture-incompatible legacy. | KEEP. |
| §10 (entire) | Per-omission impact statement | "No carry-blindness": every omission named with V1 receiver + impact. | KEEP at row level; some rows missing wave-granular receiver (Lane 8 fault). |

**Lane 9 verdict: honoured (1 REINVENT, 9 KEEP).** Greenfield discipline is the strongest lane in WORKSPACE. The one REINVENT (`simd-scan` dead-arch carry) is Lens I cosmetic, not greenfield-violating.

---

## §12 Lenses F-K (cross-cutting)

### Lens F — LLM bias

Two pseudo-precise numerics flagged Lane 6 (90s build-time without provenance; 31,400 LOC sum that no longer balances post-redress). One unfalsifiable claim: §2.1 line 87 "the SOTA-viability test cannot run cheaply" — this is a Lens-F unfalsifiable framing if not bounded by a measurement. **Counter:** §2.1 frames the claim AS a flag-as-binding-signal, not as a verdict; the test of "cannot run cheaply" is measurable post-implementation by counting `passes` LOC at lint-loc time. KEEP.

### Lens G — Overfitting

The skinny is JSON-overfit by design (the entire premise). Lens G is N/A here because skinny over-fitting IS the test design. Verdict: N/A.

### Lens H — Hallucination + provenance

Two citations that the orchestrator must verify:
1. §2 line 61 + §3 line 180: `restart/audit/pass-2-codegen/PASS-2.md:432` — claim: 3,500 LOC json baseline + 2% = 4,000.
2. §10 last row (line 590): `PASS-2.md:435` — claim: 172,125 LOC nine-grammar generated ceiling.

Both cite specific lines in PASS-2.md. Verifiable post-audit by reading those lines. Spec follows the cite-path:line discipline. KEEP-with-rec.

### Lens I — Contrivance

| Site | Item | Surgery | Verdict |
|---|---|---|---|
| §4.8 | `avx512/`, `wasm/` dead arches in `simd-scan/` | Cargo-cfg gate at file level; do not carry empty dirs purely for child-count. | REINVENT — but KEEP under verbatim-carry of 2,607 LOC. |
| §4.4 | `bridge/` "intentionally vestigial" stubs in `passes/` | The directory exists "so the V1 `passes::bridge` import path is reserved" (§4.4 line 294). Reserving a path with stubs is path-reservation contrivance. | KEEP — the alternative (no `bridge/`) means V1 graduation creates a new sibling, which contradicts §0 Boundary's mechanical migration discipline. Path reservation is mechanical-precondition, not contrivance. |
| §3 (the four explicit recogniser overrides) | `pratt = "off"`, `simd = "json-structural-always"`, `literal_trie = "off"`, `regex_prefilter = "json-regex-only"` | These four enum-string overrides are skinny-mode escape valves. ARCH §5 line 732 says "pratt, simd, and recognizers default to `auto`". Five overrides is more than the cost-model skip needs — could be folded into the `profile = "skinny-json-curated"` row + a profile resolver that produces the four downstream values. | REINVENT (already counted Lane 1 as REINVENT 2). |

### Lens J — Host-language leverage

WORKSPACE.md leverages Cargo idiomatically (`workspace.dependencies`, `workspace.metadata.bbnf`, `[profile.bench] inherits = "release"`); leverages `samply` natively. One redundancy: `sha2 = "0.10"` in workspace.deps + `blake3 = "1"` in BENCH dev-deps — two hash families for fixture verification + parity hashing. **Surgery (12):** standardise on one. SHA-256 is the manifest fingerprint per BENCH §3.2 (committed manifest format); `blake3` is parity-hash per BENCH §4.2 (not user-visible; performance-driven). Two distinct uses; KEEP — redundancy is mechanism-justified.

### Lens K — Meta-grammar discipline

WORKSPACE.md owns no meta-grammar surface beyond delegating to `grammar` crate's metadata schema. Lens K honoured by delegation.

---

## §13 Lens L, M, N — Skinny-specific lenses

### Lens L — Premise fidelity

Each WORKSPACE-listed omission must be classified FAITHFUL / FAITHFUL-with-V1-grammar-caveat / MASKING.

| §10 omission | Classification | Bench-recoverable signal | Verdict |
|---|---|---|---|
| Per-grammar declaration crates | FAITHFUL — declaration crates are V1's rare-exception (Lock 14:60); skinny has no host fns. | BENCH §7.8.1 host-call probes A & B bound CallHost dispatch + eager-decode delta. | FAITHFUL. |
| LSP / DAP / incremental | FAITHFUL — editor surfaces don't influence parse throughput. | None needed. | FAITHFUL. |
| GADT / DK13 / OutsideIn / CSP | **FAITHFUL with V1-grammar caveat** — orthogonal for monomorphic JSON; load-bearing for CSS L4 generic colour-function chains and Sheets host-chain refinements. **WORKSPACE §10 names the caveat ("V1 grammars (CSS L4, Sheets) carry generics + GADTs")** and routes to "BENCH agent must mark the JSON number as *necessary but insufficient* SOTA-viability signal." | None — by design, skinny cannot recover this signal. The spec correctly marks it insufficient. | FAITHFUL-with-V1-grammar-caveat. |
| Cost-model + e-graph + CSP optimization graph | **MASKING-bounded** — the canonical-plan pre-selection could mask a cost-model win the V1 would discover. WORKSPACE §10 row 4 explicitly cites "BENCH.md's alternate-plan probes bound whether the canonical plan is hiding a missing cost-model win." | BENCH §7.8.2 alternate-plan probes (alternate_scalar_plan, alternate_dispatch_table_plan, alternate_pext_mask_plan). | FAITHFUL-with-bound. The bound is delegated to BENCH; honest accounting. |
| Pratt auto-detection | FAITHFUL — JSON has no operator chain. | None needed for JSON. | FAITHFUL. |
| SIMD auto-detection | FAITHFUL — V1 auto-detector would also choose SIMD for JSON. | BENCH structural-scan microbench. | FAITHFUL. |
| WASM / TS backends | FAITHFUL — V2 territory per Lock 5 amendment. | None needed. | FAITHFUL. |
| Path / select macros | FAITHFUL — visitor/access throughput is a different gate. | None needed at skinny. | FAITHFUL. |
| Host fns + chains | **FAITHFUL conditional on probe pass** — WORKSPACE §10 row 9 says "JSON-FAITHFUL only after the one-host-fn probe passes; CSS / Sheets carry host calls and the V1 must measure their cost separately." | BENCH §7.8.1 probes A + B. | FAITHFUL-conditional. |
| Recovery / `@error` directives | FAITHFUL — recovery is its own gate (tranche I). | None needed at skinny. | FAITHFUL. |
| Multiple grammars | **FAITHFUL with V1-grammar caveat** — JSON SIMD-beat doesn't imply CSS L4 SIMD-beat; the V1 H tranche owns per-grammar closure. | None at skinny; H tranche territory. | FAITHFUL-with-V1-grammar-caveat. |
| `egraph-derive` / proc-macro | FAITHFUL. | None. | FAITHFUL. |
| Workspace metadata cross-grammar coherence | FAITHFUL. | None. | FAITHFUL. |
| Generated LOC budget at scale | **FAITHFUL with caveat** — skinny enforces JSON-only ≤4,000 LOC; nine-grammar scale (172,125 LOC ceiling) routes to F.W3. The ceiling is not exercised by skinny. | None at skinny. | FAITHFUL-with-caveat. |
| **Box<[T]> sealing inversion (deviation ledger row 6)** | **MASKING-MECHANICAL** — the skinny's seal-at-parse vs V1's seal-at-snapshot inversion. SUBSTRATE §1.2 calls this MECHANICAL with named inversion under Lens N. WORKSPACE.md does not list this as an omission in §10 because it's a SUBSTRATE-side deviation. | None — the inversion is mechanical-only, no throughput cost. | FAITHFUL via SUBSTRATE delegation. KEEP — but WORKSPACE §8.1's mechanical-closure table should have a Box<[T]>-sealing row; INDEX has it (§"Open contradictions" row 6), WORKSPACE §8.1 does not. **Surgery already counted Lane 6.** |
| **HM hierarchy inversion (deviation ledger row 7)** | **FAITHFUL conditional on graduation reversibility** — Lens N classifies as MECHANICAL with named inversion (INDEX §"Open contradictions" row 7). WORKSPACE §8.1 row 1 names the closure cost at "150-300 LOC wrapper". | The inversion's cost is graduation-cost not skinny-cost; not bench-recoverable at skinny. | FAITHFUL via INDEX delegation. KEEP. |

**Lens L verdict: violated-with-recommendation (2 REINVENT, 6 KEEP).** WORKSPACE.md classifies its 14 omissions correctly; two SUBSTRATE-side deviations (Box<[T]>; HM-inversion) appear in INDEX but are partial in WORKSPACE §8.1. Surgery: WORKSPACE §8.1 should mirror the seven-row INDEX deviation ledger.

### Lens M — Falsifiability

Threshold matrix lives in BENCH.md §6 — N/A in WORKSPACE.

### Lens N — Graduation mechanicality

WORKSPACE §8.1 has FIVE rows; INDEX §"Open contradictions" has SEVEN deviation rows.

| INDEX row | In WORKSPACE §8.1? | V1 closure type | Estimated LOC | Lens N verdict |
|---|---|---|---|---|
| HM hierarchy inversion (COMPILER §9.1) | ✓ row 1 | wrapper move; algorithm-W not rewritten | 150-300 LOC | MECHANICAL with named inversion |
| JSON host-fn-free (COMPILER §1.3) | ✓ row 2 | additive `@host fn` decode-string + registry dispatch | 150-250 LOC | MECHANICAL |
| `parse-that-regex` directory promotion (WORKSPACE §4.7) | ✓ row 3 | V1 inherits shape + adds siblings | 0-100 LOC | MECHANICAL (trivial) |
| `passes` HM-only constraint (WORKSPACE §2.1) | ✓ row 4 | DK13/GADT/CSP additive siblings around `algorithm_w` | 1,500-3,000 LOC | MECHANICAL |
| `wasm = false` metadata (WORKSPACE §3) | ✓ row 5 | V2 schema flag flip | 50-100 LOC | MECHANICAL (V2 trivial) |
| **Tape Box<[T]> sealing inversion (SUBSTRATE §1.2)** | **✗ MISSING** | TapeBuilder<'input> upstream + snapshot view re-seals as Box<[T]>; read-side type-shape unchanged | not estimated in WORKSPACE | MECHANICAL with named inversion (per SUBSTRATE §1.2) — but WORKSPACE §8.1 should carry the row |
| **HM hierarchy inversion as workspace-side mechanical row** (INDEX last row) | partially — WORKSPACE §8.1 row 1 names the LOC range but the WORKSPACE column "Skinny shape" reads `layout/types::algorithm_w` is the skinny top-level type pass (correct) | wrapper move | 150-300 LOC | MECHANICAL — same as row 1 above; the INDEX last row is the sharper restatement of row 1's closure |

**Surgery (already counted as REINVENT 9):** WORKSPACE §8.1 should add a Box<[T]>-sealing row with closure cost in LOC.

**Lens N verdict: violated-with-recommendation (3 REINVENT, 4 KEEP).** Five rows in WORKSPACE §8.1; INDEX has seven; one (Box<[T]>) clearly missing. The graduation-mechanical contract is honoured at the row level (every named deviation closes by additive code or wrapper move) but the row count drifts between INDEX and WORKSPACE.

---

## §14 Punch list

Ordered surgical edits to apply BEFORE WORKSPACE.md advances to SK-V2.

| # | Target file:line | Edit | Source verdict | Lane(s) |
|---|---|---|---|---|
| 1 | `restart/skinny/WORKSPACE.md:36` (§1.1 row 9) | Strike "≤500 LOC". Replace text with: "the Track 2 handwritten substrate probe (LOC measurement-driven per BENCH.md §1.2; reference-class 800-1,500 LOC; gated by the substrate-API correspondence checklist per BENCH.md §10.6, not by a LOC cap)". | REINVENT | Lane 6 |
| 2 | `restart/skinny/WORKSPACE.md:73` (§2 table row `bbnf-bench`) | Re-decompose the 2,000 LOC budget. Either: (a) raise to 2,500 LOC + raise total to 31,900 LOC, propagating to lines 75 ("Skinny total (handwritten) 31,400") and §11 line 596; OR (b) split Track 2 (≤1,500 LOC) from `bbnf-bench`-other (~1,000 LOC) into two budget rows with combined ceiling 2,500 LOC; OR (c) raise to 3,000 LOC if CSS-prior probe is in scope. The author should choose one and commit it. | REINVENT | Lane 6 |
| 3 | `restart/skinny/WORKSPACE.md:487-489` (§6 `mod loc`) | Strike "≤500 LOC Track 2 handwritten probe budget". Replace with: "the substrate-API correspondence checklist per BENCH.md §10.6 — Track 2 must call `runtime::tape::*` and `simd_scan::*` directly". | REINVENT | Lane 6 |
| 4 | `restart/skinny/WORKSPACE.md:590` (§10 last row) | Strike "≤500 Track 2 LOC". Replace with: "the substrate-API correspondence checklist per BENCH.md §10.6". | REINVENT | Lane 6 |
| 5 | `restart/skinny/WORKSPACE.md:596-604` (§11) | After re-arithmetic per surgery 2, update the "31,400 handwritten LOC plus ≤4,000 generated LOC" claim to the new total. | REINVENT | Lane 6 |
| 6 | `restart/skinny/WORKSPACE.md:537` (§8.1 mechanical-closure table) | Add a sixth row: `\| Tape Box<[T]> sealing inversion \| `Tape<'input>` seals tokens as `Box<[TapeToken]>` at parse boundary. \| V1 inverts: `TapeBuilder<'input>` (Vec or chunked) is upstream; `Tape<'input>` is the committed-snapshot projection. Read-side type-shape unchanged. \| 200-400 LOC additive (TapeBuilder) + 0 LOC change to `Tape<'input>` consumers. \|` This mirrors INDEX §"Open contradictions" row 6 and SUBSTRATE §1.2's "MECHANICAL with named inversion" classification. | REINVENT | Lens N, Lane 6 |
| 7 | `restart/skinny/WORKSPACE.md:142` (§3 `host_fns.default_registry = "host::primitives"`) + `restart/skinny/WORKSPACE.md:51` (§1.1 row 4) | Reconcile. Either: (a) the metadata schema is symbol-only and accepts "host::primitives" symbolically (skinny's `host_stubs` does not need to publish that symbol); spec the symbol-only behaviour in §3 commentary; OR (b) the `host_stubs` shim publishes a `host::primitives` namespace alias; spec the alias in §1.1 row 4. | REINVENT | Lane 1, Lane 3, Lane 5 |
| 8 | `restart/skinny/WORKSPACE.md:140` (§3 `host_registry = "skinny-none"`) + `restart/skinny/WORKSPACE.md:166` (§3 `[grammars.json.host] registry = "skinny-none"`) | The metadata validator (`grammar::metadata` per ARCH §5:729) must accept `"skinny-none"` as a sentinel. Spec the schema extension in §3 commentary: "the metadata validator (`crates/grammar/src/validate/host_registry.rs`, ~30 LOC) admits the `"skinny-none"` sentinel for the duration of skinny life. V1 graduation removes the sentinel; the cost is a rename in `grammars/json.bbnf` metadata block." | REINVENT | Lane 1 |
| 9 | `restart/skinny/WORKSPACE.md:170-175` (§3 `[grammars.json.optimization]`) | Either fold the four override fields (`pratt = "off"`, `simd = "json-structural-always"`, `literal_trie = "off"`, `regex_prefilter = "json-regex-only"`) into a single `profile = "skinny-json-curated"` row that the metadata reader resolves; OR keep them and explicitly extend the ARCH §5 schema enum to admit the four new values. The spec must pick one. | REINVENT | Lane 1, Lens I |
| 10 | `restart/skinny/WORKSPACE.md:139` (§3 `fixture_root = "crates/test-fixtures/corpus"`) | Reconcile with BENCH §3.2 (`tests/fixtures/json/`). Either: (a) `fixture_root` names the manifest dir (in `crates/test-fixtures/corpus`); the loader resolves to `tests/fixtures/json/` per manifest entries; OR (b) fixture acquisition writes directly to `crates/test-fixtures/corpus/json/`; BENCH §3.2 should be amended to match. Spec the resolution. | REINVENT | Lane 1, Lane 3 |
| 11 | `restart/skinny/WORKSPACE.md:46-48` (§1.1 row 2 LSP) + `:46-50` (§1.1 row 3 VM) + `:579` (§10 row 3 type-system) | Append wave-granular receivers per INDEX.md cross-references: "tranche I.W2 (LSP/DAP)" (row 2), "tranche E.W2 (VM debug-replay)" (row 3), "Tranche D" (§10 row 3). | REINVENT | Lane 8 |
| 12 | `restart/skinny/WORKSPACE.md:55` (§1.1 row 11 `pipeline` shim) + `restart/skinny/WORKSPACE.md:503` (§7 row 4 same shim) | Standardise on one phrasing. Recommended: "inlined as a 200-LOC orchestrator function `compile_grammar(metadata) -> Result<RustModule>` exported from `crates/bbnf/src/parse/pipeline.rs`, plus the regen subcommand body in `xtask/src/main.rs::regen`." | REINVENT | Lane 3 |
| 13 | `restart/skinny/WORKSPACE.md:130-132` (§3 workspace.deps) | Move `serde_json = "1"` from `[workspace.dependencies]` to BENCH-side dev-dep only. Verify by inspection of the per-crate role list — only `bbnf-bench` consumes `serde_json` (parity oracle's `serialize_canonical`). | REINVENT | Lane 3, Lens J |
| 14 | `restart/skinny/WORKSPACE.md:287` (§4.4 `passes/src/layout/types/`) + `restart/skinny/WORKSPACE.md:319` (§4.6 `runtime/src/grammars/`) | Add explicit Lock-13 ratification text: "deliberate single-child mount-point; reserves the V1 expansion path." Cite Lock 13's "cohesive concern" exception for fixed-shape regen mount-points. | REINVENT | Lane 1 |
| 15 | `restart/skinny/WORKSPACE.md:444-453` (§5 build-time guard discussion) | Add provenance for the 90-second clean-release-build target. Either: (a) cite a measurement source (e.g., the existing `crates/core/` clean release build observed at X seconds; skinny ≤ 90s as ≤ X-15%); OR (b) declare it engineering-only without Lock 8 anchor and explicitly disclaim SOTA framing. | REINVENT | Lens F, Lane 4 |
| 16 | `restart/skinny/WORKSPACE.md:600-604` (§11 closure conditions) | Add a 7th condition: "Cross-quadrant deviation ledger consistency (this WORKSPACE §8.1 mirrors INDEX.md §'Open contradictions' deviation rows). A row here that disagrees with INDEX is a synthesis-pass fault." | REINVENT | Lens N |

Total: **16 surgical edits**, all narrow / mechanical. Most cluster around Lane 6 (5 edits on the stale Track 2 cap + budget arithmetic).

---

## §15 Final readiness verdict

> **Decision: SK-AMENDMENT-REQUIRED-NARROW.**
>
> WORKSPACE.md SK-V1 survives the 17-lens audit (Lanes 1, 3, 4, 5, 6, 7, 8, 9 + Lenses F, G, H, I, J, K, L, M, N) with no architectural rewrites required. The dominant fault class is **post-redress drift**: the BENCH redress dropped the Track 2 ≤500 LOC cap and refreshed the `bbnf-bench` LOC decomposition, but WORKSPACE.md still cites the stale cap in four places (§1.1, §2 table, §6 xtask body, §10 omissions table) and the §2 `bbnf-bench` budget arithmetic no longer balances against BENCH §11.1's revised internal split. Two cross-quadrant inconsistencies (`fixture_root` path between WORKSPACE §3 and BENCH §3.2; `pipeline` shim location between §1.1 and §7) round out the cohesion lane. Lens N surfaces one missing deviation row (Box<[T]> sealing inversion) in §8.1 that INDEX.md and SUBSTRATE.md both carry. Lens L correctly classifies the 14 enumerated omissions (FAITHFUL with V1-grammar caveat for the GADT/DK13/CSP and multi-grammar omissions; FAITHFUL-conditional-on-probe for host-fn cuts; FAITHFUL via delegation for SUBSTRATE-side and BENCH-side cuts).
>
> The 31,400 handwritten LOC ceiling holds **only if** punch-list item 2 is applied — the `bbnf-bench` line item flexes by 200-1,000 LOC depending on whether Track 2 is split out and whether CSS-prior probe is in scope. The author must commit one of three resolutions before SK-V2 dispatch. None of the 16 surgical edits requires re-architecture; all are textual-mechanical and close in a single SK-V2 amendment cycle.
>
> Hereupon: dispatch the WORKSPACE-amendment agent against the 16-row punch list. The amended WORKSPACE.md re-enters the SK-V1 cohort for SKINNY-SUITE consolidation; SK-V2 is a verify-then-rerun cycle, not a re-draft.
