# HARDENING-SUBSTRATE-SK-V1

## §1 Target identification

- **Target**: `restart/skinny/SUBSTRATE.md`
- **Lines audited**: 1-583 (full file)
- **Cycle**: SK-V1 (initial; first independent audit cycle of the post-redress skinny corpus)
- **Lens stack applied**: Lanes 1-9 + Lenses F/G/H/I/J/K + Lenses L/M/N
- **Sister quadrants cross-checked**: COMPILER.md (especially §1.3, §4.4, §6.3); BENCH.md (§1.2, §3.4, §7.8); INDEX.md (deviation ledger)
- **Authority cross-checked**: ARCH §9 (lines 1373-1426), ARCH §3.1 (lines 191-244), Locks 1, 8, 9, 13, PASS-3 §4 (lines 150-191)
- **Time consumed**: ~30 minutes

The SK-V1 redress between author and prior cycle landed two new entries in the INDEX deviation ledger (Box<[T]> sealing; HM-as-top-level inversion) and one multi-paragraph deviation note inside SUBSTRATE.md §1.2 explaining the sealing→`TapeBuilder` graduation path. The audit verifies whether those landings are clean and whether the substrate spec is now SK-READY.

## §2 Cohort verdict

| Lane / Lens | Verdict | KEEP/FAITHFUL/MECHANICAL | REINVENT/CAVEAT | DISCARD/MASKING/ANTI-MECH | Recommendation |
|---|---|---:|---:|---:|---|
| Lane 1 — Lock adherence | honoured-with-narrow-amendment | 6 | 1 | 1 | Lock 1 silent on the `parse` signature drift; Lock 13 scrutinised |
| Lane 2 — Sequencing | N/A | — | — | — | Skinny is single-wave |
| Lane 3 — Cohesion | violated | 3 | 4 | 0 | Cross-quadrant API mismatches (arena counters, parse signature, JsonRoot shadow) |
| Lane 4 — SOTA anchoring | honoured | 4 | 0 | 0 | All anchors trace to ARCH §11 row + Lock 8 |
| Lane 5 — Grammar-authoritative | violated-narrow | 3 | 1 | 1 | `decode_string` mentioned but the SUBSTRATE-side surface is silent; `JsonObjectOpen → 0u16` hardcode is fault |
| Lane 6 — LOC budget | silent-must-add | 0 | 1 | 0 | SUBSTRATE.md carries no per-section LOC budget; defers entirely to WORKSPACE |
| Lane 7 — Friction | partial | 1 | 2 | 0 | `JsonRoot` overloaded name + `'doc`/`'input` discriminant burden are friction surfaces uncovered by error-message commitments |
| Lane 8 — Carry & deferral | violated | 4 | 2 | 1 | Several "BENCH agent" / "post-skinny" / "future V1" carries lack a named gate row |
| Lane 9 — Greenfield | honoured | 5 | 1 | 0 | One contrivance — feature-gated `writes/allocations` counters duplicate what `Tape` ought to expose |
| Lens F — LLM bias | partial | 4 | 3 | 0 | "Hot path", "load-bearing", "byte-clean", "byte-faithful" used as ornament not mechanism |
| Lens G — Overfitting | partial | 2 | 3 | 0 | `STRING_BORROWS_SOURCE` flag is JSON-shaped; Lens G caveat for CSS L4 / Sheets unstated |
| Lens H — Provenance | violated-narrow | 4 | 1 | 1 | One PASS-3 line citation drifts; PASS-3 token shape carries `payload + sibling_skip` fields, SUBSTRATE collapses to `payload_or_skip` and cites the page but not the divergence |
| Lens I — Contrivance | partial | 5 | 1 | 1 | Visitor §6 carries 50+ LOC of trait surface and stub list when only the read-traversal touchpoint is required |
| Lens J — Host leverage | honoured | 3 | 1 | 0 | `PhantomData<fn() -> K>` for auto-trait control is sound Rust idiom |
| Lens K — Meta-grammar discipline | honoured | 2 | 1 | 0 | Substrate is properly grammar-neutral; one JSON-specific bleed at §3.3 (string-content prefilter) |
| **Lens L — Premise fidelity** | **mostly FAITHFUL with three caveats** | **8** | **2** | **1** | Two cuts are FAITHFUL with V1-grammar caveat; one (visitor stub list) borders MASKING |
| **Lens M — Falsifiability** | **N/A for SUBSTRATE** | — | — | — | SUBSTRATE owns no thresholds; falsifiability is a BENCH lens |
| **Lens N — Graduation mechanicality** | **MECHANICAL with named inversion** | **2** | **1** | **0** | Box<[T]> sealing graduates as projection; `passes::layout`-passthrough graduates by wrapper relocate; both audit-defended |

**Final readiness signal: SK-AMENDMENT-REQUIRED-NARROW.** The substrate spec's premise-fidelity story survives steelman; the redresses landed cleanly under Lens N. The amendment scope is bounded — three cross-quadrant API names, one shadow type identifier, one missing payload-class enumerator, the parse-signature drift, three carries to name, and one diagnostic-friction surface. None block dispatch on principle; all block dispatch on coherence.

## §3 Lane 1 — Lock adherence

The 14-lock table walked against SUBSTRATE.md.

| Site | Lock | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|---|
| §1.2:73-91 | Lock 1 | Tape is THE substrate; no parallel substrate; no OpenFrame clone | The skinny `Tape<'input>` carries `source`, `tokens: Box<[TapeToken]>`, `payloads: PayloadArena`, `id: TapeId`. No second tree. §5:429 explicitly states "No second tree." | Honours Lock 1's structural insight and the 2026-05-04 reframe (tape as proper substrate, no rename). PASS-3 §4 (line 187) admits its own layout illustrative; the skinny pins one without contrivance. | None of substance. The collapse of `payload + sibling_skip` into one `payload_or_skip` slot is a deviation from PASS-3 §4 illustration but is justified at §1.1:60-66 and is a settled design choice within Lock 1 spirit. | Steelman: PASS-3 declares the layout illustrative; the deviation costs nothing. The skinny still lands one-substrate-no-OpenFrame. Defeated. | **KEEP** |
| §1.3:117 vs §4.3:410 vs sister COMPILER.md §6.3 | Lock 1 (silent) + Lock 9 cohesion | The `parse` API signature | §1.3 reads `parse(&'a [u8]) -> Result<Self::View<'a>, _>`; §4.3 reads `parse(&'a str) -> JsonDocument<'a>`; COMPILER.md §6.3:530 reads `parse<'i>(input: &'i str) -> Result<JsonRoot<'i>, ParseError>`. ARCH §3.1:202 reads `parse<'a>(&self, input: &'a [u8]) -> Result<Self::View<'a>, ParseError>`. Three signatures across two skinny files; ARCH-divergent. | If the skinny intends `&str` with UTF-8 prevalidation outside the timed region (§1.3:119), that is a deliberate skinny deviation. | The deviation is undeclared. The skinny has not stated whether it conforms to ARCH §3.1's `&[u8]` shape or sheds it for `&str`. The reader cannot tell. | Steelman: the skinny does not need to land the full ARCH grammar trait; `&str` is a deliberate prevalidation cut. **Counter:** if so, INDEX.md must enumerate the deviation; right now INDEX.md is silent on parse-API divergence. The SUBSTRATE-internal contradiction (line 117 vs line 410) is a separate fault. | **REINVENT** — pick one. State whether the skinny `Grammar::parse` takes `&[u8]` (ARCH-faithful) or `&str` (skinny-deviation requiring INDEX entry). |
| §1.4:155-162 | Lock 9 (slice-borrow primary) | `JsonDocument::root_value` collapses `'doc = 'input` | §1.4 declares `'doc` and `'input` are unified at `JsonDocument`; the cold `OwnedDocument` wrapper is the escape. | Aligns with Lock 9's "slice-borrow primary" mandate. The lifetime collapse keeps the API surface small. | The Lock 9 amendment requires ALL THREE forms (`parse`, `parse_in`, `parse_owned`); §1.3:118-119 admits `parse_owned` is "cold wrapper" only. No bumpalo arena story. | Steelman: the skinny is a SOTA-throughput probe; only `parse` matters. **Counter defeated:** Lock 9 names three; the skinny's substrate exposes only one with a defer for the second; the third (`parse_in`) is half-named ("the `Arena` only widens the payload arena's backing storage"). The full bumpalo escape hatch is silent. | **KEEP with carry** — the skinny may legitimately defer `parse_in` and `parse_owned` as cold wrappers, but the substrate-side `parse_in` shape (does the caller's `&Arena` widen `PayloadArena.bytes`? does it back-the existing arena? does it alias?) needs one paragraph. |
| §3.5 throughput targets | Lock 8 | SOTA anchors: 7 GB/s AVX2; 5 GB/s NEON | Cited verbatim from ARCH §11:1519. | Provenance is correct. | None. | None to defeat. | **KEEP** |
| §1.2:80-82 | Lock 13 | `Box<[T]>` not `Vec<T>` | The sealing is justified for codegen quality and cache-line discipline. | Codegen reads tokens through `&Tape<'input>`, single allocation, no len/cap divergence. | Forecloses incremental append (§1.2:99 redress note acknowledges this; ledger row added in INDEX). | Steelman: the redress note + INDEX deviation row land cleanly under Lens N MECHANICAL with named inversion. **Defeated** — the named inversion ("skinny seals at parse boundary; V1 seals at snapshot boundary") is reversible without rewriting `Tape<'input>` or `ValueRef`. | **KEEP** |
| §9:545-568 | Lock 13 | Module layout (no god directories) | `runtime/src/{tape,visitor,grammars/json}/` with 6 children under `tape/`. | 6 ≤ 10; passes Lock 13's child-count rule. | `tape/` carries `mod.rs, token.rs, builder.rs, payload.rs, scan.rs, view.rs` — all peer concerns. No mixed-domain god dir. | Steelman: the layout matches PASS-3 §6's `bbnf-runtime` shape, which itself passed audit. Defeated — no fault. | **KEEP** |
| §3.1:236 dispatch | Lock 14 | `is_x86_feature_detected!`, runtime ISA dispatch | Substrate-level glue routes to `simd_scan::{avx2,neon,scalar}::structural_scan`; no per-grammar arms. | Honours Lock 14's "zero grammar-specific code in generic crates" — the dispatcher names ISAs not grammars. | None. | Defeated. | **KEEP** |
| §1.1:68 + §3.1 | Lock 14 (narrow) | `JsonObjectOpen → 0u16, JsonObjectClose → 1u16` mapping | The substrate text says "the mapping is generated per grammar; the substrate never sees the names" (line 68) — which is right. But the §1.1 prose also asserts the JSON-specific kind values inline. | The disclaimer is correct; the prose intent is right. | The text "JsonObjectOpen → 0u16, JsonObjectClose → 1u16, etc." appears in a substrate-spec section. A reader could mistake this for a substrate ground truth. | Steelman: the comment is illustrative ("the mapping `JsonObjectOpen → 0u16` ... is generated per grammar"). **Defeated** — but a one-line clarification ("illustrative; the substrate exposes only `NodeKindId`, not the labels") would foreclose Lock 14 ambiguity. | **REINVENT** — restate "illustrative" inline at §1.1:68 to foreclose Lock 14 grammar-bleed risk. |

Lane 1 verdict: **honoured-with-amendment**. 6 KEEP; 1 REINVENT (the parse-API trifurcation); 1 narrow REINVENT (the JSON-kind illustration). No DISCARD.

## §4 Lane 2 — Sequencing discipline

**N/A** — the skinny is single-wave per the skinny HARDENING.md §4 contract. No sequencing claims to verify.

## §5 Lane 3 — Cohesion

Every claim must be verifiable from artefacts the target produces or cites. SUBSTRATE.md cohesion against its sister quadrants is the load-bearing cohesion check.

| Site | Item | Explication | Pros | Cons | Challenge | Verdict |
|---|---|---|---|---|---|---|
| §1.4:151 vs COMPILER.md §6.3:530 | `JsonDocument` vs `JsonRoot` as parse return type | SUBSTRATE.md §1.4 declares `pub struct JsonDocument<'input>` as the public root and `Json::parse` returning `JsonDocument<'a>`. COMPILER.md §6.3 declares `pub struct Json` with `parse(&'i str) -> Result<JsonRoot<'i>, ParseError>`. Two siblings disagree on the return type. | Each quadrant on its own is internally consistent. | Cross-quadrant return type does not match. INDEX.md cross-quadrant invariant 3 ("Tape + direct-to-struct as one substrate") implies one return shape. | Steelman: `JsonRoot<'i>` may be a typed projection over `JsonDocument<'i>` (i.e., `JsonDocument::root_value() -> JsonRoot<'i>`). **Defeated counter:** if so, COMPILER.md should call `parse(&str) -> Result<JsonDocument<'i>, _>` and let the user call `.root_value()`. The current shape skips the document and returns the root directly, which is incoherent with SUBSTRATE.md §1.4 where root_value is a method on `JsonDocument`. | **REINVENT** — name the return type. Either (a) `parse(&str) -> JsonDocument<'i>` and let the user call `root_value()`, or (b) `parse(&str) -> JsonRoot<'i>` directly and drop `JsonDocument`. The skinny does not need both. |
| §1.3:127 + §4.1:321 | Two `JsonRoot` types | §1.3 declares `pub enum JsonRoot {}` (uninhabited marker for kind discrimination). §4.1 declares `pub struct JsonRoot<'doc, 'input: 'doc> { cursor: ValueRef<'doc, 'input, super::JsonRoot> }`. Same name; two distinct types. | The shadow is technically resolvable via `super::JsonRoot` import path. | Identifier shadowing of this strength is a deliberate friction surface (Lane 7) and a Lens F (verbal complexity hiding semantic ambiguity) violation. The reader sees `JsonRoot` in two places and must context-switch. | Steelman: Rust permits the shadow; codegen will emit it. **Defeated:** Lock 13's "uniform sibling APIs" pattern argues against name overload at this scale. The kind marker is `JsonRootKind` or `kind::JsonRoot`; the typed view is `JsonRoot`. | **REINVENT** — rename the kind-marker enum to `kind::JsonRoot` or `JsonRootKind` (matching how `AnyKind` reads as a kind tag). The typed-view struct keeps the `JsonRoot` name. |
| §2:217 vs BENCH.md §3.4:350-353 | Arena counter API surface | SUBSTRATE.md exposes `PayloadArena::write_count() -> u32` and `allocation_count() -> u32`. BENCH.md §3.4 calls `t1.payload_arena_writes()` and `t1.payload_arena_allocations()` — methods on `Tape`, not `PayloadArena`, and named `_writes` / `_allocations` (plural noun) not `write_count` / `allocation_count`. | Each side is internally readable. | The methods don't match. BENCH cannot call SUBSTRATE's API as written. | Steelman: the BENCH side is shorthand for `t1.tape.payloads.write_count()`. **Counter defeated:** the parity oracle uses `t1.payload_arena_writes()` directly; the underlying field path is private; and the cross-quadrant invariant 3 says "tape + direct-to-struct as one substrate" — the substrate must expose the methods on `Tape` (or `JsonDocument`) not bury them three indirections deep. | **REINVENT** — SUBSTRATE.md §2 must add `Tape::payload_arena_writes()` and `Tape::payload_arena_allocations()` (or rename to BENCH's call sites). Either is acceptable; misalignment is not. |
| §1.1:46-54 vs §2 table | `INLINE_STRING_BORROW` payload class undefined in the discriminator | §1.1 enumerates payload classes: `INLINE_BOOL_NULL`, `INLINE_NUMBER_FAST`, `ARENA_OFFSET`, `SIBLING_SKIP`. §2 table at line 178-179 names `INLINE_STRING_BORROW` as a payload class. `INLINE_STRING_BORROW` is not in §1.1's list. | The intent reads through. | The 4-bit `PAYLOAD_CLASS` field has 16 possible values; only 4 are named in §1.1; one more (`INLINE_STRING_BORROW`) is named in §2. Either §1.1 is incomplete or §2 names an off-roster class. | Steelman: §1.1 is illustrative and the full enum is in `runtime/src/tape/token.rs`. **Defeated:** the spec should name the full class roster at §1.1 because the spec is the contract. The 4-bit field with 5 used values out of 16 leaves 11 reserved; that should be explicit. | **REINVENT** — extend §1.1's enum sketch to include `INLINE_STRING_BORROW` (and any other classes §2 references); list reserved values. |
| §8:521 hand-coded surface | `build_tape_for_json` API placement | The fn lives in SUBSTRATE.md §8 and is "Public to the workspace; gated by `#[doc(hidden)]`". Its caller is BENCH-side (Track 2 hand-coded parser). | The contract is named. | The function's home crate is unspecified; the spec just shows it as a free fn. The §9 module layout doesn't list it. Where in `runtime/src/tape/` does it live? | Steelman: §9's `builder.rs // TapeBuilder, push_token` accommodates it. **Counter defeated:** `build_tape_for_json` is not `push_token`; it is a JSON-specific entry. Per Lock 14, it must NOT live in `runtime/src/tape/builder.rs` (that path is grammar-neutral); it must live in `runtime/src/grammars/json/` (per-grammar) — and it can't, because that file is generated code not hand-public. | **REINVENT** — Lock 14 conflict. The hand-coded JSON parallel is BENCH-side; either (a) move `build_tape_for_json` to `crates/bbnf-bench/src/track2/json.rs` and let it call `runtime::tape::TapeBuilder::push_token` with no JSON-specific helper in the substrate, or (b) acknowledge the helper as a JSON-specific BENCH-only escape and tag it `#[cfg(feature = "bench")]` in `runtime/src/grammars/json/`. Either way, the current "Public to the workspace" framing leaves Lock 14 ambiguous. |
| §6:482-489 visitor stubs | Visitor surface | The trait list (`Visit`, `JsonVisitor`) is named; the stubs (`VisitMut`, `VisitTypes` bitflag pruning, `LayoutVisitor`) are listed as omitted. | Honest deferral. | The stubs land at trait surfaces (e.g., `VisitMut`) without naming a V1 receiver gate (BENCH? COMPILER? MIGRATION?). The deferral is named but not landed. | Steelman: `INDEX.md` "What the skinny is NOT testing" ledger covers `Visit::Mut` ("read-only is sufficient for parse-throughput SOTA"). **Defeated** — that's the rationale. The receiver-gate naming is still partial: `VisitTypes` bitflag pruning is missing from INDEX.md's NOT-tested table, and PASS-3 §3 ("the W5 visitor pruning trick") expects it. | **KEEP with carry** — add `VisitTypes::pruning` to INDEX.md "What the skinny is NOT testing" with V1 owner row. |
| §5:417 identity proof | "Snapshot identity" | §5 declares "every public node has one `(TapeId, node id, payload class)` identity" cited from ARCH:1401. | Provenance correct. The identity proof on paper at §5:427 is well-argued. | The proof relies on `tokens[index].flags & PAYLOAD_CLASS_MASK`. `PAYLOAD_CLASS_MASK` is not declared. The §1.1 token says PAYLOAD_CLASS is 4 bits but doesn't fix the bit position. | Steelman: deferred to implementation. **Defeated for spec purposes** — a 16-byte token with 4-bit fields needs the bit layout to be canonical, especially because BENCH's parity oracle compares tokens byte-equal. | **REINVENT** — name the bit layout for `TokenFlags` at §1.1 (or a sibling table). |

Lane 3 verdict: **violated-narrow**. 3 KEEP; 4 REINVENT — all cross-quadrant API names that, in implementation, would error at compile-time.

## §6 Lane 4 — SOTA anchoring

| Site | Item | Verdict |
|---|---|---|
| §3.5 | AVX2 ≥ 7 GB/s, NEON ≥ 5 GB/s | **KEEP** — cited from ARCH:1519, anchored against simdjson on Intel Skylake / Apple Silicon |
| §0:11 | sonic-rs / simdjson / lightning-css anchors | **KEEP** — Lock 8 anchored |
| §0:18 | "≥7 GB/s AVX2, ≥5 GB/s NEON, scalar parity hash mandatory" | **KEEP** |
| §3.4 | Scalar parity hash | **KEEP** — bench-time only; runtime trusts after corpus validation |

SUBSTRATE.md does not own throughput thresholds; those live at BENCH.md. SUBSTRATE.md owns floor-class commitments (the kernel must be byte-faithful to scalar). All cited correctly. **Lane 4: honoured.**

## §7 Lane 5 — Grammar-authoritative discipline

| Site | Item | Pros | Cons | Verdict |
|---|---|---|---|---|
| §1.1:68 | "JsonObjectOpen → 0u16, JsonObjectClose → 1u16" | Marked "generated per grammar; the substrate never sees the names" | Inline in a substrate-spec doc, easy misread; see Lane 1 entry | **REINVENT** (small) |
| §3.3:279 | "string-content scans" using `SimdScanMode::Prefilter` | Honest declaration that string-content is JSON-style; substrate accepts both modes | The Prefilter route's verifier (looking for the closing `"` past escapes) is JSON-shaped — CSS strings have different escape rules; Sheets has different quoting | **REINVENT** — move the JSON-specific Prefilter verifier description into §4 (typed-view JSON-specific) or into COMPILER.md; the substrate should declare only that Prefilter mode exists and the verifier is a per-grammar concern |
| §1.1 `STRING_BORROWS_SOURCE`, `STRING_NEEDS_UNESCAPE` | Two flags carrying JSON-specific semantics on a grammar-neutral token | These two flag names will not load-bear for CSS L4 (CSS uses `Cow` directly without unescape) or Sheets | These flag names appear in a grammar-neutral struct definition | **REINVENT** — rename to `PAYLOAD_BORROWS_SOURCE` / `PAYLOAD_NEEDS_NORMALIZE` (or similar grammar-neutral names) and document in §1.1 that the meaning of "needs normalize" is per-grammar |
| §4.1:367 `unescape_json` | JSON-specific unescape function in `view.rs` | The function is in `runtime/src/grammars/json/view.rs` (per §9) — that path is per-grammar, hand-stubbed for skinny, generated otherwise. Lock 14 honoured. | None — this is correct placement | **KEEP** |
| §3.1 dispatch | ISA dispatch by `cfg(target_arch)` and `is_x86_feature_detected!` | No grammar arms; only ISA arms | None | **KEEP** |

Lane 5 verdict: **violated-narrow**. The substrate doc bleeds JSON-specific identifier names (`STRING_*`) into grammar-neutral structures. Acceptable in skinny scope (one grammar) but a Lens G overfitting risk for V1 graduation; the rename is one commit.

## §8 Lane 6 — LOC budget

| Site | Item | Verdict |
|---|---|---|
| Whole file | No per-section LOC budget at all | SUBSTRATE.md defers to WORKSPACE.md per its scope. WORKSPACE §2 budgets `runtime` at 4,000 LOC and `simd-scan` at 3,500. No SUBSTRATE-internal split. | **silent-must-add** — SUBSTRATE.md should at minimum echo the WORKSPACE budget split per `tape/`, `visitor/`, `grammars/json/skeleton` so the SUBSTRATE author knows where the 4,000-LOC budget lands. The spec without per-section LOC commitment leaves implementation drift unbounded. |

The skinny enforces JSON ≤4,000 LOC generated (per WORKSPACE) but does not constrain SUBSTRATE's emission split. If the codegen `view.rs` (~250 LOC budget per COMPILER §6.2) drifts to 600 LOC, the SUBSTRATE-side handwritten contract is silent on the budget pressure.

**Lane 6 verdict: silent — must add.** Echo WORKSPACE.md §2's `runtime/` 4,000-LOC split per the §9 module list.

## §9 Lane 7 — Friction forecast

| Friction surface | User | Mental model gap | Verbatim error message recommended |
|---|---|---|---|
| `JsonRoot` overload (kind marker vs typed view) | Generator output reader | Reader sees `JsonRoot` in `JsonDocument::Root = ValueRef<_, _, JsonRoot>` and again as a typed view; thinks both are the same type; rustc errors "expected struct, found enum" | "`JsonRoot` is overloaded in skinny — `kind::JsonRoot` is an uninhabited kind tag; `JsonRoot<'doc, 'input>` is the typed view. Use `kind::JsonRoot` in `ValueRef` parameters" |
| `'doc` and `'input` lifetime discriminant | Bench author calling `parse_in` | Reader sees `<'doc, 'input: 'doc>` and tries to express the input outliving the document; unsure why both exist when they collapse for `parse` | "The lifetime parameters discriminate Lock 9's three forms: `parse` collapses `'doc = 'input`; `parse_in` keeps them collapsed; `parse_owned` splits them via `OwnedDocument`. For `parse(&str)`, both are `'a`" |
| `payload_or_skip` field discriminator | Bench author handcoding Track 2 | Reader does not know whether to read `payload_or_skip` as `payload` or `sibling_skip`; reads `flags.PAYLOAD_CLASS` first; if that's a sibling-skip class, treats as offset count | "`TokenFlags::PAYLOAD_CLASS` discriminates: `INLINE_BOOL_NULL` and `INLINE_NUMBER_FAST` ignore `payload_or_skip`; `INLINE_STRING_BORROW` and `ARENA_OFFSET` interpret as payload pointer; `SIBLING_SKIP` interprets as token-slot count" |
| `parse(&[u8])` vs `parse(&str)` ambiguity | Skinny implementor | Reader sees three signatures across SUBSTRATE.md §1.3, §4.3, COMPILER.md §6.3; cannot tell which is canon | (Spec must pick) |

| Verdict |
|---|
| **REINVENT-narrow** — at minimum the JsonRoot overload (already recommended in Lane 3) and the `payload_or_skip` discriminator commentary; the `'doc`/`'input` story is sound but a friction surface that the COOKBOOK (deferred to V1 G tranche) must absorb. |

## §10 Lane 8 — Carry & deferral audit

Every "deferred to" / "carries to" / "future" must name receiver, blocker, gate.

| Site | Carry | Receiver | Blocker | Gate | Verdict |
|---|---|---|---|---|---|
| §1.2:99 | Box<[T]> sealing → V1 `TapeBuilder<'input>` | tranche I (incremental) | Box<[T]> precludes append-after-parse | INDEX deviation ledger row + tranche I `ReparsePlan` gate | **KEEP** — well-named |
| §1.4:165 | `OwnedDocument` cold wrapper for `'doc != 'input` | tranche I or future SUBSTRATE addition | `'doc` and `'input` collapsed in skinny | (silent) | **REINVENT** — name the V1 receiver crate (`runtime/src/owned/`?) |
| §2:217 | "If the bench shows arena cache pressure on the empty path, the field becomes `Option<Box<PayloadArena>>` behind a feature gate" | unspecified | bench-driven | (silent) | **REINVENT** — name BENCH outcome that triggers the change. Is this an SK-V2 amendment? An H tranche signal? Without a gate, this is hedge-not-commitment. |
| §6:482-489 | `VisitMut`, `VisitTypes` bitflag pruning, `LayoutVisitor` / `@pretty` | Tranche I, F, G respectively | none stated for VisitTypes | partially named | **REINVENT-narrow** — see Lane 3 entry |
| §10 | Open questions list (4 entries) | "bench result will turn them into commit-able decisions" | bench-pending | BENCH.md §6 matrix? | **KEEP** — the §10 framing is honest open-question-pending-measurement; matches Lens M intent |
| §1.4:151 | `OwnedDocument` wrapper "self-references" | future | self-reference is V1-cold-path | (silent) | **REINVENT** — name the V1 receiver |
| §4.1:386 | "`parse-that`'s number kernel here post-skinny" | `parse-that` regex/number sub-crate | post-skinny implementation | tranche? | **REINVENT** — `parse-that` is named in WORKSPACE.md as a V1 crate; SUBSTRATE.md should name the skinny-substitute (`std::str::FromStr`) and the V1 receiver explicitly |

Lane 8 verdict: **violated**. 4 KEEP; 2 REINVENT (carry receivers); 1 KEEP (open-question framing). Three carries (`Option<Box<PayloadArena>>`, `OwnedDocument`, `parse-that` number kernel) need named receivers + gates.

## §11 Lane 9 — Greenfield discipline

| Site | Item | Verdict |
|---|---|---|
| §1.1:60-66 | `payload_or_skip` union vs split fields | Honest steelman of trade-off: 16-byte token vs 24-byte; cache-line discipline favours 16; fallback path named | **KEEP** |
| §1.2 redress note | Box<[T]> sealing trade-off | Steelmanned; named the V1 closure path | **KEEP** |
| §2:185-196 | Feature-gated `writes/allocations` counters | Bench-only counters; cleanly gated | **KEEP** |
| §2:217 | "If the bench shows arena cache pressure...field becomes `Option<Box<PayloadArena>>`" | Speculative response to unmeasured pressure | **REINVENT** — see Lane 8 |
| §3.4:284-295 | Scalar parity hash + ScanReport struct | Mechanism named | **KEEP** |
| §5:415-429 | Identity proof on paper | Argued cleanly; defeats the obvious challenge | **KEEP** |
| §6:482-489 | Visitor stubs list | Slightly contrivance-prone — the §6 trait surface (50+ LOC of trait definition + walker defaults) exceeds what BENCH needs (read-traversal touchpoint only). | **REINVENT** — see Lens I |

Lane 9 verdict: **honoured**. The greenfield voice is consistent; trade-offs are steelmanned; one apparatus drop opportunity surfaced (visitor §6) under Lens I.

## §12 Lens F — LLM bias

| Site | Pathology | Subclass | Recommendation |
|---|---|---|---|
| §1.1:58 "the SOTA target lives or dies by token-cache density" | Confident generality | Buzzword reliance | Replace with mechanism: "AVX2 may load eight tokens (128 bytes) with two aligned 32-byte loads; a 24-byte token costs three loads for the same window, +50% scan cost" |
| §1.2:80 "removes the `len/cap` divergence and improves codegen for traversal" | Buzzword reliance / unfalsifiable | Unfalsifiable claims | Cite the codegen win: "rustc emits `[T]::iter()` over `&[TapeToken]` without bounds-check elision through `len`; equivalent over `Vec<T>` re-loads `len` per iteration on `-O0`" or — if not measurable — strike |
| §1.3:121 "`Copy`/`Send`/`Sync` regardless of `K`'s auto-trait posture" | Verbal complexity hiding semantic ambiguity | Nominalisation | Rephrase: "`PhantomData<fn() -> K>` makes `ValueRef` `Copy + Send + Sync` even if `K` is `!Send`" |
| §2:217 "BENCH must assert `PayloadArena::write_count() == 0`" | Hedging where commitment is needed | Soft-imperative | "BENCH **asserts** `write_count() == 0` ... If the assert fails, the bench row is INVALID and the run halts" |
| §3.1:236 "is_x86_feature_detected!" check "once per `Tape` build via `std::sync::Once`" | Pseudo-precise numerics | Semi-mechanism | The `std::sync::Once` is correct mechanism; KEEP |
| §3.4:286 "kernel_id: KernelId" | Apparatus chain | Cardinality bloat | Sufficient — three variants (Avx2, Neon, Scalar), all load-bearing |
| §4.1:373 "SAFETY: JSON parser validated UTF-8 boundaries during the structural scan's verifier route" | Confident generality | Wrong — SIMD structural scan does NOT validate UTF-8 boundaries; that's a separate UTF-8 prevalidation pass. The SAFETY comment is cargo-culted | **REINVENT** — either cite the UTF-8 prevalidation path (§1.3:119 mentions "with UTF-8 prevalidation outside the timed region") or strip the SAFETY comment in favour of `from_utf8` (checked) |

Lens F verdict: **partial — 4 honoured, 3 REINVENT**. The SAFETY comment at §4.1:373 is the most load-bearing fault — `unsafe { str::from_utf8_unchecked(raw) }` in production code needs a discharged invariant; the present comment claims an invariant the spec elsewhere disclaims. This is a soundness gap in the spec.

## §13 Lens G — Overfitting

| Site | Pathology | Verdict |
|---|---|---|
| `STRING_BORROWS_SOURCE`, `STRING_NEEDS_UNESCAPE` flag names | JSON-shaped flag names on a grammar-neutral token struct | **REINVENT** — see Lane 5 |
| §3.3:279 "string-content scans" using Prefilter | JSON-specific verifier described in grammar-neutral §3.3 | **REINVENT** — see Lane 5 |
| §4.1 `unescape_json`, `JsonObject`, `JsonArray` typed views | Grammar-named in `runtime/src/grammars/json/`, generated per grammar | **KEEP** — correct placement |
| §1.4:151 `JsonDocument` | Per-grammar root struct | **KEEP** — correct placement |
| §6 visitor `JsonVisitor` trait | Generated per grammar | **KEEP** — correct placement |

Lens G verdict: **partial — JSON-specific identifiers leaked into substrate-grammar-neutral types** (`STRING_*` flag names; Prefilter verifier description). Cosmetic, but Lock 14's "zero overfitting" demands the rename. Counter-example: `payload_or_skip` is grammar-neutral and load-bearing.

## §14 Lens H — Hallucination + provenance

| Site | Citation | Status |
|---|---|---|
| §0 | ARCH §9 (lines 1373-1426) | ✓ verified |
| §0 | ARCH §3.1 (lines 191-244) | ✓ verified |
| §0 | PASS-3 §4 (lines 150-191) | ✓ verified |
| §0 | ARCH §11 row `simd/structural_scan` (line 1519) | ✓ verified — exactly matches "≥7 GB/s AVX2, ≥5 GB/s NEON, scalar parity hash" |
| §0:20 | "PASS-3.md:187" — "This layout is not a PASS-1 mandate" | ✓ verified — exact quote present at PASS-3 §4 line 187 |
| §1.2:99 | "`ReparsePlan` per ARCH §3.3" | ⚠ — ARCH §3.3 not directly verified in this audit window; but PASS-3 §5:208-220 carries `ReparsePlan` definition. The §3.3 anchor may be displaced; verify-and-fix is one search. |
| §4.2:404 | "`materialisation_cost.toml` per `restart/audit/pass-3-runtime/PASS-3.md:144`" | ✓ verified — PASS-3 line 144 references `materialisation_cost.toml` |
| §1.1 PASS-3 deviation | PASS-3 §4 carries `payload + sibling_skip` as separate fields; SUBSTRATE collapses to `payload_or_skip` | ✓ — declared deviation, justified |

Lens H verdict: **honoured-narrow**. One citation (`ARCH §3.3`) is unverified in this audit window; one path-line citation (PASS-3 §4) is correctly traced. The substrate is well-grounded. **REINVENT** — verify ARCH §3.3 carries the `ReparsePlan` reference; if it lives at PASS-3 §5 instead, update §1.2:99.

## §15 Lens I — Contrivance

| Site | Apparatus | Recommendation |
|---|---|---|
| §6 visitor trait + walkers | The `Visit<'doc, 'input, K>` trait + the `JsonVisitor` trait + 6 default `walk_*` methods + 4 default `visit_*` methods | The §6 contract is overscoped for the bench's "read-twice" touchpoint. BENCH needs a way to walk the tape and touch every value. The default walker can be a single ~40-LOC fn. **SIMPLIFY** — collapse `JsonVisitor` to one method `for_each_value(tape, &mut FnMut(JsonValueRef))` and absorb the named-method ceremony into V1's PASS-3 §3 contract. |
| §1.1 `TokenFlags` field | 16-bit bitfield with 4-bit PAYLOAD_CLASS, 1-bit HAS_SCALAR_CACHE, 1-bit STRING_NEEDS_UNESCAPE, 1-bit STRING_BORROWS_SOURCE, 1-bit IS_STRUCTURAL_OPEN, 1-bit IS_STRUCTURAL_CLOSE, 2-bit RECOVERY_KIND, 5-bit reserved | RECOVERY_KIND is on a token even though §7 says recovery is OMITTED. The 2 bits are dead in the skinny. **SIMPLIFY** — strip RECOVERY_KIND from the skinny token; mark the 2 bits reserved-for-V1; INDEX ledger row. |
| §4.2 cache table | Per-kind cache decisions for 6 kinds | Honest; load-bearing | **KEEP** |
| §3.4 ScanReport | 3-field struct | Sufficient | **KEEP** |
| §1.4 `DocumentView` trait | 3-method trait (`root_value`, `tape_id`, `source`) | Each method load-bearing | **KEEP** |
| §1.3 `AnyKind`, `JsonRoot`, `JsonValue`, `JsonObject`, `JsonArray`, `JsonString`, `JsonNumber`, `JsonBool`, `JsonNull`, `JsonMember` | 10 uninhabited marker enums | The marker tags inform method dispatch on `ValueRef<_, _, K>`. Sufficient for typed view. | **KEEP** — see Lane 1 / Lens K |

Lens I verdict: **partial — 5 honoured, 1 REINVENT (RECOVERY_KIND), 1 SIMPLIFY (visitor trait)**. The visitor stub is the largest apparatus opportunity. Defeating the steelman: the spec calls visitor "the bench harness's 'read-twice' path"; a single `for_each_value` method serves that path; the full `visit_*` / `walk_*` ceremony is V1 PASS-3 §3 territory. Simplify-now-graduate-additively.

## §16 Lens J — Host-language leverage

| Site | Item | Verdict |
|---|---|---|
| §1.3 `PhantomData<fn() -> K>` | Auto-trait control via fn-ptr trick | **KEEP** — Rust idiom; correct |
| §1.2 `Box<[T]>` | Sealed slice | **KEEP** — Rust idiom; std-library leverage |
| §2 `Vec<u8>` payload arena | Standard bump region; not bumpalo | **KEEP** — leverages Rust std; deferred to bumpalo at `parse_in` |
| §4.1 `Cow<'input, str>` | Lazy unescape model | **KEEP** — Lock 9 alignment; lightning-css model |
| §4.1:373 `unsafe { str::from_utf8_unchecked }` | Bypass UTF-8 check | **REINVENT** — see Lens F; the SAFETY discharge is incorrect |

Lens J verdict: **honoured-narrow**. One unsoundness (the `from_utf8_unchecked` SAFETY claim) needs the prevalidation discharge nailed down or the call must use checked `from_utf8`.

## §17 Lens K — Meta-grammar discipline

| Site | Item | Classification |
|---|---|---|
| §1.1 `TapeToken` 16-byte layout | V1 substrate token | **LOAD-BEARING** — V1 mandatory |
| §1.4 `DocumentView` trait | User-surface contract | **LOAD-BEARING** — V1 mandatory |
| §3 SIMD scan integration | Substrate-level glue | **LOAD-BEARING** — Lock 8 + ARCH §11:1519 |
| §6 visitor | Read-only walker | **ASPIRATIONAL** — V1 surface; tranche-deferrable for body (PASS-3 §3 owns the full visitor) |
| §5 snapshot identity | Identity invariant | **LOAD-BEARING** — V1 mandatory (debug, DAP, path, parity oracle all consume) |
| §8 hand-coded parity contract | BENCH-side surface | **ASPIRATIONAL** — skinny BENCH-only; V1 J.W1 owns the long-term parity gate |

Lens K verdict: **honoured**. Substrate is appropriately scoped for V1 meta-grammar correctness; aspirational items (visitor, hand-coded parity) are tranche-receivers for body work but the surface is V1.

## §18 Lens L — Premise fidelity

The load-bearing skinny lens for SUBSTRATE. Each substrate cut classified.

| Cut | Classification | Bench-recoverable signal | Verdict |
|---|---|---|---|
| `payload_or_skip` union (vs split 24-byte token) | **JSON-FAITHFUL** | A 16-byte vs 24-byte token measurement diff would surface in cache-line throughput; if BENCH outcome G fires (substrate gap), the §10 open-question table names this as a perturbation lever | **FAITHFUL** — defeats steelman that the union encoding masks scan cost; bench measures end-to-end |
| Recovery omission | **JSON-FAITHFUL** | The BENCH §6 matrix gates valid input; recovery is cold path; no measurement signal lost | **FAITHFUL** — well-defended at §7 |
| `Box<[Diagnostic]>` field omission | **JSON-FAITHFUL** | Diagnostics are zero-length on valid input; no throughput cost | **FAITHFUL** |
| `OwnedDocument` cold wrapper | **JSON-FAITHFUL** | Owned form is wrapper; no parse cost | **FAITHFUL** |
| Closure environment frames | **JSON-FAITHFUL with V1-grammar caveat** | JSON has no closures (Lock 4 four-site closure list does not fire on JSON hot path); for CSS L4 host-chain or BBNF-self predicate closures, this becomes load-bearing | **FAITHFUL with V1-grammar caveat** — caveat is named at §7 ("Lock 4 amendment names four sites — host-chain, map, predicate, recovery — none of which fire on JSON's hot path") |
| `JsonObject::get` PHF cache omission | **JSON-FAITHFUL** | Object-key lookup is post-parse; not on parse hot path | **FAITHFUL** |
| Eager number parse omission | **JSON-FAITHFUL** | Sonic-rs and simdjson both lazy-parse; matches SOTA convention | **FAITHFUL** |
| Multi-grammar tape kind sharing | **JSON-FAITHFUL** | One grammar in skinny; kind table hardcoded is correct scope | **FAITHFUL** |
| Path-schema stash | **JSON-FAITHFUL** | Path eval is post-parse; not on parse hot path | **FAITHFUL** |
| `VisitMut` mutation | **JSON-FAITHFUL** | Read-only suffices for SOTA throughput | **FAITHFUL** |
| `@layout` / layout policy | **JSON-FAITHFUL with V1-grammar caveat** | JSON has no layout-significant whitespace; CSS / Sheets do | **FAITHFUL with V1-grammar caveat** — caveat named |
| DAP trace events | **JSON-FAITHFUL** | Pure debug instrumentation; off in release | **FAITHFUL** |
| Visitor §6 surface (full trait + walkers) | **possibly OVER-SHIPPED**, not MASKING | The §6 visitor lands more apparatus than BENCH needs; the cut here is "we ship the visitor surface even though the bench only needs read-traversal" — Lens I SIMPLIFY territory, not Lens L MASKING. The skinny does NOT under-ship the test; it over-ships the apparatus | **FAITHFUL** (but apparatus contrivance flagged at Lens I) |
| `ReparsePlan` / incremental reuse | **JSON-FAITHFUL** | Cold-parse only; no edit-reuse signal | **FAITHFUL** |

Lens L verdict: **mostly FAITHFUL with two V1-grammar caveats** (closures, layout). Both caveats are named in the spec. **No MASKING verdicts.** This is a strong Lens L showing: every substrate cut survives steelman, and the cuts that are conditionally orthogonal name the future grammars where they re-bind.

The borderline case — visitor §6 over-shipping — does NOT mask the bench (the bench can use the simpler walker; the apparatus is dead code). Lens I SIMPLIFY suffices.

## §19 Lens M — Falsifiability

**N/A for SUBSTRATE.** SUBSTRATE.md does not own thresholds; falsifiability is a BENCH lens. SUBSTRATE provides the mechanism BENCH measures; the threshold gates live at BENCH.md §6 / §4.3.

The one falsifiability-adjacent SUBSTRATE claim: §3.5 "AVX2 ≥ 7 GB/s; NEON ≥ 5 GB/s." These are SOTA floors, not skinny gates; they constrain substrate viability and are tested by BENCH §4 microbench. Lens M passes by reference to BENCH.

The §2 claim "zero arena allocations and zero arena writes on the JSON hot path" IS falsifiable — by the parity oracle's `assert_eq!(t1.payload_arena_writes(), 0)` (BENCH §3.4). But the API name mismatch (Lane 3) means the assertion as written cannot fire. **Falsifiability of the zero-arena-writes claim is currently broken by the API mismatch.**

## §20 Lens N — Graduation mechanicality

The load-bearing skinny lens for the deviations.

| Deviation | Site | V1 closure path | Mechanical-closure cost (LOC) | Verdict |
|---|---|---|---|---|
| Box<[TapeToken]> sealing precludes incremental append | §1.2:99 + INDEX deviation row | V1 adds `TapeBuilder<'input>` upstream with `Vec<TapeToken>` internal; the read-side `Tape<'input>` and `ValueRef` shapes do not change | The TapeBuilder is ~200-300 LOC additive; no rewrite of `Tape` or `ValueRef`. The skinny's `Tape<'input>` becomes the *committed-snapshot projection* of the V1 builder. | **MECHANICAL with named inversion** — the inversion is "skinny seals at parse boundary; V1 seals at snapshot boundary." Reversibility verified: read-side type-shape (`&Tape<'input>`, `ValueRef<_, _, K>`) does not change; only the producer-of-`Tape` changes. |
| HM hierarchy inversion (skinny puts HM as top-level; V1 puts HM under `passes::layout`) | INDEX deviation row + COMPILER §9.1 | V1 adds `@layout` lowering inside `passes::layout`, restoring direction. HM module relocates from sibling to subroutine via wrapper, not rewrite. | The HM module's source path (`crates/passes/src/layout/types/`) is already in the destination location; only the *call hierarchy* changes. Wrapper relocation is a function-rename + import-path adjustment. ~20-30 LOC. | **MECHANICAL with named inversion** |
| `payload_or_skip` union vs PASS-3's split fields | §1.1 redress vs PASS-3 §4 illustration | PASS-3 §4:187 admits its own layout illustrative; if V1 splits the field for some reason, the skinny's union encoding migrates per-token-class without touching consumers (consumers go through `flags.PAYLOAD_CLASS` discriminator). | If V1 grows the token to 24 bytes by un-collapsing, every token-walk is a 50% cache-line increase. Consumer code unchanged. **Migration is purely substrate-internal.** | **MECHANICAL** — defeats the steelman that "splitting at V1 requires every codegen consumer to rewrite"; consumers go through the discriminator, which is invariant |
| `parse-that-regex` directory promotion | INDEX row | V1 inherits the same shape (one-time promotion) | 0 LOC at graduation; promotion is a directory rename | **MECHANICAL trivial** |
| `wasm = false` metadata flag | INDEX row | V2 flips the flag | 0 LOC at graduation | **MECHANICAL trivial** |
| `passes` HM-only constraint | INDEX row + COMPILER §4.2 + WORKSPACE §2.1 | V1 adds DK13/GADT/CSP siblings under `passes::layout/types/` without touching `algorithm_w.rs` | Additive; no rewrite | **MECHANICAL** |
| `@host fn` decode-string surface (skinny removes) | INDEX row + COMPILER §1.3 | V1 adds `@host fn` surface; decode moves back. Saves the `host` + `csp-solver` crates in skinny. | The skinny's SUBSTRATE-side `decode_string` (referenced in COMPILER.md §1.3:95 — but NOT defined in SUBSTRATE.md) needs to migrate to a host-fn registry call at V1. | **possibly MECHANICAL but underspecified** — SUBSTRATE.md does not name the `decode_string` API surface (it's in `JsonString::as_str()` per §4.1:367, which calls `unescape_json`). At V1, that becomes a `CallHost` dispatch. Migration cost depends on whether `JsonString::as_str()` is in the per-grammar generated code (yes, per §9: `runtime/src/grammars/json/view.rs`) or in the substrate. Per Lock 14, must be per-grammar. **If per-grammar, MECHANICAL; if substrate-bleed, ANTI-MECHANICAL.** |

Lens N verdict: **MECHANICAL across the board, with one underspecification on `@host fn` decode-string graduation path** (the SUBSTRATE-side hook for `decode_string` is named in COMPILER.md but not defined in SUBSTRATE.md; the surface should be made explicit at SUBSTRATE.md §4.1 to verify Lock 14 compliance).

The two named-inversion deviations (Box<[T]> sealing; HM hierarchy) survive steelman: each preserves the read-side public type-shape; each isolates the inversion to a single producer; each is reversible by relocating one function or wrapping one module.

## §21 Cross-cutting fault summary (Lenses across lanes)

| # | Fault | Source lane(s) | Severity |
|---|---|---|---|
| 1 | Cross-quadrant `payload_arena_writes()` API mismatch | Lane 3, Lens M | high — falsifiability gate for zero-arena claim is broken |
| 2 | `JsonRoot` identifier overload (kind marker vs typed view) | Lane 3, Lane 7, Lens F | high — compile error if not renamed |
| 3 | `parse(&[u8])` vs `parse(&str)` signature trifurcation | Lane 1, Lane 3 | high — three sources disagree |
| 4 | `JsonDocument` vs `JsonRoot` return type mismatch | Lane 3 | high — cross-quadrant return type undecided |
| 5 | `INLINE_STRING_BORROW` payload class undefined in §1.1 enum | Lane 3 | medium — discriminator vocabulary incomplete |
| 6 | `unsafe { str::from_utf8_unchecked }` SAFETY claim depends on undischarged invariant | Lens F, Lens J | high — soundness gap |
| 7 | `STRING_BORROWS_SOURCE`, `STRING_NEEDS_UNESCAPE` flag names overfitted to JSON | Lane 5, Lens G | low — cosmetic but Lock 14-relevant |
| 8 | `RECOVERY_KIND` token bits dead in skinny | Lens I | low — strip and reserve |
| 9 | Visitor §6 surface over-shipped relative to bench need | Lens I | medium — SIMPLIFY opportunity |
| 10 | `decode_string` graduation path underspecified | Lens N | medium — Lock 14 ambiguity |
| 11 | Three carries lacking named receivers | Lane 8 | medium — `OwnedDocument` home, `Option<Box<PayloadArena>>` trigger gate, `parse-that` number kernel V1 receiver |
| 12 | LOC budget silent for SUBSTRATE-internal sections | Lane 6 | low — defers to WORKSPACE.md but should echo |
| 13 | `JsonObjectOpen → 0u16` illustrative-but-prose-confused | Lane 1 | low — one-line clarification |
| 14 | `ARCH §3.3` citation possibly displaced | Lens H | low — verify-and-fix |
| 15 | `build_tape_for_json` home crate undeclared | Lane 3, Lane 5 | medium — Lock 14 ambiguity |

## §22 Punch list (ordered surgical edits)

| # | Target | Edit | Source verdict | Owner | Scope | Lane(s) |
|---|---|---|---|---|---|---|
| 1 | SUBSTRATE.md §2 (PayloadArena impl block) | Add `pub fn payload_arena_writes(&self) -> u64` and `pub fn payload_arena_allocations(&self) -> u64` methods on `Tape<'input>` (delegating to `self.payloads.write_count() as u64`) so BENCH §3.4 parity oracle can call them. | REINVENT | SUBSTRATE author | API surface | Lane 3, Lens M |
| 2 | SUBSTRATE.md §1.3 (line 127) | Rename `pub enum JsonRoot {}` to `pub enum JsonRootKind {}` (or move under `kind::JsonRoot`). Update §1.4 reference at line 156 from `ValueRef<'input, 'input, JsonRoot>` to `ValueRef<'input, 'input, JsonRootKind>`. | REINVENT | SUBSTRATE author | Identifier | Lane 3, Lane 7 |
| 3 | SUBSTRATE.md §1.3 (line 117) AND §4.3 (line 410) AND COMPILER.md §6.3 (line 530) | Pick ONE parse-API signature. Recommended: `parse<'a>(&self, input: &'a [u8]) -> Result<Self::View<'a>, ParseError>` per ARCH §3.1; if skinny deviates to `&str`, add an INDEX deviation ledger row naming the divergence + V1 closure (`&[u8]` re-shape at graduation). | REINVENT | SUBSTRATE + COMPILER + INDEX authors | API surface | Lane 1, Lane 3 |
| 4 | SUBSTRATE.md §1.4 (line 151) vs COMPILER.md §6.3 (line 530) | Pick ONE return type for `Json::parse`. Either (a) `Json::parse -> Result<JsonDocument<'i>, _>` and user calls `.root_value()` to get `JsonRoot<'i>`, or (b) `Json::parse -> Result<JsonRoot<'i>, _>` directly and drop the `JsonDocument` wrapper. | REINVENT | SUBSTRATE + COMPILER authors | API surface | Lane 3 |
| 5 | SUBSTRATE.md §1.1 (lines 46-54) | Extend payload-class enum sketch to include `INLINE_STRING_BORROW` (referenced at §2 lines 178-179); list reserved values; name the bit-position layout for `TokenFlags`. | REINVENT | SUBSTRATE author | Spec rigor | Lane 3, Lane 5 |
| 6 | SUBSTRATE.md §4.1 (line 373) | Either (a) replace `unsafe { std::str::from_utf8_unchecked(raw) }` with checked `std::str::from_utf8(raw).expect("UTF-8 prevalidated")`, or (b) cite the UTF-8 prevalidation pass that discharges the invariant (§1.3:119 mentions it at the timing-region boundary; verify the prevalidation runs and document the SAFETY discharge inline). | REINVENT | SUBSTRATE author | Soundness | Lens F, Lens J |
| 7 | SUBSTRATE.md §1.1 (line 35-42) | Rename `STRING_BORROWS_SOURCE` → `PAYLOAD_BORROWS_SOURCE`; `STRING_NEEDS_UNESCAPE` → `PAYLOAD_NEEDS_NORMALIZE`; add a substrate-comment "the per-grammar discriminator interprets normalize as JSON unescape, CSS char-decode, etc." | REINVENT | SUBSTRATE author | Lock 14 | Lane 5, Lens G |
| 8 | SUBSTRATE.md §1.1 (line 38-41) | Strip `RECOVERY_KIND (2)` from the skinny token; mark the 2 bits "reserved (V1 recovery; INDEX deviation row)"; add INDEX row. | SIMPLIFY | SUBSTRATE + INDEX authors | Apparatus | Lens I |
| 9 | SUBSTRATE.md §6 (lines 437-481) | Collapse `JsonVisitor` trait body to one method `for_each_value<F: FnMut(JsonValueRef<'doc, 'input>)>(self, &mut F)`. Defer the multi-method visit/walk surface to V1 PASS-3 §3 (tranche I or G). Add INDEX deviation row "skinny visitor: single-method `for_each_value`; V1 expands to multi-method dispatch." | SIMPLIFY | SUBSTRATE + INDEX authors | Apparatus | Lens I |
| 10 | SUBSTRATE.md §4.1 (lines 363-374) | Name the `decode_string` API surface explicitly: where does `unescape_json` live in the skinny (per-grammar `runtime/src/grammars/json/view.rs`)? Confirm Lock 14 compliance. Add INDEX deviation row covering the V1 graduation path: "skinny: `JsonString::as_str` calls `unescape_json` directly; V1: `JsonString::as_str` calls `host::decode_json_string_to_arena` via `CallHost` registry." | REINVENT | SUBSTRATE + INDEX + COMPILER authors | Lock 14 + Lens N | Lens N |
| 11 | SUBSTRATE.md §1.4 (line 165) | Name the V1 receiver crate for `OwnedDocument` (recommended: `runtime/src/owned/`); state the V1 graduation gate. | REINVENT | SUBSTRATE author | Carry | Lane 8 |
| 12 | SUBSTRATE.md §2 (line 217) | Name the BENCH outcome that triggers the `Option<Box<PayloadArena>>` change. State whether this is SK-V2 amendment territory or H tranche. | REINVENT | SUBSTRATE + BENCH authors | Carry | Lane 8 |
| 13 | SUBSTRATE.md §4.1 (line 386) | Replace "parse-that's number kernel here post-skinny" with a named V1 receiver: "Skinny uses `std::str::FromStr`; V1 routes through `parse-that-regex::number_kernel` per WORKSPACE.md §1." | REINVENT | SUBSTRATE author | Carry | Lane 8 |
| 14 | SUBSTRATE.md §9 module layout | Echo WORKSPACE.md §2's `runtime/` 4,000-LOC budget split per `tape/`, `visitor/`, `grammars/json/skeleton`. | silent-must-add | SUBSTRATE + WORKSPACE authors | LOC budget | Lane 6 |
| 15 | SUBSTRATE.md §1.1 (line 68) | Add inline clarification at the `JsonObjectOpen → 0u16` line: "Illustrative — the substrate exposes only `NodeKindId`; the symbol-to-id mapping is per-grammar codegen output, never seen by `runtime/src/tape/`." | REINVENT | SUBSTRATE author | Lock 14 clarity | Lane 1 |
| 16 | SUBSTRATE.md §1.2 (line 99) | Verify the `ARCH §3.3` citation; if `ReparsePlan` lives at PASS-3 §5 (lines 208-220), update the cite. | REINVENT | SUBSTRATE author | Provenance | Lens H |
| 17 | SUBSTRATE.md §8 | Resolve `build_tape_for_json` home: either move to `crates/bbnf-bench/src/track2/json.rs` (BENCH-side, removes Lock 14 conflict) or tag `#[cfg(feature = "bench")]` in `runtime/src/grammars/json/`. State the choice. | REINVENT | SUBSTRATE + BENCH authors | Lock 14 | Lane 3, Lane 5 |
| 18 | SUBSTRATE.md §3.3 | Move JSON-specific Prefilter verifier description (the "looking for the closing `\"` past escapes" prose) into per-grammar territory (COMPILER.md §3 BIR row for RegexProgram, or §6 lowering); SUBSTRATE.md retains only the abstract Prefilter/Exact mode statement. | REINVENT | SUBSTRATE + COMPILER authors | Lock 14, Lens G | Lane 5 |

## §23 Steelman summary (KEEP/FAITHFUL/MECHANICAL verdicts that defeated their challenge)

| Verdict | Site | Steelman | Defeat |
|---|---|---|---|
| KEEP — Lock 1 | §1.2 Tape definition | "PASS-3 §4 admits illustrative; the skinny pins one without basis" | The skinny's pin is justified at §1.1:60-66 (token-cache density argument) and at §1.2:80-82 (Box<[T]> codegen quality argument); both arguments cite mechanism, not preference |
| FAITHFUL — recovery omission | §7 | "Recovery is on every parser hot path; cutting it masks V1 cost" | The bench measures valid-input throughput; recovery is dispatched only on parse error; valid-input bench cannot exercise the recovery codepath; cut is JSON-FAITHFUL by construction |
| FAITHFUL — closure environment | §7 | "Closures fire all the time in production grammars" | Lock 4 amendment names exactly four closure sites (host-chain, map, predicate, recovery); JSON's grammar uses none of them; cut is JSON-FAITHFUL with V1-grammar caveat (CSS / BBNF-self) |
| MECHANICAL — Box<[T]> sealing | §1.2 redress + INDEX | "Box<[T]> at parse boundary forecloses TapeBuilder; V1 must rewrite Tape" | The named inversion "skinny seals at parse; V1 seals at snapshot" preserves the read-side `&Tape<'input>` and `ValueRef<_, _, K>` types; only the producer changes; mechanical relocate, ~200-300 LOC additive |
| MECHANICAL — HM hierarchy inversion | INDEX + COMPILER §9.1 | "Inverting HM as `passes::layout` subroutine vs top-level requires Algorithm-W rewrite" | The HM module's source path already lands at `crates/passes/src/layout/types/`; only the call hierarchy inverts; wrapper relocate, ~20-30 LOC; algorithm-W code untouched |
| KEEP — `payload_or_skip` union | §1.1 | "Discriminated union on every token-walk costs a branch; PASS-3's split form is a wash" | The branch is on `flags.PAYLOAD_CLASS` which is read regardless (kind dispatch); the union saves one cache-line per pair; the split costs a cache-line per pair; union dominates on twitter/citm; canada is array-of-numbers where `INLINE_NUMBER_FAST` carries no payload anyway. The union is justified |

These six steelman defeats are the audit's load-bearing KEEP/FAITHFUL/MECHANICAL verdicts. They should not be revisited unless bench-time evidence contradicts.

## §24 Final readiness verdict

**Decision: SK-AMENDMENT-REQUIRED-NARROW.**

The post-redress SUBSTRATE.md lands the two new INDEX deviation ledger rows (Box<[TapeToken]> sealing; HM hierarchy inversion) cleanly. Lens N classifies both as MECHANICAL with named inversion under steelman; the read-side public type shapes (`&Tape<'input>`, `ValueRef<_, _, K>`) survive both graduations untouched. Lens L is dominated by FAITHFUL verdicts — every documented substrate cut survives steelman; the two V1-grammar caveats (closure environment frames; @layout) name their re-binding grammars (CSS L4, BBNF-self) explicitly.

The amendment scope is bounded: 18 punch-list items, of which 4 are high-severity (cross-quadrant API mismatches and one soundness gap), 6 are medium (Lock 14 / carry / Lens I SIMPLIFY), and 8 are low (cosmetic / clarification / LOC echo). None impeach the substrate spec's premise; all impeach its coherence. A single redress wave on SUBSTRATE.md, BENCH.md §3.4, COMPILER.md §6.3, and INDEX.md (3-4 new ledger rows) closes the punch list.

The most load-bearing fault is item 1 (the `payload_arena_writes()` API mismatch). The skinny's "zero arena allocations on the JSON hot path" claim is the substrate's primary FAITHFUL pillar (Lens L), and the parity oracle that verifies it (BENCH §3.4) cannot fire as written. Until item 1 lands, SUBSTRATE.md cannot claim FAITHFUL on its zero-arena commitment under measurement; it can claim only "FAITHFUL by intent." That is one redress commit.

Items 2 (`JsonRoot` overload), 3 (parse signature), 4 (JsonDocument vs JsonRoot return), and 6 (UTF-8 SAFETY discharge) are compile-time / soundness items: implementing the spec as written errors at the rustc boundary or invokes UB. These four block dispatch on principle, not on style.

Hereupon: dispatch the SK-V2 amendment narrow-scope agent against the §22 punch list. After SK-V2 returns the 18 items closed (or down-scoped with named justification), the substrate spec is SK-READY.

The substrate spec's premise survives this audit: a 16-byte token, structural SIMD scan, lazy scalar materialization, single-substrate read view, sealed Box<[T]> with named-inversion graduation. If the bench result lands in BENCH outcome A or B, this substrate is the V1 receiver. If the bench lands G, the §10 open-question table names the perturbation levers (token width, kernel hash, NodeKindId width) without re-architecture.

### Critical Files for Implementation

- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/SUBSTRATE.md
- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/BENCH.md
- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/COMPILER.md
- /Users/mkbabb/Programming/bbnf-lang/restart/skinny/INDEX.md
- /Users/mkbabb/Programming/bbnf-lang/restart/ARCHITECTURE.md
