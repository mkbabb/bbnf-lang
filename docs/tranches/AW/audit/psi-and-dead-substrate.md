# PSI Runtime Status + Substrate-Without-Consumer Catalog

## Angle headline

**Is PSI still in use on master? Yes — but only behind the walker fallback path.** Post-AW-V, JSON's `parse()` routes through shape dispatch and bypasses PSI entirely; CSS, Sheets, and BBNF still funnel every parse through `dta_run_*` → `psi.push` → `psi.fill_columns`. PSI is therefore alive on the non-JSON hot path and dead on the JSON hot path. Once AX.W0b deletes the walker, PSI loses every production consumer; its rayon integration is DTA-parallel-coupled, not independent. **Verdict: PSI deletes in AX.W0b alongside the walker.** Separately, seven `GrammarProfile` slots and the entire `bbnf-simd-scan/src/emit/*` fragment exporter directory are substrate-without-consumer.

## 1. PSI runtime-status walkthrough

**Module**: `crates/bbnf-tape/src/psi.rs` (805 lines). Exports `PayloadStream`, `PayloadJob`, `PayloadKind`. Only two runtime entry points: `push()` (line 378) and `fill_columns()` (line 448).

**Push sites** (grepped for `psi.push` + `PayloadJob` constructor):

- `crates/bbnf-tape/src/driver.rs:1474` — `psi.push(PayloadJob::new(rec_idx, lo, *pos, kind, arena_off))` inside `dispatch_one` (cold-path replay). Also at `3290` inside `dta_run_parallel`.
- `crates/core/src/backend/rust/emitter/dta_walker/lower_state.rs:691` — `emit_psi_push_inline` splice emitted into every Regex arm whose payload is NOT `F64`. The F64 path decodes inline (W2.3.a, `lower_state.rs:615`, `eisel_lemire_body`).
- `crates/core/src/backend/rust/emitter/dta_walker/helpers.rs` — `emit_psi_push_inline` helper.

**Zero push sites in the shape emitters**: `grep -rn "PayloadStream\|PayloadJob\|psi\.push\|psi::" crates/core/src/backend/rust/emitter/shapes/` returns `No matches found`. Every file in `shapes/{arglist,array,dispatcher,flat,hregex,keyword,mod,number,object,pratt,scalar,string,unordered,wrap}.rs` is PSI-free. Shape-routed parse (JSON today, CSS/Sheets/BBNF at AX.W0a) allocates a `TapeBuilder` and calls the per-shape fn directly — no `psi_with_capacity`, no `fill_columns`, no `PayloadJob` construction.

**Scheduling conditions.** `psi.push` fires at codegen-time only when the walker's Regex-arm lifter emits the inline splice. Per `lower_state.rs:599-698`, the payload kind is matched: `None` → no PSI; `Some(F64)` → inline Eisel-Lemire, no PSI; `Some(U8|I64|Bool|HexU32|String|AggregateLarge)` → splice `emit_psi_push_inline`. At runtime all splice sites fire unconditionally inside the walker's hot loop.

**Feature flag.** `#[cfg(feature = "rayon")]` at `psi.rs:119, 121, 510`. Gates `fill_parallel` (the rayon `par_chunks` path). The sequential `fill_sequential` path is unconditional. The rayon feature also gates `driver::dta_run_parallel` (`driver.rs:2949, 2959`) which is co-invoked with PSI.

**W2.1 prototype.** `crates/bbnf-json-prototype/src/string.rs:97` carries a doc-comment reference to `psi::write_decoded` from the P1 profile but never calls into PSI — the prototype uses its own packed `Value` representation.

**Emitted `parse()` routing (verified via `crates/target/.bbnf-cache/24b013820395026e.rs:8352` for JSON, `e510c0bb4263fa28.rs:541354` for CSS):**

| Grammar | `parse()` path | PSI reached? |
|---------|---------------|--------------|
| JSON | `parse_JsonParser_value(...)` (shape dispatch) | **No** |
| CSS L4 | `psi_with_capacity` → `dta_run_CssL4Parser` → `psi.fill_columns` | **Yes** |
| Sheets | `psi_with_capacity` → `dta_run_GoogleSheetsParser` → `psi.fill_columns` | **Yes** |
| BBNF | `psi_with_capacity` → `dta_run_BbnfBootstrap` → `psi.fill_columns` (`generated.rs:93497-93541`) | **Yes** |

**Sheets 6 MB/s evidence.** Post-AW-V Sheets throughput is 6–7 MB/s; per `aw4-profile-p6-begotten-code-audit.md` §3 the walker's cross-crate BL calls into `driver::{try_branch, advance_or_pop_with}` and `psi::write_decoded` are hot. PSI `write_decoded` showed up in the nm symbol-presence matrix (`aw4-profile-p6-begotten-code-audit.md` §1, row 10) for all four grammars — so PSI overhead is part of the CSS/Sheets/BBNF 0.03–0.07× regression, not the cause but a contributor alongside the 1.9 MB CSS walker text.

## 2. PSI post-W0b disposition

Per AX.md:74 (W0b): `nm + grep zero DTA symbols; cargo test --workspace green; bootstrap idempotent; generated.rs shrinks ~57K lines`. The W0b deletion ledger spans `dta_walker/` (4,360 LOC), `emitter/dta.rs` (935), driver carve (~2,873 including `dta_run_cold`, `dta_run_with_replay`, `dta_run_parallel`, `dispatch_one`, `FrameStack`, `Frame`, `DtaSnapshot`).

**Consumers of `psi.rs` after W0b:**

- `dispatch_one` (driver.rs:1474 push site) — **deleted**.
- `dta_run_parallel` (driver.rs:3290 push site) — **deleted**.
- `dta_walker/lower_state.rs::emit_psi_push_inline` splice — **deleted** (entire directory).
- `generated.rs:93497, 93541` — `psi_with_capacity(input.len())` / `psi.fill_columns(...)` — **deleted** by bootstrap regen once the walker path evaporates.
- Shape-dispatch `parse()` bodies — **never used PSI**.

**Surviving consumers**: none in production; only the PSI tests at `crates/bbnf-tape/tests/tape_basic.rs` (lines 1393, 1439, 1446, 1498, 1523, 1542, 1558) exercise `fill_columns` directly.

**Rayon integration.** The rayon feature on `psi.rs` is load-bearing ONLY for `fill_parallel` (PSI's own rayon path) and is compiled in tandem with `dta_run_parallel`. No non-walker consumer imports rayon via PSI. Post-W0b, the rayon-feature `#[cfg]` markers in `psi.rs:119, 121, 510` and `lib.rs:120` have no production-path consumer.

**Verdict: PSI is dead code post-W0b.** It deletes alongside the walker. The AX.W9 document-parallel fork would build its own fork-join on top of `TapeBuilder` (per-worker builders merged), not PSI — PSI is specifically the "scalar payload post-pass" abstraction that the inline-decoder discipline of W2.3 already rendered unnecessary for F64, and the shape emitter renders unnecessary for all remaining kinds.

## 3. Substrate-without-consumer full catalog

Verified via `grep` against master HEAD `0f69e08d`. "Emitted" = literal in `generated.rs`; "Consumer" = runtime code that reads the literal.

| Item | Emit site | Intended consumer | Actual consumer | Verdict |
|------|-----------|-------------------|-----------------|---------|
| `GrammarProfile.active_columns` | `profile.rs:208`, emitter `emit_active_columns` | V2 columnar substrate | **None** (V2 never shipped; `generated.rs:264` emits `&[]`) | **DELETE** slot + helper |
| `GrammarProfile.branch_priors` | `profile.rs:232` | V4 speculative Alt dispatch | **None** (`generated.rs:268` = `&[]`) | **DELETE** |
| `GrammarProfile.reorder_unroll_visitors` | `profile.rs:243` | V2 4-lane unroll visitors | **None** (`generated.rs:270` = `&[]`) | **DELETE** |
| `GrammarProfile.shape_dict` | `profile.rs:226` | Walker ShapeRef expansion | **None at `GRAMMAR_PROFILE.shape_dict`**; walker reads a separate `SHAPE_DICT` const (`generated.rs:12495` etc.) | **INVESTIGATE**: two parallel emission sites for the same data; collapse to one |
| `GrammarProfile.keyword_tables` | `profile.rs:220` | V7 PHF + SIMD keyword dispatch | **None** (emitted at `generated.rs:266` but no reader; keyword tables are inlined per-shape in `shapes/keyword.rs`) | **INVESTIGATE** vs. DELETE |
| `GrammarProfile.dedup_eligible_rules` | `profile.rs:237` | V8 runtime bloom+GADT dedup | **None** (V8 never shipped; emitted at `generated.rs:269` but no reader) | **DELETE** |
| `GrammarProfile.payload_bytes_per_input_byte` | `profile.rs:158` | V4 per-scanner actuals | **None** (read only by tests) | **DELETE** |
| `GrammarProfile.expected_ns_per_byte` | `profile.rs:166` | V6 parallel-parse cost model | **None** (parallel gate reads only `parallel_break_even_bytes`) | **DELETE** |
| `GrammarProfile.push_*_count` fields | `profile.rs:135-144` | AW-IV wire-contract tests | **Tests only** (`grammar_profile_wire_contract.rs`) | **INVESTIGATE**: retain as test contract if tests survive, else delete |
| `bbnf-simd-scan/src/emit/{clmul_parity, eisel_lemire_body, first_quote_or_backslash, multi_cmp_scan, nibble_lut_scan, nospace64_scan, quoted_string_simd_body, shift_xor_parity, tzcnt_compact}` | Fragment exporters (1,344 LOC across 9 files) | Per-shape emitter splice (per module doc `emit/mod.rs:3-5`) | **None** — `grep -rn "bbnf_simd_scan::emit\|simd_scan::emit" crates/` returns ONE hit: the crate's own `tests/emit_fragments.rs`. Zero shape emitters import them. | **DELETE** all 9 fragment modules |
| `KEYWORD_PHF` | Referenced in docs; not emitted | Scanner dispatch | **None** — `grep KEYWORD_PHF generated.rs` → no matches | Already absent — no action |
| `CLASSIFY_TABLE_*` | `classify_byte.rs` emitter | Walker state dispatch | **Live** (`generated.rs:8852` etc. consumed at `generated.rs:9111` inside walker arms) | **RETAIN** — deletes with walker at W0b |
| `BLOOM_SIGNATURES` | Not emitted | V8 dedup | **None** — zero hits in workspace | No action — unemitted |
| `LIST_RULES` | `__GRAMMAR_PROFILE_LIST_RULES` at `generated.rs:49` | Parallel-path gate | **Live** (`generated.rs:93504-93512` + `driver.rs:2847`) — populated for BBNF (1 entry) | **RETAIN** (but deletes with walker at W0b; no shape-path consumer) |
| `PRECEDENCE_LUT` / `lookup_precedence` | Walker Pratt/ShuntingYard arms | Pratt operator dispatch | **Live** on Sheets + BBNF per P6 audit §7 (0 nonzero for JSON, 4 CSS, 8 Sheets, 5 BBNF). `lookup_precedence` fn used by `bbnf-tape-codegen/src/advance.rs:147` and `driver.rs:2666` | **RETAIN** — deletes with walker |
| `DEDUP_ELIGIBLE_RULES` literal | `generated.rs:221` (`__GRAMMAR_PROFILE_DEDUP_RULES`, 16 entries for BBNF) | V8 runtime bloom dedup | **None** — populated but unread | **DELETE** |

## 4. Deletion ordering recommendation

**Delete in AX.W0b alongside the walker** (single unit of work — these are walker-coupled or dead):
- `crates/bbnf-tape/src/psi.rs` (805 LOC) — entire module.
- `PayloadStream`, `PayloadJob`, `PayloadKind` re-exports in `bbnf-tape/src/lib.rs:140`.
- `psi_with_capacity`, `fill_payloads` fns at `emitter/grammar.rs` + `generated.rs:7327-7344`.
- PSI tests in `bbnf-tape/tests/tape_basic.rs` lines 1393-1558.
- `GrammarProfile.shape_dict`, `keyword_tables`, `dedup_eligible_rules`, `branch_priors`, `active_columns`, `reorder_unroll_visitors`, `payload_bytes_per_input_byte`, `expected_ns_per_byte` slots + their emitter helpers in `profile.rs` — **all populated as `&[]` or populated-but-unread; all deletable with zero consumer impact**.
- `bbnf-simd-scan/src/emit/*` entire directory (9 fragment modules, 1,344 LOC) — zero non-test consumers.

**Investigate at AX.W3+ wave opening** (may acquire consumer):
- `GrammarProfile.push_*_count` fields — retain if wire-contract test regime survives W0b; delete otherwise.
- `SHAPE_DICT` vs. `GRAMMAR_PROFILE.shape_dict` — only one emission path; delete the unread slot.
- `KEYWORD_PHF` population — shape emitters may acquire this as a W4 lever (cf. `shapes/keyword.rs`); decide at W4 open.

## 5. Wire-contract fixture pattern

The recurring accumulation mechanism: **every AV/AW wave landed an emitter pass + a `GrammarProfile` slot + an IR mining hook without a runtime consumer.** V1 shipped `push_*_count`; V2 shipped `active_columns` + `reorder_unroll_visitors`; V4 shipped `branch_priors` + PSI; V5 shipped `shape_dict`; V6 shipped `list_rules`; V7 shipped `keyword_tables`; V8 shipped `dedup_eligible_rules`. Each "V{N}" wave closed on the emitter side without the runtime hook.

**One-commit wire-contract fixture class** (`crates/core/tests/profile_consumer_wire_contract.rs`, new file):

For every slot in `GrammarProfile`, one `#[test]` asserts — for each of the four grammars — that when the IR mining pass populates the slot with known data, a runtime probe (an inserted counter or a `samply` symbol check via `nm`) fires. Test shape:

```rust
#[test] fn active_columns_reaches_runtime() {
    let prof = JsonParser::GRAMMAR_PROFILE;
    if !prof.active_columns.is_empty() {
        let counter = COLUMNAR_WALK_COUNTER.get();
        JsonParser::parse(FIXTURE).unwrap();
        assert!(COLUMNAR_WALK_COUNTER.get() > counter);
    }
}
```

The test body must fail to compile if the consumer path does not exist. One such test per slot, parameterised over the four grammars, catches substrate-without-consumer at wave open. The README.md §wave-verification-ledger already codifies this as the mandatory close condition; the fixture formalises it as CI-gated.
