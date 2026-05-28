# SK-V15 P2-B: DAV1D/FFmpeg ASM Process

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-28.
Scope: dav1d / FFmpeg / VLC hand-written ASM and SIMD admission process mapped to bbnf-simd checkasm discipline.
Output: this file.
P1 hot-leaf antecedents: SK-V15 P1-E normalized attribution classes `cursor/skip/ws`, `scan/string`, `unicode/string`, `simd structural scan`, and `tape/allocation`; generated schema wrappers, comparator frames, checksum paths, sidecar-symbolization drift, and harness materialization are blocked or diagnostic.
Lock surface: Lock 16 primarily; Lock 14 for grammar-policy ownership; Lock 1 for no retained sidecar or parallel substrate.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

1. The upstream process is scalar-oracle first. FFmpeg checkasm declares both a reference function and a candidate function, then routes `call_ref` through the reference and `call_new` through the checked candidate wrapper (FFmpeg `tests/checkasm/checkasm.h` at `5f14108864416ff0fbfe83fdb16eb6554d83347f`, lines 214-239). dav1d's loopfilter cell uses that shape directly: randomized input is copied into independent reference/candidate buffers, `call_ref` runs first, `call_new` runs second, and `checkasm_check_pixel` compares output before `bench_new` is allowed to time it (dav1d `tests/checkasm/loopfilter.c` at `62501cc7db378532d7e85ea434b70d57e1ba2cb0`, lines 93-102 and 141-188). bbnf-simd already mirrors the core shape: `checkasm_parity.rs` says every primitive is run on bit-identical scalar/candidate inputs, compares outputs and source buffers, sweeps misalignment, installs signal guards, and uses the same robust timing rule (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1-20`, `:200-220`).

2. CPU-feature routing is part of admission, not a later deployment detail. dav1d registers checkasm CPU feature rows for x86 and ARM, including NEON/DOTPROD/I8MM/SVE/SVE2 (`tests/checkasm/checkasm.c` lines 63-88), then initializes and masks CPU flags before entering `checkasm_main` (`:91-112`). FFmpeg exposes a broader feature matrix including AArch64 NEON, DOTPROD, I8MM, SVE, SVE2, SME, PMULL/EOR3 and x86 AVX families (`tests/checkasm/checkasm.c` lines 382-450). dav1d's Apple ARM detection queries DotProd and I8MM and explicitly notes no Apple SVE/SVE2 detection path (`src/arm/cpu.c` lines 75-95). SK-V15 must preserve the current native admission rule: SIMD claims anchor on Apple M5 Max / aarch64 only, while x86 and AVX-512 remain diagnostic (`restart/skinny/tranches/sk-v15/SYNTHESIS.md:36-44`).

3. VLC is a same-wave consumer exemplar, not an extra primitive source for bbnf. VLC's dav1d module is a production consumer of the proved library surface: it opens the dav1d decoder, configures frame threading, wraps VLC blocks as `Dav1dData`, calls `dav1d_send_data`, obtains pictures with `dav1d_get_picture`, and queues decoded video (`modules/codec/dav1d.c` at `792e135f18ff134695c5f3b002f2a07481752931`, lines 60-78, 83-88, 290-419, 424-443). The process lesson for bbnf-simd is that a SIMD primitive must be attached to a production caller with output/equality evidence in the same wave. A source-present kernel without a caller is inventory, not an admitted primitive.

4. bbnf-simd's local gate is already close to the upstream discipline and stricter in the relevant places. `CHECKASM-REPORT.md` maps FFmpeg checkasm features to Rust equivalents: identical cloned buffers, scalar-vs-candidate compare, source-buffer mutation check, alignment sweep, stack canary, signal guard, and robust mean (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:39-52`). It records strict-mode commands for `checkasm_escape_mask_64`, `checkasm_parity`, runtime JSON scanner parity, and corpus parity (`:102-125`). It also records the admitted primitive floor and same-wave consumers for `BYTE_CLASS_FROM_TABLE_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, and `EOB_PAD_CLAMP` (`:229-248`). The register-sentinel note is important: AArch64 callee-saved register sentinels are reserved for explicit FFI/ASM callee boundaries, not arbitrary Rust closures (`:241-248`, `skinny/REDRESS.md:1268-1274`).

5. SK-V15 P1 narrows the admissible antecedent surface. P1-E names the current primitive classes and makes the normalized attribution TSV the binding S-P2 antecedent surface (`restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:10-35`). P1-B exposes current product-plane misses on `mesh` direct and `unicode_escapes` direct/typed (`restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:16-37`). S-P1 hardening locked this input and explicitly authorizes S-P2 to consume the normalized attribution and PMU summaries (`restart/skinny/tranches/sk-v15/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md:1-19`). Therefore P2-B does not import old SK-V14 candidate enthusiasm unless a row maps to a current SK-V15 hot leaf.

## §2 — Candidate primitives (admission process and candidate table)

S-P3 may shortlist a SIMD/ASM primitive only if all five stages below pass. Failure at any stage rejects the candidate for this S-P2 cycle or demotes it to diagnostic inventory.

| Stage | Gate | Required evidence |
|---|---|---|
| A | Scalar oracle | Portable Rust scalar function or independent byte-walk oracle exists before SIMD/ASM body. It is the executable spec and compiles on non-target hosts. |
| B | SIMD/ASM path | Candidate body is behind explicit target feature / arch dispatch. Hand-written `asm!` is allowed only where Lock 16 allows missing-intrinsic cases. |
| C | Checkasm parity | Dedicated `checkasm_<primitive>` or existing checkasm cell compares scalar vs candidate, covers deterministic boundaries, xorshift/random cases, alignment where applicable, stack canary, signal guard, and strict command with `BBNF_SIMD_STRICT=1`. |
| D | Same-wave consumer | The same wave wires the primitive into a production caller or records a measured deletion/rejection. No orphan primitive body may move a row. |
| E | Manifest and locks | Lock 16 manifest row names primitive id, ISA/library citation, hardware gate, scalar reference, strict parity command, corpus/equality parity, grammar policy source, substrate target, retention lifetime, policy owner, rollback path, abrogate threshold, and final disposition. Lock 14 forbids grammar literals in generic bbnf-simd policy. Lock 1 forbids retained class/mask/cursor sidecars. |

Standing admission command shape:

```bash
BBNF_SIMD_STRICT=1 RUSTFLAGS="-C target-cpu=native" cargo test -p bbnf-simd --release --test checkasm_<primitive> -- --nocapture
cargo run -p xtask --release -- primitive-checkasm
```

Those commands are necessary, not sufficient. Stage D still must move or explicitly reject the named consumer row under the same strict comparator/equality plane. Lock 16 requires strict checkasm and rejects every scalar/checkasm/equality failure (`restart/locks/LOCKS.md:478-504`). At close, every source-present primitive must be `wired`, `deleted`, `scalar-delegate-non-ASM`, or `architectural-block-with-REDRESS`; orphan intrinsic/ASM files do not close Lock 16 (`restart/locks/LOCKS.md:506-518`).

### Candidate table

| Candidate / process unit | Scalar oracle | SIMD/ASM path | Checkasm parity | Same-wave consumer | P1 antecedent | REDRESS blocks |
|---|---|---|---|---|---|---|
| `BYTE_CLASS_FROM_TABLE_64` structural classifier floor | `scalar::byte_class_from_table_64` plus full `scan_scalar` parity; `scan_dispatch` builds a class table from caller alphabet (`skinny/crates/bbnf-simd/src/lib.rs:106-127`, `:251-257`). | Current dispatch-backed primitive; aarch64/x86 bodies only through `dispatch::primitive_kernels`. | `checkasm_byte_class_from_table_64`; included in primitive-checkasm floor (`CHECKASM-REPORT.md:234-243`). | Existing generic structural scanner consumer. | `p1c_simd_scan` rows for `citm_catalog`, `random`, and JSON scan wrapper rows in normalized P1-E; P1-E allows only after generic scanner boundary is cited. | Must not become retained structural sidecar or union substrate; REDRESS 96/97/98 retire that hypothesis (`skinny/REDRESS.md:2910-2950`). |
| `BYTE_CLASS_FROM_EQ_SET_64` set membership / first special byte | `byte_class_from_eq_set_64_scalar` returns a 64-bit membership mask and caps set length at 8 (`skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:20-38`). | AArch64 NEON fans `vceqq_u8` over four 16-byte stripes and packs movemasks (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:27-87`). | `checkasm_byte_class_from_eq_set_64` documents scalar-as-spec, alignment sweep, signal guard, and stack canary (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1-17`, `:156-216`). | Existing `find_ascii_set_member64`/scanner consumer only; any new string consumer must land same wave. | `scan/string`, `unicode/string`, and `simd structural scan` rows; P1-B highlights `unicode_escapes` product-plane misses. | Do not replay REDRESS 82 single-quartet helper or REDRESS 83 StringBlock16 retained tiny probe without new same-row evidence (`skinny/REDRESS.md:2285-2317`, `:2318-2356`). |
| `BITMAP_PREFIX_XOR_64` | Scalar bit-parallel prefix carry oracle (`scalar::bitmap_prefix_xor_64`). | Production path currently scalar/delegate. PMULL hot body is not admitted by default. | `checkasm_bitmap_prefix_xor_64` fixed cases plus 4096 random masks and carry true/false (`skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs:5-37`). | Existing JSON string-region scan via `prefix_xor_64` (`skinny/crates/bbnf-simd/src/lib.rs:169-206`). | `unicode/string` and string-region scanner rows only when a concrete caller is named. | PMULL as default hot body is rejected by REDRESS 88; any PMULL retry needs a narrow consumer and same-row non-regression (`skinny/REDRESS.md:2510-2540`). |
| `BITMAP_NEXT_SET_BIT` + `BULK_EMIT_POSITIONS_64` | Scalar `ctz`/position-emission oracle through `scalar::bitmap_next_set_bit` and `scalar::bulk_emit_positions_64`. | Current aarch64 paths are scalar delegates or narrow local bodies; CSSC/CTZ production body is not admitted on M5 unless measured. | `checkasm_bitmap_next_set_bit` and `checkasm_bulk_emit_positions_64`; primitive-checkasm floor. | Existing `compact_mask` structural projection emit (`skinny/crates/bbnf-simd/src/lib.rs:228-243`). | `simd structural scan`, `allocation/tape` only when consumed by existing tape/projection code. | CSSC CTZ bulk consumer is rejected by REDRESS 89; do not reopen without material differential and row maintain proof (`skinny/REDRESS.md:2542-2585`). |
| `EOB_PAD_CLAMP` support inventory | Scalar zero-pad/clamp block under `scalar::eob_pad_clamp`. | Current path may delegate; hand ASM would require missing-intrinsic proof. | `checkasm_eob_pad_clamp` and primitive-checkasm floor. | Existing JSON scan tail handling only. | No named S-P1 hot leaf survives for EOB/tail-clamp work. | Existing support primitive, not an S-P2 implementation candidate; keep wired/delegated, but do not shortlist. |
| `escape_mask_64` correctness prerequisite | Independent byte-walk scalar reference in `checkasm_escape_mask_64`; runtime scanner scalar parity. | No admitted throughput SIMD/ASM body by itself. | Historical falsifier seed and scanner parity are covered; strict commands recorded in REDRESS 122 (`skinny/REDRESS.md:3603-3632`). | None for throughput. A future string/escape consumer must wire same wave and move or reject the row. | `unicode/string`, especially escape-heavy `unicode_escapes` / `y_string_unicode` evidence. | Correctness prerequisite only per Lock 16; cannot be credited as a production primitive without consumer (`restart/locks/LOCKS.md:497-504`). |
| UTF-8 block validation | `validate_block_scalar` is compared to NEON status (`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:1-17`). | AArch64 NEON `validate_block`; SVE/SVE2 is not an Apple M5 anchor. | `checkasm_utf8_block` covers ASCII, complete multibyte, boundary continuation, overlong/surrogate rejection, plus `unescape_uxxxx_x4` smoke (`:19-68`). | Only admissible with a string/Unicode caller that removes measured duplicate validation or moves a strict product row. | P1-B/P1-E `unicode/string`; direct `unicode_escapes` miss. | REDRESS 50-55 and 59 reject "UTF-8 fusion closes the row" as a thesis; a retry must name same-row c/B/Mbps gates (`skinny/REDRESS.md:1331-1342`). |
| `unescape_uxxxx_x4` / unicode escape SIMD | Scalar quartet decoder in `aarch64/unescape_uxxxx.rs` plus scalar join/reference behavior. | AArch64 NEON TBL nibble decode. | Covered partially by `checkasm_utf8_block`; a production candidate still needs a dedicated checkasm cell for its exact shape and caller. | Must be a same-wave generated string/unicode materializer consumer, not a one-quartet helper swapped into the current loop. | `unicode/string`; `unicode_escapes` direct and typed c/B misses. | REDRESS 82 rejects the one-quartet unicode classifier despite correctness/parity green; REDRESS 66-69 reject current direct escaped-string materializer families (`skinny/REDRESS.md:2285-2317`, `:1736-1886`). |
| Long-string special-byte scanner / string block | Scalar string-special-byte oracle must be exact for quote, escape, control policy passed by caller. | AArch64 string block / movemask body may be candidate only if lower overhead than current scalar leaf. | Needs dedicated checkasm over deterministic, random, and boundary quote/backslash/control cases. | Same-wave `parse-that-regex` or generated string caller and strict row movement. | P1-E `scan/string`; P1-B `unicode_escapes` product miss. | REDRESS 61/62 and 83 show correctness-green string scanners regressed or missed row gates; no retained parse string scan retry without fresh P1 row evidence (`skinny/REDRESS.md:1380-1488`, `:2318-2356`). |
| Direct parser cursor / whitespace skip primitive | Scalar oracle is current generated/hand direct parser behavior, not bbnf-simd. | Not a SIMD/ASM candidate until extracted as grammar-neutral byte/control primitive with policy parameters. | If extracted, gets its own checkasm/equality cell against scalar direct cursor behavior. | Same-wave direct strict/typed caller. | `cursor/skip/ws`: P1-B rows for `skip_value`, `ws`, and P1-E structural/dispatch classes (`restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:19-28`). | Must not become parser-owned cursor, function-pointer dispatch, or sidecar. REDRESS 60-72 and 96/97/98 are pre-blocks. |
| Tape/allocation materialization pressure | Scalar/equality oracle is existing tape/allocation behavior and product equality. | Reject as SIMD/ASM primitive. Handle in P2-D/P3 as substrate/cost-model work, not bbnf-simd body admission. | No checkasm; requires equality/corpus gates instead. | Existing tape or direct sink only. | P1-E `allocation/tape` rows across parse, typed, and mode-III masking. | Lock 1 forbids parallel substrate; REDRESS 96/97/98 retire union substrate (`skinny/REDRESS.md:2910-2950`). |
| Schema-shaped generated product builder / harness hash rows | No primitive oracle. These are generated product or benchmark artifacts. | Reject as SIMD/ASM primitive. | Not applicable. | Not eligible. | P1-E marks generated `parse_type_*`, checksum, comparator frames, sidecar drift as blocked/diagnostic (`restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:31-43`). | REDRESS/FNV quarantine and SK-V15 REBUILD-WAVE-G handle closed-enum bench concerns; do not treat these as primitive candidates. |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

The admission process is grammar-neutral; individual primitive claims are not grammar-neutral until their policy source and consumer prove it.

- `bbnf-simd` may accept byte sets, tables, masks, and scalar/candidate functions. It may not hardcode JSON policy such as quote, slash, control-byte, delimiter, or number grammar in a generic primitive. Policy must come from generated grammar config, caller data, or none.
- A JSON-only consumer may admit a JSON-scoped row, but cannot claim fleet-wide closure. Fleet-wide wording requires a non-JSON consumer or an explicit measured non-JSON deletion/rejection in the same wave.
- Structural offsets retained across calls are tape, not a second SIMD sidecar. Per Lock 1, classifier state must be transient or existing-output owned.
- x86/AVX-512 citations are architecture pressure only in SK-V15. They cannot close M5/aarch64 rows.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- REDRESS 28 and 33: active Class A tiny-string wiring regressed and remains blocked as a retained parse fix (`skinny/REDRESS.md:324-337`, `:394-405`). Direct-only scalar early-outs are a different surface, but cannot be credited as SIMD admission.
- REDRESS 50-55 and 59: UTF-8 fusion, retained side tables, byte-class cursor, parser-local structural-mask cursor, stats sinks, and quote-source streaming materializer do not close current rows (`skinny/REDRESS.md:1331-1342`).
- REDRESS 60-72: retained parser shortcut and direct materializer families require same-row proof and cannot be reopened as allocation reuse or parser-owned decode (`skinny/REDRESS.md:1344-1886`).
- REDRESS 80: canada mantissa-widen had no measured fallback pool (`skinny/REDRESS.md:2215-2248`).
- REDRESS 82-84: one-quartet unicode helper, retained StringBlock16 tiny probe, and object-pair value-byte control compaction were correctness/parity-green but failed row gates (`skinny/REDRESS.md:2285-2396`).
- REDRESS 88-89: PMULL prefix-XOR and CSSC CTZ/bulk consumer are correctness-green but production-row rejected on this host (`skinny/REDRESS.md:2510-2585`).
- REDRESS 96-98: retained class columns, streaming structural cursors, and class-lane-only retained substrate are retired, not merely deferred (`skinny/REDRESS.md:2910-2950`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

External primary sources:

- FFmpeg `tests/checkasm/checkasm.h` at `5f14108864416ff0fbfe83fdb16eb6554d83347f`: `https://github.com/FFmpeg/FFmpeg/blob/5f14108864416ff0fbfe83fdb16eb6554d83347f/tests/checkasm/checkasm.h#L214-L239` and `#L396-L430`.
- FFmpeg `tests/checkasm/checkasm.c` at `5f14108864416ff0fbfe83fdb16eb6554d83347f`: `https://github.com/FFmpeg/FFmpeg/blob/5f14108864416ff0fbfe83fdb16eb6554d83347f/tests/checkasm/checkasm.c#L382-L450`.
- dav1d `tests/checkasm/checkasm.c` at `62501cc7db378532d7e85ea434b70d57e1ba2cb0`: `https://github.com/videolan/dav1d/blob/62501cc7db378532d7e85ea434b70d57e1ba2cb0/tests/checkasm/checkasm.c#L37-L112`.
- dav1d `tests/checkasm/loopfilter.c` at `62501cc7db378532d7e85ea434b70d57e1ba2cb0`: `https://github.com/videolan/dav1d/blob/62501cc7db378532d7e85ea434b70d57e1ba2cb0/tests/checkasm/loopfilter.c#L93-L188`.
- dav1d `src/arm/cpu.c` at `62501cc7db378532d7e85ea434b70d57e1ba2cb0`: `https://github.com/videolan/dav1d/blob/62501cc7db378532d7e85ea434b70d57e1ba2cb0/src/arm/cpu.c#L75-L95`.
- VLC `modules/codec/dav1d.c` at `792e135f18ff134695c5f3b002f2a07481752931`: `https://code.videolan.org/videolan/vlc/-/blob/792e135f18ff134695c5f3b002f2a07481752931/modules/codec/dav1d.c#L60-L78`, `#L290-L419`, `#L424-L443`.

Local sources:

- `restart/prompts/skinny/PASS-2-RESEARCH.md`.
- `restart/skinny/tranches/sk-v15/HANDOFF.md`.
- `restart/skinny/tranches/sk-v15/SYNTHESIS.md:36-53`.
- `restart/locks/LOCKS.md:453-565`.
- `restart/skinny/tranches/sk-v15/research/p1/p1b-samply-mode-2.md:16-45`.
- `restart/skinny/tranches/sk-v15/research/p1/p1e-hot-leaf-attribution.md:10-43`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/p1e-normalized-attribution.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/evidence/pmu-cpb-summary.tsv`.
- `restart/skinny/tranches/sk-v15/research/p1/hardening/HARDENING-S-P1-V2-CONSOLIDATED.md:1-19`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md:1241-1278`, `:1331-1342`, `:1380-1488`, `:1736-1886`, `:2215-2248`, `:2285-2396`, `:2510-2585`, `:2910-2950`, `:3603-3632`.
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:39-125`, `:229-253`.
- `skinny/crates/bbnf-simd/src/lib.rs:106-157`, `:169-206`, `:208-243`, `:251-292`.
- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:20-38`.
- `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:27-87`.
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs:33-72`, `:83-192`.
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:1-20`, `:200-220`.
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:1-17`, `:156-216`.
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs:5-37`.
- `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:1-68`.
