# SK-V7 W2 Phase 1 Research: Eisel-Lemire Mantissa Feasibility

Date: 2026-05-16
Workspace: `/Users/mkbabb/Programming/bbnf-lang`
Scope: read-only review for Wave 2 B5b, focused on Eisel-Lemire mantissa widening and canada fallback elimination.

## Question

Can Wave 2 close `canada` direct by widening the Eisel-Lemire mantissa path, and is the prescribed "additional powers-of-10 table entries" route actually the missing mechanism?

## Findings

1. The Wave 2 contract is explicit but currently over-specific about the likely patch site. SK-V7 §4 asks W2 to confirm about 25% canada f64 fallback, investigate widening EL mantissa range, land wider mantissa table entries, then bench canada/numbers/mesh/marine_ik direct (`restart/skinny/tranches/sk-v7/SPEC.md:149-160`). Its gate is canada direct >=100% sonic-strict, numbers direct still >=100%, and no row regression (`restart/skinny/tranches/sk-v7/SPEC.md:162-165`).

2. The current results authority still makes canada direct a legitimate W2 target. W0b results show canada direct Track 1 10464 Mbps, Track 2 10119 Mbps, sonic strict 12509 Mbps, hence `N-direct / NO-GO` (`skinny/RESULTS.md:10-11`). The notes restate the same gate failure and record canada as 111126 numbers, 12 string quotes, and a 69075 Mbps structural scan floor, so the row is number-heavy rather than scan-floor blocked (`skinny/RESULTS.md:158-162`).

3. The vendored EL table is already full f64 exponent coverage. `SMALLEST_POWER_OF_FIVE=-342` and `LARGEST_POWER_OF_FIVE=308` define the accepted exponent range (`skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:21-24`), and the table declares 651 entries spanning that exact inclusive range (`skinny/crates/parse-that-regex/src/number/eisel_lemire/table.rs:1-8`). `compute_product_approx` indexes the table as `q - SMALLEST_POWER_OF_FIVE`, so adding table entries does not affect mantissa overflow for in-range f64 values unless the exponent constants and algorithm contract are also changed (`skinny/crates/parse-that-regex/src/number/eisel_lemire/algorithm.rs:76-84`).

4. Fallback is currently decided before EL when the scanner cannot retain the significant digits in the 64-bit mantissa. `NumberSpan` carries `mantissa: u64` plus `mantissa_overflow: bool` (`skinny/crates/parse-that-regex/src/number/mod.rs:4-14`). `materialize_f64` only calls `eisel_lemire::compute_f64` when `!span.mantissa_overflow`; otherwise it falls through to UTF-8 conversion and `text.parse::<f64>()` (`skinny/crates/parse-that-regex/src/number/mod.rs:260-271`). That fallback is the expensive route the W2 owner path names in bench direct code (`restart/skinny/tranches/sk-v7/SPEC.md:151-155`).

5. The scanner's mantissa budget is a hard 19 significant digits, not a power-table limitation. `push_digit` only appends through `digit_count <= 19`; after that, any non-zero digit marks `mantissa_overflow=true` (`skinny/crates/parse-that-regex/src/number/mod.rs:304-319`). The vectorized append helpers are likewise capped so 8/4/2-digit batches never exceed 19 digits (`skinny/crates/parse-that-regex/src/number/mod.rs:323-361`). Therefore, canada fallback elimination must change scanner accumulation semantics or add a second conversion path for overflowed spans; adding `POWER_OF_FIVE_128` rows alone is falsified by the current code shape.

6. The EL core itself accepts any `u64` mantissa and normalizes it with `leading_zeros`; it has no 19-digit policy of its own (`skinny/crates/parse-that-regex/src/number/eisel_lemire/algorithm.rs:14-23`). Its only `None` signal is the ambiguous-rounding path via `(0, -1)` when the approximation is unsafe (`skinny/crates/parse-that-regex/src/number/eisel_lemire/algorithm.rs:24-29`, `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:166-169`). This supports a narrower conclusion: EL can consume wider *u64-fitting* mantissas, but the scanner must first produce a valid rounded/truncated representation with enough discarded-digit facts.

7. The Clinger fast path is not the canada W2 answer by itself. It only accepts mantissa <= 2^53 and exponent in `[-22, 37]` (`skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:43-56`, `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:99-128`). The file comments estimate it intercepts about 85% of canada literals (`skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:93-97`), matching the handoff's statement that about 25% of canada f64 overflows the EL fast path (`restart/skinny/tranches/sk-v7/HANDOFF.md:35-39`). The residual is not an exponent-table miss.

8. Prior redress supports keeping the work inside the shared number materializer, not re-opening raw parse shortcuts. SK-V5 Wave 2 closed by vendoring EL into generated and hand SinkOnly, explicitly avoiding the rejected `raw.parse::<f64>()` shortcut while leaving canada/mesh/marine_ik residual in float materialization and structural/event consumption (`skinny/REDRESS.md:517-533`). The direct hand parser already routes `NumberSpan` through `materialize_f64` after integer attempts (`skinny/crates/bbnf-bench/src/direct_struct.rs:88-105`, `skinny/crates/bbnf-bench/src/direct_struct.rs:579-584`), so a shared scanner/materializer change should affect both Track 1 and Track 2.

## Feasibility

Mantissa widening is feasible only if scoped as a scanner/materializer change, not as a table append.

The viable shape is: keep collecting up to the full `u64` decimal budget where possible, preserve enough information about discarded non-zero digits and decimal exponent adjustment, and call a proven wide/overflow Eisel-Lemire path only when the shortened representation is known to round identically to `str::parse::<f64>()`. The current scanner stops at 19 digits even though some 20-digit values fit in `u64` and even though EL can normalize any `u64` input. That is the narrow admissible widening.

The non-viable shape is: add rows to `POWER_OF_FIVE_128`. The table already has 651 rows for `[-342, 308]`; canada geographic decimals should fall well inside that range, and `materialize_f64` never reaches the table when `mantissa_overflow` is true.

## Recommendations

1. Phase 1 instrumentation gate before implementation:
   - Add temporary attribution only in the W2 implementation branch, not this research artefact.
   - Count canada direct f64 materializations by: Clinger hit, EL hit, EL ambiguous `None`, `mantissa_overflow` fallback, exponent underflow/overflow.
   - Falsify if `mantissa_overflow` is not the dominant fallback class or if total fallback is materially below the handoff's about 25% claim (`restart/skinny/tranches/sk-v7/HANDOFF.md:35-39`).

2. Patch candidate A: extend scanner accumulation to all safe `u64` digits.
   - Replace the hard 19-digit cap with checked accumulation until `checked_mul(10).checked_add(digit)` fails.
   - Preserve the existing integer fast path constraints: i64/u64 materializers may still require `digit_count <= 19` where needed (`skinny/crates/parse-that-regex/src/number/mod.rs:226-257`).
   - Falsifiability gate: canada `mantissa_overflow` fallback count drops by at least 50%; bit-parity tests comparing `materialize_f64` to `raw.parse::<f64>()` pass on targeted 19-20 digit, trailing-zero, trailing-nonzero, subnormal, max-finite, and canada-sampled literals; numbers direct remains >=100% sonic and no measured direct row regresses (`restart/skinny/tranches/sk-v7/SPEC.md:162-165`).

3. Patch candidate B: if A is insufficient, add a guarded truncated-mantissa path.
   - Track discarded digit count and whether any discarded digit was non-zero.
   - Only bypass `str::parse` when the algorithm can prove the truncated representation and sticky bit produce the same IEEE-754 result as full parsing; otherwise keep fallback.
   - Falsifiability gate: no bit mismatch against `str::parse::<f64>()` over a generated corpus of long decimal literals plus all observed canada fallback shapes; canada direct reaches the W2 gate, not just a lower fallback count.

4. Do not implement table-only widening unless instrumentation proves exponent-range misses.
   - Falsifiability gate for table work: at least one canada fallback must show `decimal_exp < -342` or `decimal_exp > 308`, or `compute_float` indexing must be unreachable due to exponent constants. Without that proof, table edits are non-causal.

## Risks And Pre-Blocked Routes

- Raw f64 shortcut is pre-blocked in the handoff (`restart/skinny/tranches/sk-v7/HANDOFF.md:84-92`) and was explicitly avoided when EL was admitted (`skinny/REDRESS.md:517-526`). Replacing the path with unconditional `text.parse::<f64>()` or a bench-private shortcut would reopen a rejected route.
- Function-pointer dispatch tables, generic SWAR whitespace skipper, separator elision, capacity prescan, and event-cursor-style substrate work are pre-blocked and unrelated to this W2 number-specific route (`restart/skinny/tranches/sk-v7/HANDOFF.md:84-93`).
- String-scan routes rejected in SK-V6 Wave 2 must not be smuggled into W2. The retained trusted-string boundary collapse regressed every measured row and is blocked (`skinny/REDRESS.md:1344-1378`); delayed 64-byte trusted scanning is also blocked on the current baseline (`skinny/REDRESS.md:1439-1488`).
- Correctness risk is high around decimal tie cases. The EL implementation already returns `None` for ambiguous rounding (`skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:166-169`), so any widened scanner path must preserve or conservatively expand that fallback band.
- Performance risk: reducing fallback count may not be enough if digit scanning remains dominant. C2 says mesh EL is only 5.2% of total and digit scan dominates (`restart/skinny/tranches/sk-v7/HANDOFF.md:35-39`). Canada is more promising because the same handoff names about 25% f64 overflow fallback, but Phase 1 must measure this on the current W0b/W1 baseline.

## Sources

- `restart/skinny/tranches/sk-v7/SPEC.md:149-177` — Wave 2 scope, tasks, gates, revert protocol, hard cap.
- `restart/skinny/tranches/sk-v7/HANDOFF.md:19-39`, `:47-52`, `:66-93`, `:100-105` — V7 state, W2 dispatch, pre-blocked routes, W2 entry dependency.
- `skinny/RESULTS.md:10-11`, `:58-63`, `:158-162` — canada current direct result, masking probes, number-heavy row facts.
- `skinny/REDRESS.md:517-533`, `:1344-1378`, `:1439-1488`, `:2128-2198` — EL admit/residual, blocked string routes, current W0/W1 baseline history.
- `skinny/crates/parse-that-regex/src/number/eisel_lemire/mod.rs:1-177` — EL public surface, constants, Clinger path, fallback signal.
- `skinny/crates/parse-that-regex/src/number/eisel_lemire/algorithm.rs:14-93` — EL core, table indexing, ambiguous rounding.
- `skinny/crates/parse-that-regex/src/number/eisel_lemire/table.rs:1-8` — table range and cardinality.
- `skinny/crates/parse-that-regex/src/number/mod.rs:4-14`, `:226-272`, `:304-361`, `:438-447` — number span state, materializers, mantissa overflow policy, f64 parity tests.
- `skinny/crates/bbnf-bench/src/direct_struct.rs:88-105`, `:579-584` — direct hand parser number materialization path.
