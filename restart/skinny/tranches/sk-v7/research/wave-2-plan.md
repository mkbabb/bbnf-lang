# SK-V7 Wave 2 Plan: Zero-Fallback Mantissa-Widen Redress

Inputs: `wave-2-r1-el-mantissa.md`, `wave-2-r2-fallback-profile.md`, `wave-2-r3-direct-gate.md`, `restart/skinny/tranches/sk-v7/SPEC.md` §4, and `restart/skinny/tranches/sk-v7/HANDOFF.md` §3.

Intervention: falsify the W2 mantissa-widen candidate on current HEAD by measuring the actual canada f64 fallback pool, preserving the EL code unchanged, and closing W2 with a REDRESS rejection if the named consumer is absent.

Owner paths:
- `skinny/REDRESS.md`
- `restart/skinny/tranches/sk-v7/research/wave-2-redress.md`
- `/tmp/skv7-wave-2-rejected.patch`

Falsifiability gate:
- `canada` direct remains measured against strict `sonic_rs_direct_to_struct`.
- `numbers` direct remains measured and must not regress from PASS.
- Fresh fallback attribution must show whether `materialize_f64` reaches `text.parse::<f64>()` for canada.
- If canada has a non-zero fallback pool and a scoped mantissa patch closes the row, admit the patch with `primitive-checkasm` green.
- If canada has a zero fallback pool or a mantissa patch cannot make canada direct >=100% sonic-strict without regression, reject W2 and record the next candidate as numeric-array scan/dispatch work.

Hard cap: 105 minutes total for W2 redress, with measurement prioritized over source changes because research has already falsified the stated causal path.

Revert protocol:
- If any exploratory source patch is attempted and fails, save it to `/tmp/skv7-wave-2-rejected.patch`.
- Revert all source changes before the redress commit.
- Commit only the REDRESS entry and redress artefact for a rejection.

Same-wave consumer: `bbnf_bench::direct_struct::track1_digest` and `track2_digest` on the `canada` direct-to-struct row, via `match_number_span_from_first` and `materialize_f64`.

Pre-blocked routes:
- REDRESS 28+33: `match_tiny_plain_string` wiring as parse-G fix.
- REDRESS 50-55: SK-V5 UTF-8 fusion routes.
- REDRESS 60-72: SK-V6 retained-parse and direct-materialization routes.
- Handoff §3 also blocks raw f64 shortcuts, function-pointer dispatch tables, capacity prescan, generic SWAR whitespace skipping, separator elision, pair-token fusion, and EventCursor-style parallel prepasses.

Challenge notes:
- The EL power table already spans f64 `[-342, 308]`; table-only widening has no exponent miss to consume.
- The current canada attribution reports 111,080 f64 candidates, zero mantissa overflow, zero ambiguous EL returns, and zero `str::parse::<f64>()` fallback.
- A replacement implementation inside W2 would violate profile-first discipline unless a fresh profile names a concrete numeric-array scan/dispatch leaf and a same-wave consumer.
