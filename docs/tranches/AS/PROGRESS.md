# Tranche AS — Progress Log

Operational protocol: see `/INSTRUCTIONS.md` at repo root.

## Pre-AS (landed during AR audit phase)

- **Bootstrap loop closed** — modifier recovery in `lower_factor`
  (commit `0c6e011`). Idempotent regen: gen1 == gen2. 10-tranche
  deferral resolved.
- **JSON monolithic bench restored** — canada 1097, citm 2353,
  twitter 2069, data 1832, data_xl 1084 MB/s.
- **Google Sheets bench restored** — 122-129 MB/s.
- **4 broken test binaries fixed** — regex_classify, optimize,
  runtime_root, tape_parity (commit `8e1af6c`).
- **Feature wiring audit**: 12/12 AR features verified WIRED
  (including SIMD scan in parse-that).

## Phase 1 — CSS L4 parse activation

Status: **DONE** — all 4 sub-items complete

### AS.1.1 Diagnose CSS L4 parse failure

Root cause: `plan_regex_scanner` at `scanner_plan.rs:146` used a
`RegexClass::Identifier { .. }` catch-all that discarded
`allows_leading_dash` and `allows_double_dash_prefix` sub-flags.
The regex `/-?[a-zA-Z_][\w-]*/` in `propertyName` classified as
`Identifier { allows_leading_dash: true }` but codegen emitted
`scan_ident(DEFAULT_IDENT_CONFIG)`, which rejects leading `-`.
All vendor-prefixed CSS properties (`-webkit-*`, `-moz-*`, etc.)
failed to parse.

Parse failure offset in normalize.css: byte 301, line 11 (`html {`)
— the very first rule, because `line-height` succeeded but
`-webkit-text-size-adjust` failed inside the rule block.

### AS.1.2 Fix remaining lowering gaps

Fix: explicit dispatch on Identifier sub-flags in
`plan_regex_scanner` (commit `2d03c7b`):
- Plain `Identifier { false, false, false }` → `emit_call()` (DEFAULT)
- CSS `Identifier { allows_escapes: false, .. }` → `emit_call_css()` (CSS_IDENT_CONFIG)
- Escape-augmented `Identifier { allows_escapes: true, .. }` → fall through to generalized emitter

`kernels::identifier::emit_call_css()` already existed — just
needed the routing. Result: 15/15 CSS L4 tests pass including
normalize.css (6KB) and bootstrap.css (280KB).

Bootstrap regen: idempotent (gen1 == gen2). 24,685 lines.
Grammar roundtrip: 6/6 green. Leaf crate tests: 295 passed.
All bbnf-specific tests: 40 passed.

### AS.1.3 Fix VM parse path

JSON VM: canada 22, citm 51, data 76 MB/s. Working.
CSS VM: normalize 57, bootstrap 25, tailwind 22 MB/s. Working.
Both VM paths benefit from the scanner_plan fix.

### AS.1.4 Validate all bench targets

**Hard gate PASSED**: all bench targets compile and run.

| Bench target | Status | Key number |
|-------------|--------|------------|
| compile_pipeline | WORKING | JSON 0.13ms, CSS L4 9.9ms |
| json_monolithic | WORKING | citm 2296, twitter 2015 MB/s |
| css_l4 | **FIXED** | bootstrap 513, tailwind 556 MB/s |
| css_competitors | WORKING | beats lightningcss 4-6x, cssparser 1.1-1.3x |
| css_stress | WORKING | selectors 28 GB/s |
| css_vm | **FIXED** | bootstrap 25 MB/s |
| json_vm | WORKING | citm 51, data 76 MB/s |
| json_stress | WORKING | strings 14 GB/s |
| json_parse_that | WORKING | citm 1082, twitter 985 MB/s |
| google_sheets | WORKING | 119-126 MB/s parse |

Results captured in `docs/benchmarks/post-AS-phase1.json`.

## Phase 2 — Span scalar admission + direct projection

Status: NOT STARTED

## Phase 3 — Scanner truth

Status: NOT STARTED

## Phase 4 — Profile and close sonic-rs gap

Status: NOT STARTED

### Pre-Phase 4 analysis: JSON regression root cause

The AS.md claim that "post-AQ JSON numbers were from a broken
parser" is **incorrect for JSON**. The modifier fix (commit
`0c6e011`) only changed CSS L4 (171→184 rules) and Sheets (37→38).
JSON stayed at 10 rules — identical IR, identical generated parser.

The regression (AQ→AR) comes from AR codegen changes:

| Dataset | post-AQ | post-AR | Delta |
|---------|---------|---------|-------|
| canada | 1796 | 1097 | **-39%** |
| citm | 2698 | 2353 | **-13%** |
| twitter | 2086 | 2069 | **-1%** |
| data | 1939 | 1832 | **-6%** |
| data_xl | 1348 | 1084 | **-20%** |

Primary suspect: `meta: Vec<u8>` parallel side-channel (commit
`d9a760a`). Extra byte write per record + separate cache line
pressure. Canada is record-heavy (~500K records) so per-record
overhead is highly visible.

Fix path: fold `meta_idx` into `TapeRec.kind` byte — TapeKind
only uses values 0-15 (4 bits), leaving 4 upper bits for meta_idx.
Eliminates the parallel Vec entirely. For Alt branches > 15, use
an overflow mechanism.

## Phase 5 — Scanner consolidation

Status: NOT STARTED
