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

Status: NOT STARTED

CSS L4 bootstrap.css parse still fails. JSON and Google Sheets
work post-modifier-fix. The CSS grammar likely has additional
expression shapes where modifier or binary-operator recovery fails.

## Phase 2 — Span scalar admission + direct projection

Status: NOT STARTED

## Phase 3 — Scanner truth

Status: NOT STARTED

## Phase 4 — Profile and close sonic-rs gap

Status: NOT STARTED

## Phase 5 — Scanner consolidation

Status: NOT STARTED
