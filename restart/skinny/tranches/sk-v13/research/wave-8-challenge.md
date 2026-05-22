# SK-V13 W8 CHALLENGE - Per-Grammar Policy, Sink/View, Flag Surface

Cycle: W8 CHALLENGE. Disposition: ACCEPT WITH CONSTRAINTS.

The first CHALLENGE pass returned REVISE on CH1, CH2, and CH3, PASS with
constraints on CH4, and local CH5/CH6 review aligned with the same required
constraints. The plan is accepted only with the redress contract below; redress
may not start from the unrevised plan alone.

## CH1 Correctness

REVISE -> ACCEPT WITH CONSTRAINTS.

Redress must preserve raw `OffsetFlags` bytes while removing JSON semantic
names from generic tape roots. `HAS_ESC = 0x01` and `HAS_CONTROL = 0x02` must
be renamed to neutral physical storage names, preserving `0x01` and `0x02`.
No parity fixture or raw `flag_values` byte may drift unless explicitly
regenerated and gate-accepted.

JSON must consume grammar-owned flag semantics through generated config
helpers. Runtime and template `json::config` must own
`STRING_NEEDS_DECODE`, `needs_decode_flags()`, and
`string_needs_decode(flags)`. Runtime and template `view.rs` must use the
helper instead of direct bit containment. Track 2 JSON must stop calling the
generic JSON-named flag constant directly.

CSS `declaration_values_extended` policy must actually move into generated
config and be consumed by the live scanner and sink. The touched policy
includes `MAX_RECURSION`, normalized flag text, quote/escape/trivia helpers,
URL/identifier handling constants, and any moved row metadata. Runtime and
codegen templates must stay in sync and strict equality must remain
byte-for-byte stable.

The W8 report/gate surface must validate all planned fields, print
`G-W8-PER-GRAMMAR-POLICY <status> <path>`, reject support-only closure, and
enforce the measured block id when no row moves.

## CH2 Generality / Lock 14

REVISE -> ACCEPT WITH CONSTRAINTS.

Lock 14 must add exact W8 owner authorization:

- `SK_V13_W8_OWNER_PATHS`.
- Inclusion in `current_lock14_owner_paths`.
- Parent-diff authorization for `sk-v13-waveW8` and
  `sk-v13-wave8-challenge`.

Generic scan must reject bare JSON flag-name leakage in generic roots, not only
`OffsetFlags::HAS_ESC`. At minimum the token set must catch `HAS_ESC` and
`HAS_CONTROL` in scanned generic roots after W8. The tape root is already a
scan root; if W8 touches `parse-that-regex`, that root must be authorized and
covered, otherwise it stays untouched.

`GrammarConfig` remains absent from the public API. `JsonSink` remains
JSON-owned only; no generic `JsonSink` acceleration or generic quote,
backslash, control, or JSON policy branch may land.

## CH3 Regression / REDRESS

REVISE -> ACCEPT WITH CONSTRAINTS.

Redress must carry this revert protocol:

If W8 fails, revert runtime/codegen JSON config/view/generated edits, CSS
config/generated/sink edits, tape flag-name edits, gate/report/xtask/Lock14
edits, retained generated outputs, and W8 report artifacts as one slice. Retain
only research/plan/challenge evidence, write the rejected patch to
`/tmp/skv13-waveW8-rejected.patch`, and append REDRESS with the leakage path or
failed consumer/block evidence.

`skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` must not change
unless a JSON/CSS row moves or admits. JSON and CSS strict equality for the
named row consumers is mandatory; unchanged output is guard evidence only.

REDRESS 121 remains gate feed only. Generated per-grammar config is legal, but
public `GrammarConfig`, generic `JsonSink` acceleration, generic JSON
quote/backslash/control policy, and prose-only Lock 14 claims remain blocked.
W5/W6/W7 advisory evidence stays consumed and unchanged; their blocked decision
surfaces cannot count as W8 row movement.

## CH4 Cost

PASS with constraints.

Redress fits the cap only if it stays on the selected implementation paths.
CSS policy extraction is limited to `css_l4_declaration_values_extended`
runtime/template config, scanner, and sink. Other `css_l4_*` rows are read-only
guards unless a targeted test failure proves a direct dependency.

Do not edit `parse-that-regex` unless an implementation blocker proves it is
required. Do not run broad regen, broad formatting, or an all-CSS test matrix.
Use targeted report/gate/xtask plumbing by following W5-W7 patterns.

Minimum tests:

- `cargo test -p runtime json -- --nocapture`
- `cargo test -p runtime css_l4_declaration_values_extended_emit_fact_stream -- --nocapture`
- `cargo test -p codegen css_l4_declaration_values_extended_generated_runtime_reproducible -- --nocapture`
- declaration-values-extended cssparser/lightningcss equality tests.
- W8 report/gate tests.
- W8 xtask passthrough test.
- targeted `lock14_baseline` tests for token and owner-path logic.
- final advisory `gate-json` with W5/W6/W7/W8 evidence chained.

## CH5 Hidden Coupling

PASS with constraints.

Do not treat `parse-that-regex::StringFlags` as tape storage. It remains
lexical matcher state; the retained JSON tape fact is "string needs decode"
stored in physical bit `0x01`.

Track 2 JSON must mirror the JSON-owned physical flag meaning without using a
generic JSON-named flag. CSS declaration-values-extended must be the live row
consumer; rows with captured static fast paths cannot close W8. Report fields
must distinguish scanner/sink consumption from path-string or policy-only
claims.

## CH6 Anti-Paper-Close

PASS with constraints.

Support-only, policy-only, scaffold-only, path-string-only, and future-consumer
closures reject. W8 can close without row movement only through the exact
measured block:

`JSON-CSS-W8-PER-GRAMMAR-POLICY-CONSUMED-BUT-NO-ROW-MOVEMENT`.

The block must include strict equality for
`json/y_string_unicode/direct_to_struct/main` and
`css_l4/declaration_values_extended/direct_to_struct/main`, a hash-checked W8
policy artifact, Lock 14 PASS, JSON/CSS guard states, material differential,
and REDRESS-139.

## Accepted Redress Contract

- Rename generic tape flag constants to neutral physical bit names while
  preserving raw bytes.
- Move JSON flag interpretation behind generated JSON config helpers and use
  the read helper from runtime/template view code.
- Remove direct Track 2 JSON use of generic JSON-named flag constants.
- Move selected CSS declaration-values-extended policy into generated config
  and consume it from runtime/template scanner and sink.
- Add W8 report/gate/xtask plumbing with support-only rejection and measured
  block enforcement.
- Add W8 Lock 14 owner paths, parent-diff authorization, and generic-token
  leakage tests.
- Produce a hash-checked W8 policy artifact and W8 report under
  `restart/skinny/tranches/sk-v13/research/w8/`.
- Run the verification commands from CH4 plus the final advisory gate.
- Append REDRESS-139. Update RESULTS/ROLLING-SOTA only if a row moves or
  admits.
