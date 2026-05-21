# SK-V13 S-P2 V3 CH2: Generality / Lock 14

## Verdict

ACCEPT.

## Evidence

- The CH2 contract remains unchanged: every candidate must carry a P2-F
  grammar-neutral verdict, and JSON-only candidates without a byte-set,
  classifier, tape, or per-grammar-template expression must be revised or
  dropped (`restart/prompts/skinny/PASS-2-RESEARCH.md:102`-`107`). Lock 14 is
  still the controlling rule: generic crates may not gain grammar arms,
  grammar-named modules, grammar-specific public types, grammar feature flags,
  or hand-written per-grammar runtime files
  (`restart/locks/LOCKS.md:78`).

- The V2 consolidated blocker was narrow: P2-F needed to stop exposing CSS
  row/fact-stream scopes as immediate primitive eligibility, while preserving
  all accepted V2 folds (`restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V2-CONSOLIDATED.md:12`-`24`,
  `:29`-`38`). V3 does that. P2-F is stamped Cycle V3 and says its scope
  includes the V2 CH1 CSS row-scope fold
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:1`-`8`).

- The new `CSS-ROW-SCOPE-CONDITIONAL` verdict preserves Lock 14. P2-F defines
  it as a generated CSS parity row/fact-stream scope, not primitive admission;
  S-P3 may plan it only with fresh narrow CSS parser profiling or same-wave
  strict lightningcss/cssparser row movement
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:44`-`47`).
  The six CSS rows are all stamped with that verdict and each is constrained to
  generated row/fact-stream output plus scalar oracle / same-wave movement
  requirements (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:69`-`74`).

- The fold does not smuggle CSS logic into generic crates. P2-F explicitly says
  CSS row scopes are generated fact streams, not generic runtime primitives or
  CH1 hot-leaf evidence; the grammar-neutral property is the shared template and
  metadata mechanism, not hardcoded CSS branches
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:123`-`132`).
  It also rejects generic at-rule branches and requires vendor/custom taxonomy
  to come from grammar metadata rather than hardcoded generic code
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:72`-`74`).

- The V3 fold does not remove the P2-A C1-C8 mapping. P2-A defines C1-C8 as
  `class_mask64_transient`, `bounded_special_string_end`,
  `escape_segment_hex_decode`, `digit_run_accumulate`,
  `generated_first_follow_probe`, `same_loop_structural_mask_consume`,
  `ascii_set_member_find64_css`, and `output_digest_fold_u64x2_sink`
  (`restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md:70`-`81`).
  P2-F still carries the literal table mapping those same eight candidates to
  admissible, conditional, route-production, or fact-stream-only verdicts, with
  boundaries against retained sidecars, generic JSON object/array/key-colon
  branches, parser-speed digest claims, and `JsonDigestSink` internals
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:108`-`119`).

- P2-F V3 explicitly states that the accepted V2 folds remain unchanged, that
  the literal C1-C8 mapping resolves the cross-read blocker, and that
  grammar-neutrality is admitted only at byte-set, policy, fact-stream,
  regex-analysis, resolver, or codegen-private same-substrate boundaries
  (`restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md:247`-`253`).
  That is exactly the Lock 14 line: CSS-specific semantics may be generated from
  grammar metadata, but they do not become generic-crate control flow.

## Blockers / Fold Requirements

No CH2 blocker remains.

Carry-forward requirements:

1. Preserve `CSS-ROW-SCOPE-CONDITIONAL` as row-production scope, not primitive
   eligibility. Any S-P3 CSS row must keep the fresh narrow CSS parser profile
   or same-wave strict lightningcss/cssparser row-movement gate.
2. Preserve the P2-A C1-C8 verdict table and its rejection boundaries. Renames
   or decompositions must retain auditable lineage back to C1-C8 and the P2-F
   verdict vocabulary.
3. Do not promote CSS taxonomy, declaration-value, selector, media, nesting, or
   vendor/custom logic into generic crates. Those differences remain grammar
   metadata plus generated output only.
4. Do not promote inventory-only or `NOT-S-P3-ELIGIBLE` support primitives
   without a later accepted research fold naming fresh evidence, scalar parity,
   checkasm where applicable, and a same-wave non-JSON or row-moving consumer.

## Disposition

CH2 generality / Lock 14 accepts the V3 fold. `CSS-ROW-SCOPE-CONDITIONAL`
closes the V2 CSS eligibility wording problem without weakening Lock 14 and
without removing the P2-A C1-C8 grammar-neutral mapping.
