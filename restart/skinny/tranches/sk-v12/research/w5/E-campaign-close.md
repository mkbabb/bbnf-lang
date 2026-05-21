# SK-V12 W5 Research E: Campaign Close Shape

## Campaign Close File

Use `restart/skinny/CAMPAIGN-CLOSE-SK-V12-V12.md`. The user close-output shape
names `CAMPAIGN-CLOSE-SK-V12-V{N}.md`, and the closing tranche is V12.

## Required Content

- Close disposition: `PASS-ADMIT` by clause (a), not `FIXPOINT`.
- Final CSS row:
  - Row: `css_l4/declaration_values/direct_to_struct/main`
  - Plane: `css_l4_declaration_value_fact_stream`
  - Track 1: `429.34420791225705 Mbps`
  - cssparser oracle: `217.42665242186035 Mbps`
  - lightningcss: `168.92962215656692 Mbps`
  - Threshold: `169.92962215656692 Mbps`
  - Margin: `259.41458575569015 Mbps`
  - Ratio vs lightningcss: `2.5415566697611705x`
  - Strict equality: `pass:track1=cssparser=lightningcss`
  - Fact SHA-256:
    `caf97bee6e413157e6114985bc1108bc3a8fbf597a1e519b3ccff905d2e5236c`
- Per-wave disposition:
  - W0 artifact-only revalidation.
  - W1a JSON guard refresh: REDRESS-121.
  - W2 `escape_mask_64` correctness prerequisite: REDRESS-122.
  - W1b-1 CSS generated Track 1 + cssparser oracle scaffold: REDRESS-123.
  - W1b-2a lightningcss same-plane comparator: REDRESS-124.
  - W1b-2b CSS SOTA report gate: REDRESS-125.
  - W4 delimiter ASM microbench route and zero orphan disposition:
    REDRESS-126.
  - W5 close reconciliation: REDRESS-127.
- SIMD coverage final state: W2 resolved the escape-mask blocker; W4 final
  orphan count is zero; W4 selected candidate passed microbench but stayed
  routed, not production-wired.
- Union disposition: USER PIN D3 left the category unblocked, but ADMIT close
  does not require a fresh W3. REDRESS 96/97/98 remain historical failed union
  implementations.
- ASM-gen disposition: USER PIN D4 left the category unblocked; W4 records the
  measured new ASM-gen route attempt through `find_ascii_set_member64` over
  `b"{};"`.
- Routed remainder: optional future W4 production/gate split; future union
  attempts legal but not needed for SK-V12 close; JSON direct residuals remain
  governed by REDRESS-119; Sheets and BBNF-self remain fallback-only history.
- Totality fold deltas from `skv12-totality-fold-scout.md` Section 4:
  Lock 1 keeps REDRESS 96/97 history; Lock 14 now has a concrete
  GrammarConfig/non-JSON gate; Lock 16 escape-mask blocker is resolved; BENCH
  has `sk-v12-css-l4-sota-v1`; SK-V11 close remains guard/routed history; W4
  inventory demotion is valid zero-orphan evidence; no totality architecture
  rewrite is implied by close.
