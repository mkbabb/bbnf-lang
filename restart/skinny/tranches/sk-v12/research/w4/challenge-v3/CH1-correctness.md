# SK-V12 W4 CHALLENGE V3 - CH1 Correctness

Verdict: ACCEPT.

PLAN-V3 corrects the previous caller ambiguity by selecting the exact
delimiter-member route:

- `find_ascii_set_member64` is the scalar reference shape.
- The generated CSS `scan_block` caller is limited to ASCII delimiter search for
  `{`, `;`, and `}`.
- Caller parity is scoped to `checkasm_ascii_set_member_find_64`, not only the
  raw equality-mask primitive.

The route is semantically admissible because the current CSS declaration-value
scanner already treats all non-delimiter bytes as payload bytes and advances one
byte at a time. A member-find primitive that returns the first delimiter offset
preserves that fact stream when it is later wired into production. PLAN-V3 also
marks the stale A5 `skip_ws_and_comments` layout framing as superseded, so the
plan no longer asks redress to optimize a call shape that does not exist in the
current generated parser.

CH1 does not require a PLAN-V4 correctness rewrite.
