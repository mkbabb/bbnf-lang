# SK-V12 W4 A2 - Selectable ASM-Gen Candidate Feasibility

Date: 2026-05-20.
Scope: W4 research A2; compare the five SPEC Section 9 selectable
aarch64/Apple Silicon ASM-gen candidates against the landed CSS L4 row,
JSON guard evidence, Lock 16, and orphan-disposition obligations.
Ownership: research artifact only; no source edits.

## Authority Read

- SPEC Section 9 makes W4 an ASM-gen CSS consumer plus aarch64 orphan
  disposition wave. The plan may select at most one primary candidate from
  `a64_tbl_tbx_byte_class_mask64`, `a64_udot_digit_run_span`,
  `a64_wide_string_special_scan64`, `a64_hex_quartet_decode_x4`, and
  `a64_ascii_set_run_skip`. It requires a same-host microbench before
  production routing, a same-wave CSS or JSON-guard consumer, strict equality,
  JSON guard disposition, Lock 16, and zero production orphans by close.
- W2 resolved the `escape_mask_64` correctness prerequisite in REDRESS-122:
  the historical xorshift falsifier `0xCAFEF00DBAADF00D` is now covered by
  direct checkasm plus runtime scanner parity. W4 may attempt SIMD admission,
  but it still must carry scalar reference + checkasm + same-wave consumer.
- W1b-2b already records a CSS L4 same-plane admission candidate:
  Track 1 `429.34420791225705 Mbps`, cssparser oracle `217.42665242186035 Mbps`,
  lightningcss `168.92962215656692 Mbps`, USER PIN threshold
  `169.92962215656692 Mbps`, and margin `259.41458575569015 Mbps`. Therefore
  W4 does not need a broad speed rewrite to reach the CSS close bar; it needs a
  legal ASM-gen attempt and zero-orphan disposition without breaking the row.
- The current CSS runtime is a tiny scalar declaration-value scanner. Its real
  local consumers are block/declaration separators, colon search, layout/comment
  skipping, trimming, ident/number runs, and token delimiter dispatch. The
  fixture has no quoted string or CSS escape token; string/hex candidates would
  require a wider CSS fixture/parser surface before they are CSS-consumed.

## Candidate Matrix

| Candidate | Target hot leaf | CSS / JSON consumer | Existing primitive status | Required scalar, checkasm, microbench | Likely cost | Feasibility |
|---|---|---|---|---|---:|---|
| `a64_tbl_tbx_byte_class_mask64` | `container_dispatch`, `bounded_plain_string_scan`, `ascii_whitespace_skip`, `simd_movemask` | CSS delimiter/layout byte-set classifier in `css_l4_declaration_values::generated`; JSON guard through existing classifier paths only | `byte_class_from_eq_set_64_neon` is real NEON with scalar/checkasm. `classify_tbl4` is real TBL. `byte_class_from_table_64_neon` delegates to scalar. TBX is absent. | If W4 adds a table/TBX body: refresh scalar table oracle, checkasm low-6 collisions, high-bit bytes, empty/full sets, CSS delimiter/layout sets, all alignments/tails; microbench CSS delimiter/layout loops. If W4 only consumes existing eq-set: no new body, but caller microbench must prove the consumer. | Medium | Good support route, but as the primary it risks becoming "classifier only." It needs a concrete generated CSS byte-set caller to avoid proof-only admission. |
| `a64_udot_digit_run_span` | `number_digit_span`; CSS `<number>`, `%`, dimensions, RGB numeric args | CSS `consume_number` in generated declaration-values scanner; JSON numeric rows as guards | `digit_mac::parse_4_digits` has scalar fallback and dotprod body; current test is smoke-level only. No public digit-run/span API is landed. | New digit-run scalar oracle returning end/count/prefix/truncation; strict UDOT parity for valid/invalid groups, mixed lanes, non-digit offsets, all alignments/tails, overflow/truncation; microbench generated CSS number/dimension/percentage loops and JSON numeric guards. | High | Legal but overbuilt for this fixture. The CSS row has a small number-token count, so fixed-width UDOT setup overhead is likely to dominate. Use only as measured-reject evidence if W4 deliberately wants numeric ASM-gen proof. |
| `a64_wide_string_special_scan64` | `bounded_plain_string_scan`, `string_escape_decode`, `simd_movemask` | JSON string scanner exists through parse-that; CSS consumer would require quoted strings, URL/raw spans, or escaped identifiers in the CSS fixture/runtime | Current `string_block` is 16-byte scalar + NEON and is consumed by JSON parse-that. No 64-byte scalar oracle exists. `byte_context` is an orphan support helper that would only be consumed by this family. | New 64-byte scalar oracle for terminator/escape/control/non-ASCII masks; checkasm all special positions, multi-hit priority, alignments/tails, non-ASCII boundary, cross-block context; CSS string/URL/escape fixture plus strict lightningcss equality; JSON guard microbench. | High | Not the W4 first pick. It can consume `byte_context`, but it expands the CSS surface substantially and reopens prior string proof-to-production failure modes. Candidate is better as a later string-specific wave if CSS strings become measured hot. |
| `a64_hex_quartet_decode_x4` | `unicode_escape_hex_decode`, `string_escape_decode`; CSS hex/escape subcases | JSON `\uXXXX` path has an x4 call in parse-that; CSS consumer is not current because CSS escapes are variable-width and `#ff00ff` is emitted as a hash lexeme, not decoded hex | x1 scalar and x1 NEON exist. x4 NEON exists but only smoke-tested; audit marks x4 proof-only/no same-wave consumer. | Scalar x4 oracle built lane-by-lane from x1; checkasm invalid nibble in every position, mixed valid/invalid quartets, alignments/tails, boundary cases, surrogate policy handoff; CSS caller must keep CSS variable-width policy outside the primitive. | High | Weak CSS fit for the current row. JSON reuse alone is insufficient under the user pin. Select only for a measured-reject route if W4 intentionally records hex ASM-gen failure evidence. |
| `a64_ascii_set_run_skip` | `ascii_whitespace_skip`, `container_dispatch`, local first-nonmember extraction | CSS `skip_ws_and_comments`, `trim_start`, and `trim_end` can consume a grammar-supplied layout byte-set; comments remain generated caller policy. JSON `skip_ascii_whitespace` can be a guard, not the admission proof. | No dedicated run-skip primitive today. Existing eq-set/table scalar refs and `byte_class_from_eq_set_64_neon` can back the first-nonmember mask. Current CSS runtime has direct scalar loops at the exact consumer sites. | Add scalar `skip_byte_set_run` oracle or CSS-local scalar reference; if SIMD-backed, reuse/refresh eq-set checkasm for first-nonmember offsets, high-bit bytes, empty/all-member sets, tails, alignments, and CSS layout sets; microbench CSS layout/trivia loops plus full `nonjson_css_l4` equality/throughput. | Low/Medium | Best W4 primary candidate. It is narrow, CSS-consumed, grammar-neutral, and can use the already-proven byte-class mask without forcing string/number/escape policy into generic SIMD. |

## Candidate Notes

### `a64_tbl_tbx_byte_class_mask64`

This is the strongest primitive foundation but not the cleanest primary W4
unit by itself. The in-tree facts are favorable: `byte_class_from_eq_set_64` has
a scalar executable spec, a real aarch64 NEON equality fan-out, and a strict
checkasm file; `classify_tbl4` already uses TBL for low-6 classification.
However, the table64 aarch64 entry currently delegates to scalar, and TBX is
not present. A W4 plan that selects C1 must name the generated CSS caller in
the same commit, otherwise it lands a classifier body with no row-moving
consumer.

The practical C1 use is as backing for `a64_ascii_set_run_skip`: classify a
64-byte window against a generated layout/delimiter set, then the caller extracts
the first nonmember and keeps comment/trivia policy in generated CSS code.

### `a64_udot_digit_run_span`

UDOT is technically attractive but mismatched to the immediate CSS row. The
current `digit_mac` helper proves only four digits, and the CSS declaration
fixture has sparse, short numeric spans (`50%`, `.5`, `-10px`, `255`, `128`,
`0`, `0.5`, `100px`). A useful W4 admission would need a full
digit-run/span API with caller-owned CSS number policy. That is more surface
than W4 needs after W1b-2b already cleared the lightningcss bar.

This route is suitable as measured-reject evidence if the campaign needs an
ASM-gen attempt and the plan prefers numeric proof, but it should not be the
first production candidate.

### `a64_wide_string_special_scan64`

The wide string route is a poor fit for this exact CSS row. It has real JSON
guard consumers, and a 64-byte scanner could eventually be useful, but the
landed CSS runtime currently does not exercise quoted strings, raw URL spans,
or escaped identifiers. Making it CSS-consumed would require expanding both the
fixture and generated scanner semantics, then proving strict equality against
lightningcss. That is an expensive string wave, not a contained W4 ASM-gen
attempt.

The only orphan it naturally consumes is `byte_context`; selecting it just to
save that orphan is backwards. Demote/remove `byte_context` unless a later
string wave has measured CSS evidence.

### `a64_hex_quartet_decode_x4`

The x4 hex body is already present but proof-light. It needs the most obvious
Lock 16 work: strict x4 scalar oracle and mixed-validity checkasm. The blocker
is the consumer. JSON `\uXXXX` reuse is not CSS admission, and CSS escapes are
variable-width with caller-owned termination semantics. The current CSS fact
stream does not decode hex colors; it emits the hash lexeme bytes. A W4 x4 hex
attempt would therefore either become JSON-guard-only or require a CSS escape
feature expansion. Both are too broad for the first W4 candidate.

### `a64_ascii_set_run_skip`

This is the smallest legal W4 primary. It maps directly onto the current CSS
scanner: layout trimming and `skip_ws_and_comments` are scalar loops today, and
comments can remain generated caller policy around a generic byte-set run
primitive. The SIMD body can be the existing eq-set NEON mask plus a local
first-nonmember extraction. That keeps Lock 14 clean: the generic primitive
sees only a caller-supplied byte set, while CSS decides which bytes count as
layout and how comments terminate.

The micro-proof should be explicit and modest:

- isolated same-host microbench for `skip_css_layout_run` over retained fixture
  windows plus adversarial synthetic CSS trivia windows;
- scalar oracle over empty spans, all-member spans, first miss at every offset,
  comments immediately before/after whitespace, high-bit bytes, and tails 0..63;
- strict equality on `css_l4_declaration_value_fact_stream`;
- Criterion rerun of `nonjson_css_l4` and JSON guard command;
- no claim that JSON `skip_ascii_whitespace` is the admission proof.

## Orphan Interaction

The selectable candidates do not automatically clear the required five-orphan
ledger:

| Orphan | Candidate interaction | W4 disposition implication |
|---|---|---|
| `bitmap_prefix_xor_64` | Only string-region / PMULL-style routes would consume it. None of the recommended C6 work needs it. | Inventory-demote or remove the aarch64 wrapper with REDRESS-88 citation unless a narrow measured string consumer is selected. |
| `bitmap_next_set_bit` | C6 may need local first-set extraction, but using the existing wrapper as a production CTZ route would reopen REDRESS-89. | Keep first-nonmember extraction local/scalar or compiler-lowered; demote/remove the wrapper unless a narrow CSS consumer proves it. |
| `bulk_emit_positions_64` | No selected C6 path emits retained positions. | Demote/remove; do not create a position side vector. |
| `byte_context` | Only C4 wide string scanning has a natural use. | Demote/remove if C4 is not selected. |
| `cache_hints` | No candidate has a proven writer hot leaf. | Demote/remove; no scalar/checkasm semantics exist for hints. |

If W4 chooses C6, zero-orphan close should be achieved by explicit
inventory-demotion/removal evidence for all five, not by widening C6 until it
touches unrelated support modules.

## Recommendation

Select `a64_ascii_set_run_skip` as the W4 primary candidate, implemented as a
CSS layout/trivia run-skip consumer backed by the already-proven aarch64
byte-class mask where the microbench proves a gain.

This gives the best engineering shape:

- it is CSS-consumed on the current row;
- it is grammar-neutral when the byte set comes from generated CSS metadata;
- it keeps comments and broader CSS trivia policy in generated caller code;
- it reuses the strongest existing Lock 16 primitive family;
- it avoids reopening string, Unicode, numeric materialization, digest, or
substrate categories;
- it leaves room for honest zero-orphan demotion/removal in the same W4 packet.

Measured-reject route: if the isolated CSS layout/trivia microbench does not
beat the scalar loop, or if the full `nonjson_css_l4` row regresses beyond the
accepted guard slack, reject C6 with REDRESS evidence rather than switching
mid-wave. The rejected evidence still satisfies the campaign's ASM-gen-attempt
requirement if it includes scalar reference, checkasm/microbench, same-wave CSS
consumer attempt, strict equality/JSON guard measurement, material differential,
and the five-orphan disposition table.
