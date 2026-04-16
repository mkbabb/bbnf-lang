# AT retrospective (for AW re-planning)

Sources: `docs/tranches/AT/{AT.md,PROGRESS.md}`, AT commit span
`0efc52d2..74ade4c6` (18 commits), AS FINAL, AU §"What AT broke".

## 1. Scope vs plan — deltas

Seven phases, 18 hard gates declared. Landed:

| Phase | Landed | Dropped |
|------|--------|---------|
| 1 projection | `resolve_branch_type` + multi-type `__payload_tag` (0ff06bc) | KvPair; `.map(\|_\|())` sweep only on JSON (CSS L4 still 206 at AU entry). |
| 2 redress | SIMD guard, meta fold, capacity. canada 1089→1464 (gate met). | — |
| 3 decode | kernel only (parse-that accb3c0) | builder, scanner, grammar, view, bench — gates 8-9 silent. |
| 4 profile | payload pre-alloc | samply, hot-path, NEON frac, `post-AT.json` — gate 10 missed. |
| 5 structural | JSON deep-walk + `validate.rs` | CSS / Sheets / BBNF deep tests — gate 11 partial. |
| 6 cleanup | dead code, fixtures, parse-that commits | StructRegistry, named-struct view, ParsedGrammar — gate 13 missed. |
| 7 CSS parity | `\|=` disambig, non-ASCII ident, `\xHH` escape | Semantic parity audit never written. |

Headline: Phase-1 passed its grep gate (`push_leaf_with_f64`
appears in expanded JSON) but failed runtime purpose.
`branch_pushes_children` still classified every `value` branch
as compound; the new typed writes were dead stores. AU.1.1
(83357e4) is the real activation patch.

## 2. Silent vs declared deferrals

**Declared**: NEON frac, samply, StructRegistry, named-struct
view, ParsedGrammar.

**Silent**: string-decode codegen (3.2-3.5), `post-AT.json`,
semantic parity audit, CSS/Sheets/BBNF deep tests, gate-5
"resolver handles Constant" (only Map/FnDescriptor exercised),
AS parse-that trail (337a0b2 stale test emerged post-landing).

## 3. Orchestration friction

Single-agent linear execution. No wave table (AU introduces
that). Commits strictly sequential; no fan-out, no worktree
contention.

## 4. Agent-layer friction

Emitter mental model ("Alt captures typed payload") layered on
an unread driver assumption: `branch_pushes_children` returns
false for leaf-only Ref branches. It didn't. `driver/alt.rs`
was absent from AT.md's critical-files table; the agent never
opened it. The bug lived in the unlisted file.

## 5. Edict adherence

- **no-workarounds**: honoured — SIMD root-caused, meta folded
  (not side-tabled).
- **no-deferrals**: violated thrice (3.2-3.5, 4.1+4.3+4.4,
  6.1+6.2+6.6) — each chains into AU's ledger.
- **no-value-discard**: enforced at emission, not runtime.
- **typed-materialization-invariant**: claimed; empirically
  dead until AU.

## 6. Chronic deferrals

**IN**: 64-byte padding (AR.5.2 +3), NEON frac (AR.8.1 +3),
Named struct ABI, StructRegistry (AS.2.3 +2), string-decode
wiring (new).

**OUT → AU**: ParsedGrammar (now +11), StructRegistry (AU.4.2
deletes it — resolution by demolition), named-struct view,
string-decode codegen, padding, NEON frac, samply,
`post-AT.json` equivalent.

## 7. Mid-tranche restructuring

None. Phase 7 folded pre-execution (0efc52d2, 3218aed3 record
AS audit findings into AT.md before Phase-1 dispatch) —
respects `new-tranche-new-doc` because no mid-execution pivot
occurred.

## 8. Lessons

1. **Emission is not effect.** A codegen change that emits
   `push_leaf_with_f64` is not activation; the driver decides
   whether that code runs. AT passed its grep gate and failed
   its purpose. Projection gates must close on runtime
   assertions (bench delta, tape-walk test asserting leaf kind),
   never on expanded-source grep alone.

2. **Critical-files table is the contract.** AT.md listed 13
   files; `driver/alt.rs` was absent; the agent read exactly
   those 13. What isn't on the list will not be read, and the
   bug will live in the unread file. Plan reviews must audit the
   file table against the data flow, not the phase narrative.

3. **"Kernel landed, wiring deferred" is how debt compounds.**
   AT.3 landed `decode_json_string_to_arena`; builder method,
   scanner variant, grammar annotation, view accessor, and
   bench all deferred. AU inherited a feature that looks
   half-done from commit history but is zero-percent exercised.
   Prefer landing no kernel over a kernel whose consumers are
   deferred.
