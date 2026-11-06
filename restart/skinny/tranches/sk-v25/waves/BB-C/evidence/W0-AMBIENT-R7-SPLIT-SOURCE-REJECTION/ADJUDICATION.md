# BB-C.W0 ambient R7 structural/semantic split — source adjudication

MODEL: gpt-5.6-sol; EFFORT: ultra; ROLE: fresh hostile adjudicator. Model and
effort are self-declared and were not independently verified.

## Custody

- Source subject: `W0-AMBIENT-R7-SPLIT-SOURCE-SUBJECT/AUTHOR.md`, SHA-256
  `f6b2493c5ad479d1bd08faec62be7a2070fc95e09c80729037cd2bffda7851d5`.
- Binding predecessor: `W0-AMBIENT-R7-FULL-U8-REJECTION/ADJUDICATION.md`,
  SHA-256 `f58781ad6867b75aaef4c9febb19e85d2086bac078f53dc75611b2568b9741cb`.
- Critic A: `CRITIC-A.md`, SHA-256
  `4092de277cd002c7a4442a34a1c5b6b3d5ec8a8aea221bbeadcb55ac863ebb9b`.
- Critic B: `CRITIC-B.md`, SHA-256
  `eea1915def9fbdf141e7ea448cf6bda9df17608ea44c108f1423215120d135eb`.
- The subject receipt reproduces 36 files / 697,249 bytes, path digest
  `2bfe7c279e970b259c10d8791d21383afc1660882ca3668a5ff9c3d99ee12a1f`,
  and ordered-ledger digest
  `6940c6c098161575ce30188cfc071cc8fce041bbb2b6842bac9bc301d68a6ddb`.

## Verdict

**REJECT.** The split is directionally correct, but the source subject is not
safe for isolated regeneration and earns zero BB-C.W0, wave, lock, tranche, or
movement credit.

The following claims are correct but do not rescue the subject: the nominal
`AltBranchId(u16)` domain and 65,536-count ceiling; repeatable semantic U8 Pratt
emission; 16-byte/alignment-4 Tape layout; full Tape scope/bounds rollback; and
the present JSON grammar's sole `0u8` fact.

## Proven R7 blockers

1. Both alternative-width gates omit `RuleDirectives.recover`.
2. HRegex omits typed U64 input, mishandles suffixed U8, and fabricates zero on
   failed integer conversion.
3. delegated `Map(Ref)` and mapped mixed sequences can omit the outer payload;
   Wrap has the same ownership defect.
4. public Tape fields/constructors and unchecked typed cursor reads permit safe
   release-mode invariant violations.
5. inline U64 `u32::MAX` aliases `TAPE_NONE` and reads back as zero.
6. BBNF Pratt emits semantic Tag leaves while lowering still expects Span text
   and a legacy structural reducer.
7. CSS `colorFn` routes its color-space U8 through the functional-kind slot,
   and its controlling projection was absent from the subject receipt.
8. CSS rollback records only the span-stack length, so speculative pops cannot
   be restored.
9. emitter constant paths coerce non-U8 signed integers through U64.
10. generated Sheets, BBNF, and CSS authorities silently narrow wide integers.
11. six non-generated external tests still use removed `push_branch_tag`.

## Routed findings

- Token-led `usize as u16`: the cast is real, but present Strong/AltDispatch
  formation rejects above 127 branches. Replace it with checked conversion in a
  later hardening slice; it is not a current R7 escape.
- Keyword mining stops after index 255, but shape admission requires complete
  mining and general dispatch retains the fallback. Prefer explicit up-front
  abstention later; no lost branch is proved here.
- JSON's current `0u8 -> Null` mapping is correct. Add a generation-time
  zero-only assertion before the grammar expands.
- grammar-fact composite payload lifting drops outer ownership and signedness,
  but no current production DTA payload consumer was proved. Repair before DTA
  payload code generation is activated.

## Minimal clean-break repair

1. Validate body and recovery roots at both gates.
2. Use one exact scalar payload descriptor and one fallible integer decoder
   across Alt, Flat, Wrap, Keyword, and HRegex; retain outer Map as an exactly
   once, post-success continuation.
3. Seal Tape construction/representation, check typed readers in release, and
   encode `u32::MAX` unambiguously without changing the 16/4 footprint.
4. Lower BBNF semantic Tags through authoritative tag-to-operator metadata.
5. Separate CSS predefined and functional color roles and checkpoint the full
   span-start stack.
6. Preserve signed/unsigned widths exactly in generated authorities, then
   migrate stale tests without a compatibility shim.
7. Freeze a new source-only receipt containing every controlling projection and
   external test before any isolated regeneration.

After source acceptance, the required gates are isolated allowlisted
regeneration, byte-identical second regeneration, full workspace/all-target
compilation, absence of `push_branch_tag`, focused boundary and rollback tests,
wide-value projection tests, and the JSON/Sheets/BBNF/CSS parity fleet. Native
ARM64 profiling remains a later fleet-authorization gate.
