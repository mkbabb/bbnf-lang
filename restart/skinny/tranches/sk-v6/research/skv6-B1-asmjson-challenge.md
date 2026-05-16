# SK-V6 B1: asmjson fold-back challenge

Date: 2026-05-15.
Workspace read-only: `/Users/mkbabb/Programming/bbnf-lang`.
Artifact: `/tmp/skv6-B1-asmjson-challenge.md`.

## Verdict

The asmjson fold-back is directionally useful only as a primitive/backend-shape
reference. It is overclaiming if it is read as proof that BBNF can now emit a
strict, grammar-general, asmjson-class parser from Grammar IR. The defensible
claim is narrower:

> asmjson demonstrates a high-throughput JSON-specific DPDA architecture:
> chunk byte-class masks, `tzcnt`/next-event seeking, direct-threaded finite
> control, and a bounded explicit stack. BBNF may reuse this as an x86
> `CollapsedStage` research shape only for grammar/ISA/output rows that pass
> explicit strictness, stack, parity, and same-plane gates.

Anything stronger risks reviving the PSI/DTA failure shape: a new substrate or
automaton promise that is one wave away from a measured, correctness-equivalent
consumer.

## Overclaims To Cut

1. **"asmjson proves the generic `CollapsedStage` route."**
   It does not. asmjson is a hand-written JSON DPDA. It proves the finite
   control + stack + mask-seek architecture can be made fast for one grammar,
   not that Grammar IR can mechanically derive correct direct-threaded DPDA
   kernels for arbitrary grammars.

2. **"The executable spine is identical across JSON, CSS L4, BBNF-self, and
   Sheets."**
   This is too broad. CSS L4 has recovery/layout pressure, BBNF and Sheets have
   Pratt/host-fn shape, and the V9.5 PSI audit says those route to RD/EagerTape
   or EventTape, not collapsed-stage DPDA. The safe phrase is "identical across
   admitted `CollapsedStage` grammars", where the current known set is JSON and
   maybe CSV.

3. **"CollapsedStage is auto-selected via CPUID."**
   CPUID is necessary but not close to sufficient. Auto-selection by ISA alone
   is a contrivance: the row also needs grammar admission, strictness
   validation, a committed kernel/template for that grammar class, parity
   tests, and equivalent-hardware measurement.

4. **"Past asmjson" / "1.28x asmjson" as a current SOTA-beat claim.**
   This is only an aspirational x86 successor-tranche target. It cannot classify
   the Apple Silicon SK-V6 gate, cannot use asmjson's permissive rows as strict
   anchors, and cannot be accepted without same-corpus, same-output,
   same-strictness measurement.

## Strictness Caveat

asmjson is not a strict JSON anchor by default. The local A1/A3 notes and
`skinny/RESULTS.md` agree on the flaw shape:

- asmjson treats all bytes `0x00..0x20` as whitespace, while RFC 8259 permits
  only `0x20`, `0x09`, `0x0A`, and `0x0D`.
- asmjson does not reject unescaped C0 controls inside strings.
- the parser relies on `&str`/unchecked string handoff in places rather than a
  timed byte-entry UTF-8 validation plane.
- the headline AVX-512 path is x86-only; the M5 Max row is SWAR and synthetic.

Therefore a BBNF `CollapsedStage` row cannot inherit asmjson's exact masks and
still claim strict JSON. Strict admission requires:

- layout mask generated from grammar policy; JSON strict layout is exactly
  `{0x20, 0x09, 0x0A, 0x0D}`;
- an explicit invalid-control mask for JSON strings;
- complete escape validation, including unescaped controls and malformed
  escape sequences;
- UTF-8 validation boundary recorded as `strict_bytes` if byte-entry validation
  is timed, or `strict_after_utf8_view` if the row takes `&str`;
- trailing junk, delimiter, number, literal, close-token, and depth-overflow
  rejection in the measured parse scope;
- row metadata for `max_depth`, because a bounded DPDA stack is a resource
  policy and not the full unbounded JSON language.

If any of those are absent, the row is advisory/permissive and cannot ratify a
strict SOTA-beat claim.

## DPDA / CollapsedStage Admission

`CollapsedStage` is admissible only when all of these are true at the same time:

1. **Grammar shape.** The candidate rule cluster is byte-oriented,
   deterministic, and first/FOLLOW-disjoint enough to generate a finite
   control table without speculative parse. No `@error(recover)`, no
   parse-time host decode, no layout scope, no Pratt/host-action obligation in
   the collapsed region.

2. **Stack discipline.** The grammar has an explicit bounded stack model:
   open-token set, close-token set, pair validation, push/pop states,
   overflow behavior, and depth bound. JSON's `frames_buf[64]`/`open_buf[64]`
   is one instance, not the generic rule.

3. **Strict scalar reference.** A scalar/RD reference for the same grammar,
   output plane, resource bounds, and validation policy exists and is the
   executable spec for parity.

4. **Primitive parity.** Every SIMD/ASM primitive used by the kernel has
   checkasm parity against its scalar reference under strict mode.

5. **DPDA parity.** The composed collapsed-stage kernel has its own differential
   harness against the scalar/RD reference on valid corpora plus invalid
   probes. Primitive-level checkasm is not enough; the state machine and stack
   transitions need parity too.

6. **Kernel availability.** A grammar-class kernel/template is actually
   committed. Codegen may fill tables and constants, but the spec must not
   pretend arbitrary grammar DPDA codegen is solved until the parity harness
   proves that derivation.

7. **Plane match.** The row produces the same workload/output plane it is
   compared against: retained tape/DOM, SAX sink, generated typed direct output,
   or full semantic digest. SAX speed is not DOM speed; a permissive DOM row is
   not a strict typed-output row.

8. **Hardware and freshness.** The measurement is on matching x86_64
   AVX-512-class hardware with exact CPU feature metadata, same corpus,
   current sources, and current sidecar metadata.

Failing any condition must route to `OffsetTape`/`EventTape`/`SinkOnly` or to
an advisory research row, not to a degraded `CollapsedStage` close claim.

## Exact Spec Wording

### Replace the `SOTA-BEAT-DESIGN.md` Phase 4 gate wording

```markdown
Phase 4 (`CollapsedStage`) is not selected by CPUID alone. It is selected only
when the cost model proves the grammar cluster is collapsed-stage-admissible,
the target ISA has the exact primitive feature set required by the selected
kernel, strict validation is implemented in the collapsed path, primitive
checkasm parity is green, DPDA-level differential parity against the scalar/RD
reference is green, and a committed grammar-class kernel/template exists. If
any predicate fails, the compiler emits `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` and
falls back to the next eligible non-collapsed shape.
```

### Replace the broad generalization claim in §5.3

```markdown
The shared ASM spine is grammar-neutral only for grammars that satisfy the
`CollapsedStage` admission predicates. Per-grammar variation may be data
tables and macro composition for admitted DPDA-shaped grammars such as JSON
and possibly CSV. Recovery-bearing, layout-bearing, host-fn-bearing, or
Pratt-heavy grammars such as CSS L4, BBNF-self, Sheets, and math route through
`EagerTape`, `EventTape`, `OffsetTape`, or `SinkOnly`; they are not evidence
that arbitrary grammar DPDA codegen has been solved.
```

### Add to `BENCH.md` asmjson comparator text

```markdown
asmjson rows are advisory unless a strict same-plane row exists. The upstream
architecture accepts non-RFC whitespace and misses unescaped-control rejection
inside strings, so default asmjson DOM/SAX/SWAR/AVX-512 rows are
`strictness=permissive`, `parse_utf8=none`, `escape_complete=no`, and
`s_anchor_eligible=false`. A BBNF collapsed-stage row may compare against
strict S anchors only if its measured path validates UTF-8, escapes, controls,
numbers, delimiters, trailing content, close-token stack matching, and declared
depth overflow inside the measured parse scope.
```

### Add to row metadata schema

```markdown
Collapsed-stage rows must record:

- `automaton_class = dpda`
- `collapsed_stage_admissible = true|false`
- `collapsed_stage_rejection_reason`
- `grammar_cluster`
- `state_count`
- `class_count`
- `max_depth`
- `stack_policy`
- `strict_layout_mask`
- `invalid_body_mask`
- `dpda_parity_probe = pass|fail|missing`
- `primitive_checkasm = pass|fail|missing`
- `kernel_source = committed_template|generated_data_only|missing`
- `s_anchor_eligible`
```

Rows missing these fields are invalid for S-anchor selection.

### Tighten the "no contrivance" rule

```markdown
No benchmark may convert an advisory asmjson-inspired route into a close claim
by changing the language contract, output contract, corpus, schema source,
strictness plane, or hardware plane. No new BBNF directive or BIR variant may
be introduced solely to force `CollapsedStage`; the shape is derived from
existing grammar, target, cost, and host/API output facts. A JSON-only
collapsed-stage kernel is acceptable only if reported as JSON-only scope or
paired with a non-JSON admission result; it is not evidence of arbitrary
grammar generality by itself.
```

## Bottom Line

Keep the asmjson fold-back, but demote it to a conditional research/backend
shape until strict DPDA parity exists. The immediate SK-V6 spec should say:

- asmjson default rows are permissive/advisory;
- BBNF retained rows remain view-boundary unless byte-entry UTF-8 validation is
  timed;
- `CollapsedStage` is a DPDA, not an FSM;
- `CollapsedStage` needs grammar, strictness, stack, parity, kernel, hardware,
  and same-plane gates before release dispatch;
- any "past asmjson" number is successor-tranche x86 evidence, not an Apple
  Silicon strict SOTA close.
