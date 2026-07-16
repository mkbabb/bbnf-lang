# PROTO-1 — LOCK-6 pass-3 probe 1: 6B TS `emit_classify` (SPEC-GENERATED-SKELETON)

**The isomorphism plane (rust/ts split). Build the crown, measure gen-vs-hand +
V8 monomorphism on the tape face.**

- **HEAD measured on:** `ce51c36fcbdf5108eb028005586672d9a6d2ca1b` (BASE-PIN).
  Worktree started at `d2b32f6e3` (stale — did NOT descend from the pin);
  `git merge --ff-only ce51c36fc` succeeded, HEAD now == pin. Numbers are
  on-artifact (no wrong-artifact-risk).
- **loadavg during run:** ~11.9 / 10.1 / 9.7 (busy host). Every PASS criterion is
  **deterministic** (byte-diff, V8 boolean natives, u32 fingerprint) — none is a
  timing measurement — so host load does not compromise the verdict.
- **node:** v26.0.0. **rustfmt:** 1.9.0-nightly.

---

## Question

Does ONE data-driven machine `emit_classify(spec, Backend)` — consuming a single
backend-agnostic `LayoutSpec` — emit BOTH the Rust classify face and the TS
classify face, **byte-identical** to the hand code, while the generated TS
**preserves the hand V8 monomorphism** (the single `TapeDoc` hidden class the
4.4x stop-early depends on)?

If generated TS trips a hidden-class transition (`%HaveSameMap` false),
generation lost the monomorphism → **REFUTES the crown (R3)**. This one probe
discharges the 6A/6B partition number and the AoS↔SoA expressibility item.

## Method

Live faces at skinny@`ce51c36fc`:
- JSON `FromSource`: `runtime/src/grammars/json/value.rs` — `JsonNodeKind::at_cursor`
  (reads `source[offset]`, switches byte→kind).
- Sheets `FromFlagTag`: `runtime/src/grammars/sheets/view.rs` — `GoogleSheetsKind`
  + `KIND_TABLE` + `at_cursor` (reads sparse flag tag, indexes `KIND_TABLE[tag-1]`).
- Hand TS classify (PROVE-14): `bbnf-bench/ts-tape/tape.mjs` (located in worktree
  `wf_386900ed-462-14`; not at HEAD) — `TapeDoc.classify` + `K_*` consts.

**The prototype** (`emit_classify.rs`, standalone `rustc` single file — parsimony,
no workspace/target touch):

1. `LayoutSpec` as DATA:
   `discriminator: FromSource{arms, rust_default} | FromFlagTag{table, zero_is_none}`;
   `flag: EscapeBits{sym,value,comment} | DiscriminatorTag`; `kinds` (enum members
   with stable u16 discriminants + optional TS symbol). The discriminator STRUCTURE
   (ordered arms, byte ranges, default) is truly shared; per-backend surface tokens
   (the `K_OBJ_OPEN` naming, doc comments) are carried as data — exactly what a
   codegen naming layer owns.
2. `emit_classify(spec, Backend::Rust)` and `emit_classify(spec, Backend::Ts)` —
   ONE machine, two arms, from the spec. **No second `include_str!` TS template.**
3. Byte-match generated Rust vs hand (rustfmt both; `diff`).
4. Byte-match generated TS classify + consts vs hand `tape.mjs` slices (`diff`).
5. Splice generated TS fragments into the hand decoder harness → `gen_tape.mjs`;
   `node --allow-natives-syntax` checks `%HaveSameMap(a,b) && %HasFastProperties`
   across two decoder instances AND after one `walk()`.
6. One-line spec edit (add a kind), recompile+regen, confirm both Sheets faces move
   while JSON PROVE-14 fp `986912547` (twitter.json) holds.

## What ran, and the numbers

**(1) Rust byte-match — 0-diff, both dialects** (after rustfmt):
```
JSON   at_cursor + enum        -> JSON-RUST:   0-DIFF
Sheets enum + KIND_TABLE + at_cursor -> SHEETS-RUST: 0-DIFF
```

**(2) TS byte-match — 0-diff** (raw `diff`, no formatter for JS):
```
consts (K_* + STRING_NEEDS_DECODE)  -> TS-CONSTS:   0-DIFF
classify(cursor) switch method      -> TS-CLASSIFY: 0-DIFF
```
(A real generation bug was found and fixed in-probe: the shared arm list places
`Number` at arm position 8, but JS `default:` must render LAST — the machine now
defers the single default arm to the end of the switch. Post-fix: 0-diff.)

**(3) V8 monomorphism on the GENERATED decoder** (`gen_tape.mjs` = hand harness
with generated const+classify fragments spliced in; splice is 0-diff vs hand,
i.e. generation reproduces the monomorphic hand code exactly):
```
[pre-walk]  HaveSameMap(a,b)=true  HasFastProperties(a)=true  (b)=true
[walk fp]   2561419173   (synthetic {"a":[1,true,null,"x"]}; classify+walk ran)
[post-walk] HaveSameMap(a,b)=true  HasFastProperties(a)=true  (b)=true
MONO-GATE: PASS (single hidden class, fast props, preserved across walk)
```

**(4) One-line spec edit → both faces move, JSON invariant holds.**
Added `"NamedRange"` (1 data line) to the Sheets kind list; recompile + regen:
```
gen_sheets.rs  MOVED:  +  NamedRange,                 (enum)
                       +  GoogleSheetsKind::NamedRange, (KIND_TABLE)   [2 sites]
gen_sheets.ts  MOVED:  +  const K_NAMED_RANGE = 40;                   [1 site]
gen_json.rs / gen_json.ts:  UNCHANGED (invariant)
PROVE-14 fp (walkJs over twitter.json): 986912547  MATCH  (pre AND post edit)
post-edit gen_tape.mjs splice: STILL 0-DIFF vs hand -> tape-lane walk unchanged
```
The JSON classify byte is untouched, so the tape-lane walk fingerprint is
`986912547` by construction; one data edit drove 3 emit sites across 2 backends.

**LOC — gen-vs-hand on the classify tape face (both sides, post-generation):**
| face | hand LOC (was) | hand-authored now |
|---|---|---|
| TS classify face (consts 12 + classify 15) | 27 | **0** (generated) |
| Rust JSON (enum 17 + at_cursor 20) | 37 | **0** (generated) |
| Rust Sheets (doc+enum+KIND_TABLE 89 + at_cursor 12) | 101 | **0** (generated) |
| **classify-face total** | **165** | **0** |

Replaced by ONE 375-line machine: ~289 lines of shared rendering logic + ~86
lines of spec DATA (both grammars). Per-grammar spec data ≈ 40–45 lines; adding a
kind = 1 data line. NB: the brief's "TS ~703-hand → ~0" is broader than the
classify face — the full hand `ts-tape` decoder surface measures **625** LOC
(tape.mjs 364 + load/child/bench/pointer/verify). This probe generates the
**classify FACE** specifically (27 TS LOC → 0) — the R3-critical monomorphic
piece; the walk/leaf/nav body is out of this probe's parsimonious scope.

## Verdict

**PASS — RAN_MEASURED. The crown holds; R3 is NOT refuted.**

All four gates hold: (a) Rust byte-match 0-diff both dialects; (b) TS byte-match
0-diff; (c) `%HaveSameMap && %HasFastProperties` true both pre- and post-walk on
the generated decoder; (d) one-line spec edit regenerates both faces with fp
`986912547` unchanged. Generation did **not** lose the hand monomorphism — the
generated classify face is byte-for-byte the hand face, so the single `TapeDoc`
hidden class (and the stop-early it enables) is preserved by construction. The
6A/6B partition and the AoS↔SoA expressibility item are discharged: one machine
expresses both the `FromSource` (SoA byte re-derive) and `FromFlagTag` (tag-table)
discriminators, and both Rust and TS backends, from shared data.

### Caveats (honest)
- Per-backend surface tokens (the `Object`→`OBJ` abbreviation in `K_OBJ_OPEN`,
  doc-comment text) are carried as spec DATA, not derived by convention. This is
  the correct home (a naming layer), but it means "backend-agnostic" = shared
  discriminator STRUCTURE + per-backend surface strings, not a single symbol set.
- The prototype is a standalone `rustc` binary, not yet wired into
  `codegen/runtime_generator.rs` + xtask regen. Byte-identity to the live hand
  faces at `ce51c36fc` is proven; landing the machine into the emit spine (and
  deleting the hand faces) is the follow-on tranche step.
- The monomorphism check uses a synthetic 9-token tape (all kinds exercised); it
  proves classify+walk run with a stable hidden class. The twitter.json fp
  (`986912547`) is anchored via the classify-independent `walkJs` lane and holds
  by classify byte-identity — a full real-tape `doc.walk` run needs the binary
  SoA dump (tape_dump.rs), which was not rebuilt (out of the 50-min parsimony cap).
