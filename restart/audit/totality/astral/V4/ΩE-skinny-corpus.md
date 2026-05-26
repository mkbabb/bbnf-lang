# Omega-E Skinny Corpus Alignment - Pass Omega V4 W4R

Pass: Pass Omega V4.
Date: 2026-05-26.
Scope: align `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md`
with REDRESS-184 / W4R.

## Verdict

ACCEPT-WITH-LIMITED-PATCH.

W4R needs limited skinny corpus updates where the corpus names CSS provider
deletion or generated-provider manifest timing. BENCH, COMPILER, and SUBSTRATE
are otherwise read/no-op because W4R does not change benchmark semantics,
compiler IR, or substrate shape.

## Surface Disposition

| Surface | V4 action |
|---|---|
| `restart/skinny/INDEX.md` | Add Pass Omega V4 W4R to the active authority list after V3 W2R. |
| `restart/skinny/WORKSPACE.md` | Update generated-provider manifest wording: CSS provider/template deletion is W5 after W4 ledger prune, not W4 itself. |
| `restart/skinny/HARDENING.md` | Add REDRESS-184 hardening refusal: provider deletion before replacement generator is a sequencing fault. |
| `restart/skinny/BENCH.md` | Read/no-op unless wording says W4 deletes providers. |
| `restart/skinny/COMPILER.md` | Read/no-op; W5 still owns provider collapse. |
| `restart/skinny/SUBSTRATE.md` | Read/no-op; no substrate or BackendShape change. |

## Path Correction

Where SK-V14 documents cite `skinny/ROLLING-SOTA-DELTA.md`, the active file is
`restart/skinny/ROLLING-SOTA-DELTA.md`. W4R CRUD should correct active
dispatch surfaces that route W4 ledger work.

## Supersession Correction

REDRESS-183 remains historically true for the pre-W2R W2 shape, but its
"W2 rejection blocks W3/W4/W5/W6/W7" sentence is superseded by amended W2
admission at `45568e669` and W3 admission at `b0a864f0b`. The current block is
REDRESS-184. V4 CRUD should add a narrow supersession note rather than rewrite
the historical rejection.
