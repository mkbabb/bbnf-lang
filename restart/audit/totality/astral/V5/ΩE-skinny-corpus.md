# Omega-E Skinny Corpus Alignment - Pass Omega V5 W5R

Pass: Pass Omega V5.
Date: 2026-05-26.
Scope: align `restart/skinny/{BENCH,COMPILER,HARDENING,INDEX,SUBSTRATE,WORKSPACE}.md`
with REDRESS-209 / W5R.

## Verdict

ACCEPT-WITH-LIMITED-PATCH.

W5R needs limited skinny corpus updates where the corpus says W5 owns a single
provider-collapse/delete wave or implies the generic provider already exists.
BENCH and SUBSTRATE are read/no-op because W5R does not change benchmark
semantics, substrate shape, or BackendShape. COMPILER needs only wording that
the grammar-neutral source-consuming runtime generator contract is W5A before
deletion.

## Surface Disposition

| Surface | V5 action |
|---|---|
| `restart/skinny/INDEX.md` | Add Pass Omega V5 W5R to active authority after V4 W4R; route W5A/W5B before W6. |
| `restart/skinny/WORKSPACE.md` | Update provider/template deletion ownership: W5A creates load-bearing grammar-neutral source-consuming generator; W5B deletes providers/templates. |
| `restart/skinny/HARDENING.md` | Add REDRESS-209 refusal: static centralization of CSS provider/template bodies is not Lock 14 closure. |
| `restart/skinny/COMPILER.md` | Limited wording alignment for grammar-neutral source-consuming runtime emission request before provider deletion, including JSON/Sheets/BBNF-self non-JSON proof. |
| `restart/skinny/BENCH.md` | Read/no-op. No benchmark gate changes in W5R. |
| `restart/skinny/SUBSTRATE.md` | Read/no-op. No substrate or BackendShape change. |

## Forward Addendum

The skinny hardening corpus should carry the W2R/W4R/W5R procedural lesson:
deletion waves must name the already-load-bearing rebuild capability and its
same-wave consumer before any provider/template/runtime source is removed.

## Supersession Correction

REDRESS-209 is historical evidence for the pre-W5R W5 shape. V5 CRUD may add a
narrow supersession note after G-Omega closes, but it must not erase the
rejection or rewrite REDRESS-209.
