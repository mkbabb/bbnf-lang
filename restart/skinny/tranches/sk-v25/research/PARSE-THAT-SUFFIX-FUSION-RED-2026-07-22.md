# parse-that retained-value suffix fusion — RED receipt

Date: 2026-07-22 EDT  
Authority: bounded read-only consumer assay; zero implementation, package,
acceptance, or Value authority.

## Verdict

**RED: no fully hardened, published, or explicitly hand-backable parse-that
artifact supplies static retained-left sequencing/map fusion.** Consumers must
remain on published `@mkbabb/parse-that@1.0.0`; no private T/U bytes are
consumable.

The direct grammar shape
`consumeNumber.skip(string("%"))` followed by its semantic map is a canonical
prospective beneficiary of a future grammar-general
retained-left + terminal-suffix + projection lowering. Such a lowering must
reuse the numeric parser owner, avoid reparsing or regex duplication, preserve
furthest-progress diagnostics and state semantics, and prove a performance win.
No current artifact proves that closure.

## Published authority

| Operand | Identity |
| --- | --- |
| tag `v1.0.0` commit | `7eab78c89961001a689952c091fdbbf64af735da` |
| `typescript/package.json` | SHA-256 `f53ca9b85c43e65c1f3f8a21414f990211d8d0e6a7f07df805557cdb6b50f4ff` |
| `typescript/src/parse/parser.ts` | SHA-256 `16978b12a0674a218e4af041606232d0d81fc57f3f6d7bdb29149173b4fe3c50` |

Published `Parser.skip` is already one dedicated closure: it invokes the left
parser, retains its value, invokes the suffix parser, and restores the retained
value on success. It still dispatches both child parser functions. It has no
static terminal-suffix specialization and does not provide generic
sequencing/map fusion. `all`/`any` fusion does not remove this wrapper. The
former speculative `thenMap` and `fuse` seams were deleted after zero consumers;
they are not an adoption route.

## Independent formation evidence

Current parse-that HEAD observed read-only:
`ef10d5b78236c4a30a7bb22a6113b60bdc4bdf42`.

| Assay | SHA-256 | Judgment |
| --- | --- | --- |
| `docs/tranches/T/TRANSACTION-ASSAY.md` | `1759fa01dba04b63261b34739621d3027b3a4861b9d52972ff41aedd0f7baa31` | RED; no consumable transaction uplift |
| `docs/tranches/T/G15-TERMINAL-PROJECTION-ASSAY.md` | `9bd8c3ceacddc448d043dbf7fe1611f71ef6ecc4989fb87edfff0d9296db0b6a` | RED; no consumable generic terminal projection |

Those T/U materials are formation/private evidence, not a package or handback.
Their existence grants no consumer adoption authority.

## Adoption boundary

The percentage composition is expected to benefit only if a future uplift is
grammar-general and proves all of the following together:

1. one authoritative numeric child parser, with no duplicated terminal regex;
2. identical success value, offset, failure, and furthest-progress behavior;
3. complete predecessor-value transaction semantics on failed composition;
4. no parser-ID growth, CSS-specific scanner, unsafe type abuse, or hidden
   fallback;
5. a fresh local grammar × engine A/B replay that beats the direct published
   composition under the pinned Apple-ARM benchmark contract.

Until then, the exact direct `.skip(string("%"))` composition is the correct
architectural control, not evidence of an available engine uplift.
