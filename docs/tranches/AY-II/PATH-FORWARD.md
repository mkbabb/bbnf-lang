# AY-II — Path Forward (2026-04-24, amended 2026-04-25 post-B3)

AY-II is not on a parallel-infra path any more. The immediate execution
order is:

1. **B1 has closed.** The prelude annex landed at the W3 close commit
   (see `docs/tranches/B1/FINAL.md`); substrate is pinned, divan
   harness is live, alias surface is rewritten, abrogation catalog is
   executed, sibling-repo triad is in sync. AY-II.W0' close ceremony
   was unblocked on this baseline.
2. **B3 has closed (parser-baseline restoration).** The runtime parser
   regression originally attributed to AY-II.W0' source landings was
   traced to a latent contract violation between `derive_frame_depth`'s
   reverse-walk algorithm and the Pratt shape's pre-order emission;
   resolved at B3.W0 via five forward architectural fixes (γ retire
   `derive_frame_depth`; δ atomic depth rollback in `Columns`; ε
   cycle-safe cursor walk; ζ widened `end_compound_post_order` bump
   scope; η Pratt operand seeding + lowering cousin-leak guard). No
   W0' source landings were reverted. See
   `docs/tranches/B3/FINAL.md`.
3. **B2 W0.c re-execution unblocks** against the post-B3 substrate;
   `cargo xtask regen --grammar bbnf` no longer hangs in
   `BbnfBootstrap::parse`. The bbnf self-host regen surfaces a
   separate downstream `syn::parse2` codegen-emission defect that
   opens B4.
4. **AY-II.W0' close ceremony shifts to B4 close** on the post-B2
   substrate. The compressed-honest 15-min ceremony per AUDIT-B
   remains the operational spec; it executes against the post-B4
   substrate (where the codegen `syn::parse2` defect is fixed).
   AY-II.W1-W5 sequencing operates on whatever runtime B4 produces,
   regardless of how B4's emit-correctness work unwinds.

Anything else reintroduces the same ambiguity B1 existed to delete.

## Current truth

- B1 closed at the 2026-04-24 W3 commit. Master HEAD carries the
  pinned `nightly-2026-04-11`, the four-exclude `iter-check` alias
  surface with per-exclude fast-paths, the four-profile nextest config,
  the simplified Makefile (~210 lines), the divan bench harness (19+1
  ports landed; `bencher` purged), the rewired `ci.yml` plus
  `bench-iai.yml`, the executed abrogation catalog (5 DELETE + 1
  REWRITE + 13 KEEP-AS-IS), and the cross-repo pin triad
  (`../parse-that` + `../pprint`).
- W0'.a / W0'.b / W0'.c / W0'.d1 / W0'.d3 / W0'.d4-d7 source landings
  are in.
- `generated.rs` is still pre-regen with the bridge-era parse entry, so
  AY-II.W0' is **not** formally closed yet — the close ceremony is now
  unblocked and is the immediate next step.
- The W0'.a compose-boundary aliases and shim surfaces are still
  present by design until the post-B1 regen replaces them.

## Ordered work

### 1. B1 prelude annex (closed 2026-04-24)

B1 closes at `docs/tranches/B1/FINAL.md`. Close artefacts:

- [B1.md](/Users/mkbabb/Programming/bbnf-lang/docs/tranches/B1/B1.md) — plan
- [FINAL.md](/Users/mkbabb/Programming/bbnf-lang/docs/tranches/B1/FINAL.md) — close report (invariant + hard-gate + commit + handoff tables)
- [PROGRESS.md](/Users/mkbabb/Programming/bbnf-lang/docs/tranches/B1/PROGRESS.md) — execution ledger
- [post-B1.json](/Users/mkbabb/Programming/bbnf-lang/docs/benchmarks/post-B1.json) — aggregate proof matrix

Close conditions B1 established before AY-II resumed:

1. The routine command surface is truthful and documented as-built
   under the pinned nightly.
2. `bootstrap-bbnf`, `ay-prepare-profile-wave`, `ay-samply-*`, and
   `ay-bench-close` are all named in the public command surface;
   first-run wall-clocks under the divan harness land at AY-II.W0'
   close ceremony.
3. The stale workflow comments/docs inherited from B0 + AY-II infra
   churn are deleted (5 ABROGATEs landed; `profile.sh` +
   `cost-grid-sweep.sh` + `check-cst-invariants.sh` +
   `verify-w2-asm.sh` + `verify-w2-symbols.sh`).
4. AY-II docs are normalized so B1 is predecessor, not sidecar.

### 2. AY-II.W0' close ceremony (UNBLOCKED — immediate next step)

Finish W0' in one uninterrupted sequence:

1. Run bootstrap regen.
2. Run double-regen idempotency.
3. Retire the W0'.a compose-boundary aliases and shim surfaces.
4. Capture fresh expands for JSON, CSS, Sheets, and BBNF.
5. Run the fat-LTO 5-bench matrix.
6. Capture samply on the four primary grammars.
7. Run `nm` on the bench binaries.
8. Verify `<Grammar>Value::Unknown` retirement per grammar per
   `AY-II.md` §Plan-audit findings bullet 2; record the
   per-grammar exception ledger (`audit/W0p-PAUSE-SNAPSHOT.md:87-89`
   marked this unresolved at pause).
9. Update `PROGRESS.md`, `AY-II.md`, and `waves/W0p.md` to mark
   W0' closed.

The W0' close gate remains the one declared in
[W0p.md](/Users/mkbabb/Programming/bbnf-lang/docs/tranches/AY-II/waves/W0p.md).

### 3. AY-II W1-W5

Dispatch only after W0' closes:

1. W1 — JSON semantic parity + peer-referenced performance.
2. W2 — CSS L4 typed-semantic parity.
3. W3 — Sheets typed semantics + performance.
4. W4 — BBNF self-hosting identity + grammar-meta typed semantics.
5. W5 — close matrix + FINAL + successor handoff.

No annex, no sidecar wave, and no infra detour runs in parallel with
these waves.

## Immediate cleanup targets already identified

These are not optional polish items; they are W0' close work.
Canonical list; if in doubt against `audit/W0p-PAUSE-SNAPSHOT.md`
§Transient compose-escape aliases, the SNAPSHOT is the
source of truth for alias-kind enumeration.

- `crates/tape/src/builder/mod.rs` — retire `pub type TapeBuilder = FusedBuilder;`
- `crates/core/src/runtime/mod.rs` — retire `ValueBuilderOutput` alias
  and the `value_builder` shim module
- `crates/core/src/runtime/mod.rs` — retire `_ValueBuilderShim` /
  `ValueBuilder<R>` ZST once the counter imports in
  `value_api_apples_to_apples.rs` migrate to the fused surface
- `crates/core/src/runtime/parsed.rs` — retire the 4-arg `new_fused`
  bridge once regen no longer emits it
- `crates/core/tests/value_api_apples_to_apples.rs` — move the builder
  counter imports onto the fused builder surface

## Discipline

- One codepath.
- No parallel B1/AY-II execution.
- No pre-regen assumptions treated as runtime truth.
- No stale docs left active once superseded.
- No quick solutions, no workarounds.
