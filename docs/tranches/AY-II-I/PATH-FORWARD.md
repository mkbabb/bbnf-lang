# AY-II — Path Forward (2026-04-24, amended 2026-04-25 post-B2 close)

AY-II is not on a parallel-infra path any more. The immediate execution
order is:

1. **B1 has closed.** The prelude annex landed at the 2026-04-24 W3
   close commit (see `docs/tranches/B1/FINAL.md`); substrate is pinned,
   divan harness is live, alias surface is rewritten, abrogation
   catalog is executed, sibling-repo triad is in sync.
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
3. **B4.W0 has closed (codegen `syn::parse2` emit-correctness).** The
   downstream emit defect B3 surfaced — the SIMD bitmap kernel emitting
   a token sequence `syn::parse2` rejected — landed a single-source
   emitter fix; the bbnf self-host regen now reaches `prettyplease`
   end-to-end without rejection.
4. **B2 has closed (build-time codegen transposition).** The
   `bbnf_derive` proc-macro IR-pipeline contract retired; `cargo xtask
   regen` is the canonical regen entrypoint; per-grammar source lives
   on disk under `crates/core/src/grammar/generated/<ident>.rs`;
   consumer crates `pub use ::bbnf::grammar::generated::<ident>::*` in
   place of `#[derive(Parser)]`; `crates/derive/` deleted outright
   (3 files / 457 lines); `BBNF_SCHEMA_VERSION` retired; the pre-B2
   80-min cold rustc-side IR-pipeline wall no longer exists; CI +
   pre-commit gate on `cargo xtask regen --check`. See
   `docs/tranches/B2/FINAL.md`.
5. **AY-II.W0' close ceremony resumes on the post-B2 substrate.** The
   ceremony shrinks to its compressed-honest form (~15 min per
   AUDIT-B): cycle-1 regen via `cargo xtask regen` (~5 min wall,
   dominated by xtask incremental compile) + invariant greps +
   `projection_totality.rs` test + close-status formalisation in
   `PROGRESS.md` + `waves/W0p.md`. Cycle-2 idempotency, the fat-LTO
   5-bench matrix, samply per primary grammar, and `nm` of bench
   binaries route to wave-specific close gates (W1.c JSON, W2 CSS,
   W3 Sheets, W4.e BBNF) where peer-parity context is meaningful.
   AY-II.W1-W5 sequencing operates on the post-B2 runtime regardless
   of subsequent polish.

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
- The pre-B2 monolithic `crates/core/src/grammar/generated.rs` retired
  at B2.W0.c; per-grammar source now lives under
  `crates/core/src/grammar/generated/<ident>.rs` (9 grammars; bbnf
  self-host at 34 048 lines), refreshed by `cargo xtask regen`.
- The W0'.a compose-boundary aliases and shim surfaces are still
  present by design until the W0' close ceremony's invariant audit
  retires them.
- AY-II.W0' is **not** formally closed yet — the compressed-honest
  close ceremony is now unblocked on the post-B2 substrate and is the
  immediate next step.

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

### 2. AY-II.W0' close ceremony (UNBLOCKED on post-B2 substrate — immediate next step)

The compressed-honest form per AUDIT-B is the operational spec on the
post-B2 substrate (~15 min, no 80-min bootstrap wall to fight):

1. **Cycle-1 regen** via `cargo xtask regen` (~5 min wall, dominated
   by xtask incremental compile; the IR pipeline itself runs in
   milliseconds per grammar).
2. **Invariant verification** — run the AY-II.W0' invariant grep
   suite (`pub struct ValueBuilder|ValueBuilderOutput`, `pub fn
   push_compound|mark_children`, `STRUCTURAL_SCAN_POLICY` reference
   count, `#[allow(dead_code)]` delta vs pre-W0').
3. **Projection-totality test** — `cargo test -p bbnf --test
   projection_totality --release` runtime-call-count assertion green.
4. **Retire the W0'.a compose-boundary aliases and shim surfaces** in
   line with the `<Grammar>Value::Unknown` retirement audit; record
   the per-grammar exception ledger.
5. **Close-status formalisation** — update `PROGRESS.md`, `AY-II.md`,
   and `waves/W0p.md` to mark W0' closed.

Cycle-2 idempotency, the fat-LTO 5-bench matrix, samply per primary
grammar, and `nm` on bench binaries route to wave-specific close
gates (W1.c JSON, W2 CSS, W3 Sheets, W4.e BBNF) where peer-parity
context is meaningful.

### 3. AY-II W1-W5

Dispatch only after W0' closes:

1. W1 — JSON semantic parity + peer-referenced performance.
2. W2 — CSS L4 typed-semantic parity.
3. W3 — Sheets typed semantics + performance.
4. W4 — BBNF self-hosting identity + grammar-meta typed semantics.
5. W5 — close matrix + FINAL + successor handoff.

No annex, no sidecar wave, and no infra detour runs in parallel with
these waves.

## Immediate cleanup targets — all retired

The alias surface enumerated below retired entirely at B4.W1 (the
W0' close-ceremony fold) and B5.W1 (substrate restoration). The
list survives as archaeology; every entry below carries `closed`
in the post-B5 substrate. Per `audit/W0p-PAUSE-SNAPSHOT.md`
§Transient compose-escape aliases, the SNAPSHOT carries the full
alias-kind enumeration; the post-B5 substrate exposes none of
those aliases. The surface today is `Tape<R>` over `Columns`
plus `Parsed<'p, R>` 3-field record, with `Tape::position()` and
`Tape::rollback_to(open)` as the parser-substrate boundary.

- `crates/tape/src/builder/` — directory deleted at B5.W1; no
  builder module remains in the tape crate.
- `crates/core/src/runtime/mod.rs` — every value-builder alias
  and shim module retired by B4.W1 alias retirement; the
  `value_builder` shim module ceases to exist.
- `crates/core/src/runtime/parsed.rs` — `Parsed<'p, R>` returns
  to a 3-field record (`tape`, `input`, `root_offset`) at
  B5.W1; multi-arg `new_fused` bridges retire.
- `crates/core/tests/value_api_apples_to_apples.rs` — counter
  imports route off the substrate's canonical accessor surface
  (`Tape::frame`, `Tape::payload_for`, etc., un-prefixed
  per B5.W1).

## Discipline

- One codepath.
- No parallel B1/AY-II execution.
- No pre-regen assumptions treated as runtime truth.
- No stale docs left active once superseded.
- No quick solutions, no workarounds.
