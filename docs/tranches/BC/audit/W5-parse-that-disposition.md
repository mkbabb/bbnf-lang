# BC.W5 — `parse-that` Disposition Decision

Date: 2026-05-03
Status: settled. Closes Phase-4 spec-depth gap I and Lock 11 parse-that omission per L11 `audit/HARDENING-PLAN-2026-05-03-01-lock-adherence.md:20`.

## §1 Decision

**Option (i) — `parse-that` is a permanent private path-dep. Never published.**

`parse-that` carries grammar-coupling at multiple layers:

1. **`parse-that/rust/parse-that/`**: the BBNF self-host substrate. Its combinator surface (`seq`, `alt`, `pratt_with`, `repeat_with_separator`, etc.) was tuned against the BBNF self-host grammar's specific recursion / left-factor / Pratt patterns. Decoupling it for general publication means decoupling combinator API from BBNF's specific needs — a project of its own scope.
2. **`parse-that/rust/regex/`** (renamed to `parse-that/rust/bbnf-regex/` per BC.W5b): grammar-derived regex DFA synthesis. Its NFA-to-DFA pipeline assumes the bbnf-lang grammar dialect's character class semantics. Decoupling for general publication is feasible at later stage but distinct from the parent workspace's release.
3. **`parse-that/rust/pprint/`**: bbnf-grammar-author-targeted pretty-printer. Its formatting rules are bound to bbnf grammar shape.

**Conclusion**: the `parse-that` *workspace* remains a private dependency of bbnf-lang. Individual sister sub-crates within `parse-that/rust/` may publish independently — `bbnf-regex` is the first publication candidate at BC.W5 — but `parse-that` itself never becomes a published crate.

## §2 Options surveyed (and rejected)

### Option (ii) — future publication candidate at BD or post-BD

**Rejected.** The bbnf-lang use-case never requires `parse-that` to be a public dependency. Downstream bbnf consumers depend on `bbnf-parse` (which compiles grammars to source via xtask regen, embedding the parse-that combinator surface at compile time), not on `parse-that` directly. Publishing `parse-that` would create a public surface that must be semver-stabilised across grammar evolution, but the combinator API evolves with bbnf-grammar features. The friction-cost of stable-API semver compatibility outweighs the marginal benefit of public availability.

### Option (iii) — stabilised + frozen + published in BC.W5 alongside other sister crates

**Rejected.** The freeze cost is substantial: every combinator's public type signature must be locked. `parse-that` does not yet have a stable public surface; BBNF self-host parser has been the only consumer, and the surface evolves as new BBNF features land (per `project_grammar_authoritative_status` Phase 3 host fns). Freezing now means freezing a surface that has not been pressure-tested against multiple grammar dialects.

## §3 API-freeze checklist for the chosen option (i)

Permanent private path-dep means:
- `parse-that/Cargo.toml` `[package].publish = false` for the workspace root and for `parse-that/rust/parse-that/` (the combinator crate)
- `parse-that/rust/bbnf-regex/Cargo.toml` `[package].publish = ["crates-io"]` (publication candidate; freeze at BC.W5)
- `parse-that/rust/pprint/Cargo.toml` `[package].publish = false` (private; bbnf-grammar-targeted)
- `bbnf-lang/.cargo/config.toml` `[patch.crates-io]` retains the `parse-that` patch entry indefinitely; downstream consumers of bbnf-lang inherit the patch transitively or inline it in their own `Cargo.toml` (the cookbook documents both)
- Workspace dependency table in `bbnf-lang/Cargo.toml` `[workspace.dependencies]` retains `parse-that = { path = "../parse-that/rust/parse-that" }` (or absolute path during dev)

## §4 Semver impact

| Surface | Pre-W5 | Post-W5 | Semver discipline |
|---|---|---|---|
| `parse-that` (workspace root) | path-dep, no publication | path-dep, no publication | not applicable (no public release) |
| `parse-that/rust/parse-that/` (combinator crate) | path-dep | path-dep | not applicable |
| `parse-that/rust/regex/` | path-dep | renamed to `parse-that/rust/bbnf-regex/` (W5b); path-dep + publication candidate | `bbnf-regex` follows semver from 0.x onwards; publication post-BC.W5 |
| `parse-that/rust/pprint/` | path-dep | path-dep | not applicable |

The user-facing semver surface is therefore unchanged for `parse-that` itself; downstream bbnf-lang consumers are unaffected by `parse-that`'s never-publish posture because they consume `bbnf-parse` (which incorporates `parse-that` at compile time through the xtask-emitted source).

## §5 Downstream user friction

| Friction point | Severity | Mitigation |
|---|---|---|
| Consumer cannot `cargo add parse-that` from crates.io | None — by design | The bbnf-parse crate is the public entry; `parse-that` is internal to bbnf-lang's compile chain |
| Consumer wants to use bbnf's combinator surface for their own parser | Medium — out of bbnf scope | Recommend chumsky / pest / lalrpop as published alternatives; bbnf-lang's combinator surface is BBNF-specific; if the user really wants the bbnf combinators, they vendor `parse-that` as a path-dep just like bbnf-lang does |
| Consumer wants to fork `parse-that` for their own use | Low | The repo is public on GitHub at `parse-that/`; fork-and-vendor is the standard pattern |
| Patch-file maintenance burden for downstream | Low | The patch entry in `.cargo/config.toml` is a one-time setup; once vendored, no further work; downstream-of-downstream consumers transitively inherit the patch |

## §6 Cross-references

| Reference | Description |
|---|---|
| `audit/HARDENING-PLAN-2026-05-03-01-lock-adherence.md:20` (L11) | The lock-adherence audit caught `parse-that` omission from BC.W5's freeze list; this document closes the omission via option (i) |
| `audit/HARDENING-PLAN-2026-05-03-08-carry-deferral.md:21` (D08-12) | The carry-deferral audit calls for `parse-that` inclusion in BB→BC.C4; option (i) ratifies the carry's posture (parse-that is path-dep into perpetuity; the carry is structural, not publication-bound) |
| `audit/MODULES-2026-05-03.md:43` | The crate name `bbnf-regex` is canonical; the rename per BC.W5b achieves crate-name canonicality |
| `feedback_csp_always_optimize` | csc411 sibling discipline; analogous private-sibling pattern for csp-solver |
| `feedback_general_infra_crates` | General-purpose constructs in own crates; parse-that is grammar-coupled, not general-purpose; the feedback ratifies private status |

## §7 Closing

The decision is settled. `parse-that` is a permanent private path-dep. Its sister sub-crates (`bbnf-regex` at W5b, others later) may publish independently. The user-facing bbnf-lang surface is unaffected. BD does not inherit a `parse-that` publication carry; BC→BD.C2 names the publication candidates as the BC.W5-frozen sister crates only (egraph, egraph-derive, csp-solver, bbnf-regex), explicitly excluding `parse-that` itself.
