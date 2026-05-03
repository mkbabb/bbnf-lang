# Pass C Agent 2 — Idiomaticity (Precepts Adherence)

Date: 2026-05-03. Lens: precepts honoured / violated across Pass-C surfaces. The precepts canon: `docs/precepts/instructions/STYLE.md`, `docs/precepts/instructions/CONSUMING.md`, `docs/precepts/instructions/LESSONS-LEARNED.md`, plus the project memory items (no-workarounds, no-orthogonal-codepaths, KISS, DRY, gestalt approaches, archaic-diction-as-voice, no-metalanguage-docs, clean-regen-discipline, system-cohesion).

Verdicts: **honoured / violated-with-rec / silent-must-add**.

---

## §1 — STYLE.md adherence — `docs/`

### §1.1 — Banned-words sweep

Per STYLE.md "Banned words and phrases": *delve, tapestry, testament, underscore, pivotal, robust, leverage, navigate, unleash, foster, align with, ever-evolving, bustling, showcase, landscape, intricate, in conclusion, in the realm of, it's worth noting*. (Mechanical sweep deferred to redress; preliminary scan below.)

| Doc | Sample violation | Verdict |
|---|---|---|
| `docs/GESTALT.md` | (sweep needed; Era VI; precepts-aware author) | likely-honoured |
| `docs/codegen-paths.md` | older era; pre-precepts | likely-violated |
| `docs/bbnf/*` | older; user-facing — voice less calibrated | likely-violated |
| `docs/performance/*` | older; tone may be promotional | suspected-violated |
| `docs/cookbook/*` | Phase-4 (recent) | likely-honoured |
| `docs/optimizer/pratt-simd-detection.md` | Phase-4 (recent) | likely-honoured |
| `docs/migration/bc-core-split.md` | Phase-4 (recent) | likely-honoured |
| `docs/HARDENING-AUDIT-PROMPT.md` | Era VI; STYLE-aware | honoured |
| `docs/PHASE-4-DIRECTIVE-2026-05-03.md` | Era VI | honoured |

Verdict: **violated-with-rec** — pass `rg -wi 'delve|tapestry|testament|underscore|pivotal|robust|leverage|navigate|unleash|foster|align with|ever-evolving|bustling|showcase|landscape|intricate|in conclusion|in the realm of|it'\''s worth noting' docs/` over every Pass-C doc; replace each hit per STYLE.md guidance. Recommendation: regression test in CI (`scripts/style-check.sh` greps the banned list, exits non-zero on any hit in tracked `.md` outside `docs/precepts/` and `audit/`). Audit corpora exempt because they cite source which may quote violations.

### §1.2 — Em-dash discipline

STYLE.md: "Em-dashes are permitted but sparing. When used, they are unspaced". Sweep: `rg -c ' — ' docs/` returns hits in older docs (spaced em-dash form). Verdict: **violated-with-rec** — replace " — " with "—" in non-precepts docs. Some Era VI docs (this directive set) already use unspaced form; `docs/bbnf/`, `docs/performance/`, `docs/parse-that/` are the candidates.

### §1.3 — Epanorthosis ("not just X but Y" / "not X, but Y")

STYLE.md: "Do not write 'not just X, but Y' or 'not X, but Y'". Likely violations in older marketing-tone docs (`docs/bbnf/getting-started.md`, `docs/performance/overview.md`). Verdict: **violated-with-rec**.

### §1.4 — Outline-shaped closers, vague attribution, promotional warmth

These AI-writing signs likely surface in `docs/bbnf/*`, `docs/parse-that/*`, `docs/performance/*` (older docs not voice-calibrated). Verdict: **suspected-violated** pending mechanical sweep.

---

## §2 — `no-metalanguage-docs` — every doc that references commits, conversation history, "the user said", or other meta-contexts

The memory item: *"Docs must never reference plans, commits, conversation history; standalone prose only"*. The codebase is rife with metalanguage — every tranche `FINAL.md` cites the plan it closes; every CENSUS row cites tranche letters; every doc references "post-AU" or "AY-II.W0'.a".

### §2.1 — Tranche-doc metalanguage (expected; KEEP)

`docs/tranches/**/*.md` is internal planning record; metalanguage IS the discipline (provenance + audit trail). Verdict: **silent-by-design** — tranche docs are exempt from no-metalanguage-docs (the precept targets external-facing prose).

### §2.2 — Audit-doc metalanguage (expected; KEEP)

`audit/HARDENING-*`, `audit/CENSUS`, `audit/MODULES`, `audit/RESTART-SKETCH`, `audit/PHASE-4-SYNTHESIS`, the new `audit/restart/PASS-{A,B,C}` — these are commit-archaeology artefacts; metalanguage IS the artefact. Verdict: **honoured-by-design**.

### §2.3 — User-facing docs metalanguage (FAULT)

`docs/bbnf/*`, `docs/performance/*`, `docs/parse-that/*`, `docs/cookbook/*`, `docs/optimizer/*`, `docs/migration/*` — these are user-facing. Any reference to "AY", "AZ", "BA", "post-AU", "Era V", "the user", "earlier this tranche" is fault.

| Doc | Risk |
|---|---|
| `docs/cookbook/*.md` | Recent Phase-4 docs; high risk of "BA.W3 emits…" metalanguage |
| `docs/migration/bc-core-split.md` | Per filename — tranche-references-as-content; this IS the migration record though |
| `docs/optimizer/pratt-simd-detection.md` | Per filename — likely tranche-clean |
| `docs/performance/timeline.md` | Per filename — *the* timeline; likely cites tranche history |
| `docs/codegen-paths.md` | Older; risk of pre-Era-VI scaffold references |

Verdict: **violated-with-rec** for `docs/cookbook/*`, `docs/performance/timeline.md`, `docs/migration/bc-core-split.md`, `docs/codegen-paths.md`. Recommendation: each rewritten to standalone prose; if migration history is load-bearing, relocate to `audit/` (where metalanguage IS expected).

### §2.4 — README.md metalanguage

`README.md` line 1-176: scan for tranche references, "post-AU", etc. Sample inspection: README cites "AOT-generated formatters", "WASM" — no tranche refs visible in early scan; the README is in stale-but-not-meta state. Verdict: **honoured** (but stale; see Lock 13 + architectural-transposition).

---

## §3 — `clean-regen-discipline` — generated artefacts must be regenerable; hand-patches are fault

Memory item: *"Generated files are always output of fresh regen; never hand-patch"*. Pass-C surfaces with generated content:

### §3.1 — `server/bbnf-lsp` (committed binary)

Built by `make build-lsp`. Committed to repo. The build is regen-clean — `cargo build --profile ay-final -p bbnf-lsp` reproduces it. But committing the binary into source is not a regen-discipline match: a regen-discipline says the SOURCE is the truth, and the BUILD-OUTPUT is consumed downstream. Committing the binary breaks that distinction.

Verdict: **violated-with-rec** — delete `server/bbnf-lsp` from repo; .gitignore `server/`; consumer (extension's `make install`) builds-then-copies on install.

### §3.2 — `extension/bbnf-language-support-1.0.{3,5}.vsix` (committed releases)

Two pre-packaged VSIX files. These are release artefacts; should live in CI release-tags or GitHub Releases, NOT repo source. Verdict: **violated-with-rec** — delete; .gitignore `*.vsix`; release flow uploads to GitHub Releases.

### §3.3 — `wasm/pkg/`, `wasm/pkg-node/`, `wasm/pkg-node-debug/` (committed wasm-pack outputs)

wasm-pack outputs committed. Same issue. Verdict: **violated-with-rec** — delete; .gitignore `wasm/pkg*/`; consumer (`playground/src/wasm/` build step) generates fresh on demand.

### §3.4 — Generated docs (none observed; honoured)

No `*.generated.md` files visible. Verdict: **honoured**.

---

## §4 — `system-cohesion` — scripts / Makefile / xtask must not duplicate logic

Memory item + KISS DRY: scripts must form one cohesive system, not duplicate ad hoc.

### §4.1 — Makefile + scripts/ + xtask split

The Makefile already declares (line 8): "*High-level convenience targets only. Iteration surfaces (check / test / clippy per bounded working-set) live as cargo aliases in .cargo/config.toml*". The Makefile delegates to:
- `cargo xtask regen` (line 209)
- `scripts/doctor.sh` (line 126)
- `scripts/profile-bench-headless.sh` (lines 320, 350)
- `scripts/prepare-profile-wave.sh` (line 419)

Verdict: **honoured** for Makefile↔script delegation pattern.

### §4.2 — script-internal duplication

Sample inspection (without reading every script): `scripts/iai-compare.sh`, `scripts/bisect-fastpath.sh`, `scripts/test-tier.sh` all might invoke `cargo` with similar flag-builders. Without verbatim source the verdict cannot land. Recommendation: **silent-must-add** — a `scripts/lib/cargo.sh` shared helper extracts the flag-build logic; per-script source `lib/cargo.sh` for `cargo_invoke()`. Smaller surfaces if duplication is small.

### §4.3 — extension `package.json` + `esbuild.mjs` + `dist/`

VS Code extension builds via esbuild. The `Makefile` `build-ext` (line 58) delegates to `cd extension && npm run build`. Verdict: **honoured** (no dual-build).

### §4.4 — playground `package.json` + `vite.config.ts` + `playwright.config.ts`

Three TS configs (`tsconfig.json`, `tsconfig.node.json`); Vite + Playwright. Verdict: **honoured** — typical Vite layout; no duplication.

---

## §5 — `no-workarounds` — verify ser + gorgeous archive is clean (no backward-compat shims)

Memory item: *"Zero tolerance for workarounds, fallbacks, stubs, or legacy code in any implementation"*.

### §5.1 — Lock 12 archive ceremony NOT executed

`Cargo.toml` line 2 still lists `crates/ser` and `crates/gorgeous` in `members`. This is precondition-failure — the Lock 12 ceremony was supposed to land BEFORE BA.W0. The plan-set restart cannot proceed until this lands.

Verdict: **violated-with-blocking-rec** — execute Lock 12 ceremony as the first restart-suite ratification step. Move `crates/ser` → `archive/ser/`, `crates/gorgeous` → `archive/gorgeous/`; remove from workspace `members`; verify zero referent in active workspace via `rg 'bbnf-ser|bbnf-gorgeous|crates/ser|crates/gorgeous' crates/`.

### §5.2 — Gorgeous per-grammar wrappers + match-on-grammar-name

Per CENSUS §2.5: `crates/gorgeous/src/{json,bnf,bbnf,ebnf,css,google_sheets}.rs` (~10-15 LOC each) + `crates/gorgeous/src/builtin.rs:9-22` match. These violate Lock 14 (full grammar generalisation); violate `no-workarounds` (match-on-name dispatch is an architectural workaround for not having metadata-driven dispatch).

Verdict: **violated-with-rec** — when gorgeous is archived, the violations move to `archive/gorgeous/` and become provenance, not active workarounds. If gorgeous must live (TBD per Replacement Design), the per-grammar wrappers should collapse to `prettify_grammar(ident, input, config)` consuming `[workspace.metadata.bbnf]` for dispatch. The right-shape is the latter; archive is the easier path.

### §5.3 — Gorgeous `vm.rs:217` "shouldn't happen in practice" fallback

Per CENSUS §3.1: `crates/gorgeous/src/vm.rs:217 ">8-byte separators: fall through to default (shouldn't happen in practice)"`. Classic workaround marker. Verdict: **violated-with-rec** — `unreachable!()` or `panic!("8-byte separator unexpected: {:?}", sep)` per FAIL-EXPLICIT discipline. (Moot if archive lands.)

### §5.4 — Analysis `crates/analysis/src/state/parsing.rs:81-83` panic-payload downcast

CENSUS §6 row: `panic_info.downcast_ref::<String>()` — std panic API; KEEP.

### §5.5 — LSP bench `crates/lsp/benches/bench_lsp.rs:197` `Box<dyn Fn(usize) -> String>`

CENSUS §6 row — KEEP (bench input alphabet).

---

## §6 — `archaic-diction-as-voice` for docs

The user's memory item: *"User's archaic diction (begets, therein, thereof, etc.) is deliberate voice, not AI artefacts"*. Per STYLE.md §Word-level register markers, "be-" compounds, "heretofore", "hitherto", "whereof" are deployed where befitting.

### §6.1 — Restart-suite docs (this Pass C output)

Use of "begets", "therein", "thereof", "appurtenant", "wherein" — recommended throughout per STYLE.md calibration spectrum (`unpretentious-academic` register for tranche/audit/plan docs). Verdict: **honoured** (this doc deploys "thereof", "wherein", "by way of" deliberately).

### §6.2 — User-facing `docs/bbnf/` getting-started, etc.

Calibration: user-facing prose at `getting-started.md` should sit at `unpretentious-academic` register, not at full `mild-lilt`. Archaic diction works for grammar-specification docs; works less well for "How do I get started" prose. Per STYLE.md: "Forcing register up the spectrum (cosmopolitan phrase in a commit body, poetic lilt in a hard-gate clause) is worse than the absence of register."

Verdict: **silent-must-add** — re-calibrate `docs/bbnf/*` to register-appropriate voice during the Pass-C docs re-do. Don't force archaic; don't strip plain prose.

---

## §7 — `KISS DRY` and `system-cohesion` — Makefile/script/xtask cohesion

The Makefile is 420 lines. Some heavy AY-W5-W7 gate commands (lines 241-420) duplicate orchestration logic that the orchestrator-side scripts handle. This is meta-language in Makefile form — Makefile targets named after waves (AY.W5, AY.W6, AY.W7) couple the build system to specific tranche letters.

Verdict: **violated-with-rec** — extract AY-specific gates from Makefile to a per-tranche `docs/tranches/AY-{I,II,III}/Makefile.gates` (or wave-spec embedded shell snippets). Keep top-level Makefile generic and tranche-letter-free. The KISS DRY shape: Makefile = build system; scripts/ = orchestration; tranche docs = wave-specific recipes.

---

## §8 — `archaic-permissive` audit-voice (per HARDENING-AUDIT-PROMPT §V1)

The HARDENING-AUDIT-PROMPT directs: "Audit voice is direct, archaic-permissive". This audit document is auditing per the same precept. The current Pass-C output deploys "thereof", "wherein", "by way of"; voice is calibrated. Verdict: **honoured** for the new audit corpus.

---

## §9 — Top-line idiomaticity verdict

| Item | Verdict | Surgery |
|---|---|---|
| STYLE.md banned words | violated-with-rec | Mechanical sweep across `docs/bbnf/*`, `docs/performance/*`, `docs/parse-that/*`, `docs/cookbook/*` |
| Em-dash discipline | violated-with-rec | Replace " — " with "—" in older docs |
| Epanorthosis | violated-with-rec | Sweep + rewrite |
| Outline-shaped AI closers | suspected-violated | Sweep + rewrite |
| no-metalanguage-docs (user-facing) | violated-with-rec | Rewrite `docs/cookbook/*`, `docs/performance/timeline.md`, `docs/migration/bc-core-split.md` to standalone prose; relocate migration narrative to `audit/` |
| no-metalanguage-docs (tranche/audit) | honoured-by-design | exempt |
| clean-regen-discipline (`server/bbnf-lsp`) | violated-with-rec | Delete; .gitignore |
| clean-regen-discipline (`extension/*.vsix`) | violated-with-rec | Delete; .gitignore |
| clean-regen-discipline (`wasm/pkg*/`) | violated-with-rec | Delete; .gitignore |
| Makefile/scripts/xtask cohesion | honoured (delegation) + violated (AY-* gates couple wave-letters into Makefile) | Extract AY-* targets out of root Makefile |
| no-workarounds (`gorgeous/builtin.rs:9-22` match) | violated-with-rec | Lock 12 archive solves it |
| no-workarounds (`gorgeous/vm.rs:217`) | violated-with-rec | FAIL-EXPLICIT or archive |
| Lock 12 archive ceremony NOT executed | violated-with-blocking-rec | Execute as first restart step; move `crates/ser` + `crates/gorgeous` to `archive/`; drop from workspace `members` |
| archaic-diction calibration | mostly-honoured + register-mismatch in user-facing | Re-calibrate `docs/bbnf/*` per STYLE spectrum |

---

## Closing

The largest idiomaticity faults are: (a) Lock 12 archive ceremony unexecuted (hardening-plan-synthesis identifies this as blocking BA.W0), (b) committed build artefacts (`server/bbnf-lsp`, `extension/*.vsix`, `wasm/pkg*/`) violate `clean-regen-discipline`, (c) older user-facing docs at `docs/bbnf/*`, `docs/performance/*`, `docs/parse-that/*` likely exhibit AI-writing-sign drift requiring mechanical sweep + rewrite. The restart's docs re-do absorbs (c); the prelude executes (a) + (b).
