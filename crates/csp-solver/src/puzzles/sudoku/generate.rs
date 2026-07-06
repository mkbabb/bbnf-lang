//! Board generation and difficulty measurement.

use include_dir::{Dir, include_dir};

use crate::ordering::Ordering;
use crate::{Pruning, SolveConfig};

use super::csp::{create_sudoku_csp, solve_sudoku};
use super::rng::SimpleRng;
use super::transform::{SudokuTransform, apply_random_transform};

/// The crate-owned puzzle-template bank, embedded into the compiled artifact at
/// *build* time via `include_dir!`.
///
/// Data now lives at `csp-solver/data/sudoku_puzzles/{N}/{difficulty}/` (crate
/// root, sibling of `tests/`) and ships inside the wheel/cdylib itself — zero
/// runtime filesystem access, zero cross-language directory coupling. This
/// replaces the Python service's former runtime directory-glob
/// (`board.py`'s `_load_templates`/`DATA_DIR`/`functools.cache`), which is now
/// deleted: `create_random_board` reads templates from here rather than having
/// them handed across the FFI boundary.
///
/// Regenerate the bank with the one-command generator:
/// `cargo run --release --example generate_templates -- <N> <difficulty> <count>`.
static SUDOKU_PUZZLES: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/data/sudoku_puzzles");

/// Puzzle difficulty level.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Difficulty {
    Easy,
    Medium,
    Hard,
}

impl Difficulty {
    /// Lowercase directory name, matching the on-disk
    /// `data/sudoku_puzzles/{N}/{name}` partitioning.
    pub(crate) fn dir_name(self) -> &'static str {
        match self {
            Difficulty::Easy => "easy",
            Difficulty::Medium => "medium",
            Difficulty::Hard => "hard",
        }
    }
}

/// Measure difficulty by backtrack count (FC + FailFirst).
pub fn measure_difficulty(board: &[u32], n: u32) -> u32 {
    let (mut csp, given) = create_sudoku_csp(board, n);
    let config = SolveConfig {
        pruning: Pruning::ForwardChecking,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };
    csp.solve_with_given(&config, &given);
    csp.stats().backtracks as u32
}

// ─── Embedded template access ───────────────────────────────────────────────

/// Minimal, dependency-free parser for the fixed template schema
/// `{"solution": {...}, "puzzle": {...}, "backtracks": N}`. Extracts the flat
/// `puzzle` object (the given cells) as a dense `Vec<u32>`.
///
/// Whitespace-tolerant, so it reads both the committed pretty-printed files
/// (`"0": 1`) and the generator's compact output (`"0":1`). No `serde`
/// dependency — the schema is a flat `{"<int>": <int>}` map with no nesting,
/// arrays, string values, or escaping.
fn parse_puzzle_field(json: &str, total: usize) -> Vec<u32> {
    let key = json
        .find("\"puzzle\"")
        .expect("template missing \"puzzle\" key");
    let obj_start = json[key..].find('{').expect("puzzle object missing '{'") + key;
    let obj_end = json[obj_start..]
        .find('}')
        .expect("puzzle object missing '}'")
        + obj_start;
    let body = &json[obj_start + 1..obj_end];

    let mut board = vec![0u32; total];
    for entry in body.split(',') {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        let (k, v) = entry.split_once(':').expect("malformed template entry");
        let pos: usize = k
            .trim()
            .trim_matches('"')
            .parse()
            .expect("bad position key");
        let val: u32 = v.trim().parse().expect("bad cell value");
        if pos < total {
            board[pos] = val;
        }
    }
    board
}

/// Load the crate-owned templates for `(n, difficulty)` from the compile-time
/// embed. Returns an empty vec when no bank is embedded for that size/difficulty
/// (e.g. N=5 Medium/Hard, deliberately rejected at the API — see the N=5 policy).
pub fn embedded_templates(n: u32, difficulty: Difficulty) -> Vec<Vec<u32>> {
    let total = (n * n * n * n) as usize;
    let path = format!("{n}/{}", difficulty.dir_name());
    let Some(dir) = SUDOKU_PUZZLES.get_dir(&path) else {
        return Vec::new();
    };
    dir.files()
        .filter(|f| f.path().extension().is_some_and(|e| e == "json"))
        .map(|f| {
            let contents = f.contents_utf8().expect("embedded template must be UTF-8");
            parse_puzzle_field(contents, total)
        })
        .collect()
}

/// Number of templates embedded for `(n, difficulty)`, counted from the
/// directory listing without parsing any file.
///
/// Zero means that size/difficulty is not shipped (e.g. N=5 Medium/Hard,
/// rejected at the API per the locked N=5 policy). The PyO3 `template_count`
/// wraps this so the Python service can reject such a request up front — with a
/// `NOT_FOUND` — instead of falling through to unbounded hole-digging (the old
/// N=5 DoS surface).
pub fn embedded_template_count(n: u32, difficulty: Difficulty) -> usize {
    let path = format!("{n}/{}", difficulty.dir_name());
    SUDOKU_PUZZLES.get_dir(&path).map_or(0, |dir| {
        dir.files()
            .filter(|f| f.path().extension().is_some_and(|e| e == "json"))
            .count()
    })
}

/// Historical difficulty → backtrack-count bands (N=3 only), ported from the
/// dead `generate_templates.py`. Consulted *solely* by the debug-build
/// consistency assertion in [`generate_board_with_templates`] — never a
/// release-path gate.
///
/// Non-N=3 sizes have no literature-backed band (clue-count is the only signal
/// there), so they accept any count rather than assert a fabricated bound. The
/// `Hard`/`Medium` upper bounds are `u32::MAX` on purpose: a genuinely-hard N=3
/// board can measure into the millions of backtracks (measured ~3.8M), so a
/// finite upper bound would false-fire. The bands exist to catch the *gross*
/// "wrong directory" mismatch — an easy claim served a non-easy board, or a
/// medium/hard claim served a trivially-easy (0-backtrack) board — not to
/// tightly grade each tier.
#[cfg(debug_assertions)]
fn expected_backtrack_band(n: u32, difficulty: Difficulty) -> (u32, u32) {
    if n != 3 {
        return (0, u32::MAX);
    }
    match difficulty {
        Difficulty::Easy => (0, 0),
        Difficulty::Medium => (1, u32::MAX),
        Difficulty::Hard => (100, u32::MAX),
    }
}

/// Generate a Sudoku board with the given sub-grid size and difficulty.
///
/// If `templates` is non-empty, picks a random template and applies a random
/// symmetry transform (fast path). Otherwise falls back to hole-digging
/// generation (slow path).
///
/// **R13 — difficulty honored on the template fast path.** On the fast path the
/// caller has already selected the difficulty-partitioned template list (the
/// embed keys on `{N}/{difficulty}`), so `difficulty` arrives pre-satisfied and
/// cannot be re-derived here without re-running the expensive `measure_difficulty`
/// full search this fast path exists to avoid. Rather than silently discarding
/// the parameter, this is a **debug-build-only consistency assertion** (zero
/// release cost): it fails loudly in `cargo test`/debug builds if a caller's
/// template list doesn't match its claimed difficulty (the "future template
/// loader mixes up the directory" scenario), and costs nothing in the release
/// path that serves real requests. The slow path honors `difficulty` directly.
pub fn generate_board_with_templates(
    n: u32,
    difficulty: Difficulty,
    templates: &[Vec<u32>],
) -> Vec<u32> {
    if !templates.is_empty() {
        let mut rng = SimpleRng::from_time();
        let idx = rng.next_usize(templates.len());
        let board = apply_random_transform(&templates[idx], n);

        #[cfg(debug_assertions)]
        {
            let (min_bt, max_bt) = expected_backtrack_band(n, difficulty);
            let bt = measure_difficulty(&board, n);
            debug_assert!(
                (min_bt..=max_bt).contains(&bt),
                "template claimed difficulty {difficulty:?} but measured {bt} backtracks \
                 (expected {min_bt}..={max_bt}) for N={n} — the template list does not match \
                 its claimed difficulty; check the caller's template-loading path"
            );
        }

        return board;
    }
    generate_board_slow(n, difficulty)
}

/// Generate a Sudoku board via hole-digging (slow fallback).
pub fn generate_board(n: u32, difficulty: Difficulty) -> Vec<u32> {
    generate_board_slow(n, difficulty)
}

/// Seeded, platform-independent counterpart of [`generate_board`].
///
/// Threads an explicit `seed` instead of reading `SimpleRng::from_time()`
/// (→ `std::time::SystemTime::now()`), which **panics** on
/// `wasm32-unknown-unknown` (no wall clock). Same `seed` + `n` +
/// `difficulty` ⇒ the same board on native and wasm — the invariant the
/// client-solve wasm surface (`csp-solver/wasm/src/sudoku.rs`) and its
/// cross-target parity harness rely on. Additive: `generate_board` still
/// seeds from the wall clock on native.
pub fn generate_board_seeded(n: u32, difficulty: Difficulty, seed: u64) -> Vec<u32> {
    generate_board_slow_with_rng(n, difficulty, &mut SimpleRng::new(seed))
}

/// Seeded, platform-independent counterpart of
/// [`generate_board_with_templates`]. When `templates` is non-empty it
/// picks one and applies a seeded random symmetry transform — the fast
/// path, threading the seed through `SudokuTransform::random_with_rng`
/// rather than reaching the `from_time()` seam inside
/// [`apply_random_transform`] (which panics on wasm32). An empty
/// `templates` falls back to seeded hole-digging.
///
/// Unlike [`generate_board_with_templates`], this does not run the R13
/// debug difficulty-consistency assertion: it serves the browser
/// client-solve path, where the frontend supplies templates it already
/// partitioned by `{N}/{difficulty}`, and a debug-build `measure_difficulty`
/// full search on every generated board would be a per-call cliff the
/// wasm/native parity harness would pay repeatedly.
pub fn generate_board_with_templates_seeded(
    n: u32,
    difficulty: Difficulty,
    templates: &[Vec<u32>],
    seed: u64,
) -> Vec<u32> {
    let mut rng = SimpleRng::new(seed);
    if templates.is_empty() {
        return generate_board_slow_with_rng(n, difficulty, &mut rng);
    }
    let idx = rng.next_usize(templates.len());
    SudokuTransform::random_with_rng(n, &mut rng).apply(&templates[idx], n)
}

fn generate_board_slow(n: u32, difficulty: Difficulty) -> Vec<u32> {
    generate_board_slow_with_rng(n, difficulty, &mut SimpleRng::from_time())
}

fn generate_board_slow_with_rng(n: u32, difficulty: Difficulty, rng: &mut SimpleRng) -> Vec<u32> {
    let m = n * n;
    let total = (m * m) as usize;

    // Step 1: Generate a complete valid solution.
    let mut seed_board = vec![0u32; total];
    let mut first_row: Vec<u32> = (1..=m).collect();
    rng.shuffle(&mut first_row);
    seed_board[..m as usize].copy_from_slice(&first_row);

    let config = SolveConfig {
        pruning: Pruning::Ac3,
        ordering: Ordering::FailFirst,
        max_solutions: 1,
        ..Default::default()
    };

    let solution = solve_sudoku(&seed_board, n, &config).expect("seeded board must be solvable");

    // Step 2: Remove cells by random hole-digging with uniqueness check.
    let target_holes = match difficulty {
        Difficulty::Easy => total / 4,
        Difficulty::Medium => (total as f64 / 1.75) as usize,
        Difficulty::Hard => (total as f64 / 1.25) as usize,
    };

    let mut board = solution.clone();
    let mut indices: Vec<usize> = (0..total).collect();
    rng.shuffle(&mut indices);

    let mut holes = 0usize;
    let uniqueness_config = SolveConfig {
        pruning: Pruning::Ac3,
        ordering: Ordering::FailFirst,
        max_solutions: 2,
        ..Default::default()
    };

    for &idx in &indices {
        if holes >= target_holes {
            break;
        }

        let saved = board[idx];
        board[idx] = 0;

        let (mut csp, given) = create_sudoku_csp(&board, n);
        let solutions = csp.solve_with_given(&uniqueness_config, &given);

        if solutions.len() == 1 {
            holes += 1;
        } else {
            board[idx] = saved;
        }
    }

    board
}

// ─── Whitebox tests ─────────────────────────────────────────────────────────
//
// The crate's tests-of-record are blackbox integration tests in `tests/`. These
// inline `#[cfg(test)]` tests are the narrow exception the manifest sanctions
// (mirroring `error.rs`): they exercise a *module-private* contract — the R13
// difficulty-consistency assertion and its private `expected_backtrack_band`
// table — which a blackbox test in `tests/` cannot see. Both cases use N=3 Easy
// boards (fast to generate *and* measure: 0 backtracks), so neither pays the
// multi-second `measure_difficulty` cost a genuinely-hard board would incur.
#[cfg(test)]
mod tests {
    use super::*;

    /// Happy path: a template that genuinely matches its claimed difficulty must
    /// not trip the debug consistency assertion (it is silent when the contract
    /// holds).
    #[test]
    fn generate_with_templates_matching_difficulty_does_not_panic() {
        let easy = generate_board(3, Difficulty::Easy);
        let board = generate_board_with_templates(3, Difficulty::Easy, &[easy]);
        assert_eq!(board.len(), 81);
    }

    /// Mismatch: a trivially-easy board (0 backtracks) served under a `Hard`
    /// claim (band `100..=MAX`) must fail the debug assertion loudly — the exact
    /// "wrong directory" scenario R13 guards against. Uses an easy board so the
    /// measurement is fast and the 0-backtracks result is stable.
    #[test]
    #[cfg_attr(
        not(debug_assertions),
        ignore = "R13 guard is a debug_assert; compiled out in release"
    )]
    #[should_panic(expected = "does not match its claimed difficulty")]
    fn generate_with_templates_mismatched_difficulty_panics_in_debug() {
        let easy = generate_board(3, Difficulty::Easy);
        let _ = generate_board_with_templates(3, Difficulty::Hard, &[easy]);
    }

    /// The embed is populated for the sizes/difficulties the bank ships, and
    /// degrades to an empty vec (not a panic) for absent ones — mirroring the
    /// former `_load_templates` "return []" behavior, now at compile time.
    #[test]
    fn embedded_templates_present_and_absent() {
        let hard = embedded_templates(3, Difficulty::Hard);
        assert!(!hard.is_empty(), "N=3 hard bank should be embedded");
        // The no-parse count agrees with the parsed length.
        assert_eq!(embedded_template_count(3, Difficulty::Hard), hard.len());
        // N=5 medium is deliberately not shipped (rejected at the API).
        assert!(embedded_templates(5, Difficulty::Medium).is_empty());
        assert_eq!(embedded_template_count(5, Difficulty::Medium), 0);
        // Every embedded N=3 easy template parses to a full 81-cell board.
        for board in embedded_templates(3, Difficulty::Easy) {
            assert_eq!(board.len(), 81);
        }
    }
}
