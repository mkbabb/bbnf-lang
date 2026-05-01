//! Bipartite assignment COP builder.
//!
//! Fluent API for the common pattern of "assign N source rows to M
//! target columns with per-cell costs, role-based AllDifferent groups,
//! and optional hard pin constraints." Internally constructs a
//! [`Csp<CostFiniteDomain>`] with one variable per row, an
//! [`AllDifferentExcept`] per row-group, and `-1` as the unmatched
//! sentinel; the underlying branch-and-bound search is invoked through
//! [`Csp::solve_optimized`] with [`OptimizationMode::MinimizeCost`] and
//! [`Pruning::AcFc`].
//!
//! `AssignmentBuilder` is intended for `n ≤ ~100` rows / cols. The
//! branch-and-bound search degrades super-linearly past that point;
//! larger problems should prefer a specialized Hungarian algorithm
//! and feed the resulting permutation back into a Csp only if
//! additional constraints (groups, pins) make the closed-form
//! solution infeasible.
//!
//! # Example
//!
//! ```
//! use csp_solver::assignment;
//!
//! let sol = assignment()
//!     .rows(3)
//!     .cols(3)
//!     .cost(|i, k| if i == k { 0.0 } else { 10.0 })
//!     .unmatch_penalty(100.0)
//!     .solve()
//!     .expect("solvable");
//!
//! assert_eq!(sol.assign, vec![0, 1, 2]);
//! assert_eq!(sol.cost, 0.0);
//! ```

use crate::constraint::{AllDifferentExcept, ConstraintEnum};
use crate::domain::CostFiniteDomain;
use crate::{Csp, OptimizationMode, Pruning, SolveConfig, SolveStats};

/// Sentinel value used in [`AssignmentSolution::assign`] to denote an
/// unmatched row.
///
/// Encoded as a negative `i32` so it can never collide with a valid
/// 0-indexed column. The internal `CostFiniteDomain` for each row
/// always carries this value as a real domain entry priced at the
/// caller-supplied [`AssignmentBuilder::unmatch_penalty`]; the
/// branch-and-bound search treats it as just another option whose
/// dominance is decided by total cost.
pub const SENTINEL: i32 = -1;

/// Default node budget applied to the underlying branch-and-bound
/// search when the caller does not override it via
/// [`AssignmentBuilder::node_budget`].
const DEFAULT_NODE_BUDGET: u64 = 1_000_000;

/// Fluent builder for bipartite assignment COPs.
///
/// Construct via [`assignment()`] (preferred) or [`Default::default`].
/// All setters consume `self` and return `self`, allowing chained
/// configuration. The terminal [`AssignmentBuilder::solve`] call
/// validates the configuration, materializes the underlying
/// [`Csp<CostFiniteDomain>`], runs branch-and-bound, and returns an
/// [`AssignmentSolution`] (or an [`AssignmentError`] on
/// mis-configuration / infeasibility).
#[derive(Debug, Default)]
pub struct AssignmentBuilder {
    n_rows: usize,
    n_cols: usize,
    /// Row-major `n_rows × n_cols` matrix of per-cell costs. Populated
    /// eagerly by [`AssignmentBuilder::cost`] so the builder owns no
    /// closure state.
    cost_matrix: Vec<f64>,
    /// Length `n_rows`; defaults to all-zero (single group) if the
    /// caller never invoked [`AssignmentBuilder::row_group`].
    row_groups: Vec<u8>,
    /// Length `n_cols`; defaults to all-zero (single group) if the
    /// caller never invoked [`AssignmentBuilder::col_group`].
    col_groups: Vec<u8>,
    /// Hard `(row, col)` equality pins. Validated against the row's
    /// computed domain at [`AssignmentBuilder::solve`] time.
    pins: Vec<(usize, i32)>,
    /// Per-row cost paid when the assigned column is [`SENTINEL`].
    unmatch_penalty: f64,
    /// Optional cap on branch-and-bound nodes; `None` means use the
    /// crate default of `1_000_000`. See
    /// [`crate::SolveConfig::node_budget`] for the contract.
    node_budget: Option<u64>,
    /// Tracks whether [`AssignmentBuilder::cost`] has been called so
    /// `.solve()` can return [`AssignmentError::CostNotSet`] without
    /// guessing from `cost_matrix.is_empty()`.
    cost_set: bool,
}

/// Result of a successful [`AssignmentBuilder::solve`] call.
#[derive(Debug, Clone)]
pub struct AssignmentSolution {
    /// Length `n_rows`. Each entry is the assigned column index in
    /// `0..n_cols`, or [`SENTINEL`] (`-1`) if the row was left
    /// unmatched.
    pub assign: Vec<i32>,
    /// Total cost of the assignment: the sum of `cost_matrix[i][k]`
    /// for each matched row `i → k`, plus
    /// [`AssignmentBuilder::unmatch_penalty`] for each unmatched row.
    pub cost: f64,
    /// Statistics from the underlying branch-and-bound run. Inspect
    /// [`SolveStats::budget_exceeded`] to distinguish best-so-far
    /// from optimal solutions.
    pub stats: SolveStats,
}

/// Errors from [`AssignmentBuilder::solve`].
#[derive(Debug)]
pub enum AssignmentError {
    /// `.rows()` or `.cols()` was not called before `.solve()` (or
    /// either was set to zero).
    DimensionsNotSet,
    /// `.cost()` was not called before `.solve()`.
    CostNotSet,
    /// A custom `row_group` / `col_group` slice did not match the
    /// declared dimensions.
    GroupLengthMismatch,
    /// A pin references an out-of-range row or a column that is
    /// neither [`SENTINEL`] nor a valid `0..n_cols` index, or whose
    /// row-group does not match its target column's group.
    InvalidPin {
        /// Row index supplied to [`AssignmentBuilder::pin`].
        row: usize,
        /// Column index (or [`SENTINEL`]) supplied to
        /// [`AssignmentBuilder::pin`].
        col: i32,
    },
    /// The CSP has no feasible solution under the supplied
    /// constraints. Note that with [`SENTINEL`] always available a
    /// pure assignment problem is always feasible; this variant
    /// surfaces when pins or group constraints are mutually
    /// incompatible.
    Infeasible,
}

impl std::fmt::Display for AssignmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DimensionsNotSet => {
                write!(f, "AssignmentBuilder: .rows() and .cols() must both be set to a non-zero value before .solve()")
            }
            Self::CostNotSet => {
                write!(f, "AssignmentBuilder: .cost() must be called before .solve()")
            }
            Self::GroupLengthMismatch => {
                write!(f, "AssignmentBuilder: row_groups / col_groups length does not match the declared dimensions")
            }
            Self::InvalidPin { row, col } => {
                write!(
                    f,
                    "AssignmentBuilder: invalid pin (row={row}, col={col}); col must be SENTINEL or a valid 0..n_cols index sharing the row's group"
                )
            }
            Self::Infeasible => {
                write!(f, "AssignmentBuilder: CSP is infeasible under the supplied constraints")
            }
        }
    }
}

impl std::error::Error for AssignmentError {}

/// Top-level constructor for an empty [`AssignmentBuilder`].
///
/// Equivalent to [`AssignmentBuilder::default`] but reads more
/// naturally at the call site:
///
/// ```
/// use csp_solver::assignment;
///
/// let sol = assignment()
///     .rows(2)
///     .cols(2)
///     .cost(|i, k| (i + k) as f64)
///     .solve()
///     .expect("trivially solvable");
/// assert_eq!(sol.assign.len(), 2);
/// ```
pub fn assignment() -> AssignmentBuilder {
    AssignmentBuilder::default()
}

impl AssignmentBuilder {
    /// Set the number of source rows.
    pub fn rows(mut self, n: usize) -> Self {
        self.n_rows = n;
        self
    }

    /// Set the number of target columns.
    pub fn cols(mut self, n: usize) -> Self {
        self.n_cols = n;
        self
    }

    /// Eagerly populate the row-major cost matrix.
    ///
    /// Calls `f(i, k)` exactly once per `(row, col)` cell during this
    /// method, stores the result in an internal `Vec<f64>`, and
    /// returns `self`. No closure is retained, which keeps the
    /// builder `Send + Sync` even when constructed from non-`'static`
    /// captures.
    ///
    /// # Panics
    ///
    /// Panics if [`AssignmentBuilder::rows`] or
    /// [`AssignmentBuilder::cols`] has not been called yet — both
    /// dimensions are required to know how to walk `f`.
    pub fn cost(mut self, f: impl Fn(usize, usize) -> f64) -> Self {
        assert!(
            self.n_rows > 0 && self.n_cols > 0,
            "AssignmentBuilder::cost() requires .rows() and .cols() to be set first"
        );
        let mut matrix = Vec::with_capacity(self.n_rows * self.n_cols);
        for i in 0..self.n_rows {
            for k in 0..self.n_cols {
                matrix.push(f(i, k));
            }
        }
        self.cost_matrix = matrix;
        self.cost_set = true;
        self
    }

    /// Tag each row with a `u8` group identifier.
    ///
    /// Rows in different groups are placed in independent
    /// [`AllDifferentExcept`] scopes, and a row may only be assigned
    /// to a column whose group identifier matches. Omitting the call
    /// (or supplying `|_| 0`) puts every row in a single group, which
    /// is the standard bipartite-assignment shape.
    pub fn row_group(mut self, f: impl Fn(usize) -> u8) -> Self {
        self.row_groups = (0..self.n_rows).map(f).collect();
        self
    }

    /// Tag each column with a `u8` group identifier.
    ///
    /// See [`AssignmentBuilder::row_group`] for the semantics.
    pub fn col_group(mut self, f: impl Fn(usize) -> u8) -> Self {
        self.col_groups = (0..self.n_cols).map(f).collect();
        self
    }

    /// Hard-pin row `row` to column `col`.
    ///
    /// `col` may be [`SENTINEL`] to force the row unmatched. Multiple
    /// pins are accumulated; conflicting pins on the same row are
    /// detected at [`AssignmentBuilder::solve`] time as
    /// [`AssignmentError::Infeasible`].
    pub fn pin(mut self, row: usize, col: i32) -> Self {
        self.pins.push((row, col));
        self
    }

    /// Set the per-row cost paid when a row is assigned to
    /// [`SENTINEL`] (left unmatched).
    pub fn unmatch_penalty(mut self, penalty: f64) -> Self {
        self.unmatch_penalty = penalty;
        self
    }

    /// Override the underlying branch-and-bound node budget.
    ///
    /// Passing `None` here is *not* the same as never calling this
    /// method: `None` requests an unbounded search, while the default
    /// (no call) installs a `1_000_000` node guard so a pathological
    /// problem cannot hang the caller. See
    /// [`crate::SolveConfig::node_budget`].
    pub fn node_budget(mut self, budget: Option<u64>) -> Self {
        self.node_budget = budget;
        self
    }

    /// Validate the configuration, build the underlying CSP, and run
    /// branch-and-bound to find the minimum-cost assignment.
    pub fn solve(self) -> Result<AssignmentSolution, AssignmentError> {
        // 1. Dimensions + cost must be set.
        if self.n_rows == 0 || self.n_cols == 0 {
            return Err(AssignmentError::DimensionsNotSet);
        }
        if !self.cost_set {
            return Err(AssignmentError::CostNotSet);
        }

        // 2. Default groups to all-zero if the caller did not supply
        //    them; otherwise verify lengths match the declared
        //    dimensions.
        let row_groups: Vec<u8> = if self.row_groups.is_empty() {
            vec![0; self.n_rows]
        } else if self.row_groups.len() == self.n_rows {
            self.row_groups
        } else {
            return Err(AssignmentError::GroupLengthMismatch);
        };
        let col_groups: Vec<u8> = if self.col_groups.is_empty() {
            vec![0; self.n_cols]
        } else if self.col_groups.len() == self.n_cols {
            self.col_groups
        } else {
            return Err(AssignmentError::GroupLengthMismatch);
        };

        // 3. Pre-validate pins and collapse them into a per-row map.
        //    Pins are baked directly into each row's CostFiniteDomain
        //    at construction time so the variable's `original_domain`
        //    already encodes the singleton; this matters because
        //    `Csp::solve_optimized` calls `Variable::reset()` at
        //    search start and would otherwise undo any post-hoc
        //    domain mutation. Multiple pins on the same row are
        //    accepted only if they agree.
        let mut row_pin: Vec<Option<i32>> = vec![None; self.n_rows];
        for &(row, col) in &self.pins {
            if row >= self.n_rows {
                return Err(AssignmentError::InvalidPin { row, col });
            }
            if col != SENTINEL && (col < 0 || col as usize >= self.n_cols) {
                return Err(AssignmentError::InvalidPin { row, col });
            }
            // Verify pin is compatible with the row's group: SENTINEL
            // is always allowed, otherwise the column's group must
            // match the row's.
            if col != SENTINEL && col_groups[col as usize] != row_groups[row] {
                return Err(AssignmentError::InvalidPin { row, col });
            }
            match row_pin[row] {
                None => row_pin[row] = Some(col),
                Some(prev) if prev == col => {} // duplicate, fine
                Some(_) => return Err(AssignmentError::Infeasible),
            }
        }

        // 4. Build one CostFiniteDomain per row, restricted to columns
        //    whose group matches the row's group (and to the pinned
        //    singleton when a pin is present). SENTINEL is always
        //    available at the unmatch penalty unless overridden by a
        //    non-SENTINEL pin.
        let mut csp: Csp<CostFiniteDomain> = Csp::new();
        let mut row_var_ids: Vec<u32> = Vec::with_capacity(self.n_rows);

        for i in 0..self.n_rows {
            let row_group = row_groups[i];
            let row_offset = i * self.n_cols;

            let mut values: Vec<i32> = Vec::with_capacity(self.n_cols + 1);
            let mut costs: Vec<f64> = Vec::with_capacity(self.n_cols + 1);

            match row_pin[i] {
                Some(SENTINEL) => {
                    values.push(SENTINEL);
                    costs.push(self.unmatch_penalty);
                }
                Some(col) => {
                    // col is guaranteed in 0..n_cols and group-compatible
                    // by the pin validation above.
                    values.push(col);
                    costs.push(self.cost_matrix[row_offset + col as usize]);
                }
                None => {
                    // SENTINEL first; CostFiniteDomain canonicalises to
                    // ascending value order internally so the order at
                    // construction is irrelevant for correctness, but
                    // starting from SENTINEL keeps the (values, costs)
                    // slices easy to read in a debugger.
                    values.push(SENTINEL);
                    costs.push(self.unmatch_penalty);
                    for (k, &cg) in col_groups.iter().enumerate() {
                        if cg == row_group {
                            values.push(k as i32);
                            costs.push(self.cost_matrix[row_offset + k]);
                        }
                    }
                }
            }

            let domain = CostFiniteDomain::new(values, costs);
            row_var_ids.push(csp.add_variable(domain));
        }

        // 5. Add one AllDifferentExcept per distinct row group.
        let mut unique_groups: Vec<u8> = row_groups.clone();
        unique_groups.sort_unstable();
        unique_groups.dedup();
        for group in unique_groups {
            let scope: Vec<u32> = (0..self.n_rows)
                .filter(|&i| row_groups[i] == group)
                .map(|i| row_var_ids[i])
                .collect();
            // A single-row group still benefits from the constraint
            // for symmetry — it's a no-op at search time but keeps
            // the adjacency structure uniform across groups.
            csp.add_constraint_enum(ConstraintEnum::AllDifferentExcept(
                AllDifferentExcept::new(scope, SENTINEL),
            ));
        }

        // 6. Finalize and run branch-and-bound.
        csp.finalize();

        let config = SolveConfig {
            optimization_mode: OptimizationMode::MinimizeCost,
            max_solutions: 1,
            pruning: Pruning::AcFc,
            node_budget: self.node_budget.or(Some(DEFAULT_NODE_BUDGET)),
            ..SolveConfig::default()
        };

        let solutions = csp.solve_optimized(&config);
        let stats = csp.stats().clone();

        let solution = match solutions.into_iter().next() {
            Some(s) => s,
            None => return Err(AssignmentError::Infeasible),
        };

        // 7. Project the Solution<CostFiniteDomain> back into the
        //    row-indexed `assign` vector and recompute the total cost
        //    from the cost matrix + unmatch penalty so callers see a
        //    value that matches their inputs exactly (as opposed to
        //    the search's running total, which can drift through
        //    floating-point summation order).
        let mut assign: Vec<i32> = vec![SENTINEL; self.n_rows];
        let mut cost: f64 = 0.0;
        for i in 0..self.n_rows {
            let v = solution[row_var_ids[i] as usize];
            assign[i] = v;
            if v == SENTINEL {
                cost += self.unmatch_penalty;
            } else {
                cost += self.cost_matrix[i * self.n_cols + v as usize];
            }
        }

        Ok(AssignmentSolution {
            assign,
            cost,
            stats,
        })
    }
}
