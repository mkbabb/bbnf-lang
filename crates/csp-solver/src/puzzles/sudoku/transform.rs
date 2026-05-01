//! Symmetry-preserving transforms for Sudoku boards.

use super::rng::SimpleRng;

/// A symmetry-preserving transform for Sudoku boards.
pub struct SudokuTransform {
    pub digit_perm: Vec<u32>,
    pub row_perms: Vec<Vec<usize>>,
    pub col_perms: Vec<Vec<usize>>,
    pub band_perm: Vec<usize>,
    pub stack_perm: Vec<usize>,
    pub do_transpose: bool,
}

impl SudokuTransform {
    pub fn random(n: u32) -> Self {
        let mut rng = SimpleRng::from_time();
        Self::random_with_rng(n, &mut rng)
    }

    pub(crate) fn random_with_rng(n: u32, rng: &mut SimpleRng) -> Self {
        let m = n * n;
        let n = n as usize;

        let mut digits: Vec<u32> = (1..=m).collect();
        rng.shuffle(&mut digits);
        let mut digit_perm = vec![0u32];
        digit_perm.extend_from_slice(&digits);

        let row_perms: Vec<Vec<usize>> = (0..n)
            .map(|_| {
                let mut p: Vec<usize> = (0..n).collect();
                rng.shuffle(&mut p);
                p
            })
            .collect();

        let col_perms: Vec<Vec<usize>> = (0..n)
            .map(|_| {
                let mut p: Vec<usize> = (0..n).collect();
                rng.shuffle(&mut p);
                p
            })
            .collect();

        let mut band_perm: Vec<usize> = (0..n).collect();
        rng.shuffle(&mut band_perm);

        let mut stack_perm: Vec<usize> = (0..n).collect();
        rng.shuffle(&mut stack_perm);

        let do_transpose = rng.next_u64().is_multiple_of(2);

        Self {
            digit_perm,
            row_perms,
            col_perms,
            band_perm,
            stack_perm,
            do_transpose,
        }
    }

    pub fn apply(&self, board: &[u32], n: u32) -> Vec<u32> {
        let m = (n * n) as usize;
        let n = n as usize;
        let total = m * m;
        assert_eq!(board.len(), total);

        let mut result = vec![0u32; total];

        for (old_pos, &val) in board.iter().enumerate().take(total) {
            let old_row = old_pos / m;
            let old_col = old_pos % m;

            let old_band = old_row / n;
            let old_row_in_band = old_row % n;
            let old_stack = old_col / n;
            let old_col_in_stack = old_col % n;

            let new_band = self.band_perm[old_band];
            let new_row_in_band = self.row_perms[new_band][old_row_in_band];
            let new_stack = self.stack_perm[old_stack];
            let new_col_in_stack = self.col_perms[new_stack][old_col_in_stack];

            let mut new_row = new_band * n + new_row_in_band;
            let mut new_col = new_stack * n + new_col_in_stack;

            if self.do_transpose {
                std::mem::swap(&mut new_row, &mut new_col);
            }

            let new_pos = new_row * m + new_col;
            result[new_pos] = if val == 0 { 0 } else { self.digit_perm[val as usize] };
        }

        result
    }
}

/// Apply a random symmetry transform to a Sudoku board.
pub fn apply_random_transform(board: &[u32], n: u32) -> Vec<u32> {
    let transform = SudokuTransform::random(n);
    transform.apply(board, n)
}
