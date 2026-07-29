use core::fmt;
use core::ops::Range;

use super::{CompileOptions, CompiledModule, ModuleInput, StageResult, compile_module};

/// A stateful source-edit route with deterministic full invalidation.
///
/// W2 deliberately does not claim incremental compilation or cache reuse.
/// Each valid byte edit updates the owned source and recompiles the complete
/// immutable carrier through the same production boundary as a fresh build.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompilationSession {
    logical_path: String,
    bytes: Vec<u8>,
    options: CompileOptions,
}

impl CompilationSession {
    pub fn new(
        logical_path: impl Into<String>,
        bytes: impl Into<Vec<u8>>,
        options: CompileOptions,
    ) -> Self {
        Self {
            logical_path: logical_path.into(),
            bytes: bytes.into(),
            options,
        }
    }

    pub fn logical_path(&self) -> &str {
        &self.logical_path
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub const fn options(&self) -> CompileOptions {
        self.options
    }

    pub fn compile(&self) -> StageResult<CompiledModule> {
        compile_module(
            ModuleInput::new(&self.logical_path, &self.bytes),
            self.options,
        )
    }

    /// Apply a half-open byte range and fully rebuild the immutable carrier.
    ///
    /// A syntactically valid range always becomes the session's new source,
    /// even when compilation returns diagnostics. Invalid ranges leave the
    /// session untouched.
    pub fn apply_edit(
        &mut self,
        range: Range<usize>,
        replacement: &[u8],
    ) -> Result<StageResult<CompiledModule>, EditError> {
        if range.start > range.end {
            return Err(EditError::ReversedRange {
                start: range.start,
                end: range.end,
            });
        }
        if range.end > self.bytes.len() {
            return Err(EditError::OutOfBounds {
                start: range.start,
                end: range.end,
                source_len: self.bytes.len(),
            });
        }

        self.bytes.splice(range, replacement.iter().copied());
        Ok(self.compile())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum EditError {
    ReversedRange {
        start: usize,
        end: usize,
    },
    OutOfBounds {
        start: usize,
        end: usize,
        source_len: usize,
    },
}

impl fmt::Display for EditError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ReversedRange { start, end } => {
                write!(
                    formatter,
                    "edit range starts at {start} after its end {end}"
                )
            }
            Self::OutOfBounds {
                start,
                end,
                source_len,
            } => write!(
                formatter,
                "edit range {start}..{end} exceeds source length {source_len}"
            ),
        }
    }
}

impl std::error::Error for EditError {}
