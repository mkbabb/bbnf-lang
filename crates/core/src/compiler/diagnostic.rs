use core::fmt;

use serde::Serialize;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[non_exhaustive]
pub enum DiagnosticCode {
    SourceTooLarge,
    Syntax,
    ResourceLimit,
    DuplicateRule,
    DuplicateField,
    UndefinedRule,
    RecordMustBeRuleRoot,
    RecursiveRule,
    NullableRepeat,
    Regex,
    RevisionMismatch,
    GeneratedSourceTooLarge,
    InternalInvariant,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct Diagnostic {
    code: DiagnosticCode,
    start: usize,
    end: usize,
    message: String,
}

impl Diagnostic {
    pub(crate) fn new(
        code: DiagnosticCode,
        start: usize,
        end: usize,
        message: impl Into<String>,
    ) -> Self {
        Self {
            code,
            start,
            end,
            message: message.into(),
        }
    }

    pub const fn code(&self) -> DiagnosticCode {
        self.code
    }

    pub const fn start(&self) -> usize {
        self.start
    }

    pub const fn end(&self) -> usize {
        self.end
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "{:?} at {}..{}: {}",
            self.code, self.start, self.end, self.message
        )
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize)]
pub struct DiagnosticSet {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticSet {
    pub(crate) fn one(diagnostic: Diagnostic) -> Self {
        Self {
            diagnostics: vec![diagnostic],
        }
    }

    pub(crate) fn from_vec(mut diagnostics: Vec<Diagnostic>) -> Self {
        diagnostics.sort_by(|left, right| {
            (left.start, left.end, left.code, &left.message).cmp(&(
                right.start,
                right.end,
                right.code,
                &right.message,
            ))
        });
        diagnostics.dedup();
        Self { diagnostics }
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    pub const fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }
}

#[must_use = "compiler stage results must be handled"]
pub enum StageResult<T> {
    Accepted {
        value: T,
        diagnostics: DiagnosticSet,
    },
    Rejected {
        diagnostics: DiagnosticSet,
    },
}

impl<T> StageResult<T> {
    pub(crate) fn accepted(value: T) -> Self {
        Self::Accepted {
            value,
            diagnostics: DiagnosticSet::default(),
        }
    }

    pub(crate) fn rejected(diagnostic: Diagnostic) -> Self {
        Self::Rejected {
            diagnostics: DiagnosticSet::one(diagnostic),
        }
    }

    pub(crate) fn rejected_set(diagnostics: DiagnosticSet) -> Self {
        Self::Rejected { diagnostics }
    }
}
