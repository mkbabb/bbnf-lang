use ir::RuleId;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PassDiagnostic {
    pub code: PassDiagnosticCode,
    pub rule: Option<RuleId>,
    pub message: String,
}

impl PassDiagnostic {
    pub fn collapsed_stage_not_viable(rule: RuleId, reason: impl Into<String>) -> Self {
        Self {
            code: PassDiagnosticCode::CollapsedStageNotViable,
            rule: Some(rule),
            message: reason.into(),
        }
    }

    pub fn backend_shape_inconsistent(rule: RuleId, reason: impl Into<String>) -> Self {
        Self {
            code: PassDiagnosticCode::BackendShapeInconsistent,
            rule: Some(rule),
            message: reason.into(),
        }
    }

    pub fn dominated_alternative(rule: RuleId, reason: impl Into<String>) -> Self {
        Self {
            code: PassDiagnosticCode::DominatedAlternative,
            rule: Some(rule),
            message: reason.into(),
        }
    }

    pub fn cost_facts_missing_evidence(rule: RuleId, reason: impl Into<String>) -> Self {
        Self {
            code: PassDiagnosticCode::CostFactsMissingEvidence,
            rule: Some(rule),
            message: reason.into(),
        }
    }

    pub fn code(&self) -> &'static str {
        self.code.as_str()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PassDiagnosticCode {
    CollapsedStageNotViable,
    BackendShapeInconsistent,
    DominatedAlternative,
    CostFactsMissingEvidence,
}

impl PassDiagnosticCode {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::CollapsedStageNotViable => "BBNF-COLLAPSEDSTAGE-NOT-VIABLE",
            Self::BackendShapeInconsistent => "BBNF-BACKEND-SHAPE-INCONSISTENT",
            Self::DominatedAlternative => "BBNF-DOMINATED-ALTERNATIVE",
            Self::CostFactsMissingEvidence => "BBNF-COSTFACTS-MISSING-EVIDENCE",
        }
    }
}
