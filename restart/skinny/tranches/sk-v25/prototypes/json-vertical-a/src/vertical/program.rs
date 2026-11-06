use std::{collections::BTreeSet, sync::Arc};

use serde::{Deserialize, Serialize};

use super::{Digest, VerticalError, budget::Budget, canonical_digest, source::SourceUnit};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct RuleId(u16);

impl RuleId {
    #[must_use]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct Rule {
    id: RuleId,
    name: Arc<str>,
    body: Arc<str>,
    references: Arc<[RuleId]>,
}

impl Rule {
    #[must_use]
    pub const fn id(&self) -> RuleId {
        self.id
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn body(&self) -> &str {
        &self.body
    }

    #[must_use]
    pub fn references(&self) -> &[RuleId] {
        &self.references
    }
}

#[derive(Clone, Debug)]
pub struct Program {
    source_digest: Digest,
    revision: Digest,
    entry: RuleId,
    rules: Arc<[Rule]>,
}

impl Program {
    pub fn from_source(source: &SourceUnit, budget: Budget) -> Result<Self, VerticalError> {
        let definitions = definitions(source.bytes())?;
        Budget::require(budget.rules, definitions.len(), "rules")?;
        let names: BTreeSet<_> = definitions.iter().map(|(name, _)| name.as_str()).collect();
        if names.len() != definitions.len() {
            return Err(VerticalError::Program("duplicate rule name".into()));
        }
        let mut rules = Vec::with_capacity(definitions.len());
        for (index, (name, body)) in definitions.iter().enumerate() {
            let references = definitions
                .iter()
                .enumerate()
                .filter(|(_, (candidate, _))| contains_identifier(body, candidate))
                .map(|(target, _)| {
                    u16::try_from(target)
                        .map(RuleId)
                        .map_err(|_| VerticalError::Program("rule identifier overflow".into()))
                })
                .collect::<Result<Vec<_>, _>>()?;
            rules.push(Rule {
                id: RuleId(
                    u16::try_from(index)
                        .map_err(|_| VerticalError::Program("rule identifier overflow".into()))?,
                ),
                name: Arc::from(name.as_str()),
                body: Arc::from(body.as_str()),
                references: references.into(),
            });
        }
        let entry = rules
            .last()
            .map(Rule::id)
            .ok_or_else(|| VerticalError::Program("module has no rules".into()))?;
        let revision = canonical_digest(&(source.digest(), &rules))?;
        Ok(Self {
            source_digest: source.digest(),
            revision,
            entry,
            rules: rules.into(),
        })
    }

    #[must_use]
    pub const fn source_digest(&self) -> Digest {
        self.source_digest
    }

    #[must_use]
    pub const fn revision(&self) -> Digest {
        self.revision
    }

    #[must_use]
    pub const fn entry(&self) -> RuleId {
        self.entry
    }

    #[must_use]
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    pub fn rule(&self, id: RuleId) -> Result<&Rule, VerticalError> {
        self.rules
            .get(id.index())
            .ok_or(VerticalError::Identity { stage: "program" })
    }
}

fn definitions(source: &str) -> Result<Vec<(String, String)>, VerticalError> {
    let mut definitions = Vec::new();
    let mut start = 0;
    let mut string = false;
    let mut regex = false;
    let mut escaped = false;
    for (index, byte) in source.bytes().enumerate() {
        if escaped {
            escaped = false;
            continue;
        }
        if byte == b'\\' && (string || regex) {
            escaped = true;
            continue;
        }
        if byte == b'\"' && !regex {
            string = !string;
        } else if byte == b'/' && !string {
            regex = !regex;
        } else if byte == b';' && !string && !regex {
            let statement = source[start..index].trim();
            start = index + 1;
            if statement.is_empty() || statement.starts_with('@') {
                continue;
            }
            let (name, body) = statement
                .split_once('=')
                .ok_or_else(|| VerticalError::Program("rule statement has no '='".into()))?;
            let name = name.trim();
            if name.is_empty()
                || !name
                    .bytes()
                    .all(|byte| byte == b'_' || byte.is_ascii_alphanumeric())
            {
                return Err(VerticalError::Program("invalid rule name".into()));
            }
            definitions.push((name.into(), body.trim().into()));
        }
    }
    Ok(definitions)
}

fn contains_identifier(body: &str, candidate: &str) -> bool {
    let bytes = body.as_bytes();
    let needle = candidate.as_bytes();
    let mut index = 0;
    let mut string = false;
    let mut regex = false;
    let mut escaped = false;
    while index < bytes.len() {
        let byte = bytes[index];
        if escaped {
            escaped = false;
            index += 1;
            continue;
        }
        if byte == b'\\' && (string || regex) {
            escaped = true;
            index += 1;
            continue;
        }
        if byte == b'\"' && !regex {
            string = !string;
            index += 1;
            continue;
        }
        if byte == b'/' && !string {
            regex = !regex;
            index += 1;
            continue;
        }
        if !string && !regex && bytes[index..].starts_with(needle) {
            let before = index.checked_sub(1).and_then(|at| bytes.get(at)).copied();
            let after = bytes.get(index + needle.len()).copied();
            if !before.is_some_and(|value| value == b'_' || value.is_ascii_alphanumeric())
                && !after.is_some_and(|value| value == b'_' || value.is_ascii_alphanumeric())
            {
                return true;
            }
        }
        index += 1;
    }
    false
}
