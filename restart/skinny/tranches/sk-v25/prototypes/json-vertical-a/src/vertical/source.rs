use std::sync::Arc;

use super::{Digest, VerticalError, budget::Budget};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SourceLock<'a> {
    pub bytes: usize,
    pub sha256: &'a str,
}

#[derive(Clone, Debug)]
pub struct SourceUnit {
    bytes: Arc<str>,
    digest: Digest,
}

impl SourceUnit {
    pub fn acquire(bytes: &str, budget: Budget) -> Result<Self, VerticalError> {
        Budget::require(budget.source_bytes, bytes.len(), "source_bytes")?;
        let digest = Digest::of(bytes.as_bytes());
        Ok(Self {
            bytes: Arc::from(bytes),
            digest,
        })
    }

    pub fn acquire_locked(
        bytes: &str,
        lock: SourceLock<'_>,
        budget: Budget,
    ) -> Result<Self, VerticalError> {
        let source = Self::acquire(bytes, budget)?;
        if bytes.len() != lock.bytes || source.digest.hex() != lock.sha256 {
            return Err(VerticalError::Source(format!(
                "expected {} bytes/{}, got {}/{}",
                lock.bytes,
                lock.sha256,
                bytes.len(),
                source.digest.hex()
            )));
        }
        Ok(source)
    }

    #[must_use]
    pub fn bytes(&self) -> &str {
        &self.bytes
    }

    #[must_use]
    pub const fn digest(&self) -> Digest {
        self.digest
    }
}
