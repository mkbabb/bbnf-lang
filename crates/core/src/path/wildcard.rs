//! Wildcard expansion — lazy iteration over a `Wildcard` path step.
//!
//! A `PathSegment::Wildcard` step matches every element under a list
//! (or every value under a map; the parsed-document executor lands in
//! W3). The default execution lane is a lazy `Iter<Item = T>`: no
//! allocation, no structural recursion, evaluation deferred to the
//! call site. Adapters layer on top:
//!
//! - [`WildcardIter::with_anchors`] yields `(Path<'_>, T)` so a caller
//!   can reconstitute the path that produced each element.
//! - `.collect()` is the standard `Iterator` collect — call-site
//!   choice. `Vec<T>` falls out of `iter.collect()` at the consumer's
//!   discretion; the wildcard machinery never materialises a list on
//!   its own.
//!
//! [`WildcardConfig`] carries the depth cap (default 8). When a
//! wildcard chain would exceed the cap, the iterator surfaces a
//! [`PathErrorReason::WildcardOverflow`] diagnostic the proc-macro and
//! the runtime executor convert to a Span-anchored error.
//!
//! The IR-side wildcard segment is already declared on
//! [`PathSegment::Wildcard`] (W2.1). This module supplies the
//! execution-time machinery the W3 lazy bail-out parse will dispatch
//! through.

use core::iter::FusedIterator;

use super::error::{PathError, PathErrorReason};
use super::ir::{Path, PathSegment};

/// Default depth cap for nested wildcard expansion. Eight levels
/// matches the AZ-IV.W2 hard gate; consumers override via
/// [`WildcardConfig::with_depth_cap`].
pub const DEFAULT_WILDCARD_DEPTH_CAP: usize = 8;

/// Configuration knob set carried alongside a wildcard expansion.
///
/// Currently only the depth cap; future tranches add ordering hints
/// (depth-first vs breadth-first) without breaking the constructor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WildcardConfig {
    depth_cap: usize,
}

impl WildcardConfig {
    /// Build a config with the default depth cap
    /// ([`DEFAULT_WILDCARD_DEPTH_CAP`]).
    pub const fn new() -> Self {
        Self {
            depth_cap: DEFAULT_WILDCARD_DEPTH_CAP,
        }
    }

    /// Override the depth cap. Setting `depth_cap = 0` disables
    /// wildcard expansion entirely (every wildcard step overflows
    /// immediately).
    pub const fn with_depth_cap(mut self, depth_cap: usize) -> Self {
        self.depth_cap = depth_cap;
        self
    }

    /// Read the configured depth cap.
    #[inline]
    pub const fn depth_cap(&self) -> usize {
        self.depth_cap
    }
}

impl Default for WildcardConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Lazy iterator over the elements a `Wildcard` path step expands to.
///
/// `WildcardIter<I>` wraps any `Iterator<Item = T>` plus the path
/// prefix that produced it. The iterator is `FusedIterator`-safe — once
/// the underlying source returns `None`, subsequent calls also return
/// `None`. Allocation-free in the default lane: the iterator carries
/// only the source, the prefix, and the configured depth cap.
pub struct WildcardIter<'a, I>
where
    I: Iterator,
{
    source: I,
    prefix: Path<'a>,
    config: WildcardConfig,
    depth: usize,
}

impl<'a, I> WildcardIter<'a, I>
where
    I: Iterator,
{
    /// Build a wildcard iterator wrapping `source` rooted under
    /// `prefix`. `depth` is the current wildcard-nesting depth (zero
    /// for an outermost expansion); subsequent nested wildcards
    /// increment the count and surface
    /// [`PathErrorReason::WildcardOverflow`] once it exceeds the
    /// configured cap.
    pub fn new(source: I, prefix: Path<'a>, config: WildcardConfig, depth: usize) -> Self {
        Self {
            source,
            prefix,
            config,
            depth,
        }
    }

    /// Adapt the iterator so each item arrives paired with the path
    /// that produced it (the prefix plus a synthesised wildcard step).
    /// The returned iterator yields `(Path<'_>, T)`.
    pub fn with_anchors(self) -> WithAnchors<'a, I>
    where
        I::Item: Sized,
    {
        WithAnchors { inner: self }
    }

    /// True if the next `next()` call would overflow the depth cap.
    #[inline]
    pub fn would_overflow(&self) -> bool {
        self.depth >= self.config.depth_cap
    }

    /// Build the diagnostic the consumer renders when the cap fires.
    /// Carries the prefix as `segment_str` so the proc-macro can quote
    /// the offending path back to the user.
    pub fn overflow_error(&self) -> PathError {
        PathError::new(
            self.prefix.len(),
            self.prefix.to_string(),
            "Wildcard",
            Vec::new(),
            PathErrorReason::WildcardOverflow,
        )
    }
}

impl<'a, I> Iterator for WildcardIter<'a, I>
where
    I: Iterator,
{
    type Item = I::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.would_overflow() {
            return None;
        }
        self.source.next()
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.would_overflow() {
            (0, Some(0))
        } else {
            self.source.size_hint()
        }
    }
}

impl<'a, I> FusedIterator for WildcardIter<'a, I> where I: FusedIterator {}

/// Adapter that pairs every wildcard yield with the path prefix that
/// produced it. Caller receives `(Path<'_>, T)` tuples.
pub struct WithAnchors<'a, I>
where
    I: Iterator,
{
    inner: WildcardIter<'a, I>,
}

impl<'a, I> Iterator for WithAnchors<'a, I>
where
    I: Iterator,
{
    type Item = (Path<'a>, I::Item);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let item = self.inner.next()?;
        Some((self.inner.prefix, item))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, I> FusedIterator for WithAnchors<'a, I> where I: FusedIterator {}

/// Helper — true if the borrowed path's last segment is a wildcard.
/// The proc-macro consults this before lowering the macro to a
/// [`WildcardIter`] adapter; non-wildcard paths skip the lazy-iter
/// route entirely.
#[inline]
pub fn ends_with_wildcard(path: Path<'_>) -> bool {
    matches!(path.as_slice().last(), Some(PathSegment::Wildcard))
}
