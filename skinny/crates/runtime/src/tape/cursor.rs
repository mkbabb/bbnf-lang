use super::Tape;

/// The lazy cursor primitive every grammar's node view shares: a position into
/// the single [`Tape`] plus the lazily-decoded structural kind. It captures ONLY
/// the laziness/cursor contract — the `(tape, cursor, offset, kind)` plumbing
/// every grammar's node view hand-rolls today.
///
/// It carries NO navigation verb (no keyed-child access, no sweep): it is
/// structurally incapable of seeing keyed nesting or a flat sweep, so it can
/// neither force a common value shape nor collapse a rich tree-shaped view down
/// to a flat one. The associated [`Cursor::Kind`] keeps every grammar's OWN
/// kind enum native — there is no shared structural vocabulary to flatten into.
///
/// Navigation lives ENTIRELY outside this trait, owned by each grammar at full
/// fidelity (a tree-shaped grammar keeps its keyed/recursive navigation + typed
/// value tree; a flat-tape grammar keeps its node sweep + rich summary). The
/// trait is therefore non-collapsible by construction: substituting one grammar's
/// node view for another's does NOT compile, because the navigation each callsite
/// needs is not on the shared seam.
pub trait Cursor<'doc, 'input: 'doc> {
    /// Each grammar's OWN structural-kind enum, chosen by the impl — there is no
    /// shared kind type, so the trait names none.
    type Kind: Copy;

    /// The single backing tape this cursor indexes into.
    fn tape(&self) -> &'doc Tape<'input>;

    /// The cursor's position in the sealed offset tape.
    fn cursor(&self) -> u32;

    /// The source byte offset, re-derived from the tape — nothing eager.
    fn offset(&self) -> usize;

    /// The lazily-decoded structural kind (the `at_cursor` decode).
    fn kind(&self) -> Self::Kind;
}
