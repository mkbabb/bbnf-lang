//! The reference unifier (LAYOUT-HM-V2 §11 item 2; sk-v27 LW3 HM pre-work).
//!
//! Types of §1.2, kinded variables over the chain `num ⊂ eq ⊂ any`,
//! idempotent substitutions, first-order unification with the occurs check,
//! and prenex schemes with generalise and instantiate. It is written for
//! clarity, not speed: it is the reference H1's checker (L-REF, L-PRINCIPAL)
//! compares the spine's solver against.
//!
//! Lock 2: this is not a pass. The module is private to `bbnf-oracle` (which
//! is `publish = false` and never a dependency of a published crate), exports
//! nothing, and nothing in the compiler reads it. Its only consumer today is
//! its own law suite in [`laws`]; H1's reference checker is the next.

mod order;
mod scheme;
mod subst;
mod ty;

#[cfg(test)]
mod arb;
#[cfg(test)]
mod laws;
