//! Payload decoder kernels — variable-width arena writers consumed by
//! [`crate::psi`]'s `write_decoded` dispatcher.
//!
//! Each kernel under this directory owns one decoder shape:
//!
//! - [`json_string`] — JSON `string` with `\n` / `\t` / `\"` / `\uXXXX`
//!   escape decoding, including UTF-16 surrogate-pair recovery to
//!   4-byte UTF-8.
//!
//! The kernels are general — `decode_into` takes a raw matched byte
//! slice and stages bytes into a caller-owned destination buffer
//! starting at a known arena offset. The PSI dispatcher (the producer
//! of `PayloadJob`s) carries the routing key from the lifter, so the
//! per-kernel selection is data-driven rather than per-grammar.

pub mod json_string;
