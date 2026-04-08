//! Shared string interner for e-graph rewrite rules that mutate
//! `GrammarIR::strings`.
//!
//! Rewrite rules like `merge_literals`, `merge_regex_alts`, and
//! `simplify_regex_algebra` create new string literals during
//! saturation. The `Rewrite::apply` signature has no access to the
//! grammar, so we route string interning through a shared
//! `SharedStrings` handle with interior mutability.
//!
//! ```text
//!    GrammarIR::strings ────► SharedStrings ────► rewrite rules
//!                              (Arc<Mutex>)      (intern on apply)
//!                                     │
//!                                     ▼
//!                            egraph build phase
//!                            (intern on insert)
//!                                     │
//!                                     ▼
//!                          post-saturation write-back
//!                          (into_vec → ir.strings)
//! ```
//!
//! `Arc<Mutex>` (not `Rc<RefCell>`) is required because the e-graph's
//! `Rewrite` trait demands `Send + Sync`; a potential future parallel
//! scheduler still works with this shape. Contention is non-existent
//! in practice: rewrite rules run single-threaded and each intern is
//! a handful of instructions.
//!
//! The lifetime runs: `from_ir` → clone handles into rules and build
//! phase → run saturation → drop rules → `into_vec` to extract the
//! final pool.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::{GrammarIR, StringId};

/// Interior-mutable string pool shared between the e-graph build
/// phase and rewrite rules that need to intern new literals.
///
/// Clone-able — cloning increments the `Arc` and shares the same
/// underlying storage. `Send + Sync` via `Arc<Mutex<…>>`.
#[derive(Clone, Debug)]
pub struct SharedStrings {
    strings: Arc<Mutex<Vec<String>>>,
    dedup: Arc<Mutex<HashMap<String, StringId>>>,
}

impl SharedStrings {
    /// Initialize the shared pool from a grammar's current string table.
    pub fn from_ir(ir: &GrammarIR) -> Self {
        let strings: Vec<String> = ir.strings.clone();
        let dedup: HashMap<String, StringId> = strings
            .iter()
            .enumerate()
            .map(|(i, s)| (s.clone(), i as StringId))
            .collect();
        Self {
            strings: Arc::new(Mutex::new(strings)),
            dedup: Arc::new(Mutex::new(dedup)),
        }
    }

    /// Intern a string — returns the existing `StringId` if the pool
    /// already contains an equal string, or allocates a new entry.
    pub fn intern(&self, s: &str) -> StringId {
        // Fast path: existing entry.
        {
            let dedup = self.dedup.lock().expect("SharedStrings dedup poisoned");
            if let Some(&existing) = dedup.get(s) {
                return existing;
            }
        }
        // Slow path: allocate. Lock order is strings → dedup to stay
        // consistent with `from_ir`.
        let mut strings = self.strings.lock().expect("SharedStrings strings poisoned");
        let id = strings.len() as StringId;
        strings.push(s.to_string());
        drop(strings);
        self.dedup
            .lock()
            .expect("SharedStrings dedup poisoned")
            .insert(s.to_string(), id);
        id
    }

    /// Look up the string for an existing `StringId`. Returns an owned
    /// clone (the underlying storage is behind a `Mutex`, so we
    /// can't expose borrows).
    pub fn get(&self, id: StringId) -> String {
        self.strings.lock().expect("SharedStrings strings poisoned")[id as usize].clone()
    }

    /// Current pool size.
    pub fn len(&self) -> usize {
        self.strings
            .lock()
            .expect("SharedStrings strings poisoned")
            .len()
    }

    /// Whether the pool is empty.
    pub fn is_empty(&self) -> bool {
        self.strings
            .lock()
            .expect("SharedStrings strings poisoned")
            .is_empty()
    }

    /// Extract the final string table. When no other clones are
    /// alive, unwraps in place; otherwise clones out the contents.
    /// The latter path runs only when a caller forgets to drop
    /// rewrite rules before reading back.
    pub fn into_vec(self) -> Vec<String> {
        let Self { strings, dedup: _ } = self;
        match Arc::try_unwrap(strings) {
            Ok(mutex) => mutex
                .into_inner()
                .expect("SharedStrings strings poisoned"),
            Err(arc) => arc
                .lock()
                .expect("SharedStrings strings poisoned")
                .clone(),
        }
    }

    /// Write the pool back into an `ir`, consuming this handle.
    ///
    /// Use after saturation completes and rewrite rules referencing
    /// the same pool have been dropped.
    pub fn write_back(self, ir: &mut GrammarIR) {
        ir.strings = self.into_vec();
    }
}

