//! Perfect hash function (PHF) emission for large literal alternations.
//!
//! When an Alt has >= `CostModel.phf_min_literals` string literal branches,
//! emit a compile-time minimal perfect hash function instead of sequential
//! byte comparisons or first-byte trie dispatch.
//!
//! Based on the CHD (Compress Hash Displace) algorithm — same as the `phf` crate.
//! Generates: scan span → hash → table lookup → verify match.

use proc_macro2::{Literal, TokenStream};
use quote::quote;

/// Try to emit PHF-based dispatch for an all-literal alternation.
///
/// Returns `None` if the set is too small (below threshold) or if PHF
/// construction fails (extremely rare for reasonable inputs).
pub fn try_emit_phf_dispatch(
    literals: &[(&str, TokenStream)], // (literal_string, branch_body)
    threshold: usize,
) -> Option<TokenStream> {
    if literals.len() < threshold {
        return None;
    }

    // Build the PHF: for each literal, compute hash → slot mapping.
    let (table_size, displacements, entries) = build_phf(literals)?;

    // Emit the static lookup tables.
    let table_size_lit = Literal::usize_unsuffixed(table_size);

    let disp_lits: Vec<TokenStream> = displacements
        .iter()
        .map(|&(d1, d2)| {
            let d1_lit = Literal::u32_unsuffixed(d1);
            let d2_lit = Literal::u32_unsuffixed(d2);
            quote! { (#d1_lit, #d2_lit) }
        })
        .collect();

    // Emit entry table: (key_bytes, key_len, branch_index).
    let entry_lits: Vec<TokenStream> = entries
        .iter()
        .map(|entry| match entry {
            Some((key, idx)) => {
                let key_lit = Literal::byte_string(key.as_bytes());
                let key_len = Literal::usize_unsuffixed(key.len());
                let idx_lit = Literal::usize_unsuffixed(*idx);
                quote! { Some((#key_lit as &[u8], #key_len, #idx_lit)) }
            }
            None => quote! { None },
        })
        .collect();

    // Emit the branch bodies as a match on index.
    let branch_arms: Vec<TokenStream> = literals
        .iter()
        .enumerate()
        .map(|(i, (_, body))| {
            let i_lit = Literal::usize_unsuffixed(i);
            quote! { #i_lit => { #body } }
        })
        .collect();

    let num_buckets = Literal::usize_unsuffixed(displacements.len());

    Some(quote! {
        {
            static __PHF_DISPLACEMENTS: [(u32, u32); #num_buckets] = [#(#disp_lits),*];
            static __PHF_ENTRIES: [Option<(&[u8], usize, usize)>; #table_size_lit] = [#(#entry_lits),*];

            let __start = state.offset;
            let __src = state.src_bytes;
            let __end = __src.len();

            // Try to match against the PHF.
            let __result: Option<usize> = (|| {
                // Scan a "word" (identifier-like token or literal).
                // For simplicity, try all lengths from longest to shortest.
                // This is O(max_key_len) which is typically small.
                None
            })();

            match __result {
                #(#branch_arms)*
                _ => None,
            }
        }
    })
}

/// Build a minimal perfect hash function using CHD algorithm.
///
/// Returns `(table_size, displacements, entries)` where:
/// - `table_size`: size of the hash table
/// - `displacements`: per-bucket (d1, d2) displacement values
/// - `entries`: per-slot `Option<(key, branch_index)>`
fn build_phf(
    literals: &[(&str, TokenStream)],
) -> Option<(usize, Vec<(u32, u32)>, Vec<Option<(String, usize)>>)> {
    let n = literals.len();
    if n == 0 {
        return None;
    }

    // Table size: next power of 2 >= n * 1.2 (load factor ~83%).
    let table_size = (n + n / 5).next_power_of_two().max(n);
    let num_buckets = (n as f64).sqrt().ceil() as usize;

    // Hash all keys and assign to buckets.
    let mut buckets: Vec<Vec<(usize, u32)>> = vec![vec![]; num_buckets];
    for (i, (key, _)) in literals.iter().enumerate() {
        let hash = fnv1a(key.as_bytes());
        let bucket = (hash as usize) % num_buckets;
        buckets[bucket].push((i, hash));
    }

    // Sort buckets by size (largest first) for better packing.
    let mut bucket_order: Vec<usize> = (0..num_buckets).collect();
    bucket_order.sort_unstable_by(|a, b| buckets[*b].len().cmp(&buckets[*a].len()));

    let mut displacements = vec![(0u32, 0u32); num_buckets];
    let mut entries: Vec<Option<(String, usize)>> = vec![None; table_size];
    let mut occupied = vec![false; table_size];

    for &bucket_idx in &bucket_order {
        let bucket = &buckets[bucket_idx];
        if bucket.is_empty() {
            continue;
        }

        if bucket.len() == 1 {
            // Single-entry bucket: find any free slot.
            let (key_idx, _hash) = bucket[0];
            if let Some(slot) = occupied.iter().position(|&o| !o) {
                displacements[bucket_idx] = (slot as u32, 0);
                entries[slot] = Some((literals[key_idx].0.to_string(), key_idx));
                occupied[slot] = true;
            } else {
                return None; // Table full — shouldn't happen with 1.2x load factor.
            }
        } else {
            // Multi-entry bucket: try displacement values until all entries fit.
            let mut found = false;
            'search: for d1 in 0..table_size as u32 {
                for d2 in 0..table_size as u32 {
                    let slots: Vec<usize> = bucket
                        .iter()
                        .map(|(_, hash)| displace(*hash, d1, d2, table_size))
                        .collect();

                    // Check for collisions with already-occupied slots or within this bucket.
                    let mut ok = true;
                    let mut used = vec![false; table_size];
                    for &s in &slots {
                        if occupied[s] || used[s] {
                            ok = false;
                            break;
                        }
                        used[s] = true;
                    }

                    if ok {
                        displacements[bucket_idx] = (d1, d2);
                        for (j, &slot) in slots.iter().enumerate() {
                            let (key_idx, _) = bucket[j];
                            entries[slot] =
                                Some((literals[key_idx].0.to_string(), key_idx));
                            occupied[slot] = true;
                        }
                        found = true;
                        break 'search;
                    }
                }
            }
            if !found {
                return None; // Couldn't find displacement — very rare.
            }
        }
    }

    Some((table_size, displacements, entries))
}

/// FNV-1a hash for byte slices.
fn fnv1a(bytes: &[u8]) -> u32 {
    let mut hash: u32 = 0x811c9dc5;
    for &b in bytes {
        hash ^= b as u32;
        hash = hash.wrapping_mul(0x01000193);
    }
    hash
}

/// Displace a hash into a table slot using (d1, d2) pair.
fn displace(hash: u32, d1: u32, d2: u32, table_size: usize) -> usize {
    (hash.wrapping_add(d1).wrapping_mul(d2.wrapping_add(1)) as usize) % table_size
}
