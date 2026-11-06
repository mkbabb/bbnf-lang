use std::alloc::{GlobalAlloc, Layout, System};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};

use xtask::architecture::manifest::ConstellationManifest;
use xtask::architecture::test_support::{
    RunEpoch, RunEpochReceipt, ToolSelection, capture_current_exporter,
};

struct TrackingAllocator;

static ACTIVE_HEAP_BYTES: AtomicU64 = AtomicU64::new(0);
static PEAK_HEAP_BYTES: AtomicU64 = AtomicU64::new(0);

pub(crate) fn assert_epoch(
    epoch: &RunEpoch,
    matched: u8,
    pending: bool,
    cancelled_by_primary_failure: bool,
) {
    assert_eq!(
        epoch.receipt().expect("epoch receipt"),
        RunEpochReceipt {
            matched,
            pending,
            cancelled_by_primary_failure,
        }
    );
}

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        if !pointer.is_null() {
            add_heap(layout.size() as u64);
        }
        pointer
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        if !pointer.is_null() {
            add_heap(layout.size() as u64);
        }
        pointer
    }

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        ACTIVE_HEAP_BYTES.fetch_sub(layout.size() as u64, Ordering::Relaxed);
        unsafe { System.dealloc(pointer, layout) };
    }

    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let resized = unsafe { System.realloc(pointer, layout, new_size) };
        if !resized.is_null() {
            if new_size >= layout.size() {
                add_heap((new_size - layout.size()) as u64);
            } else {
                ACTIVE_HEAP_BYTES.fetch_sub((layout.size() - new_size) as u64, Ordering::Relaxed);
            }
        }
        resized
    }
}

#[global_allocator]
static ALLOCATOR: TrackingAllocator = TrackingAllocator;

fn add_heap(bytes: u64) {
    let active = ACTIVE_HEAP_BYTES.fetch_add(bytes, Ordering::Relaxed) + bytes;
    PEAK_HEAP_BYTES.fetch_max(active, Ordering::Relaxed);
}

fn take_heap_usage() -> (u64, u64) {
    let active = ACTIVE_HEAP_BYTES.load(Ordering::Relaxed);
    let peak = PEAK_HEAP_BYTES.swap(active, Ordering::Relaxed);
    (active, peak)
}

fn tool_selection() -> ToolSelection {
    static SELECTION: std::sync::OnceLock<ToolSelection> = std::sync::OnceLock::new();
    SELECTION
        .get_or_init(|| ToolSelection::capture().expect("shared test tool selection"))
        .clone()
}

#[path = "architecture/artifact.rs"]
mod artifact;
#[path = "architecture/cargo.rs"]
mod cargo;
#[path = "architecture/evidence.rs"]
mod evidence;
#[path = "architecture/frontend.rs"]
mod frontend;
#[path = "architecture/hash.rs"]
mod hash;
#[path = "architecture/inventory.rs"]
mod inventory;
#[path = "architecture/manifest.rs"]
mod manifest_tests;
#[path = "architecture/project.rs"]
mod project;
#[path = "architecture/resource.rs"]
mod resource;
#[path = "architecture/resource_successor.rs"]
mod resource_successor;
#[path = "architecture/rust.rs"]
mod rust;
#[path = "architecture/schema.rs"]
mod schema;
#[path = "architecture/worktree.rs"]
mod worktree;

fn manifest_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("architecture/constellation-v3.toml")
}

fn manifest() -> ConstellationManifest {
    ConstellationManifest::load(&manifest_path()).expect("P3 manifest")
}
