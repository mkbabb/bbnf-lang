use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};

use bbnf_type_kernel_v4::*;

mod type_kernel {
    pub mod infer;
    pub mod layout;
    pub mod project;
    pub mod scheme;
    pub mod solve;
    pub mod store;
}

static TEMP_ID: AtomicU64 = AtomicU64::new(0);

fn operations() -> OperationContext {
    OperationContext::new(20_000_000)
}

fn temp_dir(label: &str) -> PathBuf {
    let id = TEMP_ID.fetch_add(1, Ordering::Relaxed);
    let path = std::env::temp_dir().join(format!(
        "bbnf-type-kernel-v4-{label}-{}-{id}",
        std::process::id()
    ));
    std::fs::create_dir_all(&path).expect("create temporary directory");
    path
}

fn toolchain() -> ToolchainIdentity {
    ToolchainIdentity {
        target: "aarch64-apple-darwin".to_owned(),
        rustc: "rustc 1.96.0-nightly (02c7f9bec 2026-04-10)".to_owned(),
        typescript: "TypeScript 5.9.3".to_owned(),
    }
}

fn primitive_solution(primitive: Primitive, name: &str) -> TypeSolution {
    let mut operations = operations();
    let mut builder = ProblemBuilder::new();
    let ty = builder
        .primitive(primitive, &mut operations)
        .expect("primitive");
    let (problem, _) = builder
        .close([(ExportKey::new(name), ty)], &mut operations)
        .expect("close");
    solve(problem, &mut operations).expect("solve")
}
