#![forbid(unsafe_code)]

#[path = "type_kernel/infer.rs"]
mod infer;
#[path = "type_kernel/layout.rs"]
mod layout;
#[path = "type_kernel/project.rs"]
mod project;
#[path = "type_kernel/scheme.rs"]
mod scheme;
#[path = "type_kernel/solve.rs"]
mod solve;
#[path = "type_kernel/solve/row.rs"]
mod solve_row;
#[path = "type_kernel/solve/unify.rs"]
mod solve_unify;
#[path = "type_kernel/solve/zonk.rs"]
mod solve_zonk;
#[path = "type_kernel/store.rs"]
mod store;
