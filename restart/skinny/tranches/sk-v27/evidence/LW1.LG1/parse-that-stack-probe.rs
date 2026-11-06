use parse_that::parsers::css::css_parser;
fn need(css: &'static str) -> usize {
    let (mut lo, mut hi) = (16 * 1024usize, 64 * 1024 * 1024usize);
    while hi - lo > 8 * 1024 {
        let mid = (lo + hi) / 2;
        let ok = std::process::Command::new(std::env::current_exe().unwrap())
            .arg(mid.to_string()).arg(css).status().unwrap().success();
        if ok { hi = mid } else { lo = mid }
    }
    hi
}
fn main() {
    let a: Vec<String> = std::env::args().collect();
    if a.len() == 3 {
        let sz: usize = a[1].parse().unwrap();
        let css: &'static str = Box::leak(a[2].clone().into_boxed_str());
        let h = std::thread::Builder::new().stack_size(sz).spawn(move || { let p = css_parser(); if css != "-" { let _ = p.parse(css); } }).unwrap();
        std::process::exit(if h.join().is_ok() { 0 } else { 1 });
    }
    for c in ["-", ".a{}", ".sm { display: block; }", "@media screen { }", "@media screen { .a{} }", "@media screen { @media print { } }", "@media screen { @media print { .a{} } }", "@supports (a:b) { @supports (a:b) { .a{} } }", "@foo { @bar { .a{} } }"] {
        println!("{c:50}: {} KiB", need(c) / 1024);
    }
}
