#[cfg(not(target_arch = "wasm32"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[cfg(feature = "bbnf-grammar")]
pub mod bbnf;
#[cfg(feature = "bnf-grammar")]
pub mod bnf;
#[cfg(feature = "css-grammar")]
pub mod css;
#[cfg(feature = "ebnf-grammar")]
pub mod ebnf;
#[cfg(feature = "sheets-grammar")]
pub mod google_sheets;
#[cfg(feature = "json-grammar")]
pub mod json;

#[cfg(feature = "vm")]
pub mod vm;

/// Configuration for the prettifier output.
#[derive(Debug, Clone)]
pub struct PrinterConfig {
    pub max_width: usize,
    pub indent: usize,
    pub use_tabs: bool,
}

impl Default for PrinterConfig {
    fn default() -> Self {
        Self {
            max_width: 80,
            indent: 4,
            use_tabs: false,
        }
    }
}

impl PrinterConfig {
    pub fn new(max_width: usize, indent: usize) -> Self {
        Self {
            max_width,
            indent,
            use_tabs: false,
        }
    }

    pub fn to_printer(&self) -> pprint::Printer {
        pprint::Printer::new(self.max_width, self.indent, self.use_tabs)
    }
}
