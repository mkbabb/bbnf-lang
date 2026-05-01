mod server;

// Re-export bbnf-analysis modules for use within this crate.
pub(crate) mod analysis {
    pub use bbnf_analysis::analysis::*;
}
pub(crate) mod state {
    pub use bbnf_analysis::state::*;
}
pub(crate) mod features {
    pub use bbnf_analysis::features::*;
}

use server::BbnfLanguageServer;
use tower_lsp_server::{LspService, Server};

#[tokio::main]
async fn main() {
    // --dap flag: run as a Debug Adapter Protocol server instead of LSP.
    if std::env::args().any(|a| a == "--dap") {
        bbnf_lsp::dap::serve_dap();
        return;
    }

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(BbnfLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
