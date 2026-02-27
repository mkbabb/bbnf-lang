import * as path from "node:path";
import * as os from "node:os";
import type { ExtensionContext } from "vscode";
import * as vscode from "vscode";
import {
    LanguageClient,
    type LanguageClientOptions,
    type ServerOptions,
} from "vscode-languageclient/node.js";

let client: LanguageClient | undefined;

function getServerPath(context: ExtensionContext): string {
    // 1. VS Code setting.
    const config = vscode.workspace.getConfiguration("BBNF");
    const customPath = config.get<string>("server.path");
    if (customPath) {
        return customPath;
    }

    // 2. Environment variable (set by launch.json during development).
    const envPath = process.env.BBNF_SERVER_PATH;
    if (envPath) {
        return envPath;
    }

    // 3. Bundled binary inside extension.
    const ext = os.platform() === "win32" ? ".exe" : "";
    const bundled = path.join(context.extensionPath, "server", `bbnf-lsp${ext}`);
    try {
        const fs = require("node:fs");
        if (fs.existsSync(bundled)) {
            return bundled;
        }
    } catch {}

    // 4. Fallback: binary next to extension (dev layout).
    return path.join(context.extensionPath, "..", "server", `bbnf-lsp${ext}`);
}

export async function activate(context: ExtensionContext) {
    const serverPath = getServerPath(context);

    const serverOptions: ServerOptions = {
        run: { command: serverPath, args: [] },
        debug: { command: serverPath, args: [] },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ language: "bbnf", scheme: "file" }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher("**/*.bbnf"),
        },
    };

    client = new LanguageClient(
        "bbnf-lsp",
        "BBNF Language Server",
        serverOptions,
        clientOptions,
    );

    // Register restart command.
    context.subscriptions.push(
        vscode.commands.registerCommand("bbnf.restartServer", async () => {
            if (client) {
                await client.restart();
                vscode.window.showInformationMessage(
                    "BBNF language server restarted.",
                );
            }
        }),
    );

    await client.start();
}

export async function deactivate() {
    if (client) {
        await client.stop();
        client = undefined;
    }
}
