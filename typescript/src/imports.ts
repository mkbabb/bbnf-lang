/**
 * Import resolution types and AST merging for BBNF grammars.
 *
 * Provides type definitions ({@link ImportError}, {@link ModuleRegistry}, etc.)
 * and merge utilities ({@link mergeModuleAST}, {@link mergeModuleRecovers},
 * {@link mergeModulePretties}) for combining multi-file grammars.
 *
 * Loading and file-system operations live in `imports-loader.ts`.
 */

import type { RecoverDirective, PrettyDirective, ProductionRule } from "./types.js";
import { fullModuleRules } from "./imports-loader.js";

// Re-export loader functions so existing consumers don't break.
export {
    loadModuleGraphSync,
    loadModuleGraph,
    resolveImportsFor,
    fullModuleRules,
} from "./imports-loader.js";

// ---------------------------------------------------------------------------
// Error types
// ---------------------------------------------------------------------------

export type ImportError =
    | { type: "FileNotFound"; path: string; importedFrom: string }
    | { type: "CircularImport"; path: string; chain: string[] }
    | {
          type: "MissingRule";
          ruleName: string;
          path: string;
          importedFrom: string;
      }
    | {
          type: "NameConflict";
          ruleName: string;
          sourceA: string;
          sourceB: string;
          importedFrom: string;
      }
    | { type: "ParseError"; path: string; message: string };

/**
 * Format an {@link ImportError} into a human-readable string.
 */
export function formatImportError(err: ImportError): string {
    switch (err.type) {
        case "FileNotFound":
            return `File not found: \`${err.path}\` (imported from \`${err.importedFrom}\`)`;
        case "CircularImport":
            return `Circular import: \`${err.path}\` (chain: ${err.chain.join(" → ")} → ${err.path})`;
        case "MissingRule":
            return `Rule \`${err.ruleName}\` not found in \`${err.path}\` (imported from \`${err.importedFrom}\`)`;
        case "NameConflict":
            return `Name conflict: rule \`${err.ruleName}\` is imported from both \`${err.sourceA}\` and \`${err.sourceB}\` in \`${err.importedFrom}\``;
        case "ParseError":
            return `Parse error in \`${err.path}\`: ${err.message}`;
    }
}

// ---------------------------------------------------------------------------
// Module registry
// ---------------------------------------------------------------------------

/** Parsed AST type alias (re-exported for convenience). */
export type { AST } from "./types.js";

/** Per-file module data after parsing. */
export interface ModuleData {
    /** Source text. */
    source: string;
    /** Import directives found in this file. */
    imports: import("./types.js").ImportDirective[];
    /** Recover directives found in this file. */
    recovers: RecoverDirective[];
    /** No-collapse directives found in this file. */
    no_collapses: import("./types.js").NoCollapseDirective[];
    /** Pretty directives found in this file. */
    pretties: PrettyDirective[];
    /** The parsed AST (rule name → ProductionRule). */
    rules: import("./types.js").AST;
    /** Names of rules defined locally in this file. */
    localRuleNames: string[];
}

/** A resolved import: which rules are visible and where they come from. */
export interface ResolvedImport {
    /** Source file path (canonical). */
    source: string;
    /** Rule names imported from this source. */
    ruleNames: string[];
}

/** Registry of all loaded modules in an import graph. */
export interface ModuleRegistry {
    /** Canonical path → module data. */
    modules: Map<string, ModuleData>;
    /** Canonical path → resolved imports (which rules are visible from imports). */
    resolvedImports: Map<string, ResolvedImport[]>;
    /** All errors encountered during loading. */
    errors: ImportError[];
}

/**
 * Get all imported rule names for a file (flattened).
 */
export function importedRuleNames(
    registry: ModuleRegistry,
    filePath: string,
): Set<string> {
    const names = new Set<string>();
    const imports = registry.resolvedImports.get(filePath);
    if (imports) {
        for (const imp of imports) {
            for (const name of imp.ruleNames) {
                names.add(name);
            }
        }
    }
    return names;
}

// ---------------------------------------------------------------------------
// AST merging
// ---------------------------------------------------------------------------

/**
 * Merge imported rules into a single AST for the entry file.
 *
 * Imported rules come first, then local rules (so local rules override on
 * conflict). For each resolved import, copy the rules from the source module
 * into the merged AST.
 */
export function mergeModuleAST(
    registry: ModuleRegistry,
    entryPath: string,
): import("./types.js").AST {
    const merged = new Map<string, ProductionRule>() as import("./types.js").AST;

    const entryModule = registry.modules.get(entryPath);
    if (!entryModule) {
        return merged;
    }

    // First: add imported rules.
    const imports = registry.resolvedImports.get(entryPath);
    if (imports) {
        for (const imp of imports) {
            // Build the full rule set for the source module (local + its own imports).
            const { rules: sourceRules } = fullModuleRules(imp.source, registry);
            for (const ruleName of imp.ruleNames) {
                const rule = sourceRules.get(ruleName);
                if (rule) {
                    merged.set(ruleName, rule);
                }
            }
        }
    }

    // Second: add local rules (overrides imported on conflict).
    for (const [name, rule] of entryModule.rules) {
        merged.set(name, rule);
    }

    return merged;
}

/**
 * Collect all @recover directives from the entry module and its transitive imports.
 * Only recovers from the entry module itself are returned (imported modules'
 * recovers are not carried — recovery is local to where it's declared).
 */
export function mergeModuleRecovers(
    registry: ModuleRegistry,
    entryPath: string,
): RecoverDirective[] {
    const entryModule = registry.modules.get(entryPath);
    if (!entryModule) {
        return [];
    }
    return [...entryModule.recovers];
}

/**
 * Collect all @pretty directives from the entry module and its transitive imports.
 * Pretties from imported modules are also included (merged, entry overrides).
 */
export function mergeModulePretties(
    registry: ModuleRegistry,
    entryPath: string,
): PrettyDirective[] {
    const pretties: PrettyDirective[] = [];
    const seen = new Set<string>();

    // Collect from imports first.
    const imports = registry.resolvedImports.get(entryPath);
    if (imports) {
        for (const imp of imports) {
            const sourceModule = registry.modules.get(imp.source);
            if (sourceModule) {
                for (const p of sourceModule.pretties) {
                    if (!seen.has(p.ruleName)) {
                        seen.add(p.ruleName);
                        pretties.push(p);
                    }
                }
            }
        }
    }

    // Entry module pretties override imports.
    const entryModule = registry.modules.get(entryPath);
    if (entryModule) {
        for (const p of entryModule.pretties) {
            if (seen.has(p.ruleName)) {
                // Replace imported version with entry version.
                const idx = pretties.findIndex(x => x.ruleName === p.ruleName);
                if (idx >= 0) pretties[idx] = p;
            } else {
                pretties.push(p);
            }
        }
    }

    return pretties;
}
