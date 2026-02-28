/**
 * Import resolution for BBNF grammars.
 *
 * Provides a {@link ModuleRegistry} that loads a graph of `.bbnf` files connected
 * by `@import` directives. Each file is parsed once, and imports are resolved to
 * produce a per-file namespace of visible rules.
 *
 * Mirrors the Rust implementation in `rust/bbnf/src/imports.rs`.
 */

import * as path from "node:path";
import * as fs from "node:fs";

import type { ImportDirective, AST, ProductionRule, Expression } from "./types.js";
import { BBNFToASTWithImports } from "./parse.js";

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

/** Per-file module data after parsing. */
export interface ModuleData {
    /** Source text. */
    source: string;
    /** Import directives found in this file. */
    imports: ImportDirective[];
    /** The parsed AST (rule name → ProductionRule). */
    rules: AST;
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
// Path helpers
// ---------------------------------------------------------------------------

/**
 * Resolve an import path relative to the importing file's directory.
 * Appends `.bbnf` if no extension is given.
 */
function resolveImportPath(dir: string, importPath: string): string {
    const joined = path.resolve(dir, importPath);
    const ext = path.extname(joined);
    if (!ext) {
        return joined + ".bbnf";
    }
    return joined;
}

/**
 * Canonicalize a path. When a custom readFileSync is provided we skip
 * `fs.realpathSync` (since the file may not exist on disk) and just
 * use `path.resolve`.
 */
function canonicalize(filePath: string, useRealFs: boolean): string {
    if (useRealFs) {
        try {
            return fs.realpathSync(filePath);
        } catch {
            // Fall through to path.resolve if the file doesn't exist yet.
            return path.resolve(filePath);
        }
    }
    return path.resolve(filePath);
}

// ---------------------------------------------------------------------------
// Synchronous loader
// ---------------------------------------------------------------------------

/**
 * Load a module graph starting from an entry file (synchronous).
 *
 * Performs a DFS traversal of `@import` directives, parsing each file exactly
 * once (canonical path dedup). Returns a {@link ModuleRegistry} with all
 * modules and resolved imports. Errors are collected rather than failing on
 * first error.
 *
 * @param entryPath - Path to the root `.bbnf` file.
 * @param readFileSync - Optional file reader for testing / browser use.
 *   When provided, `fs.realpathSync` is skipped and paths are resolved
 *   via `path.resolve` only.
 */
export function loadModuleGraphSync(
    entryPath: string,
    readFileSync?: (path: string) => string,
): ModuleRegistry {
    const useRealFs = !readFileSync;
    const reader = readFileSync ?? ((p: string) => fs.readFileSync(p, "utf-8"));

    const entry = canonicalize(path.resolve(entryPath), useRealFs);

    const registry: ModuleRegistry = {
        modules: new Map(),
        resolvedImports: new Map(),
        errors: [],
    };

    const visited = new Set<string>();

    loadRecursiveSync(entry, "<entry>", registry, visited, reader, useRealFs);

    // Phase 2: resolve imports for every visited module.
    for (const filePath of visited) {
        resolveImportsFor(filePath, registry, useRealFs);
    }

    return registry;
}

function loadRecursiveSync(
    filePath: string,
    importedFrom: string,
    registry: ModuleRegistry,
    visited: Set<string>,
    reader: (path: string) => string,
    useRealFs: boolean,
): void {
    // Already parsed (or currently being parsed — cycle). Return harmlessly.
    if (visited.has(filePath)) {
        return;
    }

    // Read source.
    let source: string;
    try {
        source = reader(filePath);
    } catch {
        registry.errors.push({
            type: "FileNotFound",
            path: filePath,
            importedFrom,
        });
        return;
    }

    // Parse.
    const result = BBNFToASTWithImports(source);
    if (result.length < 2 || !result[1]) {
        registry.errors.push({
            type: "ParseError",
            path: filePath,
            message: "Failed to parse grammar",
        });
        return;
    }

    const parsed = result[1];
    const localRuleNames = [...parsed.rules.keys()];

    // Register BEFORE recursing (partial-init, like Python module loading).
    // This allows cyclic imports to find the module already registered.
    visited.add(filePath);
    registry.modules.set(filePath, {
        source,
        imports: parsed.imports,
        rules: parsed.rules,
        localRuleNames,
    });

    // Recurse on imports. Cycles find the file already in visited and return.
    const dir = path.dirname(filePath);
    for (const imp of parsed.imports) {
        const importPath = resolveImportPath(dir, imp.path);
        const canonical = canonicalize(importPath, useRealFs);
        loadRecursiveSync(canonical, filePath, registry, visited, reader, useRealFs);
    }
}

// ---------------------------------------------------------------------------
// Dependency collection (local copy to avoid circular import with analysis.ts)
// ---------------------------------------------------------------------------

function collectDepsFromExpr(expr: Expression, deps: Set<string>): void {
    if (!expr?.type) return;
    if (expr.type === "nonterminal") {
        deps.add(expr.value as string);
        return;
    }
    if (expr.value instanceof Array) {
        for (const child of expr.value) {
            collectDepsFromExpr(child as Expression, deps);
        }
    } else if (
        expr.value &&
        typeof expr.value === "object" &&
        "type" in (expr.value as object)
    ) {
        collectDepsFromExpr(expr.value as Expression, deps);
    }
}

/**
 * Compute the transitive closure of local dependencies starting from `ruleName`
 * within the given module. Returns a set of all rule names that `ruleName`
 * transitively depends on (including itself).
 */
function transitiveLocalDeps(ruleName: string, moduleData: ModuleData): Set<string> {
    const deps = new Set<string>();
    const queue = [ruleName];
    while (queue.length > 0) {
        const name = queue.pop()!;
        if (deps.has(name)) continue;
        deps.add(name);
        const rule = moduleData.rules.get(name);
        if (!rule) continue;
        const refs = new Set<string>();
        collectDepsFromExpr(rule.expression, refs);
        for (const ref of refs) {
            if (moduleData.localRuleNames.includes(ref) && !deps.has(ref)) {
                queue.push(ref);
            }
        }
    }
    return deps;
}

// ---------------------------------------------------------------------------
// Import resolution (shared by sync and async)
// ---------------------------------------------------------------------------

/**
 * Resolve imports for a single module in the registry.
 *
 * For each import directive in the module at `filePath`:
 * - Glob imports (`@import "file"`) bring in all local rules from the target.
 * - Selective imports (`@import { a, b } from "file"`) verify each named rule
 *   exists and push a `MissingRule` error if not.
 * - Name conflicts (same rule imported from two different sources) are reported.
 * - Imports are **non-transitive**: if A imports B and B imports C, A cannot
 *   see C's rules.
 */
export function resolveImportsFor(
    filePath: string,
    registry: ModuleRegistry,
    useRealFs: boolean = true,
): void {
    const module = registry.modules.get(filePath);
    if (!module) {
        return;
    }

    const dir = path.dirname(filePath);
    const resolved: ResolvedImport[] = [];
    // Track which names have been imported and from where (for conflict detection).
    const importedNames = new Map<string, string>();

    for (const imp of module.imports) {
        const importPath = resolveImportPath(dir, imp.path);
        const canonical = canonicalize(importPath, useRealFs);

        const target = registry.modules.get(canonical);
        if (!target) {
            // Already reported as FileNotFound or ParseError during loading.
            continue;
        }

        let ruleNames: string[];

        if (imp.items && imp.items.length > 0) {
            // Selective import: verify each named rule exists, then unfurl
            // transitive local dependencies so callers don't have to
            // manually import every sub-rule.
            const verified: string[] = [];
            for (const name of imp.items) {
                if (target.localRuleNames.includes(name)) {
                    verified.push(name);
                } else {
                    registry.errors.push({
                        type: "MissingRule",
                        ruleName: name,
                        path: canonical,
                        importedFrom: filePath,
                    });
                }
            }
            // Expand with transitive deps.
            const expanded = new Set<string>();
            for (const name of verified) {
                for (const dep of transitiveLocalDeps(name, target)) {
                    expanded.add(dep);
                }
            }
            ruleNames = [...expanded];
        } else {
            // Glob import: all local rules.
            ruleNames = [...target.localRuleNames];
        }

        // Check for name conflicts.
        for (const name of ruleNames) {
            const prevSource = importedNames.get(name);
            if (prevSource !== undefined) {
                registry.errors.push({
                    type: "NameConflict",
                    ruleName: name,
                    sourceA: prevSource,
                    sourceB: canonical,
                    importedFrom: filePath,
                });
            } else {
                importedNames.set(name, canonical);
            }
        }

        resolved.push({
            source: canonical,
            ruleNames,
        });
    }

    registry.resolvedImports.set(filePath, resolved);
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
): AST {
    const merged = new Map<string, ProductionRule>() as AST;

    const entryModule = registry.modules.get(entryPath);
    if (!entryModule) {
        return merged;
    }

    // First: add imported rules.
    const imports = registry.resolvedImports.get(entryPath);
    if (imports) {
        for (const imp of imports) {
            const sourceModule = registry.modules.get(imp.source);
            if (!sourceModule) {
                continue;
            }
            for (const ruleName of imp.ruleNames) {
                const rule = sourceModule.rules.get(ruleName);
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

// ---------------------------------------------------------------------------
// Asynchronous loader
// ---------------------------------------------------------------------------

/**
 * Load a module graph starting from an entry file (asynchronous).
 *
 * Same algorithm as {@link loadModuleGraphSync} but uses async file reading.
 *
 * @param entryPath - Path to the root `.bbnf` file.
 * @param readFile - Optional async file reader for testing / browser use.
 */
export async function loadModuleGraph(
    entryPath: string,
    readFile?: (path: string) => Promise<string>,
): Promise<ModuleRegistry> {
    const useRealFs = !readFile;
    const reader =
        readFile ??
        ((p: string) => fs.promises.readFile(p, "utf-8") as Promise<string>);

    const entry = canonicalize(path.resolve(entryPath), useRealFs);

    const registry: ModuleRegistry = {
        modules: new Map(),
        resolvedImports: new Map(),
        errors: [],
    };

    const visited = new Set<string>();

    await loadRecursiveAsync(
        entry,
        "<entry>",
        registry,
        visited,
        reader,
        useRealFs,
    );

    // Phase 2: resolve imports for every visited module.
    for (const filePath of visited) {
        resolveImportsFor(filePath, registry, useRealFs);
    }

    return registry;
}

async function loadRecursiveAsync(
    filePath: string,
    importedFrom: string,
    registry: ModuleRegistry,
    visited: Set<string>,
    reader: (path: string) => Promise<string>,
    useRealFs: boolean,
): Promise<void> {
    // Already parsed (or currently being parsed — cycle). Return harmlessly.
    if (visited.has(filePath)) {
        return;
    }

    // Read source.
    let source: string;
    try {
        source = await reader(filePath);
    } catch {
        registry.errors.push({
            type: "FileNotFound",
            path: filePath,
            importedFrom,
        });
        return;
    }

    // Parse.
    const result = BBNFToASTWithImports(source);
    if (result.length < 2 || !result[1]) {
        registry.errors.push({
            type: "ParseError",
            path: filePath,
            message: "Failed to parse grammar",
        });
        return;
    }

    const parsed = result[1];
    const localRuleNames = [...parsed.rules.keys()];

    // Register BEFORE recursing (partial-init, like Python module loading).
    visited.add(filePath);
    registry.modules.set(filePath, {
        source,
        imports: parsed.imports,
        rules: parsed.rules,
        localRuleNames,
    });

    // Recurse on imports. Cycles find the file already in visited and return.
    const dir = path.dirname(filePath);
    for (const imp of parsed.imports) {
        const importPath = resolveImportPath(dir, imp.path);
        const canonical = canonicalize(importPath, useRealFs);
        await loadRecursiveAsync(
            canonical,
            filePath,
            registry,
            visited,
            reader,
            useRealFs,
        );
    }
}
