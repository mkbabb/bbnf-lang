/**
 * @mkbabb/latex-paper/transform — AST→HTML transformer entry point.
 *
 * Re-exports everything from the main entry plus the transformer.
 */

export * from "./index";

export {
    Transformer,
    transformDocument,
    cleanRawLatex,
    validateOutput,
    DEFAULT_MACROS,
} from "./transform/html";
export type {
    TransformOptions,
    TransformResult,
    ValidationIssue,
} from "./transform/html";
export type { PaperLabelInfo } from "./types/output";
