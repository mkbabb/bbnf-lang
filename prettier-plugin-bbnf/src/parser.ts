import { BBNFToAST, topologicalSort } from "@mkbabb/bbnf-lang";

export function locStart(node: object): number {
    return 0;
}
export function locEnd(node: object): number {
    return 0;
}
export function preprocess(text: string, options: object): string {
    return text;
}

export function parse(text: string, parsers: object, options: object) {
    let [parser, ast] = BBNFToAST(text);

    if (parser.state.isError) {
        // @ts-ignore
        throw new Error(`Error parsing BBNF: ${parser.state}`, {
            cause: parser,
        });
    }

    // ast = removeAllLeftRecursion(ast);
    let prettierAST;
    // @ts-ignore
    if (options.sort) {
        prettierAST = [...topologicalSort(ast).entries()].reverse();
    } else {
        prettierAST = [...ast.entries()];
    }

    return prettierAST
        .filter(([key]) => key)
        .reduce((acc, [key, value]) => {
            return acc.set(key, value);
        }, new Map());
}
