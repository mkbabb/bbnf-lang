import { describe, it, expect } from "vitest";
// Import document.ts first to register inlineNode
import "../../src/grammar/document";
import { environment } from "../../src/grammar/environments";

describe("environment parsers", () => {
    it("parses theorem environment", () => {
        const result = environment.parse(
            "\\begin{theorem}[Name]\nSome theorem body.\n\\end{theorem}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("theorem");
        if (result!.type === "theorem") {
            expect(result.envType).toBe("theorem");
            expect(result.name).toBeDefined();
        }
    });

    it("parses definition environment", () => {
        const result = environment.parse(
            "\\begin{definition}\nA definition.\n\\end{definition}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("theorem");
        if (result!.type === "theorem") {
            expect(result.envType).toBe("definition");
        }
    });

    it("parses enumerate", () => {
        const result = environment.parse(
            "\\begin{enumerate}\n\\item First\n\\item Second\n\\end{enumerate}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("list");
        if (result!.type === "list") {
            expect(result.ordered).toBe(true);
            // Items: may include leading whitespace as empty item
            expect(result.items.length).toBeGreaterThanOrEqual(2);
        }
    });

    it("parses itemize", () => {
        const result = environment.parse(
            "\\begin{itemize}\n\\item Bullet\n\\end{itemize}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("list");
        if (result!.type === "list") {
            expect(result.ordered).toBe(false);
        }
    });

    it("parses figure environment", () => {
        const result = environment.parse(
            "\\begin{figure}\n\\includegraphics{test.png}\n\\caption{A figure.}\n\\label{fig:test}\n\\end{figure}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("figure");
        if (result!.type === "figure") {
            expect(result.filename).toBe("test.png");
            expect(result.label).toBe("fig:test");
        }
    });

    it("parses equation environment as math", () => {
        const result = environment.parse(
            "\\begin{equation}\nx^2 + y^2 = z^2\n\\end{equation}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("math");
        if (result!.type === "math") {
            expect(result.display).toBe(true);
        }
    });

    it("parses proof environment", () => {
        const result = environment.parse(
            "\\begin{proof}\nObvious.\n\\end{proof}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("proof");
    });

    it("parses quote environment", () => {
        const result = environment.parse(
            "\\begin{quote}\nA wise saying.\n\\end{quote}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("quote");
    });

    it("parses lstlisting with caption metadata", () => {
        const result = environment.parse(
            "\\begin{lstlisting}[caption={Example listing (\\texttt{foo.py})}, language=Python]\nprint('ok')\n\\end{lstlisting}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("codeBlock");
        if (result!.type === "codeBlock") {
            expect(result.caption).toContain("foo.py");
            expect(result.language).toBe("Python");
            expect(result.code).toContain("print('ok')");
        }
    });

    it("parses center environment as passthrough", () => {
        const result = environment.parse(
            "\\begin{center}\nsome content\n\\end{center}",
        );
        expect(result).not.toBeNull();
        expect(result!.type).toBe("environment");
        expect((result as any).name).toBe("center");
        expect((result as any).body.length).toBeGreaterThan(0);
    });
});
