export interface CardLine {
    spans: { text: string; cls?: string }[];
}

export interface CodeCard {
    title: string;
    color: string;
    lines: CardLine[];
}

export const codeCards: CodeCard[] = [
    {
        title: "Grammar",
        color: "pastel-green",
        lines: [
            { spans: [
                { text: "value", cls: "hl-type" }, { text: " = object " }, { text: "|", cls: "hl-operator" },
                { text: " array " }, { text: "|", cls: "hl-operator" }, { text: " string " },
                { text: "|", cls: "hl-operator" }, { text: " number " }, { text: ";", cls: "hl-operator" },
            ] },
            { spans: [
                { text: "object", cls: "hl-type" }, { text: " = " },
                { text: '"{" ', cls: "hl-string" }, { text: ", ", cls: "hl-operator" },
                { text: "members", cls: "hl-type" }, { text: "?", cls: "hl-operator" },
                { text: " , ", cls: "hl-operator" }, { text: '"}"', cls: "hl-string" },
                { text: " ;", cls: "hl-operator" },
            ] },
            { spans: [
                { text: "@pretty", cls: "hl-decorator" }, { text: " object " },
                { text: "group", cls: "hl-builtin" }, { text: " " },
                { text: "indent", cls: "hl-builtin" }, { text: " " },
                { text: "sep", cls: "hl-builtin" }, { text: "(" },
                { text: '", "', cls: "hl-string" }, { text: ")" },
                { text: " ;", cls: "hl-operator" },
            ] },
        ],
    },
    {
        title: "Input",
        color: "pastel-blue",
        lines: [
            { spans: [
                { text: "{" }, { text: '"name"', cls: "hl-string" }, { text: ": " },
                { text: '"BBNF"', cls: "hl-string" }, { text: ", " },
                { text: '"version"', cls: "hl-string" }, { text: ": " },
                { text: "1", cls: "hl-number" }, { text: "," },
            ] },
            { spans: [
                { text: ' ' }, { text: '"items"', cls: "hl-string" }, { text: ": [" },
                { text: "1", cls: "hl-number" }, { text: ", " },
                { text: "2", cls: "hl-number" }, { text: ", " },
                { text: "3", cls: "hl-number" }, { text: "]}" },
            ] },
        ],
    },
    {
        title: "Parsed AST",
        color: "pastel-purple",
        lines: [
            { spans: [
                { text: "{ " }, { text: '"type"', cls: "hl-string" }, { text: ": " },
                { text: '"object"', cls: "hl-string" }, { text: "," },
            ] },
            { spans: [
                { text: '  ' }, { text: '"members"', cls: "hl-string" }, { text: ": [" },
            ] },
            { spans: [
                { text: '    { ' }, { text: '"key"', cls: "hl-string" }, { text: ": " },
                { text: '"name"', cls: "hl-string" }, { text: " }," },
            ] },
            { spans: [{ text: "    ..." }, { text: " ]}" }] },
        ],
    },
    {
        title: "Formatted",
        color: "pastel-amber",
        lines: [
            { spans: [{ text: "{" }] },
            { spans: [
                { text: '  ' }, { text: '"name"', cls: "hl-string" }, { text: ": " },
                { text: '"BBNF"', cls: "hl-string" }, { text: "," },
            ] },
            { spans: [
                { text: '  ' }, { text: '"version"', cls: "hl-string" }, { text: ": " },
                { text: "1", cls: "hl-number" }, { text: "," },
            ] },
            { spans: [
                { text: '  ' }, { text: '"items"', cls: "hl-string" }, { text: ": [" },
                { text: "1", cls: "hl-number" }, { text: ", " },
                { text: "2", cls: "hl-number" }, { text: ", " },
                { text: "3", cls: "hl-number" }, { text: "]" },
            ] },
            { spans: [{ text: "}" }] },
        ],
    },
];

export const expandConfigs = [
    { tx: -380, ty: -20, tz: 100, ry: -30, scale: 1.08 },
    { tx: -120, ty: -55, tz: 150, ry: -10, scale: 1.12 },
    { tx: 160,  ty: -45, tz: 160, ry: 12,  scale: 1.12 },
    { tx: 400,  ty: 15,  tz: 90,  ry: 28,  scale: 1.06 },
];
