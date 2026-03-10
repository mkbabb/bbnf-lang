export interface DemoStep {
    /** Grammar text — null keeps previous step's grammar. */
    grammar?: string;
    /** Input text — null keeps previous step's input. */
    input?: string;
    /** Entry rule override. */
    entryRule?: string;
    /** Annotation text shown in the overlay (supports basic markdown). */
    annotation: string;
    /** Which region to highlight. */
    highlightRegion?: "grammar" | "input" | "ast" | "format" | "controls";
}

export interface Demo {
    id: string;
    title: string;
    description: string;
    steps: DemoStep[];
}
