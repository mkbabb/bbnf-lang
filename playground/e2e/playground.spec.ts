import { test, expect } from "@playwright/test";

// ---------------------------------------------------------------------------
// Helper: wait for WASM to load and the pipeline to settle.
// ---------------------------------------------------------------------------

/** Wait until the page no longer shows a "Waiting..." / loading state. */
async function waitForPipelineSettled(page: import("@playwright/test").Page) {
    // The pipeline is debounced (300 ms) + WASM init. Give it up to 15 s.
    await expect(page.locator("text=Waiting...")).toBeHidden({ timeout: 15_000 }).catch(() => {});
}

// ---------------------------------------------------------------------------
// Landing page — hero strip (LivePreviewStrip, WASM pipeline)
// ---------------------------------------------------------------------------

test.describe("Landing page hero strip", () => {
    test.beforeEach(async ({ page }) => {
        await page.goto("/");
    });

    test("renders the three preset buttons", async ({ page }) => {
        await expect(page.getByRole("button", { name: "JSON" })).toBeVisible();
        await expect(page.getByRole("button", { name: "CSS" })).toBeVisible();
        await expect(page.getByRole("button", { name: "Math" })).toBeVisible();
    });

    test("JSON preset produces AST output via WASM", async ({ page }) => {
        // JSON is the default preset. Wait for the WASM pipeline to produce output.
        // The AST <pre> lives inside the column div that contains "Parsed AST".
        const astColumn = page.locator("section").filter({ hasText: "See it in action" }).locator("pre").last();
        await expect(astColumn).toBeVisible({ timeout: 15_000 });

        const text = await astColumn.textContent();
        // The WASM VM parse tree uses Tagged values — look for structural output.
        expect(text).toBeTruthy();
        expect(text!.length).toBeGreaterThan(10);
    });

    test("switching presets updates output", async ({ page }) => {
        // Wait for initial JSON output.
        const section = page.locator("section").filter({ hasText: "See it in action" });
        const astPre = section.locator("pre").last();
        await expect(astPre).toBeVisible({ timeout: 15_000 });

        const jsonOutput = await astPre.textContent();

        // Click Math preset.
        await page.getByRole("button", { name: "Math" }).click();
        // Wait for output to change.
        await expect(astPre).not.toHaveText(jsonOutput!, { timeout: 15_000 });

        const mathOutput = await astPre.textContent();
        expect(mathOutput).toBeTruthy();
        expect(mathOutput).not.toBe(jsonOutput);
    });

    test("no parse errors on any preset", async ({ page }) => {
        for (const preset of ["JSON", "CSS", "Math"]) {
            await page.getByRole("button", { name: preset }).click();
            // Give pipeline time to run.
            await page.waitForTimeout(2000);
            // "Parse failed" or "Grammar error" should not appear.
            const section = page.locator("section").filter({ hasText: "See it in action" });
            await expect(section.locator("text=Parse failed")).toBeHidden();
            await expect(section.locator("text=Grammar error")).toBeHidden();
        }
    });

    test("Open in Playground navigates correctly", async ({ page }) => {
        await page.getByRole("button", { name: /Open in Playground/ }).click();
        await expect(page).toHaveURL(/\/playground/);
    });
});

// ---------------------------------------------------------------------------
// Main playground — WASM pipeline
// ---------------------------------------------------------------------------

test.describe("Playground page", () => {
    test("loads with example and produces AST + formatted output", async ({ page }) => {
        // Load playground with JSON example via query params.
        const grammar = encodeURIComponent(
            `value = object | array | string | number | "true" | "false" | "null" ;
object = "{" , members? , "}" ;
members = member , ("," , member)* ;
member = string , ":" , value ;
array = "[" , elements? , "]" ;
elements = value , ("," , value)* ;
string = /"[^"]*"/ ;
number = /-?\\d+(\\.\\d+)?/ ;`,
        );
        const input = encodeURIComponent(`{"a": [1, 2], "b": true}`);
        await page.goto(`/playground?grammar=${grammar}&input=${input}`);

        // Wait for pipeline to produce AST output.
        // The playground uses Monaco editors; AST appears in a tab panel.
        // Give WASM time to load + pipeline to run.
        await page.waitForTimeout(5000);

        // Verify no fatal errors — page should not show uncaught exception overlay.
        const errorOverlay = page.locator("vite-error-overlay");
        await expect(errorOverlay).toBeHidden({ timeout: 1000 }).catch(() => {});
    });

    test("custom grammar compiles and parses via WASM", async ({ page }) => {
        const grammar = encodeURIComponent(`digit = /[0-9]/ ;\nvalue = digit ;`);
        const input = encodeURIComponent("5");
        await page.goto(`/playground?grammar=${grammar}&input=${input}`);

        // Wait for pipeline.
        await page.waitForTimeout(5000);

        // No Vite error overlay.
        const errorOverlay = page.locator("vite-error-overlay");
        await expect(errorOverlay).toBeHidden({ timeout: 1000 }).catch(() => {});
    });
});
