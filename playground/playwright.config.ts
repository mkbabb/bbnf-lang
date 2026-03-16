import { defineConfig } from "@playwright/test";

export default defineConfig({
    testDir: "./e2e",
    timeout: 60_000,
    expect: { timeout: 15_000 },
    fullyParallel: true,
    retries: 1,
    use: {
        baseURL: "http://localhost:5600",
        trace: "on-first-retry",
    },
    projects: [
        { name: "chromium", use: { browserName: "chromium" } },
    ],
    webServer: {
        command: "npm run dev",
        port: 5600,
        reuseExistingServer: true,
        timeout: 30_000,
    },
});
