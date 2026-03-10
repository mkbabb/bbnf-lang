import { createRouter, createWebHistory } from "vue-router";

const router = createRouter({
    history: createWebHistory(),
    routes: [
        {
            path: "/",
            name: "landing",
            component: () => import("@/views/LandingPage.vue"),
        },
        {
            path: "/playground",
            name: "playground",
            component: () => import("@/views/PlaygroundPage.vue"),
        },
        {
            path: "/docs/:slug(.*)?",
            name: "docs",
            component: () => import("@/views/DocsPage.vue"),
            props: true,
        },
        {
            path: "/:pathMatch(.*)*",
            redirect: "/",
        },
    ],
    scrollBehavior(to) {
        if (to.hash) {
            return { el: to.hash, behavior: "smooth" };
        }
        return { top: 0 };
    },
});

const titles: Record<string, string> = {
    landing: "BBNF — Grammar-driven parser & formatter",
    playground: "BBNF Playground",
    docs: "BBNF Docs",
};

router.beforeEach((to) => {
    document.title = titles[to.name as string] ?? "BBNF";
});

export default router;
