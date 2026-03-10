<script setup lang="ts">
import { computed } from "vue";
import { useRoute } from "vue-router";
import { TooltipProvider } from "@/components/ui/tooltip";
import NavBar from "@/components/layout/NavBar.vue";
import FooterSection from "@/components/landing/FooterSection.vue";

const route = useRoute();
const showFooter = computed(() => route.path !== "/playground" && !route.path.startsWith("/docs"));
</script>

<template>
    <TooltipProvider :delay-duration="300">
        <div class="relative min-h-dvh w-dvw bg-background text-foreground">
            <!-- Shared grid background -->
            <div
                class="pointer-events-none fixed inset-0 opacity-[0.06]"
                style="
                    background-image: url(&quot;data:image/svg+xml,%3Csvg width='40' height='40' xmlns='http://www.w3.org/2000/svg'%3E%3Cpath d='M0 0h40v40H0z' fill='none' stroke='%23888' stroke-width='1'/%3E%3C/svg%3E&quot;);
                "
            />
            <NavBar />
            <router-view v-slot="{ Component }">
                <transition name="page-fade" mode="out-in">
                    <component :is="Component" />
                </transition>
            </router-view>
            <FooterSection v-if="showFooter" />
        </div>
    </TooltipProvider>
</template>
