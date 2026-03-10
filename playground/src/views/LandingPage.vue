<script setup lang="ts">
import { ref } from "vue";
import HeroSection from "@/components/landing/HeroSection.vue";
import FeatureCards from "@/components/landing/FeatureCards.vue";
import LivePreviewStrip from "@/components/landing/LivePreviewStrip.vue";
import DemoCards from "@/components/landing/DemoCards.vue";
import { useScrollTimeline } from "@/composables/useScrollTimeline";

const featureRef = ref<HTMLElement | null>(null);
const previewRef = ref<HTMLElement | null>(null);
const demoRef = ref<HTMLElement | null>(null);

useScrollTimeline(featureRef, { translateY: 40 });
useScrollTimeline(previewRef, { translateY: 50 });
useScrollTimeline(demoRef, { translateY: 40 });
</script>

<template>
    <div class="pt-14">
        <HeroSection />
        <hr class="border-border/60 border-t-2 max-w-5xl mx-auto" />
        <div ref="featureRef">
            <FeatureCards />
        </div>
        <hr class="border-border/60 border-t-2 max-w-5xl mx-auto" />
        <div ref="previewRef">
            <LivePreviewStrip />
        </div>
        <hr class="border-border/60 border-t-2 max-w-5xl mx-auto" />
        <div ref="demoRef">
            <DemoCards />
        </div>
        <hr class="border-border/60 border-t-2 max-w-5xl mx-auto" />
    </div>
</template>
