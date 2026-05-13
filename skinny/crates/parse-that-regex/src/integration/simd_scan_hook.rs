use bbnf_simd::ClassifyResult;

pub trait SimdScannerHook {
    fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult;
    fn alphabet(&self) -> &'static [u8; 64];
}

impl<T> SimdScannerHook for T
where
    T: bbnf_simd::SimdClassifier,
{
    fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult {
        bbnf_simd::SimdClassifier::classify_chunk(self, bytes)
    }

    fn alphabet(&self) -> &'static [u8; 64] {
        bbnf_simd::SimdClassifier::alphabet(self)
    }
}
