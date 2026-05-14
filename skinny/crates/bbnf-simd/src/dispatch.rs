use crate::classifier::{ClassifyResult, SimdClassifier};
use crate::scalar::eob_pad_clamp::EobBlock;
use std::sync::OnceLock;

#[derive(Clone, Copy)]
pub struct SelectedClassifier {
    alphabet: &'static [u8; 64],
    backend: SelectedBackend,
}

#[derive(Clone, Copy)]
enum SelectedBackend {
    Scalar,
    #[cfg(target_arch = "aarch64")]
    NeonTbl4,
}

impl SimdClassifier for SelectedClassifier {
    fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult {
        match self.backend {
            SelectedBackend::Scalar => crate::scalar::classify_chunk(bytes, self.alphabet),
            #[cfg(target_arch = "aarch64")]
            SelectedBackend::NeonTbl4 => unsafe {
                let table = crate::aarch64::classify_tbl4::build_lo6_table(self.alphabet);
                let table = crate::aarch64::classify_tbl4::load_lo6_table(&table);
                crate::aarch64::classify_tbl4::classify_block_from_table(
                    bytes.as_ptr(),
                    table,
                    b'"',
                    b'\\',
                    0x20,
                )
            },
        }
    }

    fn alphabet(&self) -> &'static [u8; 64] {
        self.alphabet
    }
}

pub fn select_classifier(alphabet: &'static [u8; 64]) -> SelectedClassifier {
    SelectedClassifier {
        alphabet,
        backend: select_backend(alphabet),
    }
}

#[derive(Clone, Copy)]
pub struct PrimitiveKernels {
    pub byte_class_from_table_64: fn(&[u8; 64], &[u8; 256]) -> u64,
    pub bitmap_prefix_xor_64: fn(u64, bool) -> u64,
    pub bitmap_next_set_bit: fn(u64, u8) -> u8,
    pub eob_pad_clamp: fn(&[u8]) -> EobBlock,
}

pub fn primitive_kernels() -> &'static PrimitiveKernels {
    static KERNELS: OnceLock<PrimitiveKernels> = OnceLock::new();
    KERNELS.get_or_init(select_primitive_kernels)
}

fn select_primitive_kernels() -> PrimitiveKernels {
    #[cfg(target_arch = "aarch64")]
    {
        return PrimitiveKernels {
            byte_class_from_table_64:
                crate::aarch64::byte_class_from_table_64::byte_class_from_table_64_neon,
            bitmap_prefix_xor_64: crate::aarch64::bitmap_prefix_xor_64::bitmap_prefix_xor_64_neon,
            bitmap_next_set_bit: crate::aarch64::bitmap_next_set_bit::bitmap_next_set_bit_neon,
            eob_pad_clamp: crate::aarch64::eob_pad_clamp::eob_pad_clamp_neon,
        };
    }

    #[allow(unreachable_code)]
    PrimitiveKernels {
        byte_class_from_table_64:
            crate::scalar::byte_class_from_table_64::byte_class_from_table_64_scalar,
        bitmap_prefix_xor_64: crate::scalar::bitmap_prefix_xor_64::bitmap_prefix_xor_64_scalar,
        bitmap_next_set_bit: crate::scalar::bitmap_next_set_bit::bitmap_next_set_bit_scalar,
        eob_pad_clamp: crate::scalar::eob_pad_clamp::eob_pad_clamp_scalar,
    }
}

fn select_backend(alphabet: &[u8; 64]) -> SelectedBackend {
    #[cfg(target_arch = "aarch64")]
    {
        if lo6_table_admissible(alphabet) {
            return SelectedBackend::NeonTbl4;
        }
    }

    SelectedBackend::Scalar
}

#[cfg(target_arch = "aarch64")]
fn lo6_table_admissible(alphabet: &[u8; 64]) -> bool {
    let mut seen = [false; 64];
    let mut len = 0usize;
    for byte in alphabet.iter().copied().take_while(|byte| *byte != 0) {
        len += 1;
        let slot = (byte & 0x3f) as usize;
        if seen[slot] {
            return false;
        }
        seen[slot] = true;
    }
    len != 0
}
