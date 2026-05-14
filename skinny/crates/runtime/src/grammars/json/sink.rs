pub trait JsonSink {
    fn begin_object(&mut self);
    fn end_object(&mut self);
    fn begin_array(&mut self);
    fn end_array(&mut self);
    fn key(&mut self, value: &str);
    fn string(&mut self, value: &str);
    fn i64(&mut self, value: i64);
    fn u64(&mut self, value: u64);
    fn f64(&mut self, value: f64);
    fn bool(&mut self, value: bool);
    fn null(&mut self);

    #[inline(always)]
    fn array_string(&mut self, value: &str) {
        self.string(value);
    }

    #[inline(always)]
    fn array_i64(&mut self, value: i64) {
        self.i64(value);
    }

    #[inline(always)]
    fn array_u64(&mut self, value: u64) {
        self.u64(value);
    }

    #[inline(always)]
    fn array_f64(&mut self, value: f64) {
        self.f64(value);
    }

    #[inline(always)]
    fn array_bool(&mut self, value: bool) {
        self.bool(value);
    }

    #[inline(always)]
    fn array_null(&mut self) {
        self.null();
    }

    #[inline(always)]
    fn object_string(&mut self, value: &str) {
        self.string(value);
    }

    #[inline(always)]
    fn object_i64(&mut self, value: i64) {
        self.i64(value);
    }

    #[inline(always)]
    fn object_u64(&mut self, value: u64) {
        self.u64(value);
    }

    #[inline(always)]
    fn object_f64(&mut self, value: f64) {
        self.f64(value);
    }

    #[inline(always)]
    fn object_bool(&mut self, value: bool) {
        self.bool(value);
    }

    #[inline(always)]
    fn object_null(&mut self) {
        self.null();
    }
}
