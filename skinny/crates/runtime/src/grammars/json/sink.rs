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
}
