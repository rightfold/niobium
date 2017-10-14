use code::Chunk;
use std::rc::Rc;

pub enum Type {
    Int,
}

pub enum Value {
    Int(i32),

    Procedure(Rc<Closure>),

    ReportHandler(Rc<ReportHandler>),
}

pub struct Closure {
    pub chunk: Rc<Chunk>,
    pub free_variables: Vec<Value>,
}

pub struct ReportHandler {
    pub implementation: Rc<Closure>,
    pub using: Vec<Type>,
    pub giving: Vec<Type>,
}
