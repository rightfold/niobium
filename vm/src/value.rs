use code::Chunk;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Type {
    Int,
}

#[derive(Clone, Debug)]
pub enum Value {
    Null,

    Int(i32),

    Procedure(Rc<Closure>),
    Function(Rc<Closure>),

    ReportHandler(Rc<ReportHandler>),
}

#[derive(Clone, Debug)]
pub struct Closure {
    pub chunk: Rc<Chunk>,
    pub free_variables: Vec<Value>,
}

#[derive(Clone, Debug)]
pub struct ReportHandler {
    pub procedure: Rc<Closure>,
    pub using: Rc<[Type]>,
    pub giving: Rc<[Type]>,
}
