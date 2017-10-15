use std::rc::Rc;
use value::{Type, Value};

#[derive(Clone, Copy, Debug)]
pub struct Global(pub u32);

#[derive(Clone, Copy, Debug)]
pub struct Local(pub u32);

#[derive(Clone, Debug)]
pub enum Source {
    Constant(Value),
    Global(Global),
    Local(Local),
}

#[derive(Clone, Copy, Debug)]
pub enum Destination {
    Discard,
    Local(Local),
}

#[derive(Clone, Debug)]
pub enum Instruction {
    AddInt(Source, Source, Destination),
    MultiplyInt(Source, Source, Destination),

    CallProcedure(Source, Vec<Source>, Vec<Destination>),
    ReturnFromProcedure,

    ReturnFromFunction(Source),

    ExposeHandler(Vec<(Rc<str>, Source)>),
    MakeReportHandler(Source, Rc<[Type]>, Rc<[Type]>, Destination),
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub name: Rc<str>,
    pub local_count: u32,
    pub instructions: Vec<Instruction>,
}

pub mod archive;
pub mod linking;
pub mod object;
