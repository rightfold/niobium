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

    MakeReportHandler(Source, Vec<Type>, Vec<Type>),
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub local_count: u32,
    pub instructions: Vec<Instruction>,
}
