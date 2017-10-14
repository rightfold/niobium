use value::{Type, Value};

pub struct Global(pub u32);

pub struct Local(pub u32);

pub enum Source {
    Constant(Value),
    Global(Global),
    Local(Local),
}

pub enum Destination {
    Discard,
    Local(Local),
}

pub enum Instruction {
    AddInt(Source, Source, Destination),
    MultiplyInt(Source, Source, Destination),

    CallProcedure(Source, Vec<Source>, Vec<Destination>),
    ReturnFromProcedure,

    ReturnFromFunction(Source),

    MakeReportHandler(Source, Vec<Type>, Vec<Type>),
}

pub struct Chunk {
    pub local_count: u32,
    pub instructions: Vec<Instruction>,
}
