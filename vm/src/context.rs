use interpret::Exception;

pub trait ExecutionLog {
    fn enter(self, Option<&str>);
    fn leave(self, Option<&Exception>);
}

#[derive(Clone, Copy, Debug)]
pub struct NullExecutionLog;

impl ExecutionLog for NullExecutionLog {
    fn enter(self, _: Option<&str>) { }
    fn leave(self, _: Option<&Exception>) { }
}
