use code::{Destination, Global, Instruction, Local, Source};
use context::ExecutionLog;
use std::rc::Rc;
use value::{Closure, ReportHandler, Value};

#[derive(Clone, Debug)]
pub enum Status {
    ReturnFromProcedure,
    ReturnFromFunction(Value),
    JumpRelative(usize),
}

#[derive(Clone, Debug)]
pub enum Exception {
    NoSuchGlobal(Global),
    NoSuchLocal(Local),
    NoSuchInstruction,
    NotAnInt,
    NotAProcedure,
}

pub fn call_procedure<Context>(context: Context, globals: &[Value], caller_locals: &mut [Value], procedure: &Closure, caller_using: &[Source], caller_giving: &[Destination]) -> Result<(), Exception>
    where Context: Copy + ExecutionLog {
    context.enter(procedure.chunk.name.as_ref().map(AsRef::as_ref));
    let result = call_procedure_inner(context, globals, caller_locals, procedure, caller_using, caller_giving);
    context.leave(result.as_ref().err());
    result
}

fn call_procedure_inner<Context>(context: Context, globals: &[Value], caller_locals: &mut [Value], procedure: &Closure, caller_using: &[Source], caller_giving: &[Destination]) -> Result<(), Exception>
    where Context: Copy + ExecutionLog {
    let free_variables_offset = 0;
    let using_offset = free_variables_offset + procedure.free_variables.len();
    let giving_offset = using_offset + caller_using.len();
    let auxiliary_offset = giving_offset + caller_giving.len();

    let mut callee_locals = vec![Value::Null; procedure.chunk.local_count as usize];

    callee_locals[free_variables_offset .. using_offset]
        .clone_from_slice(&procedure.free_variables);

    for (local, source) in callee_locals[using_offset .. giving_offset].iter_mut().zip(caller_using) {
        *local = read_source(globals, caller_locals, source)? }

    interpret_many(context, globals, &mut callee_locals, &procedure.chunk.instructions)?;

    for (local, &destination) in callee_locals[giving_offset .. auxiliary_offset].iter_mut().zip(caller_giving) {
        write_destination(caller_locals, destination, local.clone())? }

    Ok(())
}

pub fn interpret_many<Context>(context: Context, globals: &[Value], locals: &mut [Value], instructions: &[Instruction]) -> Result<Option<Value>, Exception>
    where Context: Copy + ExecutionLog {
    let mut program_counter = 0;
    loop {
        let instruction = instructions.get(program_counter).ok_or(Exception::NoSuchInstruction)?;
        let status = interpret_one(context, globals, locals, &instruction)?;
        match status {
            Status::ReturnFromProcedure => return Ok(None),
            Status::ReturnFromFunction(value) => return Ok(Some(value)),
            Status::JumpRelative(diff) => program_counter += diff,
        }
    }
}

pub fn interpret_one<Context>(context: Context, globals: &[Value], locals: &mut [Value], instruction: &Instruction) -> Result<Status, Exception>
    where Context: Copy + ExecutionLog {
    match instruction {
        &Instruction::AddInt(ref source_a, ref source_b, destination) => {
            let int_a = read_int(&read_source(globals, locals, source_a)?)?;
            let int_b = read_int(&read_source(globals, locals, source_b)?)?;
            let result = Value::Int(int_a.wrapping_add(int_b));
            write_destination(locals, destination, result)?;
            Ok(Status::JumpRelative(1))
        },

        &Instruction::MultiplyInt(ref source_a, ref source_b, destination) => {
            let int_a = read_int(&read_source(globals, locals, source_a)?)?;
            let int_b = read_int(&read_source(globals, locals, source_b)?)?;
            let result = Value::Int(int_a.wrapping_mul(int_b));
            write_destination(locals, destination, result)?;
            Ok(Status::JumpRelative(1))
        },

        &Instruction::CallProcedure(ref callee, ref using, ref giving) => {
            let procedure_value = read_source(globals, locals, callee)?;
            let procedure = read_procedure(&procedure_value)?;
            call_procedure(context, globals, locals, &procedure, using, giving)?;
            Ok(Status::JumpRelative(1))
        },

        &Instruction::ReturnFromProcedure =>
            Ok(Status::ReturnFromProcedure),

        &Instruction::ReturnFromFunction(ref source) => {
            let result = read_source(globals, locals, source)?;
            Ok(Status::ReturnFromFunction(result))
        },

        &Instruction::MakeReportHandler(ref callee, ref using, ref giving, destination) => {
            let procedure_value = read_source(globals, locals, callee)?;
            let procedure = read_procedure(&procedure_value)?;
            let report_handler = ReportHandler{
                procedure: procedure,
                using: using.clone(),
                giving: giving.clone(),
            };
            let result = Value::ReportHandler(Rc::new(report_handler));
            write_destination(locals, destination, result)?;
            Ok(Status::JumpRelative(1))
        },
    }
}

fn read_int(value: &Value) -> Result<i32, Exception> {
    match value {
        &Value::Int(result) => Ok(result),
        _ => Err(Exception::NotAnInt),
    }
}

fn read_procedure(value: &Value) -> Result<Rc<Closure>, Exception> {
    match value {
        &Value::Procedure(ref closure) => Ok(closure.clone()),
        _ => Err(Exception::NotAProcedure),
    }
}

fn read_source(globals: &[Value], locals: &[Value], source: &Source) -> Result<Value, Exception> {
    match source {
        &Source::Constant(ref value) =>
            Ok(value.clone()),
        &Source::Global(global) =>
            globals.get(global.0 as usize)
            .ok_or(Exception::NoSuchGlobal(global))
            .map(|x| x.clone()),
        &Source::Local(local) =>
            locals.get(local.0 as usize)
            .ok_or(Exception::NoSuchLocal(local))
            .map(|x| x.clone()),
    }
}

fn write_destination(locals: &mut [Value], destination: Destination, value: Value) -> Result<(), Exception> {
    match destination {
        Destination::Discard => Ok(()),
        Destination::Local(local) => {
            let local_ref =
                locals.get_mut(local.0 as usize)
                .ok_or(Exception::NoSuchLocal(local))?;
            *local_ref = value;
            Ok(())
        },
    }
}

#[cfg(test)]
mod tests {
    use code::Chunk;
    use context::NullExecutionLog;
    use rand;
    use rand::Rng;
    use super::*;

    pub fn new_source(globals: &mut Vec<Value>, locals: &mut Vec<Value>, value: Value) -> Source {
        match rand::thread_rng().gen_range(0, 3) {
            0 => Source::Constant(value),
            1 => {
                globals.push(value);
                let global = Global(globals.len() as u32 - 1);
                Source::Global(global)
            },
            2 => {
                locals.push(value);
                let local = Local(locals.len() as u32 - 1);
                Source::Local(local)
            },
            _ => panic!("random number out of range"),
        }
    }

    pub fn new_destination(locals: &mut Vec<Value>) -> Destination {
        match rand::thread_rng().gen_range(0, 2) {
            0 => Destination::Discard,
            1 => {
                locals.push(Value::Int(0));
                let local = Local(locals.len() as u32 - 1);
                Destination::Local(local)
            },
            _ => panic!("random number out of range"),
        }
    }

    fn with_destination<F>(locals: &[Value], destination: Destination, body: F)
        where F: FnOnce(&Value) -> () {
        match destination {
            Destination::Discard => (),
            Destination::Local(local) => body(&locals[local.0 as usize]),
        }
    }

    fn test_arithmetic_int<F>(make_instruction: F, a: i32, b: i32, expected: i32)
        where F: Fn(Source, Source, Destination) -> Instruction {
        for _ in 0 .. 100 {
            let mut globals = vec![];
            let mut locals = vec![];

            let source_a = new_source(&mut globals, &mut locals, Value::Int(a));
            let source_b = new_source(&mut globals, &mut locals, Value::Int(b));
            let destination = new_destination(&mut locals);

            let context = NullExecutionLog;
            let instruction = make_instruction(source_a, source_b, destination);
            let status = interpret_one(context, &globals, &mut locals, &instruction);

            assert!(status.is_ok(), "status: {:?}", status);
            with_destination(&locals, destination, |result| {
                match result {
                    &Value::Int(actual) => assert_eq!(actual, expected),
                    _ => assert!(false, "result: {:?}", *result),
                }
            });
        }
    }

    #[test]
    fn test_add_int() {
        test_arithmetic_int(Instruction::AddInt, 8, 3, 11);
    }

    #[test]
    fn test_multiply_int() {
        test_arithmetic_int(Instruction::MultiplyInt, 8, 3, 24);
    }

    #[test]
    fn test_call_procedure() {
        for _ in 0 .. 100 {
            let mut globals = vec![];
            let mut locals = vec![];

            let closure = Rc::new(Closure{
                chunk: Rc::new(Chunk{
                    name: Some(Rc::from("example")),
                    local_count: 1 + 2 + 2 + 1,
                    instructions: vec![
                        Instruction::AddInt(
                            Source::Local(Local(0)),
                            Source::Local(Local(1)),
                            Destination::Local(Local(5)),
                        ),
                        Instruction::AddInt(
                            Source::Local(Local(5)),
                            Source::Local(Local(2)),
                            Destination::Local(Local(3)),
                        ),
                        Instruction::MultiplyInt(
                            Source::Local(Local(0)),
                            Source::Local(Local(1)),
                            Destination::Local(Local(5)),
                        ),
                        Instruction::MultiplyInt(
                            Source::Local(Local(5)),
                            Source::Local(Local(2)),
                            Destination::Local(Local(4)),
                        ),
                        Instruction::ReturnFromProcedure,
                    ],
                }),
                free_variables: vec![Value::Int(2)],
            });
            let callee = new_source(&mut globals, &mut locals, Value::Procedure(closure));

            let using_a = new_source(&mut globals, &mut locals, Value::Int(8));
            let using_b = new_source(&mut globals, &mut locals, Value::Int(3));

            let giving_a = new_destination(&mut locals);
            let giving_b = new_destination(&mut locals);

            let context = NullExecutionLog;
            let instruction = Instruction::CallProcedure(
                callee,
                vec![using_a, using_b],
                vec![giving_a, giving_b],
            );
            let status = interpret_one(context, &globals, &mut locals, &instruction);

            assert!(status.is_ok(), "status: {:?}", status);
            with_destination(&locals, giving_a, |result| {
                match result {
                    &Value::Int(13) => (),
                    _ => assert!(false, "result: {:?}", *result),
                }
            });
            with_destination(&locals, giving_b, |result| {
                match result {
                    &Value::Int(48) => (),
                    _ => assert!(false, "result: {:?}", *result),
                }
            });
        }
    }
}
