extern crate niobiumvm;

use niobiumvm::code::{Chunk, Destination, Global, Instruction, Local, Source};
use niobiumvm::context::XMLExecutionLog;
use niobiumvm::interpret::interpret_many;
use niobiumvm::value::{Closure, Value};
use std::io;
use std::rc::Rc;

fn main() {
    let mut context = XMLExecutionLog::new(io::stderr());

    let mut globals = vec![];
    let mut locals = vec![Value::Int(8), Value::Int(3), Value::Null, Value::Null];

    globals.push(get_add_procedure());
    globals.push(get_multiply_procedure());
    globals.push(get_arithmetic_procedure());

    let instructions = [
        Instruction::CallProcedure(Source::Global(Global(2)),
                                   vec![Source::Local(Local(0)),
                                        Source::Local(Local(1))],
                                   vec![Destination::Local(Local(2)),
                                        Destination::Local(Local(3))]),
        Instruction::ReturnFromProcedure,
    ];
    interpret_many(&mut context, &globals, &mut locals, &instructions).unwrap();
    println!("{:?}", locals);
}

fn get_add_procedure() -> Value {
    let closure = Rc::new(Closure{
        chunk: Rc::new(Chunk{
            name: Some(Rc::from("example.add")),
            local_count: 3,
            instructions: vec![
                Instruction::AddInt(Source::Local(Local(0)),
                                    Source::Local(Local(1)),
                                    Destination::Local(Local(2))),
                Instruction::ReturnFromProcedure,
            ],
        }),
        free_variables: vec![],
    });
    Value::Procedure(closure)
}

fn get_multiply_procedure() -> Value {
    let closure = Rc::new(Closure{
        chunk: Rc::new(Chunk{
            name: Some(Rc::from("example.multiply")),
            local_count: 3,
            instructions: vec![
                Instruction::MultiplyInt(Source::Local(Local(0)),
                                         Source::Local(Local(1)),
                                         Destination::Local(Local(2))),
                Instruction::ReturnFromProcedure,
            ],
        }),
        free_variables: vec![],
    });
    Value::Procedure(closure)
}

fn get_arithmetic_procedure() -> Value {
    let closure = Rc::new(Closure{
        chunk: Rc::new(Chunk{
            name: Some(Rc::from("example.arithmetic")),
            local_count: 4,
            instructions: vec![
                Instruction::CallProcedure(Source::Global(Global(0)),
                                           vec![Source::Local(Local(0)),
                                                Source::Local(Local(1))],
                                           vec![Destination::Local(Local(2))]),
                Instruction::CallProcedure(Source::Global(Global(1)),
                                           vec![Source::Local(Local(0)),
                                                Source::Local(Local(1))],
                                           vec![Destination::Local(Local(3))]),
                Instruction::ReturnFromProcedure,
            ],
        }),
        free_variables: vec![],
    });
    Value::Procedure(closure)
}
