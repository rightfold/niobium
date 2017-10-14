use code::{Destination, Global, Instruction, Local, Source};
use value::Value;

#[derive(Clone, Debug)]
pub enum Exception {
    NoSuchGlobal(Global),
    NoSuchLocal(Local),
    NotAnInt,
}

pub fn interpret(globals: &[Value], locals: &mut [Value], instruction: &Instruction) -> Result<(), Exception> {
    match instruction {
        &Instruction::AddInt(ref source_a, ref source_b, destination) => {
            let int_a = read_int(&read_source(globals, locals, source_a)?)?;
            let int_b = read_int(&read_source(globals, locals, source_b)?)?;
            let result = Value::Int(int_a.wrapping_add(int_b));
            write_destination(locals, destination, result)?;
            Ok(())
        },

        _ => unimplemented!(),
    }
}

fn read_int(value: &Value) -> Result<i32, Exception> {
    match value {
        &Value::Int(result) => Ok(result),
        _ => Err(Exception::NotAnInt),
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
    use rand;
    use rand::Rng;
    use super::*;

    macro_rules! assert_pattern {
        ($a:expr, $b:pat) => {
            let a = $a;
            assert!(match a { $b => true, _ => false }, "{:?}", a);
        }
    }

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

    #[test]
    fn test_add_int() {
        for _ in 0 .. 100 {
            let mut globals = vec![];
            let mut locals = vec![];

            let source_a = new_source(&mut globals, &mut locals, Value::Int(1));
            let source_b = new_source(&mut globals, &mut locals, Value::Int(2));
            let destination = new_destination(&mut locals);

            let instruction = Instruction::AddInt(source_a, source_b, destination);
            let status = interpret(&globals, &mut locals, &instruction);

            assert!(status.is_ok(), "status: {:?}", status);
            with_destination(&locals, destination, |result| {
                assert_pattern!(result, &Value::Int(3));
            });
        }
    }
}
