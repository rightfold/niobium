use code::linking::Linking;
use code::{Chunk, Destination, Global, Instruction, Local, Source};
use regex::Regex;
use std::io;
use std::rc::Rc;
use std::string;
use value::{Closure, Type, Value};



pub const MAGIC: [u8; 8] = [0x53, 0x49, 0x4E, 0x45, 0x57, 0x41, 0x56, 0x45];

pub const VERSION: [u8; 2] = [0, 0];

const NAME_REGEX_SOURCE: &str = "^[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*$";
lazy_static! {
    static ref NAME_REGEX: Regex = Regex::new(NAME_REGEX_SOURCE).unwrap();
}



#[derive(Debug)]
pub enum Error {
    InvalidMagic,
    InvalidVersion,
    InvalidObjectKind,
    InvalidName,
    InvalidOpcode,
    InvalidSourceKind,
    InvalidDestinationKind,
    InvalidConstantKind,
    InvalidTypeKind,
    UnknownGlobal,
    IOError(io::Error),
    UTF8Error(string::FromUtf8Error),
}

impl From<io::Error> for Error {
    fn from(other: io::Error) -> Self {
        Error::IOError(other)
    }
}

impl From<string::FromUtf8Error> for Error {
    fn from(other: string::FromUtf8Error) -> Self {
        Error::UTF8Error(other)
    }
}



#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ObjectKind {
    Procedure,
    Function,
}



pub fn read<R>(linking: &Linking, r: &mut R) -> Result<Value, Error>
    where R: io::Read {
    read_magic(r)?;
    read_version(r)?;
    let kind = read_object_kind(r)?;
    match kind {
        ObjectKind::Procedure => {
            let closure = read_closure(linking, r)?;
            Ok(Value::Procedure(Rc::new(closure)))
        },
        ObjectKind::Function => {
            let closure = read_closure(linking, r)?;
            Ok(Value::Function(Rc::new(closure)))
        },
    }
}

pub fn read_magic<R>(r: &mut R) -> Result<(), Error>
    where R: io::Read {
    let mut bytes = [0; 8];
    r.read_exact(&mut bytes)?;
    if bytes == MAGIC {
        Ok(())
    } else {
        Err(Error::InvalidMagic)
    }
}

pub fn read_version<R>(r: &mut R) -> Result<(), Error>
    where R: io::Read {
    let mut bytes = [0; 2];
    r.read_exact(&mut bytes)?;
    if bytes == VERSION {
        Ok(())
    } else {
        Err(Error::InvalidMagic)
    }
}

pub fn read_object_kind<R>(r: &mut R) -> Result<ObjectKind, Error>
    where R: io::Read {
    let byte = read_u8(r)?;
    match byte {
        0x00 => Ok(ObjectKind::Procedure),
        0x01 => Ok(ObjectKind::Function),
        _ => Err(Error::InvalidObjectKind),
    }
}

pub fn read_closure<R>(linking: &Linking, r: &mut R) -> Result<Closure, Error>
    where R: io::Read {
    let chunk = read_chunk(linking, r)?;
    Ok(Closure{chunk: Rc::new(chunk), free_variables: vec![]})
}

pub fn read_chunk<R>(linking: &Linking, r: &mut R) -> Result<Chunk, Error>
    where R: io::Read {
    let name = read_name(r)?;
    let local_count = read_u32(r)?;
    let instructions = read_array(r, |r| read_instruction(linking, r))?;
    Ok(Chunk{name: Rc::from(name), local_count, instructions})
}

pub fn read_instruction<R>(linking: &Linking, r: &mut R) -> Result<Instruction, Error>
    where R: io::Read {
    let opcode = read_u16(r)?;
    match opcode {
        0x00 => {
            let source_a = read_source(linking, r)?;
            let source_b = read_source(linking, r)?;
            let destination = read_destination(r)?;
            Ok(Instruction::AddInt(source_a, source_b, destination))
        },

        0x01 => {
            let source_a = read_source(linking, r)?;
            let source_b = read_source(linking, r)?;
            let destination = read_destination(r)?;
            Ok(Instruction::MultiplyInt(source_a, source_b, destination))
        },

        0x10 => {
            let callee = read_source(linking, r)?;
            let using = read_array(r, |r| read_source(linking, r))?;
            let giving = read_array(r, read_destination)?;
            Ok(Instruction::CallProcedure(callee, using, giving))
        },

        0x11 => Ok(Instruction::ReturnFromProcedure),

        0x20 => {
            let source = read_source(linking, r)?;
            Ok(Instruction::ReturnFromFunction(source))
        },

        0x30 => {
            let handlers = read_array(r, |r| {
                let name = read_string(r)?;
                let handler = read_source(linking, r)?;
                Ok((Rc::from(name), handler))
            })?;
            Ok(Instruction::ExposeHandler(handlers))
        },

        0x31 => {
            let source = read_source(linking, r)?;
            let using = read_array(r, read_type)?;
            let giving = read_array(r, read_type)?;
            let destination = read_destination(r)?;
            Ok(Instruction::MakeReportHandler(source, Rc::from(using), Rc::from(giving), destination))
        },

        _ => Err(Error::InvalidOpcode),
    }
}

pub fn read_source<R>(linking: &Linking, r: &mut R) -> Result<Source, Error>
    where R: io::Read {
    let kind = read_u8(r)?;
    match kind {
        0x00 => {
            let value = read_constant(r)?;
            Ok(Source::Constant(value))
        },
        0x01 => {
            let global = read_global(linking, r)?;
            Ok(Source::Global(global))
        },
        0x02 => {
            let local = read_local(r)?;
            Ok(Source::Local(local))
        },
        _ => Err(Error::InvalidSourceKind)
    }
}

pub fn read_destination<R>(r: &mut R) -> Result<Destination, Error>
    where R: io::Read {
    let kind = read_u8(r)?;
    match kind {
        0x00 => Ok(Destination::Discard),
        0x01 => {
            let local = read_local(r)?;
            Ok(Destination::Local(local))
        },
        _ => Err(Error::InvalidDestinationKind)
    }
}

pub fn read_constant<R>(r: &mut R) -> Result<Value, Error>
    where R: io::Read {
    let kind = read_u8(r)?;
    match kind {
        0x00 => read_i32(r).map(Value::Int),
        _ => Err(Error::InvalidConstantKind),
    }
}

pub fn read_local<R>(r: &mut R) -> Result<Local, Error>
    where R: io::Read {
    let index = read_u32(r)?;
    Ok(Local(index))
}

pub fn read_global<R>(linking: &Linking, r: &mut R) -> Result<Global, Error>
    where R: io::Read {
    let name = read_name(r)?;
    linking.get_global(&name).map(|g| g.0).ok_or(Error::UnknownGlobal)
}

pub fn read_type<R>(r: &mut R) -> Result<Type, Error>
    where R: io::Read {
    let kind = read_u8(r)?;
    match kind {
        0x00 => Ok(Type::Int),
        _ => Err(Error::InvalidTypeKind),
    }
}



pub fn read_array<R, T, F>(r: &mut R, f: F) -> Result<Vec<T>, Error>
    where R: io::Read, F: Fn(&mut R) -> Result<T, Error> {
    let length = read_u32(r)?;
    let mut items = Vec::with_capacity(length as usize);
    for _ in 0 .. length {
        let item = f(r)?;
        items.push(item);
    }
    Ok(items)
}

pub fn read_name<R>(r: &mut R) -> Result<String, Error>
    where R: io::Read {
    let name = read_string(r)?;
    if NAME_REGEX.is_match(&name) {
        Ok(name)
    } else {
        Err(Error::InvalidName)
    }
}

pub fn read_string<R>(r: &mut R) -> Result<String, Error>
    where R: io::Read {
    let length = read_u32(r)?;
    let mut bytes = vec![0; length as usize];
    r.read_exact(&mut bytes)?;
    String::from_utf8(bytes).map_err(Error::UTF8Error)
}

pub fn read_u8<R>(r: &mut R) -> Result<u8, Error>
    where R: io::Read {
    let mut bytes = [0; 1];
    r.read_exact(&mut bytes)?;
    Ok(bytes[0])
}

pub fn read_u16<R>(r: &mut R) -> Result<u16, Error>
    where R: io::Read {
    let mut bytes = [0; 2];
    r.read_exact(&mut bytes)?;
    Ok((bytes[0] as u16) << 16 - 08 |
       (bytes[1] as u16) << 16 - 16)
}

pub fn read_i32<R>(r: &mut R) -> Result<i32, Error>
    where R: io::Read {
    read_u32(r).map(|x| x as i32)
}

pub fn read_u32<R>(r: &mut R) -> Result<u32, Error>
    where R: io::Read {
    let mut bytes = [0; 4];
    r.read_exact(&mut bytes)?;
    Ok((bytes[0] as u32) << 32 - 08 |
       (bytes[1] as u32) << 32 - 16 |
       (bytes[2] as u32) << 32 - 24 |
       (bytes[3] as u32) << 32 - 32)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_name() {
        let buffers = [(&b"\x00\x00\x00\x00"[..],            None),
                       (&b"\x00\x00\x00\x05hello"[..],       Some("hello")),
                       (&b"\x00\x00\x00\x0Bfoo.bar.baz"[..], Some("foo.bar.baz")),
                       (&b"\x00\x00\x00\x09not valid"[..],   None),
                       (&b"\x00\x00\x00\x05"[..],            None),
                       (&b"\x00\x00\x00\x01\xFF"[..],        None)];
        for &(mut buffer, expected) in buffers.iter() {
            let actual = read_name(&mut buffer);
            match expected {
                None => assert!(actual.is_err(), "{:?} {:?}"),
                Some(expected_str) => {
                    assert!(actual.is_ok());
                    assert_eq!(actual.unwrap(), expected_str);
                },
            }
        }
    }

    #[test]
    fn test_read_string() {
        let buffers = [(&b"\x00\x00\x00\x00"[..],      Some("")),
                       (&b"\x00\x00\x00\x05hello"[..], Some("hello")),
                       (&b"\x00\x00\x00\x05"[..],      None),
                       (&b"\x00\x00\x00\x01\xFF"[..],  None)];
        for &(mut buffer, expected) in buffers.iter() {
            let actual = read_string(&mut buffer);
            match expected {
                None => assert!(actual.is_err()),
                Some(expected_str) => {
                    assert!(actual.is_ok());
                    assert_eq!(actual.unwrap(), expected_str);
                },
            }
        }
    }

    #[test]
    fn test_read_u16() {
        let buffers = [([0x00u8, 0x00u8], 0x0000),
                       ([0x00u8, 0x01u8], 0x0001),
                       ([0xFFu8, 0xFFu8], 0xFFFF),
                       ([0x89u8, 0xABu8], 0x89AB)];
        for &(buffer, expected) in buffers.iter() {
            let actual = read_u16(&mut &buffer[..]);
            assert!(actual.is_ok());
            assert_eq!(actual.unwrap(), expected);
        }
    }

    #[test]
    fn test_read_u32() {
        let buffers = [([0x00u8, 0x00u8, 0x00u8, 0x00u8], 0x00000000),
                       ([0x00u8, 0x00u8, 0x00u8, 0x01u8], 0x00000001),
                       ([0xFFu8, 0xFFu8, 0xFFu8, 0xFFu8], 0xFFFFFFFF),
                       ([0x89u8, 0xABu8, 0xCDu8, 0xEFu8], 0x89ABCDEF)];
        for &(buffer, expected) in buffers.iter() {
            let actual = read_u32(&mut &buffer[..]);
            assert!(actual.is_ok());
            assert_eq!(actual.unwrap(), expected);
        }
    }
}
