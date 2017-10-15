extern crate niobiumvm;
extern crate zip;

use niobiumvm::code::linking::Linking;
use niobiumvm::code::object;
use niobiumvm::code::{Instruction, Source};
use niobiumvm::context::XMLExecutionLog;
use niobiumvm::interpret::interpret_one;
use std::env;
use std::fs::File;
use std::io;
use std::rc::Rc;
use zip::read::ZipArchive;

fn main() {
    let mut linking = Linking::new();

    let mut args = env::args();
    args.next().unwrap();
    let archive_path = args.next().unwrap();
    let archive_file = File::open(archive_path).unwrap();
    let mut archive = ZipArchive::new(archive_file).unwrap();
    for i in 0 .. archive.len() {
        let file = archive.by_index(i).unwrap();
        let file_name = file.name();
        if file_name.starts_with("code/") && file_name.ends_with(".nbob") {
            let name = file_name[5 .. file_name.len() - 5].replace("/", ".");
            linking.reserve_global(Rc::from(name));
        }
    }
    for i in 0 .. archive.len() {
        let mut file = archive.by_index(i).unwrap();
        let (ok, name) = {
            let file_name = file.name();
            let ok = file_name.starts_with("code/") && file_name.ends_with(".nbob");
            let name = file_name[5 .. file_name.len() - 5].replace("/", ".");
            (ok, name)
        };
        if ok {
            let value = object::read(&linking, &mut file).unwrap();
            *linking.get_global_mut(&name).unwrap().1 = value;
        }
    }

    let entry_point = linking.get_global("example.main").unwrap().1.clone();

    let mut context = XMLExecutionLog::new(io::stderr());
    let globals = linking.get_globals();
    let mut locals = vec![];
    let instruction = Instruction::CallProcedure(Source::Constant(entry_point), vec![], vec![]);
    interpret_one(&mut context, &globals, &mut locals, &instruction).unwrap();
}
