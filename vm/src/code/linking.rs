use code::Global;
use std::collections::HashMap;
use std::rc::Rc;
use value::Value;

#[derive(Clone, Debug)]
pub struct Linking {
    globals: Vec<Value>,
    names: HashMap<Rc<str>, Global>,
}

impl Linking {
    pub fn reserve_global(&mut self, name: Rc<str>) {
        self.globals.push(Value::Null);
        let global = Global(self.globals.len() as u32 - 1);
        self.names.insert(name, global);
    }

    pub fn get_global(&self, name: &str) -> Option<(Global, &Value)> {
        match self.names.get(name) {
            None => None,
            Some(&global) => Some((global, &self.globals[global.0 as usize])),
        }
    }

    pub fn get_global_mut(&mut self, name: &str) -> Option<(Global, &mut Value)> {
        match self.names.get(name) {
            None => None,
            Some(&global) => Some((global, &mut self.globals[global.0 as usize])),
        }
    }

    pub fn get_globals(self) -> Vec<Value> {
        self.globals
    }
}
