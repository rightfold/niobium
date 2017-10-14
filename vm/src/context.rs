use interpret::Exception;
use std::fmt;
use std::io;

pub trait ExecutionLog {
    fn enter(&mut self, &str);
    fn leave(&mut self, Option<&Exception>);
}

#[derive(Clone, Copy, Debug)]
pub struct NullExecutionLog;

impl ExecutionLog for NullExecutionLog {
    fn enter(&mut self, _: &str) { }
    fn leave(&mut self, _: Option<&Exception>) { }
}

#[derive(Clone, Debug)]
pub struct XMLExecutionLog<W> {
    w: W,
    nesting: usize,
}

impl<W> XMLExecutionLog<W> {
    pub fn new(w: W) -> Self {
        XMLExecutionLog{w, nesting: 0}
    }
}

impl<W> XMLExecutionLog<W>
    where W: io::Write {
    fn append(&mut self, fmt: fmt::Arguments) {
        for _ in 0 .. self.nesting {
            let _ = write!(self.w, " ");
        }
        let _ = self.w.write_fmt(fmt);
        let _ = write!(self.w, "\n");
    }
}

impl<W> ExecutionLog for XMLExecutionLog<W>
    where W: io::Write {
    fn enter(&mut self, name: &str) {
        self.append(format_args!("<call procedure=\"{}\">", name));
        self.nesting += 1;
    }

    fn leave(&mut self, exceptions: Option<&Exception>) {
        for exception in exceptions {
            self.append(format_args!("<exception><![CDATA[{:?}]]></exception>", exception));
        }
        self.nesting -= 1;
        self.append(format_args!("</call>"));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_xml_log() {
        let mut w = Vec::<u8>::new();
        {
            let mut log = XMLExecutionLog::new(&mut w);
            log.enter("foo");
            log.enter("bar");
            log.enter("baz");
            log.leave(None);
            log.enter("qux");
            log.leave(Some(&Exception::NotAnInt));
            log.leave(None);
            log.leave(None);
        }
        assert_eq!(w, concat!("<call procedure=\"foo\">\n",
                              " <call procedure=\"bar\">\n",
                              "  <call procedure=\"baz\">\n",
                              "  </call>\n",
                              "  <call procedure=\"qux\">\n",
                              "   <exception><![CDATA[NotAnInt]]></exception>\n",
                              "  </call>\n",
                              " </call>\n",
                              "</call>\n").as_bytes());
    }
}
