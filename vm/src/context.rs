use interpret::Exception;
use std::fmt;
use std::io;

pub trait ExecutionLog {
    fn enter(&mut self, Option<&str>);
    fn leave(&mut self, Option<&Exception>);
}

#[derive(Clone, Copy, Debug)]
pub struct NullExecutionLog;

impl ExecutionLog for NullExecutionLog {
    fn enter(&mut self, _: Option<&str>) { }
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
    fn enter(&mut self, name: Option<&str>) {
        self.append(format_args!("<call procedure=\"{}\">", name.unwrap_or("ANONYMOUS")));
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
            log.enter(None);
            log.enter(None);
            log.enter(Some("foo"));
            log.leave(None);
            log.enter(Some("bar"));
            log.leave(Some(&Exception::NotAnInt));
            log.leave(None);
            log.leave(None);
        }
        assert_eq!(w, concat!("<call procedure=\"ANONYMOUS\">\n",
                              " <call procedure=\"ANONYMOUS\">\n",
                              "  <call procedure=\"foo\">\n",
                              "  </call>\n",
                              "  <call procedure=\"bar\">\n",
                              "   <exception><![CDATA[NotAnInt]]></exception>\n",
                              "  </call>\n",
                              " </call>\n",
                              "</call>\n").as_bytes());
    }
}
