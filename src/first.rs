
extern crate glsl;
use std::result::{Result};
use glsl::syntax;
use glsl::visitor::{Host, Visit, Visitor};
use std::fmt::Write;

pub struct Parser {
  error: String,

  b_header: String,
}

impl Parser {
  fn err(&mut self, e: &str) {
    // TODO: report more then 1 error...
    if self.error.is_empty() {
      self.error = String::from(e)
    }
  }

  pub fn write_go<F>(&mut self, mut f: F) where F: std::io::Write {
    // TODO: Do not ignore errors...
    let _ = f.write_all(b"package kernel\n\n");
    let _ = f.write_all(self.b_header.as_bytes());
  }
}

impl Visitor for Parser {

  fn visit_preprocessor_version(&mut self, d: &mut syntax::PreprocessorVersion) -> Visit {
    if d.version != 450 {
      self.err("only shader version 450 is supported")
    }
    Visit::Parent
  }

  fn visit_preprocessor_define(&mut self, d: &mut syntax::PreprocessorDefine) -> Visit {
    match d {
      syntax::PreprocessorDefine::FunctionLike{ .. } => self.err("function like defines not supported"),
      syntax::PreprocessorDefine::ObjectLike{ref ident, ref value} => {
        let _ = write!(self.b_header, "const {} = {}\n", ident, value);
      }
    };
    Visit::Parent
  }

  fn visit_layout_qualifier(&mut self, _: &mut syntax::LayoutQualifier) -> Visit {
    
    Visit::Children
  }

}



pub fn walk_ast(tu: &mut syntax::TranslationUnit) -> Result<Parser,String>
{
  let mut parser = Parser { 
    error: String::from(""),
    b_header: String::with_capacity(1024),
  };

  tu.visit(&mut parser);

  if parser.error.is_empty() {
    Ok(parser)
  } else {
    Err(parser.error)
  }
}