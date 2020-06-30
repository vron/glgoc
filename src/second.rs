
  extern crate glsl;

  use glsl::syntax;
  use std::fmt::Write;
  use std::collections::HashMap;
  use itertools::Itertools;


// Represents the go type 
type GoType = String;

struct Argument {
  ty: GoType,
  name: String,
}

pub struct Parser {
  prefix: String, // the unique prefix used one everything generated TODO: check so nothing collides..
  error: String,

  arguments: Vec<Argument>,

  wg_size: [u32; 3],

  b_constants: HashMap<String, String>,
  b_header: String,
  b_body: String,

  f: String,
}

impl Parser {
  fn err(&mut self, e: &str) {
    // TODO: report more then 1 error...
    if self.error.is_empty() {
      self.error = String::from(e)
    }
  }

fn unsupported(&mut self, e: &str) {
  self.err(e);
}

  pub fn write_go<F>(&mut self, mut f: F) where F: std::io::Write {
    // TODO: Do not ignore errors...
    let _ = f.write_all(b"package kernel\n\n");

    let _ = f.write_all(self.b_header.as_bytes());

    // write all the constants defined
    for (q, v) in self.b_constants.iter().sorted() {
      let _ = write!(f, "const {} = {}\n", q, v);
    }

    // Write constants representing the wg sizes
    for i in 0..3 {
      let _ = write!(f, "const glgWgSize{} = {}\n", i, self.wg_size[i]);
    }


    // dumpt the struct on which the ernel will operate

    let _ = write!(f, "type {}Kern struct {}\n", self.prefix, "{");
    for arg in self.arguments.iter() { // TODO: sepperate globals and shared variables
      let _ = write!(f, "\t{} {}\n", arg.name, arg.ty);
    }
    let _ = f.write(b"}\n\n");


    // write the main body of the code
    let _ = f.write_all(self.b_body.as_bytes());

    // And dump the rest
    let _ = write!(f, "\n\n\n");
    let _ = write!(f, "{}", self.b_body);


  }


  pub fn visit_external_declaration(&mut self, ed: &syntax::ExternalDeclaration)
  {
    match *ed {
      syntax::ExternalDeclaration::Preprocessor(ref pp) => self.visit_preprocessor(pp),
      syntax::ExternalDeclaration::FunctionDefinition(ref fd) => self.visit_function_definition(fd),
      syntax::ExternalDeclaration::Declaration(ref d) => self.visit_declaration(d),
    }
  }


  pub fn visit_preprocessor(&mut self,  pp: &syntax::Preprocessor)
{
    match *pp {
      syntax::Preprocessor::Define(ref pd) => self.visit_preprocessor_define(pd),
      syntax::Preprocessor::Version(ref pd) => if pd.version != 450 { self.unsupported("only version 450 shaders supported")},
      _ => self.unsupported("non-supported preprocessor directive"),
    }
  }
  
  pub fn visit_preprocessor_define(&mut self,  pd: &syntax::PreprocessorDefine)
  {
    match *pd {
      syntax::PreprocessorDefine::ObjectLike {
        ref ident,
        ref value,
      } => {
        self.b_constants.insert(String::from(ident.as_str()), String::from(value.as_str()));
      },
  
      syntax::PreprocessorDefine::FunctionLike {..} => self.unsupported("#define only supported for"),
    }
  }
  

  pub fn visit_declaration(&mut self, d: &syntax::Declaration)
  {
    match *d {
      syntax::Declaration::FunctionPrototype(ref proto) => {
        self.visit_function_prototype(&proto);
        let _ = self.b_body.write_str(";\n");
      }
      syntax::Declaration::InitDeclaratorList(ref list) => {
        if self.maybe_handle_inout(&list) {
          return;
        }
        self.visit_init_declarator_list(&list);
        let _ = self.b_body.write_str(";\n");
      }
      syntax::Declaration::Precision(ref qual, ref ty) => {
        self.visit_precision_qualifier(&qual);
        self.visit_type_specifier(&ty);
        let _ = self.b_body.write_str(";\n");
      }
      syntax::Declaration::Block(ref block) => {
        self.visit_block(&block);
        let _ = self.b_body.write_str(";\n");
      }
      syntax::Declaration::Global(ref qual, ref identifiers) => {
        // chec if it is a specification of the input sizes, if so use it
        //qual.qualifiers.0.
        if self.maybe_handle_wg(&qual) {
          return
        }

        self.visit_type_qualifier(&qual);
  
        if !identifiers.is_empty() {
          let mut iter = identifiers.iter();
          let first = iter.next().unwrap();
          self.visit_identifier(first);
  
          for identifier in iter {
            let _ = write!(self.b_body, ", {}", identifier);
          }
        }
  
        let _ = self.b_body.write_str(";\n");
      }
    }
  }


  pub fn maybe_handle_wg(&mut self, q: &syntax::TypeQualifier) -> bool {
    // try to chec if this is a wg size spec, if so eat it, else let it fall through
    let mut qualifiers = q.qualifiers.0.iter();
    let first = qualifiers.next().unwrap();

    let l = match first {
      syntax::TypeQualifierSpec::Layout(ref l) => l,
      _ => return false, // this was clearly not such a layout
    };
    
    let qualifiers = l.ids.0.iter();
    for qual_spec in qualifiers {
      match qual_spec {
        syntax::LayoutQualifierSpec::Identifier(ref i, Some(ref e)) => {
          let s  = i.as_str();
          if s == "local_size_x" {
            match self.as_number(e) {
              Ok(v) => self.wg_size[0] = v,
              Err(v) => {
                self.err(&v[..]);
                return false
              }
            };
          } else if s == "local_size_y" {
            match self.as_number(e) {
              Ok(v) => self.wg_size[1] = v,
              Err(v) => {
                self.err(&v[..]);
                return false
              }
            };
          } else if s == "local_size_z" {
            match self.as_number(e) {
              Ok(v) => self.wg_size[2] = v,
              Err(v) => {
                self.err(&v[..]);
                return false
              }
            };
          } else {
            return false; // was not a layout
          }
        }
        _ => return false // they must specify size
        };
      }

      return true
    }
  

  pub fn visit_identifier(&mut self, i: &syntax::Identifier){
    let _ = self.b_body.write_str(&i.0);
  }
  
  fn as_number(&mut self, expr: &syntax::Expr) -> Result<u32, String> {
    // convert an expression to a number, including using constants, or
    // not
    match expr {
      syntax::Expr::Variable(v) => {
        // chec if it is a constant that we now, else sipp it
        let s = String::from(v.as_str());
        if self.b_constants.contains_key(&s) {
          let v = self.b_constants.get(&s).expect("cannot happen");
          return Ok( match v.parse() {
            Ok(v) => v,
            Err(_) => return Err(String::from("size refered to a non-numeric constant"))
          });
        }
        Err(String::from("cannot use non-constant variable for wg size"))
      },
      syntax::Expr::IntConst(v) => Ok(*v as u32),
      syntax::Expr::UIntConst(v) => Ok(*v),
      _ => Err(String::from("could not convert expression to numeric constant"))
    }
  }

  pub fn visit_type_name(&mut self, t: &syntax::TypeName){
    let _ = self.b_body.write_str(&t.0);
  }
  
  pub fn visit_type_specifier_non_array(&mut self, t: &syntax::TypeSpecifierNonArray){
    match *t {
      syntax::TypeSpecifierNonArray::Void => {
        let _ = self.b_body.write_str("void");
      }
      syntax::TypeSpecifierNonArray::Bool => {
        let _ = self.b_body.write_str("bool");
      }
      syntax::TypeSpecifierNonArray::Int => {
        let _ = self.b_body.write_str("int");
      }
      syntax::TypeSpecifierNonArray::UInt => {
        let _ = self.b_body.write_str("uint");
      }
      syntax::TypeSpecifierNonArray::Float => {
        let _ = self.b_body.write_str("float");
      }
      syntax::TypeSpecifierNonArray::Double => {
        let _ = self.b_body.write_str("double");
      }
      syntax::TypeSpecifierNonArray::Vec2 => {
        let _ = self.b_body.write_str("vec2");
      }
      syntax::TypeSpecifierNonArray::Vec3 => {
        let _ = self.b_body.write_str("vec3");
      }
      syntax::TypeSpecifierNonArray::Vec4 => {
        let _ = self.b_body.write_str("vec4");
      }
      syntax::TypeSpecifierNonArray::DVec2 => {
        let _ = self.b_body.write_str("dvec2");
      }
      syntax::TypeSpecifierNonArray::DVec3 => {
        let _ = self.b_body.write_str("dvec3");
      }
      syntax::TypeSpecifierNonArray::DVec4 => {
        let _ = self.b_body.write_str("dvec4");
      }
      syntax::TypeSpecifierNonArray::BVec2 => {
        let _ = self.b_body.write_str("bvec2");
      }
      syntax::TypeSpecifierNonArray::BVec3 => {
        let _ = self.b_body.write_str("bvec3");
      }
      syntax::TypeSpecifierNonArray::BVec4 => {
        let _ = self.b_body.write_str("bvec4");
      }
      syntax::TypeSpecifierNonArray::IVec2 => {
        let _ = self.b_body.write_str("ivec2");
      }
      syntax::TypeSpecifierNonArray::IVec3 => {
        let _ = self.b_body.write_str("ivec3");
      }
      syntax::TypeSpecifierNonArray::IVec4 => {
        let _ = self.b_body.write_str("ivec4");
      }
      syntax::TypeSpecifierNonArray::UVec2 => {
        let _ = self.b_body.write_str("uvec2");
      }
      syntax::TypeSpecifierNonArray::UVec3 => {
        let _ = self.b_body.write_str("uvec3");
      }
      syntax::TypeSpecifierNonArray::UVec4 => {
        let _ = self.b_body.write_str("uvec4");
      }
      syntax::TypeSpecifierNonArray::Mat2 => {
        let _ = self.b_body.write_str("mat2");
      }
      syntax::TypeSpecifierNonArray::Mat3 => {
        let _ = self.b_body.write_str("mat3");
      }
      syntax::TypeSpecifierNonArray::Mat4 => {
        let _ = self.b_body.write_str("mat4");
      }
      syntax::TypeSpecifierNonArray::Mat23 => {
        let _ = self.b_body.write_str("mat23");
      }
      syntax::TypeSpecifierNonArray::Mat24 => {
        let _ = self.b_body.write_str("mat24");
      }
      syntax::TypeSpecifierNonArray::Mat32 => {
        let _ = self.b_body.write_str("mat32");
      }
      syntax::TypeSpecifierNonArray::Mat34 => {
        let _ = self.b_body.write_str("mat34");
      }
      syntax::TypeSpecifierNonArray::Mat42 => {
        let _ = self.b_body.write_str("mat42");
      }
      syntax::TypeSpecifierNonArray::Mat43 => {
        let _ = self.b_body.write_str("mat43");
      }
      syntax::TypeSpecifierNonArray::DMat2 => {
        let _ = self.b_body.write_str("dmat2");
      }
      syntax::TypeSpecifierNonArray::DMat3 => {
        let _ = self.b_body.write_str("dmat3");
      }
      syntax::TypeSpecifierNonArray::DMat4 => {
        let _ = self.b_body.write_str("dmat4");
      }
      syntax::TypeSpecifierNonArray::DMat23 => {
        let _ = self.b_body.write_str("dmat23");
      }
      syntax::TypeSpecifierNonArray::DMat24 => {
        let _ = self.b_body.write_str("dmat24");
      }
      syntax::TypeSpecifierNonArray::DMat32 => {
        let _ = self.b_body.write_str("dmat32");
      }
      syntax::TypeSpecifierNonArray::DMat34 => {
        let _ = self.b_body.write_str("dmat34");
      }
      syntax::TypeSpecifierNonArray::DMat42 => {
        let _ = self.b_body.write_str("dmat42");
      }
      syntax::TypeSpecifierNonArray::DMat43 => {
        let _ = self.b_body.write_str("dmat43");
      }
      syntax::TypeSpecifierNonArray::Sampler1D => {
        let _ = self.b_body.write_str("sampler1D");
      }
      syntax::TypeSpecifierNonArray::Image1D => {
        let _ = self.b_body.write_str("image1D");
      }
      syntax::TypeSpecifierNonArray::Sampler2D => {
        let _ = self.b_body.write_str("sampler2D");
      }
      syntax::TypeSpecifierNonArray::Image2D => {
        let _ = self.b_body.write_str("image2D");
      }
      syntax::TypeSpecifierNonArray::Sampler3D => {
        let _ = self.b_body.write_str("sampler3D");
      }
      syntax::TypeSpecifierNonArray::Image3D => {
        let _ = self.b_body.write_str("image3D");
      }
      syntax::TypeSpecifierNonArray::SamplerCube => {
        let _ = self.b_body.write_str("samplerCube");
      }
      syntax::TypeSpecifierNonArray::ImageCube => {
        let _ = self.b_body.write_str("imageCube");
      }
      syntax::TypeSpecifierNonArray::Sampler2DRect => {
        let _ = self.b_body.write_str("sampler2DRect");
      }
      syntax::TypeSpecifierNonArray::Image2DRect => {
        let _ = self.b_body.write_str("image2DRect");
      }
      syntax::TypeSpecifierNonArray::Sampler1DArray => {
        let _ = self.b_body.write_str("sampler1DArray");
      }
      syntax::TypeSpecifierNonArray::Image1DArray => {
        let _ = self.b_body.write_str("image1DArray");
      }
      syntax::TypeSpecifierNonArray::Sampler2DArray => {
        let _ = self.b_body.write_str("sampler2DArray");
      }
      syntax::TypeSpecifierNonArray::Image2DArray => {
        let _ = self.b_body.write_str("image2DArray");
      }
      syntax::TypeSpecifierNonArray::SamplerBuffer => {
        let _ = self.b_body.write_str("samplerBuffer");
      }
      syntax::TypeSpecifierNonArray::ImageBuffer => {
        let _ = self.b_body.write_str("imageBuffer");
      }
      syntax::TypeSpecifierNonArray::Sampler2DMS => {
        let _ = self.b_body.write_str("sampler2DMS");
      }
      syntax::TypeSpecifierNonArray::Image2DMS => {
        let _ = self.b_body.write_str("image2DMS");
      }
      syntax::TypeSpecifierNonArray::Sampler2DMSArray => {
        let _ = self.b_body.write_str("sampler2DMSArray");
      }
      syntax::TypeSpecifierNonArray::Image2DMSArray => {
        let _ = self.b_body.write_str("image2DMSArray");
      }
      syntax::TypeSpecifierNonArray::SamplerCubeArray => {
        let _ = self.b_body.write_str("samplerCubeArray");
      }
      syntax::TypeSpecifierNonArray::ImageCubeArray => {
        let _ = self.b_body.write_str("imageCubeArray");
      }
      syntax::TypeSpecifierNonArray::Sampler1DShadow => {
        let _ = self.b_body.write_str("sampler1DShadow");
      }
      syntax::TypeSpecifierNonArray::Sampler2DShadow => {
        let _ = self.b_body.write_str("sampler2DShadow");
      }
      syntax::TypeSpecifierNonArray::Sampler2DRectShadow => {
        let _ = self.b_body.write_str("sampler2DRectShadow");
      }
      syntax::TypeSpecifierNonArray::Sampler1DArrayShadow => {
        let _ = self.b_body.write_str("sampler1DArrayShadow");
      }
      syntax::TypeSpecifierNonArray::Sampler2DArrayShadow => {
        let _ = self.b_body.write_str("sampler2DArrayShadow");
      }
      syntax::TypeSpecifierNonArray::SamplerCubeShadow => {
        let _ = self.b_body.write_str("samplerCubeShadow");
      }
      syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow => {
        let _ = self.b_body.write_str("samplerCubeArrayShadow");
      }
      syntax::TypeSpecifierNonArray::ISampler1D => {
        let _ = self.b_body.write_str("isampler1D");
      }
      syntax::TypeSpecifierNonArray::IImage1D => {
        let _ = self.b_body.write_str("iimage1D");
      }
      syntax::TypeSpecifierNonArray::ISampler2D => {
        let _ = self.b_body.write_str("isampler2D");
      }
      syntax::TypeSpecifierNonArray::IImage2D => {
        let _ = self.b_body.write_str("iimage2D");
      }
      syntax::TypeSpecifierNonArray::ISampler3D => {
        let _ = self.b_body.write_str("isampler3D");
      }
      syntax::TypeSpecifierNonArray::IImage3D => {
        let _ = self.b_body.write_str("iimage3D");
      }
      syntax::TypeSpecifierNonArray::ISamplerCube => {
        let _ = self.b_body.write_str("isamplerCube");
      }
      syntax::TypeSpecifierNonArray::IImageCube => {
        let _ = self.b_body.write_str("iimageCube");
      }
      syntax::TypeSpecifierNonArray::ISampler2DRect => {
        let _ = self.b_body.write_str("isampler2DRect");
      }
      syntax::TypeSpecifierNonArray::IImage2DRect => {
        let _ = self.b_body.write_str("iimage2DRect");
      }
      syntax::TypeSpecifierNonArray::ISampler1DArray => {
        let _ = self.b_body.write_str("isampler1DArray");
      }
      syntax::TypeSpecifierNonArray::IImage1DArray => {
        let _ = self.b_body.write_str("iimage1DArray");
      }
      syntax::TypeSpecifierNonArray::ISampler2DArray => {
        let _ = self.b_body.write_str("isampler2DArray");
      }
      syntax::TypeSpecifierNonArray::IImage2DArray => {
        let _ = self.b_body.write_str("iimage2DArray");
      }
      syntax::TypeSpecifierNonArray::ISamplerBuffer => {
        let _ = self.b_body.write_str("isamplerBuffer");
      }
      syntax::TypeSpecifierNonArray::IImageBuffer => {
        let _ = self.b_body.write_str("iimageBuffer");
      }
      syntax::TypeSpecifierNonArray::ISampler2DMS => {
        let _ = self.b_body.write_str("isampler2MS");
      }
      syntax::TypeSpecifierNonArray::IImage2DMS => {
        let _ = self.b_body.write_str("iimage2DMS");
      }
      syntax::TypeSpecifierNonArray::ISampler2DMSArray => {
        let _ = self.b_body.write_str("isampler2DMSArray");
      }
      syntax::TypeSpecifierNonArray::IImage2DMSArray => {
        let _ = self.b_body.write_str("iimage2DMSArray");
      }
      syntax::TypeSpecifierNonArray::ISamplerCubeArray => {
        let _ = self.b_body.write_str("isamplerCubeArray");
      }
      syntax::TypeSpecifierNonArray::IImageCubeArray => {
        let _ = self.b_body.write_str("iimageCubeArray");
      }
      syntax::TypeSpecifierNonArray::AtomicUInt => {
        let _ = self.b_body.write_str("atomic_uint");
      }
      syntax::TypeSpecifierNonArray::USampler1D => {
        let _ = self.b_body.write_str("usampler1D");
      }
      syntax::TypeSpecifierNonArray::UImage1D => {
        let _ = self.b_body.write_str("uimage1D");
      }
      syntax::TypeSpecifierNonArray::USampler2D => {
        let _ = self.b_body.write_str("usampler2D");
      }
      syntax::TypeSpecifierNonArray::UImage2D => {
        let _ = self.b_body.write_str("uimage2D");
      }
      syntax::TypeSpecifierNonArray::USampler3D => {
        let _ = self.b_body.write_str("usampler3D");
      }
      syntax::TypeSpecifierNonArray::UImage3D => {
        let _ = self.b_body.write_str("uimage3D");
      }
      syntax::TypeSpecifierNonArray::USamplerCube => {
        let _ = self.b_body.write_str("usamplerCube");
      }
      syntax::TypeSpecifierNonArray::UImageCube => {
        let _ = self.b_body.write_str("uimageCube");
      }
      syntax::TypeSpecifierNonArray::USampler2DRect => {
        let _ = self.b_body.write_str("usampler2DRect");
      }
      syntax::TypeSpecifierNonArray::UImage2DRect => {
        let _ = self.b_body.write_str("uimage2DRect");
      }
      syntax::TypeSpecifierNonArray::USampler1DArray => {
        let _ = self.b_body.write_str("usampler1DArray");
      }
      syntax::TypeSpecifierNonArray::UImage1DArray => {
        let _ = self.b_body.write_str("uimage1DArray");
      }
      syntax::TypeSpecifierNonArray::USampler2DArray => {
        let _ = self.b_body.write_str("usampler2DArray");
      }
      syntax::TypeSpecifierNonArray::UImage2DArray => {
        let _ = self.b_body.write_str("uimage2DArray");
      }
      syntax::TypeSpecifierNonArray::USamplerBuffer => {
        let _ = self.b_body.write_str("usamplerBuffer");
      }
      syntax::TypeSpecifierNonArray::UImageBuffer => {
        let _ = self.b_body.write_str("uimageBuffer");
      }
      syntax::TypeSpecifierNonArray::USampler2DMS => {
        let _ = self.b_body.write_str("usampler2DMS");
      }
      syntax::TypeSpecifierNonArray::UImage2DMS => {
        let _ = self.b_body.write_str("uimage2DMS");
      }
      syntax::TypeSpecifierNonArray::USampler2DMSArray => {
        let _ = self.b_body.write_str("usamplerDMSArray");
      }
      syntax::TypeSpecifierNonArray::UImage2DMSArray => {
        let _ = self.b_body.write_str("uimage2DMSArray");
      }
      syntax::TypeSpecifierNonArray::USamplerCubeArray => {
        let _ = self.b_body.write_str("usamplerCubeArray");
      }
      syntax::TypeSpecifierNonArray::UImageCubeArray => {
        let _ = self.b_body.write_str("uimageCubeArray");
      }
      syntax::TypeSpecifierNonArray::Struct(ref s) => self.visit_struct_non_declaration( s),
      syntax::TypeSpecifierNonArray::TypeName(ref tn) => self.visit_type_name( tn),
    }
  }
  
  pub fn visit_type_specifier(&mut self, t: &syntax::TypeSpecifier){
    self.visit_type_specifier_non_array( &t.ty);
  
    if let Some(ref arr_spec) = t.array_specifier {
      self.visit_array_spec( arr_spec);
    }
  }
  
  pub fn visit_fully_specified_type(&mut self, t: &syntax::FullySpecifiedType){
    if let Some(ref qual) = t.qualifier {
      self.visit_type_qualifier( &qual);
      let _ = self.b_body.write_str(" ");
    }
  
    self.visit_type_specifier( &t.ty);
  }
  
  pub fn visit_struct_non_declaration(&mut self, s: &syntax::StructSpecifier){
    let _ = self.b_body.write_str("struct ");
  
    if let Some(ref name) = s.name {
      let _ = write!(self.b_body, "{} ", name);
    }
  
    let _ = self.b_body.write_str("{\n");
  
    for field in &s.fields.0 {
      self.visit_struct_field( field);
    }
  
    let _ = self.b_body.write_str("}");
  }
  
  pub fn visit_struct(&mut self, s: &syntax::StructSpecifier){
    self.visit_struct_non_declaration( s);
    let _ = self.b_body.write_str(";\n");
  }
  
  pub fn visit_struct_field(&mut self, field: &syntax::StructFieldSpecifier){
    if let Some(ref qual) = field.qualifier {
      self.visit_type_qualifier( &qual);
      let _ = self.b_body.write_str(" ");
    }
  
    self.visit_type_specifier( &field.ty);
    let _ = self.b_body.write_str(" ");
  
    // there’s at least one identifier
    let mut identifiers = field.identifiers.0.iter();
    let identifier = identifiers.next().unwrap();
  
    self.visit_arrayed_identifier( identifier);
  
    // write the rest of the identifiers
    for identifier in identifiers {
      let _ = self.b_body.write_str(", ");
      self.visit_arrayed_identifier( identifier);
    }
  
    let _ = self.b_body.write_str(";\n");
  }
  
  pub fn visit_array_spec(&mut self, a: &syntax::ArraySpecifier){
    match *a {
      syntax::ArraySpecifier::Unsized => {
        let _ = self.b_body.write_str("[]"); // TODO: This one should be moved in front.
      }
      syntax::ArraySpecifier::ExplicitlySized(ref e) => {
        let _ = self.b_body.write_str("[");
        self.visit_expr( &e);
        let _ = self.b_body.write_str("]");
      }
    }
  }
  
  pub fn visit_arrayed_identifier(&mut self, a: &syntax::ArrayedIdentifier){
    let _ = write!(self.b_body,  "{}", a.ident);
  
    if let Some(ref arr_spec) = a.array_spec {
      self.visit_array_spec( arr_spec);
    }
  }
  
  pub fn visit_type_qualifier(&mut self, q: &syntax::TypeQualifier){
    let mut qualifiers = q.qualifiers.0.iter();
    let first = qualifiers.next().unwrap();
  
    self.visit_type_qualifier_spec( first);
  
    for qual_spec in qualifiers {
      let _ = self.b_body.write_str(" ");
      self.visit_type_qualifier_spec( qual_spec)
    }
  }
  
  pub fn visit_type_qualifier_spec(&mut self, q: &syntax::TypeQualifierSpec){
    match *q {
      syntax::TypeQualifierSpec::Storage(ref s) => self.visit_storage_qualifier( &s),
      syntax::TypeQualifierSpec::Layout(ref l) => self.visit_layout_qualifier( &l),
      syntax::TypeQualifierSpec::Precision(ref p) => self.visit_precision_qualifier( &p),
      syntax::TypeQualifierSpec::Interpolation(ref i) => self.visit_interpolation_qualifier( &i),
      syntax::TypeQualifierSpec::Invariant => {
        let _ = self.b_body.write_str("invariant");
      }
      syntax::TypeQualifierSpec::Precise => {
        let _ = self.b_body.write_str("precise");
      }
    }
  }
  
  pub fn visit_storage_qualifier(&mut self, q: &syntax::StorageQualifier){
    match *q {
      syntax::StorageQualifier::Const => {
        let _ = self.b_body.write_str("const");
      }
      syntax::StorageQualifier::InOut => {
        let _ = self.b_body.write_str("inout");
      }
      syntax::StorageQualifier::In => {
        let _ = self.b_body.write_str("in");
      }
      syntax::StorageQualifier::Out => {
        let _ = self.b_body.write_str("out");
      }
      syntax::StorageQualifier::Centroid => {
        let _ = self.b_body.write_str("centroid");
      }
      syntax::StorageQualifier::Patch => {
        let _ = self.b_body.write_str("patch");
      }
      syntax::StorageQualifier::Sample => {
        let _ = self.b_body.write_str("sample");
      }
      syntax::StorageQualifier::Uniform => {
        let _ = self.b_body.write_str("uniform");
      }
      syntax::StorageQualifier::Attribute => {
        let _ = self.b_body.write_str("attribute");
      }
      syntax::StorageQualifier::Varying => {
        let _ = self.b_body.write_str("varying");
      }
      syntax::StorageQualifier::Buffer => {
        let _ = self.b_body.write_str("buffer");
      }
      syntax::StorageQualifier::Shared => {
        let _ = self.b_body.write_str("shared");
      }
      syntax::StorageQualifier::Coherent => {
        let _ = self.b_body.write_str("coherent");
      }
      syntax::StorageQualifier::Volatile => {
        let _ = self.b_body.write_str("volatile");
      }
      syntax::StorageQualifier::Restrict => {
        let _ = self.b_body.write_str("restrict");
      }
      syntax::StorageQualifier::ReadOnly => {
        let _ = self.b_body.write_str("readonly");
      }
      syntax::StorageQualifier::WriteOnly => {
        let _ = self.b_body.write_str("writeonly");
      }
      syntax::StorageQualifier::Subroutine(ref n) => self.visit_subroutine( &n),
    }
  }
  
  pub fn visit_subroutine(&mut self, types: &Vec<syntax::TypeName>){
    let _ = self.b_body.write_str("subroutine");
  
    if !types.is_empty() {
      let _ = self.b_body.write_str("(");
  
      let mut types_iter = types.iter();
      let first = types_iter.next().unwrap();
  
      self.visit_type_name( first);
  
      for type_name in types_iter {
        let _ = self.b_body.write_str(", ");
        self.visit_type_name( type_name);
      }
  
      let _ = self.b_body.write_str(")");
    }
  }
  
  pub fn visit_layout_qualifier(&mut self, l: &syntax::LayoutQualifier){
    let mut qualifiers = l.ids.0.iter();
    let first = qualifiers.next().unwrap();
  
    let _ = self.b_body.write_str("layout (");
    self.visit_layout_qualifier_spec( first);
  
    for qual_spec in qualifiers {
      let _ = self.b_body.write_str(", ");
      self.visit_layout_qualifier_spec( qual_spec);
    }
  
    let _ = self.b_body.write_str(")");
  }
  
  pub fn visit_layout_qualifier_spec(&mut self, l: &syntax::LayoutQualifierSpec){
    match *l {
      syntax::LayoutQualifierSpec::Identifier(ref i, Some(ref e)) => {
        let _ = write!(self.b_body,  "{} = ", i);
        self.visit_expr( &e);
      }
      syntax::LayoutQualifierSpec::Identifier(ref i, None) => self.visit_identifier( &i),
      syntax::LayoutQualifierSpec::Shared => {
        let _ = self.b_body.write_str("shared");
      }
    }
  }
  
  pub fn visit_precision_qualifier(&mut self, p: &syntax::PrecisionQualifier){
    match *p {
      syntax::PrecisionQualifier::High => {
        let _ = self.b_body.write_str("highp");
      }
      syntax::PrecisionQualifier::Medium => {
        let _ = self.b_body.write_str("mediump");
      }
      syntax::PrecisionQualifier::Low => {
        let _ = self.b_body.write_str("low");
      }
    }
  }
  
  pub fn visit_interpolation_qualifier(&mut self, i: &syntax::InterpolationQualifier){
    match *i {
      syntax::InterpolationQualifier::Smooth => {
        let _ = self.b_body.write_str("smooth");
      }
      syntax::InterpolationQualifier::Flat => {
        let _ = self.b_body.write_str("flat");
      }
      syntax::InterpolationQualifier::NoPerspective => {
        let _ = self.b_body.write_str("noperspective");
      }
    }
  }
  
  pub fn visit_float(&mut self, x: f32){
    if x.fract() == 0. {
      let _ = write!(self.b_body,  "{}.", x);
    } else {
      let _ = write!( self.b_body, "{}", x);
    }
  }
  
  pub fn visit_double(&mut self, x: f64){
    if x.fract() == 0. {
      let _ = write!( self.b_body, "{}.", x);
    } else {
      let _ = write!( self.b_body, "{}", x);
    }
  }
  
  pub fn visit_expr(&mut self, expr: &syntax::Expr){
    match *expr {
      syntax::Expr::Variable(ref i) => self.visit_identifier( &i),
      syntax::Expr::IntConst(ref x) => {
        let _ = write!(self.b_body,  "{}", x);
      }
      syntax::Expr::UIntConst(ref x) => {
        let _ = write!(self.b_body,  "{}u", x);
      }
      syntax::Expr::BoolConst(ref x) => {
        let _ = write!( self.b_body, "{}", x);
      }
      syntax::Expr::FloatConst(ref x) => {
        let _ = write!( self.b_body, "{}", x);
      }
      syntax::Expr::DoubleConst(ref x) => {
        let _ = write!( self.b_body, "{}", x);
      }
      syntax::Expr::Unary(ref op, ref e) => {
        match op {
          syntax::UnaryOp::Inc => {
            let _ = self.b_body.write_str("(");
            self.visit_expr( &e);
            let _ = self.b_body.write_str(")++");
          }
          syntax::UnaryOp::Dec => {
            let _ = self.b_body.write_str("(");
            self.visit_expr( &e);
            let _ = self.b_body.write_str(")--");
          }
          syntax::UnaryOp::Add => {
            let _ = self.b_body.write_str("+(");
            self.visit_expr( &e);
            let _ = self.b_body.write_str(")");
          }
          syntax::UnaryOp::Minus => {
            let _ = self.b_body.write_str("-(");
            self.visit_expr( &e);
            let _ = self.b_body.write_str(")");
          }
          syntax::UnaryOp::Not => {
            let _ = self.b_body.write_str("!(");
            self.visit_expr( &e);
            let _ = self.b_body.write_str(")");
          }
          syntax::UnaryOp::Complement => {
            let _ = self.b_body.write_str("~(");
            self.visit_expr( &e);
            let _ = self.b_body.write_str(")");
          }
        }
      }
      syntax::Expr::Binary(ref op, ref l, ref r) => {
        let _ = self.b_body.write_str("(");
        self.visit_expr( &l);
        let _ = self.b_body.write_str(")");
        self.visit_binary_op( &op);
        let _ = self.b_body.write_str("(");
        self.visit_expr( &r);
        let _ = self.b_body.write_str(")");
      }
      syntax::Expr::Ternary(ref c, ref s, ref e) => {
        // argh - cant be bothered right now
        self.err("ternary expression not supported");
        /*self.visit_expr( &c);
        let _ = self.b_body.write_str(" ? ");
        self.visit_expr( &s);
        let _ = self.b_body.write_str(" : ");
        self.visit_expr( &e);
        */
      }
      syntax::Expr::Assignment(ref v, ref op, ref e) => {
        self.visit_expr( &v);
        let _ = self.b_body.write_str(" ");
        self.visit_assignment_op( &op);
        let _ = self.b_body.write_str(" ");
        self.visit_expr( &e);
      }
      syntax::Expr::Bracket(ref e, ref a) => {
        self.visit_expr( &e);
        self.visit_array_spec( &a);
      }
      syntax::Expr::FunCall(ref fun, ref args) => {
        self.visit_function_identifier( &fun);
        let _ = self.b_body.write_str("(");
  
        if !args.is_empty() {
          let mut args_iter = args.iter();
          let first = args_iter.next().unwrap();
          self.visit_expr( first);
  
          for e in args_iter {
            let _ = self.b_body.write_str(", ");
            self.visit_expr( e);
          }
        }
  
        let _ = self.b_body.write_str(")");
      }
      syntax::Expr::Dot(ref e, ref i) => {
        let _ = self.b_body.write_str("(");
        self.visit_expr( &e);
        let _ = self.b_body.write_str(")");
        let _ = self.b_body.write_str(".");
        // TODO: here we should special case stuff such as .xyz?
        self.visit_identifier( &i);
      }
      syntax::Expr::PostInc(ref e) => {
        self.visit_expr( &e);
        let _ = self.b_body.write_str("++");
      }
      syntax::Expr::PostDec(ref e) => {
        self.visit_expr( &e);
        let _ = self.b_body.write_str("--");
      }
      syntax::Expr::Comma(ref a, ref b) => {
        self.visit_expr( &a);
        let _ = self.b_body.write_str(", ");
        self.visit_expr( &b);
      }
    }
  }
  
  pub fn visit_path(&mut self, path: &syntax::Path){
    match path {
      syntax::Path::Absolute(s) => {
        let _ = write!(self.b_body,  "<{}>", s);
      }
      syntax::Path::Relative(s) => {
        let _ = write!(self.b_body,  "\"{}\"", s);
      }
    }
  }
  
  
  pub fn visit_binary_op(&mut self, op: &syntax::BinaryOp){
    match *op {
      syntax::BinaryOp::Or => {
        let _ = self.b_body.write_str("||");
      }
      syntax::BinaryOp::Xor => {
        let _ = self.b_body.write_str("^^");
      }
      syntax::BinaryOp::And => {
        let _ = self.b_body.write_str("&&");
      }
      syntax::BinaryOp::BitOr => {
        let _ = self.b_body.write_str("|");
      }
      syntax::BinaryOp::BitXor => {
        let _ = self.b_body.write_str("^");
      }
      syntax::BinaryOp::BitAnd => {
        let _ = self.b_body.write_str("&");
      }
      syntax::BinaryOp::Equal => {
        let _ = self.b_body.write_str("==");
      }
      syntax::BinaryOp::NonEqual => {
        let _ = self.b_body.write_str("!=");
      }
      syntax::BinaryOp::LT => {
        let _ = self.b_body.write_str("<");
      }
      syntax::BinaryOp::GT => {
        let _ = self.b_body.write_str(">");
      }
      syntax::BinaryOp::LTE => {
        let _ = self.b_body.write_str("<=");
      }
      syntax::BinaryOp::GTE => {
        let _ = self.b_body.write_str(">=");
      }
      syntax::BinaryOp::LShift => {
        let _ = self.b_body.write_str("<<");
      }
      syntax::BinaryOp::RShift => {
        let _ = self.b_body.write_str(">>");
      }
      syntax::BinaryOp::Add => {
        let _ = self.b_body.write_str("+");
      }
      syntax::BinaryOp::Sub => {
        let _ = self.b_body.write_str("-");
      }
      syntax::BinaryOp::Mult => {
        let _ = self.b_body.write_str("*");
      }
      syntax::BinaryOp::Div => {
        let _ = self.b_body.write_str("/");
      }
      syntax::BinaryOp::Mod => {
        let _ = self.b_body.write_str("%");
      }
    }
  }
  
  pub fn visit_assignment_op(&mut self, op: &syntax::AssignmentOp){
    match *op {
      syntax::AssignmentOp::Equal => {
        let _ = self.b_body.write_str("=");
      }
      syntax::AssignmentOp::Mult => {
        let _ = self.b_body.write_str("*=");
      }
      syntax::AssignmentOp::Div => {
        let _ = self.b_body.write_str("/=");
      }
      syntax::AssignmentOp::Mod => {
        let _ = self.b_body.write_str("%=");
      }
      syntax::AssignmentOp::Add => {
        let _ = self.b_body.write_str("+=");
      }
      syntax::AssignmentOp::Sub => {
        let _ = self.b_body.write_str("-=");
      }
      syntax::AssignmentOp::LShift => {
        let _ = self.b_body.write_str("<<=");
      }
      syntax::AssignmentOp::RShift => {
        let _ = self.b_body.write_str(">>=");
      }
      syntax::AssignmentOp::And => {
        let _ = self.b_body.write_str("&=");
      }
      syntax::AssignmentOp::Xor => {
        let _ = self.b_body.write_str("^=");
      }
      syntax::AssignmentOp::Or => {
        let _ = self.b_body.write_str("|=");
      }
    }
  }
  
  pub fn visit_function_identifier(&mut self, i: &syntax::FunIdentifier){
    // Special case calls such as barrier? (or should they simply be defined?)
    match *i {
      syntax::FunIdentifier::Identifier(ref n) => {
        // TOOD: now all function calls should be tied to type, e.g. vec4() calls etc.
        let _ = write!(self.b_body, "{}k.{}", self.prefix, n.0);
      }
      syntax::FunIdentifier::Expr(ref e) => {
        assert_eq!(true, false, "HOW CAN THIS HAPPEN, test it out");
    }
  }
}
  
  pub fn visit_function_prototype(&mut self, fp: &syntax::FunctionPrototype){
    self.visit_fully_specified_type( &fp.ty);
    let _ = self.b_body.write_str(" ");

    // all functions should be members of th ernel
    let _ = write!(self.b_body, "func ({}k *{}Kern) {}(", self.prefix, self.prefix, &fp.name.0);
  
    let _ = self.b_body.write_str("(");
  
    if !fp.parameters.is_empty() {
      let mut iter = fp.parameters.iter();
      let first = iter.next().unwrap();
      self.visit_function_parameter_declaration( first);
  
      for param in iter {
        let _ = self.b_body.write_str(", ");
        self.visit_function_parameter_declaration( param);
      }
    }
  
    let _ = self.b_body.write_str(")");
  }
  pub fn visit_function_parameter_declaration(&mut self, p: &syntax::FunctionParameterDeclaration){
    match *p {
      syntax::FunctionParameterDeclaration::Named(ref qual, ref fpd) => {
        if let Some(ref q) = *qual {
          self.visit_type_qualifier( q);
          let _ = self.b_body.write_str(" ");
        }
  
        self.visit_function_parameter_declarator( fpd);
      }
      syntax::FunctionParameterDeclaration::Unnamed(ref qual, ref ty) => {
        if let Some(ref q) = *qual {
          self.visit_type_qualifier( q);
          let _ = self.b_body.write_str(" ");
        }
  
        self.visit_type_specifier( ty);
      }
    }
  }
  
  pub fn visit_function_parameter_declarator(&mut self, p: &syntax::FunctionParameterDeclarator){
    self.visit_type_specifier( &p.ty);
    let _ = self.b_body.write_str(" ");
    self.visit_arrayed_identifier( &p.ident);
  }
  
  pub fn visit_init_declarator_list(&mut self, i: &syntax::InitDeclaratorList){
    self.visit_single_declaration( &i.head);

    for decl in &i.tail {
      let _ = self.b_body.write_str(", ");
      self.visit_single_declaration_no_type( decl);
    }
  }
  
  pub fn maybe_handle_inout(&mut self, i: &syntax::InitDeclaratorList) -> bool{
    // for now we believe that everything that has a layout specifier is a input
    // or output value that we should bind to, so we want to find all of them, thec if this
    // is that first.

    // chec if this is a layout specifier as we expect
    if let Some(ref q) = i.head.ty.qualifier {
      let mut qualifiers = q.qualifiers.0.iter();
      let first = qualifiers.next().unwrap();
      match first {
        syntax::TypeQualifierSpec::Layout(..) => (),
        _ => return false
      };
    } else {
      return false
    }

    // TOOD: Also handle complex types - for now simply handle those that are one directly.


    let name = String::from(i.head.name.as_ref().expect("could not find name of input/output").as_str());
    let ty = self.to_go_type(&i.head.ty.ty.ty );

    let arg = Argument {
      name: name,
      ty: ty,
    };
    self.arguments.push(arg);

    return true
  }
  
  fn to_go_type(&mut self,t: &syntax::TypeSpecifierNonArray) -> GoType {
    // this one should also register which types we actually need to emit to the
    // source file such that it does not get unreasonably large.
    let gt = match *t {
      syntax::TypeSpecifierNonArray::Void => "",
     syntax::TypeSpecifierNonArray::Bool => "bool",
     syntax::TypeSpecifierNonArray::Int => "",
     syntax::TypeSpecifierNonArray::UInt => "",
     syntax::TypeSpecifierNonArray::Float => "",
     syntax::TypeSpecifierNonArray::Double => "",
     syntax::TypeSpecifierNonArray::Vec2 => "",
     syntax::TypeSpecifierNonArray::Vec3 => "",
     syntax::TypeSpecifierNonArray::Vec4 => "",
     syntax::TypeSpecifierNonArray::DVec2 => "",
     syntax::TypeSpecifierNonArray::DVec3 => "",
     syntax::TypeSpecifierNonArray::DVec4 => "",
     syntax::TypeSpecifierNonArray::BVec2 => "",
     syntax::TypeSpecifierNonArray::BVec3 => "",
     syntax::TypeSpecifierNonArray::BVec4 => "",
     syntax::TypeSpecifierNonArray::IVec2 => "",
     syntax::TypeSpecifierNonArray::IVec3 => "",
     syntax::TypeSpecifierNonArray::IVec4 => "",
     syntax::TypeSpecifierNonArray::UVec2 => "",
     syntax::TypeSpecifierNonArray::UVec3 => "",
     syntax::TypeSpecifierNonArray::UVec4 => "",
     syntax::TypeSpecifierNonArray::Mat2 => "",
     syntax::TypeSpecifierNonArray::Mat3 => "",
     syntax::TypeSpecifierNonArray::Mat4 => "mat4",
     syntax::TypeSpecifierNonArray::Mat23 => "",
     syntax::TypeSpecifierNonArray::Mat24 => "",
     syntax::TypeSpecifierNonArray::Mat32 => "",
     syntax::TypeSpecifierNonArray::Mat34 => "",
     syntax::TypeSpecifierNonArray::Mat42 => "",
     syntax::TypeSpecifierNonArray::Mat43 => "",
     syntax::TypeSpecifierNonArray::DMat2 => "",
     syntax::TypeSpecifierNonArray::DMat3 => "",
     syntax::TypeSpecifierNonArray::DMat4 => "",
     syntax::TypeSpecifierNonArray::DMat23 => "",
     syntax::TypeSpecifierNonArray::DMat24 => "",
     syntax::TypeSpecifierNonArray::DMat32 => "",
     syntax::TypeSpecifierNonArray::DMat34 => "",
     syntax::TypeSpecifierNonArray::DMat42 => "",
     syntax::TypeSpecifierNonArray::DMat43 => "",
      // floating point opaque types
     syntax::TypeSpecifierNonArray::Sampler1D => "",
     syntax::TypeSpecifierNonArray::Image1D => "",
     syntax::TypeSpecifierNonArray::Sampler2D => "",
     syntax::TypeSpecifierNonArray::Image2D => "image2D",
     syntax::TypeSpecifierNonArray::Sampler3D => "",
     syntax::TypeSpecifierNonArray::Image3D => "",
     syntax::TypeSpecifierNonArray::SamplerCube => "",
     syntax::TypeSpecifierNonArray::ImageCube => "",
     syntax::TypeSpecifierNonArray::Sampler2DRect => "",
     syntax::TypeSpecifierNonArray::Image2DRect => "",
     syntax::TypeSpecifierNonArray::Sampler1DArray => "",
     syntax::TypeSpecifierNonArray::Image1DArray => "",
     syntax::TypeSpecifierNonArray::Sampler2DArray => "",
     syntax::TypeSpecifierNonArray::Image2DArray => "",
     syntax::TypeSpecifierNonArray::SamplerBuffer => "",
     syntax::TypeSpecifierNonArray::ImageBuffer => "",
     syntax::TypeSpecifierNonArray::Sampler2DMS => "",
     syntax::TypeSpecifierNonArray::Image2DMS => "",
     syntax::TypeSpecifierNonArray::Sampler2DMSArray => "",
     syntax::TypeSpecifierNonArray::Image2DMSArray => "",
     syntax::TypeSpecifierNonArray::SamplerCubeArray => "",
     syntax::TypeSpecifierNonArray::ImageCubeArray => "",
     syntax::TypeSpecifierNonArray::Sampler1DShadow => "",
     syntax::TypeSpecifierNonArray::Sampler2DShadow => "",
     syntax::TypeSpecifierNonArray::Sampler2DRectShadow => "",
     syntax::TypeSpecifierNonArray::Sampler1DArrayShadow => "",
     syntax::TypeSpecifierNonArray::Sampler2DArrayShadow => "",
     syntax::TypeSpecifierNonArray::SamplerCubeShadow => "",
     syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow => "",
      // signed integer opaque types
     syntax::TypeSpecifierNonArray::ISampler1D => "",
     syntax::TypeSpecifierNonArray::IImage1D => "",
     syntax::TypeSpecifierNonArray::ISampler2D => "",
     syntax::TypeSpecifierNonArray::IImage2D => "",
     syntax::TypeSpecifierNonArray::ISampler3D => "",
     syntax::TypeSpecifierNonArray::IImage3D => "",
     syntax::TypeSpecifierNonArray::ISamplerCube => "",
     syntax::TypeSpecifierNonArray::IImageCube => "",
     syntax::TypeSpecifierNonArray::ISampler2DRect => "",
     syntax::TypeSpecifierNonArray::IImage2DRect => "",
     syntax::TypeSpecifierNonArray::ISampler1DArray => "",
     syntax::TypeSpecifierNonArray::IImage1DArray => "",
     syntax::TypeSpecifierNonArray::ISampler2DArray => "",
     syntax::TypeSpecifierNonArray::IImage2DArray => "",
     syntax::TypeSpecifierNonArray::ISamplerBuffer => "",
     syntax::TypeSpecifierNonArray::IImageBuffer => "",
     syntax::TypeSpecifierNonArray::ISampler2DMS => "",
     syntax::TypeSpecifierNonArray::IImage2DMS => "",
     syntax::TypeSpecifierNonArray::ISampler2DMSArray => "",
     syntax::TypeSpecifierNonArray::IImage2DMSArray => "",
     syntax::TypeSpecifierNonArray::ISamplerCubeArray => "",
     syntax::TypeSpecifierNonArray::IImageCubeArray => "",
      // unsigned integer opaque types
     syntax::TypeSpecifierNonArray::AtomicUInt => "",
     syntax::TypeSpecifierNonArray::USampler1D => "",
     syntax::TypeSpecifierNonArray::UImage1D => "",
     syntax::TypeSpecifierNonArray::USampler2D => "",
     syntax::TypeSpecifierNonArray::UImage2D => "",
     syntax::TypeSpecifierNonArray::USampler3D => "",
     syntax::TypeSpecifierNonArray::UImage3D => "",
     syntax::TypeSpecifierNonArray::USamplerCube => "",
     syntax::TypeSpecifierNonArray::UImageCube => "",
     syntax::TypeSpecifierNonArray::USampler2DRect => "",
     syntax::TypeSpecifierNonArray::UImage2DRect => "",
     syntax::TypeSpecifierNonArray::USampler1DArray => "",
     syntax::TypeSpecifierNonArray::UImage1DArray => "",
     syntax::TypeSpecifierNonArray::USampler2DArray => "",
     syntax::TypeSpecifierNonArray::UImage2DArray => "",
     syntax::TypeSpecifierNonArray::USamplerBuffer => "",
     syntax::TypeSpecifierNonArray::UImageBuffer => "",
     syntax::TypeSpecifierNonArray::USampler2DMS => "",
     syntax::TypeSpecifierNonArray::UImage2DMS => "",
     syntax::TypeSpecifierNonArray::USampler2DMSArray => "",
     syntax::TypeSpecifierNonArray::UImage2DMSArray => "",
     syntax::TypeSpecifierNonArray::USamplerCubeArray => "",
     syntax::TypeSpecifierNonArray::UImageCubeArray => "",
     _ => "",
      //Struct(StructSpecifier) => "",
      //TypeName(TypeName) => "",
    };
    if gt.is_empty() {
      self.err("see src, did not now how to convert type to go type");
      return String::from(gt)
    }
    return String::from(gt)
  }

  pub fn visit_single_declaration(&mut self, d: &syntax::SingleDeclaration){
    self.visit_fully_specified_type( &d.ty);
  
    if let Some(ref name) = d.name {
      let _ = self.b_body.write_str(" ");
      self.visit_identifier( name);
    }
  
    if let Some(ref arr_spec) = d.array_specifier {
      self.visit_array_spec( arr_spec);
    }
  
    if let Some(ref initializer) = d.initializer {
      let _ = self.b_body.write_str(" = ");
      self.visit_initializer( initializer);
    }
  }
  
  pub fn visit_single_declaration_no_type(&mut self, d: &syntax::SingleDeclarationNoType){
    self.visit_arrayed_identifier( &d.ident);
  
    if let Some(ref initializer) = d.initializer {
      let _ = self.b_body.write_str(" = ");
      self.visit_initializer( initializer);
    }
  }
  
  pub fn visit_initializer(&mut self, i: &syntax::Initializer){
    match *i {
      syntax::Initializer::Simple(ref e) => self.visit_expr( e),
      syntax::Initializer::List(ref list) => {
        let mut iter = list.0.iter();
        let first = iter.next().unwrap();
  
        let _ = self.b_body.write_str("{ ");
        self.visit_initializer( first);
  
        for ini in iter {
          let _ = self.b_body.write_str(", ");
          self.visit_initializer( ini);
        }
  
        let _ = self.b_body.write_str(" }");
      }
    }
  }
  
  pub fn visit_block(&mut self, b: &syntax::Block){
    self.visit_type_qualifier( &b.qualifier);
    let _ = self.b_body.write_str(" ");
    self.visit_identifier( &b.name);
    let _ = self.b_body.write_str(" {");
  
    for field in &b.fields {
      self.visit_struct_field( field);
      let _ = self.b_body.write_str("\n");
    }
    let _ = self.b_body.write_str("}");
  
    if let Some(ref ident) = b.identifier {
      self.visit_arrayed_identifier( ident);
    }
  }
  
  pub fn visit_function_definition(&mut self, fd: &syntax::FunctionDefinition){
    self.visit_function_prototype( &fd.prototype);
    let _ = self.b_body.write_str(" ");
    self.visit_compound_statement( &fd.statement);
  }
  
  pub fn visit_compound_statement(&mut self, cst: &syntax::CompoundStatement){
    let _ = self.b_body.write_str("{\n");
  
    for st in &cst.statement_list {
      self.visit_statement( st);
    }
  
    let _ = self.b_body.write_str("}\n");
  }
  
  pub fn visit_statement(&mut self, st: &syntax::Statement){
    match *st {
      syntax::Statement::Compound(ref cst) => self.visit_compound_statement( cst),
      syntax::Statement::Simple(ref sst) => self.visit_simple_statement( sst),
    }
  }
  
  pub fn visit_simple_statement(&mut self, sst: &syntax::SimpleStatement){
    match *sst {
      syntax::SimpleStatement::Declaration(ref d) => self.visit_declaration( d),
      syntax::SimpleStatement::Expression(ref e) => self.visit_expression_statement( e),
      syntax::SimpleStatement::Selection(ref s) => self.visit_selection_statement( s),
      syntax::SimpleStatement::Switch(ref s) => self.visit_switch_statement( s),
      syntax::SimpleStatement::CaseLabel(ref cl) => self.visit_case_label( cl),
      syntax::SimpleStatement::Iteration(ref i) => self.visit_iteration_statement( i),
      syntax::SimpleStatement::Jump(ref j) => self.visit_jump_statement( j),
    }
  }
  
  pub fn visit_expression_statement(&mut self, est: &syntax::ExprStatement){
    if let Some(ref e) = *est {
      self.visit_expr( e);
    }
  
    let _ = self.b_body.write_str(";\n");
  }
  
  pub fn visit_selection_statement(&mut self, sst: &syntax::SelectionStatement){
    let _ = self.b_body.write_str("if (");
    self.visit_expr( &sst.cond);
    let _ = self.b_body.write_str(") {\n");
    self.visit_selection_rest_statement( &sst.rest);
  }
  
  pub fn visit_selection_rest_statement(&mut self, sst: &syntax::SelectionRestStatement){
    match *sst {
      syntax::SelectionRestStatement::Statement(ref if_st) => {
        self.visit_statement( if_st);
        let _ = self.b_body.write_str("}\n");
      }
      syntax::SelectionRestStatement::Else(ref if_st, ref else_st) => {
        self.visit_statement( if_st);
        let _ = self.b_body.write_str("} else ");
        self.visit_statement( else_st);
      }
    }
  }
  
  pub fn visit_switch_statement(&mut self, sst: &syntax::SwitchStatement){
    let _ = self.b_body.write_str("switch (");
    self.visit_expr( &sst.head);
    let _ = self.b_body.write_str(") {\n");
  
    for st in &sst.body {
      self.visit_statement( st);
    }
  
    let _ = self.b_body.write_str("}\n");
  }
  
  pub fn visit_case_label(&mut self, cl: &syntax::CaseLabel){
    match *cl {
      syntax::CaseLabel::Case(ref e) => {
        let _ = self.b_body.write_str("case ");
        self.visit_expr( e);
        let _ = self.b_body.write_str(":\n");
      }
      syntax::CaseLabel::Def => {
        let _ = self.b_body.write_str("default:\n");
      }
    }
  }
  
  pub fn visit_iteration_statement(&mut self, ist: &syntax::IterationStatement){
    match *ist {
      syntax::IterationStatement::While(ref cond, ref body) => {
        let _ = self.b_body.write_str("while (");
        self.visit_condition( cond);
        let _ = self.b_body.write_str(") ");
        self.visit_statement( body);
      }
      syntax::IterationStatement::DoWhile(ref body, ref cond) => {
        let _ = self.b_body.write_str("do ");
        self.visit_statement( body);
        let _ = self.b_body.write_str(" while (");
        self.visit_expr( cond);
        let _ = self.b_body.write_str(")\n");
      }
      syntax::IterationStatement::For(ref init, ref rest, ref body) => {
        let _ = self.b_body.write_str("for (");
        self.visit_for_init_statement( init);
        self.visit_for_rest_statement( rest);
        let _ = self.b_body.write_str(") ");
        self.visit_statement( body);
      }
    }
  }
  
  pub fn visit_condition(&mut self, c: &syntax::Condition){
    match *c {
      syntax::Condition::Expr(ref e) => self.visit_expr( e),
      syntax::Condition::Assignment(ref ty, ref name, ref initializer) => {
        self.visit_fully_specified_type( ty);
        let _ = self.b_body.write_str(" ");
        self.visit_identifier( name);
        let _ = self.b_body.write_str(" = ");
        self.visit_initializer( initializer);
      }
    }
  }
  
  pub fn visit_for_init_statement(&mut self, i: &syntax::ForInitStatement){
    match *i {
      syntax::ForInitStatement::Expression(ref expr) => {
        if let Some(ref e) = *expr {
          self.visit_expr( e);
        }
      }
      syntax::ForInitStatement::Declaration(ref d) => self.visit_declaration( d),
    }
  }
  
  pub fn visit_for_rest_statement(&mut self, r: &syntax::ForRestStatement){
    if let Some(ref cond) = r.condition {
      self.visit_condition( cond);
    }
  
    let _ = self.b_body.write_str("; ");
  
    if let Some(ref e) = r.post_expr {
      self.visit_expr( e);
    }
  }
  
  pub fn visit_jump_statement(&mut self, j: &syntax::JumpStatement){
    match *j {
      syntax::JumpStatement::Continue => {
        let _ = self.b_body.write_str("continue;\n");
      }
      syntax::JumpStatement::Break => {
        let _ = self.b_body.write_str("break;\n");
      }
      syntax::JumpStatement::Discard => {
        let _ = self.b_body.write_str("discard;\n");
      }
      syntax::JumpStatement::Return(ref e) => {
        let _ = self.b_body.write_str("return ");
        if let Some(e) = e {
          self.visit_expr( e);
        }
        let _ = self.b_body.write_str(";\n");
      }
    }
  }
  
  
}

pub fn transpile(tu: &mut syntax::TranslationUnit) -> Result<Parser,String>
{
  let mut parser = Parser { 
    prefix: String::from("_goc_"),
    error: String::from(""),


    arguments: Vec::new(),

    wg_size: [0, 1, 1],

    b_constants: HashMap::new(),
    b_header: String::with_capacity(1024),
    b_body: String::with_capacity(1024*1024),
    f: String::with_capacity(1024),
  };

  
  for ed in &(tu.0).0 {
    parser.visit_external_declaration(ed);
  }

  // ensure we have a wg size
  if parser.wg_size[0] == 0 {
    return Err(String::from("found no wg layout specification"));
  }


  if parser.error.is_empty() {
    Ok(parser)
  } else {
    Err(parser.error)
  }
}