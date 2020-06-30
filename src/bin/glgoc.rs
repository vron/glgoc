extern crate glsl;
use glsl::parser::Parse;
use std::io;
use glsl::syntax::ShaderStage;

fn main() {
// TODO: First pass where we find all the includes and merge it all to one big source file to use
// TODO: Document that we rely on the input being well-formatted and correct...
    let glsl = r#"
    #version 450
    #define SIZE 8
    layout(local_size_x = SIZE, local_size_y = 8, local_size_z = 1) in;

    layout(rgba32f, binding = 0) uniform image2D img;
    
    void main() {
        vec4 pixel = vec4(0.0f, 0.0f, 0.0f, 1.0f);
    
        ivec2 pixel_coords;
        pixel_coords.x = int(gl_GlobalInvocationID.x);
       pixel_coords.y = int(gl_GlobalInvocationID.y);
       pixel_coords *= 8;
       pixel_coords.x += int(gl_LocalInvocationID.x);
       pixel_coords.y += int(gl_LocalInvocationID.y);
       if(pixel_coords.x%2 == 0) {
           pixel = vec4(1.0f, 1.0f, 1.0f, 1.0f);
         } 
         imageStore(img, pixel_coords, pixel);
}
"#;
/*
    let mut ast = ShaderStage::parse(glsl).expect("failed to parse");

    let mut parser = match glgoc::first::walk_ast(&mut ast) {
        Err(v) => panic!("{}", v),
        Ok(p) => p,
    };

    let stdout = io::stdout();
    let handle = stdout.lock();
    parser.write_go(handle);
*/

let mut ast = ShaderStage::parse(glsl).expect("failed to parse");

let mut parser = match glgoc::second::transpile(&mut ast) {
    Err(v) => panic!("{}", v),
    Ok(p) => p,
};
let stdout = io::stdout();
let handle = stdout.lock();
parser.write_go(handle);
}