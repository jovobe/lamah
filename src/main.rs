mod lexer;
mod parser;
mod ast;
mod backend;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let Some(path) = std::env::args().nth(1) else {
        eprintln!("Missing parameter! Usage:");
        eprintln!("   lamah <file>");
        std::process::exit(1);
    };

    let file = std::fs::read_to_string(path)?;
    let out = parser::parse(&file);
    println!("{out:#?}");
    /*
    let lexer = lexer::lex(&file);
    for token in lexer {
        println!("{token:?}");
    }
    */

    Ok(())
}
