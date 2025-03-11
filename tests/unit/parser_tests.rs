use codespan::Files;
use Ve::lexer::Lexer;

#[test]
fn test_function_parsing() {
    let mut files = Files::new();
    let source = String::from("fn add(a: i32, b: i32) -> i32 { a + b }");

    let file_id = files.add("test", source);

    let lexer = Lexer::new(&files, file_id);
    let mut parser = Ve::parser::Parser::new(lexer);
    let program = parser.parse().unwrap();

    assert_eq!(program.functions.len(), 1);
    assert_eq!(program.functions[0].name, "add");
}
