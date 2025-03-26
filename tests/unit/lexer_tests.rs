use ve::lexer::{Lexer, Token};
use codespan::Files;

#[test]
fn test_keyword_recognition() {
    let mut files = Files::new();
    let source = String::from("fn let if");
    let file_id = files.add("test", source);

    let lexer = Lexer::new(&files, file_id);
    
    let tokens_vec = lexer.tokens();
    let tokens: Vec<_> = tokens_vec.iter().map(|(t, _)| t).collect();

    assert_eq!(
        tokens,
        vec![
            &Token::KwFn,
            &Token::KwLet,
            &Token::KwIf
        ]
    );
}