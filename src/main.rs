use std::env;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq)]
enum Token {
    LParen,
    RParen,
    Dot,
    Atom(String),
}
#[derive(Debug)]
struct Lexer<'a> {
    data: Peekable<Chars<'a>>,
}
impl<'a> Lexer<'a> {
    pub fn new(s: &str) -> Lexer {
        Lexer {
            data: s.chars().peekable().clone(),
        }
    }
}
impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.data.next().and_then(|c| match c {
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '.' => Some(Token::Dot),
            ' ' | '\t' | '\n' => self.next(),
            c => {
                let mut atom = c.to_string();
                loop {
                    match self.data.peek() {
                        Some(&d) if "() \t\n".contains(d) => return Some(Token::Atom(atom)),
                        None => return Some(Token::Atom(atom)),
                        Some(_) => atom.push(self.data.next().unwrap()), // == d
                    }
                }
            }
        })
    }
}

#[derive(Debug)]
enum S {
    Atom(String),
    List(Vec<S>), // nilは空のリストになる
    Invalid(String),
}
impl S {
    pub fn parse_str(data: &str) -> S {
        S::parse(&mut Lexer::new(data).peekable())
    }
    pub fn parse(lexer: &mut Peekable<Lexer>) -> S {
        if let Some(l) = lexer.next() {
            match l {
                Token::LParen => S::get_list(lexer),
                Token::Atom(x) => S::Atom(x),
                _ => S::Invalid("unexpected token".to_string()),
            }
        } else {
            S::Invalid("early EOF".to_string())
        }
    }
    fn get_list(lexer: &mut Peekable<Lexer>) -> S {
        let mut list = Vec::<S>::new();
        match lexer.peek() {
            Some(Token::RParen) => {
                lexer.next(); // )
                              // nil
            }
            Some(_) => {
                list.push(S::parse(lexer)); // car
                match lexer.peek() {
                    Some(Token::Dot) => {
                        lexer.next(); // .
                        match S::parse(lexer) {
                            // cdr
                            S::List(x) => list.extend(x),
                            x => list.push(x),
                        }
                        if lexer.next() != Some(Token::RParen) {
                            list.push(S::Invalid("expected )".to_string()))
                        }
                    }
                    Some(_) => {
                        match S::get_list(lexer) {
                            S::List(cdr) => list.extend(cdr),
                            x => list.push(x), // Invalid
                        }
                    }
                    None => return S::Invalid("early EOF in list".to_string()),
                }
            }
            None => return S::Invalid("early EOF in list".to_string()),
        }
        S::List(list)
    }
}

fn main() {
    let s = env::args().skip(1).collect::<Vec<String>>().join(" ");
    dbg!(Lexer::new(&s).collect::<Vec<Token>>());
    dbg!(S::parse_str(&s));
}
