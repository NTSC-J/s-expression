use std::prelude::v1::*;
use std::env;
use std::iter::Peekable;
use std::str::Chars;
use std::io::{Result, Error, ErrorKind};

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
}
impl S {
    pub fn parse_str(data: &str) -> Result<S> {
        S::parse(&mut Lexer::new(data).peekable())
    }
    pub fn parse(lexer: &mut Peekable<Lexer>) -> Result<S> {
        if let Some(l) = lexer.next() {
            match l {
                Token::LParen => S::get_list(lexer),
                Token::Atom(x) => Ok(S::Atom(x)),
                _ => S::data_error("unexpected token"),
            }
        } else {
            S::data_error("early EOF")
        }
    }
    fn get_list(lexer: &mut Peekable<Lexer>) -> Result<S> {
        let mut list = Vec::<S>::new();
        match lexer.peek() {
            Some(Token::RParen) => {
                lexer.next(); // )
                              // nil
            }
            Some(_) => {
                list.push(S::parse(lexer)?); // car
                match lexer.peek() {
                    Some(Token::Dot) => {
                        lexer.next(); // .
                        match S::parse(lexer) {
                            // cdr
                            Ok(S::List(x)) => list.extend(x),
                            Ok(x) => list.push(x),
                            e => return e,
                        }
                        if lexer.next() != Some(Token::RParen) {
                            return S::data_error("expected )")
                        }
                    }
                    Some(_) => {
                        match S::get_list(lexer) {
                            Ok(S::List(cdr)) => list.extend(cdr),
                            Ok(x) => list.push(x), // Invalid
                            e => return e,
                        }
                    }
                    None => return S::data_error("early EOF in list"),
                }
            }
            None => return S::data_error("early EOF in list"),
        }
        Ok(S::List(list))
    }
    fn data_error(s: &str) -> Result<S> {
        Err(Error::new(ErrorKind::InvalidData, s))
    }
}

fn main() {
    let s = env::args().skip(1).collect::<Vec<String>>().join(" ");
    dbg!(Lexer::new(&s).collect::<Vec<Token>>());
    dbg!(S::parse_str(&s).unwrap());
}
