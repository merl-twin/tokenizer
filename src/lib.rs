#[macro_use]
extern crate lazy_static;
extern crate unicode_segmentation;


use unicode_segmentation::{UnicodeSegmentation,UWordBounds};
use std::str::FromStr;

mod emoji;

pub use emoji::EMOJIMAP;



#[derive(Debug,Clone,Copy,PartialEq,PartialOrd)]
pub enum Number {
    Integer(i64),
    Float(f64),
}


#[derive(Debug,Clone,Copy,Eq,PartialEq,Ord,PartialOrd)]
pub enum Separator {
    Space,
    Tab,
    Newline,
    Unknown,
}

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub enum BasicToken<'t> {
    Alphanumeric(&'t str),
    Number(&'t str),
    Punctuation(&'t str),
    Emoji(&'t str),
    Separator(&'t str),
}

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub enum Token {
    Word(String),
    Hashtag(String),
    Mention(String),
    Punctuation(String),
    Number(Number),
    Emoji(String),
    Unicode(String),
    Separator(Separator),
}

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub struct PositionalToken {
    pub offset: usize,
    pub length: usize,
    pub token: Token,   
}


struct Breaker<'t> {
    offset: usize,
    initial: &'t str,
    bounds: std::iter::Peekable<UWordBounds<'t>>,
}
impl<'t> Breaker<'t> {
    fn new<'a>(s: &'a str) -> Breaker<'a> {
        Breaker {
            offset: 0,
            initial: s,
            bounds: s.split_word_bounds().peekable(),
        }
    }
}
impl<'t> Iterator for Breaker<'t> {
    type Item = BasicToken<'t>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.bounds.next() {
            Some(w) => {
                if w.len() == 1 {
                    let c = w.chars().next().unwrap(); //safe unwrap 
                    if c.is_ascii_punctuation() || c.is_whitespace() {
                        let mut len = 1;
                        loop {
                            match self.bounds.peek() {
                                Some(p) if *p==w => len += 1,
                                _ => break,
                            }
                            self.bounds.next();
                        }
                        let p = &self.initial[self.offset .. self.offset+len];
                        self.offset += len;
                        if c.is_ascii_punctuation() {
                            return Some(BasicToken::Punctuation(p));
                        } else {
                            return Some(BasicToken::Separator(p));
                        }
                    }
                }
                let mut an = true;
                let mut num = true;
                for c in w.chars() {
                    an = an && (c.is_alphanumeric() || (c == '.') || (c == '\'') || (c == '-') || (c == '+') || (c == '_'));
                    num = num && (c.is_digit(10) || (c == '.') || (c == '-') || (c == '+'));
                }
                self.offset += w.len();
                if num {
                    return Some(BasicToken::Number(w));
                }
                if an {
                    return Some(BasicToken::Alphanumeric(w));
                }
                Some(BasicToken::Emoji(w))
            },
            None => None,
        }
    }
}


pub trait Tokenizer {
    fn next_token(&mut self) -> Option<PositionalToken>;
}

pub struct Tokens<'t> {
    offset: usize,
    bounds: std::iter::Peekable<Breaker<'t>>,
}
impl<'t> Tokens<'t> {
    fn new<'a>(s: &'a str) -> Tokens<'a> {
        Tokens {
            offset: 0,
            bounds: Breaker::new(s).peekable(),
        }
    }
}
impl<'t> Tokenizer for Tokens<'t> {
    fn next_token(&mut self) -> Option<PositionalToken> {
        match self.bounds.next() {
            Some(w) => {
                let (tok,len) = match w {
                    BasicToken::Alphanumeric(s) => { (Token::Word(s.to_string()), s.len()) },
                    BasicToken::Number(s) => {
                        (match i64::from_str(s) {
                            Ok(n) => Token::Number(Number::Integer(n)),
                            Err(_) => {
                                match f64::from_str(s) {
                                    Ok(n) => Token::Number(Number::Float(n)),
                                    Err(..) => Token::Word(s.to_string()),
                                }
                            }
                        }, s.len())
                    },
                    BasicToken::Punctuation(s) => {
                        if match (s.len()==1)&&((&s[0..1]=="#")||((&s[0..1]=="@"))) {
                            true => match self.bounds.peek() {
                                Some(BasicToken::Alphanumeric(..)) | Some(BasicToken::Number(..)) => true,
                                _ => false,
                            },
                            false => false,
                        } {
                            match (s,self.bounds.next()) {
                                ("#",Some(BasicToken::Alphanumeric(w))) |
                                ("#",Some(BasicToken::Number(w))) => {
                                    (Token::Hashtag(w.to_string()),w.len()+1)
                                },
                                ("@",Some(BasicToken::Alphanumeric(w))) |
                                ("@",Some(BasicToken::Number(w))) => {
                                    (Token::Mention(w.to_string()),w.len()+1)
                                },
                                _ => unreachable!(),
                            }
                        } else {
                            (Token::Punctuation(s.to_string()), s.len())
                        }
                    },
                    BasicToken::Emoji(s) => {
                        let rs = s.replace("\u{fe0f}","");
                        match EMOJIMAP.get(&rs as &str) {
                            Some(em) => (Token::Emoji(em.to_string()), s.len()),
                            None => (Token::Unicode({
                                let mut us = "".to_string();
                                for c in rs.chars() {
                                    if us!="" { us += "_"; }
                                    us += "u";
                                    let ns = format!("{}",c.escape_unicode());
                                    us += &ns[3 .. ns.len()-1];
                                }
                                us
                            }), s.len()),
                        }
                    },
                    BasicToken::Separator(s) => { (match &s[0..1] {
                        " " => Token::Separator(Separator::Space),
                        "\n" => Token::Separator(Separator::Newline),
                        "\t" => Token::Separator(Separator::Tab),
                        _ => Token::Separator(Separator::Unknown),
                    }, s.len()) },
                };
                let r = PositionalToken {
                    offset: self.offset,
                    length: len,
                    token: tok
                };
                self.offset += len;
                Some(r)
            },
            None => None,
        }
    }
}
impl<'t> Iterator for Tokens<'t> {
    type Item = PositionalToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

pub trait IntoTokenizer {
    type IntoTokens: Tokenizer;
    fn into_tokens(self) -> Self::IntoTokens;
}
impl<'t> IntoTokenizer for &'t str {
    type IntoTokens = Tokens<'t>;
    fn into_tokens(self) -> Self::IntoTokens {
        Tokens::new(self)
    }
}

/*

fn main() {
    let uws = "The quick (\"brown\") fox can't jump 32.3 feet, right? 4pda etc. qeq U.S.A  asd\n\n\nBrr, it's 29.3°F!\n Русское предложение #36.6 для тестирования деления по юникод-словам...\n🇷🇺 🇸🇹\n👱🏿👶🏽👨🏽\n👱\nС.С.С.Р.\n👨‍👩‍👦‍👦\n🧠\nSome ##text with #hashtags and @other components";
    println!("{}\n",uws);
    for tok in uws.into_tokens() {
        match &tok.token {
            Token::Separator(..) | Token::Punctuation(..) => continue,
            _ => println!("{:?} [{}]",tok,&uws[tok.offset .. tok.offset+tok.length]),
        }
    }
}
*/
