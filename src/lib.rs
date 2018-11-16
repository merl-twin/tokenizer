#[macro_use]
extern crate lazy_static;
extern crate unicode_segmentation;


use unicode_segmentation::{UnicodeSegmentation,UWordBounds};
use std::str::FromStr;
use std::collections::VecDeque;

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
    Url(String),
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
    bounds: Breaker<'t>,
    buffer: VecDeque<BasicToken<'t>>,
}
impl<'t> Tokens<'t> {
    fn new<'a>(s: &'a str) -> Tokens<'a> {
        Tokens {
            offset: 0,
            bounds: Breaker::new(s),
            buffer: VecDeque::new(),
        }
    }
    fn basic_separator_to_pt(&mut self, s: &str) -> PositionalToken {
        let tok = PositionalToken {
            offset: self.offset,
            length: s.len(),
            token: match &s[0..1] {
                " " => Token::Separator(Separator::Space),
                "\n" => Token::Separator(Separator::Newline),
                "\t" => Token::Separator(Separator::Tab),
                _ => Token::Separator(Separator::Unknown),
            }
        };
        self.offset += s.len();
        tok
    }
    fn basic_number_to_pt(&mut self, s: &str) -> PositionalToken {
        let tok = PositionalToken {
            offset: self.offset,
            length: s.len(),
            token: match i64::from_str(s) {
                Ok(n) => Token::Number(Number::Integer(n)),
                Err(_) => {
                    match f64::from_str(s) {
                        Ok(n) => Token::Number(Number::Float(n)),
                        Err(..) => Token::Word(s.to_string()),
                    }
                }
            },
        };
        self.offset += s.len();
        tok
    }
    fn basic_emoji_to_pt(&mut self, s: &str) -> PositionalToken {
        let tok = PositionalToken {
            offset: self.offset,
            length: s.len(),
            token: {
                let rs = s.replace("\u{fe0f}","");
                match EMOJIMAP.get(&rs as &str) {
                    Some(em) => Token::Emoji(em.to_string()),
                    None => Token::Unicode({
                        let mut us = "".to_string();
                        for c in rs.chars() {
                            if us!="" { us += "_"; }
                            us += "u";
                            let ns = format!("{}",c.escape_unicode());
                            us += &ns[3 .. ns.len()-1];
                        }
                        us
                    }),
                }
            }
        };
        self.offset += s.len();
        tok
    }
    fn basic_alphanumeric_to_pt(&mut self, s: &str) -> PositionalToken {
        let tok = PositionalToken {
            offset: self.offset,
            length: s.len(),
            token: Token::Word(s.to_string()),
        };
        self.offset += s.len();
        tok
    }
    fn basic_punctuation_to_pt(&mut self, s: &str) -> PositionalToken {
        let tok = PositionalToken {
            offset: self.offset,
            length: s.len(),
            token: Token::Punctuation(s.to_string()),
        };
        self.offset += s.len();
        tok
    }
    fn check_url(&mut self) -> Option<PositionalToken> {
        let check = if self.buffer.len()>3 {
            match (&self.buffer[0],&self.buffer[1],&self.buffer[2]) {
                (BasicToken::Alphanumeric("http"),BasicToken::Punctuation(":"),BasicToken::Punctuation("//")) |
                (BasicToken::Alphanumeric("https"),BasicToken::Punctuation(":"),BasicToken::Punctuation("//")) => true,
                _ => false,
            }
        } else { false };
        if check {
            let mut url = "".to_string();
            loop {
                match self.buffer.pop_front() {
                    None => break,
                    Some(BasicToken::Separator(s)) => {
                        self.buffer.push_front(BasicToken::Separator(s));
                        break;
                    },
                    Some(BasicToken::Alphanumeric(s)) |
                    Some(BasicToken::Number(s)) |
                    Some(BasicToken::Punctuation(s)) |
                    Some(BasicToken::Emoji(s)) => {
                        url += s;
                    },
                }
            }
            let len = url.len();
            let tok = PositionalToken {
                offset: self.offset,
                length: len,
                token: Token::Url(url),
            };
            self.offset += len;
            Some(tok)
        } else { None }
    }
    fn check_hashtag(&mut self) -> Option<PositionalToken> {
        let tok = if self.buffer.len()>1 {
            match (&self.buffer[0],&self.buffer[1]) {
                (BasicToken::Punctuation("#"),BasicToken::Alphanumeric(s)) |
                (BasicToken::Punctuation("#"),BasicToken::Number(s)) => {
                    let tok = PositionalToken {
                        offset: self.offset,
                        length: s.len()+1,
                        token: Token::Hashtag(format!("#{}",s)),
                    };
                    self.offset += s.len()+1;
                    Some(tok)
                },
                _ => None,
            }
        } else { None };
        if tok.is_some() {
            self.buffer.pop_front();
            self.buffer.pop_front();
        }
        tok
    }
    fn check_mention(&mut self) -> Option<PositionalToken> {
        let tok = if self.buffer.len()>1 {
            match (&self.buffer[0],&self.buffer[1]) {
                (BasicToken::Punctuation("@"),BasicToken::Alphanumeric(s)) |
                (BasicToken::Punctuation("@"),BasicToken::Number(s)) => {
                    let tok = PositionalToken {
                        offset: self.offset,
                        length: s.len()+1,
                        token: Token::Hashtag(format!("@{}",s)),
                    };
                    self.offset += s.len()+1;
                    Some(tok)
                },
                _ => None,
            }
        } else { None };
        if tok.is_some() {
            self.buffer.pop_front();
            self.buffer.pop_front();
        }
        tok
    }
}

impl<'t> Tokenizer for Tokens<'t> {
    fn next_token(&mut self) -> Option<PositionalToken> {
        if self.buffer.len()>0 {
            if let Some(t) = self.check_url() { return Some(t); }
            if let Some(t) = self.check_hashtag() { return Some(t); }
            if let Some(t) = self.check_mention() { return Some(t); }
            match self.buffer.pop_front() {
                Some(BasicToken::Alphanumeric(s)) => Some(self.basic_alphanumeric_to_pt(s)),
                Some(BasicToken::Number(s)) => Some(self.basic_number_to_pt(s)),
                Some(BasicToken::Punctuation(s)) => Some(self.basic_punctuation_to_pt(s)),
                Some(BasicToken::Emoji(s)) => Some(self.basic_emoji_to_pt(s)),
                Some(BasicToken::Separator(s)) => Some(self.basic_separator_to_pt(s)),
                None => unreachable!(),
            }
        } else {
            loop {
                match self.bounds.next() {
                    Some(BasicToken::Separator(s)) => {
                        self.buffer.push_back(BasicToken::Separator(s));
                        return self.next_token();
                    },
                    Some(bt) => self.buffer.push_back(bt),
                    None if self.buffer.len()>0 => return self.next_token(),
                    None => return None,
                }
            }
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
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn main() {
        let uws = "The quick (\"brown\") fox can't jump 32.3 feet, right? 4pda etc. qeq U.S.A  asd\n\n\nBrr, it's 29.3°F!\n Русское предложение #36.6 для тестирования деления по юникод-словам...\n🇷🇺 🇸🇹\n👱🏿👶🏽👨🏽\n👱\nС.С.С.Р.\n👨‍👩‍👦‍👦\n🧠\nSome ##text with #hashtags and @other components\nadfa wdsfdf asdf asd http://asdfasdfsd.com/fasdfd/sadfsadf/sdfas/12312_12414/asdf?fascvx=fsfwer&dsdfasdf=fasdf#fasdf asdfa sdfa sdf\nasdfas df asd who@bla-bla.com asdfas df asdfsd\n";
        println!("{}\n",uws);
        for tok in uws.into_tokens() {
            match &tok.token {
                //Token::Separator(..) | Token::Punctuation(..) => continue,
                _ => println!("{:?} [{}]",tok,&uws[tok.offset .. tok.offset+tok.length]),
            }
        }
        panic!("")
    }
}
 */
