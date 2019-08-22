#[macro_use]
extern crate lazy_static;
extern crate unicode_segmentation;
extern crate regex;

use regex::Regex;

use unicode_segmentation::{UnicodeSegmentation,UWordBounds};
use std::str::FromStr;
use std::collections::{VecDeque,BTreeSet,BTreeMap};

mod emoji;

pub use emoji::EMOJIMAP;



#[derive(Debug,Clone,Copy,PartialEq,PartialOrd)]
pub enum Number {
    Integer(i64),
    Float(f64),
}

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub enum Numerical {
    //Date(String),
    //Ip(String),
    DotSeparated(String),
    Measures(String),
    //Countable(String),
    Alphanumeric(String),
}

#[derive(Debug,Clone,Copy,Eq,PartialEq,Ord,PartialOrd)]
pub enum Separator {
    Space,
    Tab,
    Newline,
    Unknown,
}

#[derive(Debug,Clone,PartialEq,PartialOrd,Eq)]
pub enum BasicToken<'t> {
    Alphanumeric(&'t str),
    Number(&'t str),
    Punctuation(&'t str),
    Separator(&'t str),
    Mixed(&'t str),
}
impl<'t> BasicToken<'t> {
    fn len(&self) -> usize {
        match &self {
            BasicToken::Alphanumeric(s) |
            BasicToken::Number(s) |
            BasicToken::Punctuation(s) |
            BasicToken::Mixed(s) |
            BasicToken::Separator(s) => s.len(),
        }
    }
}

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub enum Token {
    Word(String),
    StrangeWord(String),
    Numerical(Numerical),
    Hashtag(String),
    Mention(String),
    Punctuation(String),
    Number(Number),
    Emoji(String),
    Unicode(String),
    Separator(Separator),
    Url(String),
    BBCode { text: Vec<Token>, data: Vec<Token> },
}

#[derive(Debug,Clone,PartialEq,PartialOrd)]
pub struct PositionalToken {
    pub offset: usize,
    pub length: usize,
    pub token: Token,   
}

#[derive(Debug,Copy,Clone,PartialEq,Eq,PartialOrd,Ord)]
pub enum TokenizerOptions {
    DetectHtml,
    DetectBBCode,
    RusLatConversion,
}

enum ConvertBothPolicy {
    Immutable,
    ToCyrillic,
    ToLatin,
}
enum WordStatus {
    Cyrillic,
    Latin,
    Both,
    MixedLatinable,
    MixedCyrillicable,
    Mixed,
    Unknown,
}
struct WordConvertor {
    cyrillic: BTreeSet<char>,
    latin: BTreeSet<char>,
    cyr2lat: BTreeMap<char,char>,
    lat2cyr: BTreeMap<char,char>,

    policy: ConvertBothPolicy,
}
impl WordConvertor {
    fn new() -> WordConvertor {
        let similar: Vec<(char,char)>  /* rus,lat */ = vec![
            ('а','a'),('е','e'),('к','k'),('о','o'),('р','p'),('с','c'),('у','y'),('х','x'),('и','u'),('п','n'),//('т','m'),('м','m'),('н','h'),('т','t'), r -> г р
            ('А','A'),('В','B'),('Е','E'),('К','K'),('М','M'),('Н','H'),('О','O'),('Р','P'),('С','C'),('Т','T'),('У','Y'),('Х','X'),
            ];
        WordConvertor {
            cyrillic: "абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ".chars().collect(),
            latin: "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".chars().collect(),
            cyr2lat: similar.iter().cloned().collect(),
            lat2cyr: similar.iter().cloned().map(|(c,l)| (l,c)).collect(),
            policy: ConvertBothPolicy::Immutable,
        }
    }
    fn set_policy(&mut self, policy: ConvertBothPolicy) {
        self.policy = policy;
    }
    fn word_status(&self,s: &str) -> WordStatus {
        let mut cyr = 0;
        let mut lat = 0;
        let mut sim_cyr = 0;
        let mut sim_lat = 0;
        for c in s.chars() {
            if self.cyrillic.contains(&c) {
                match self.cyr2lat.contains_key(&c) {
                    true => sim_cyr += 1,
                    false => cyr += 1,
                }
            }
            if self.latin.contains(&c) {
                match self.lat2cyr.contains_key(&c) {
                    true => sim_lat += 1,
                    false => lat += 1,
                }
            }
        }
        match (cyr,lat) {
            (0,0) => match sim_cyr+sim_lat {
                0 => { WordStatus::Unknown },
                _ => { WordStatus::Both },
            },
            (0,_) => match sim_cyr {
                0 => { WordStatus::Latin },
                _ => { WordStatus::MixedLatinable },
            },
            (_,0) => match sim_lat {
                0 => { WordStatus::Cyrillic },
                _ => { WordStatus::MixedCyrillicable },
            },
            (_,_) => { WordStatus::Mixed },
        }
    }
    fn to_cyrillic(&self, s: &str) -> String {
        let mut res = String::with_capacity(s.len());
        for c in s.chars() {
            match self.lat2cyr.get(&c) {
                None => res.push(c),
                Some(c) => res.push(*c),
            }
        }
        res
    }
    fn to_latin(&self, s: &str) -> String {
        let mut res = String::with_capacity(s.len());
        for c in s.chars() {
            match self.cyr2lat.get(&c) {
                None => res.push(c),
                Some(c) => res.push(*c),
            }
        }
        res
    }
    fn convert(&self,s: &str) -> String {
        let status = self.word_status(s);
        match (status,&self.policy) {
            (WordStatus::Cyrillic,_) => s.to_string(),
            (WordStatus::Latin,_) => s.to_string(),       
            (WordStatus::MixedLatinable,_) => self.to_latin(s),
            (WordStatus::MixedCyrillicable,_) => self.to_cyrillic(s),
            (_,ConvertBothPolicy::Immutable) => s.to_string(),
            (_,ConvertBothPolicy::ToCyrillic) => self.to_cyrillic(s),
            (_,ConvertBothPolicy::ToLatin) => self.to_latin(s),
        }
    }
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
                let mut dot_count = 0;
                for c in w.chars() {
                    an = an && (c.is_alphanumeric() || (c == '.') || (c == '\'') || (c == '-') || (c == '+') || (c == '_'));
                    num = num && (c.is_digit(10) || (c == '.') || (c == '-') || (c == '+'));
                    if c == '.' { dot_count += 1; }
                }
                if dot_count>1 { num = false; }
                self.offset += w.len();
                if num {
                    return Some(BasicToken::Number(w));
                }
                if an {
                    return Some(BasicToken::Alphanumeric(w));
                }
                Some(BasicToken::Mixed(w))
            },
            None => None,
        }
    }
}

pub trait Tokenizer {
    fn next_token(&mut self) -> Option<PositionalToken>;
}

fn detect_bbcodes(s: &str) -> VecDeque<(usize,usize,usize)> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"\[(.*?)\|(.*?)\]").unwrap();
    }
    let mut res = VecDeque::new(); 
    for cap in RE.captures_iter(s) {
        //println!("{:?} {:?}",cap,cap.get(0).map(|m0| (m0.start(),m0.end()-m0.start())));
        match (cap.get(0),cap.get(1),cap.get(2)) {
            (Some(m0),Some(m1),Some(m2)) => res.push_back((m0.start(),m1.end()-m1.start(),m2.end()-m2.start())),
            _ => continue,
        }
    }
    res
}

fn detect_html(s: &str) -> usize {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"</?\w+?.*?>").unwrap();
    }
    let mut res = VecDeque::new(); 
    for cap in RE.captures_iter(s) {
        //println!("{:?} {:?}",cap,cap.get(0).map(|m0| (m0.start(),m0.end()-m0.start())));
        match cap.get(0) {
            Some(m0) => res.push_back((m0.start(),m0.end()-m0.start())),
            _ => continue,
        }
    }
    res.len()
}

#[derive(Debug,Default)]
struct WC {
    cyrillic: usize,
    latin: usize,
    both: usize,
    mixed: usize,
    unknown: usize,
}
fn detect_conversion(s: &str) -> Option<WordConvertor> {
    let mut wconv = WordConvertor::new();
    let mut before = WC::default();
    let mut after = WC::default();
    for tok in s.into_tokens_with_options(vec![TokenizerOptions::DetectBBCode].into_iter().collect()).unwrap() {
        match &tok.token {
            Token::Word(s) => {
                match wconv.word_status(s) {
                    WordStatus::Unknown => { before.unknown += 1; after.unknown += 1; },
                    WordStatus::Both => { before.both += 1; after.both += 1; },
                    WordStatus::Latin => { before.latin += 1; after.latin +=1; },
                    WordStatus::MixedLatinable => { before.mixed += 1; after.latin +=1; },
                    WordStatus::Cyrillic => { before.cyrillic += 1; after.cyrillic +=1; },
                    WordStatus::MixedCyrillicable => { before.mixed += 1; after.cyrillic +=1; },
                    WordStatus::Mixed => { before.mixed += 1; after.mixed +=1; },
                }
            },
            _ => continue,
        }
    }
    //println!("{:?}",before);
    //println!("{:?}",after);
    if (before.mixed>0)&&((f64::from(after.mixed as u32)/f64::from(before.mixed as u32))<0.5) {
        if (after.cyrillic>0)&&((f64::from(after.latin as u32)/f64::from(after.cyrillic as u32))<0.1) {
            wconv.set_policy(ConvertBothPolicy::ToCyrillic);
        }
        if (after.latin>0)&&((f64::from(after.cyrillic as u32)/f64::from(after.latin as u32))<0.1) {
            wconv.set_policy(ConvertBothPolicy::ToLatin);
        }
        Some(wconv)
    } else {
        None
    }
}

#[derive(Debug)]
pub enum Untokenizable {
    Html,
}

pub struct Tokens<'t> {
    offset: usize,
    bounds: Breaker<'t>,
    buffer: VecDeque<BasicToken<'t>>,
    bbcodes: VecDeque<(usize,usize,usize)>,
    wconv: Option<WordConvertor>,
}
impl<'t> Tokens<'t> {
    fn new<'a>(s: &'a str, options: BTreeSet<TokenizerOptions>) -> Result<Tokens<'a>,Untokenizable> {
        if options.contains(&TokenizerOptions::DetectHtml)&&(detect_html(s)>5) {
            return Err(Untokenizable::Html)
        }
        Ok(Tokens {
            offset: 0,
            bounds: Breaker::new(s),
            buffer: VecDeque::new(),
            bbcodes: if options.contains(&TokenizerOptions::DetectBBCode) { detect_bbcodes(s) } else { VecDeque::new() },
            wconv: if options.contains(&TokenizerOptions::RusLatConversion) { detect_conversion(s) } else { None },
        })
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
    fn basic_mixed_to_pt(&mut self, s: &str) -> PositionalToken {
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
        /*
        Word
        StrangeWord
        pub enum Numerical {
            Date(String),
            Ip(String),
            DotSeparated(String),
            Countable(String),
            Measures(String),
            Alphanumeric(String),
        }*/
        //let mut wrd = true;
        let mut digits = false;
        let mut digits_begin_only = false;
        let mut dots = false;
        let mut alphas_and_apos = false;
        let mut other = false;

        let mut start_digit = true;
        for c in s.chars() {
            if start_digit && (!c.is_digit(10)) { start_digit = false; }
            match c {
                c @ _ if c.is_digit(10) => {
                    digits = true;
                    if start_digit { digits_begin_only = true; }
                    else { digits_begin_only = false; }
                },
                c @ _ if c.is_alphabetic() => { alphas_and_apos = true; },
                '\'' => { alphas_and_apos = true; },
                '.' => { dots = true; },
                _ => { other = true; },
            }
        }
        let tok = PositionalToken {
            offset: self.offset,
            length: s.len(),
            token: match (digits,digits_begin_only,dots,alphas_and_apos,other) {
                (true,false,true,false,false) => {
                    // TODO: Date, Ip, DotSeparated
                    Token::Numerical(Numerical::DotSeparated(s.to_string()))
                },
                (true,true,_,true,false) => {
                    // TODO: Countable or Measures
                    Token::Numerical(Numerical::Measures(s.to_string()))
                },
                (true, _, _, _, _) => {
                    // Numerical trash, ids, etc.
                    Token::Numerical(Numerical::Alphanumeric(s.to_string()))
                }
                (false,false,_,true,false) => {
                    // Word
                    Token::Word(match &self.wconv {
                        None => s.to_string(),
                        Some(wconv) => wconv.convert(s),
                    })
                },
                (false,false,_,_,_) => {
                    // Strange
                    Token::StrangeWord(s.to_string())
                },
                (false,true,_,_,_) => unreachable!(),
            },
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
            let tag_bound = {
                if self.bbcodes.len()>0 { Some(self.bbcodes[0].0) } else { None }
            };
            loop {
                if let Some(b) = tag_bound {
                    if (self.offset + url.len()) >= b { break; }
                }
                match self.buffer.pop_front() {
                    None => break,
                    Some(BasicToken::Separator(s)) => {
                        self.buffer.push_front(BasicToken::Separator(s));
                        break;
                    },
                    Some(BasicToken::Alphanumeric(s)) |
                    Some(BasicToken::Number(s)) |
                    Some(BasicToken::Punctuation(s)) |
                    Some(BasicToken::Mixed(s)) => {
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
                        token: Token::Hashtag(format!("{}",s)),
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
                        token: Token::Mention(format!("{}",s)),
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
    fn check_bb_code(&mut self, text_len: usize, data_len: usize) -> Option<PositionalToken> {
        if self.buffer.len() >= (text_len+data_len+3) {
            if (self.buffer[0] == BasicToken::Punctuation("["))&&
                (self.buffer[text_len+1] == BasicToken::Punctuation("|"))&&
                (self.buffer[text_len+data_len+2] == BasicToken::Punctuation("]")) {
                    let offset = self.offset;
                    self.buffer.pop_front(); self.offset += 1;
                    let mut tail = self.buffer.split_off(text_len);
                    let mut text_vec = Vec::new(); 
                    while let Some(t) = self.next_from_buffer() {
                        text_vec.push(t.token);
                    }
                    std::mem::swap(&mut tail,&mut self.buffer);
                    self.buffer.pop_front(); self.offset += 1;
                    tail = self.buffer.split_off(data_len);
                    let mut data_vec = Vec::new(); 
                    while let Some(t) = self.next_from_buffer() {
                        data_vec.push(t.token);
                    }
                    std::mem::swap(&mut tail,&mut self.buffer);
                    self.buffer.pop_front(); self.offset += 1;
                    // vk bbcode check
                    if (text_vec.len()==1)&&(match text_vec[0] {
                        Token::Numerical(Numerical::Alphanumeric(..)) => true,
                        _ => false,
                    }) {
                        std::mem::swap(&mut text_vec,&mut data_vec);
                    }
                    Some(PositionalToken {
                        offset: offset,
                        length: self.offset - offset,
                        token: Token::BBCode{ text: text_vec, data: data_vec },
                    })
                } else { None }
        } else { None }

    }
    fn next_from_buffer(&mut self) -> Option<PositionalToken> {
        if let Some(t) = self.check_url() { return Some(t); }
        if let Some(t) = self.check_hashtag() { return Some(t); }
        if let Some(t) = self.check_mention() { return Some(t); }
        match self.buffer.pop_front() {
            Some(BasicToken::Alphanumeric(s)) => Some(self.basic_alphanumeric_to_pt(s)),
            Some(BasicToken::Number(s)) => Some(self.basic_number_to_pt(s)),
            Some(BasicToken::Punctuation(s)) => Some(self.basic_punctuation_to_pt(s)),
            Some(BasicToken::Mixed(s)) => Some(self.basic_mixed_to_pt(s)),
            Some(BasicToken::Separator(s)) => Some(self.basic_separator_to_pt(s)),
            None => None,
        }
    }
}

impl<'t> Tokenizer for Tokens<'t> {
    fn next_token(&mut self) -> Option<PositionalToken> { loop {
        if self.buffer.len()>0 {
            if (self.bbcodes.len()>0)&&(self.bbcodes[0].0 == self.offset) {
                let get_len = self.bbcodes[0].1 + self.bbcodes[0].2 + 3;
                let (text_from,text_len) = (self.bbcodes[0].0+1,self.bbcodes[0].1);
                let (text2_from,text2_len) = (self.bbcodes[0].0+self.bbcodes[0].1+2,self.bbcodes[0].2);
                let mut cur_len = 0;
                let mut cur_off = self.offset;
                let mut buf1_len = 0;
                let mut buf2_len = 0;
                for bt in &self.buffer {
                    if (cur_off>=text_from)&&(cur_off<(text_from+text_len)) { buf1_len += 1; } 
                    if (cur_off>=text2_from)&&(cur_off<(text2_from+text2_len)) { buf2_len += 1; }
                    cur_off += bt.len();
                    cur_len += bt.len();
                }
                while cur_len<get_len {
                    match self.bounds.next() {
                        None => break,
                        Some(bt) => {
                            if (cur_off>=text_from)&&(cur_off<(text_from+text_len)) { buf1_len += 1; } 
                            if (cur_off>=text2_from)&&(cur_off<(text2_from+text2_len)) { buf2_len += 1; }
                            cur_off += bt.len();
                            cur_len += bt.len();
                            self.buffer.push_back(bt);
                        }
                    }
                }
                //println!("{:?} {} {} {}",self.bbcodes[0],self.buffer.len(),buf1_len,buf2_len);
                //println!("{:?}",self.buffer);
                self.bbcodes.pop_front();
                if let Some(t) = self.check_bb_code(buf1_len,buf2_len) { return Some(t); }
            }
            return self.next_from_buffer();
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
    }}
}

impl<'t> Iterator for Tokens<'t> {
    type Item = PositionalToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

pub trait IntoTokenizer {
    type IntoTokens: Tokenizer;
    fn into_tokens(self) -> Result<Self::IntoTokens,Untokenizable>;
    fn into_tokens_with_options(self, options:BTreeSet<TokenizerOptions>) -> Result<Self::IntoTokens,Untokenizable>;
}
impl<'t> IntoTokenizer for &'t str {
    type IntoTokens = Tokens<'t>;
    fn into_tokens(self) -> Result<Self::IntoTokens,Untokenizable> {
        Tokens::new(self,vec![TokenizerOptions::DetectBBCode,TokenizerOptions::RusLatConversion,TokenizerOptions::DetectHtml].into_iter().collect())
    }
    fn into_tokens_with_options(self, options:BTreeSet<TokenizerOptions>) -> Result<Self::IntoTokens,Untokenizable> {
        Tokens::new(self,options)
    }
}



#[cfg(test)]
mod test {
    use super::*;

    fn print_pt(tok: &PositionalToken) {
        let mut r = format!("PositionalToken {{ offset: {}, length: {}, token: Token::{:?} }},",tok.offset,tok.length,tok.token);
        r = r.replace("\")","\".to_string())");
        r = r.replace("Separator(","Separator(Separator::");
        r = r.replace("Number(","Number(Number::");
        r = r.replace("Numerical(","Numerical(Numerical::");
        println!("{}",r);
    }

    fn print_result(lib_res: &Vec<PositionalToken>) {
        for tok in lib_res {        
            print_pt(&tok);
        }
    }

    fn check_results(result: &Vec<PositionalToken>, lib_res: &Vec<PositionalToken>, uws: &str) {
        if result.len()!=lib_res.len() { assert_eq!(result,lib_res); }
        for i in 0 .. result.len() {
            assert_eq!(result[i],lib_res[i]);
            /*let tok = &lib_res[i];
            match &tok.token {
                Token::Punctuation(s) => assert_eq!(s,&uws[tok.offset .. tok.offset+tok.length]),
                _ => {},
            }*/
        }
    }
    
    #[test]
    fn general() {
        let uws = "The quick (\"brown\") fox can't jump 32.3 feet, right? 4pda etc. qeq U.S.A  asd\n\n\nBrr, it's 29.3°F!\n Русское предложение #36.6 для тестирования деления по юникод-словам...\n";
        let result = vec![
            PositionalToken { offset: 0, length: 3, token: Token::Word("The".to_string()) },
            PositionalToken { offset: 3, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 4, length: 5, token: Token::Word("quick".to_string()) },
            PositionalToken { offset: 9, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 10, length: 1, token: Token::Punctuation("(".to_string()) },
            PositionalToken { offset: 11, length: 1, token: Token::Punctuation("\"".to_string()) },
            PositionalToken { offset: 12, length: 5, token: Token::Word("brown".to_string()) },
            PositionalToken { offset: 17, length: 1, token: Token::Punctuation("\"".to_string()) },
            PositionalToken { offset: 18, length: 1, token: Token::Punctuation(")".to_string()) },
            PositionalToken { offset: 19, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 20, length: 3, token: Token::Word("fox".to_string()) },
            PositionalToken { offset: 23, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 24, length: 5, token: Token::Word("can\'t".to_string()) },
            PositionalToken { offset: 29, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 30, length: 4, token: Token::Word("jump".to_string()) },
            PositionalToken { offset: 34, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 35, length: 4, token: Token::Number(Number::Float(32.3)) },
            PositionalToken { offset: 39, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 40, length: 4, token: Token::Word("feet".to_string()) },
            PositionalToken { offset: 44, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 45, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 46, length: 5, token: Token::Word("right".to_string()) },
            PositionalToken { offset: 51, length: 1, token: Token::Punctuation("?".to_string()) },
            PositionalToken { offset: 52, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 53, length: 4, token: Token::Numerical(Numerical::Measures("4pda".to_string())) }, // TODO
            PositionalToken { offset: 57, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 58, length: 3, token: Token::Word("etc".to_string()) },
            PositionalToken { offset: 61, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 62, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 63, length: 3, token: Token::Word("qeq".to_string()) },
            PositionalToken { offset: 66, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 67, length: 5, token: Token::Word("U.S.A".to_string()) },
            PositionalToken { offset: 72, length: 2, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 74, length: 3, token: Token::Word("asd".to_string()) },
            PositionalToken { offset: 77, length: 3, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 80, length: 3, token: Token::Word("Brr".to_string()) },
            PositionalToken { offset: 83, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 84, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 85, length: 4, token: Token::Word("it\'s".to_string()) },
            PositionalToken { offset: 89, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 90, length: 4, token: Token::Number(Number::Float(29.3)) },
            PositionalToken { offset: 94, length: 2, token: Token::Unicode("ub0".to_string()) },
            PositionalToken { offset: 96, length: 1, token: Token::Word("F".to_string()) },
            PositionalToken { offset: 97, length: 1, token: Token::Punctuation("!".to_string()) },
            PositionalToken { offset: 98, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 99, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 100, length: 14, token: Token::Word("Русское".to_string()) },
            PositionalToken { offset: 114, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 115, length: 22, token: Token::Word("предложение".to_string()) },
            PositionalToken { offset: 137, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 138, length: 5, token: Token::Hashtag("36.6".to_string()) },
            PositionalToken { offset: 143, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 144, length: 6, token: Token::Word("для".to_string()) },
            PositionalToken { offset: 150, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 151, length: 24, token: Token::Word("тестирования".to_string()) },
            PositionalToken { offset: 175, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 176, length: 14, token: Token::Word("деления".to_string()) },
            PositionalToken { offset: 190, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 191, length: 4, token: Token::Word("по".to_string()) },
            PositionalToken { offset: 195, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 196, length: 12, token: Token::Word("юникод".to_string()) },
            PositionalToken { offset: 208, length: 1, token: Token::Punctuation("-".to_string()) },
            PositionalToken { offset: 209, length: 12, token: Token::Word("словам".to_string()) },
            PositionalToken { offset: 221, length: 3, token: Token::Punctuation("...".to_string()) },
            PositionalToken { offset: 224, length: 1, token: Token::Separator(Separator::Newline) },
            ];
        let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        check_results(&result,&lib_res,uws);
    }

    #[test]
    fn emoji_and_rusabbr() {
        let uws = "🇷🇺 🇸🇹\n👱🏿👶🏽👨🏽\n👱\nС.С.С.Р.\n👨‍👩‍👦‍👦\n🧠\n";
        let result = vec![
            PositionalToken { offset: 0, length: 8, token: Token::Emoji("russia".to_string()) },
            PositionalToken { offset: 8, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 9, length: 8, token: Token::Emoji("sao_tome_and_principe".to_string()) },
            PositionalToken { offset: 17, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 18, length: 8, token: Token::Emoji("blond_haired_person_dark_skin_tone".to_string()) },
            PositionalToken { offset: 26, length: 8, token: Token::Emoji("baby_medium_skin_tone".to_string()) },
            PositionalToken { offset: 34, length: 8, token: Token::Emoji("man_medium_skin_tone".to_string()) },
            PositionalToken { offset: 42, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 43, length: 4, token: Token::Emoji("blond_haired_person".to_string()) },
            PositionalToken { offset: 47, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 48, length: 11, token: Token::Word("С.С.С.Р".to_string()) },
            PositionalToken { offset: 59, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 60, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 61, length: 25, token: Token::Emoji("family_man_woman_boy_boy".to_string()) },
            PositionalToken { offset: 86, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 87, length: 4, token: Token::Emoji("brain".to_string()) },
            PositionalToken { offset: 91, length: 1, token: Token::Separator(Separator::Newline) },
            ];
        let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        check_results(&result,&lib_res,uws);
        //print_result(&lib_res);
    }

    #[test]
    fn hashtags_mentions_urls() {
        let uws = "\nSome ##text with #hashtags and @other components\nadfa wdsfdf asdf asd http://asdfasdfsd.com/fasdfd/sadfsadf/sdfas/12312_12414/asdf?fascvx=fsfwer&dsdfasdf=fasdf#fasdf asdfa sdfa sdf\nasdfas df asd who@bla-bla.com asdfas df asdfsd\n";
        let result = vec![
            PositionalToken { offset: 0, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 1, length: 4, token: Token::Word("Some".to_string()) },
            PositionalToken { offset: 5, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 6, length: 2, token: Token::Punctuation("##".to_string()) },
            PositionalToken { offset: 8, length: 4, token: Token::Word("text".to_string()) },
            PositionalToken { offset: 12, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 13, length: 4, token: Token::Word("with".to_string()) },
            PositionalToken { offset: 17, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 18, length: 9, token: Token::Hashtag("hashtags".to_string()) },
            PositionalToken { offset: 27, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 28, length: 3, token: Token::Word("and".to_string()) },
            PositionalToken { offset: 31, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 32, length: 6, token: Token::Mention("other".to_string()) },
            PositionalToken { offset: 38, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 39, length: 10, token: Token::Word("components".to_string()) },
            PositionalToken { offset: 49, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 50, length: 4, token: Token::Word("adfa".to_string()) },
            PositionalToken { offset: 54, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 55, length: 6, token: Token::Word("wdsfdf".to_string()) },
            PositionalToken { offset: 61, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 62, length: 4, token: Token::Word("asdf".to_string()) },
            PositionalToken { offset: 66, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 67, length: 3, token: Token::Word("asd".to_string()) },
            PositionalToken { offset: 70, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 71, length: 95, token: Token::Url("http://asdfasdfsd.com/fasdfd/sadfsadf/sdfas/12312_12414/asdf?fascvx=fsfwer&dsdfasdf=fasdf#fasdf".to_string()) },
            PositionalToken { offset: 166, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 167, length: 5, token: Token::Word("asdfa".to_string()) },
            PositionalToken { offset: 172, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 173, length: 4, token: Token::Word("sdfa".to_string()) },
            PositionalToken { offset: 177, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 178, length: 3, token: Token::Word("sdf".to_string()) },
            PositionalToken { offset: 181, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 182, length: 6, token: Token::Word("asdfas".to_string()) },
            PositionalToken { offset: 188, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 189, length: 2, token: Token::Word("df".to_string()) },
            PositionalToken { offset: 191, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 192, length: 3, token: Token::Word("asd".to_string()) },
            PositionalToken { offset: 195, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 196, length: 3, token: Token::Word("who".to_string()) },
            PositionalToken { offset: 199, length: 4, token: Token::Mention("bla".to_string()) },
            PositionalToken { offset: 203, length: 1, token: Token::Punctuation("-".to_string()) },
            PositionalToken { offset: 204, length: 7, token: Token::Word("bla.com".to_string()) },
            PositionalToken { offset: 211, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 212, length: 6, token: Token::Word("asdfas".to_string()) },
            PositionalToken { offset: 218, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 219, length: 2, token: Token::Word("df".to_string()) },
            PositionalToken { offset: 221, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 222, length: 6, token: Token::Word("asdfsd".to_string()) },
            PositionalToken { offset: 228, length: 1, token: Token::Separator(Separator::Newline) },
            ];
        let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        check_results(&result,&lib_res,uws);
        //print_result(&lib_res); panic!("")
    }

    #[test]
    fn bb_code() {
        let uws = "[Oxana Putan|1712640565] shared a [post|100001150683379_1873048549410150]. \nAndrew\n[link|https://www.facebook.com/100001150683379/posts/1873048549410150]\nДрузья мои, издатели, редакторы, просветители, культуртрегеры, субъекты мирового рынка и ту хум ит ещё мей консёрн.\nНа текущий момент я лишен былой подвижности, хоть и ковыляю по больничных коридорам по разным нуждам и за кипятком.\nВрачи обещают мне заживление отверстых ран моих в течение полугода и на этот период можно предполагать с уверенностью преимущественно домашний образ жизни.\n[|]";
        let result = vec![
            PositionalToken { offset: 0, length: 24, token: Token::BBCode {
                text: vec![
                    Token::Word("Oxana".to_string()),
                    Token::Separator(Separator::Space),
                    Token::Word("Putan".to_string()) ],
                data: vec![ Token::Number(Number::Integer(1712640565))],
            } },
            PositionalToken { offset: 24, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 25, length: 6, token: Token::Word("shared".to_string()) },
            PositionalToken { offset: 31, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 32, length: 1, token: Token::Word("a".to_string()) },
            PositionalToken { offset: 33, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 34, length: 39, token: Token::BBCode {
                text: vec![ Token::Word("post".to_string()) ],
                data: vec![ Token::Numerical(Numerical::Alphanumeric("100001150683379_1873048549410150".to_string())) ],
            } },
            PositionalToken { offset: 73, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 74, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 75, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 76, length: 6, token: Token::Word("Andrew".to_string()) },
            PositionalToken { offset: 82, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 83, length: 70, token: Token::BBCode {
                text: vec![ Token::Word("link".to_string()) ],
                data: vec![ Token::Url("https://www.facebook.com/100001150683379/posts/1873048549410150".to_string()) ],
            } },
            PositionalToken { offset: 153, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 154, length: 12, token: Token::Word("Друзья".to_string()) },
            PositionalToken { offset: 166, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 167, length: 6, token: Token::Word("мои".to_string()) },
            PositionalToken { offset: 173, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 174, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 175, length: 16, token: Token::Word("издатели".to_string()) },
            PositionalToken { offset: 191, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 192, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 193, length: 18, token: Token::Word("редакторы".to_string()) },
            PositionalToken { offset: 211, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 212, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 213, length: 24, token: Token::Word("просветители".to_string()) },
            PositionalToken { offset: 237, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 238, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 239, length: 28, token: Token::Word("культуртрегеры".to_string()) },
            PositionalToken { offset: 267, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 268, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 269, length: 16, token: Token::Word("субъекты".to_string()) },
            PositionalToken { offset: 285, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 286, length: 16, token: Token::Word("мирового".to_string()) },
            PositionalToken { offset: 302, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 303, length: 10, token: Token::Word("рынка".to_string()) },
            PositionalToken { offset: 313, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 314, length: 2, token: Token::Word("и".to_string()) },
            PositionalToken { offset: 316, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 317, length: 4, token: Token::Word("ту".to_string()) },
            PositionalToken { offset: 321, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 322, length: 6, token: Token::Word("хум".to_string()) },
            PositionalToken { offset: 328, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 329, length: 4, token: Token::Word("ит".to_string()) },
            PositionalToken { offset: 333, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 334, length: 6, token: Token::Word("ещё".to_string()) },
            PositionalToken { offset: 340, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 341, length: 6, token: Token::Word("мей".to_string()) },
            PositionalToken { offset: 347, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 348, length: 14, token: Token::Word("консёрн".to_string()) },
            PositionalToken { offset: 362, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 363, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 364, length: 4, token: Token::Word("На".to_string()) },
            PositionalToken { offset: 368, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 369, length: 14, token: Token::Word("текущий".to_string()) },
            PositionalToken { offset: 383, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 384, length: 12, token: Token::Word("момент".to_string()) },
            PositionalToken { offset: 396, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 397, length: 2, token: Token::Word("я".to_string()) },
            PositionalToken { offset: 399, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 400, length: 10, token: Token::Word("лишен".to_string()) },
            PositionalToken { offset: 410, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 411, length: 10, token: Token::Word("былой".to_string()) },
            PositionalToken { offset: 421, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 422, length: 22, token: Token::Word("подвижности".to_string()) },
            PositionalToken { offset: 444, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 445, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 446, length: 8, token: Token::Word("хоть".to_string()) },
            PositionalToken { offset: 454, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 455, length: 2, token: Token::Word("и".to_string()) },
            PositionalToken { offset: 457, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 458, length: 14, token: Token::Word("ковыляю".to_string()) },
            PositionalToken { offset: 472, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 473, length: 4, token: Token::Word("по".to_string()) },
            PositionalToken { offset: 477, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 478, length: 20, token: Token::Word("больничных".to_string()) },
            PositionalToken { offset: 498, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 499, length: 18, token: Token::Word("коридорам".to_string()) },
            PositionalToken { offset: 517, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 518, length: 4, token: Token::Word("по".to_string()) },
            PositionalToken { offset: 522, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 523, length: 12, token: Token::Word("разным".to_string()) },
            PositionalToken { offset: 535, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 536, length: 12, token: Token::Word("нуждам".to_string()) },
            PositionalToken { offset: 548, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 549, length: 2, token: Token::Word("и".to_string()) },
            PositionalToken { offset: 551, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 552, length: 4, token: Token::Word("за".to_string()) },
            PositionalToken { offset: 556, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 557, length: 16, token: Token::Word("кипятком".to_string()) },
            PositionalToken { offset: 573, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 574, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 575, length: 10, token: Token::Word("Врачи".to_string()) },
            PositionalToken { offset: 585, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 586, length: 14, token: Token::Word("обещают".to_string()) },
            PositionalToken { offset: 600, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 601, length: 6, token: Token::Word("мне".to_string()) },
            PositionalToken { offset: 607, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 608, length: 20, token: Token::Word("заживление".to_string()) },
            PositionalToken { offset: 628, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 629, length: 18, token: Token::Word("отверстых".to_string()) },
            PositionalToken { offset: 647, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 648, length: 6, token: Token::Word("ран".to_string()) },
            PositionalToken { offset: 654, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 655, length: 8, token: Token::Word("моих".to_string()) },
            PositionalToken { offset: 663, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 664, length: 2, token: Token::Word("в".to_string()) },
            PositionalToken { offset: 666, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 667, length: 14, token: Token::Word("течение".to_string()) },
            PositionalToken { offset: 681, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 682, length: 16, token: Token::Word("полугода".to_string()) },
            PositionalToken { offset: 698, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 699, length: 2, token: Token::Word("и".to_string()) },
            PositionalToken { offset: 701, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 702, length: 4, token: Token::Word("на".to_string()) },
            PositionalToken { offset: 706, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 707, length: 8, token: Token::Word("этот".to_string()) },
            PositionalToken { offset: 715, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 716, length: 12, token: Token::Word("период".to_string()) },
            PositionalToken { offset: 728, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 729, length: 10, token: Token::Word("можно".to_string()) },
            PositionalToken { offset: 739, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 740, length: 24, token: Token::Word("предполагать".to_string()) },
            PositionalToken { offset: 764, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 765, length: 2, token: Token::Word("с".to_string()) },
            PositionalToken { offset: 767, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 768, length: 24, token: Token::Word("уверенностью".to_string()) },
            PositionalToken { offset: 792, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 793, length: 30, token: Token::Word("преимущественно".to_string()) },
            PositionalToken { offset: 823, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 824, length: 16, token: Token::Word("домашний".to_string()) },
            PositionalToken { offset: 840, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 841, length: 10, token: Token::Word("образ".to_string()) },
            PositionalToken { offset: 851, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 852, length: 10, token: Token::Word("жизни".to_string()) },
            PositionalToken { offset: 862, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 863, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 864, length: 3, token: Token::BBCode {
                text: Vec::new(),
                data: Vec::new(),
            } },
            ];
        let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        check_results(&result,&lib_res,uws);
        //print_result(&lib_res); panic!("")
    }

    #[test]
    fn ruslat() {
        let uws = "Именнo этoт мужчинa пришёл в вашу жизнь неспрoста\n\nЕсли вы пoнимаете назначение челoвека в жизни, вам станет легче научиться испытывать к нему любoвь, пoтoму чтo будет пoнимание, чтo челoвек в мoей жизни - учитель, и я ему за этo благoдарна.\nПоказать полностью…";
        let result = vec![
            PositionalToken { offset: 0, length: 11, token: Token::Word("Именно".to_string()) },
            PositionalToken { offset: 11, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 12, length: 7, token: Token::Word("этот".to_string()) },
            PositionalToken { offset: 19, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 20, length: 13, token: Token::Word("мужчина".to_string()) },
            PositionalToken { offset: 33, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 34, length: 12, token: Token::Word("пришёл".to_string()) },
            PositionalToken { offset: 46, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 47, length: 2, token: Token::Word("в".to_string()) },
            PositionalToken { offset: 49, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 50, length: 8, token: Token::Word("вашу".to_string()) },
            PositionalToken { offset: 58, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 59, length: 10, token: Token::Word("жизнь".to_string()) },
            PositionalToken { offset: 69, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 70, length: 17, token: Token::Word("неспроста".to_string()) },
            PositionalToken { offset: 87, length: 2, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 89, length: 8, token: Token::Word("Если".to_string()) },
            PositionalToken { offset: 97, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 98, length: 4, token: Token::Word("вы".to_string()) },
            PositionalToken { offset: 102, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 103, length: 17, token: Token::Word("понимаете".to_string()) },
            PositionalToken { offset: 120, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 121, length: 20, token: Token::Word("назначение".to_string()) },
            PositionalToken { offset: 141, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 142, length: 15, token: Token::Word("человека".to_string()) },
            PositionalToken { offset: 157, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 158, length: 2, token: Token::Word("в".to_string()) },
            PositionalToken { offset: 160, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 161, length: 10, token: Token::Word("жизни".to_string()) },
            PositionalToken { offset: 171, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 172, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 173, length: 6, token: Token::Word("вам".to_string()) },
            PositionalToken { offset: 179, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 180, length: 12, token: Token::Word("станет".to_string()) },
            PositionalToken { offset: 192, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 193, length: 10, token: Token::Word("легче".to_string()) },
            PositionalToken { offset: 203, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 204, length: 18, token: Token::Word("научиться".to_string()) },
            PositionalToken { offset: 222, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 223, length: 20, token: Token::Word("испытывать".to_string()) },
            PositionalToken { offset: 243, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 244, length: 2, token: Token::Word("к".to_string()) },
            PositionalToken { offset: 246, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 247, length: 8, token: Token::Word("нему".to_string()) },
            PositionalToken { offset: 255, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 256, length: 11, token: Token::Word("любовь".to_string()) },
            PositionalToken { offset: 267, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 268, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 269, length: 10, token: Token::Word("потому".to_string()) },
            PositionalToken { offset: 279, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 280, length: 5, token: Token::Word("что".to_string()) },
            PositionalToken { offset: 285, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 286, length: 10, token: Token::Word("будет".to_string()) },
            PositionalToken { offset: 296, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 297, length: 17, token: Token::Word("понимание".to_string()) },
            PositionalToken { offset: 314, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 315, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 316, length: 5, token: Token::Word("что".to_string()) },
            PositionalToken { offset: 321, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 322, length: 13, token: Token::Word("человек".to_string()) },
            PositionalToken { offset: 335, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 336, length: 2, token: Token::Word("в".to_string()) },
            PositionalToken { offset: 338, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 339, length: 7, token: Token::Word("моей".to_string()) },
            PositionalToken { offset: 346, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 347, length: 10, token: Token::Word("жизни".to_string()) },
            PositionalToken { offset: 357, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 358, length: 1, token: Token::Punctuation("-".to_string()) },
            PositionalToken { offset: 359, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 360, length: 14, token: Token::Word("учитель".to_string()) },
            PositionalToken { offset: 374, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 375, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 376, length: 2, token: Token::Word("и".to_string()) },
            PositionalToken { offset: 378, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 379, length: 2, token: Token::Word("я".to_string()) },
            PositionalToken { offset: 381, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 382, length: 6, token: Token::Word("ему".to_string()) },
            PositionalToken { offset: 388, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 389, length: 4, token: Token::Word("за".to_string()) },
            PositionalToken { offset: 393, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 394, length: 5, token: Token::Word("это".to_string()) },
            PositionalToken { offset: 399, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 400, length: 19, token: Token::Word("благодарна".to_string()) },
            PositionalToken { offset: 419, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 420, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 421, length: 16, token: Token::Word("Показать".to_string()) },
            PositionalToken { offset: 437, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 438, length: 18, token: Token::Word("полностью".to_string()) },
            PositionalToken { offset: 456, length: 3, token: Token::Unicode("u2026".to_string()) },
            ];
        let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        check_results(&result,&lib_res,uws);
        //print_result(&lib_res); panic!("")
    }
    
 

    #[test]
    fn html() {
        let uws = "<div class=\"article article_view \" id=\"article_view_-113039156_9551\" data-article-url=\"/@chaibuket-o-chem-ne-zabyt-25-noyabrya\" data-audio-context=\"article:-113039156_9551\"><h1  class=\"article_decoration_first article_decoration_last\" >День Мамы </h1><p  class=\"article_decoration_first article_decoration_last\" >День, когда поздравляют мам, бабушек, сестер и жён — это всемирный праздник, называемый «День Мамы». В настоящее время его отмечают почти в каждой стране, просто везде разные даты и способы празднования. </p><h3  class=\"article_decoration_first article_decoration_last\" ><span class='article_anchor_title'>\n  <span class='article_anchor_button' id='pochemu-my-ego-prazdnuem'></span>\n  <span class='article_anchor_fsymbol'>П</span>\n</span>ПОЧЕМУ МЫ ЕГО ПРАЗДНУЕМ</h3><p  class=\"article_decoration_first article_decoration_last article_decoration_before\" >В 1987 году комитет госдумы по делам женщин, семьи и молодежи выступил с предложением учредить «День мамы», а сам приказ был подписан уже 30 января 1988 года Борисом Ельциным. Было решено, что ежегодно в России празднество дня мамы будет выпадать на последнее воскресенье ноября. </p><figure data-type=\"101\" data-mode=\"\"  class=\"article_decoration_first article_decoration_last\" >\n  <div class=\"article_figure_content\" style=\"width: 1125px\">\n    <div class=\"article_figure_sizer_content\"><div class=\"article_object_sizer_wrap\" data-sizes=\"[{&quot;s&quot;:[&quot;https://pp.userapi.com/c849128/v849128704/c0ffd/pcNJaBH3NDo.jpg&quot;,75,50],&quot;m&quot;:[&quot;https://pp.userapi.com/c849128/v849128704/c0ffe/ozCLs2kHtRY.jpg&quot;,130,87],&quot;x&quot;:[&quot;https://pp.userapi.com/c849128/v849128704/c0fff/E4KtTNDydzE.jpg&quot;,604,403],&quot;y&quot;:[&quot;https://pp.userapi.com/c849128/v849128704/c1000/1nLxpYKavzU.jpg&quot;,807,538],&quot;z&quot;:[&quot;https://pp.userapi.com/c849128/v849128704/c1001/IgEODe90yEk.jpg&quot;,1125,750],&quot;o&quot;:[&quot;https://pp.userapi.com/c849128/v849128704/c1002/01faNwVZ2_E.jpg&quot;,130,87],&quot;p&quot;:[&quot;https://pp.userapi.com/c849128/v849128704/c1003/baDFzbdRP2s.jpg&quot;,200,133],&quot;q&quot;:[&quot;https://pp.userapi.com/c849128/v849128704/c1004/CY4khI6KJKA.jpg&quot;,320,213],&quot;r&quot;:[&quot;https://pp.userapi.com/c849128/v849128704/c1005/NOvAJ6-VltY.jpg&quot;,510,340]}]\">\n  <img class=\"article_object_sizer_inner article_object_photo__image_blur\" src=\"https://pp.userapi.com/c849128/v849128704/c0ffd/pcNJaBH3NDo.jpg\" data-baseurl=\"\"/>\n  \n</div></div>\n    <div class=\"article_figure_sizer\" style=\"padding-bottom: 66.666666666667%\"></div>";
        let result = vec![
            PositionalToken { offset: 236, length: 8, token: Token::Word("День".to_string()) },
            PositionalToken { offset: 244, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 245, length: 8, token: Token::Word("Мамы".to_string()) },
            PositionalToken { offset: 253, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 321, length: 8, token: Token::Word("День".to_string()) },
            PositionalToken { offset: 329, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 330, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 331, length: 10, token: Token::Word("когда".to_string()) },
            PositionalToken { offset: 341, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 342, length: 22, token: Token::Word("поздравляют".to_string()) },
            PositionalToken { offset: 364, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 365, length: 6, token: Token::Word("мам".to_string()) },
            PositionalToken { offset: 371, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 372, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 373, length: 14, token: Token::Word("бабушек".to_string()) },
            PositionalToken { offset: 387, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 388, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 389, length: 12, token: Token::Word("сестер".to_string()) },
            PositionalToken { offset: 401, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 402, length: 2, token: Token::Word("и".to_string()) },
            PositionalToken { offset: 404, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 405, length: 6, token: Token::Word("жён".to_string()) },
            PositionalToken { offset: 411, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 412, length: 3, token: Token::Unicode("u2014".to_string()) },
            PositionalToken { offset: 415, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 416, length: 6, token: Token::Word("это".to_string()) },
            PositionalToken { offset: 422, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 423, length: 18, token: Token::Word("всемирный".to_string()) },
            PositionalToken { offset: 441, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 442, length: 16, token: Token::Word("праздник".to_string()) },
            PositionalToken { offset: 458, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 459, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 460, length: 20, token: Token::Word("называемый".to_string()) },
            PositionalToken { offset: 480, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 481, length: 2, token: Token::Unicode("uab".to_string()) },
            PositionalToken { offset: 483, length: 8, token: Token::Word("День".to_string()) },
            PositionalToken { offset: 491, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 492, length: 8, token: Token::Word("Мамы".to_string()) },
            PositionalToken { offset: 500, length: 2, token: Token::Unicode("ubb".to_string()) },
            PositionalToken { offset: 502, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 503, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 504, length: 2, token: Token::Word("В".to_string()) },
            PositionalToken { offset: 506, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 507, length: 18, token: Token::Word("настоящее".to_string()) },
            PositionalToken { offset: 525, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 526, length: 10, token: Token::Word("время".to_string()) },
            PositionalToken { offset: 536, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 537, length: 6, token: Token::Word("его".to_string()) },
            PositionalToken { offset: 543, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 544, length: 16, token: Token::Word("отмечают".to_string()) },
            PositionalToken { offset: 560, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 561, length: 10, token: Token::Word("почти".to_string()) },
            PositionalToken { offset: 571, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 572, length: 2, token: Token::Word("в".to_string()) },
            PositionalToken { offset: 574, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 575, length: 12, token: Token::Word("каждой".to_string()) },
            PositionalToken { offset: 587, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 588, length: 12, token: Token::Word("стране".to_string()) },
            PositionalToken { offset: 600, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 601, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 602, length: 12, token: Token::Word("просто".to_string()) },
            PositionalToken { offset: 614, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 615, length: 10, token: Token::Word("везде".to_string()) },
            PositionalToken { offset: 625, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 626, length: 12, token: Token::Word("разные".to_string()) },
            PositionalToken { offset: 638, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 639, length: 8, token: Token::Word("даты".to_string()) },
            PositionalToken { offset: 647, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 648, length: 2, token: Token::Word("и".to_string()) },
            PositionalToken { offset: 650, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 651, length: 14, token: Token::Word("способы".to_string()) },
            PositionalToken { offset: 665, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 666, length: 24, token: Token::Word("празднования".to_string()) },
            PositionalToken { offset: 690, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 691, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 794, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 795, length: 2, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 870, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 871, length: 2, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 910, length: 2, token: Token::Word("П".to_string()) },
            PositionalToken { offset: 919, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 927, length: 12, token: Token::Word("ПОЧЕМУ".to_string()) },
            PositionalToken { offset: 939, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 940, length: 4, token: Token::Word("МЫ".to_string()) },
            PositionalToken { offset: 944, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 945, length: 6, token: Token::Word("ЕГО".to_string()) },
            PositionalToken { offset: 951, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 952, length: 18, token: Token::Word("ПРАЗДНУЕМ".to_string()) },
            PositionalToken { offset: 1063, length: 2, token: Token::Word("В".to_string()) },
            PositionalToken { offset: 1065, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1066, length: 4, token: Token::Number(Number::Integer(1987)) },
            PositionalToken { offset: 1070, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1071, length: 8, token: Token::Word("году".to_string()) },
            PositionalToken { offset: 1079, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1080, length: 14, token: Token::Word("комитет".to_string()) },
            PositionalToken { offset: 1094, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1095, length: 14, token: Token::Word("госдумы".to_string()) },
            PositionalToken { offset: 1109, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1110, length: 4, token: Token::Word("по".to_string()) },
            PositionalToken { offset: 1114, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1115, length: 10, token: Token::Word("делам".to_string()) },
            PositionalToken { offset: 1125, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1126, length: 12, token: Token::Word("женщин".to_string()) },
            PositionalToken { offset: 1138, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 1139, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1140, length: 10, token: Token::Word("семьи".to_string()) },
            PositionalToken { offset: 1150, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1151, length: 2, token: Token::Word("и".to_string()) },
            PositionalToken { offset: 1153, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1154, length: 16, token: Token::Word("молодежи".to_string()) },
            PositionalToken { offset: 1170, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1171, length: 16, token: Token::Word("выступил".to_string()) },
            PositionalToken { offset: 1187, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1188, length: 2, token: Token::Word("с".to_string()) },
            PositionalToken { offset: 1190, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1191, length: 24, token: Token::Word("предложением".to_string()) },
            PositionalToken { offset: 1215, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1216, length: 16, token: Token::Word("учредить".to_string()) },
            PositionalToken { offset: 1232, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1233, length: 2, token: Token::Unicode("uab".to_string()) },
            PositionalToken { offset: 1235, length: 8, token: Token::Word("День".to_string()) },
            PositionalToken { offset: 1243, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1244, length: 8, token: Token::Word("мамы".to_string()) },
            PositionalToken { offset: 1252, length: 2, token: Token::Unicode("ubb".to_string()) },
            PositionalToken { offset: 1254, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 1255, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1256, length: 2, token: Token::Word("а".to_string()) },
            PositionalToken { offset: 1258, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1259, length: 6, token: Token::Word("сам".to_string()) },
            PositionalToken { offset: 1265, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1266, length: 12, token: Token::Word("приказ".to_string()) },
            PositionalToken { offset: 1278, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1279, length: 6, token: Token::Word("был".to_string()) },
            PositionalToken { offset: 1285, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1286, length: 16, token: Token::Word("подписан".to_string()) },
            PositionalToken { offset: 1302, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1303, length: 6, token: Token::Word("уже".to_string()) },
            PositionalToken { offset: 1309, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1310, length: 2, token: Token::Number(Number::Integer(30)) },
            PositionalToken { offset: 1312, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1313, length: 12, token: Token::Word("января".to_string()) },
            PositionalToken { offset: 1325, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1326, length: 4, token: Token::Number(Number::Integer(1988)) },
            PositionalToken { offset: 1330, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1331, length: 8, token: Token::Word("года".to_string()) },
            PositionalToken { offset: 1339, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1340, length: 14, token: Token::Word("Борисом".to_string()) },
            PositionalToken { offset: 1354, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1355, length: 16, token: Token::Word("Ельциным".to_string()) },
            PositionalToken { offset: 1371, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 1372, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1373, length: 8, token: Token::Word("Было".to_string()) },
            PositionalToken { offset: 1381, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1382, length: 12, token: Token::Word("решено".to_string()) },
            PositionalToken { offset: 1394, length: 1, token: Token::Punctuation(",".to_string()) },
            PositionalToken { offset: 1395, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1396, length: 6, token: Token::Word("что".to_string()) },
            PositionalToken { offset: 1402, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1403, length: 16, token: Token::Word("ежегодно".to_string()) },
            PositionalToken { offset: 1419, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1420, length: 2, token: Token::Word("в".to_string()) },
            PositionalToken { offset: 1422, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1423, length: 12, token: Token::Word("России".to_string()) },
            PositionalToken { offset: 1435, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1436, length: 22, token: Token::Word("празднество".to_string()) },
            PositionalToken { offset: 1458, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1459, length: 6, token: Token::Word("дня".to_string()) },
            PositionalToken { offset: 1465, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1466, length: 8, token: Token::Word("мамы".to_string()) },
            PositionalToken { offset: 1474, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1475, length: 10, token: Token::Word("будет".to_string()) },
            PositionalToken { offset: 1485, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1486, length: 16, token: Token::Word("выпадать".to_string()) },
            PositionalToken { offset: 1502, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1503, length: 4, token: Token::Word("на".to_string()) },
            PositionalToken { offset: 1507, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1508, length: 18, token: Token::Word("последнее".to_string()) },
            PositionalToken { offset: 1526, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1527, length: 22, token: Token::Word("воскресенье".to_string()) },
            PositionalToken { offset: 1549, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1550, length: 12, token: Token::Word("ноября".to_string()) },
            PositionalToken { offset: 1562, length: 1, token: Token::Punctuation(".".to_string()) },
            PositionalToken { offset: 1563, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1664, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 1665, length: 2, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 1725, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 1726, length: 4, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 2725, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 2726, length: 2, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 2888, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 2889, length: 2, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 2891, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 2904, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 2905, length: 4, token: Token::Separator(Separator::Space) },
            ];
        match uws.into_tokens() {
            Err(Untokenizable::Html) => {},
            _ => panic!("Untokenizable::Html"),
        }
        //let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        //check_results(&result,&lib_res,uws);
        //print_result(&lib_res); panic!("")
    }

    #[test]
    fn vk_bbcode() {
        let uws = "[club113623432|💜💜💜 - для девушек] \n[club113623432|💛💛💛 - для сохраненок]";
        let result = vec![
            PositionalToken { offset: 0, length: 52, token: Token::BBCode {
                text: vec![
                    Token::Emoji("purple_heart".to_string()),
                    Token::Emoji("purple_heart".to_string()),
                    Token::Emoji("purple_heart".to_string()),
                    Token::Separator(Separator::Space),
                    Token::Punctuation("-".to_string()),
                    Token::Separator(Separator::Space),
                    Token::Word("для".to_string()),
                    Token::Separator(Separator::Space),
                    Token::Word("девушек".to_string())],
                data: vec![Token::Numerical(Numerical::Alphanumeric("club113623432".to_string()))] } },
            PositionalToken { offset: 52, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 53, length: 1, token: Token::Separator(Separator::Newline) },
            PositionalToken { offset: 54, length: 58, token: Token::BBCode {
                text: vec![
                    Token::Emoji("yellow_heart".to_string()),
                    Token::Emoji("yellow_heart".to_string()),
                    Token::Emoji("yellow_heart".to_string()),
                    Token::Separator(Separator::Space),
                    Token::Punctuation("-".to_string()),
                    Token::Separator(Separator::Space),
                    Token::Word("для".to_string()),
                    Token::Separator(Separator::Space),
                    Token::Word("сохраненок".to_string())],
                data: vec![Token::Numerical(Numerical::Alphanumeric("club113623432".to_string()))] } },
            ];
        let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        check_results(&result,&lib_res,uws);
        //print_result(&lib_res); panic!("")
    }

    /*#[test]
    fn text_href_and_html () {
        let uws = "https://youtu.be/dQErLQZw3qA</a></p><figure data-type=\"102\" data-mode=\"\"  class=\"article_decoration_first article_decoration_last\" >\n";
        let result =  vec![
            PositionalToken { offset: 0, length: 28, token: Token::Url("https://youtu.be/dQErLQZw3qA".to_string()) },
            PositionalToken { offset: 132, length: 1, token: Token::Separator(Separator::Newline) },
            ];
        let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        check_results(&result,&lib_res,uws);
        //print_result(&lib_res); panic!("")
    }*/

    #[test]
    fn numerical() {
        let uws = "12.02.18 31.28.34 23.11.2018 123.568.365.234.578 127.0.0.1 1st 1кг 123123афываыв 12321фвафыов234выалфо 12_123_343.4234_4234";
        let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        //print_result(&lib_res); panic!("");
        let result = vec![
            PositionalToken { offset: 0, length: 8, token: Token::Numerical(Numerical::DotSeparated("12.02.18".to_string())) },
            PositionalToken { offset: 8, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 9, length: 8, token: Token::Numerical(Numerical::DotSeparated("31.28.34".to_string())) },
            PositionalToken { offset: 17, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 18, length: 10, token: Token::Numerical(Numerical::DotSeparated("23.11.2018".to_string())) },
            PositionalToken { offset: 28, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 29, length: 19, token: Token::Numerical(Numerical::DotSeparated("123.568.365.234.578".to_string())) },
            PositionalToken { offset: 48, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 49, length: 9, token: Token::Numerical(Numerical::DotSeparated("127.0.0.1".to_string())) },
            PositionalToken { offset: 58, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 59, length: 3, token: Token::Numerical(Numerical::Measures("1st".to_string())) },
            PositionalToken { offset: 62, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 63, length: 5, token: Token::Numerical(Numerical::Measures("1кг".to_string())) },
            PositionalToken { offset: 68, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 69, length: 20, token: Token::Numerical(Numerical::Measures("123123афываыв".to_string())) },
            PositionalToken { offset: 89, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 90, length: 34, token: Token::Numerical(Numerical::Alphanumeric("12321фвафыов234выалфо".to_string())) },
            PositionalToken { offset: 124, length: 1, token: Token::Separator(Separator::Space) },
            PositionalToken { offset: 125, length: 20, token: Token::Numerical(Numerical::Alphanumeric("12_123_343.4234_4234".to_string())) },
            ];
        check_results(&result,&lib_res,uws);
       
    }
    
    /*#[test]
    fn new_test() {
        let uws = "";
        let lib_res = uws.into_tokens().unwrap().collect::<Vec<_>>();
        print_result(&lib_res); panic!("");
        let result = vec![];
        check_results(&result,&lib_res,uws);
        
}*/
}
 
