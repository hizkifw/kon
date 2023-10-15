use std::{borrow::BorrowMut, iter::Peekable, slice::Iter};

type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    Ident(Identifier),
    Keyword(KeywordType),
    Number(i64),
    Whitespace(char),
    Lparen,
    Rparen,
    Lbracket,
    Rbracket,
    Lbrace,
    Rbrace,
    Comma,
    Period,
    Semi,
    Unknown(char),
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = match &self {
            TokenType::Ident(s) => s.to_string(),
            TokenType::Keyword(kt) => match kt {
                KeywordType::Fun => "fun".to_string(),
                KeywordType::Return => "return".to_string(),
            },
            TokenType::Number(n) => n.to_string(),
            TokenType::Whitespace(s) => s.to_string(),
            TokenType::Lparen => "(".to_string(),
            TokenType::Rparen => ")".to_string(),
            TokenType::Lbracket => "[".to_string(),
            TokenType::Rbracket => "]".to_string(),
            TokenType::Lbrace => "{".to_string(),
            TokenType::Rbrace => "}".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Period => ".".to_string(),
            TokenType::Semi => ";".to_string(),
            TokenType::Unknown(ch) => ch.to_string(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum KeywordType {
    Fun,
    Return,
}

#[derive(Debug, Clone, PartialEq)]
struct Loc {
    pub file: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.col)
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    pub typ: TokenType,
    pub loc: Loc,
}

#[derive(Debug, Clone, PartialEq)]
struct Program {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
struct Function {
    pub name: String,
    pub args: Vec<(Vec<Token>, Identifier)>,
    pub rets: Vec<(Vec<Token>, Identifier)>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq)]
struct Block {
    functions: Vec<Function>,
    body: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Return,
    IntLiteral(i64),
}

fn compiler_error(loc: &Loc, message: &str) {
    println!("{}: {}", loc, message);
}

fn compiler_fatal(loc: &Loc, message: &str) -> ! {
    compiler_error(loc, message);
    std::process::exit(1);
}

fn parse_argslist(tokens: &mut Peekable<Iter<'_, Token>>) -> Vec<(Vec<Token>, String)> {
    let mut args = Vec::new();
    let mut tokens = tokens.take_while(|t| t.typ != TokenType::Rparen).peekable();

    while tokens.peek().is_some() {
        let loc = {
            let tok = tokens.peek().unwrap();
            if tok.typ == TokenType::Rparen {
                tokens.next();
                break;
            }
            tok.loc.clone()
        };

        let mut part = tokens
            .borrow_mut()
            .take_while(|t| t.typ != TokenType::Comma && t.typ != TokenType::Rparen)
            .map(|t| t.clone())
            .collect::<Vec<_>>();

        let tok_ident = match part.pop() {
            Some(tok) => tok,
            _ => compiler_fatal(&loc, "Expected identifier but found nothing"),
        };
        let ident = match tok_ident.typ {
            TokenType::Ident(i) => i,
            _ => compiler_fatal(
                &tok_ident.loc,
                &format!("Expected identifier but found {}", tok_ident.typ),
            ),
        };

        args.push((part, ident));
    }

    args
}

fn parse_block(depth: usize, tokens: &mut Peekable<Iter<'_, Token>>) -> Block {
    let mut block = Block {
        functions: Vec::new(),
        body: Vec::new(),
    };

    while let Some(token) = tokens.next() {
        match &token.typ {
            TokenType::Ident(_) => todo!(),
            TokenType::Keyword(kw) => match kw {
                KeywordType::Fun => {
                    // Get the function name
                    let name = {
                        if let Some(tok) = tokens.next() {
                            if let TokenType::Ident(name) = &tok.typ {
                                name.to_owned()
                            } else {
                                compiler_fatal(
                                    &tok.loc,
                                    &format!("Expected identifier but found {:?}", tok.typ),
                                )
                            }
                        } else {
                            compiler_fatal(&token.loc, "Expected function name but found nothing");
                        }
                    };

                    let lparen = tokens.next().expect("EOF");
                    if lparen.typ != TokenType::Lparen {
                        compiler_fatal(&lparen.loc, "Expected `(`");
                    };

                    let args = parse_argslist(tokens);

                    let tok = match tokens.next() {
                        Some(tok) => tok,
                        _ => compiler_fatal(
                            &token.loc,
                            "Expected return type or block start but found EOF",
                        ),
                    };

                    let rets = match tok.typ {
                        TokenType::Lparen => parse_argslist(tokens),
                        _ => Vec::new(),
                    };

                    match tokens.next() {
                        Some(tok) => match tok.typ {
                            TokenType::Lbrace => (),
                            _ => compiler_fatal(
                                &tok.loc,
                                &format!("Expected `{{` but found {}", tok.typ),
                            ),
                        },
                        _ => compiler_fatal(&tok.loc, "Expected `{` but found EOF"),
                    };

                    let fun = Function {
                        name,
                        args,
                        rets,
                        block: parse_block(depth + 1, tokens),
                    };
                    block.functions.push(fun);
                }
                KeywordType::Return => block.body.push(Expr::Return),
            },
            TokenType::Number(n) => block.body.push(Expr::IntLiteral(*n)),
            TokenType::Whitespace(_) => todo!(),
            TokenType::Lparen => todo!(),
            TokenType::Rparen => todo!(),
            TokenType::Lbracket => todo!(),
            TokenType::Rbracket => todo!(),
            TokenType::Lbrace => {
                // Start of a new block
            }
            TokenType::Rbrace => {
                // End of current block
                break;
            }
            TokenType::Comma => todo!(),
            TokenType::Period => todo!(),
            TokenType::Semi => todo!(),
            TokenType::Unknown(_) => todo!(),
        }
    }

    block
}

fn tokenize_word(word: &str) -> TokenType {
    match word {
        "fun" => TokenType::Keyword(KeywordType::Fun),
        "return" => TokenType::Keyword(KeywordType::Return),
        _ => TokenType::Ident(word.to_string()),
    }
}

fn tokenize_line(loc: Loc, line: &str) -> Vec<Token> {
    let line = line.split_once("//").unwrap_or((line, "")).0;

    let mut tokens = Vec::new();

    let mut linechars = line.chars().enumerate().peekable();
    while let Some((i, c)) = linechars.next() {
        let mut loc = loc.clone();
        loc.col = i + 1;

        let token_type = match c {
            '(' => TokenType::Lparen,
            ')' => TokenType::Rparen,
            '[' => TokenType::Lbracket,
            ']' => TokenType::Rbracket,
            '{' => TokenType::Lbrace,
            '}' => TokenType::Rbrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Period,
            ';' => TokenType::Semi,
            '0'..='9' => {
                let mut number: i64 = c.to_digit(10).unwrap().into();
                while let Some((_, nc)) = linechars.next_if(|(_, nc)| nc.is_ascii_digit()) {
                    let digit: i64 = nc.to_digit(10).unwrap().into();
                    number = number * 10 + digit;
                }
                TokenType::Number(number)
            }
            ' ' | '\t' | '\n' => TokenType::Whitespace(c),
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut str_end = i;
                while let Some((ni, _)) =
                    linechars.next_if(|(_, nc)| nc.is_ascii_alphanumeric() || *nc == '_')
                {
                    str_end = ni + 1;
                }
                tokenize_word(&line[i..str_end])
            }
            chr => {
                compiler_error(&loc, &format!("Unknown character `{}`", chr));
                TokenType::Unknown(chr)
            }
        };

        match token_type {
            TokenType::Whitespace(_) => (),
            _ => tokens.push(Token {
                typ: token_type,
                loc: loc.clone(),
            }),
        };
    }

    tokens
}

fn tokenize_file(filename: String) -> Vec<Token> {
    let mut toks = Vec::new();

    for (nr, line) in std::fs::read_to_string(filename.clone())
        .expect("error opening file for reading")
        .lines()
        .enumerate()
    {
        let line_loc = Loc {
            file: filename.clone(),
            line: nr + 1,
            col: 0,
        };
        let line_toks = tokenize_line(line_loc, line);
        toks.extend(line_toks);
    }

    toks
}

fn main() {
    let arg: Vec<String> = std::env::args().collect();

    if arg.len() < 2 {
        println!("usage: {} <input>", arg[0]);
        std::process::exit(1);
    }

    let input_file = arg[1].to_string();
    let toks = tokenize_file(input_file);
    for tok in &toks {
        println!("{} -> {:?}", tok.loc, tok.typ);
    }
    println!("\n---\nProgram:\n");
    let mut tok_iter = toks.iter().peekable();
    let prog = parse_block(0, &mut tok_iter);
    println!("{:?}", prog);
}
