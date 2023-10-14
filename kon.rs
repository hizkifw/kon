#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    Ident(String),
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

#[derive(Debug, Clone, PartialEq)]
struct Token {
    pub typ: TokenType,
    pub loc: Loc,
}

fn compiler_error(loc: &Loc, message: String) {
    println!("{}:{}:{}: {}", loc.file, loc.line, loc.col, message);
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
                compiler_error(&loc, format!("Unknown character `{}`", chr));
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

    if arg.len() < 3 {
        println!("usage: {} <input> <output>", arg[0]);
        std::process::exit(1);
    }

    let input_file = arg[1].to_string();
    let toks = tokenize_file(input_file);
    println!("Tokens: {:?}", toks);
}
