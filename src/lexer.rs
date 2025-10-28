use std::iter::Peekable;
use std::str::CharIndices;

use crate::ast::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub lexeme: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier,
    NumberLiteral(f64),
    StringLiteral,
    Keyword(Keyword),
    Punctuator(Punctuator),
    Operator(Operator),
    JsxText,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Import,
    From,
    As,
    Let,
    Const,
    Var,
    Function,
    Return,
    If,
    Else,
    Export,
    Default,
    Async,
    Await,
    True,
    False,
    Null,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punctuator {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Semi,
    Dot,
    Question,
    Spread,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Bang,
    EqEq,
    NotEq,
    StrictEq,
    StrictNotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    AndAnd,
    Or,
    OrOr,
    NullishCoalescing,
    FatArrow,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug, Clone)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

impl LexError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LastTokenKind {
    Identifier,
    Number,
    String,
    Boolean,
    Null,
    RParen,
    RBracket,
    RBrace,
    JsxText,
    Other,
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    current: usize,
    tokens: Vec<Token>,
    jsx_depth: usize,
    inside_jsx_tag: bool,
    last_significant: Option<LastTokenKind>,
    pending_jsx_closing: usize,
    jsx_expression_depth: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            current: 0,
            tokens: Vec::new(),
            jsx_depth: 0,
            inside_jsx_tag: false,
            last_significant: None,
            pending_jsx_closing: 0,
            jsx_expression_depth: 0,
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, LexError> {
        while self.peek_char().is_some() {
            if self.jsx_depth > 0 && !self.inside_jsx_tag && self.jsx_expression_depth == 0 {
                if let Some(token) = self.read_jsx_text()? {
                    self.push_token(token);
                    continue;
                }
            }

            match self.peek_char().unwrap().1 {
                ch if ch.is_whitespace() => {
                    self.consume_whitespace();
                }
                '/' => {
                    if self.consume_comment()? {
                        continue;
                    }
                    let token = self.simple_token(Operator::Div, "/");
                    self.push_token(token);
                }
                ch if is_identifier_start(ch) => {
                    let token = self.consume_identifier();
                    self.push_token(token);
                }
                ch if ch.is_ascii_digit() => {
                    let token = self.consume_number()?;
                    self.push_token(token);
                }
                '"' | '\'' | '`' => {
                    let token = self.consume_string()?;
                    self.push_token(token);
                }
                '.' => {
                    let token = self.consume_dot_or_spread()?;
                    self.push_token(token);
                }
                '=' => {
                    let token = self.consume_equals()?;
                    self.push_token(token);
                }
                '!' => {
                    let token = self.consume_bang()?;
                    self.push_token(token);
                }
                '&' => {
                    let token = self.consume_ampersand()?;
                    self.push_token(token);
                }
                '|' => {
                    let token = self.consume_pipe()?;
                    self.push_token(token);
                }
                '+' => {
                    let token = self.consume_plus()?;
                    self.push_token(token);
                }
                '-' => {
                    let token = self.consume_minus()?;
                    self.push_token(token);
                }
                '*' => {
                    self.advance_char();
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Operator(Operator::Mul),
                        span,
                        lexeme: "*".into(),
                    };
                    self.push_token(token);
                }
                '%' => {
                    self.advance_char();
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Operator(Operator::Mod),
                        span,
                        lexeme: "%".into(),
                    };
                    self.push_token(token);
                }
                '<' => {
                    let token = self.consume_lt_or_jsx()?;
                    self.push_token(token);
                }
                '>' => {
                    let token = self.consume_gt()?;
                    self.push_token(token);
                }
                '?' => {
                    let token = self.consume_question()?;
                    self.push_token(token);
                }
                ':' => {
                    self.advance_char();
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Punctuator(Punctuator::Colon),
                        span,
                        lexeme: ":".into(),
                    };
                    self.push_token(token);
                }
                ';' => {
                    self.advance_char();
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Punctuator(Punctuator::Semi),
                        span,
                        lexeme: ";".into(),
                    };
                    self.push_token(token);
                }
                ',' => {
                    self.advance_char();
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Punctuator(Punctuator::Comma),
                        span,
                        lexeme: ",".into(),
                    };
                    self.push_token(token);
                }
                '(' => {
                    self.advance_char();
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Punctuator(Punctuator::LParen),
                        span,
                        lexeme: "(".into(),
                    };
                    self.push_token(token);
                }
                ')' => {
                    self.advance_char();
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Punctuator(Punctuator::RParen),
                        span,
                        lexeme: ")".into(),
                    };
                    self.push_token(token);
                }
                '{' => {
                    self.advance_char();
                    if self.jsx_depth > 0 && !self.inside_jsx_tag {
                        self.jsx_expression_depth += 1;
                    }
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Punctuator(Punctuator::LBrace),
                        span,
                        lexeme: "{".into(),
                    };
                    self.push_token(token);
                }
                '}' => {
                    self.advance_char();
                    if self.jsx_depth > 0 && self.jsx_expression_depth > 0 {
                        self.jsx_expression_depth -= 1;
                    }
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Punctuator(Punctuator::RBrace),
                        span,
                        lexeme: "}".into(),
                    };
                    self.push_token(token);
                }
                '[' => {
                    self.advance_char();
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Punctuator(Punctuator::LBracket),
                        span,
                        lexeme: "[".into(),
                    };
                    self.push_token(token);
                }
                ']' => {
                    self.advance_char();
                    let span = self.token_span(self.current - 1, self.current);
                    let token = Token {
                        kind: TokenKind::Punctuator(Punctuator::RBracket),
                        span,
                        lexeme: "]".into(),
                    };
                    self.push_token(token);
                }
                _ => {
                    let (idx, ch) = self.advance_char().unwrap();
                    let span = self.token_span(idx, self.current);
                    return Err(LexError::new(
                        format!("Unexpected character '{}'", ch),
                        span,
                    ));
                }
            }
        }

        let eof = Token {
            kind: TokenKind::Eof,
            span: Span::new(self.current, self.current),
            lexeme: String::new(),
        };
        self.tokens.push(eof);
        Ok(self.tokens)
    }

    fn push_token(&mut self, token: Token) {
        let last_kind = match &token.kind {
            TokenKind::Identifier => Some(LastTokenKind::Identifier),
            TokenKind::NumberLiteral(_) => Some(LastTokenKind::Number),
            TokenKind::StringLiteral => Some(LastTokenKind::String),
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::True | Keyword::False => Some(LastTokenKind::Boolean),
                Keyword::Null => Some(LastTokenKind::Null),
                _ => Some(LastTokenKind::Other),
            },
            TokenKind::Punctuator(punct) => match punct {
                Punctuator::RParen => Some(LastTokenKind::RParen),
                Punctuator::RBracket => Some(LastTokenKind::RBracket),
                Punctuator::RBrace => Some(LastTokenKind::RBrace),
                _ => Some(LastTokenKind::Other),
            },
            TokenKind::JsxText => Some(LastTokenKind::JsxText),
            TokenKind::Operator(_) => Some(LastTokenKind::Other),
            TokenKind::Eof => None,
        };
        if !matches!(token.kind, TokenKind::Eof) {
            self.last_significant = last_kind;
        }
        self.tokens.push(token);
    }

    fn peek_char(&mut self) -> Option<(usize, char)> {
        self.chars.peek().copied()
    }

    fn advance_char(&mut self) -> Option<(usize, char)> {
        if let Some((idx, ch)) = self.chars.next() {
            self.current = idx + ch.len_utf8();
            Some((idx, ch))
        } else {
            None
        }
    }

    fn peek_offset(&self, offset: usize) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.nth(offset).map(|(_, ch)| ch)
    }

    fn token_span(&self, start: usize, end: usize) -> Span {
        Span::new(start, end)
    }

    fn consume_whitespace(&mut self) {
        while let Some((_, ch)) = self.peek_char() {
            if ch.is_whitespace() {
                self.advance_char();
            } else {
                break;
            }
        }
    }

    fn consume_comment(&mut self) -> Result<bool, LexError> {
        if self.peek_offset(1) == Some('/') {
            // line comment
            self.advance_char(); // '/'
            self.advance_char(); // second '/'
            while let Some((_, ch)) = self.peek_char() {
                if ch == '\n' {
                    break;
                }
                self.advance_char();
            }
            Ok(true)
        } else if self.peek_offset(1) == Some('*') {
            self.advance_char();
            self.advance_char();
            let start = self.current - 2;
            while let Some((_, ch)) = self.advance_char() {
                if ch == '*' && self.peek_offset(0) == Some('/') {
                    self.advance_char();
                    return Ok(true);
                }
            }
            Err(LexError::new(
                "Unterminated block comment",
                Span::new(start, self.current),
            ))
        } else {
            Ok(false)
        }
    }

    fn consume_identifier(&mut self) -> Token {
        let (start, _) = self.peek_char().unwrap();
        self.advance_char();
        while let Some((_, ch)) = self.peek_char() {
            if is_identifier_part(ch) {
                self.advance_char();
            } else {
                break;
            }
        }
        let end = self.current;
        let lexeme = &self.source[start..end];
        let kind = match lexeme {
            "import" => TokenKind::Keyword(Keyword::Import),
            "from" => TokenKind::Keyword(Keyword::From),
            "as" => TokenKind::Keyword(Keyword::As),
            "let" => TokenKind::Keyword(Keyword::Let),
            "const" => TokenKind::Keyword(Keyword::Const),
            "var" => TokenKind::Keyword(Keyword::Var),
            "function" => TokenKind::Keyword(Keyword::Function),
            "return" => TokenKind::Keyword(Keyword::Return),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "export" => TokenKind::Keyword(Keyword::Export),
            "default" => TokenKind::Keyword(Keyword::Default),
            "async" => TokenKind::Keyword(Keyword::Async),
            "await" => TokenKind::Keyword(Keyword::Await),
            "true" => TokenKind::Keyword(Keyword::True),
            "false" => TokenKind::Keyword(Keyword::False),
            "null" => TokenKind::Keyword(Keyword::Null),
            _ => TokenKind::Identifier,
        };
        Token {
            kind,
            span: Span::new(start, end),
            lexeme: lexeme.to_string(),
        }
    }

    fn consume_number(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.peek_char().unwrap();
        self.advance_char();
        let mut has_dot = false;
        while let Some((_, ch)) = self.peek_char() {
            if ch.is_ascii_digit() {
                self.advance_char();
            } else if ch == '.' && !has_dot {
                has_dot = true;
                self.advance_char();
            } else {
                break;
            }
        }
        let end = self.current;
        let lexeme = &self.source[start..end];
        let value = lexeme.parse::<f64>().map_err(|_| {
            LexError::new(
                format!("Invalid number literal '{}'", lexeme),
                Span::new(start, end),
            )
        })?;
        Ok(Token {
            kind: TokenKind::NumberLiteral(value),
            span: Span::new(start, end),
            lexeme: lexeme.to_string(),
        })
    }

    fn consume_string(&mut self) -> Result<Token, LexError> {
        let (start, quote) = self.advance_char().unwrap();
        let mut value = String::new();
        let mut escaped = false;
        while let Some((_, ch)) = self.advance_char() {
            if escaped {
                let decoded = match ch {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '\'' => '\'',
                    '"' => '"',
                    '`' => '`',
                    '0' => '\0',
                    other => other,
                };
                value.push(decoded);
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if ch == quote {
                let span = Span::new(start, self.current);
                return Ok(Token {
                    kind: TokenKind::StringLiteral,
                    span,
                    lexeme: value,
                });
            }
            value.push(ch);
        }
        Err(LexError::new(
            "Unterminated string literal",
            Span::new(start, self.current),
        ))
    }

    fn consume_dot_or_spread(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('.') && self.peek_offset(1) == Some('.') {
            self.advance_char();
            self.advance_char();
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Punctuator(Punctuator::Spread),
                span,
                lexeme: "...".into(),
            })
        } else {
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Punctuator(Punctuator::Dot),
                span,
                lexeme: ".".into(),
            })
        }
    }

    fn consume_equals(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('=') {
            self.advance_char();
            if self.peek_offset(0) == Some('=') {
                self.advance_char();
                let span = Span::new(start, self.current);
                Ok(Token {
                    kind: TokenKind::Operator(Operator::StrictEq),
                    span,
                    lexeme: "===".into(),
                })
            } else {
                let span = Span::new(start, self.current);
                Ok(Token {
                    kind: TokenKind::Operator(Operator::EqEq),
                    span,
                    lexeme: "==".into(),
                })
            }
        } else if self.peek_offset(0) == Some('>') {
            self.advance_char();
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::FatArrow),
                span,
                lexeme: "=>".into(),
            })
        } else {
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::Assign),
                span,
                lexeme: "=".into(),
            })
        }
    }

    fn consume_bang(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('=') {
            self.advance_char();
            if self.peek_offset(0) == Some('=') {
                self.advance_char();
                let span = Span::new(start, self.current);
                Ok(Token {
                    kind: TokenKind::Operator(Operator::StrictNotEq),
                    span,
                    lexeme: "!==".into(),
                })
            } else {
                let span = Span::new(start, self.current);
                Ok(Token {
                    kind: TokenKind::Operator(Operator::NotEq),
                    span,
                    lexeme: "!=".into(),
                })
            }
        } else {
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::Bang),
                span,
                lexeme: "!".into(),
            })
        }
    }

    fn consume_ampersand(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('&') {
            self.advance_char();
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::AndAnd),
                span,
                lexeme: "&&".into(),
            })
        } else {
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::And),
                span,
                lexeme: "&".into(),
            })
        }
    }

    fn consume_pipe(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('|') {
            self.advance_char();
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::OrOr),
                span,
                lexeme: "||".into(),
            })
        } else {
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::Or),
                span,
                lexeme: "|".into(),
            })
        }
    }

    fn consume_plus(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('=') {
            self.advance_char();
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::AddAssign),
                span,
                lexeme: "+=".into(),
            })
        } else {
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::Add),
                span,
                lexeme: "+".into(),
            })
        }
    }

    fn consume_minus(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('=') {
            self.advance_char();
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::SubAssign),
                span,
                lexeme: "-=".into(),
            })
        } else {
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::Sub),
                span,
                lexeme: "-".into(),
            })
        }
    }

    fn consume_question(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('?') {
            self.advance_char();
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Operator(Operator::NullishCoalescing),
                span,
                lexeme: "??".into(),
            })
        } else {
            let span = Span::new(start, self.current);
            Ok(Token {
                kind: TokenKind::Punctuator(Punctuator::Question),
                span,
                lexeme: "?".into(),
            })
        }
    }

    fn consume_lt_or_jsx(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('=') {
            self.advance_char();
            let span = Span::new(start, self.current);
            return Ok(Token {
                kind: TokenKind::Operator(Operator::Lte),
                span,
                lexeme: "<=".into(),
            });
        }

        let next_char = self.peek_char().map(|(_, ch)| ch);
        let is_closing_tag = matches!(next_char, Some('/'));
        if self.should_enter_jsx() {
            self.inside_jsx_tag = true;
            if is_closing_tag {
                if self.jsx_depth > 0 {
                    self.pending_jsx_closing += 1;
                }
            } else {
                self.jsx_depth += 1;
            }
        }

        let span = Span::new(start, self.current);
        Ok(Token {
            kind: TokenKind::Operator(Operator::Lt),
            span,
            lexeme: "<".into(),
        })
    }

    fn consume_gt(&mut self) -> Result<Token, LexError> {
        let (start, _) = self.advance_char().unwrap();
        if self.peek_offset(0) == Some('=') {
            self.advance_char();
            let span = Span::new(start, self.current);
            return Ok(Token {
                kind: TokenKind::Operator(Operator::Gte),
                span,
                lexeme: ">=".into(),
            });
        }

        if self.inside_jsx_tag {
            self.inside_jsx_tag = false;
        }

        if self.jsx_depth > 0 {
            let prev_char = self
                .source
                .get(..start)
                .and_then(|prefix| prefix.chars().last())
                .unwrap_or('\0');
            let is_self_closing = prev_char == '/';
            if is_self_closing && self.jsx_depth > 0 {
                self.jsx_depth -= 1;
            } else if self.pending_jsx_closing > 0 {
                self.pending_jsx_closing -= 1;
                if self.jsx_depth > 0 {
                    self.jsx_depth -= 1;
                }
            }
            if self.jsx_depth == 0 {
                self.inside_jsx_tag = false;
            }
        }

        let span = Span::new(start, self.current);
        Ok(Token {
            kind: TokenKind::Operator(Operator::Gt),
            span,
            lexeme: ">".into(),
        })
    }

    fn read_jsx_text(&mut self) -> Result<Option<Token>, LexError> {
        let start = self.current;
        let mut content = String::new();
        while let Some((_, ch)) = self.peek_char() {
            if ch == '<' || ch == '{' {
                break;
            }
            self.advance_char();
            content.push(ch);
        }
        if content.is_empty() {
            return Ok(None);
        }
        let span = Span::new(start, self.current);
        if content.trim().is_empty() {
            return Ok(None);
        }
        Ok(Some(Token {
            kind: TokenKind::JsxText,
            span,
            lexeme: content,
        }))
    }

    fn should_enter_jsx(&mut self) -> bool {
        let lookahead = self.peek_char();
        let next_ch = lookahead.map(|(_, ch)| ch);

        if let Some(ch) = next_ch {
            if !(is_identifier_start(ch) || ch == '/' || ch == '>') {
                return false;
            }
        }

        match (self.last_significant, next_ch) {
            (_, Some('/')) => true,
            (None, _) => true,
            (Some(LastTokenKind::Other), _) => true,
            (Some(LastTokenKind::RBrace), _) => true,
            (Some(LastTokenKind::RBracket), _) => true,
            (Some(LastTokenKind::RParen), _) => true,
            _ => false,
        }
    }

    fn simple_token(&mut self, op: Operator, lexeme: &str) -> Token {
        let (start, _) = self.advance_char().unwrap();
        let span = Span::new(start, self.current);
        Token {
            kind: TokenKind::Operator(op),
            span,
            lexeme: lexeme.into(),
        }
    }
}

fn is_identifier_start(ch: char) -> bool {
    ch == '$' || ch == '_' || ch.is_ascii_alphabetic()
}

fn is_identifier_part(ch: char) -> bool {
    is_identifier_start(ch) || ch.is_ascii_digit()
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, LexError> {
    Lexer::new(source).tokenize()
}
