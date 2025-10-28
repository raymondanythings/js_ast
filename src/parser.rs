use std::fmt;

use crate::ast::*;
use crate::lexer::{self, Keyword, Operator, Punctuator, Token, TokenKind};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.message, self.span)
    }
}

impl std::error::Error for ParseError {}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

pub fn parse_program(source: &str) -> Result<Program, ParseError> {
    let mut parser = Parser::new(source)?;
    parser.parse_program()
}

impl Parser {
    pub fn new(source: &str) -> Result<Self, ParseError> {
        let tokens =
            lexer::tokenize(source).map_err(|err| ParseError::new(err.message, err.span))?;
        Ok(Self { tokens, pos: 0 })
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut body = Vec::new();
        while !self.is_at_end() {
            if self.match_punctuator(Punctuator::Semi) {
                continue;
            }
            let item = self.parse_module_item()?;
            body.push(item);
        }
        Ok(Program { body })
    }

    fn parse_module_item(&mut self) -> Result<ModuleItem, ParseError> {
        if self.check_keyword(Keyword::Import) {
            let decl = self.parse_import_declaration()?;
            Ok(ModuleItem::Import(decl))
        } else if self.check_keyword(Keyword::Export) {
            let decl = self.parse_export_declaration()?;
            Ok(ModuleItem::Export(decl))
        } else {
            let stmt = self.parse_statement()?;
            Ok(ModuleItem::Statement(stmt))
        }
    }

    fn parse_import_declaration(&mut self) -> Result<ImportDeclaration, ParseError> {
        let import_token = self.expect_keyword(Keyword::Import)?;
        let mut specifiers = Vec::new();

        if self.check_kind(&TokenKind::StringLiteral) {
            let source_token = self.advance().clone();
            let span = Span::new(import_token.span.start, source_token.span.end);
            let source = StringLiteral {
                span: source_token.span,
                value: source_token.lexeme.clone(),
            };
            self.eat_semicolon();
            return Ok(ImportDeclaration {
                span,
                specifiers,
                source,
            });
        }

        if self.check_kind(&TokenKind::Identifier) {
            let ident = self.parse_identifier()?;
            let span = ident.span;
            specifiers.push(ImportSpecifier::Default { local: ident, span });
            if self.match_punctuator(Punctuator::Comma) {
                specifiers.extend(self.parse_named_import_specifiers()?);
            }
        } else if self.match_operator(Operator::Mul) {
            self.expect_keyword(Keyword::As)?;
            let name = self.parse_identifier()?;
            let span = Span::new(import_token.span.start, name.span.end);
            specifiers.push(ImportSpecifier::Namespace { local: name, span });
        } else if self.check_punctuator(Punctuator::LBrace) {
            specifiers.extend(self.parse_named_import_specifiers()?);
        }

        self.expect_keyword(Keyword::From)?;
        let source_token = self.expect_string_literal()?;
        let source = StringLiteral {
            span: source_token.span,
            value: source_token.lexeme.clone(),
        };
        let span = Span::new(import_token.span.start, source_token.span.end);
        self.eat_semicolon();
        Ok(ImportDeclaration {
            span,
            specifiers,
            source,
        })
    }

    fn parse_named_import_specifiers(&mut self) -> Result<Vec<ImportSpecifier>, ParseError> {
        let mut specifiers = Vec::new();
        let start_token = self.expect_punctuator(Punctuator::LBrace)?;
        while !self.check_punctuator(Punctuator::RBrace) && !self.is_at_end() {
            let imported_token = self.current().clone();
            let imported = self.parse_identifier()?;
            let mut local = imported.clone();
            if self.match_keyword(Keyword::As) {
                local = self.parse_identifier()?;
            }
            let span = Span::new(imported_token.span.start, local.span.end);
            specifiers.push(ImportSpecifier::Named {
                span,
                local,
                imported: Some(imported),
            });
            if !self.match_punctuator(Punctuator::Comma) {
                break;
            }
        }
        let end_token = self.expect_punctuator(Punctuator::RBrace)?;
        if specifiers.is_empty() && start_token.span.end == end_token.span.start {
            return Err(self.error(end_token.span, "Expected at least one named import"));
        }
        Ok(specifiers)
    }

    fn parse_export_declaration(&mut self) -> Result<ExportDeclaration, ParseError> {
        let export_token = self.expect_keyword(Keyword::Export)?;
        let start = export_token.span.start;

        if self.match_keyword(Keyword::Default) {
            if self.check_keyword(Keyword::Function) || self.check_keyword(Keyword::Async) {
                let decl = self.parse_function_declaration()?;
                let span = Span::new(start, decl.span.end);
                return Ok(ExportDeclaration::Default {
                    span,
                    declaration: Declaration::Function(decl),
                });
            }
            let expression = self.parse_expression()?;
            let span = Span::new(start, expression.span().end);
            self.eat_semicolon();
            return Ok(ExportDeclaration::DefaultExpression { span, expression });
        }

        if self.check_punctuator(Punctuator::LBrace) {
            let specifiers = self.parse_export_specifiers()?;
            let mut source = None;
            if self.match_keyword(Keyword::From) {
                let token = self.expect_string_literal()?;
                source = Some(StringLiteral {
                    span: token.span,
                    value: token.lexeme.clone(),
                });
            }
            self.eat_semicolon();
            let end = source
                .as_ref()
                .map(|lit| lit.span.end)
                .unwrap_or_else(|| self.previous().span.end);
            return Ok(ExportDeclaration::Named {
                span: Span::new(start, end),
                specifiers,
                source,
            });
        }

        if self.match_operator(Operator::Mul) {
            let mut source = None;
            if self.match_keyword(Keyword::From) {
                let token = self.expect_string_literal()?;
                source = Some(StringLiteral {
                    span: token.span,
                    value: token.lexeme.clone(),
                });
            }
            self.eat_semicolon();
            let end = source
                .as_ref()
                .map(|lit| lit.span.end)
                .unwrap_or_else(|| self.previous().span.end);
            return Ok(ExportDeclaration::All {
                span: Span::new(start, end),
                source,
            });
        }

        let declaration = self.parse_declaration()?;
        let end = match &declaration {
            Declaration::Variable(var) => var.span.end,
            Declaration::Function(func) => func.span.end,
        };
        Ok(ExportDeclaration::Declaration {
            span: Span::new(start, end),
            declaration,
        })
    }

    fn parse_export_specifiers(&mut self) -> Result<Vec<ExportSpecifier>, ParseError> {
        self.expect_punctuator(Punctuator::LBrace)?;
        let mut specifiers = Vec::new();
        while !self.check_punctuator(Punctuator::RBrace) && !self.is_at_end() {
            let local_token = self.current().clone();
            let local = self.parse_identifier()?;
            let mut exported = None;
            if self.match_keyword(Keyword::As) {
                exported = Some(self.parse_identifier()?);
            }
            let end = exported
                .as_ref()
                .map(|id| id.span.end)
                .unwrap_or(local.span.end);
            specifiers.push(ExportSpecifier {
                span: Span::new(local_token.span.start, end),
                local,
                exported,
            });
            if !self.match_punctuator(Punctuator::Comma) {
                break;
            }
        }
        self.expect_punctuator(Punctuator::RBrace)?;
        Ok(specifiers)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        if self.check_keyword(Keyword::Return) {
            let stmt = self.parse_return_statement()?;
            return Ok(Statement::Return(stmt));
        }
        if self.check_keyword(Keyword::Const)
            || self.check_keyword(Keyword::Let)
            || self.check_keyword(Keyword::Var)
        {
            let decl = self.parse_variable_declaration()?;
            return Ok(Statement::Declaration(Declaration::Variable(decl)));
        }
        if self.check_keyword(Keyword::Function) || self.check_keyword(Keyword::Async) {
            let decl = self.parse_function_declaration()?;
            return Ok(Statement::Declaration(Declaration::Function(decl)));
        }
        if self.check_punctuator(Punctuator::LBrace) {
            let block = self.parse_block_statement()?;
            return Ok(Statement::Block(block));
        }
        let expr = self.parse_expression()?;
        let span = expr.span();
        self.eat_semicolon();
        Ok(Statement::Expression(ExpressionStatement {
            span,
            expression: expr,
        }))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let start_token = self.expect_punctuator(Punctuator::LBrace)?;
        let mut body = Vec::new();
        while !self.check_punctuator(Punctuator::RBrace) && !self.is_at_end() {
            body.push(self.parse_statement()?);
        }
        let end_token = self.expect_punctuator(Punctuator::RBrace)?;
        Ok(BlockStatement {
            span: Span::new(start_token.span.start, end_token.span.end),
            body,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
        let keyword = self.expect_keyword(Keyword::Return)?;
        if self.check_punctuator(Punctuator::Semi) || self.check_punctuator(Punctuator::RBrace) {
            self.eat_semicolon();
            return Ok(ReturnStatement {
                span: Span::new(keyword.span.start, keyword.span.end),
                argument: None,
            });
        }
        let argument = self.parse_expression()?;
        let end = argument.span().end;
        self.eat_semicolon();
        Ok(ReturnStatement {
            span: Span::new(keyword.span.start, end),
            argument: Some(argument),
        })
    }

    fn parse_declaration(&mut self) -> Result<Declaration, ParseError> {
        if self.check_keyword(Keyword::Const)
            || self.check_keyword(Keyword::Let)
            || self.check_keyword(Keyword::Var)
        {
            self.parse_variable_declaration().map(Declaration::Variable)
        } else {
            self.parse_function_declaration().map(Declaration::Function)
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration, ParseError> {
        let keyword = self.advance().clone();
        let kind = match keyword.kind {
            TokenKind::Keyword(Keyword::Const) => VariableKind::Const,
            TokenKind::Keyword(Keyword::Let) => VariableKind::Let,
            TokenKind::Keyword(Keyword::Var) => VariableKind::Var,
            _ => return Err(self.error(keyword.span, "Expected variable declaration keyword")),
        };

        let mut declarations = Vec::new();
        loop {
            let pattern = self.parse_pattern()?;
            let mut type_annotation = None;
            if self.match_punctuator(Punctuator::Colon) {
                type_annotation = Some(self.parse_type_annotation(pattern.span_start())?);
            }
            let mut init = None;
            let mut end = pattern.span_end();
            if self.match_operator(Operator::Assign) {
                let expr = self.parse_expression()?;
                end = expr.span().end;
                init = Some(expr);
            } else if matches!(kind, VariableKind::Const) {
                return Err(self.error(
                    self.current().span,
                    "const declarations must be initialized",
                ));
            }
            let span = Span::new(pattern.span_start(), end);
            declarations.push(VariableDeclarator {
                span,
                id: pattern,
                type_annotation,
                init,
            });
            if !self.match_punctuator(Punctuator::Comma) {
                break;
            }
        }
        self.eat_semicolon();
        let end = declarations
            .last()
            .map(|decl| decl.span.end)
            .unwrap_or(keyword.span.end);
        Ok(VariableDeclaration {
            span: Span::new(keyword.span.start, end),
            kind,
            declarations,
        })
    }

    fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration, ParseError> {
        let is_async = if self.match_keyword(Keyword::Async) {
            true
        } else {
            false
        };
        let function_token = self.expect_keyword(Keyword::Function)?;
        let is_generator = self.match_operator(Operator::Mul);
        let name = self.parse_identifier()?;
        self.expect_punctuator(Punctuator::LParen)?;
        let params = self.parse_parameter_list()?;
        self.expect_punctuator(Punctuator::RParen)?;
        let return_type = if self.match_punctuator(Punctuator::Colon) {
            Some(self.parse_type_annotation(function_token.span.start)?)
        } else {
            None
        };
        let body = self.parse_block_statement()?;
        let span = Span::new(function_token.span.start, body.span.end);
        Ok(FunctionDeclaration {
            span,
            name,
            params,
            return_type,
            body,
            is_async,
            is_generator,
        })
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<FunctionParam>, ParseError> {
        let mut params = Vec::new();
        if self.check_punctuator(Punctuator::RParen) {
            return Ok(params);
        }
        loop {
            let pattern = self.parse_pattern()?;
            let mut type_annotation = None;
            if self.match_punctuator(Punctuator::Colon) {
                type_annotation = Some(self.parse_type_annotation(pattern.span_start())?);
            }
            let span = Span::new(pattern.span_start(), pattern.span_end());
            params.push(FunctionParam {
                span,
                pattern,
                type_annotation,
            });
            if !self.match_punctuator(Punctuator::Comma) {
                break;
            }
        }
        Ok(params)
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let ident = self.parse_identifier()?;
        Ok(Pattern::Identifier(ident))
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression, ParseError> {
        let left = self.parse_binary_expression(1)?;
        if let Some(op) = self.match_assignment_operator() {
            let right = self.parse_assignment_expression()?;
            let span = Span::new(left.span().start, right.span().end);
            return Ok(Expression::Assignment(AssignmentExpression {
                span,
                left: Box::new(left),
                operator: op,
                right: Box::new(right),
            }));
        }
        Ok(left)
    }

    fn parse_binary_expression(&mut self, min_precedence: u8) -> Result<Expression, ParseError> {
        let mut left = self.parse_unary_expression()?;

        while let Some((precedence, _, binary_op)) = self.current_binary_operator() {
            if precedence < min_precedence {
                break;
            }
            self.advance();
            let right = self.parse_binary_expression(precedence + 1)?;
            let span = Span::new(left.span().start, right.span().end);
            left = Expression::Binary(BinaryExpression {
                span,
                left: Box::new(left),
                operator: binary_op,
                right: Box::new(right),
            });
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParseError> {
        if self.match_operator(Operator::Add) {
            let operator = self.previous().clone();
            let expr = self.parse_unary_expression()?;
            let span = Span::new(operator.span.start, expr.span().end);
            return Ok(Expression::Unary(UnaryExpression {
                span,
                operator: UnaryOperator::Plus,
                argument: Box::new(expr),
                prefix: true,
            }));
        }
        if self.match_operator(Operator::Sub) {
            let operator = self.previous().clone();
            let expr = self.parse_unary_expression()?;
            let span = Span::new(operator.span.start, expr.span().end);
            return Ok(Expression::Unary(UnaryExpression {
                span,
                operator: UnaryOperator::Minus,
                argument: Box::new(expr),
                prefix: true,
            }));
        }
        if self.match_operator(Operator::Bang) {
            let operator = self.previous().clone();
            let expr = self.parse_unary_expression()?;
            let span = Span::new(operator.span.start, expr.span().end);
            return Ok(Expression::Unary(UnaryExpression {
                span,
                operator: UnaryOperator::Not,
                argument: Box::new(expr),
                prefix: true,
            }));
        }
        self.parse_left_hand_side_expression()
    }

    fn parse_left_hand_side_expression(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_primary_expression()?;
        loop {
            if self.match_punctuator(Punctuator::LParen) {
                let args = self.parse_argument_list()?;
                let closing = self.expect_punctuator(Punctuator::RParen)?;
                let span = Span::new(expr.span().start, closing.span.end);
                expr = Expression::Call(CallExpression {
                    span,
                    callee: Box::new(expr),
                    arguments: args,
                    type_arguments: Vec::new(),
                });
            } else if self.match_punctuator(Punctuator::Dot) {
                let property = self.parse_identifier()?;
                let span = Span::new(expr.span().start, property.span.end);
                expr = Expression::Member(MemberExpression {
                    span,
                    object: Box::new(expr),
                    property: MemberProperty::Identifier(property),
                    computed: false,
                });
            } else if self.match_punctuator(Punctuator::LBracket) {
                let property = self.parse_expression()?;
                let closing = self.expect_punctuator(Punctuator::RBracket)?;
                let span = Span::new(expr.span().start, closing.span.end);
                expr = Expression::Member(MemberExpression {
                    span,
                    object: Box::new(expr),
                    property: MemberProperty::Expression(Box::new(property)),
                    computed: true,
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ParseError> {
        let token = self.current().clone();
        match token.kind {
            TokenKind::NumberLiteral(value) => {
                self.advance();
                Ok(Expression::Literal(Literal::Number(NumberLiteral {
                    span: token.span,
                    value,
                })))
            }
            TokenKind::StringLiteral => {
                self.advance();
                Ok(Expression::Literal(Literal::String(StringLiteral {
                    span: token.span,
                    value: token.lexeme,
                })))
            }
            TokenKind::Keyword(Keyword::True) => {
                self.advance();
                Ok(Expression::Literal(Literal::Boolean(true, token.span)))
            }
            TokenKind::Keyword(Keyword::False) => {
                self.advance();
                Ok(Expression::Literal(Literal::Boolean(false, token.span)))
            }
            TokenKind::Keyword(Keyword::Null) => {
                self.advance();
                Ok(Expression::Literal(Literal::Null(token.span)))
            }
            TokenKind::Identifier => {
                let identifier = self.parse_identifier()?;
                Ok(Expression::Identifier(identifier))
            }
            TokenKind::Punctuator(Punctuator::LParen) => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect_punctuator(Punctuator::RParen)?;
                let span = Span::new(token.span.start, self.previous().span.end);
                Ok(Expression::Parenthesized(ParenthesizedExpression {
                    span,
                    expression: Box::new(expr),
                }))
            }
            TokenKind::Punctuator(Punctuator::LBracket) => self.parse_array_expression(),
            TokenKind::Punctuator(Punctuator::LBrace) => self.parse_object_expression(),
            TokenKind::Operator(Operator::Lt) => self.parse_jsx_element(),
            TokenKind::JsxText => {
                self.advance();
                Ok(Expression::Literal(Literal::String(StringLiteral {
                    span: token.span,
                    value: token.lexeme.trim().to_string(),
                })))
            }
            _ => Err(self.error(token.span, "Unexpected token in expression")),
        }
    }

    fn parse_array_expression(&mut self) -> Result<Expression, ParseError> {
        let open = self.expect_punctuator(Punctuator::LBracket)?;
        let mut elements = Vec::new();
        if self.check_punctuator(Punctuator::RBracket) {
            let close = self.expect_punctuator(Punctuator::RBracket)?;
            return Ok(Expression::Array(ArrayExpression {
                span: Span::new(open.span.start, close.span.end),
                elements,
            }));
        }
        loop {
            if self.check_punctuator(Punctuator::RBracket) {
                break;
            }
            if self.match_punctuator(Punctuator::Comma) {
                elements.push(None);
                continue;
            }
            let expr = self.parse_expression()?;
            elements.push(Some(expr));
            if !self.match_punctuator(Punctuator::Comma) {
                break;
            }
        }
        let close = self.expect_punctuator(Punctuator::RBracket)?;
        Ok(Expression::Array(ArrayExpression {
            span: Span::new(open.span.start, close.span.end),
            elements,
        }))
    }

    fn parse_object_expression(&mut self) -> Result<Expression, ParseError> {
        let open = self.expect_punctuator(Punctuator::LBrace)?;
        let mut properties = Vec::new();
        if self.check_punctuator(Punctuator::RBrace) {
            let close = self.expect_punctuator(Punctuator::RBrace)?;
            return Ok(Expression::Object(ObjectExpression {
                span: Span::new(open.span.start, close.span.end),
                properties,
            }));
        }
        loop {
            if self.check_punctuator(Punctuator::RBrace) {
                break;
            }
            if self.match_punctuator(Punctuator::Spread) {
                let expr = self.parse_expression()?;
                let end = expr.span().end;
                properties.push(ObjectProperty::Spread(SpreadElement {
                    span: Span::new(open.span.start, end),
                    argument: expr,
                }));
            } else {
                let key_token = self.current().clone();
                let key = match key_token.kind {
                    TokenKind::Identifier => PropertyKey::Identifier(self.parse_identifier()?),
                    TokenKind::StringLiteral => {
                        self.advance();
                        PropertyKey::String(StringLiteral {
                            span: key_token.span,
                            value: key_token.lexeme,
                        })
                    }
                    TokenKind::NumberLiteral(value) => {
                        self.advance();
                        PropertyKey::Number(NumberLiteral {
                            span: key_token.span,
                            value,
                        })
                    }
                    _ => return Err(self.error(key_token.span, "Invalid object property key")),
                };
                let shorthand_possible = matches!(key, PropertyKey::Identifier(_));
                if !shorthand_possible || self.check_punctuator(Punctuator::Colon) {
                    self.expect_punctuator(Punctuator::Colon)?;
                    let value = self.parse_expression()?;
                    let span = Span::new(key.span_start(), value.span().end);
                    properties.push(ObjectProperty::Property(Property {
                        span,
                        key,
                        value,
                        shorthand: false,
                    }));
                } else {
                    let identifier = match key {
                        PropertyKey::Identifier(ref id) => id.clone(),
                        _ => unreachable!(),
                    };
                    properties.push(ObjectProperty::Property(Property {
                        span: identifier.span,
                        key,
                        value: Expression::Identifier(identifier.clone()),
                        shorthand: true,
                    }));
                }
            }
            if !self.match_punctuator(Punctuator::Comma) {
                break;
            }
        }
        let close = self.expect_punctuator(Punctuator::RBrace)?;
        Ok(Expression::Object(ObjectExpression {
            span: Span::new(open.span.start, close.span.end),
            properties,
        }))
    }

    fn parse_jsx_element(&mut self) -> Result<Expression, ParseError> {
        let lt = self.expect_operator(Operator::Lt)?;
        if self.match_operator(Operator::Div) {
            return Err(self.error(self.current().span, "Unexpected JSX closing tag"));
        }
        let name = self.parse_jsx_name()?;
        let attributes = self.parse_jsx_attributes()?;
        let mut children = Vec::new();

        let self_closing = if self.match_operator(Operator::Div) {
            self.expect_operator(Operator::Gt)?;
            true
        } else {
            self.expect_operator(Operator::Gt)?;
            false
        };

        let end = if self_closing {
            self.previous().span.end
        } else {
            let closing_end = loop {
                if self.check_jsx_closing(&name) {
                    self.expect_operator(Operator::Lt)?;
                    self.expect_operator(Operator::Div)?;
                    let closing = self.parse_jsx_name()?;
                    if !self.jsx_names_equal(&name, &closing) {
                        return Err(
                            self.error(closing.span(), "JSX closing tag must match opening tag")
                        );
                    }
                    let gt = self.expect_operator(Operator::Gt)?;
                    break gt.span.end;
                } else if self.check_kind(&TokenKind::JsxText) {
                    let text = self.advance().clone();
                    let value = text.lexeme.trim().to_string();
                    if !value.is_empty() {
                        children.push(JsxChild::Text(StringLiteral {
                            span: text.span,
                            value,
                        }));
                    }
                } else if self.match_punctuator(Punctuator::LBrace) {
                    let expr = self.parse_expression()?;
                    let end_brace = self.expect_punctuator(Punctuator::RBrace)?;
                    children.push(JsxChild::Expression(JsxExpressionContainer {
                        span: Span::new(expr.span().start, end_brace.span.end),
                        expression: expr,
                    }));
                } else if self.check_operator(Operator::Lt) {
                    let element_expr = self.parse_jsx_element()?;
                    if let Expression::JsxElement(el) = element_expr {
                        children.push(JsxChild::Element(el));
                    } else {
                        return Err(self.error(self.current().span, "Expected JSX element"));
                    }
                } else if self.is_at_end() {
                    return Err(self.error(self.current().span, "Unterminated JSX element"));
                } else {
                    self.advance();
                }
            };
            closing_end
        };

        Ok(Expression::JsxElement(JsxElement {
            span: Span::new(lt.span.start, end),
            name,
            attributes,
            children,
            self_closing,
        }))
    }

    fn parse_jsx_name(&mut self) -> Result<JsxName, ParseError> {
        let token = self.current().clone();
        if !matches!(token.kind, TokenKind::Identifier) {
            return Err(self.error(token.span, "Expected JSX identifier"));
        }
        let ident = self.parse_identifier()?;
        let mut name = JsxName::Identifier(ident);
        while self.match_punctuator(Punctuator::Dot) {
            let property = self.parse_identifier()?;
            name = JsxName::Member {
                object: Box::new(name),
                property,
            };
        }
        Ok(name)
    }

    fn parse_jsx_attributes(&mut self) -> Result<Vec<JsxAttribute>, ParseError> {
        let mut attributes = Vec::new();
        while matches!(self.current().kind, TokenKind::Identifier) {
            let name = self.parse_identifier()?;
            let mut value = None;
            if self.match_operator(Operator::Assign) {
                if matches!(self.current().kind, TokenKind::StringLiteral) {
                    let token = self.advance().clone();
                    value = Some(JsxAttributeValue::String(StringLiteral {
                        span: token.span,
                        value: token.lexeme,
                    }));
                } else if self.match_punctuator(Punctuator::LBrace) {
                    let expr = self.parse_expression()?;
                    let end_brace = self.expect_punctuator(Punctuator::RBrace)?;
                    value = Some(JsxAttributeValue::Expression(JsxExpressionContainer {
                        span: Span::new(expr.span().start, end_brace.span.end),
                        expression: expr,
                    }));
                } else {
                    return Err(self.error(self.current().span, "Invalid JSX attribute value"));
                }
            }
            let span_end = value
                .as_ref()
                .map(|value| match value {
                    JsxAttributeValue::String(lit) => lit.span.end,
                    JsxAttributeValue::Expression(expr) => expr.span.end,
                })
                .unwrap_or(name.span.end);
            attributes.push(JsxAttribute {
                span: Span::new(name.span.start, span_end),
                name: JsxAttributeName::Identifier(name),
                value,
            });
        }
        Ok(attributes)
    }

    fn check_jsx_closing(&self, _name: &JsxName) -> bool {
        if !self.check_operator(Operator::Lt) {
            return false;
        }
        if let Some(TokenKind::Operator(Operator::Div)) = self.lookahead_kind(1) {
            return true;
        }
        false
    }

    fn jsx_names_equal(&self, left: &JsxName, right: &JsxName) -> bool {
        match (left, right) {
            (JsxName::Identifier(a), JsxName::Identifier(b)) => a.name == b.name,
            (
                JsxName::Member {
                    object: a_obj,
                    property: a_prop,
                },
                JsxName::Member {
                    object: b_obj,
                    property: b_prop,
                },
            ) => a_prop.name == b_prop.name && self.jsx_names_equal(a_obj, b_obj),
            _ => false,
        }
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut args = Vec::new();
        if self.check_punctuator(Punctuator::RParen) {
            return Ok(args);
        }
        loop {
            let expr = self.parse_expression()?;
            args.push(expr);
            if !self.match_punctuator(Punctuator::Comma) {
                break;
            }
        }
        Ok(args)
    }

    fn parse_type_annotation(&mut self, start: usize) -> Result<TypeAnnotation, ParseError> {
        let mut text = String::new();
        let mut depth_angle = 0usize;
        let mut depth_paren = 0usize;
        let mut depth_bracket = 0usize;
        let mut end = start;
        while !self.is_at_end() {
            let token = self.current().clone();
            if depth_angle == 0 && depth_paren == 0 && depth_bracket == 0 {
                if matches!(token.kind, TokenKind::Operator(Operator::Assign))
                    || matches!(token.kind, TokenKind::Punctuator(Punctuator::Comma))
                    || matches!(token.kind, TokenKind::Punctuator(Punctuator::Semi))
                    || matches!(token.kind, TokenKind::Punctuator(Punctuator::RParen))
                    || matches!(token.kind, TokenKind::Punctuator(Punctuator::RBrace))
                    || matches!(token.kind, TokenKind::Punctuator(Punctuator::LBrace))
                    || matches!(token.kind, TokenKind::Operator(Operator::FatArrow))
                {
                    break;
                }
            }
            match token.kind {
                TokenKind::Operator(Operator::Lt) => depth_angle += 1,
                TokenKind::Operator(Operator::Gt) => {
                    if depth_angle == 0 {
                        break;
                    }
                    depth_angle -= 1;
                }
                TokenKind::Punctuator(Punctuator::LParen) => depth_paren += 1,
                TokenKind::Punctuator(Punctuator::RParen) => {
                    if depth_paren == 0 {
                        break;
                    }
                    depth_paren -= 1;
                }
                TokenKind::Punctuator(Punctuator::LBracket) => depth_bracket += 1,
                TokenKind::Punctuator(Punctuator::RBracket) => {
                    if depth_bracket == 0 {
                        break;
                    }
                    depth_bracket -= 1;
                }
                _ => {}
            }
            if !text.is_empty() {
                text.push(' ');
            }
            text.push_str(&token.lexeme);
            end = token.span.end;
            self.advance();
        }
        Ok(TypeAnnotation {
            span: Span::new(start, end),
            text: text.trim().to_string(),
        })
    }

    fn match_assignment_operator(&mut self) -> Option<AssignmentOperator> {
        if self.match_operator(Operator::Assign) {
            Some(AssignmentOperator::Assign)
        } else if self.match_operator(Operator::AddAssign) {
            Some(AssignmentOperator::AddAssign)
        } else if self.match_operator(Operator::SubAssign) {
            Some(AssignmentOperator::SubAssign)
        } else if self.match_operator(Operator::MulAssign) {
            Some(AssignmentOperator::MulAssign)
        } else if self.match_operator(Operator::DivAssign) {
            Some(AssignmentOperator::DivAssign)
        } else if self.match_operator(Operator::ModAssign) {
            Some(AssignmentOperator::ModAssign)
        } else {
            None
        }
    }

    fn current_binary_operator(&self) -> Option<(u8, Operator, BinaryOperator)> {
        if let TokenKind::Operator(op) = self.current().kind {
            let (precedence, binary) = match op {
                Operator::OrOr => (1, BinaryOperator::LogicalOr),
                Operator::NullishCoalescing => (2, BinaryOperator::NullishCoalescing),
                Operator::AndAnd => (3, BinaryOperator::LogicalAnd),
                Operator::EqEq => (4, BinaryOperator::EqEq),
                Operator::NotEq => (4, BinaryOperator::NotEq),
                Operator::StrictEq => (4, BinaryOperator::StrictEq),
                Operator::StrictNotEq => (4, BinaryOperator::StrictNotEq),
                Operator::Lt => (5, BinaryOperator::Lt),
                Operator::Lte => (5, BinaryOperator::Lte),
                Operator::Gt => (5, BinaryOperator::Gt),
                Operator::Gte => (5, BinaryOperator::Gte),
                Operator::Add => (6, BinaryOperator::Add),
                Operator::Sub => (6, BinaryOperator::Sub),
                Operator::Mul => (7, BinaryOperator::Mul),
                Operator::Div => (7, BinaryOperator::Div),
                Operator::Mod => (7, BinaryOperator::Mod),
                _ => return None,
            };
            return Some((precedence, op, binary));
        }
        None
    }

    fn parse_identifier(&mut self) -> Result<Identifier, ParseError> {
        let token = self.current().clone();
        if !matches!(token.kind, TokenKind::Identifier) {
            return Err(self.error(token.span, "Expected identifier"));
        }
        self.advance();
        Ok(Identifier {
            span: token.span,
            name: token.lexeme,
        })
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<Token, ParseError> {
        let token = self.current().clone();
        if matches!(token.kind, TokenKind::Keyword(k) if k == keyword) {
            self.advance();
            Ok(token)
        } else {
            Err(self.error(token.span, format!("Expected keyword {:?}", keyword)))
        }
    }

    fn expect_punctuator(&mut self, punctuator: Punctuator) -> Result<Token, ParseError> {
        let token = self.current().clone();
        if matches!(token.kind, TokenKind::Punctuator(p) if p == punctuator) {
            self.advance();
            Ok(token)
        } else {
            Err(self.error(token.span, format!("Expected punctuator {:?}", punctuator)))
        }
    }

    fn expect_operator(&mut self, operator: Operator) -> Result<Token, ParseError> {
        let token = self.current().clone();
        if matches!(token.kind, TokenKind::Operator(op) if op == operator) {
            self.advance();
            Ok(token)
        } else {
            Err(self.error(token.span, format!("Expected operator {:?}", operator)))
        }
    }

    fn expect_string_literal(&mut self) -> Result<Token, ParseError> {
        let token = self.current().clone();
        if matches!(token.kind, TokenKind::StringLiteral) {
            self.advance();
            Ok(token)
        } else {
            Err(self.error(token.span, "Expected string literal"))
        }
    }

    fn match_keyword(&mut self, keyword: Keyword) -> bool {
        if self.check_keyword(keyword) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_punctuator(&mut self, punctuator: Punctuator) -> bool {
        if self.check_punctuator(punctuator) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_operator(&mut self, operator: Operator) -> bool {
        if self.check_operator(operator) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check_keyword(&self, keyword: Keyword) -> bool {
        matches!(self.current().kind, TokenKind::Keyword(k) if k == keyword)
    }

    fn check_punctuator(&self, punctuator: Punctuator) -> bool {
        matches!(self.current().kind, TokenKind::Punctuator(p) if p == punctuator)
    }

    fn check_operator(&self, operator: Operator) -> bool {
        matches!(self.current().kind, TokenKind::Operator(op) if op == operator)
    }

    fn check_kind(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind)
    }

    fn lookahead_kind(&self, offset: usize) -> Option<&TokenKind> {
        self.tokens.get(self.pos + offset).map(|token| &token.kind)
    }

    fn eat_semicolon(&mut self) {
        let _ = self.match_punctuator(Punctuator::Semi);
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.previous()
    }

    fn current(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.pos - 1]
    }

    fn is_at_end(&self) -> bool {
        matches!(self.tokens[self.pos].kind, TokenKind::Eof)
    }

    fn error(&self, span: Span, message: impl Into<String>) -> ParseError {
        ParseError::new(message, span)
    }
}

trait SpanAccess {
    fn span_start(&self) -> usize;
    fn span_end(&self) -> usize;
}

impl SpanAccess for Pattern {
    fn span_start(&self) -> usize {
        match self {
            Pattern::Identifier(id) => id.span.start,
        }
    }

    fn span_end(&self) -> usize {
        match self {
            Pattern::Identifier(id) => id.span.end,
        }
    }
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Identifier(id) => id.span,
            Expression::Literal(lit) => match lit {
                Literal::String(l) => l.span,
                Literal::Number(n) => n.span,
                Literal::Boolean(_, span) => *span,
                Literal::Null(span) => *span,
                Literal::Undefined(span) => *span,
            },
            Expression::Array(array) => array.span,
            Expression::Object(object) => object.span,
            Expression::Call(call) => call.span,
            Expression::Member(member) => member.span,
            Expression::Binary(binary) => binary.span,
            Expression::Unary(unary) => unary.span,
            Expression::Assignment(assign) => assign.span,
            Expression::Conditional(cond) => cond.span,
            Expression::JsxElement(jsx) => jsx.span,
            Expression::Parenthesized(paren) => paren.span,
        }
    }
}

impl JsxName {
    fn span(&self) -> Span {
        match self {
            JsxName::Identifier(id) => id.span,
            JsxName::Member { object, property } => {
                let start = object.span().start;
                Span::new(start, property.span.end)
            }
        }
    }
}

impl PropertyKey {
    fn span_start(&self) -> usize {
        match self {
            PropertyKey::Identifier(id) => id.span.start,
            PropertyKey::String(lit) => lit.span.start,
            PropertyKey::Number(num) => num.span.start,
        }
    }
}
