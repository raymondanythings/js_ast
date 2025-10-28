use serde::Serialize;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Program {
    pub body: Vec<ModuleItem>,
}

impl Program {
    pub fn new(body: Vec<ModuleItem>) -> Self {
        Self { body }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ModuleItem {
    Import(ImportDeclaration),
    Export(ExportDeclaration),
    Statement(Statement),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ImportDeclaration {
    pub span: Span,
    pub specifiers: Vec<ImportSpecifier>,
    pub source: StringLiteral,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ImportSpecifier {
    Default {
        local: Identifier,
        span: Span,
    },
    Namespace {
        local: Identifier,
        span: Span,
    },
    Named {
        local: Identifier,
        imported: Option<Identifier>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ExportDeclaration {
    Named {
        span: Span,
        specifiers: Vec<ExportSpecifier>,
        source: Option<StringLiteral>,
    },
    Default {
        span: Span,
        declaration: Declaration,
    },
    DefaultExpression {
        span: Span,
        expression: Expression,
    },
    Declaration {
        span: Span,
        declaration: Declaration,
    },
    All {
        span: Span,
        source: Option<StringLiteral>,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ExportSpecifier {
    pub span: Span,
    pub local: Identifier,
    pub exported: Option<Identifier>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Statement {
    Declaration(Declaration),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
    If(IfStatement),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct VariableDeclaration {
    pub span: Span,
    pub kind: VariableKind,
    pub declarations: Vec<VariableDeclarator>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum VariableKind {
    Var,
    Let,
    Const,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct VariableDeclarator {
    pub span: Span,
    pub id: Pattern,
    pub type_annotation: Option<TypeAnnotation>,
    pub init: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Pattern {
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct TypeAnnotation {
    pub span: Span,
    pub text: String,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FunctionDeclaration {
    pub span: Span,
    pub name: Identifier,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<TypeAnnotation>,
    pub body: BlockStatement,
    pub is_async: bool,
    pub is_generator: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FunctionParam {
    pub span: Span,
    pub pattern: Pattern,
    pub type_annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BlockStatement {
    pub span: Span,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ReturnStatement {
    pub span: Span,
    pub argument: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct IfStatement {
    pub span: Span,
    pub test: Expression,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ExpressionStatement {
    pub span: Span,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Array(ArrayExpression),
    Object(ObjectExpression),
    Call(CallExpression),
    Member(MemberExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Assignment(AssignmentExpression),
    Conditional(ConditionalExpression),
    JsxElement(JsxElement),
    Parenthesized(ParenthesizedExpression),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ParenthesizedExpression {
    pub span: Span,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ConditionalExpression {
    pub span: Span,
    pub test: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct AssignmentExpression {
    pub span: Span,
    pub left: Box<Expression>,
    pub operator: AssignmentOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum AssignmentOperator {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct UnaryExpression {
    pub span: Span,
    pub operator: UnaryOperator,
    pub argument: Box<Expression>,
    pub prefix: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
    TypeOf,
    Void,
    Delete,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BinaryExpression {
    pub span: Span,
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    EqEq,
    NotEq,
    StrictEq,
    StrictNotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    LogicalAnd,
    LogicalOr,
    NullishCoalescing,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CallExpression {
    pub span: Span,
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub type_arguments: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct MemberExpression {
    pub span: Span,
    pub object: Box<Expression>,
    pub property: MemberProperty,
    pub computed: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum MemberProperty {
    Identifier(Identifier),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ArrayExpression {
    pub span: Span,
    pub elements: Vec<Option<Expression>>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ObjectExpression {
    pub span: Span,
    pub properties: Vec<ObjectProperty>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ObjectProperty {
    Property(Property),
    Spread(SpreadElement),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Property {
    pub span: Span,
    pub key: PropertyKey,
    pub value: Expression,
    pub shorthand: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SpreadElement {
    pub span: Span,
    pub argument: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum PropertyKey {
    Identifier(Identifier),
    String(StringLiteral),
    Number(NumberLiteral),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Literal {
    String(StringLiteral),
    Number(NumberLiteral),
    Boolean(bool, Span),
    Null(Span),
    Undefined(Span),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

impl Identifier {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            span,
            name: name.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StringLiteral {
    pub span: Span,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct NumberLiteral {
    pub span: Span,
    pub value: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct JsxElement {
    pub span: Span,
    pub name: JsxName,
    pub attributes: Vec<JsxAttribute>,
    pub children: Vec<JsxChild>,
    pub self_closing: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum JsxName {
    Identifier(Identifier),
    Member {
        object: Box<JsxName>,
        property: Identifier,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct JsxAttribute {
    pub span: Span,
    pub name: JsxAttributeName,
    pub value: Option<JsxAttributeValue>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum JsxAttributeName {
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum JsxAttributeValue {
    String(StringLiteral),
    Expression(JsxExpressionContainer),
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct JsxExpressionContainer {
    pub span: Span,
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum JsxChild {
    Element(JsxElement),
    Text(StringLiteral),
    Expression(JsxExpressionContainer),
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}
