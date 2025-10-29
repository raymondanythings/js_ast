use crate::ast::*;

pub trait Visitor {
    fn visit_program(&mut self, program: &Program) {
        walk_program(self, program);
    }

    fn visit_module_item(&mut self, item: &ModuleItem) {
        walk_module_item(self, item);
    }

    fn visit_import_declaration(&mut self, import: &ImportDeclaration) {
        walk_import_declaration(self, import);
    }

    fn visit_import_specifier(&mut self, specifier: &ImportSpecifier) {
        walk_import_specifier(self, specifier);
    }

    fn visit_export_declaration(&mut self, export: &ExportDeclaration) {
        walk_export_declaration(self, export);
    }

    fn visit_export_specifier(&mut self, specifier: &ExportSpecifier) {
        walk_export_specifier(self, specifier);
    }

    fn visit_statement(&mut self, statement: &Statement) {
        walk_statement(self, statement);
    }

    fn visit_declaration(&mut self, declaration: &Declaration) {
        walk_declaration(self, declaration);
    }

    fn visit_variable_declaration(&mut self, declaration: &VariableDeclaration) {
        walk_variable_declaration(self, declaration);
    }

    fn visit_variable_declarator(&mut self, declarator: &VariableDeclarator) {
        walk_variable_declarator(self, declarator);
    }

    fn visit_function_declaration(&mut self, declaration: &FunctionDeclaration) {
        walk_function_declaration(self, declaration);
    }

    fn visit_function_param(&mut self, param: &FunctionParam) {
        walk_function_param(self, param);
    }

    fn visit_block_statement(&mut self, block: &BlockStatement) {
        walk_block_statement(self, block);
    }

    fn visit_return_statement(&mut self, statement: &ReturnStatement) {
        walk_return_statement(self, statement);
    }

    fn visit_if_statement(&mut self, statement: &IfStatement) {
        walk_if_statement(self, statement);
    }

    fn visit_expression_statement(&mut self, statement: &ExpressionStatement) {
        walk_expression_statement(self, statement);
    }

    fn visit_expression(&mut self, expression: &Expression) {
        walk_expression(self, expression);
    }

    fn visit_pattern(&mut self, pattern: &Pattern) {
        walk_pattern(self, pattern);
    }

    fn visit_object_pattern(&mut self, pattern: &ObjectPattern) {
        walk_object_pattern(self, pattern);
    }

    fn visit_object_pattern_property(&mut self, property: &ObjectPatternProperty) {
        walk_object_pattern_property(self, property);
    }

    fn visit_type_annotation(&mut self, annotation: &TypeAnnotation) {
        walk_type_annotation(self, annotation);
    }

    fn visit_identifier(&mut self, _identifier: &Identifier) {}

    fn visit_string_literal(&mut self, _literal: &StringLiteral) {}

    fn visit_number_literal(&mut self, _literal: &NumberLiteral) {}

    fn visit_literal(&mut self, literal: &Literal) {
        walk_literal(self, literal);
    }

    fn visit_array_expression(&mut self, expression: &ArrayExpression) {
        walk_array_expression(self, expression);
    }

    fn visit_object_expression(&mut self, expression: &ObjectExpression) {
        walk_object_expression(self, expression);
    }

    fn visit_arrow_function_expression(&mut self, expression: &ArrowFunctionExpression) {
        walk_arrow_function_expression(self, expression);
    }

    fn visit_arrow_function_body(&mut self, body: &ArrowFunctionBody) {
        walk_arrow_function_body(self, body);
    }

    fn visit_object_property(&mut self, property: &ObjectProperty) {
        walk_object_property(self, property);
    }

    fn visit_property(&mut self, property: &Property) {
        walk_property(self, property);
    }

    fn visit_spread_element(&mut self, element: &SpreadElement) {
        walk_spread_element(self, element);
    }

    fn visit_property_key(&mut self, key: &PropertyKey) {
        walk_property_key(self, key);
    }

    fn visit_call_expression(&mut self, expression: &CallExpression) {
        walk_call_expression(self, expression);
    }

    fn visit_member_expression(&mut self, expression: &MemberExpression) {
        walk_member_expression(self, expression);
    }

    fn visit_binary_expression(&mut self, expression: &BinaryExpression) {
        walk_binary_expression(self, expression);
    }

    fn visit_unary_expression(&mut self, expression: &UnaryExpression) {
        walk_unary_expression(self, expression);
    }

    fn visit_assignment_expression(&mut self, expression: &AssignmentExpression) {
        walk_assignment_expression(self, expression);
    }

    fn visit_conditional_expression(&mut self, expression: &ConditionalExpression) {
        walk_conditional_expression(self, expression);
    }

    fn visit_parenthesized_expression(&mut self, expression: &ParenthesizedExpression) {
        walk_parenthesized_expression(self, expression);
    }

    fn visit_jsx_element(&mut self, element: &JsxElement) {
        walk_jsx_element(self, element);
    }

    fn visit_jsx_name(&mut self, name: &JsxName) {
        walk_jsx_name(self, name);
    }

    fn visit_jsx_attribute(&mut self, attribute: &JsxAttribute) {
        walk_jsx_attribute(self, attribute);
    }

    fn visit_jsx_attribute_name(&mut self, name: &JsxAttributeName) {
        walk_jsx_attribute_name(self, name);
    }

    fn visit_jsx_attribute_value(&mut self, value: &JsxAttributeValue) {
        walk_jsx_attribute_value(self, value);
    }

    fn visit_jsx_expression_container(&mut self, container: &JsxExpressionContainer) {
        walk_jsx_expression_container(self, container);
    }

    fn visit_jsx_child(&mut self, child: &JsxChild) {
        walk_jsx_child(self, child);
    }
}

pub fn walk_program<V: Visitor + ?Sized>(visitor: &mut V, program: &Program) {
    for item in &program.body {
        visitor.visit_module_item(item);
    }
}

pub fn walk_module_item<V: Visitor + ?Sized>(visitor: &mut V, item: &ModuleItem) {
    match item {
        ModuleItem::Import(import) => visitor.visit_import_declaration(import),
        ModuleItem::Export(export) => visitor.visit_export_declaration(export),
        ModuleItem::Statement(statement) => visitor.visit_statement(statement),
    }
}

pub fn walk_import_declaration<V: Visitor + ?Sized>(visitor: &mut V, import: &ImportDeclaration) {
    for specifier in &import.specifiers {
        visitor.visit_import_specifier(specifier);
    }
    visitor.visit_string_literal(&import.source);
}

pub fn walk_import_specifier<V: Visitor + ?Sized>(visitor: &mut V, specifier: &ImportSpecifier) {
    match specifier {
        ImportSpecifier::Default { local, .. } => visitor.visit_identifier(local),
        ImportSpecifier::Namespace { local, .. } => visitor.visit_identifier(local),
        ImportSpecifier::Named {
            local, imported, ..
        } => {
            if let Some(imported) = imported {
                visitor.visit_identifier(imported);
            }
            visitor.visit_identifier(local);
        }
    }
}

pub fn walk_export_declaration<V: Visitor + ?Sized>(visitor: &mut V, export: &ExportDeclaration) {
    match export {
        ExportDeclaration::Named {
            specifiers, source, ..
        } => {
            for specifier in specifiers {
                visitor.visit_export_specifier(specifier);
            }
            if let Some(source) = source {
                visitor.visit_string_literal(source);
            }
        }
        ExportDeclaration::Default { declaration, .. } => visitor.visit_declaration(declaration),
        ExportDeclaration::DefaultExpression { expression, .. } => {
            visitor.visit_expression(expression)
        }
        ExportDeclaration::Declaration { declaration, .. } => {
            visitor.visit_declaration(declaration)
        }
        ExportDeclaration::All { source, .. } => {
            if let Some(source) = source {
                visitor.visit_string_literal(source);
            }
        }
    }
}

pub fn walk_export_specifier<V: Visitor + ?Sized>(visitor: &mut V, specifier: &ExportSpecifier) {
    visitor.visit_identifier(&specifier.local);
    if let Some(exported) = &specifier.exported {
        visitor.visit_identifier(exported);
    }
}

pub fn walk_statement<V: Visitor + ?Sized>(visitor: &mut V, statement: &Statement) {
    match statement {
        Statement::Declaration(declaration) => visitor.visit_declaration(declaration),
        Statement::Return(statement) => visitor.visit_return_statement(statement),
        Statement::Expression(statement) => visitor.visit_expression_statement(statement),
        Statement::Block(block) => visitor.visit_block_statement(block),
        Statement::If(statement) => visitor.visit_if_statement(statement),
    }
}

pub fn walk_declaration<V: Visitor + ?Sized>(visitor: &mut V, declaration: &Declaration) {
    match declaration {
        Declaration::Variable(decl) => visitor.visit_variable_declaration(decl),
        Declaration::Function(func) => visitor.visit_function_declaration(func),
    }
}

pub fn walk_variable_declaration<V: Visitor + ?Sized>(
    visitor: &mut V,
    declaration: &VariableDeclaration,
) {
    for declarator in &declaration.declarations {
        visitor.visit_variable_declarator(declarator);
    }
}

pub fn walk_variable_declarator<V: Visitor + ?Sized>(
    visitor: &mut V,
    declarator: &VariableDeclarator,
) {
    visitor.visit_pattern(&declarator.id);
    if let Some(annotation) = &declarator.type_annotation {
        visitor.visit_type_annotation(annotation);
    }
    if let Some(init) = &declarator.init {
        visitor.visit_expression(init);
    }
}

pub fn walk_function_declaration<V: Visitor + ?Sized>(
    visitor: &mut V,
    declaration: &FunctionDeclaration,
) {
    visitor.visit_identifier(&declaration.name);
    for param in &declaration.params {
        visitor.visit_function_param(param);
    }
    if let Some(return_type) = &declaration.return_type {
        visitor.visit_type_annotation(return_type);
    }
    visitor.visit_block_statement(&declaration.body);
}

pub fn walk_function_param<V: Visitor + ?Sized>(visitor: &mut V, param: &FunctionParam) {
    visitor.visit_pattern(&param.pattern);
    if let Some(annotation) = &param.type_annotation {
        visitor.visit_type_annotation(annotation);
    }
}

pub fn walk_block_statement<V: Visitor + ?Sized>(visitor: &mut V, block: &BlockStatement) {
    for statement in &block.body {
        visitor.visit_statement(statement);
    }
}

pub fn walk_return_statement<V: Visitor + ?Sized>(visitor: &mut V, statement: &ReturnStatement) {
    if let Some(argument) = &statement.argument {
        visitor.visit_expression(argument);
    }
}

pub fn walk_if_statement<V: Visitor + ?Sized>(visitor: &mut V, statement: &IfStatement) {
    visitor.visit_expression(&statement.test);
    visitor.visit_statement(&statement.consequent);
    if let Some(alternate) = &statement.alternate {
        visitor.visit_statement(alternate);
    }
}

pub fn walk_expression_statement<V: Visitor + ?Sized>(
    visitor: &mut V,
    statement: &ExpressionStatement,
) {
    visitor.visit_expression(&statement.expression);
}

pub fn walk_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &Expression) {
    match expression {
        Expression::Identifier(identifier) => visitor.visit_identifier(identifier),
        Expression::Literal(literal) => visitor.visit_literal(literal),
        Expression::Array(array) => visitor.visit_array_expression(array),
        Expression::Object(object) => visitor.visit_object_expression(object),
        Expression::Call(call) => visitor.visit_call_expression(call),
        Expression::Member(member) => visitor.visit_member_expression(member),
        Expression::Binary(binary) => visitor.visit_binary_expression(binary),
        Expression::Unary(unary) => visitor.visit_unary_expression(unary),
        Expression::Assignment(assign) => visitor.visit_assignment_expression(assign),
        Expression::Conditional(cond) => visitor.visit_conditional_expression(cond),
        Expression::JsxElement(element) => visitor.visit_jsx_element(element),
        Expression::ArrowFunction(func) => visitor.visit_arrow_function_expression(func),
        Expression::Parenthesized(paren) => visitor.visit_parenthesized_expression(paren),
    }
}

pub fn walk_pattern<V: Visitor + ?Sized>(visitor: &mut V, pattern: &Pattern) {
    match pattern {
        Pattern::Identifier(identifier) => visitor.visit_identifier(identifier),
        Pattern::Object(pattern) => visitor.visit_object_pattern(pattern),
    }
}

pub fn walk_object_pattern<V: Visitor + ?Sized>(visitor: &mut V, pattern: &ObjectPattern) {
    for property in &pattern.properties {
        visitor.visit_object_pattern_property(property);
    }
}

pub fn walk_object_pattern_property<V: Visitor + ?Sized>(
    visitor: &mut V,
    property: &ObjectPatternProperty,
) {
    match property {
        ObjectPatternProperty::Shorthand(identifier) => visitor.visit_identifier(identifier),
        ObjectPatternProperty::KeyValue { key, value } => {
            visitor.visit_identifier(key);
            visitor.visit_pattern(value);
        }
    }
}

pub fn walk_type_annotation<V: Visitor + ?Sized>(_visitor: &mut V, _annotation: &TypeAnnotation) {}

pub fn walk_literal<V: Visitor + ?Sized>(visitor: &mut V, literal: &Literal) {
    match literal {
        Literal::String(lit) => visitor.visit_string_literal(lit),
        Literal::Number(lit) => visitor.visit_number_literal(lit),
        Literal::Boolean(_, _) => {}
        Literal::Null(_) => {}
        Literal::Undefined(_) => {}
    }
}

pub fn walk_array_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &ArrayExpression) {
    for element in &expression.elements {
        if let Some(expr) = element {
            visitor.visit_expression(expr);
        }
    }
}

pub fn walk_object_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &ObjectExpression) {
    for property in &expression.properties {
        visitor.visit_object_property(property);
    }
}

pub fn walk_object_property<V: Visitor + ?Sized>(visitor: &mut V, property: &ObjectProperty) {
    match property {
        ObjectProperty::Property(prop) => visitor.visit_property(prop),
        ObjectProperty::Spread(spread) => visitor.visit_spread_element(spread),
    }
}

pub fn walk_arrow_function_expression<V: Visitor + ?Sized>(
    visitor: &mut V,
    expression: &ArrowFunctionExpression,
) {
    for param in &expression.params {
        visitor.visit_function_param(param);
    }
    if let Some(return_type) = &expression.return_type {
        visitor.visit_type_annotation(return_type);
    }
    visitor.visit_arrow_function_body(&expression.body);
}

pub fn walk_arrow_function_body<V: Visitor + ?Sized>(visitor: &mut V, body: &ArrowFunctionBody) {
    match body {
        ArrowFunctionBody::Expression(expr) => visitor.visit_expression(expr),
        ArrowFunctionBody::Block(block) => visitor.visit_block_statement(block),
    }
}

pub fn walk_property<V: Visitor + ?Sized>(visitor: &mut V, property: &Property) {
    visitor.visit_property_key(&property.key);
    visitor.visit_expression(&property.value);
}

pub fn walk_spread_element<V: Visitor + ?Sized>(visitor: &mut V, element: &SpreadElement) {
    visitor.visit_expression(&element.argument);
}

pub fn walk_property_key<V: Visitor + ?Sized>(visitor: &mut V, key: &PropertyKey) {
    match key {
        PropertyKey::Identifier(identifier) => visitor.visit_identifier(identifier),
        PropertyKey::String(literal) => visitor.visit_string_literal(literal),
        PropertyKey::Number(literal) => visitor.visit_number_literal(literal),
    }
}

pub fn walk_call_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &CallExpression) {
    visitor.visit_expression(&expression.callee);
    for argument in &expression.arguments {
        visitor.visit_expression(argument);
    }
    for type_arg in &expression.type_arguments {
        visitor.visit_type_annotation(type_arg);
    }
}

pub fn walk_member_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &MemberExpression) {
    visitor.visit_expression(&expression.object);
    match &expression.property {
        MemberProperty::Identifier(identifier) => visitor.visit_identifier(identifier),
        MemberProperty::Expression(expr) => visitor.visit_expression(expr),
    }
}

pub fn walk_binary_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &BinaryExpression) {
    visitor.visit_expression(&expression.left);
    visitor.visit_expression(&expression.right);
}

pub fn walk_unary_expression<V: Visitor + ?Sized>(visitor: &mut V, expression: &UnaryExpression) {
    visitor.visit_expression(&expression.argument);
}

pub fn walk_assignment_expression<V: Visitor + ?Sized>(
    visitor: &mut V,
    expression: &AssignmentExpression,
) {
    visitor.visit_expression(&expression.left);
    visitor.visit_expression(&expression.right);
}

pub fn walk_conditional_expression<V: Visitor + ?Sized>(
    visitor: &mut V,
    expression: &ConditionalExpression,
) {
    visitor.visit_expression(&expression.test);
    visitor.visit_expression(&expression.consequent);
    visitor.visit_expression(&expression.alternate);
}

pub fn walk_parenthesized_expression<V: Visitor + ?Sized>(
    visitor: &mut V,
    expression: &ParenthesizedExpression,
) {
    visitor.visit_expression(&expression.expression);
}

pub fn walk_jsx_element<V: Visitor + ?Sized>(visitor: &mut V, element: &JsxElement) {
    visitor.visit_jsx_name(&element.name);
    for attribute in &element.attributes {
        visitor.visit_jsx_attribute(attribute);
    }
    for child in &element.children {
        visitor.visit_jsx_child(child);
    }
}

pub fn walk_jsx_name<V: Visitor + ?Sized>(visitor: &mut V, name: &JsxName) {
    match name {
        JsxName::Identifier(identifier) => visitor.visit_identifier(identifier),
        JsxName::Member { object, property } => {
            visitor.visit_jsx_name(object);
            visitor.visit_identifier(property);
        }
    }
}

pub fn walk_jsx_attribute<V: Visitor + ?Sized>(visitor: &mut V, attribute: &JsxAttribute) {
    visitor.visit_jsx_attribute_name(&attribute.name);
    if let Some(value) = &attribute.value {
        visitor.visit_jsx_attribute_value(value);
    }
}

pub fn walk_jsx_attribute_name<V: Visitor + ?Sized>(visitor: &mut V, name: &JsxAttributeName) {
    match name {
        JsxAttributeName::Identifier(identifier) => visitor.visit_identifier(identifier),
    }
}

pub fn walk_jsx_attribute_value<V: Visitor + ?Sized>(visitor: &mut V, value: &JsxAttributeValue) {
    match value {
        JsxAttributeValue::String(literal) => visitor.visit_string_literal(literal),
        JsxAttributeValue::Expression(container) => {
            visitor.visit_jsx_expression_container(container)
        }
    }
}

pub fn walk_jsx_expression_container<V: Visitor + ?Sized>(
    visitor: &mut V,
    container: &JsxExpressionContainer,
) {
    visitor.visit_expression(&container.expression);
}

pub fn walk_jsx_child<V: Visitor + ?Sized>(visitor: &mut V, child: &JsxChild) {
    match child {
        JsxChild::Element(element) => visitor.visit_jsx_element(element),
        JsxChild::Text(literal) => visitor.visit_string_literal(literal),
        JsxChild::Expression(container) => visitor.visit_jsx_expression_container(container),
    }
}
