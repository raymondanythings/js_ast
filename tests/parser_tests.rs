use js_ast::{
    BinaryOperator, ExportDeclaration, Expression, Identifier, ImportSpecifier, Literal,
    ModuleItem, Statement, VariableDeclaration, VariableKind, Visitor, parse_program,
    program_to_value,
};

#[test]
fn parses_basic_module() {
    let source = r#"
        import React, { useState as useLocalState } from "react";
        const value: number = 42;

        function greet(name: string): string {
            return "Hello, " + name;
        }
    "#;

    let program = parse_program(source).expect("failed to parse");
    assert_eq!(program.body.len(), 3);

    match &program.body[0] {
        ModuleItem::Import(import_decl) => {
            assert_eq!(import_decl.specifiers.len(), 2);
            match &import_decl.specifiers[0] {
                ImportSpecifier::Default { local, .. } => assert_eq!(local.name, "React"),
                other => panic!("unexpected import specifier: {:?}", other),
            }
            match &import_decl.specifiers[1] {
                ImportSpecifier::Named {
                    local, imported, ..
                } => {
                    assert_eq!(local.name, "useLocalState");
                    assert_eq!(imported.as_ref().unwrap().name, "useState");
                }
                other => panic!("unexpected import specifier: {:?}", other),
            }
            assert_eq!(import_decl.source.value, "react");
        }
        other => panic!("unexpected module item: {:?}", other),
    }

    match &program.body[1] {
        ModuleItem::Statement(Statement::Declaration(decl)) => match decl {
            js_ast::Declaration::Variable(VariableDeclaration {
                kind, declarations, ..
            }) => {
                assert!(matches!(kind, VariableKind::Const));
                assert_eq!(declarations.len(), 1);
                let declarator = &declarations[0];
                match &declarator.id {
                    js_ast::Pattern::Identifier(Identifier { name, .. }) => {
                        assert_eq!(name, "value")
                    }
                }
                assert_eq!(declarator.type_annotation.as_ref().unwrap().text, "number");
                match declarator.init.as_ref().unwrap() {
                    Expression::Literal(Literal::Number(number)) => {
                        assert_eq!(number.value, 42.0);
                    }
                    other => panic!("unexpected initializer: {:?}", other),
                }
            }
            other => panic!("unexpected declaration: {:?}", other),
        },
        other => panic!("unexpected module item: {:?}", other),
    }

    match &program.body[2] {
        ModuleItem::Statement(Statement::Declaration(js_ast::Declaration::Function(func))) => {
            assert_eq!(func.name.name, "greet");
            assert_eq!(func.params.len(), 1);
            assert_eq!(
                func.params[0]
                    .type_annotation
                    .as_ref()
                    .unwrap()
                    .text
                    .as_str(),
                "string"
            );
            assert_eq!(func.return_type.as_ref().unwrap().text.as_str(), "string");
            match &func.body.body[0] {
                Statement::Return(ret) => {
                    let expr = ret.argument.as_ref().expect("missing return expression");
                    match expr {
                        Expression::Binary(binary) => {
                            assert!(matches!(binary.operator, BinaryOperator::Add));
                        }
                        other => panic!("unexpected return expression: {:?}", other),
                    }
                }
                other => panic!("unexpected function statement: {:?}", other),
            }
        }
        other => panic!("unexpected module item: {:?}", other),
    }

    let serialized = program_to_value(&program).expect("serialize program");
    assert_eq!(
        serialized
            .get("body")
            .and_then(|body| body.as_array())
            .map(|items| items.len()),
        Some(3)
    );
}

#[test]
fn parses_simple_tsx() {
    let source = r#"
        const element = <Button disabled={disabled}>{label}</Button>;
    "#;

    let program = parse_program(source).expect("failed to parse");
    assert_eq!(program.body.len(), 1);

    match &program.body[0] {
        ModuleItem::Statement(Statement::Declaration(js_ast::Declaration::Variable(var))) => {
            let declarator = &var.declarations[0];
            let init = declarator.init.as_ref().expect("expected initializer");
            match init {
                Expression::JsxElement(element) => {
                    match &element.name {
                        js_ast::JsxName::Identifier(id) => assert_eq!(id.name, "Button"),
                        other => panic!("unexpected element name: {:?}", other),
                    }
                    assert_eq!(element.attributes.len(), 1);
                    assert_eq!(element.children.len(), 1);
                }
                other => panic!("unexpected initializer: {:?}", other),
            }
        }
        other => panic!("unexpected module item: {:?}", other),
    }

    let serialized = program_to_value(&program).expect("serialize tsx");
    assert_eq!(
        serialized
            .get("body")
            .and_then(|body| body.as_array())
            .map(|items| items.len()),
        Some(1)
    );
}

#[test]
fn visitor_collects_identifiers() {
    let source = r#"
        import { createContext } from "react";
        const count = 1;
        function increment(value: number) {
            return value + count;
        }
        const element = <Counter value={count} />;
    "#;

    let program = parse_program(source).expect("failed to parse");

    #[derive(Default)]
    struct IdentifierCollector {
        names: Vec<String>,
    }

    impl Visitor for IdentifierCollector {
        fn visit_identifier(&mut self, identifier: &Identifier) {
            self.names.push(identifier.name.clone());
        }
    }

    let mut collector = IdentifierCollector::default();
    collector.visit_program(&program);

    assert!(collector.names.contains(&"createContext".to_string()));
    assert!(collector.names.contains(&"count".to_string()));
    assert!(collector.names.contains(&"increment".to_string()));
    assert!(collector.names.contains(&"Counter".to_string()));
}

#[test]
fn parses_default_export_expression() {
    let source = r#"
        export default {
            value: 1,
            nested: { enabled: true }
        };
    "#;

    let program = parse_program(source).expect("failed to parse");
    assert_eq!(program.body.len(), 1);

    match &program.body[0] {
        ModuleItem::Export(ExportDeclaration::DefaultExpression { expression, .. }) => {
            match expression {
                Expression::Object(object) => {
                    assert_eq!(object.properties.len(), 2);
                }
                other => panic!("unexpected default export expression: {:?}", other),
            }
        }
        other => panic!("unexpected module item: {:?}", other),
    }
}
