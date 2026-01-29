use std::{collections::HashMap, path::Path, rc::Rc};

use anyxml::{
    sax::parser::XMLReaderBuilder,
    tree::{Document, Element, Node, TreeBuildHandler, convert::NodeKind, node::NodeSpec},
    uri::{URIStr, URIString},
    xinclude::XIncludeProcessor,
    xpath::evaluate_uri,
};

const SUPPORTED_FEATURES: &[&str] = &["unexpanded-entities", "unparsed-entities", "lang-fixup"];
const IGNORED_TESTCASE: &[&str] = &[
    // EBCDIC encodings are not supported yet
    // "harold-79",
    // HTTP resource fetching is necessary
    "harold-87",
    // HTTP resource fetching is necessary
    "harold-88",
    // HTTP resource fetching is necessary
    "harold-89",
    // HTTP resource fetching is necessary
    "harold-90",
];

#[test]
fn testsuite() {
    let uri = URIString::parse_file_path(
        Path::new("resources/XIncl20060927/testdescr.xml")
            .canonicalize()
            .unwrap(),
    )
    .unwrap();

    let testcases = evaluate_uri("//testcases", uri.as_ref(), None)
        .unwrap()
        .as_nodeset()
        .unwrap();

    for testcases in &testcases {
        handle_testcases(&uri, testcases.as_element().unwrap());
    }
}

fn handle_testcases(base_uri: &URIStr, testcases: Element) {
    let basedir = base_uri.resolve(
        &URIString::parse(testcases.get_attribute("basedir", None).unwrap() + "/").unwrap(),
    );

    let creator = testcases.get_attribute("creator", None).unwrap();
    eprintln!("=== start testcases by {} ===", creator);

    for testcase in testcases.get_elements_by_qname("testcase") {
        handle_testcase(&basedir, testcase);
    }
}

fn handle_testcase(base_uri: &URIStr, testcase: Element) {
    let id = testcase.get_attribute("id", None).unwrap();
    let description = testcase
        .get_elements_by_qname("description")
        .next()
        .unwrap()
        .text_content();
    eprintln!("====== {}: {} ======", id, description);

    if IGNORED_TESTCASE.contains(&id.as_str()) {
        eprintln!("skip testcase because using unsupported features.");
        return;
    }

    if let Some(features) = testcase
        .get_attribute("features", None)
        .filter(|f| !f.is_empty())
        && features
            .split_ascii_whitespace()
            .any(|f| !SUPPORTED_FEATURES.contains(&f))
    {
        eprintln!(
            "skip testcase because features '{}' is not supported",
            features
        );
        return;
    }

    let href = testcase.get_attribute("href", None).unwrap();
    let href = base_uri.resolve(&URIString::parse(href).unwrap());
    let test_type = testcase.get_attribute("type", None).unwrap();
    match test_type.as_str() {
        "success" => {
            let mut reader = XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::default())
                .build();
            eprintln!("href: {}", href.as_unescaped_str().unwrap());
            reader.parse_uri(href.as_ref(), None).unwrap();

            let document = reader.handler.document.clone();
            let mut processor = XIncludeProcessor::default();
            let result = processor.process(document).unwrap();

            let output = testcase
                .get_elements_by_qname("output")
                .next()
                .unwrap()
                .text_content();
            let output = base_uri.resolve(&URIString::parse(output).unwrap());
            reader.parse_uri(output, None).unwrap();
            let output = reader.handler.document.clone();

            let result = normalize_tree(result, base_uri);
            let output = normalize_tree(output, base_uri);

            let result = result.document_element().unwrap().to_string();
            let output = output.document_element().unwrap().to_string();
            assert_eq!(
                result,
                output,
                "uri: {}\n\nresult:\n{}\n\nexpected:\n{}",
                href.as_unescaped_str()
                    .as_deref()
                    .unwrap_or(href.as_escaped_str()),
                result,
                output
            );
        }
        "error" => {
            let mut reader = XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::default())
                .build();
            eprintln!("href: {}", href.as_unescaped_str().unwrap());
            reader.parse_uri(href.as_ref(), None).unwrap();

            let document = reader.handler.document.clone();
            let mut processor = XIncludeProcessor::default();
            let result = processor.process(document);
            assert!(result.is_err(), "document: {}", result.unwrap());
        }
        _ => panic!("unknown test type: {test_type}"),
    }
}

fn normalize_tree(mut document: Document, base_uri: &URIStr) -> Document {
    let mut children = document.document_element().map(Node::<dyn NodeSpec>::from);
    while let Some(child) = children {
        if let Some(first) = child.first_child() {
            children = Some(first);
        } else if let Some(next) = child.next_sibling() {
            children = Some(next);
        } else {
            children = None;

            let mut parent = child.parent_node();
            while let Some(par) = parent {
                if let Some(next) = par.next_sibling() {
                    children = Some(next);
                    break;
                }
                parent = par.parent_node();
            }
        }

        match child.downcast() {
            NodeKind::Element(mut child) => {
                let orig_base_uri = document.document_base_uri();
                document.set_document_base_uri(base_uri).unwrap();
                let base_uri = child.base_uri().unwrap().to_string();
                let mut buf = vec![];
                for att in child.attributes() {
                    buf.push((
                        att.name(),
                        att.local_name(),
                        att.namespace_name(),
                        att.value(),
                    ));
                }
                buf.sort_unstable();
                for (_, local_name, namespace_name, _) in &buf {
                    child
                        .remove_attribute(local_name, namespace_name.as_deref())
                        .unwrap();
                }
                for (name, _, namespace_name, mut value) in buf {
                    if name.as_ref() == "xml:base" {
                        value = base_uri.clone();
                    }
                    child
                        .set_attribute(&name, namespace_name.as_deref(), Some(&value))
                        .unwrap();
                }
                document
                    .set_document_base_uri(orig_base_uri.as_ref())
                    .unwrap();
            }
            NodeKind::CDATASection(mut cdata) => {
                let text = document.create_text(&*cdata.data());
                cdata.replace_subtree(text).unwrap();
            }
            _ => {}
        }
    }

    fixup_namespace(&mut document.clone().into(), &mut HashMap::new());
    document
}

fn fixup_namespace(
    node: &mut Node<dyn NodeSpec>,
    mapping: &mut HashMap<Option<Rc<str>>, Vec<Rc<str>>>,
) {
    if let Some(mut element) = node.as_element() {
        let mut removed = vec![];
        for ns in element.namespaces() {
            let namespace_name = ns.namespace_name();
            let entity = mapping.entry(ns.prefix()).or_default();
            if entity.last().is_some_and(|last| **last == *namespace_name) {
                removed.push(ns.prefix());
            } else {
                entity.push(namespace_name);
            }
        }
        for prefix in removed {
            element.undeclare_namespace(prefix.as_deref());
        }
    }
    let mut children = node.first_element_child();
    while let Some(child) = children {
        children = child.next_element_sibling();

        fixup_namespace(&mut child.into(), mapping);
    }

    if let Some(element) = node.as_element() {
        for ns in element.namespaces() {
            mapping.entry(ns.prefix()).or_default().pop();
        }
    }
}
