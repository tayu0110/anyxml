use std::{fmt::Write as _, fs::read_dir, path::Path};

use anyxml::{
    sax::{
        handler::{DebugHandler, DefaultSAXHandler},
        parser::XMLReaderBuilder,
    },
    tree::{Node, TreeBuildHandler, convert::NodeKind, node::NodeSpec},
};
use anyxml_uri::uri::URIString;

fn walk_tree(out: &mut String, node: impl Into<Node<dyn NodeSpec>>, depth: usize) {
    let node: Node<dyn NodeSpec> = node.into();
    write!(out, "{}", "  ".repeat(depth)).unwrap();
    match node.downcast() {
        NodeKind::Element(element) => {
            writeln!(
                out,
                "Element({:?}, {}, {}, {}, {})",
                element.node_type(),
                element.name(),
                element.local_name(),
                element.namespace_name().as_deref().unwrap_or("None"),
                element.prefix().as_deref().unwrap_or("None")
            )
            .unwrap();
            for att in element.attributes() {
                walk_tree(out, att, depth + 1);
            }
        }
        NodeKind::Attribute(attribute) => {
            writeln!(
                out,
                "Attribute({:?}, {}, {}, {}, {}, '{}')",
                attribute.node_type(),
                attribute.name(),
                attribute.local_name(),
                attribute.namespace_name().as_deref().unwrap_or("None"),
                attribute.prefix().as_deref().unwrap_or("None"),
                attribute.value()
            )
            .unwrap();
        }
        NodeKind::Text(text) => {
            writeln!(out, "Text({:?}, '{}')", text.node_type(), text.data()).unwrap();
        }
        NodeKind::CDATASection(cdata) => {
            writeln!(
                out,
                "CDATASection({:?}, '{}')",
                cdata.node_type(),
                cdata.data()
            )
            .unwrap();
        }
        NodeKind::EntityReference(ent) => {
            writeln!(
                out,
                "EntityReference({:?}, {})",
                ent.node_type(),
                ent.name()
            )
            .unwrap();
        }
        NodeKind::EntityDecl(ent) => {
            writeln!(
                out,
                "EntityDecl({:?}, {}, {}, {}, {}, '{}')",
                ent.node_type(),
                ent.name(),
                ent.system_id()
                    .as_deref()
                    .map(|id| id.as_escaped_str())
                    .unwrap_or("None"),
                ent.public_id().as_deref().unwrap_or("None"),
                ent.notation_name().as_deref().unwrap_or("None"),
                ent.value().as_deref().unwrap_or("None"),
            )
            .unwrap();
        }
        NodeKind::ProcessingInstruction(pi) => {
            writeln!(
                out,
                "ProcessingInstruction({:?}, {}, {})",
                pi.node_type(),
                pi.target(),
                pi.data().as_deref().unwrap_or("None")
            )
            .unwrap();
        }
        NodeKind::Comment(comment) => {
            writeln!(
                out,
                "Comment({:?}, '{}')",
                comment.node_type(),
                comment.data()
            )
            .unwrap();
        }
        NodeKind::Document(document) => {
            writeln!(
                out,
                "Document({:?}, '{}', '{}', {})",
                document.node_type(),
                document.version().as_deref().unwrap_or("None"),
                document.encoding().as_deref().unwrap_or("None"),
                document
                    .standalone()
                    .map_or("None", |sddecl| if sddecl { "yes" } else { "no" })
            )
            .unwrap();
        }
        NodeKind::DocumentType(doctype) => {
            writeln!(
                out,
                "DocumentType({:?}, {}, {}, {})",
                doctype.node_type(),
                doctype.name(),
                doctype
                    .system_id()
                    .as_deref()
                    .map(|uri| uri.as_escaped_str())
                    .unwrap_or("None"),
                doctype.public_id().as_deref().unwrap_or("None")
            )
            .unwrap();
        }
        NodeKind::DocumentFragment(_) => {
            panic!("DocumentFragment must not appear in the document tree.")
        }
        NodeKind::NotationDecl(notation) => {
            writeln!(
                out,
                "NotationDecl({:?}, {}, {}, {})",
                notation.node_type(),
                notation.name(),
                notation
                    .system_id()
                    .as_deref()
                    .map(|uri| uri.as_escaped_str())
                    .unwrap_or("None"),
                notation.public_id().as_deref().unwrap_or("None")
            )
            .unwrap();
        }
        NodeKind::ElementDecl(elemdecl) => {
            writeln!(
                out,
                "ElementDecl({:?}, {}, {})",
                elemdecl.node_type(),
                elemdecl.name(),
                elemdecl.content_spec()
            )
            .unwrap();
        }
        NodeKind::AttlistDecl(attlistdecl) => {
            writeln!(
                out,
                "AttlistDecl({:?}, {}, {}, {}, {})",
                attlistdecl.node_type(),
                attlistdecl.elem_name(),
                attlistdecl.attr_name(),
                attlistdecl.attr_type(),
                attlistdecl.default_decl()
            )
            .unwrap();
        }
        NodeKind::Namespace(namespace) => {
            writeln!(
                out,
                "Namespace({:?}, {}, {})",
                namespace.node_type(),
                namespace.prefix().as_deref().unwrap_or("None"),
                namespace.namespace_name()
            )
            .unwrap();
        }
    }
    if let Some(first_child) = node.first_child() {
        walk_tree(out, first_child, depth + 1);
    }
    if let Some(next_sibling) = node.next_sibling() {
        walk_tree(out, next_sibling, depth);
    }
}

#[test]
fn tree_walk_tests() {
    for ent in read_dir("resources/well-formed").unwrap() {
        if let Ok(ent) = ent
            && ent.metadata().unwrap().is_file()
        {
            let path = ent.path();
            let uri = URIString::parse_file_path(path.canonicalize().unwrap()).unwrap();
            let handler = TreeBuildHandler::with_handler(DebugHandler {
                child: DefaultSAXHandler,
                buffer: String::new(),
            });
            let mut reader = XMLReaderBuilder::new().set_handler(handler).build();
            reader.parse_uri(&uri, None).ok();

            let outname = path.file_name().unwrap().to_str().unwrap();
            let outname = format!("resources/well-formed/output/{outname}.sax");
            let outname = Path::new(outname.as_str());
            let output = std::fs::read_to_string(outname).unwrap();

            assert_eq!(
                output,
                reader.handler.handler.buffer,
                "uri: {}\n{}",
                uri.as_escaped_str(),
                reader.handler.handler.buffer,
            );
            assert!(!reader.handler.fatal_error);

            let document = reader.handler.document;
            let mut buf = String::new();
            walk_tree(&mut buf, document, 0);
            let outname = path.file_name().unwrap().to_str().unwrap();
            let outname = format!("resources/well-formed/output/{outname}.tree");
            let outname = Path::new(outname.as_str());
            let output = std::fs::read_to_string(outname).unwrap_or_default();
            assert_eq!(buf, output, "uri: {}\n{}", uri.as_escaped_str(), buf);
        }
    }
}
