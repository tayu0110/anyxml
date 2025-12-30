use std::rc::Rc;

use crate::tree::{
    AttlistDecl, Attribute, CDATASection, Comment, Document, DocumentFragment, DocumentType,
    Element, ElementDecl, EntityDecl, EntityReference, Node, NodeType, NotationDecl,
    ProcessingInstruction, Text, XMLTreeError,
    namespace::Namespace,
    node::{InternalNodeSpec, NodeSpec},
};

pub enum NodeKind {
    Element(Element),
    Attribute(Attribute),
    Text(Text),
    CDATASection(CDATASection),
    EntityReference(EntityReference),
    EntityDecl(EntityDecl),
    ProcessingInstruction(ProcessingInstruction),
    Comment(Comment),
    Document(Document),
    DocumentType(DocumentType),
    DocumentFragment(DocumentFragment),
    NotationDecl(NotationDecl),
    ElementDecl(ElementDecl),
    AttlistDecl(AttlistDecl),
    Namespace(Namespace),
}

macro_rules! impl_unwrap_node_kind {
    ($fn:ident, $type:ident) => {
        pub fn $fn(self) -> Option<$type> {
            if let Self::$type(ret) = self {
                Some(ret)
            } else {
                None
            }
        }
    };
}
impl NodeKind {
    impl_unwrap_node_kind!(as_element, Element);
    impl_unwrap_node_kind!(as_attribute, Attribute);
    impl_unwrap_node_kind!(as_text, Text);
    impl_unwrap_node_kind!(as_cdata_section, CDATASection);
    impl_unwrap_node_kind!(as_entity_reference, EntityReference);
    impl_unwrap_node_kind!(as_entity_decl, EntityDecl);
    impl_unwrap_node_kind!(as_processing_instruction, ProcessingInstruction);
    impl_unwrap_node_kind!(as_comment, Comment);
    impl_unwrap_node_kind!(as_document, Document);
    impl_unwrap_node_kind!(as_document_type, DocumentType);
    impl_unwrap_node_kind!(as_document_fragment, DocumentFragment);
    impl_unwrap_node_kind!(as_notation_decl, NotationDecl);
    impl_unwrap_node_kind!(as_element_decl, ElementDecl);
    impl_unwrap_node_kind!(as_attlist_decl, AttlistDecl);
    impl_unwrap_node_kind!(as_namespace, Namespace);
}

impl Node<dyn NodeSpec> {
    pub fn downcast(&self) -> NodeKind {
        let ptr = Rc::into_raw(self.core.clone());

        match self.node_type() {
            NodeType::Element => NodeKind::Element(Element {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::Attribute => NodeKind::Attribute(Attribute {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::Text => NodeKind::Text(Text {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::CDATASection => NodeKind::CDATASection(CDATASection {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::EntityReference => NodeKind::EntityReference(EntityReference {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::EntityDecl => NodeKind::EntityDecl(EntityDecl {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::ProcessingInstruction => {
                NodeKind::ProcessingInstruction(ProcessingInstruction {
                    core: unsafe { Rc::from_raw(ptr as _) },
                    owner_document: self.owner_document.clone(),
                })
            }
            NodeType::Comment => NodeKind::Comment(Comment {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::Document => NodeKind::Document(Document {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::DocumentType => NodeKind::DocumentType(DocumentType {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::DocumentFragment => NodeKind::DocumentFragment(DocumentFragment {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::NotationDecl => NodeKind::NotationDecl(NotationDecl {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::ElementDecl => NodeKind::ElementDecl(ElementDecl {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::AttlistDecl => NodeKind::AttlistDecl(AttlistDecl {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
            NodeType::Namespace => NodeKind::Namespace(Namespace {
                core: unsafe { Rc::from_raw(ptr as _) },
                owner_document: self.owner_document.clone(),
            }),
        }
    }
}

impl Node<dyn InternalNodeSpec> {
    pub fn downcast(&self) -> NodeKind {
        Node::<dyn NodeSpec>::from(self.clone()).downcast()
    }
}

impl TryFrom<Node<dyn NodeSpec>> for Node<dyn InternalNodeSpec> {
    type Error = XMLTreeError;

    fn try_from(value: Node<dyn NodeSpec>) -> Result<Self, Self::Error> {
        match value.downcast() {
            NodeKind::AttlistDecl(_)
            | NodeKind::CDATASection(_)
            | NodeKind::Comment(_)
            | NodeKind::ElementDecl(_)
            | NodeKind::Namespace(_)
            | NodeKind::NotationDecl(_)
            | NodeKind::ProcessingInstruction(_)
            | NodeKind::Text(_) => Err(XMLTreeError::IncompatibleConversion),
            NodeKind::Attribute(attr) => Ok(attr.into()),
            NodeKind::Document(document) => Ok(document.into()),
            NodeKind::DocumentFragment(frag) => Ok(frag.into()),
            NodeKind::DocumentType(doctype) => Ok(doctype.into()),
            NodeKind::Element(elem) => Ok(elem.into()),
            NodeKind::EntityDecl(decl) => Ok(decl.into()),
            NodeKind::EntityReference(ent) => Ok(ent.into()),
        }
    }
}

macro_rules! impl_specific_type_node_conversion {
    ($fn:ident, $type:ident, $( $t:ty ),*) => {
        $(
            impl $t {
                pub fn $fn(&self) -> Option<$type> {
                    self.downcast().$fn()
                }
            }
        )*
    };
}

impl_specific_type_node_conversion! {
    as_element,
    Element,
    Node<dyn NodeSpec>,
    Node<dyn InternalNodeSpec>
}
impl_specific_type_node_conversion! {
    as_attribute,
    Attribute,
    Node<dyn NodeSpec>,
    Node<dyn InternalNodeSpec>
}
impl_specific_type_node_conversion! {
    as_text,
    Text,
    Node<dyn NodeSpec>
}
impl_specific_type_node_conversion! {
    as_cdata_section,
    CDATASection,
    Node<dyn NodeSpec>
}
impl_specific_type_node_conversion! {
    as_entity_reference,
    EntityReference,
    Node<dyn NodeSpec>,
    Node<dyn InternalNodeSpec>
}
impl_specific_type_node_conversion! {
    as_entity_decl,
    EntityDecl,
    Node<dyn NodeSpec>,
    Node<dyn InternalNodeSpec>
}
impl_specific_type_node_conversion! {
    as_processing_instruction,
    ProcessingInstruction,
    Node<dyn NodeSpec>
}
impl_specific_type_node_conversion! {
    as_comment,
    Comment,
    Node<dyn NodeSpec>
}
impl_specific_type_node_conversion! {
    as_document,
    Document,
    Node<dyn NodeSpec>,
    Node<dyn InternalNodeSpec>
}
impl_specific_type_node_conversion! {
    as_document_type,
    DocumentType,
    Node<dyn NodeSpec>,
    Node<dyn InternalNodeSpec>
}
impl_specific_type_node_conversion! {
    as_document_fragment,
    DocumentFragment,
    Node<dyn NodeSpec>,
    Node<dyn InternalNodeSpec>
}
impl_specific_type_node_conversion! {
    as_notation_decl,
    NotationDecl,
    Node<dyn NodeSpec>
}
impl_specific_type_node_conversion! {
    as_element_decl,
    ElementDecl,
    Node<dyn NodeSpec>
}
impl_specific_type_node_conversion! {
    as_attlist_decl,
    AttlistDecl,
    Node<dyn NodeSpec>
}
impl_specific_type_node_conversion! {
    as_namespace,
    Namespace,
    Node<dyn NodeSpec>
}

#[cfg(test)]
mod tests {

    use std::rc::Rc;

    use crate::{
        sax::{AttributeType, DefaultDecl},
        tree::{
            Document, Node,
            convert::NodeKind,
            node::{InternalNodeSpec, NodeSpec},
        },
    };

    macro_rules! convert_and_test {
        ( $document:ident, $create:ident( $( $arg:tt )* ), $revert:ident, $match:tt, $( $to:ty ),* ) => {
            let node = $document.$create($( $arg )*);
            $(
                let converted = <$to>::from(node.clone()).downcast();
                assert!(matches!(converted, NodeKind::$match(_)));
                assert!(Rc::ptr_eq(
                    &node.core,
                    &converted.$revert().unwrap().core
                ));
            )*
        };
    }

    #[test]
    fn node_conversion_tests() {
        let document = Document::new();
        convert_and_test!(
            document,
            clone(),
            as_document,
            Document,
            Node<dyn NodeSpec>,
            Node<dyn InternalNodeSpec>
        );

        convert_and_test!(
            document,
            create_attlist_decl(
                "element",
                "attribute",
                AttributeType::CDATA,
                DefaultDecl::IMPLIED
            ),
            as_attlist_decl,
            AttlistDecl,
            Node<dyn NodeSpec>
        );
        convert_and_test!(
            document,
            create_cdata_section("CDATASection"),
            as_cdata_section,
            CDATASection,
            Node<dyn NodeSpec>
        );
        convert_and_test!(
            document,
            create_comment("Comment"),
            as_comment,
            Comment,
            Node<dyn NodeSpec>
        );
        convert_and_test!(
            document,
            create_document_fragment(),
            as_document_fragment,
            DocumentFragment,
            Node<dyn NodeSpec>,
            Node<dyn InternalNodeSpec>
        );
        convert_and_test!(
            document,
            create_document_type("dtd", None, None),
            as_document_type,
            DocumentType,
            Node<dyn NodeSpec>,
            Node<dyn InternalNodeSpec>
        );
    }
}
