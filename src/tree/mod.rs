pub mod attlist_decl;
pub mod attribute;
pub mod cdata_section;
pub mod comment;
pub mod convert;
pub mod document;
pub mod document_fragment;
pub mod document_type;
pub mod element;
pub mod element_decl;
pub mod entity_decl;
pub mod entity_reference;
pub mod namespace;
pub mod node;
pub mod notation_decl;
pub mod processing_instruction;
pub mod text;

pub use attlist_decl::AttlistDecl;
pub use attribute::Attribute;
pub use cdata_section::CDATASection;
pub use comment::Comment;
pub use document::Document;
pub use document_fragment::DocumentFragment;
pub use document_type::DocumentType;
pub use element::Element;
pub use element_decl::ElementDecl;
pub use entity_decl::EntityDecl;
pub use entity_reference::EntityReference;
pub use node::Node;
pub use notation_decl::NotationDecl;
pub use processing_instruction::ProcessingInstruction;
pub use text::Text;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeType {
    Element = 1,
    Attribute = 2,
    Text = 3,
    CDATASection = 4,
    EntityReference = 5,
    EntityDecl = 6,
    ProcessingInstruction = 7,
    Comment = 8,
    Document = 9,
    DocumentType = 10,
    DocumentFragment = 11,
    NotationDecl = 12,

    ElementDecl = 128,
    AttlistDecl = 129,
    Namespace = 130,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XMLTreeError {
    EmptyPrefix,
    UnacceptablePrefix,
    UnresolvablePrefix,
    AlreadyBoundPrefix,
    UnacceptableNamespaceBinding,
    UnacceptableNamespaceName,
    DuplicateAttribute,
}
