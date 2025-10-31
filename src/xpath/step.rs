use std::collections::HashMap;

use crate::{
    XML_XML_NAMESPACE,
    tree::{NodeType, namespace::Namespace},
    xpath::{Axis, NodeTest, XPathContext, XPathNodeSet},
};

pub(super) fn location_step(
    context: &XPathContext,
    axis: Axis,
    node_test: &NodeTest,
    node_set: &mut XPathNodeSet,
) {
    let Some(context_node) = context.node.clone() else {
        return;
    };
    match axis {
        axis @ (Axis::Ancestor | Axis::AncestorOrSelf) => {
            if matches!(axis, Axis::AncestorOrSelf)
                && node_test.match_node(&context_node, NodeType::Element, &context.namespaces)
            {
                node_set.push(&context_node);
            }
            let mut parent = context_node.parent_node();
            while let Some(now) = parent {
                parent = now.parent_node();
                if node_test.match_node(&now.clone().into(), NodeType::Element, &context.namespaces)
                {
                    node_set.push(now);
                }
            }
        }
        Axis::Attribute => {
            if let Some(element) = context_node.as_element() {
                for att in element.attributes() {
                    if node_test.match_attribute(&att, true, &context.namespaces) {
                        node_set.push(att);
                    }
                }
            }
        }
        Axis::Child => {
            let mut children = context_node.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                if node_test.match_node(&child, NodeType::Element, &context.namespaces) {
                    node_set.push(child);
                }
            }
        }
        axis @ (Axis::Descendant | Axis::DescendantOrSelf) => {
            if matches!(axis, Axis::DescendantOrSelf)
                && node_test.match_node(&context_node, NodeType::Element, &context.namespaces)
            {
                node_set.push(&context_node);
            }
            let mut children = context_node.first_child();
            while let Some(child) = children {
                if let Some(first) = child.first_child() {
                    children = Some(first);
                } else if let Some(next) = child.next_sibling() {
                    children = Some(next);
                } else {
                    children = None;
                    let mut parent = child.parent_node();
                    while let Some(now) = parent {
                        if context_node.is_same_node(&now) {
                            break;
                        }
                        if let Some(next) = now.next_sibling() {
                            children = Some(next);
                            break;
                        }
                        parent = now.parent_node();
                    }
                }

                if node_test.match_node(&child, NodeType::Element, &context.namespaces) {
                    node_set.push(child);
                }
            }
        }
        Axis::Following => {
            let mut following = match context_node.node_type() {
                NodeType::Attribute | NodeType::Namespace => {
                    context_node.parent_node().and_then(|parent| {
                        if let Some(first) = parent.first_child() {
                            Some(first)
                        } else if let Some(next) = parent.next_sibling() {
                            Some(next)
                        } else {
                            let mut parent = parent.parent_node();
                            while let Some(now) = parent {
                                if let Some(next) = now.next_sibling() {
                                    return Some(next);
                                }
                                parent = now.parent_node();
                            }
                            None
                        }
                    })
                }
                _ => Some(context_node.clone()),
            };
            while let Some(now) = following {
                if !context_node.is_same_node(&now)
                    && let Some(first) = now.first_child()
                {
                    following = Some(first);
                } else if let Some(next) = now.next_sibling() {
                    following = Some(next);
                } else {
                    following = None;
                    let mut parent = now.parent_node();
                    while let Some(now) = parent {
                        if let Some(next) = now.next_sibling() {
                            following = Some(next);
                            break;
                        }
                        parent = now.parent_node();
                    }
                }

                if !context_node.is_same_node(&now)
                    && node_test.match_node(&now, NodeType::Element, &context.namespaces)
                {
                    node_set.push(now);
                }
            }
        }
        Axis::FollowingSibling => {
            let mut sibling = context_node.next_sibling();
            while let Some(now) = sibling {
                sibling = now.next_sibling();
                if node_test.match_node(&now, NodeType::Element, &context.namespaces) {
                    node_set.push(now);
                }
            }
        }
        Axis::Namespace => {
            let mut namespace_map = HashMap::new();
            if let Some(element) = context_node.as_element() {
                for namespace in element.namespaces() {
                    namespace_map.insert(namespace.prefix().unwrap_or_default(), namespace.clone());
                    if node_test.match_namespace(&namespace, true) {
                        node_set.push(namespace);
                    }
                }

                let mut parent = element.parent_node();
                while let Some(now) = parent {
                    parent = now.parent_node();
                    if let Some(elem) = now.as_element() {
                        for namespace in elem.namespaces() {
                            if namespace_map
                                .insert(namespace.prefix().unwrap_or_default(), namespace.clone())
                                .is_none()
                            {
                                // According to the XPath data model, we must collect namespace nodes
                                // whose parent element is the context node (here, `element`).
                                // Therefore, we should not return the namespace node of another
                                // element (here, `elem`), but instead generate a new namespace node.
                                let new = Namespace::new(
                                    namespace.prefix(),
                                    namespace.namespace_name(),
                                    element.clone(),
                                );
                                node_set.push(new);
                            }
                        }
                    }
                }

                if !namespace_map.contains_key("xml") {
                    let new = Namespace::new(
                        Some("xml".into()),
                        XML_XML_NAMESPACE.into(),
                        element.clone(),
                    );
                    node_set.push(new);
                }

                // Since only explicit declarations are collected,
                // we need not scan `context.namespaces`.
            }
        }
        Axis::Parent => {
            if let Some(parent) = context_node.parent_node().map(From::from)
                && node_test.match_node(&parent, NodeType::Element, &context.namespaces)
            {
                node_set.push(parent);
            }
        }
        Axis::Preceding => {
            let mut preceding = Some(context_node.clone());
            while let Some(now) = preceding {
                if !context_node.is_same_node(&now)
                    && let Some(last) = now.last_child()
                {
                    preceding = Some(last);
                } else if let Some(previous) = now.previous_sibling() {
                    preceding = Some(previous);
                } else {
                    preceding = None;
                    let mut parent = now.parent_node();
                    while let Some(now) = parent {
                        if let Some(previous) = now.previous_sibling() {
                            preceding = Some(previous);
                            break;
                        }
                        parent = now.parent_node();
                    }
                }

                if !context_node.is_same_node(&now)
                    && node_test.match_node(&now, NodeType::Element, &context.namespaces)
                {
                    node_set.push(now);
                }
            }
        }
        Axis::PrecedingSibling => {
            let mut sibling = context_node.previous_sibling();
            while let Some(now) = sibling {
                sibling = now.previous_sibling();
                if node_test.match_node(&now, NodeType::Element, &context.namespaces) {
                    node_set.push(now);
                }
            }
        }
        Axis::SelfNode => {
            if node_test.match_node(&context_node, NodeType::Element, &context.namespaces) {
                node_set.push(context_node);
            }
        }
    }
}
