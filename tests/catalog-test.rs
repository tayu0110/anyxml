use anyxml::{
    catalog::{Catalog, CatalogEntryFile, PreferMode},
    sax::{
        handler::{DebugHandler, DefaultSAXHandler},
        parser::{ParserConfig, ParserOption, XMLReaderBuilder},
    },
    tree::TreeBuildHandler,
    uri::URIString,
    xpath::{evaluate, evaluate_uri},
};

#[test]
fn catalog_resolution_tests() {
    for test_cases in evaluate_uri(
        "/test-suite/test-cases",
        URIString::parse("resources/catalog/testsuite.xml").unwrap(),
        None,
    )
    .unwrap()
    .as_nodeset()
    .unwrap()
    .iter()
    {
        let mut catalog = Catalog::default();
        let catalog_uri = URIString::parse(
            test_cases
                .as_element()
                .unwrap()
                .get_attribute("catalog", None)
                .unwrap(),
        )
        .unwrap();
        let catalog_entry_file = CatalogEntryFile::parse_uri(
            &catalog_uri,
            None,
            None::<DefaultSAXHandler>,
            None::<DefaultSAXHandler>,
        )
        .unwrap();
        catalog.add(catalog_entry_file);

        let mut test_cases = test_cases.first_child();
        while let Some(test_case) = test_cases {
            test_cases = test_case.next_sibling();

            if let Some(test_case) = test_case.as_element() {
                let prefer_mode = test_case
                    .get_attribute("prefer", None)
                    .map(|prefer| {
                        if prefer == "public" {
                            PreferMode::Public
                        } else {
                            PreferMode::System
                        }
                    })
                    .unwrap_or_default();
                let uri = test_case
                    .get_attribute("type", None)
                    .is_some_and(|ty| ty == "uri");
                let mut system_id = None;
                let mut public_id = None;
                let mut expected = "".to_owned();
                let mut unresolvable = false;
                let mut children = test_case.first_child();
                while let Some(child) = children {
                    children = child.next_sibling();
                    if let Some(child) = child.as_element() {
                        match child.name().as_ref() {
                            "system-id" => {
                                system_id = Some(URIString::parse(child.text_content()).unwrap())
                            }
                            "public-id" => public_id = Some(child.text_content()),
                            "output" => {
                                expected = child.text_content();
                                unresolvable = child
                                    .get_attribute("unresolvable", None)
                                    .is_some_and(|val| val == "yes");
                            }
                            _ => {}
                        }
                    }
                }

                let resolved = if uri {
                    catalog.resolve_uri(system_id.as_deref().unwrap())
                } else {
                    catalog.resolve_external_id(
                        public_id.as_deref(),
                        system_id.as_deref(),
                        prefer_mode,
                    )
                };

                if unresolvable {
                    assert!(resolved.is_none());
                } else {
                    let resolved = resolved.unwrap_or_else(|| {
                        panic!("Failed to resolve correct URI: catalog: {}\nsystem_id: {:?}, public_id: {:?}, prefer: {:?}, type: {}\nexpected: {}",
                            catalog_uri.as_unescaped_str().unwrap(),
                            system_id,
                            public_id,
                            prefer_mode,
                            if uri { "uri" } else { "external-id" },
                            expected
                    )});
                    let output = evaluate_uri("/root/text()", resolved.as_ref(), None)
                        .unwrap()
                        .as_string()
                        .unwrap();
                    assert_eq!(
                        expected,
                        &*output,
                        "Failed to get correct document: catalog: {}\nresolved: {}\nsystem_id: {:?}, public_id: {:?}, prefer: {:?}, type: {}\nexpected: {}, output: {}",
                        catalog_uri.as_unescaped_str().unwrap(),
                        resolved.as_unescaped_str().unwrap(),
                        system_id,
                        public_id,
                        prefer_mode,
                        if uri { "uri" } else { "external-id" },
                        expected,
                        output
                    );
                }
            }
        }
    }
}

#[test]
fn catalog_resolution_for_entities_tests() {
    for test_cases in evaluate_uri(
        "/test-suite/test-cases",
        URIString::parse("resources/catalog/testsuite.xml").unwrap(),
        None,
    )
    .unwrap()
    .as_nodeset()
    .unwrap()
    .iter()
    {
        let catalog_uri = URIString::parse(
            test_cases
                .as_element()
                .unwrap()
                .get_attribute("catalog", None)
                .unwrap(),
        )
        .unwrap();
        let catalog_entry_file = CatalogEntryFile::parse_uri(
            &catalog_uri,
            None,
            None::<DefaultSAXHandler>,
            None::<DefaultSAXHandler>,
        )
        .unwrap();
        let mut reader = XMLReaderBuilder::new()
            .set_handler(TreeBuildHandler::with_handler(DebugHandler::default()))
            .set_parser_config(
                ParserConfig::default()
                    | ParserOption::Catalogs
                    | ParserOption::ExternalGeneralEntities,
            )
            .build();
        reader.add_catalog_entry_file(catalog_entry_file);

        let mut test_cases = test_cases.first_child();
        while let Some(test_case) = test_cases {
            test_cases = test_case.next_sibling();

            if let Some(test_case) = test_case.as_element() {
                if test_case
                    .get_attribute("prefer", None)
                    .is_some_and(|prefer| prefer == "system")
                {
                    continue;
                }
                if test_case
                    .get_attribute("type", None)
                    .is_some_and(|ty| ty == "uri")
                {
                    continue;
                }
                let mut system_id = None;
                let mut public_id = None;
                let mut expected = "".to_owned();
                let mut unresolvable = false;
                let mut children = test_case.first_child();
                while let Some(child) = children {
                    children = child.next_sibling();
                    if let Some(child) = child.as_element() {
                        match child.name().as_ref() {
                            "system-id" => {
                                system_id = Some(URIString::parse(child.text_content()).unwrap())
                            }
                            "public-id" => public_id = Some(child.text_content()),
                            "output" => {
                                expected = child.text_content();
                                unresolvable = child
                                    .get_attribute("unresolvable", None)
                                    .is_some_and(|val| val == "yes");
                            }
                            _ => {}
                        }
                    }
                }

                let Some(system_id) = system_id else {
                    continue;
                };
                let xml = if let Some(public_id) = public_id.as_deref() {
                    format!(
                        "<!DOCTYPE doc [<!ENTITY ent PUBLIC '{public_id}' '{}'>]><doc>&ent;</doc>",
                        system_id.as_unescaped_str().unwrap()
                    )
                } else {
                    format!(
                        "<!DOCTYPE doc [<!ENTITY ent SYSTEM '{}'>]><doc>&ent;</doc>",
                        system_id.as_unescaped_str().unwrap()
                    )
                };
                reader.reset().unwrap();
                reader.parse_str(&xml, None).unwrap();
                eprintln!("{}", reader.handler.handler.buffer);
                let output = evaluate("//text()[name(..)='root']", reader.handler.document.clone());

                if unresolvable {
                    assert!(output.is_err());
                } else {
                    let resolved = output.unwrap_or_else(|_| {
                        panic!("Failed to resolve correct URI: catalog: {}\nsystem_id: {:?}, public_id: {:?}\nexpected: {}",
                            catalog_uri.as_unescaped_str().unwrap(),
                            system_id,
                            public_id,
                            expected
                    )});
                    let output = resolved.as_string().unwrap();
                    assert_eq!(
                        expected,
                        &*output,
                        "Failed to get correct document: catalog: {}\nsystem_id: {:?}, public_id: {:?}\nexpected: {}, output: {}",
                        catalog_uri.as_unescaped_str().unwrap(),
                        system_id,
                        public_id,
                        expected,
                        output
                    );
                }
            }
        }
    }
}
