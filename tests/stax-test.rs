use std::{fmt::Write as _, fs::read_dir, path::Path};

use anyxml::stax::{XMLStreamReaderBuilder, events::XMLEvent};
use anyxml_uri::uri::URIString;

#[test]
fn well_formed_tests() {
    for ent in read_dir("resources/well-formed").unwrap() {
        if let Ok(ent) = ent
            && ent.metadata().unwrap().is_file()
        {
            let path = ent.path();
            let uri = URIString::parse_file_path(path.canonicalize().unwrap()).unwrap();
            let mut reader = XMLStreamReaderBuilder::new().build();
            reader.parse_uri(&uri, None).unwrap();
            let mut buffer = String::new();
            loop {
                match reader.next_event().unwrap() {
                    XMLEvent::StartDocument => writeln!(buffer, "startDocument()").unwrap(),
                    XMLEvent::EndDocument => writeln!(buffer, "endDocument()").unwrap(),
                    XMLEvent::StartElement(start) => {
                        write!(
                            buffer,
                            "startElement({}, {}, {}, {}",
                            start.namespace_uri().unwrap_or("None"),
                            start.prefix().unwrap_or("None"),
                            start.local_name().unwrap_or("None"),
                            start.name()
                        )
                        .unwrap();

                        for att in start.attributes() {
                            write!(buffer, ", ").ok();
                            if let Some(local_name) = att.local_name.as_deref() {
                                if let Some(uri) = att.uri.as_deref() {
                                    write!(buffer, "{{{uri}}}").ok();
                                }
                                write!(buffer, "{local_name}='{}'", att.value).ok();
                            } else {
                                write!(buffer, "{}='{}'", att.qname, att.value).ok();
                            }
                        }
                        writeln!(buffer, ")").ok();
                    }
                    XMLEvent::EndElement(end) => {
                        writeln!(
                            buffer,
                            "endElement({}, {}, {}, {})",
                            end.namespace_uri().unwrap_or("None"),
                            end.prefix().unwrap_or("None"),
                            end.local_name().unwrap_or("None"),
                            end.name()
                        )
                        .unwrap();
                    }
                    XMLEvent::Declaration(declaration) => {
                        write!(
                            buffer,
                            "declaration({}, {}, ",
                            declaration.version(),
                            declaration.encoding().unwrap_or("None")
                        )
                        .ok();
                        if let Some(standalone) = declaration.standalone() {
                            if standalone {
                                writeln!(buffer, "yes)").ok();
                            } else {
                                writeln!(buffer, "no)").ok();
                            }
                        } else {
                            writeln!(buffer, "None)").ok();
                        }
                    }
                    XMLEvent::DocumentType => writeln!(buffer, "documentType()").unwrap(),
                    XMLEvent::Characters(characters) => {
                        writeln!(buffer, "characters({characters})").unwrap()
                    }
                    XMLEvent::CDATASection(cdata) => {
                        writeln!(buffer, "cdataSection({cdata})").unwrap()
                    }
                    XMLEvent::Space(space) => writeln!(buffer, "space({space})").unwrap(),
                    XMLEvent::Comment(comment) => writeln!(buffer, "comment({comment})").unwrap(),
                    XMLEvent::ProcessingInstruction(pi) => writeln!(
                        buffer,
                        "processingInstruction({}, '{}')",
                        pi.target(),
                        pi.data().unwrap_or("None")
                    )
                    .unwrap(),
                    XMLEvent::StartEntity(entity) => {
                        writeln!(buffer, "startEntity({entity})").unwrap()
                    }
                    XMLEvent::EndEntity => writeln!(buffer, "endEntity()").unwrap(),
                    XMLEvent::Finished => break,
                }
            }
            let outname = path.file_name().unwrap().to_str().unwrap();
            let outname = format!("resources/well-formed/output/{outname}.stax");
            let outname = Path::new(outname.as_str());
            let output = std::fs::read_to_string(outname).unwrap();

            assert_eq!(output, buffer, "uri: {}\n{}", uri.as_escaped_str(), buffer,);
        }
    }
}
