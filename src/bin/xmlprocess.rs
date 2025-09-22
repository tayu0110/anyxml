use std::{fmt::Write as _, path::PathBuf};

use anyxml::{
    error::XMLError,
    sax::{
        handler::DebugHandler,
        parser::{ParserOption, XMLReaderBuilder},
    },
    stax::{XMLStreamReaderBuilder, events::XMLEvent},
};
use anyxml_uri::uri::URIString;
use clap::Parser;

#[derive(clap::Parser)]
struct CmdArgs {
    #[clap(long, help = "inspect with SAX API")]
    sax: bool,
    #[clap(long, help = "inspect with StAX API")]
    stax: bool,
    #[clap(long, help = "validate using DTD")]
    dtd_valid: bool,
    #[clap(long, help = "disable namespace handling")]
    no_namespace: bool,
    #[clap(help = "path to the target XML document")]
    file: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = CmdArgs::parse();

    if !args.stax {
        let mut builder = XMLReaderBuilder::new().set_handler(DebugHandler::default());
        if args.dtd_valid {
            builder = builder.enable_option(ParserOption::Validation);
        }
        if args.no_namespace {
            builder = builder.disable_option(ParserOption::Namespaces);
        }
        let mut reader = builder.build();
        for file in args.file {
            let uri = URIString::parse_file_path(file).map_err(XMLError::from)?;
            reader.parse_uri(uri, None)?;
            if args.sax {
                print!("{}", reader.handler.buffer);
            }
        }
    } else {
        for file in args.file {
            let uri = URIString::parse_file_path(file).map_err(XMLError::from)?;
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
                    XMLEvent::FatalError => writeln!(buffer, "fatalError()").unwrap(),
                    XMLEvent::Finished => break,
                }
            }

            print!("{buffer}");
        }
    }

    Ok(())
}
