use std::{
    fs::File,
    io::{BufReader, Read},
};

use anyxml::{
    error::{XMLError, XMLErrorDomain},
    relaxng::RelaxNGSchema,
    sax::{
        error::SAXParseError,
        handler::{DebugHandler, DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
        parser::{ParserOption, XMLReaderBuilder},
        source::InputSource,
    },
    stax::{XMLStreamReaderBuilder, events::XMLEvent},
    tree::TreeBuildHandler,
    uri::{URIStr, URIString},
    xpath::{XPathObject, evaluate_reader, evaluate_uri},
};
use clap::Parser;

#[derive(clap::Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(clap::Subcommand, Debug)]
enum Command {
    Show {
        document: Option<String>,
    },
    Inspect {
        #[clap(long, help = "parser mode for inspection")]
        mode: InspectMode,
        document: Option<String>,
    },
    Validate {
        #[clap(long, help = "validation scheme")]
        mode: ValidationScheme,
        #[clap(long, help = "Path to the schema (unsupported for DTD)")]
        schema: Option<String>,
        document: Option<String>,
    },
    #[clap(name = "xpath")]
    XPath {
        // TODO:
        // #[clap(long, num_args(1..=1))]
        // var: Option<Vec<String>>,
        xpath: String,
        document: Option<String>,
    },
}

#[derive(clap::ValueEnum, Debug, Clone, Copy)]
pub enum InspectMode {
    #[clap(name = "sax")]
    SAX,
    #[clap(name = "sax-progressive")]
    SAXProgressive,
    #[clap(name = "stax")]
    StAX,
}

#[derive(clap::ValueEnum, Debug, Clone, Copy)]
pub enum ValidationScheme {
    #[clap(name = "dtd")]
    DTD,
    #[clap(name = "relaxng")]
    RELAXNG,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    match cli.command {
        Command::Show { document } => do_show_command(document)?,
        Command::Inspect { mode, document } => do_inspect_command(mode, document)?,
        Command::Validate {
            mode,
            schema,
            document,
        } => do_validate_command(mode, schema, document)?,
        Command::XPath { xpath, document } => do_xpath_command(xpath, document)?,
    }
    Ok(())
}

fn do_show_command(document: Option<String>) -> Result<(), XMLError> {
    let handler = TreeBuildHandler::default();
    let mut reader = XMLReaderBuilder::new().set_handler(handler).build();
    if let Some(document) = document {
        let uri = URIString::parse(document)?;
        reader.parse_uri(uri, None)?;
    } else {
        let stdin = std::io::stdin();
        let lock = stdin.lock();
        reader.parse_reader(lock, None, None)?;
    }

    if reader.handler.fatal_error {
        eprintln!("unknown fatal error occured.");
        return Err(XMLError::ParserUnknownError);
    }

    let document = reader.handler.document;
    println!("{document}");
    Ok(())
}

fn do_inspect_command(mode: InspectMode, document: Option<String>) -> Result<(), XMLError> {
    match mode {
        InspectMode::SAX => {
            let mut reader = XMLReaderBuilder::new()
                .set_handler(DebugHandler::default())
                .build();
            if let Some(document) = document {
                let uri = URIString::parse(document)?;
                reader.parse_uri(uri, None)?;
            } else {
                let stdin = std::io::stdin();
                let lock = stdin.lock();
                reader.parse_reader(lock, None, None)?;
            }

            print!("{}", reader.handler.buffer);
        }
        InspectMode::SAXProgressive => {
            let mut reader = XMLReaderBuilder::new()
                .set_handler(DebugHandler::default())
                .progressive_parser()
                .build();

            let source: Box<dyn std::io::Read> = if let Some(document) = document {
                let uri = URIString::parse_file_path(document.as_str())
                    .or_else(|_| URIString::parse(document))?;
                let default_base_uri = reader.default_base_uri()?;
                reader.set_default_base_uri(default_base_uri.resolve(&uri))?;

                let uri = reader.default_base_uri()?;
                let path = uri.path();
                Box::new(File::open(path)?)
            } else {
                Box::new(std::io::stdin().lock())
            };
            let source = BufReader::new(source);

            for byte in source.bytes() {
                reader.parse_chunk([byte?], false)?;
                if !reader.handler.buffer.is_empty() {
                    print!("{}", reader.handler.buffer);
                    reader.handler.buffer.clear();
                }
            }
            reader.parse_chunk([], true)?;
            print!("{}", reader.handler.buffer);
        }
        InspectMode::StAX => {
            let mut reader = XMLStreamReaderBuilder::new().build();
            if let Some(document) = document {
                let uri = URIString::parse(document)?;
                reader.parse_uri(uri, None)?;
            } else {
                let stdin = std::io::stdin();
                let lock = stdin.lock();
                reader.parse_reader(lock, None, None)?;
            }

            loop {
                match reader.next_event()? {
                    XMLEvent::StartDocument => println!("startDocument()"),
                    XMLEvent::EndDocument => println!("endDocument()"),
                    XMLEvent::StartElement(start) => {
                        print!(
                            "startElement({}, {}, {}, {}",
                            start.namespace_uri().unwrap_or("None"),
                            start.prefix().unwrap_or("None"),
                            start.local_name().unwrap_or("None"),
                            start.name()
                        );

                        for att in start.attributes() {
                            print!(", ");
                            if let Some(local_name) = att.local_name.as_deref() {
                                if let Some(uri) = att.namespace_name.as_deref() {
                                    print!("{{{uri}}}");
                                }
                                print!("{local_name}='{}'", att.value);
                            } else {
                                print!("{}='{}'", att.qname, att.value);
                            }
                        }
                        println!(")");
                    }
                    XMLEvent::EndElement(end) => {
                        println!(
                            "endElement({}, {}, {}, {})",
                            end.namespace_uri().unwrap_or("None"),
                            end.prefix().unwrap_or("None"),
                            end.local_name().unwrap_or("None"),
                            end.name()
                        );
                    }
                    XMLEvent::Declaration(declaration) => {
                        print!(
                            "declaration({}, {}, ",
                            declaration.version(),
                            declaration.encoding().unwrap_or("None")
                        );
                        if let Some(standalone) = declaration.standalone() {
                            if standalone {
                                println!("yes)");
                            } else {
                                println!("no)");
                            }
                        } else {
                            println!("None)");
                        }
                    }
                    XMLEvent::DocumentType => println!("documentType()"),
                    XMLEvent::Characters(characters) => {
                        println!("characters({characters})")
                    }
                    XMLEvent::CDATASection(cdata) => println!("cdataSection({cdata})"),
                    XMLEvent::Space(space) => println!("space({space})"),
                    XMLEvent::Comment(comment) => println!("comment({comment})"),
                    XMLEvent::ProcessingInstruction(pi) => println!(
                        "processingInstruction({}, '{}')",
                        pi.target(),
                        pi.data().unwrap_or("None")
                    ),
                    XMLEvent::StartEntity(entity) => println!("startEntity({entity})"),
                    XMLEvent::EndEntity => println!("endEntity()"),
                    XMLEvent::FatalError => println!("fatalError()"),
                    XMLEvent::Finished => break,
                }
            }
        }
    }
    Ok(())
}

fn do_validate_command(
    mode: ValidationScheme,
    schema: Option<String>,
    document: Option<String>,
) -> Result<(), XMLError> {
    match mode {
        ValidationScheme::DTD => {
            if schema.is_some() {
                eprintln!("'--schema' flag is unsupported for DTD.");
                return Err(XMLError::UnsupportedError);
            }

            struct DTDValidationHandler {
                error: bool,
            }

            impl SAXHandler for DTDValidationHandler {}
            impl EntityResolver for DTDValidationHandler {
                fn get_external_subset(
                    &mut self,
                    name: &str,
                    base_uri: Option<&URIStr>,
                ) -> Result<InputSource<'static>, XMLError> {
                    DefaultSAXHandler.get_external_subset(name, base_uri)
                }
                fn resolve_entity(
                    &mut self,
                    name: &str,
                    public_id: Option<&str>,
                    base_uri: &URIStr,
                    system_id: &URIStr,
                ) -> Result<InputSource<'static>, XMLError> {
                    DefaultSAXHandler.resolve_entity(name, public_id, base_uri, system_id)
                }
            }
            impl ErrorHandler for DTDValidationHandler {
                fn fatal_error(&mut self, error: SAXParseError) {
                    self.error = true;
                    DefaultSAXHandler.fatal_error(error);
                }
                fn error(&mut self, error: SAXParseError) {
                    self.error |= matches!(error.domain, XMLErrorDomain::DTDValid);
                    DefaultSAXHandler.error(error);
                }
                fn warning(&mut self, error: SAXParseError) {
                    DefaultSAXHandler.warning(error);
                }
            }

            let mut reader = XMLReaderBuilder::new()
                .enable_option(ParserOption::Validation)
                .set_handler(DTDValidationHandler { error: false })
                .build();
            if let Some(document) = document {
                let uri = URIString::parse(document.as_str())?;
                reader.parse_uri(uri, None)?;
                if reader.handler.error {
                    eprintln!("validation of {} is failed.", document);
                } else {
                    eprintln!("{} is successfully validated", document);
                }
            } else {
                let stdin = std::io::stdin();
                let lock = stdin.lock();
                reader.parse_reader(lock, None, None)?;
                if reader.handler.error {
                    eprintln!("validation of a document from stdin is failed.");
                } else {
                    eprintln!("successfully validate a document from stdin");
                }
            }
        }
        ValidationScheme::RELAXNG => {
            let Some(schema) = schema else {
                eprintln!("'--schema' flag is missing for RELAX NG validation.");
                return Err(XMLError::UnsupportedError);
            };

            let schema_uri = URIString::parse(schema)?;
            let schema = RelaxNGSchema::parse_uri(schema_uri, None, None::<DefaultSAXHandler>)?;

            let mut reader = XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::default())
                .build();
            let document = if let Some(document) = document {
                let uri = URIString::parse(document)?;
                reader.parse_uri(uri, None)?;
                reader.handler.document
            } else {
                let stdin = std::io::stdin();
                let lock = stdin.lock();
                reader.parse_reader(lock, None, None)?;
                reader.handler.document
            };

            let Some(document_element) = document.document_element() else {
                eprintln!("Failed to parse the document because of unknown errors.");
                return Err(XMLError::InternalError);
            };

            schema
                .validate(&document_element)
                .inspect(|_| eprintln!("successfully validate a document."))
                .inspect_err(|err| {
                    eprintln!("failed to validate a document because of '{err:?}'")
                })?
        }
    }
    Ok(())
}

fn do_xpath_command(xpath: String, document: Option<String>) -> Result<(), XMLError> {
    let ret = if let Some(document) = document {
        let uri = URIString::parse(document)?;
        evaluate_uri(&xpath, uri, None)?
    } else {
        let stdin = std::io::stdin();
        let lock = stdin.lock();
        evaluate_reader(&xpath, lock, None, None)?
    };

    match ret {
        XPathObject::Boolean(boolean) => println!("{boolean}"),
        XPathObject::Number(number) => println!("{number}"),
        XPathObject::String(string) => println!("{string}"),
        XPathObject::NodeSet(nodeset) => {
            for node in &nodeset {
                println!("{node}");
            }
        }
    }
    Ok(())
}
