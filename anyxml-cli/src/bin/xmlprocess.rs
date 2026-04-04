use std::{
    fs::File,
    io::{BufReader, BufWriter, Read, StdoutLock, Write as _},
};

use anyxml::{
    error::{XMLError, XMLErrorDomain},
    relaxng::RelaxNGSchema,
    sax::{
        AttributeType, DefaultDecl,
        attributes::Attributes,
        error::SAXParseError,
        handler::{DebugHandler, DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
        parser::{ParserOption, XMLReader},
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
    let mut reader = XMLReader::builder().set_handler(handler).build();
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

struct CompactDebugHandler {
    stdout: StdoutLock<'static>,
    child: DebugHandler,
}

impl CompactDebugHandler {
    fn new() -> Self {
        CompactDebugHandler {
            stdout: std::io::stdout().lock(),
            child: DebugHandler::default(),
        }
    }
    fn force_show(&mut self) {
        self.stdout.write_all(self.child.buffer.as_bytes()).ok();
        self.child.buffer.clear();
    }
    fn show(&mut self) {
        if self.child.buffer.len() > 4096 {
            self.force_show();
        }
    }
}

impl EntityResolver for CompactDebugHandler {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child
            .resolve_entity(name, public_id, base_uri, system_id)
    }
    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child.get_external_subset(name, base_uri)
    }
}
impl ErrorHandler for CompactDebugHandler {
    fn fatal_error(&mut self, error: SAXParseError) {
        self.child.fatal_error(error);
    }
    fn error(&mut self, error: SAXParseError) {
        self.child.error(error);
    }
    fn warning(&mut self, error: SAXParseError) {
        self.child.warning(error);
    }
}
impl SAXHandler for CompactDebugHandler {
    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        self.child
            .attribute_decl(element_name, attribute_name, attribute_type, default_decl);
        self.show();
    }
    fn characters(&mut self, data: &str) {
        self.child.characters(data);
        self.show();
    }
    fn comment(&mut self, data: &str) {
        self.child.comment(data);
        self.show();
    }
    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.child.declaration(version, encoding, standalone);
    }
    fn element_decl(&mut self, name: &str, contentspec: &anyxml::sax::contentspec::ContentSpec) {
        self.child.element_decl(name, contentspec);
        self.show();
    }
    fn end_cdata(&mut self) {
        self.child.end_cdata();
        self.show();
    }
    fn end_document(&mut self) {
        self.child.end_document();
        self.show();
    }
    fn end_dtd(&mut self) {
        self.child.end_dtd();
        self.show();
    }
    fn end_element(&mut self, namespace_name: Option<&str>, local_name: Option<&str>, qname: &str) {
        self.child.end_element(namespace_name, local_name, qname);
        self.show();
    }
    fn end_entity(&mut self) {
        self.child.end_entity();
        self.show();
    }
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        self.child.end_prefix_mapping(prefix);
        self.show();
    }
    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.child.external_entity_decl(name, public_id, system_id);
        self.show();
    }
    fn ignorable_whitespace(&mut self, data: &str) {
        self.child.ignorable_whitespace(data);
        self.show();
    }
    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.child.internal_entity_decl(name, value);
        self.show();
    }
    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.notation_decl(name, public_id, system_id);
        self.show();
    }
    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.child.processing_instruction(target, data);
        self.show();
    }
    fn set_document_locator(&mut self, locator: std::sync::Arc<anyxml::sax::Locator>) {
        self.child.set_document_locator(locator);
    }
    fn skipped_entity(&mut self, name: &str) {
        self.child.skipped_entity(name);
        self.show();
    }
    fn start_cdata(&mut self) {
        self.child.start_cdata();
        self.show();
    }
    fn start_document(&mut self) {
        self.child.start_document();
    }
    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.start_dtd(name, public_id, system_id);
        self.show();
    }
    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        self.child
            .start_element(namespace_name, local_name, qname, atts);
        self.show();
    }
    fn start_entity(&mut self, name: &str) {
        self.child.start_entity(name);
        self.show();
    }
    fn start_prefix_mapping(&mut self, prefix: Option<&str>, uri: &str) {
        self.child.start_prefix_mapping(prefix, uri);
        self.show();
    }
    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        self.child
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
        self.show();
    }
}

fn do_inspect_command(mode: InspectMode, document: Option<String>) -> Result<(), XMLError> {
    match mode {
        InspectMode::SAX => {
            let mut reader = XMLReader::builder()
                .set_handler(CompactDebugHandler::new())
                .build();
            if let Some(document) = document {
                let uri = URIString::parse(document)?;
                reader.parse_uri(uri, None)?;
            } else {
                let stdin = std::io::stdin();
                let lock = stdin.lock();
                reader.parse_reader(lock, None, None)?;
            }

            reader.handler.force_show();
        }
        InspectMode::SAXProgressive => {
            let mut reader = XMLReader::builder()
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
            let mut stdout = BufWriter::new(std::io::stdout().lock());

            for byte in source.bytes() {
                reader.parse_chunk([byte?], false)?;
                if !reader.handler.buffer.is_empty() {
                    write!(stdout, "{}", reader.handler.buffer)?;
                    reader.handler.buffer.clear();
                }
            }
            reader.parse_chunk([], true)?;
            write!(stdout, "{}", reader.handler.buffer)?;
            stdout.flush()?;
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
                            start.namespace_name().unwrap_or("None"),
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
                            end.namespace_name().unwrap_or("None"),
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

            let mut reader = XMLReader::builder()
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
            let mut schema = RelaxNGSchema::parse_uri(schema_uri, None, None::<DefaultSAXHandler>)?;
            let validator = schema.new_validate_handler(DefaultSAXHandler);
            let mut reader = XMLReader::builder().set_handler(validator).build();
            if let Some(document) = document {
                let uri = URIString::parse(document)?;
                reader.parse_uri(uri, None)?;
            } else {
                let stdin = std::io::stdin();
                let lock = stdin.lock();
                reader.parse_reader(lock, None, None)?;
            }

            reader
                .handler
                .last_error
                .inspect(|_| eprintln!("successfully validate a document."))
                .inspect_err(|err| eprintln!("failed to validate a document because of '{err:?}'"))
                .map_err(|e| e.error)?
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
