use std::path::PathBuf;

use anyxml::{
    error::XMLError,
    sax::{
        handler::DebugHandler,
        parser::{ParserOption, XMLReaderBuilder},
    },
};
use anyxml_uri::uri::URIString;
use clap::Parser;

#[derive(clap::Parser)]
struct CmdArgs {
    #[clap(long, help = "inspect with SAX API")]
    sax: bool,
    #[clap(long, help = "validate using DTD")]
    dtd_valid: bool,
    #[clap(long, help = "disable namespace handling")]
    no_namespace: bool,
    #[clap(help = "path to the target XML document")]
    file: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = CmdArgs::parse();

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
        reader.parse_uri(uri, None).ok();
        if args.sax {
            print!("{}", reader.handler.buffer);
        }
    }

    Ok(())
}
