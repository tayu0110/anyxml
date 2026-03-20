use anyxml::{sax::parser::XMLReaderBuilder, uri::URIString};
use criterion::{Criterion, criterion_group, criterion_main};

fn sax_533946_luse_6668_2_op(c: &mut Criterion) {
    c.bench_function("533946_luse_6668_2_op.gml", |b| {
        b.iter(|| {
            let mut reader = XMLReaderBuilder::new().build();
            let uri = URIString::parse("../benches/dataset/533946_luse_6668_2_op.gml").unwrap();
            reader.parse_uri(uri, None).unwrap();
        });
    });
}

fn sax_53395538_fld_6697_l2_op(c: &mut Criterion) {
    c.bench_function("53395538_fld_6697_l2_op.gml", |b| {
        b.iter(|| {
            let mut reader = XMLReaderBuilder::new().build();
            let uri = URIString::parse("../benches/dataset/53395538_fld_6697_l2_op.gml").unwrap();
            reader.parse_uri(uri, None).unwrap();
        });
    });
}

// reference: https://github.com/RazrFalcon/roxmltree/blob/master/benches/xml.rs
fn parse_via_quick_xml(path: &str) {
    let mut r = quick_xml::NsReader::from_file(path).unwrap();
    r.config_mut().check_comments = true;
    let mut buf = Vec::new();
    loop {
        match r.read_resolved_event_into(&mut buf) {
            Ok((_, quick_xml::events::Event::Start(_)))
            | Ok((_, quick_xml::events::Event::Empty(_))) => (),
            Ok((_, quick_xml::events::Event::Text(ref e))) => {
                e.decode().unwrap();
            }
            Ok((_, quick_xml::events::Event::Eof)) => break,
            _ => (),
        }
        buf.clear();
    }
}

fn sax_533946_luse_6668_2_op_quick_xml(c: &mut Criterion) {
    c.bench_function("quick-xml_533946_luse_6668_2_op.gml", |b| {
        b.iter(|| parse_via_quick_xml("../benches/dataset/533946_luse_6668_2_op.gml"));
    });
}

fn sax_53395538_fld_6697_l2_op_quick_xml(c: &mut Criterion) {
    c.bench_function("quick-xml_53395538_fld_6697_l2_op.gml", |b| {
        b.iter(|| parse_via_quick_xml("../benches/dataset/53395538_fld_6697_l2_op.gml"));
    });
}

// // reference: https://github.com/RazrFalcon/roxmltree/blob/master/benches/xml.rs
// fn parse_via_xml_rs(path: &str) {
//     let file = std::fs::File::open(path).unwrap();
//     let mut config = xml::ParserConfig::new();
//     config.ignore_comments = false;
//     let reader = xml::EventReader::new(file);
//     for event in reader {
//         let _ = event.unwrap();
//     }
// }

// fn sax_533946_luse_6668_2_op_xml_rs(c: &mut Criterion) {
//     c.bench_function("xml-rs_533946_luse_6668_2_op.gml", |b| {
//         b.iter(|| parse_via_xml_rs("benches/dataset/533946_luse_6668_2_op.gml"));
//     });
// }

// fn sax_53395538_fld_6697_l2_op_xml_rs(c: &mut Criterion) {
//     c.bench_function("xml-rs_53395538_fld_6697_l2_op.gml", |b| {
//         b.iter(|| parse_via_xml_rs("benches/dataset/53395538_fld_6697_l2_op.gml"));
//     });
// }

criterion_group!(
    anyxml,
    sax_533946_luse_6668_2_op,
    sax_53395538_fld_6697_l2_op
);
criterion_group!(
    quick_xml,
    sax_533946_luse_6668_2_op_quick_xml,
    sax_53395538_fld_6697_l2_op_quick_xml
);
// `xml` crate is far too slow, so it has been excluded from the benchmark.
// In v1.2.1, it took approximately 7000 seconds and 1200 seconds to complete each run.
// criterion_group!(
//     xml_rs,
//     sax_533946_luse_6668_2_op_xml_rs,
//     sax_53395538_fld_6697_l2_op_xml_rs
// );
criterion_main!(anyxml, quick_xml);
