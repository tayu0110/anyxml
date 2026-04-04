use std::{fs::File, io::Read};

use anyxml::{sax::XMLReader, uri::URIString};
use criterion::{Criterion, criterion_group, criterion_main};

fn sax_533946_luse_6668_2_op(c: &mut Criterion) {
    c.bench_function("533946_luse_6668_2_op.gml", |b| {
        b.iter(|| {
            let mut reader = XMLReader::builder().build();
            let uri = URIString::parse("../benches/dataset/533946_luse_6668_2_op.gml").unwrap();
            reader.parse_uri(uri, None).unwrap();
        });
    });
}

fn sax_53395538_fld_6697_l2_op(c: &mut Criterion) {
    c.bench_function("53395538_fld_6697_l2_op.gml", |b| {
        b.iter(|| {
            let mut reader = XMLReader::builder().build();
            let uri = URIString::parse("../benches/dataset/53395538_fld_6697_l2_op.gml").unwrap();
            reader.parse_uri(uri, None).unwrap();
        });
    });
}

fn sax_progressive_533946_luse_6668_2_op(c: &mut Criterion) {
    c.bench_function("progressive_533946_luse_6668_2_op.gml", |b| {
        b.iter(|| {
            let mut reader = XMLReader::builder().progressive_parser().build();
            let mut file = File::open("../benches/dataset/533946_luse_6668_2_op.gml").unwrap();
            file.lock().unwrap();
            let mut chunk = vec![0; 4096];
            while let bytes = file.read(&mut chunk).unwrap()
                && bytes != 0
            {
                reader.parse_chunk(&chunk[..bytes], false).unwrap();
            }
            reader.parse_chunk([], true).unwrap();
        });
    });
}

fn sax_progressive_53395538_fld_6697_l2_op(c: &mut Criterion) {
    c.bench_function("progressive_53395538_fld_6697_l2_op.gml", |b| {
        b.iter(|| {
            let mut reader = XMLReader::builder().progressive_parser().build();
            let mut file = File::open("../benches/dataset/53395538_fld_6697_l2_op.gml").unwrap();
            file.lock().unwrap();
            let mut chunk = vec![0; 4096];
            while let bytes = file.read(&mut chunk).unwrap()
                && bytes != 0
            {
                reader.parse_chunk(&chunk[..bytes], false).unwrap();
            }
            reader.parse_chunk([], true).unwrap();
        });
    });
}

criterion_group!(
    anyxml_sax,
    sax_533946_luse_6668_2_op,
    sax_53395538_fld_6697_l2_op
);
criterion_group!(
    anyxml_sax_progressive,
    sax_progressive_533946_luse_6668_2_op,
    sax_progressive_53395538_fld_6697_l2_op
);
criterion_main!(anyxml_sax, anyxml_sax_progressive);
