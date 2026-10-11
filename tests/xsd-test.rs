use std::{convert::Infallible, str::FromStr};

use anyxml::{
    error::XMLError,
    sax::{
        DefaultParserSpec, DefaultSAXHandler, EntityResolver, ErrorHandler, InputSource,
        ParseError, SAXHandler, XMLReader, error::SAXParseError,
    },
    tree::{Element, TreeBuildHandler},
    uri::{URIStr, URIString},
    xsd::XSDSchema,
};

const XML_XSTS_NAMESPACE: &str = "http://www.w3.org/XML/2004/xml-schema-test-suite/";
const XML_XLINK_NAMESPACE: &str = "http://www.w3.org/1999/xlink";

/// Tuple of test set, test group schema test name and reason.
const SKIP_SCHEMA_TESTS: &[(&str, &str, &str, &str)] = &[
    (
        "MS-Regex2006-07-15",
        "HighSurrogates",
        "HighSurrogates",
        "'HighSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Element2006-07-15",
        "elemE001",
        "elemE001",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Element2006-07-15",
        "elemE002",
        "elemE002",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Element2006-07-15",
        "elemE003",
        "elemE003",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Element2006-07-15",
        "elemE004",
        "elemE004",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Element2006-07-15",
        "elemE005",
        "elemE005",
        "'{,5}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reC65",
        "reC65",
        "'{,2}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reC66",
        "reC66",
        "'{,2}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reC67",
        "reC67",
        "'{,2}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reC68",
        "reC68",
        "'{,2}' is invalid quantifier in XSD 1.0 regex.",
    ),
    (
        "MS-Regex2006-07-15",
        "reM77",
        "reM77",
        "'LowSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "reN75",
        "reN75",
        "'HighSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_9",
        "RegexTest_9",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_11",
        "RegexTest_11",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_14",
        "RegexTest_14",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_15",
        "RegexTest_15",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_16",
        "RegexTest_16",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_17",
        "RegexTest_17",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_27",
        "RegexTest_27",
        "'(?r:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_28",
        "RegexTest_28",
        "'(?c:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_30",
        "RegexTest_30",
        "'(?n:(foo)(\\s+)(bar))' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_33",
        "RegexTest_33",
        "'(?e:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_34",
        "RegexTest_34",
        "'(?+i:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_57",
        "RegexTest_57",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_58",
        "RegexTest_58",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_65",
        "RegexTest_65",
        "'\\o' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_113",
        "RegexTest_113",
        "'foo(?(?#COMMENT)foo)' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_116",
        "RegexTest_116",
        "'foo(?(?afdfoo)bar)' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_119",
        "RegexTest_119",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_120",
        "RegexTest_120",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_121",
        "RegexTest_121",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_141",
        "RegexTest_141",
        "'\\077' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_142",
        "RegexTest_142",
        "'\\77' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_143",
        "RegexTest_143",
        "'\\176' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_145",
        "RegexTest_145",
        "'\\300' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_146",
        "RegexTest_146",
        "'\\300' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_147",
        "RegexTest_147",
        "'\\477' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_148",
        "RegexTest_148",
        "'\\777' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_149",
        "RegexTest_149",
        "'\\7770' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_150",
        "RegexTest_150",
        "'\\7' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_151",
        "RegexTest_151",
        "'\\077' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_152",
        "RegexTest_152",
        "'\\77' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_154",
        "RegexTest_154",
        "'\\40' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_155",
        "RegexTest_155",
        "'\\040' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_156",
        "RegexTest_156",
        "'\\176' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_157",
        "RegexTest_157",
        "'\\377' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_158",
        "RegexTest_158",
        "'\\400' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_163",
        "RegexTest_163",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_164",
        "RegexTest_164",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_165",
        "RegexTest_165",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_166",
        "RegexTest_166",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_167",
        "RegexTest_167",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_168",
        "RegexTest_168",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_169",
        "RegexTest_169",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_170",
        "RegexTest_170",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_171",
        "RegexTest_171",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_172",
        "RegexTest_172",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_173",
        "RegexTest_173",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_174",
        "RegexTest_174",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_194",
        "RegexTest_194",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_195",
        "RegexTest_195",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_196",
        "RegexTest_196",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_197",
        "RegexTest_197",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_198",
        "RegexTest_198",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_199",
        "RegexTest_199",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_200",
        "RegexTest_200",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_201",
        "RegexTest_201",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_202",
        "RegexTest_202",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_203",
        "RegexTest_203",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_204",
        "RegexTest_204",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_205",
        "RegexTest_205",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_206",
        "RegexTest_206",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_207",
        "RegexTest_207",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_208",
        "RegexTest_208",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_209",
        "RegexTest_209",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_210",
        "RegexTest_210",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_211",
        "RegexTest_211",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_212",
        "RegexTest_212",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_213",
        "RegexTest_213",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_214",
        "RegexTest_214",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_215",
        "RegexTest_215",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_216",
        "RegexTest_216",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_217",
        "RegexTest_217",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_218",
        "RegexTest_218",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_220",
        "RegexTest_220",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_221",
        "RegexTest_221",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_222",
        "RegexTest_222",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_226",
        "RegexTest_226",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_230",
        "RegexTest_230",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_232",
        "RegexTest_232",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_233",
        "RegexTest_233",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_440",
        "RegexTest_440",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_481",
        "RegexTest_481",
        "Named capturing group is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_482",
        "RegexTest_482",
        "'(?:b*)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_483",
        "RegexTest_483",
        "Positive Lookahead is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_484",
        "RegexTest_484",
        "Negative Lookahead is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_487",
        "RegexTest_487",
        "Atomic grouping is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_516",
        "RegexTest_516",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_517",
        "RegexTest_517",
        "'\\060' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_518",
        "RegexTest_518",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_519",
        "RegexTest_519",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_524",
        "RegexTest_524",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_594",
        "RegexTest_594",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_595",
        "RegexTest_595",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_596",
        "RegexTest_596",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_597",
        "RegexTest_597",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_598",
        "RegexTest_598",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_599",
        "RegexTest_599",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_600",
        "RegexTest_600",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_601",
        "RegexTest_601",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_602",
        "RegexTest_602",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_603",
        "RegexTest_603",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_604",
        "RegexTest_604",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_605",
        "RegexTest_605",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_688",
        "RegexTest_688",
        "'\\10' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_696",
        "RegexTest_696",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_697",
        "RegexTest_697",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_698",
        "RegexTest_698",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_699",
        "RegexTest_699",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_700",
        "RegexTest_700",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_701",
        "RegexTest_701",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_702",
        "RegexTest_702",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_703",
        "RegexTest_703",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_704",
        "RegexTest_704",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_705",
        "RegexTest_705",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_706",
        "RegexTest_706",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_707",
        "RegexTest_707",
        "'\\2' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_824",
        "RegexTest_824",
        "'\\10' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_826",
        "RegexTest_826",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_827",
        "RegexTest_827",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_836",
        "RegexTest_836",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_837",
        "RegexTest_837",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_841",
        "RegexTest_841",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_842",
        "RegexTest_842",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_843",
        "RegexTest_843",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_844",
        "RegexTest_844",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_845",
        "RegexTest_845",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_846",
        "RegexTest_846",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_847",
        "RegexTest_847",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_848",
        "RegexTest_848",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_851",
        "RegexTest_851",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_852",
        "RegexTest_852",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_853",
        "RegexTest_853",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_854",
        "RegexTest_854",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_855",
        "RegexTest_855",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_856",
        "RegexTest_856",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_857",
        "RegexTest_857",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_861",
        "RegexTest_861",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_862",
        "RegexTest_862",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_863",
        "RegexTest_863",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_864",
        "RegexTest_864",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_865",
        "RegexTest_865",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_866",
        "RegexTest_866",
        "'\\1' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_870",
        "RegexTest_870",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_879",
        "RegexTest_879",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_880",
        "RegexTest_880",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_888",
        "RegexTest_888",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_889",
        "RegexTest_889",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_890",
        "RegexTest_890",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_891",
        "RegexTest_891",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_892",
        "RegexTest_892",
        "Non-capturing group is unacceptable syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_893",
        "RegexTest_893",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_894",
        "RegexTest_894",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_895",
        "RegexTest_895",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_896",
        "RegexTest_896",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_897",
        "RegexTest_897",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_898",
        "RegexTest_898",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_899",
        "RegexTest_899",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_900",
        "RegexTest_900",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_901",
        "RegexTest_901",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_902",
        "RegexTest_902",
        "Non-capturing group with internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_903",
        "RegexTest_903",
        "Internal option setting syntax is unacceptable in XSD 1.0 regex",
    ),
];
/// Tuple of test set, test group instance test name and reason.
const SKIP_INSTANCE_TESTS: &[(&str, &str, &str, &str)] = &[
    (
        "MS-Regex2006-07-15",
        "reM77",
        "reM77.i",
        "'LowSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "reN75",
        "reN75.i",
        "'HighSurrogates' is unacceptable block in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_9",
        "RegexTest_9.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_11",
        "RegexTest_11.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_15",
        "RegexTest_15.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_16",
        "RegexTest_16.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_17",
        "RegexTest_17.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_27",
        "RegexTest_27.i",
        "'(?r:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_28",
        "RegexTest_28.i",
        "'(?c:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_30",
        "RegexTest_30.v",
        "'(?n:(foo)(\\s+)(bar))' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_33",
        "RegexTest_33.i",
        "'(?e:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_34",
        "RegexTest_34.v",
        "'(?+i:foo)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_57",
        "RegexTest_57.v",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_58",
        "RegexTest_58.v",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_65",
        "RegexTest_65.i",
        "'\\o' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_113",
        "RegexTest_113.i",
        "'foo(?(?#COMMENT)foo)' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_116",
        "RegexTest_116.i",
        "'foo(?(?afdfoo)bar)' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_119",
        "RegexTest_119.i",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_120",
        "RegexTest_120.i",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_121",
        "RegexTest_121.i",
        "'(foo) (?#foo) \\s+ (?#followed by...' is unkown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_141",
        "RegexTest_141.i",
        "'\\077' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_142",
        "RegexTest_142.i",
        "'\\77' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_143",
        "RegexTest_143.i",
        "'\\176' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_145",
        "RegexTest_145.i",
        "'\\300' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_146",
        "RegexTest_146.i",
        "'\\300' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_147",
        "RegexTest_147.i",
        "'\\477' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_148",
        "RegexTest_148.i",
        "'\\777' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_149",
        "RegexTest_149.i",
        "'\\7770' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_150",
        "RegexTest_150.i",
        "'\\7' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_151",
        "RegexTest_151.i",
        "'\\077' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_152",
        "RegexTest_152.i",
        "'\\77' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_154",
        "RegexTest_154.i",
        "'\\40' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_155",
        "RegexTest_155.i",
        "'\\040' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_156",
        "RegexTest_156.i",
        "'\\176' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_157",
        "RegexTest_157.i",
        "'\\377' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_158",
        "RegexTest_158.i",
        "'\\400' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_163",
        "RegexTest_163.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_164",
        "RegexTest_164.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_165",
        "RegexTest_165.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_166",
        "RegexTest_166.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_167",
        "RegexTest_167.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_168",
        "RegexTest_168.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_169",
        "RegexTest_169.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_170",
        "RegexTest_170.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_171",
        "RegexTest_171.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_172",
        "RegexTest_172.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_173",
        "RegexTest_173.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_174",
        "RegexTest_174.i",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_194",
        "RegexTest_194.v",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_195",
        "RegexTest_195.v",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_196",
        "RegexTest_196.v",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_197",
        "RegexTest_197.i",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_198",
        "RegexTest_198.i",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_199",
        "RegexTest_199.i",
        "'\\A' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_200",
        "RegexTest_200.v",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_201",
        "RegexTest_201.v",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_202",
        "RegexTest_202.v",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_203",
        "RegexTest_203.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_204",
        "RegexTest_204.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_205",
        "RegexTest_205.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_206",
        "RegexTest_206.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_207",
        "RegexTest_207.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_208",
        "RegexTest_208.i",
        "'\\Z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_209",
        "RegexTest_209.v",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_210",
        "RegexTest_210.v",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_211",
        "RegexTest_211.v",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_212",
        "RegexTest_212.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_213",
        "RegexTest_213.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_214",
        "RegexTest_214.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_215",
        "RegexTest_215.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_216",
        "RegexTest_216.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_217",
        "RegexTest_217.i",
        "'\\z' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_218",
        "RegexTest_218.i",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_220",
        "RegexTest_220.i",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_221",
        "RegexTest_221.i",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_222",
        "RegexTest_222.i",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_226",
        "RegexTest_226.i",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_230",
        "RegexTest_230.i",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_232",
        "RegexTest_232.i",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_233",
        "RegexTest_233.i",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_440",
        "RegexTest_440.i",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_481",
        "RegexTest_481.i",
        "Named capturing group is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_482",
        "RegexTest_482.v",
        "'(?:b*)' is unknown syntax in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_483",
        "RegexTest_483.v",
        "Positive Lookahead is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_484",
        "RegexTest_484.i",
        "Negative Lookahead is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_487",
        "RegexTest_487.i",
        "Atomic grouping is not supported in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_516",
        "RegexTest_516.v",
        "'\\B' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_517",
        "RegexTest_517.v",
        "'\\060' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_518",
        "RegexTest_518.v",
        "'\\x' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_519",
        "RegexTest_519.v",
        "'\\u' is unacceptable escape sequence in XSD 1.0 regex",
    ),
    (
        "MS-Regex2006-07-15",
        "RegexTest_524",
        "RegexTest_524.v",
        "'\\b' is unacceptable escape sequence in XSD 1.0 regex",
    ),
];

#[derive(Default)]
struct TestResult {
    st_total: usize,
    st_skipped: usize,
    st_unexpected_success: usize,
    st_unexpected_failure: usize,
    it_total: usize,
    it_skipped: usize,
    it_unexpected_success: usize,
    it_unexpected_failure: usize,
}

#[derive(Clone, Copy)]
enum TestOutcome {
    Valid,
    Invalid,
    NotKnown,
    RuntimeSchemaError,
    ImplementationDefined,
    ImplementationDependent,
    Indeterminate,
    InvalidLatent,
}

impl FromStr for TestOutcome {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "valid" => Ok(Self::Valid),
            "invalid" => Ok(Self::Invalid),
            "notKnown" => Ok(Self::NotKnown),
            "runtime-schema-error" => Ok(Self::RuntimeSchemaError),
            "implementation-defined" => Ok(Self::ImplementationDefined),
            "implementation-dependent" => Ok(Self::ImplementationDependent),
            "indeterminate" => Ok(Self::Indeterminate),
            "invalid-latent" => Ok(Self::InvalidLatent),
            _ => unreachable!(),
        }
    }
}

#[derive(Default)]
struct SchemaTestHandler {
    fatal_error: usize,
    error: usize,
}

impl EntityResolver for SchemaTestHandler {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        DefaultSAXHandler.resolve_entity(name, public_id, base_uri, system_id)
    }

    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        DefaultSAXHandler.get_external_subset(name, base_uri)
    }
}
impl ErrorHandler for SchemaTestHandler {
    fn warning(&mut self, _error: SAXParseError) {
        // supress warnings
        // DefaultSAXHandler.warning(error);
    }
    fn error(&mut self, error: SAXParseError) {
        if matches!(error.error, XMLError::XSDError(_)) {
            self.error += 1;
        }
        if !matches!(
            error.error,
            XMLError::XMLParseError(ParseError::NamespaceNameNotAbsoluteURI)
        ) {
            // The test suite uses a large number of namespace names that are not absolute URIs,
            // but since they are not fundamental errors, I will suppress they here.
            DefaultSAXHandler.error(error);
        }
    }
    fn fatal_error(&mut self, error: SAXParseError) {
        self.fatal_error += 1;
        DefaultSAXHandler.fatal_error(error);
    }
}
impl SAXHandler for SchemaTestHandler {}

fn get_skip_reason(
    list: &'static [(&str, &str, &str, &'static str)],
    test_set: &str,
    test_group: &str,
    test_name: &str,
) -> Option<&'static str> {
    list.iter().find_map(|item| {
        ((item.0, item.1, item.2) == (test_set, test_group, test_name)).then_some(item.3)
    })
}

fn schema_test(
    node: Element,
    base_uri: &URIStr,
    test_set: &str,
    test_group: &str,
    out: &mut TestResult,
) -> Vec<URIString> {
    let name = node
        .get_attribute("name", None)
        .unwrap_or_else(|| "unknown".into());
    eprintln!("--- Start schema test '{name}' ---");
    if let Some(reason) = get_skip_reason(SKIP_SCHEMA_TESTS, test_set, test_group, name.as_str()) {
        eprintln!("test '{}' is skipped because {}", name, reason);
        out.st_skipped += 1;
        return vec![];
    }
    let validity = node
        .get_elements_by_expanded_name("expected", Some(XML_XSTS_NAMESPACE))
        .next()
        .and_then(|e| e.get_attribute("validity", None))
        .unwrap()
        .parse::<TestOutcome>()
        .unwrap();
    let mut ret = vec![];
    for schema in node.get_elements_by_expanded_name("schemaDocument", Some(XML_XSTS_NAMESPACE)) {
        let href = schema
            .get_attribute("href", Some(XML_XLINK_NAMESPACE))
            .unwrap();
        let href = URIString::parse(href).unwrap();
        let href = base_uri.resolve(&href);
        eprintln!("----- Start parsing '{href}' -----");

        let mut handler = SchemaTestHandler::default();
        let schema = XSDSchema::parse_uri(&href, None, Some(&mut handler));
        out.st_total += 1;
        match validity {
            TestOutcome::Valid => {
                if schema.is_err() || handler.fatal_error != 0 || handler.error != 0 {
                    out.st_unexpected_failure += 1;
                    eprintln!("The schema test '{href}' is unexpected failure.");
                } else {
                    ret.push(href);
                }
            }
            TestOutcome::Invalid => {
                if schema.is_ok() && handler.fatal_error == 0 && handler.error == 0 {
                    out.st_unexpected_success += 1;
                    eprintln!("The schema test '{href}' is unexpected success.");
                }
            }
            TestOutcome::NotKnown => {
                out.st_skipped += 1;
                eprintln!("'notKnown' is is meaningless for schema tests.");
            }
            TestOutcome::RuntimeSchemaError => {
                out.st_skipped += 1;
                eprintln!("'runtime-schema-error' is is meaningless for schema tests.");
            }
            TestOutcome::ImplementationDefined => {
                out.st_skipped += 1;
                eprintln!(
                    "Implementation defined: error={}, fatal error={}",
                    handler.error, handler.fatal_error
                );
            }
            TestOutcome::ImplementationDependent => {
                out.st_skipped += 1;
                eprintln!(
                    "Implementation dependent: error={}, fatal error={}",
                    handler.error, handler.fatal_error
                );
            }
            TestOutcome::Indeterminate => {
                out.st_skipped += 1;
                eprintln!(
                    "Inderminate: error={}, fatal error={}",
                    handler.error, handler.fatal_error
                );
            }
            TestOutcome::InvalidLatent => {
                out.st_skipped += 1;
                eprintln!(
                    "InvalidLatent: error={}, fatal error={}",
                    handler.error, handler.fatal_error
                );
            }
        }
    }
    ret
}

fn instance_test(
    node: Element,
    base_uri: &URIStr,
    test_set: &str,
    test_group: &str,
    schemas: &[URIString],
    out: &mut TestResult,
) {
    let name = node
        .get_attribute("name", None)
        .unwrap_or_else(|| "unknown".into());
    eprintln!("--- Start instance test '{name}' ---");
    if let Some(reason) = get_skip_reason(SKIP_INSTANCE_TESTS, test_set, test_group, name.as_str())
    {
        eprintln!("test '{}' is skipped because {}", name, reason);
        out.it_skipped += 1;
        return;
    }
    let validity = node
        .get_elements_by_expanded_name("expected", Some(XML_XSTS_NAMESPACE))
        .next()
        .and_then(|e| e.get_attribute("validity", None))
        .unwrap()
        .parse::<TestOutcome>()
        .unwrap();
    let mut schema = XSDSchema::default();
    for sc in schemas {
        if let Ok(sc) = XSDSchema::parse_uri(sc, None, None::<DefaultSAXHandler>) {
            schema.extend(sc);
        }
    }
    for instance in node.get_elements_by_expanded_name("instanceDocument", Some(XML_XSTS_NAMESPACE))
    {
        let href = instance
            .get_attribute("href", Some(XML_XLINK_NAMESPACE))
            .unwrap();
        let href = URIString::parse(href).unwrap();
        let href = base_uri.resolve(&href);
        eprintln!("----- Start parsing '{href}' -----");

        let mut validator = schema.new_validate_handler(SchemaTestHandler::default());
        validator.use_schema_location_hint = true;
        let mut reader = XMLReader::builder().set_handler(validator).build();
        reader.parse_uri(&href, None).ok();
        let fatal_error = reader.handler.child.fatal_error;
        if fatal_error > 0 {
            eprintln!("The instance test target '{href}' is corrupted, so skip this.");
            out.it_skipped += 1;
            continue;
        }
        let error = reader.handler.child.error;
        out.it_total += 1;
        match validity {
            TestOutcome::Valid => {
                if error > 0 {
                    out.it_unexpected_failure += 1;
                    eprintln!("The instance test '{href}' is unexpected failure.");
                }
            }
            TestOutcome::Invalid => {
                if error == 0 {
                    out.it_unexpected_success += 1;
                    eprintln!("The instance test '{href}' is unexpected success.");
                }
            }
            TestOutcome::NotKnown => {
                out.it_skipped += 1;
                eprintln!("'notKnown' is not supported for instance tests.");
            }
            TestOutcome::RuntimeSchemaError => {
                if error == 0 {
                    out.it_unexpected_success += 1;
                    eprintln!("'runtime-schema-error' could not detected for '{href}'.");
                }
            }
            TestOutcome::ImplementationDefined => {
                out.it_skipped += 1;
                eprintln!(
                    "Implementation defined: error={}, fatal error={}",
                    error, fatal_error
                );
            }
            TestOutcome::ImplementationDependent => {
                out.it_skipped += 1;
                eprintln!(
                    "Implementation dependent: error={}, fatal error={}",
                    error, fatal_error
                );
            }
            TestOutcome::Indeterminate => {
                out.it_skipped += 1;
                eprintln!("Inderminate: error={}, fatal error={}", error, fatal_error);
            }
            TestOutcome::InvalidLatent => {
                out.it_skipped += 1;
                eprintln!(
                    "InvalidLatent: error={}, fatal error={}",
                    error, fatal_error
                );
            }
        }
    }
}

fn testset(
    uri: &URIStr,
    reader: &mut XMLReader<DefaultParserSpec, TreeBuildHandler>,
) -> TestResult {
    reader.parse_uri(uri, None).unwrap();

    let testset = reader.handler.document.document_element().unwrap();
    let contributor = testset
        .get_attribute("contributor", None)
        .unwrap_or_else(|| "unknown".into());
    let test_set_name = testset
        .get_attribute("name", None)
        .unwrap_or_else(|| "unknown".into());
    let banner = format!("=== Start test set '{test_set_name}' ===");
    eprintln!("{banner}");
    eprintln!("\tname       : {test_set_name}");
    eprintln!("\tcontributor: {contributor}");
    eprintln!("\tURI        : {uri}");
    eprintln!("{}", "=".repeat(banner.len()));

    let mut test_result = TestResult::default();
    for group in testset.get_elements_by_expanded_name("testGroup", Some(XML_XSTS_NAMESPACE)) {
        let test_group_name = group
            .get_attribute("name", None)
            .unwrap_or_else(|| "unknown".into());

        eprintln!("### Start test group '{test_group_name}' ###");
        let mut schemas = vec![];
        for st in group.get_elements_by_expanded_name("schemaTest", Some(XML_XSTS_NAMESPACE)) {
            schemas.extend(schema_test(
                st,
                uri,
                &test_set_name,
                &test_group_name,
                &mut test_result,
            ));
        }
        for it in group.get_elements_by_expanded_name("instanceTest", Some(XML_XSTS_NAMESPACE)) {
            instance_test(
                it,
                uri,
                &test_set_name,
                &test_group_name,
                &schemas,
                &mut test_result,
            );
        }
    }
    eprintln!();
    eprintln!(
        "schema test  : total={}, skipped={}, unexpected success={}, unexpected failure={}",
        test_result.st_total,
        test_result.st_skipped,
        test_result.st_unexpected_success,
        test_result.st_unexpected_failure
    );
    eprintln!(
        "instance test: total={}, skipped={}, unexpected success={}, unexpected failure={}",
        test_result.it_total,
        test_result.it_skipped,
        test_result.it_unexpected_success,
        test_result.it_unexpected_failure
    );
    eprintln!("=== End test set '{test_set_name}' ===");
    eprintln!();
    test_result
}

#[test]
fn xsts_tests() {
    std::env::set_current_dir(env!("CARGO_MANIFEST_DIR")).unwrap();

    let mut handler = TreeBuildHandler::default();
    handler.expand_entity_reference = true;
    handler.coalescing = true;
    handler.ignoring_comments = true;
    let mut reader = XMLReader::builder().set_handler(handler).build();
    reader
        .parse_uri(
            URIString::parse("resources/xmlschema2006-11-06/suite.xml").unwrap(),
            None,
        )
        .unwrap();

    let suite = reader.handler.document.clone();
    let base_uri = suite.document_base_uri();
    let mut result = TestResult::default();
    for reference in suite.get_elements_by_expanded_name("testSetRef", Some(XML_XSTS_NAMESPACE)) {
        let href = reference
            .get_attribute("href", Some(XML_XLINK_NAMESPACE))
            .unwrap();
        let href = URIString::parse(href).unwrap();
        let href = base_uri.resolve(&href);
        let ret = testset(&href, &mut reader);
        result.st_total += ret.st_total;
        result.st_skipped += ret.st_skipped;
        result.st_unexpected_success += ret.st_unexpected_success;
        result.st_unexpected_failure += ret.st_unexpected_failure;
        result.it_total += ret.it_total;
        result.it_skipped += ret.it_skipped;
        result.it_unexpected_success += ret.it_unexpected_success;
        result.it_unexpected_failure += ret.it_unexpected_failure;
    }

    eprintln!("====================================");
    eprintln!("Schema test:");
    eprintln!("\ttotal              : {}", result.st_total);
    eprintln!("\tskipped            : {}", result.st_skipped);
    eprintln!("\tunexpected success : {}", result.st_unexpected_success);
    eprintln!("\tunexpected failure : {}", result.st_unexpected_failure);
    eprintln!("Instance test:");
    eprintln!("\ttotal              : {}", result.it_total);
    eprintln!("\tskipped            : {}", result.it_skipped);
    eprintln!("\tunexpected success : {}", result.it_unexpected_success);
    eprintln!("\tunexpected failure : {}", result.it_unexpected_failure);
    eprintln!("====================================");
    assert!(
        result.st_unexpected_success == 0
            && result.st_unexpected_failure == 0
            && result.it_unexpected_success == 0
            && result.it_unexpected_failure == 0
    );
}
