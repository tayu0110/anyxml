use std::str::FromStr;

use crate::{MediaTypeError, PrivateSubtype};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ApplicationSubtype {
    /// 1d-interleaved-parityfec
    _1DInterleavedParityfec,
    /// 3gpdash-qoe-report+xml
    _3GpdashQoeReportPlusXml,
    /// 3gpp-ims+xml
    _3GppImsPlusXml,
    /// 3gpp-mbs-object-manifest+json
    _3GppMbsObjectManifestPlusJson,
    /// 3gpp-mbs-user-service-descriptions+json
    _3GppMbsUserServiceDescriptionsPlusJson,
    /// 3gpp-media-delivery-metrics-report+json
    _3GppMediaDeliveryMetricsReportPlusJson,
    /// 3gppHal+json
    _3GppHalPlusJson,
    /// 3gppHalForms+json
    _3GppHalFormsPlusJson,
    /// A2L
    A2L,
    /// aas+zip
    AasPlusZip,
    /// ace+cbor
    AcePlusCbor,
    /// ace+json
    AcePlusJson,
    /// ace-groupcomm+cbor
    AceGroupcommPlusCbor,
    /// ace-trl+cbor
    AceTrlPlusCbor,
    /// activemessage
    Activemessage,
    /// activity+json
    ActivityPlusJson,
    /// aif+cbor
    AifPlusCbor,
    /// aif+json
    AifPlusJson,
    /// alto-cdni+json
    AltoCdniPlusJson,
    /// alto-cdnifilter+json
    AltoCdnifilterPlusJson,
    /// alto-costmap+json
    AltoCostmapPlusJson,
    /// alto-costmapfilter+json
    AltoCostmapfilterPlusJson,
    /// alto-directory+json
    AltoDirectoryPlusJson,
    /// alto-endpointcost+json
    AltoEndpointcostPlusJson,
    /// alto-endpointcostparams+json
    AltoEndpointcostparamsPlusJson,
    /// alto-endpointprop+json
    AltoEndpointpropPlusJson,
    /// alto-endpointpropparams+json
    AltoEndpointpropparamsPlusJson,
    /// alto-error+json
    AltoErrorPlusJson,
    /// alto-networkmap+json
    AltoNetworkmapPlusJson,
    /// alto-networkmapfilter+json
    AltoNetworkmapfilterPlusJson,
    /// alto-propmap+json
    AltoPropmapPlusJson,
    /// alto-propmapparams+json
    AltoPropmapparamsPlusJson,
    /// alto-tips+json
    AltoTipsPlusJson,
    /// alto-tipsparams+json
    AltoTipsparamsPlusJson,
    /// alto-updatestreamcontrol+json
    AltoUpdatestreamcontrolPlusJson,
    /// alto-updatestreamparams+json
    AltoUpdatestreamparamsPlusJson,
    /// AML
    Aml,
    /// andrew-inset
    AndrewInset,
    /// applefile
    Applefile,
    /// asyncapi+json
    AsyncapiPlusJson,
    /// asyncapi+yaml
    AsyncapiPlusYaml,
    /// at+jwt
    AtPlusJwt,
    /// ATF
    Atf,
    /// ATFX
    Atfx,
    /// atom+xml
    AtomPlusXml,
    /// atomcat+xml
    AtomcatPlusXml,
    /// atomdeleted+xml
    AtomdeletedPlusXml,
    /// atomicmail
    Atomicmail,
    /// atomsvc+xml
    AtomsvcPlusXml,
    /// atsc-dwd+xml
    AtscDwdPlusXml,
    /// atsc-dynamic-event-message
    AtscDynamicEventMessage,
    /// atsc-held+xml
    AtscHeldPlusXml,
    /// atsc-rdt+json
    AtscRdtPlusJson,
    /// atsc-rsat+xml
    AtscRsatPlusXml,
    /// ATXML
    Atxml,
    /// auth-policy+xml
    AuthPolicyPlusXml,
    /// automationml-aml+xml
    AutomationmlAmlPlusXml,
    /// automationml-amlx+zip
    AutomationmlAmlxPlusZip,
    /// bacnet-xdd+zip
    BacnetXddPlusZip,
    /// batch-SMTP
    BatchSmtp,
    /// beep+xml
    BeepPlusXml,
    /// bufr
    Bufr,
    /// c2pa
    C2Pa,
    /// calendar+json
    CalendarPlusJson,
    /// calendar+xml
    CalendarPlusXml,
    /// call-completion
    CallCompletion,
    /// CALS-1840
    Cals1840,
    /// captive+json
    CaptivePlusJson,
    /// cbor
    Cbor,
    /// cbor-seq
    CborSeq,
    /// cccex
    Cccex,
    /// ccmp+xml
    CcmpPlusXml,
    /// ccxml+xml
    CcxmlPlusXml,
    /// cda+xml
    CdaPlusXml,
    /// CDFX+XML
    CdfxPlusXml,
    /// cdmi-capability
    CdmiCapability,
    /// cdmi-container
    CdmiContainer,
    /// cdmi-domain
    CdmiDomain,
    /// cdmi-object
    CdmiObject,
    /// cdmi-queue
    CdmiQueue,
    /// cdni
    Cdni,
    /// ce+cbor
    CePlusCbor,
    /// CEA
    Cea,
    /// cea-2018+xml
    Cea2018PlusXml,
    /// cellml+xml
    CellmlPlusXml,
    /// cfw
    Cfw,
    /// cid
    Cid,
    /// cid-edhoc+cbor-seq
    CidEdhocPlusCborSeq,
    /// city+json
    CityPlusJson,
    /// city+json-seq
    CityPlusJsonSeq,
    /// clr
    Clr,
    /// clue+xml
    CluePlusXml,
    /// clue_info+xml
    ClueInfoPlusXml,
    /// cms
    Cms,
    /// cmw+cbor
    CmwPlusCbor,
    /// cmw+cose
    CmwPlusCose,
    /// cmw+json
    CmwPlusJson,
    /// cmw+jws
    CmwPlusJws,
    /// cnrp+xml
    CnrpPlusXml,
    /// coap-eap
    CoapEap,
    /// coap-group+json
    CoapGroupPlusJson,
    /// coap-payload
    CoapPayload,
    /// commonground
    Commonground,
    /// concise-problem-details+cbor
    ConciseProblemDetailsPlusCbor,
    /// conference-info+xml
    ConferenceInfoPlusXml,
    /// cose
    Cose,
    /// cose-key
    CoseKey,
    /// cose-key-set
    CoseKeySet,
    /// cose-x509
    CoseX509,
    /// cpl+xml
    CplPlusXml,
    /// csrattrs
    Csrattrs,
    /// csta+xml
    CstaPlusXml,
    /// CSTAdata+xml
    CstAdataPlusXml,
    /// csvm+json
    CsvmPlusJson,
    /// cwl
    Cwl,
    /// cwl+json
    CwlPlusJson,
    /// cwl+yaml
    CwlPlusYaml,
    /// cwt
    Cwt,
    /// cybercash
    Cybercash,
    /// dash+xml
    DashPlusXml,
    /// dash-patch+xml
    DashPatchPlusXml,
    /// dashdelta
    Dashdelta,
    /// davmount+xml
    DavmountPlusXml,
    /// dca-rft
    DcaRft,
    /// DCD
    Dcd,
    /// dec-dx
    DecDx,
    /// dialog-info+xml
    DialogInfoPlusXml,
    /// dicom
    Dicom,
    /// dicom+json
    DicomPlusJson,
    /// dicom+xml
    DicomPlusXml,
    /// did
    Did,
    /// DII
    Dii,
    /// DIT
    Dit,
    /// dns
    Dns,
    /// dns+json
    DnsPlusJson,
    /// dns-message
    DnsMessage,
    /// dots+cbor
    DotsPlusCbor,
    /// dpop+jwt
    DpopPlusJwt,
    /// dskpp+xml
    DskppPlusXml,
    /// dssc+der
    DsscPlusDer,
    /// dssc+xml
    DsscPlusXml,
    /// dvcs
    Dvcs,
    /// eat+cwt
    EatPlusCwt,
    /// eat+jwt
    EatPlusJwt,
    /// eat-bun+cbor
    EatBunPlusCbor,
    /// eat-bun+json
    EatBunPlusJson,
    /// eat-ucs+cbor
    EatUcsPlusCbor,
    /// eat-ucs+json
    EatUcsPlusJson,
    /// ecmascript
    Ecmascript,
    /// edhoc+cbor-seq
    EdhocPlusCborSeq,
    /// EDI-consent
    EdiConsent,
    /// EDI-X12
    EdiX12,
    /// EDIFACT
    Edifact,
    /// efi
    Efi,
    /// elm+json
    ElmPlusJson,
    /// elm+xml
    ElmPlusXml,
    /// EmergencyCallData.cap+xml
    EmergencyCallDataDotCapPlusXml,
    /// EmergencyCallData.Comment+xml
    EmergencyCallDataDotCommentPlusXml,
    /// EmergencyCallData.Control+xml
    EmergencyCallDataDotControlPlusXml,
    /// EmergencyCallData.DeviceInfo+xml
    EmergencyCallDataDotDeviceInfoPlusXml,
    /// EmergencyCallData.eCall.MSD
    EmergencyCallDataDotECallDotMsd,
    /// EmergencyCallData.LegacyESN+json
    EmergencyCallDataDotLegacyEsnPlusJson,
    /// EmergencyCallData.ProviderInfo+xml
    EmergencyCallDataDotProviderInfoPlusXml,
    /// EmergencyCallData.ServiceInfo+xml
    EmergencyCallDataDotServiceInfoPlusXml,
    /// EmergencyCallData.SubscriberInfo+xml
    EmergencyCallDataDotSubscriberInfoPlusXml,
    /// EmergencyCallData.VEDS+xml
    EmergencyCallDataDotVedsPlusXml,
    /// emma+xml
    EmmaPlusXml,
    /// emotionml+xml
    EmotionmlPlusXml,
    /// encaprtp
    Encaprtp,
    /// entity-statement+jwt
    EntityStatementPlusJwt,
    /// epp+xml
    EppPlusXml,
    /// epub+zip
    EpubPlusZip,
    /// eshop
    Eshop,
    /// example
    Example,
    /// exi
    Exi,
    /// expect-ct-report+json
    ExpectCtReportPlusJson,
    /// explicit-registration-response+jwt
    ExplicitRegistrationResponsePlusJwt,
    /// express
    Express,
    /// fastinfoset
    Fastinfoset,
    /// fastsoap
    Fastsoap,
    /// fdf
    Fdf,
    /// fdt+xml
    FdtPlusXml,
    /// fhir+json
    FhirPlusJson,
    /// fhir+xml
    FhirPlusXml,
    /// fits
    Fits,
    /// flexfec
    Flexfec,
    /// font-sfnt
    FontSfntDeprecatedInFavorOfFontSfnt,
    /// font-tdpfr
    FontTdpfr,
    /// font-woff
    FontWoffDeprecatedInFavorOfFontWoff,
    /// framework-attributes+xml
    FrameworkAttributesPlusXml,
    /// geo+json
    GeoPlusJson,
    /// geo+json-seq
    GeoPlusJsonSeq,
    /// geofeed+csv
    GeofeedPlusCsv,
    /// geopackage+sqlite3
    GeopackagePlusSqlite3,
    /// geopose+json
    GeoposePlusJson,
    /// geoxacml+json
    GeoxacmlPlusJson,
    /// geoxacml+xml
    GeoxacmlPlusXml,
    /// gltf-buffer
    GltfBuffer,
    /// gml+xml
    GmlPlusXml,
    /// gnap-binding-jws
    GnapBindingJws,
    /// gnap-binding-jwsd
    GnapBindingJwsd,
    /// gnap-binding-rotation-jws
    GnapBindingRotationJws,
    /// gnap-binding-rotation-jwsd
    GnapBindingRotationJwsd,
    /// grib
    Grib,
    /// gzip
    Gzip,
    /// H224
    H224,
    /// held+xml
    HeldPlusXml,
    /// hl7v2+xml
    Hl7V2PlusXml,
    /// http
    Http,
    /// hyperstudio
    Hyperstudio,
    /// ibe-key-request+xml
    IbeKeyRequestPlusXml,
    /// ibe-pkg-reply+xml
    IbePkgReplyPlusXml,
    /// ibe-pp-data
    IbePpData,
    /// iges
    Iges,
    /// im-iscomposing+xml
    ImIscomposingPlusXml,
    /// index
    Index,
    /// index.cmd
    IndexDotCmd,
    /// index.obj
    IndexDotObj,
    /// index.response
    IndexDotResponse,
    /// index.vnd
    IndexDotVnd,
    /// inkml+xml
    InkmlPlusXml,
    /// IOTP
    Iotp,
    /// ipfix
    Ipfix,
    /// ipp
    Ipp,
    /// ISUP
    Isup,
    /// its+xml
    ItsPlusXml,
    /// java-archive
    JavaArchive,
    /// javascript
    Javascript,
    /// jf2feed+json
    Jf2FeedPlusJson,
    /// jose
    Jose,
    /// jose+json
    JosePlusJson,
    /// jrd+json
    JrdPlusJson,
    /// jscalendar+json
    JscalendarPlusJson,
    /// jscontact+json
    JscontactPlusJson,
    /// json
    Json,
    /// json-patch+json
    JsonPatchPlusJson,
    /// json-patch-query+json
    JsonPatchQueryPlusJson,
    /// json-seq
    JsonSeq,
    /// jsonpath
    Jsonpath,
    /// jwk+json
    JwkPlusJson,
    /// jwk-set+json
    JwkSetPlusJson,
    /// jwk-set+jwt
    JwkSetPlusJwt,
    /// jwt
    Jwt,
    /// kb+jwt
    KbPlusJwt,
    /// kbl+xml
    KblPlusXml,
    /// kpml-request+xml
    KpmlRequestPlusXml,
    /// kpml-response+xml
    KpmlResponsePlusXml,
    /// ld+json
    LdPlusJson,
    /// lgr+xml
    LgrPlusXml,
    /// link-format
    LinkFormat,
    /// linkset
    Linkset,
    /// linkset+json
    LinksetPlusJson,
    /// load-control+xml
    LoadControlPlusXml,
    /// logout+jwt
    LogoutPlusJwt,
    /// lost+xml
    LostPlusXml,
    /// lostsync+xml
    LostsyncPlusXml,
    /// lpf+zip
    LpfPlusZip,
    /// LXF
    Lxf,
    /// mac-binhex40
    MacBinhex40,
    /// macwriteii
    Macwriteii,
    /// mads+xml
    MadsPlusXml,
    /// manifest+json
    ManifestPlusJson,
    /// marc
    Marc,
    /// marcxml+xml
    MarcxmlPlusXml,
    /// mathematica
    Mathematica,
    /// mathml+xml
    MathmlPlusXml,
    /// mathml-content+xml
    MathmlContentPlusXml,
    /// mathml-presentation+xml
    MathmlPresentationPlusXml,
    /// mbms-associated-procedure-description+xml
    MbmsAssociatedProcedureDescriptionPlusXml,
    /// mbms-deregister+xml
    MbmsDeregisterPlusXml,
    /// mbms-envelope+xml
    MbmsEnvelopePlusXml,
    /// mbms-msk+xml
    MbmsMskPlusXml,
    /// mbms-msk-response+xml
    MbmsMskResponsePlusXml,
    /// mbms-protection-description+xml
    MbmsProtectionDescriptionPlusXml,
    /// mbms-reception-report+xml
    MbmsReceptionReportPlusXml,
    /// mbms-register+xml
    MbmsRegisterPlusXml,
    /// mbms-register-response+xml
    MbmsRegisterResponsePlusXml,
    /// mbms-schedule+xml
    MbmsSchedulePlusXml,
    /// mbms-user-service-description+xml
    MbmsUserServiceDescriptionPlusXml,
    /// mbox
    Mbox,
    /// measured-component+cbor
    MeasuredComponentPlusCbor,
    /// measured-component+json
    MeasuredComponentPlusJson,
    /// media-policy-dataset+xml
    MediaPolicyDatasetPlusXml,
    /// media_control+xml
    MediaControlPlusXml,
    /// mediaservercontrol+xml
    MediaservercontrolPlusXml,
    /// merge-patch+json
    MergePatchPlusJson,
    /// metalink4+xml
    Metalink4PlusXml,
    /// mets+xml
    MetsPlusXml,
    /// MF4
    Mf4,
    /// mikey
    Mikey,
    /// mipc
    Mipc,
    /// missing-blocks+cbor-seq
    MissingBlocksPlusCborSeq,
    /// mmt-aei+xml
    MmtAeiPlusXml,
    /// mmt-usd+xml
    MmtUsdPlusXml,
    /// mods+xml
    ModsPlusXml,
    /// moss-keys
    MossKeys,
    /// moss-signature
    MossSignature,
    /// mosskey-data
    MosskeyData,
    /// mosskey-request
    MosskeyRequest,
    /// mp21
    Mp21,
    /// mp4
    Mp4,
    /// mpeg4-generic
    Mpeg4Generic,
    /// mpeg4-iod
    Mpeg4Iod,
    /// mpeg4-iod-xmt
    Mpeg4IodXmt,
    /// mrb-consumer+xml
    MrbConsumerPlusXml,
    /// mrb-publish+xml
    MrbPublishPlusXml,
    /// msc-ivr+xml
    MscIvrPlusXml,
    /// msc-mixer+xml
    MscMixerPlusXml,
    /// msword
    Msword,
    /// mud+json
    MudPlusJson,
    /// multipart-core
    MultipartCore,
    /// mxf
    Mxf,
    /// n-quads
    NQuads,
    /// n-triples
    NTriples,
    /// nasdata
    Nasdata,
    /// news-checkgroups
    NewsCheckgroups,
    /// news-groupinfo
    NewsGroupinfo,
    /// news-transmission
    NewsTransmission,
    /// nlsml+xml
    NlsmlPlusXml,
    /// node
    Node,
    /// nss
    Nss,
    /// oauth-authz-req+jwt
    OauthAuthzReqPlusJwt,
    /// oblivious-dns-message
    ObliviousDnsMessage,
    /// ocsp-request
    OcspRequest,
    /// ocsp-response
    OcspResponse,
    /// octet-stream
    OctetStream,
    /// ODA
    Oda,
    /// odm+xml
    OdmPlusXml,
    /// ODX
    Odx,
    /// oebps-package+xml
    OebpsPackagePlusXml,
    /// ogg
    Ogg,
    /// ohttp-keys
    OhttpKeys,
    /// opc-nodeset+xml
    OpcNodesetPlusXml,
    /// oscore
    Oscore,
    /// oxps
    Oxps,
    /// p21
    P21,
    /// p21+zip
    P21PlusZip,
    /// p2p-overlay+xml
    P2POverlayPlusXml,
    /// parityfec
    Parityfec,
    /// passport
    Passport,
    /// patch-ops-error+xml
    PatchOpsErrorPlusXml,
    /// pdf
    Pdf,
    /// PDX
    Pdx,
    /// pem-certificate-chain
    PemCertificateChain,
    /// pgp-encrypted
    PgpEncrypted,
    /// pgp-keys
    PgpKeys,
    /// pgp-signature
    PgpSignature,
    /// pidf+xml
    PidfPlusXml,
    /// pidf-diff+xml
    PidfDiffPlusXml,
    /// pkcs10
    Pkcs10,
    /// pkcs12
    Pkcs12,
    /// pkcs7-mime
    Pkcs7Mime,
    /// pkcs7-signature
    Pkcs7Signature,
    /// pkcs8
    Pkcs8,
    /// pkcs8-encrypted
    Pkcs8Encrypted,
    /// pkix-attr-cert
    PkixAttrCert,
    /// pkix-cert
    PkixCert,
    /// pkix-crl
    PkixCrl,
    /// pkix-pkipath
    PkixPkipath,
    /// pkixcmp
    Pkixcmp,
    /// pls+xml
    PlsPlusXml,
    /// poc-settings+xml
    PocSettingsPlusXml,
    /// postscript
    Postscript,
    /// ppsp-tracker+json
    PpspTrackerPlusJson,
    /// private-token-issuer-directory
    PrivateTokenIssuerDirectory,
    /// private-token-request
    PrivateTokenRequest,
    /// private-token-response
    PrivateTokenResponse,
    /// problem+json
    ProblemPlusJson,
    /// problem+xml
    ProblemPlusXml,
    /// protobuf
    Protobuf,
    /// protobuf+json
    ProtobufPlusJson,
    /// provenance+xml
    ProvenancePlusXml,
    /// provided-claims+jwt
    ProvidedClaimsPlusJwt,
    /// prs.alvestrand.titrax-sheet
    PrsDotAlvestrandDotTitraxSheet,
    /// prs.bwtc32key
    PrsDotBwtc32Key,
    /// prs.cww
    PrsDotCww,
    /// prs.cyn
    PrsDotCyn,
    /// prs.hpub+zip
    PrsDotHpubPlusZip,
    /// prs.implied-document+xml
    PrsDotImpliedDocumentPlusXml,
    /// prs.implied-executable
    PrsDotImpliedExecutable,
    /// prs.implied-object+json
    PrsDotImpliedObjectPlusJson,
    /// prs.implied-object+json-seq
    PrsDotImpliedObjectPlusJsonSeq,
    /// prs.implied-object+yaml
    PrsDotImpliedObjectPlusYaml,
    /// prs.implied-structure
    PrsDotImpliedStructure,
    /// prs.mayfile
    PrsDotMayfile,
    /// prs.nprend
    PrsDotNprend,
    /// prs.plucker
    PrsDotPlucker,
    /// prs.rdf-xml-crypt
    PrsDotRdfXmlCrypt,
    /// prs.sclt
    PrsDotSclt,
    /// prs.vcfbzip2
    PrsDotVcfbzip2,
    /// prs.xsf+xml
    PrsDotXsfPlusXml,
    /// pskc+xml
    PskcPlusXml,
    /// pvd+json
    PvdPlusJson,
    /// QSIG
    Qsig,
    /// raptorfec
    Raptorfec,
    /// rdap+json
    RdapPlusJson,
    /// rdf+xml
    RdfPlusXml,
    /// reginfo+xml
    ReginfoPlusXml,
    /// relax-ng-compact-syntax
    RelaxNgCompactSyntax,
    /// remote-printing
    RemotePrinting,
    /// reputon+json
    ReputonPlusJson,
    /// resolve-response+jwt
    ResolveResponsePlusJwt,
    /// resource-lists+xml
    ResourceListsPlusXml,
    /// resource-lists-diff+xml
    ResourceListsDiffPlusXml,
    /// rfc+xml
    RfcPlusXml,
    /// riscos
    Riscos,
    /// rlmi+xml
    RlmiPlusXml,
    /// rls-services+xml
    RlsServicesPlusXml,
    /// roughtime-malfeasance+json
    RoughtimeMalfeasancePlusJson,
    /// roughtime-server+json
    RoughtimeServerPlusJson,
    /// route-apd+xml
    RouteApdPlusXml,
    /// route-s-tsid+xml
    RouteSTsidPlusXml,
    /// route-usd+xml
    RouteUsdPlusXml,
    /// rpki-checklist
    RpkiChecklist,
    /// rpki-ghostbusters
    RpkiGhostbusters,
    /// rpki-manifest
    RpkiManifest,
    /// rpki-publication
    RpkiPublication,
    /// rpki-roa
    RpkiRoa,
    /// rpki-signed-tal
    RpkiSignedTal,
    /// rpki-updown
    RpkiUpdown,
    /// rs-metadata+xml
    RsMetadataPlusXml,
    /// rtf
    Rtf,
    /// rtploopback
    Rtploopback,
    /// rtx
    Rtx,
    /// samlassertion+xml
    SamlassertionPlusXml,
    /// samlmetadata+xml
    SamlmetadataPlusXml,
    /// sarif+json
    SarifPlusJson,
    /// sarif-external-properties+json
    SarifExternalPropertiesPlusJson,
    /// sbe
    Sbe,
    /// sbml+xml
    SbmlPlusXml,
    /// scaip+xml
    ScaipPlusXml,
    /// scim+json
    ScimPlusJson,
    /// scitt-receipt+cose
    ScittReceiptPlusCose,
    /// scitt-statement+cose
    ScittStatementPlusCose,
    /// scvp-cv-request
    ScvpCvRequest,
    /// scvp-cv-response
    ScvpCvResponse,
    /// scvp-vp-request
    ScvpVpRequest,
    /// scvp-vp-response
    ScvpVpResponse,
    /// sd-jwt
    SdJwt,
    /// sd-jwt+json
    SdJwtPlusJson,
    /// sdf+json
    SdfPlusJson,
    /// sdp
    Sdp,
    /// secevent+jwt
    SeceventPlusJwt,
    /// senml+cbor
    SenmlPlusCbor,
    /// senml+json
    SenmlPlusJson,
    /// senml+xml
    SenmlPlusXml,
    /// senml-etch+cbor
    SenmlEtchPlusCbor,
    /// senml-etch+json
    SenmlEtchPlusJson,
    /// senml-exi
    SenmlExi,
    /// sensml+cbor
    SensmlPlusCbor,
    /// sensml+json
    SensmlPlusJson,
    /// sensml+xml
    SensmlPlusXml,
    /// sensml-exi
    SensmlExi,
    /// sep+xml
    SepPlusXml,
    /// sep-exi
    SepExi,
    /// session-info
    SessionInfo,
    /// set-payment
    SetPayment,
    /// set-payment-initiation
    SetPaymentInitiation,
    /// set-registration
    SetRegistration,
    /// set-registration-initiation
    SetRegistrationInitiation,
    /// SGML
    Sgml,
    /// sgml-open-catalog
    SgmlOpenCatalog,
    /// shf+xml
    ShfPlusXml,
    /// sieve
    Sieve,
    /// simple-filter+xml
    SimpleFilterPlusXml,
    /// simple-message-summary
    SimpleMessageSummary,
    /// simpleSymbolContainer
    SimpleSymbolContainer,
    /// sipc
    Sipc,
    /// slate
    Slate,
    /// smil
    Smil,
    /// smil+xml
    SmilPlusXml,
    /// smpte336m
    Smpte336M,
    /// soap+fastinfoset
    SoapPlusFastinfoset,
    /// soap+xml
    SoapPlusXml,
    /// sparql-query
    SparqlQuery,
    /// sparql-results+xml
    SparqlResultsPlusXml,
    /// spdx+json
    SpdxPlusJson,
    /// spirits-event+xml
    SpiritsEventPlusXml,
    /// sql
    Sql,
    /// srgs
    Srgs,
    /// srgs+xml
    SrgsPlusXml,
    /// sru+xml
    SruPlusXml,
    /// sslkeylogfile
    Sslkeylogfile,
    /// ssml+xml
    SsmlPlusXml,
    /// ST2110-41
    St211041,
    /// stix+json
    StixPlusJson,
    /// stratum
    Stratum,
    /// suit-envelope+cose
    SuitEnvelopePlusCose,
    /// suit-report+cose
    SuitReportPlusCose,
    /// swid+cbor
    SwidPlusCbor,
    /// swid+xml
    SwidPlusXml,
    /// syslog-msg
    SyslogMsg,
    /// tamp-apex-update
    TampApexUpdate,
    /// tamp-apex-update-confirm
    TampApexUpdateConfirm,
    /// tamp-community-update
    TampCommunityUpdate,
    /// tamp-community-update-confirm
    TampCommunityUpdateConfirm,
    /// tamp-error
    TampError,
    /// tamp-sequence-adjust
    TampSequenceAdjust,
    /// tamp-sequence-adjust-confirm
    TampSequenceAdjustConfirm,
    /// tamp-status-query
    TampStatusQuery,
    /// tamp-status-response
    TampStatusResponse,
    /// tamp-update
    TampUpdate,
    /// tamp-update-confirm
    TampUpdateConfirm,
    /// taxii+json
    TaxiiPlusJson,
    /// td+json
    TdPlusJson,
    /// teep+cbor
    TeepPlusCbor,
    /// tei+xml
    TeiPlusXml,
    /// TETRA_ISI
    TetraIsi,
    /// texinfo
    Texinfo,
    /// thraud+xml
    ThraudPlusXml,
    /// timestamp-query
    TimestampQuery,
    /// timestamp-reply
    TimestampReply,
    /// timestamped-data
    TimestampedData,
    /// tlsrpt+gzip
    TlsrptPlusGzip,
    /// tlsrpt+json
    TlsrptPlusJson,
    /// tm+json
    TmPlusJson,
    /// tnauthlist
    Tnauthlist,
    /// toc+cbor
    TocPlusCbor,
    /// token-introspection+jwt
    TokenIntrospectionPlusJwt,
    /// toml
    Toml,
    /// trickle-ice-sdpfrag
    TrickleIceSdpfrag,
    /// trig
    Trig,
    /// trust-chain+json
    TrustChainPlusJson,
    /// trust-mark+jwt
    TrustMarkPlusJwt,
    /// trust-mark-delegation+jwt
    TrustMarkDelegationPlusJwt,
    /// trust-mark-status-response+jwt
    TrustMarkStatusResponsePlusJwt,
    /// ttml+xml
    TtmlPlusXml,
    /// tve-trigger
    TveTrigger,
    /// tzif
    Tzif,
    /// tzif-leap
    TzifLeap,
    /// uccs+cbor
    UccsPlusCbor,
    /// ujcs+json
    UjcsPlusJson,
    /// ulpfec
    Ulpfec,
    /// urc-grpsheet+xml
    UrcGrpsheetPlusXml,
    /// urc-ressheet+xml
    UrcRessheetPlusXml,
    /// urc-targetdesc+xml
    UrcTargetdescPlusXml,
    /// urc-uisocketdesc+xml
    UrcUisocketdescPlusXml,
    /// v3c
    V3C,
    /// vc
    Vc,
    /// vc+cose
    VcPlusCose,
    /// vc+jwt
    VcPlusJwt,
    /// vc+sd-jwt
    VcPlusSdJwt,
    /// vcard+json
    VcardPlusJson,
    /// vcard+xml
    VcardPlusXml,
    /// vec+xml
    VecPlusXml,
    /// vec-package+gzip
    VecPackagePlusGzip,
    /// vec-package+zip
    VecPackagePlusZip,
    /// vemmi
    Vemmi,
    /// vnd.1000minds.decision-model+xml
    VndDot1000MindsDotDecisionModelPlusXml,
    /// vnd.1ob
    VndDot1Ob,
    /// vnd.3gpp-prose+xml
    VndDot3GppProsePlusXml,
    /// vnd.3gpp-prose-pc3a+xml
    VndDot3GppProsePc3APlusXml,
    /// vnd.3gpp-prose-pc3ach+xml
    VndDot3GppProsePc3AchPlusXml,
    /// vnd.3gpp-prose-pc3ch+xml
    VndDot3GppProsePc3ChPlusXml,
    /// vnd.3gpp-prose-pc8+xml
    VndDot3GppProsePc8PlusXml,
    /// vnd.3gpp-v2x-local-service-information
    VndDot3GppV2XLocalServiceInformation,
    /// vnd.3gpp.5gnas
    VndDot3GppDot5Gnas,
    /// vnd.3gpp.5gsa2x
    VndDot3GppDot5Gsa2X,
    /// vnd.3gpp.5gsa2x-local-service-information
    VndDot3GppDot5Gsa2XLocalServiceInformation,
    /// vnd.3gpp.5gsv2x
    VndDot3GppDot5Gsv2X,
    /// vnd.3gpp.5gsv2x-local-service-information
    VndDot3GppDot5Gsv2XLocalServiceInformation,
    /// vnd.3gpp.access-transfer-events+xml
    VndDot3GppDotAccessTransferEventsPlusXml,
    /// vnd.3gpp.bsf+xml
    VndDot3GppDotBsfPlusXml,
    /// vnd.3gpp.crs+xml
    VndDot3GppDotCrsPlusXml,
    /// vnd.3gpp.current-location-discovery+xml
    VndDot3GppDotCurrentLocationDiscoveryPlusXml,
    /// vnd.3gpp.GMOP+xml
    VndDot3GppDotGmopPlusXml,
    /// vnd.3gpp.gtpc
    VndDot3GppDotGtpc,
    /// vnd.3gpp.interworking-data
    VndDot3GppDotInterworkingData,
    /// vnd.3gpp.lpp
    VndDot3GppDotLpp,
    /// vnd.3gpp.mc-signalling-ear
    VndDot3GppDotMcSignallingEar,
    /// vnd.3gpp.mcdata-affiliation-command+xml
    VndDot3GppDotMcdataAffiliationCommandPlusXml,
    /// vnd.3gpp.mcdata-info+xml
    VndDot3GppDotMcdataInfoPlusXml,
    /// vnd.3gpp.mcdata-msgstore-ctrl-request+xml
    VndDot3GppDotMcdataMsgstoreCtrlRequestPlusXml,
    /// vnd.3gpp.mcdata-payload
    VndDot3GppDotMcdataPayload,
    /// vnd.3gpp.mcdata-regroup+xml
    VndDot3GppDotMcdataRegroupPlusXml,
    /// vnd.3gpp.mcdata-service-config+xml
    VndDot3GppDotMcdataServiceConfigPlusXml,
    /// vnd.3gpp.mcdata-signalling
    VndDot3GppDotMcdataSignalling,
    /// vnd.3gpp.mcdata-ue-config+xml
    VndDot3GppDotMcdataUeConfigPlusXml,
    /// vnd.3gpp.mcdata-user-profile+xml
    VndDot3GppDotMcdataUserProfilePlusXml,
    /// vnd.3gpp.mcptt-affiliation-command+xml
    VndDot3GppDotMcpttAffiliationCommandPlusXml,
    /// vnd.3gpp.mcptt-floor-request+xml
    VndDot3GppDotMcpttFloorRequestPlusXml,
    /// vnd.3gpp.mcptt-info+xml
    VndDot3GppDotMcpttInfoPlusXml,
    /// vnd.3gpp.mcptt-location-info+xml
    VndDot3GppDotMcpttLocationInfoPlusXml,
    /// vnd.3gpp.mcptt-mbms-usage-info+xml
    VndDot3GppDotMcpttMbmsUsageInfoPlusXml,
    /// vnd.3gpp.mcptt-regroup+xml
    VndDot3GppDotMcpttRegroupPlusXml,
    /// vnd.3gpp.mcptt-service-config+xml
    VndDot3GppDotMcpttServiceConfigPlusXml,
    /// vnd.3gpp.mcptt-signed+xml
    VndDot3GppDotMcpttSignedPlusXml,
    /// vnd.3gpp.mcptt-ue-config+xml
    VndDot3GppDotMcpttUeConfigPlusXml,
    /// vnd.3gpp.mcptt-ue-init-config+xml
    VndDot3GppDotMcpttUeInitConfigPlusXml,
    /// vnd.3gpp.mcptt-user-profile+xml
    VndDot3GppDotMcpttUserProfilePlusXml,
    /// vnd.3gpp.mcs-location-user-config+xml
    VndDot3GppDotMcsLocationUserConfigPlusXml,
    /// vnd.3gpp.mcvideo-affiliation-command+xml
    VndDot3GppDotMcvideoAffiliationCommandPlusXml,
    /// vnd.3gpp.mcvideo-affiliation-info+xml
    VndDot3GppDotMcvideoAffiliationInfoPlusXml,
    /// vnd.3gpp.mcvideo-info+xml
    VndDot3GppDotMcvideoInfoPlusXml,
    /// vnd.3gpp.mcvideo-location-info+xml
    VndDot3GppDotMcvideoLocationInfoPlusXml,
    /// vnd.3gpp.mcvideo-mbms-usage-info+xml
    VndDot3GppDotMcvideoMbmsUsageInfoPlusXml,
    /// vnd.3gpp.mcvideo-regroup+xml
    VndDot3GppDotMcvideoRegroupPlusXml,
    /// vnd.3gpp.mcvideo-service-config+xml
    VndDot3GppDotMcvideoServiceConfigPlusXml,
    /// vnd.3gpp.mcvideo-transmission-request+xml
    VndDot3GppDotMcvideoTransmissionRequestPlusXml,
    /// vnd.3gpp.mcvideo-ue-config+xml
    VndDot3GppDotMcvideoUeConfigPlusXml,
    /// vnd.3gpp.mcvideo-user-profile+xml
    VndDot3GppDotMcvideoUserProfilePlusXml,
    /// vnd.3gpp.mid-call+xml
    VndDot3GppDotMidCallPlusXml,
    /// vnd.3gpp.ngap
    VndDot3GppDotNgap,
    /// vnd.3gpp.pfcp
    VndDot3GppDotPfcp,
    /// vnd.3gpp.pic-bw-large
    VndDot3GppDotPicBwLarge,
    /// vnd.3gpp.pic-bw-small
    VndDot3GppDotPicBwSmall,
    /// vnd.3gpp.pic-bw-var
    VndDot3GppDotPicBwVar,
    /// vnd.3gpp.pinapp-info+xml
    VndDot3GppDotPinappInfoPlusXml,
    /// vnd.3gpp.s1ap
    VndDot3GppDotS1Ap,
    /// vnd.3gpp.seal-app-comm-requirements-info+xml
    VndDot3GppDotSealAppCommRequirementsInfoPlusXml,
    /// vnd.3gpp.seal-data-delivery-info+cbor
    VndDot3GppDotSealDataDeliveryInfoPlusCbor,
    /// vnd.3gpp.seal-data-delivery-info+xml
    VndDot3GppDotSealDataDeliveryInfoPlusXml,
    /// vnd.3gpp.seal-group-doc+xml
    VndDot3GppDotSealGroupDocPlusXml,
    /// vnd.3gpp.seal-info+xml
    VndDot3GppDotSealInfoPlusXml,
    /// vnd.3gpp.seal-location-info+cbor
    VndDot3GppDotSealLocationInfoPlusCbor,
    /// vnd.3gpp.seal-location-info+xml
    VndDot3GppDotSealLocationInfoPlusXml,
    /// vnd.3gpp.seal-mbms-usage-info+xml
    VndDot3GppDotSealMbmsUsageInfoPlusXml,
    /// vnd.3gpp.seal-mbs-usage-info+xml
    VndDot3GppDotSealMbsUsageInfoPlusXml,
    /// vnd.3gpp.seal-network-QoS-management-info+xml
    VndDot3GppDotSealNetworkQoSManagementInfoPlusXml,
    /// vnd.3gpp.seal-network-resource-info+cbor
    VndDot3GppDotSealNetworkResourceInfoPlusCbor,
    /// vnd.3gpp.seal-ue-config-info+xml
    VndDot3GppDotSealUeConfigInfoPlusXml,
    /// vnd.3gpp.seal-unicast-info+xml
    VndDot3GppDotSealUnicastInfoPlusXml,
    /// vnd.3gpp.seal-user-profile-info+xml
    VndDot3GppDotSealUserProfileInfoPlusXml,
    /// vnd.3gpp.sms
    VndDot3GppDotSms,
    /// vnd.3gpp.sms+xml
    VndDot3GppDotSmsPlusXml,
    /// vnd.3gpp.srvcc-ext+xml
    VndDot3GppDotSrvccExtPlusXml,
    /// vnd.3gpp.SRVCC-info+xml
    VndDot3GppDotSrvccInfoPlusXml,
    /// vnd.3gpp.state-and-event-info+xml
    VndDot3GppDotStateAndEventInfoPlusXml,
    /// vnd.3gpp.ussd+xml
    VndDot3GppDotUssdPlusXml,
    /// vnd.3gpp.v2x
    VndDot3GppDotV2X,
    /// vnd.3gpp.vae-info+xml
    VndDot3GppDotVaeInfoPlusXml,
    /// vnd.3gpp2.bcmcsinfo+xml
    VndDot3Gpp2DotBcmcsinfoPlusXml,
    /// vnd.3gpp2.sms
    VndDot3Gpp2DotSms,
    /// vnd.3gpp2.tcap
    VndDot3Gpp2DotTcap,
    /// vnd.3lightssoftware.imagescal
    VndDot3LightssoftwareDotImagescal,
    /// vnd.3M.Post-it-Notes
    VndDot3MDotPostItNotes,
    /// vnd.accpac.simply.aso
    VndDotAccpacDotSimplyDotAso,
    /// vnd.accpac.simply.imp
    VndDotAccpacDotSimplyDotImp,
    /// vnd.acm.addressxfer+json
    VndDotAcmDotAddressxferPlusJson,
    /// vnd.acm.chatbot+json
    VndDotAcmDotChatbotPlusJson,
    /// vnd.acucobol
    VndDotAcucobol,
    /// vnd.acucorp
    VndDotAcucorp,
    /// vnd.adobe.flash.movie
    VndDotAdobeDotFlashDotMovie,
    /// vnd.adobe.formscentral.fcdt
    VndDotAdobeDotFormscentralDotFcdt,
    /// vnd.adobe.fxp
    VndDotAdobeDotFxp,
    /// vnd.adobe.partial-upload
    VndDotAdobeDotPartialUpload,
    /// vnd.adobe.xdp+xml
    VndDotAdobeDotXdpPlusXml,
    /// vnd.aether.imp
    VndDotAetherDotImp,
    /// vnd.afpc.afplinedata
    VndDotAfpcDotAfplinedata,
    /// vnd.afpc.afplinedata-pagedef
    VndDotAfpcDotAfplinedataPagedef,
    /// vnd.afpc.cmoca-cmresource
    VndDotAfpcDotCmocaCmresource,
    /// vnd.afpc.foca-charset
    VndDotAfpcDotFocaCharset,
    /// vnd.afpc.foca-codedfont
    VndDotAfpcDotFocaCodedfont,
    /// vnd.afpc.foca-codepage
    VndDotAfpcDotFocaCodepage,
    /// vnd.afpc.modca
    VndDotAfpcDotModca,
    /// vnd.afpc.modca-cmtable
    VndDotAfpcDotModcaCmtable,
    /// vnd.afpc.modca-formdef
    VndDotAfpcDotModcaFormdef,
    /// vnd.afpc.modca-mediummap
    VndDotAfpcDotModcaMediummap,
    /// vnd.afpc.modca-objectcontainer
    VndDotAfpcDotModcaObjectcontainer,
    /// vnd.afpc.modca-overlay
    VndDotAfpcDotModcaOverlay,
    /// vnd.afpc.modca-pagesegment
    VndDotAfpcDotModcaPagesegment,
    /// vnd.age
    VndDotAge,
    /// vnd.ah-barcode
    VndDotAhBarcode,
    /// vnd.ahead.space
    VndDotAheadDotSpace,
    /// vnd.aia
    VndDotAia,
    /// vnd.airzip.filesecure.azf
    VndDotAirzipDotFilesecureDotAzf,
    /// vnd.airzip.filesecure.azs
    VndDotAirzipDotFilesecureDotAzs,
    /// vnd.amadeus+json
    VndDotAmadeusPlusJson,
    /// vnd.amazon.mobi8-ebook
    VndDotAmazonDotMobi8Ebook,
    /// vnd.americandynamics.acc
    VndDotAmericandynamicsDotAcc,
    /// vnd.amiga.ami
    VndDotAmigaDotAmi,
    /// vnd.amundsen.maze+xml
    VndDotAmundsenDotMazePlusXml,
    /// vnd.android.ota
    VndDotAndroidDotOta,
    /// vnd.anki
    VndDotAnki,
    /// vnd.anser-web-certificate-issue-initiation
    VndDotAnserWebCertificateIssueInitiation,
    /// vnd.antix.game-component
    VndDotAntixDotGameComponent,
    /// vnd.apache.arrow.file
    VndDotApacheDotArrowDotFile,
    /// vnd.apache.arrow.stream
    VndDotApacheDotArrowDotStream,
    /// vnd.apache.parquet
    VndDotApacheDotParquet,
    /// vnd.apache.thrift.binary
    VndDotApacheDotThriftDotBinary,
    /// vnd.apache.thrift.compact
    VndDotApacheDotThriftDotCompact,
    /// vnd.apache.thrift.json
    VndDotApacheDotThriftDotJson,
    /// vnd.apexlang
    VndDotApexlang,
    /// vnd.api+json
    VndDotApiPlusJson,
    /// vnd.aplextor.warrp+json
    VndDotAplextorDotWarrpPlusJson,
    /// vnd.apothekende.reservation+json
    VndDotApothekendeDotReservationPlusJson,
    /// vnd.apple.installer+xml
    VndDotAppleDotInstallerPlusXml,
    /// vnd.apple.keynote
    VndDotAppleDotKeynote,
    /// vnd.apple.mpegurl
    VndDotAppleDotMpegurl,
    /// vnd.apple.numbers
    VndDotAppleDotNumbers,
    /// vnd.apple.pages
    VndDotAppleDotPages,
    /// vnd.arastra.swi
    VndDotArastraDotSwi,
    /// vnd.aristanetworks.swi
    VndDotAristanetworksDotSwi,
    /// vnd.artisan+json
    VndDotArtisanPlusJson,
    /// vnd.artsquare
    VndDotArtsquare,
    /// vnd.as207960.vas.config+jer
    VndDotAs207960DotVasDotConfigPlusJer,
    /// vnd.as207960.vas.config+uper
    VndDotAs207960DotVasDotConfigPlusUper,
    /// vnd.as207960.vas.tap+jer
    VndDotAs207960DotVasDotTapPlusJer,
    /// vnd.as207960.vas.tap+uper
    VndDotAs207960DotVasDotTapPlusUper,
    /// vnd.astraea-software.iota
    VndDotAstraeaSoftwareDotIota,
    /// vnd.audiograph
    VndDotAudiograph,
    /// vnd.autopackage
    VndDotAutopackage,
    /// vnd.avalon+json
    VndDotAvalonPlusJson,
    /// vnd.avistar+xml
    VndDotAvistarPlusXml,
    /// vnd.balsamiq.bmml+xml
    VndDotBalsamiqDotBmmlPlusXml,
    /// vnd.balsamiq.bmpr
    VndDotBalsamiqDotBmpr,
    /// vnd.banana-accounting
    VndDotBananaAccounting,
    /// vnd.bbf.usp.error
    VndDotBbfDotUspDotError,
    /// vnd.bbf.usp.msg
    VndDotBbfDotUspDotMsg,
    /// vnd.bbf.usp.msg+json
    VndDotBbfDotUspDotMsgPlusJson,
    /// vnd.bekitzur-stech+json
    VndDotBekitzurStechPlusJson,
    /// vnd.belightsoft.lhzd+zip
    VndDotBelightsoftDotLhzdPlusZip,
    /// vnd.belightsoft.lhzl+zip
    VndDotBelightsoftDotLhzlPlusZip,
    /// vnd.bint.med-content
    VndDotBintDotMedContent,
    /// vnd.biopax.rdf+xml
    VndDotBiopaxDotRdfPlusXml,
    /// vnd.blink-idb-value-wrapper
    VndDotBlinkIdbValueWrapper,
    /// vnd.blueice.multipass
    VndDotBlueiceDotMultipass,
    /// vnd.bluetooth.ep.oob
    VndDotBluetoothDotEpDotOob,
    /// vnd.bluetooth.le.oob
    VndDotBluetoothDotLeDotOob,
    /// vnd.bmi
    VndDotBmi,
    /// vnd.bpf
    VndDotBpf,
    /// vnd.bpf3
    VndDotBpf3,
    /// vnd.businessobjects
    VndDotBusinessobjects,
    /// vnd.byu.uapi+json
    VndDotByuDotUapiPlusJson,
    /// vnd.bzip3
    VndDotBzip3,
    /// vnd.c3voc.schedule+xml
    VndDotC3VocDotSchedulePlusXml,
    /// vnd.cab-jscript
    VndDotCabJscript,
    /// vnd.canon-cpdl
    VndDotCanonCpdl,
    /// vnd.canon-lips
    VndDotCanonLips,
    /// vnd.capasystems-pg+json
    VndDotCapasystemsPgPlusJson,
    /// vnd.cel
    VndDotCel,
    /// vnd.cendio.thinlinc.clientconf
    VndDotCendioDotThinlincDotClientconf,
    /// vnd.century-systems.tcp_stream
    VndDotCenturySystemsDotTcpStream,
    /// vnd.chemdraw+xml
    VndDotChemdrawPlusXml,
    /// vnd.chess-pgn
    VndDotChessPgn,
    /// vnd.chipnuts.karaoke-mmd
    VndDotChipnutsDotKaraokeMmd,
    /// vnd.ciedi
    VndDotCiedi,
    /// vnd.cinderella
    VndDotCinderella,
    /// vnd.cirpack.isdn-ext
    VndDotCirpackDotIsdnExt,
    /// vnd.citationstyles.style+xml
    VndDotCitationstylesDotStylePlusXml,
    /// vnd.claymore
    VndDotClaymore,
    /// vnd.cloanto.rp9
    VndDotCloantoDotRp9,
    /// vnd.clonk.c4group
    VndDotClonkDotC4Group,
    /// vnd.cluetrust.cartomobile-config
    VndDotCluetrustDotCartomobileConfig,
    /// vnd.cluetrust.cartomobile-config-pkg
    VndDotCluetrustDotCartomobileConfigPkg,
    /// vnd.cmmf-configuration-information+json
    VndDotCmmfConfigurationInformationPlusJson,
    /// vnd.cmmf-efd+xml
    VndDotCmmfEfdPlusXml,
    /// vnd.cmmf-encoder-configuration+json
    VndDotCmmfEncoderConfigurationPlusJson,
    /// vnd.cncf.helm.chart.content.v1.tar+gzip
    VndDotCncfDotHelmDotChartDotContentDotV1DotTarPlusGzip,
    /// vnd.cncf.helm.chart.provenance.v1.prov
    VndDotCncfDotHelmDotChartDotProvenanceDotV1DotProv,
    /// vnd.cncf.helm.config.v1+json
    VndDotCncfDotHelmDotConfigDotV1PlusJson,
    /// vnd.coffeescript
    VndDotCoffeescript,
    /// vnd.collabio.xodocuments.document
    VndDotCollabioDotXodocumentsDotDocument,
    /// vnd.collabio.xodocuments.document-template
    VndDotCollabioDotXodocumentsDotDocumentTemplate,
    /// vnd.collabio.xodocuments.presentation
    VndDotCollabioDotXodocumentsDotPresentation,
    /// vnd.collabio.xodocuments.presentation-template
    VndDotCollabioDotXodocumentsDotPresentationTemplate,
    /// vnd.collabio.xodocuments.spreadsheet
    VndDotCollabioDotXodocumentsDotSpreadsheet,
    /// vnd.collabio.xodocuments.spreadsheet-template
    VndDotCollabioDotXodocumentsDotSpreadsheetTemplate,
    /// vnd.collection+json
    VndDotCollectionPlusJson,
    /// vnd.collection.doc+json
    VndDotCollectionDotDocPlusJson,
    /// vnd.collection.next+json
    VndDotCollectionDotNextPlusJson,
    /// vnd.comicbook+zip
    VndDotComicbookPlusZip,
    /// vnd.comicbook-rar
    VndDotComicbookRar,
    /// vnd.commerce-battelle
    VndDotCommerceBattelle,
    /// vnd.commonspace
    VndDotCommonspace,
    /// vnd.contact.cmsg
    VndDotContactDotCmsg,
    /// vnd.coreos.ignition+json
    VndDotCoreosDotIgnitionPlusJson,
    /// vnd.cosmocaller
    VndDotCosmocaller,
    /// vnd.crick.clicker
    VndDotCrickDotClicker,
    /// vnd.crick.clicker.keyboard
    VndDotCrickDotClickerDotKeyboard,
    /// vnd.crick.clicker.palette
    VndDotCrickDotClickerDotPalette,
    /// vnd.crick.clicker.template
    VndDotCrickDotClickerDotTemplate,
    /// vnd.crick.clicker.wordbank
    VndDotCrickDotClickerDotWordbank,
    /// vnd.criticaltools.wbs+xml
    VndDotCriticaltoolsDotWbsPlusXml,
    /// vnd.cryptii.pipe+json
    VndDotCryptiiDotPipePlusJson,
    /// vnd.crypto-shade-file
    VndDotCryptoShadeFile,
    /// vnd.cryptomator.encrypted
    VndDotCryptomatorDotEncrypted,
    /// vnd.cryptomator.vault
    VndDotCryptomatorDotVault,
    /// vnd.ctc-posml
    VndDotCtcPosml,
    /// vnd.ctct.ws+xml
    VndDotCtctDotWsPlusXml,
    /// vnd.cups-pdf
    VndDotCupsPdf,
    /// vnd.cups-postscript
    VndDotCupsPostscript,
    /// vnd.cups-ppd
    VndDotCupsPpd,
    /// vnd.cups-raster
    VndDotCupsRaster,
    /// vnd.cups-raw
    VndDotCupsRaw,
    /// vnd.curl
    VndDotCurl,
    /// vnd.cyan.dean.root+xml
    VndDotCyanDotDeanDotRootPlusXml,
    /// vnd.cybank
    VndDotCybank,
    /// vnd.cyclonedx+json
    VndDotCyclonedxPlusJson,
    /// vnd.cyclonedx+xml
    VndDotCyclonedxPlusXml,
    /// vnd.d2l.coursepackage1p0+zip
    VndDotD2LDotCoursepackage1P0PlusZip,
    /// vnd.d3m-dataset
    VndDotD3MDataset,
    /// vnd.d3m-problem
    VndDotD3MProblem,
    /// vnd.dart
    VndDotDart,
    /// vnd.data-vision.rdz
    VndDotDataVisionDotRdz,
    /// vnd.datalog
    VndDotDatalog,
    /// vnd.datapackage+json
    VndDotDatapackagePlusJson,
    /// vnd.dataresource+json
    VndDotDataresourcePlusJson,
    /// vnd.dbf
    VndDotDbf,
    /// vnd.dcmp+xml
    VndDotDcmpPlusXml,
    /// vnd.debian.binary-package
    VndDotDebianDotBinaryPackage,
    /// vnd.dece.data
    VndDotDeceDotData,
    /// vnd.dece.ttml+xml
    VndDotDeceDotTtmlPlusXml,
    /// vnd.dece.unspecified
    VndDotDeceDotUnspecified,
    /// vnd.dece.zip
    VndDotDeceDotZip,
    /// vnd.denovo.fcselayout-link
    VndDotDenovoDotFcselayoutLink,
    /// vnd.desmume.movie
    VndDotDesmumeDotMovie,
    /// vnd.deut+json
    VndDotDeutPlusJson,
    /// vnd.dir-bi.plate-dl-nosuffix
    VndDotDirBiDotPlateDlNosuffix,
    /// vnd.dm.delegation+xml
    VndDotDmDotDelegationPlusXml,
    /// vnd.dna
    VndDotDna,
    /// vnd.document+json
    VndDotDocumentPlusJson,
    /// vnd.dolby.mobile.1
    VndDotDolbyDotMobileDot1,
    /// vnd.dolby.mobile.2
    VndDotDolbyDotMobileDot2,
    /// vnd.doremir.scorecloud-binary-document
    VndDotDoremirDotScorecloudBinaryDocument,
    /// vnd.dpgraph
    VndDotDpgraph,
    /// vnd.dreamfactory
    VndDotDreamfactory,
    /// vnd.drive+json
    VndDotDrivePlusJson,
    /// vnd.dtg.local
    VndDotDtgDotLocal,
    /// vnd.dtg.local.flash
    VndDotDtgDotLocalDotFlash,
    /// vnd.dtg.local.html
    VndDotDtgDotLocalDotHtml,
    /// vnd.dvb.ait
    VndDotDvbDotAit,
    /// vnd.dvb.dvbisl+xml
    VndDotDvbDotDvbislPlusXml,
    /// vnd.dvb.dvbj
    VndDotDvbDotDvbj,
    /// vnd.dvb.esgcontainer
    VndDotDvbDotEsgcontainer,
    /// vnd.dvb.ipdcdftnotifaccess
    VndDotDvbDotIpdcdftnotifaccess,
    /// vnd.dvb.ipdcesgaccess
    VndDotDvbDotIpdcesgaccess,
    /// vnd.dvb.ipdcesgaccess2
    VndDotDvbDotIpdcesgaccess2,
    /// vnd.dvb.ipdcesgpdd
    VndDotDvbDotIpdcesgpdd,
    /// vnd.dvb.ipdcroaming
    VndDotDvbDotIpdcroaming,
    /// vnd.dvb.iptv.alfec-base
    VndDotDvbDotIptvDotAlfecBase,
    /// vnd.dvb.iptv.alfec-enhancement
    VndDotDvbDotIptvDotAlfecEnhancement,
    /// vnd.dvb.notif-aggregate-root+xml
    VndDotDvbDotNotifAggregateRootPlusXml,
    /// vnd.dvb.notif-container+xml
    VndDotDvbDotNotifContainerPlusXml,
    /// vnd.dvb.notif-generic+xml
    VndDotDvbDotNotifGenericPlusXml,
    /// vnd.dvb.notif-ia-msglist+xml
    VndDotDvbDotNotifIaMsglistPlusXml,
    /// vnd.dvb.notif-ia-registration-request+xml
    VndDotDvbDotNotifIaRegistrationRequestPlusXml,
    /// vnd.dvb.notif-ia-registration-response+xml
    VndDotDvbDotNotifIaRegistrationResponsePlusXml,
    /// vnd.dvb.notif-init+xml
    VndDotDvbDotNotifInitPlusXml,
    /// vnd.dvb.pfr
    VndDotDvbDotPfr,
    /// vnd.dvb.service
    VndDotDvbDotService,
    /// vnd.dxr
    VndDotDxr,
    /// vnd.dynageo
    VndDotDynageo,
    /// vnd.dzr
    VndDotDzr,
    /// vnd.easykaraoke.cdgdownload
    VndDotEasykaraokeDotCdgdownload,
    /// vnd.ecdis-update
    VndDotEcdisUpdate,
    /// vnd.ecip.rlp
    VndDotEcipDotRlp,
    /// vnd.eclipse.ditto+json
    VndDotEclipseDotDittoPlusJson,
    /// vnd.ecowin.chart
    VndDotEcowinDotChart,
    /// vnd.ecowin.filerequest
    VndDotEcowinDotFilerequest,
    /// vnd.ecowin.fileupdate
    VndDotEcowinDotFileupdate,
    /// vnd.ecowin.series
    VndDotEcowinDotSeries,
    /// vnd.ecowin.seriesrequest
    VndDotEcowinDotSeriesrequest,
    /// vnd.ecowin.seriesupdate
    VndDotEcowinDotSeriesupdate,
    /// vnd.edulith.edux+json
    VndDotEdulithDotEduxPlusJson,
    /// vnd.efi.img
    VndDotEfiDotImg,
    /// vnd.efi.iso
    VndDotEfiDotIso,
    /// vnd.eln+zip
    VndDotElnPlusZip,
    /// vnd.emclient.accessrequest+xml
    VndDotEmclientDotAccessrequestPlusXml,
    /// vnd.enliven
    VndDotEnliven,
    /// vnd.enphase.envoy
    VndDotEnphaseDotEnvoy,
    /// vnd.eprints.data+xml
    VndDotEprintsDotDataPlusXml,
    /// vnd.epson.esf
    VndDotEpsonDotEsf,
    /// vnd.epson.msf
    VndDotEpsonDotMsf,
    /// vnd.epson.quickanime
    VndDotEpsonDotQuickanime,
    /// vnd.epson.salt
    VndDotEpsonDotSalt,
    /// vnd.epson.ssf
    VndDotEpsonDotSsf,
    /// vnd.ericsson.quickcall
    VndDotEricssonDotQuickcall,
    /// vnd.erofs
    VndDotErofs,
    /// vnd.espass-espass+zip
    VndDotEspassEspassPlusZip,
    /// vnd.eszigno3+xml
    VndDotEszigno3PlusXml,
    /// vnd.etsi.aoc+xml
    VndDotEtsiDotAocPlusXml,
    /// vnd.etsi.asic-e+zip
    VndDotEtsiDotAsicEPlusZip,
    /// vnd.etsi.asic-s+zip
    VndDotEtsiDotAsicSPlusZip,
    /// vnd.etsi.cug+xml
    VndDotEtsiDotCugPlusXml,
    /// vnd.etsi.iptvcommand+xml
    VndDotEtsiDotIptvcommandPlusXml,
    /// vnd.etsi.iptvdiscovery+xml
    VndDotEtsiDotIptvdiscoveryPlusXml,
    /// vnd.etsi.iptvprofile+xml
    VndDotEtsiDotIptvprofilePlusXml,
    /// vnd.etsi.iptvsad-bc+xml
    VndDotEtsiDotIptvsadBcPlusXml,
    /// vnd.etsi.iptvsad-cod+xml
    VndDotEtsiDotIptvsadCodPlusXml,
    /// vnd.etsi.iptvsad-npvr+xml
    VndDotEtsiDotIptvsadNpvrPlusXml,
    /// vnd.etsi.iptvservice+xml
    VndDotEtsiDotIptvservicePlusXml,
    /// vnd.etsi.iptvsync+xml
    VndDotEtsiDotIptvsyncPlusXml,
    /// vnd.etsi.iptvueprofile+xml
    VndDotEtsiDotIptvueprofilePlusXml,
    /// vnd.etsi.mcid+xml
    VndDotEtsiDotMcidPlusXml,
    /// vnd.etsi.mheg5
    VndDotEtsiDotMheg5,
    /// vnd.etsi.overload-control-policy-dataset+xml
    VndDotEtsiDotOverloadControlPolicyDatasetPlusXml,
    /// vnd.etsi.pstn+xml
    VndDotEtsiDotPstnPlusXml,
    /// vnd.etsi.sci+xml
    VndDotEtsiDotSciPlusXml,
    /// vnd.etsi.simservs+xml
    VndDotEtsiDotSimservsPlusXml,
    /// vnd.etsi.timestamp-token
    VndDotEtsiDotTimestampToken,
    /// vnd.etsi.tsl+xml
    VndDotEtsiDotTslPlusXml,
    /// vnd.etsi.tsl.der
    VndDotEtsiDotTslDotDer,
    /// vnd.eu.kasparian.car+json
    VndDotEuDotKasparianDotCarPlusJson,
    /// vnd.eudora.data
    VndDotEudoraDotData,
    /// vnd.evolv.ecig.profile
    VndDotEvolvDotEcigDotProfile,
    /// vnd.evolv.ecig.settings
    VndDotEvolvDotEcigDotSettings,
    /// vnd.evolv.ecig.theme
    VndDotEvolvDotEcigDotTheme,
    /// vnd.exstream-empower+zip
    VndDotExstreamEmpowerPlusZip,
    /// vnd.exstream-package
    VndDotExstreamPackage,
    /// vnd.ezpix-album
    VndDotEzpixAlbum,
    /// vnd.ezpix-package
    VndDotEzpixPackage,
    /// vnd.f-secure.mobile
    VndDotFSecureDotMobile,
    /// vnd.faf+yaml
    VndDotFafPlusYaml,
    /// vnd.familysearch.gedcom+zip
    VndDotFamilysearchDotGedcomPlusZip,
    /// vnd.fastcopy-disk-image
    VndDotFastcopyDiskImage,
    /// vnd.fdsn.mseed
    VndDotFdsnDotMseed,
    /// vnd.fdsn.seed
    VndDotFdsnDotSeed,
    /// vnd.fdsn.stationxml+xml
    VndDotFdsnDotStationxmlPlusXml,
    /// vnd.ffsns
    VndDotFfsns,
    /// vnd.fgb
    VndDotFgb,
    /// vnd.ficlab.flb+zip
    VndDotFiclabDotFlbPlusZip,
    /// vnd.filmit.zfc
    VndDotFilmitDotZfc,
    /// vnd.fints
    VndDotFints,
    /// vnd.firemonkeys.cloudcell
    VndDotFiremonkeysDotCloudcell,
    /// vnd.FloGraphIt
    VndDotFloGraphIt,
    /// vnd.fluxtime.clip
    VndDotFluxtimeDotClip,
    /// vnd.font-fontforge-sfd
    VndDotFontFontforgeSfd,
    /// vnd.foritech.container
    VndDotForitechDotContainer,
    /// vnd.framemaker
    VndDotFramemaker,
    /// vnd.freelog.comic
    VndDotFreelogDotComic,
    /// vnd.frogans.fnc
    VndDotFrogansDotFnc,
    /// vnd.frogans.ltf
    VndDotFrogansDotLtf,
    /// vnd.fsc.weblaunch
    VndDotFscDotWeblaunch,
    /// vnd.fujifilm.fb.docuworks
    VndDotFujifilmDotFbDotDocuworks,
    /// vnd.fujifilm.fb.docuworks.binder
    VndDotFujifilmDotFbDotDocuworksDotBinder,
    /// vnd.fujifilm.fb.docuworks.container
    VndDotFujifilmDotFbDotDocuworksDotContainer,
    /// vnd.fujifilm.fb.jfi+xml
    VndDotFujifilmDotFbDotJfiPlusXml,
    /// vnd.fujitsu.oasys
    VndDotFujitsuDotOasys,
    /// vnd.fujitsu.oasys2
    VndDotFujitsuDotOasys2,
    /// vnd.fujitsu.oasys3
    VndDotFujitsuDotOasys3,
    /// vnd.fujitsu.oasysgp
    VndDotFujitsuDotOasysgp,
    /// vnd.fujitsu.oasysprs
    VndDotFujitsuDotOasysprs,
    /// vnd.fujixerox.ART-EX
    VndDotFujixeroxDotArtEx,
    /// vnd.fujixerox.ART4
    VndDotFujixeroxDotArt4,
    /// vnd.fujixerox.ddd
    VndDotFujixeroxDotDdd,
    /// vnd.fujixerox.docuworks
    VndDotFujixeroxDotDocuworks,
    /// vnd.fujixerox.docuworks.binder
    VndDotFujixeroxDotDocuworksDotBinder,
    /// vnd.fujixerox.docuworks.container
    VndDotFujixeroxDotDocuworksDotContainer,
    /// vnd.fujixerox.HBPL
    VndDotFujixeroxDotHbpl,
    /// vnd.fut-misnet
    VndDotFutMisnet,
    /// vnd.futoin+cbor
    VndDotFutoinPlusCbor,
    /// vnd.futoin+json
    VndDotFutoinPlusJson,
    /// vnd.fuzzysheet
    VndDotFuzzysheet,
    /// vnd.g3pix.g3fc
    VndDotG3PixDotG3Fc,
    /// vnd.ga4gh.passport+jwt
    VndDotGa4GhDotPassportPlusJwt,
    /// vnd.genomatix.tuxedo
    VndDotGenomatixDotTuxedo,
    /// vnd.genozip
    VndDotGenozip,
    /// vnd.gentics.grd+json
    VndDotGenticsDotGrdPlusJson,
    /// vnd.gentoo.catmetadata+xml
    VndDotGentooDotCatmetadataPlusXml,
    /// vnd.gentoo.ebuild
    VndDotGentooDotEbuild,
    /// vnd.gentoo.eclass
    VndDotGentooDotEclass,
    /// vnd.gentoo.gpkg
    VndDotGentooDotGpkg,
    /// vnd.gentoo.manifest
    VndDotGentooDotManifest,
    /// vnd.gentoo.pkgmetadata+xml
    VndDotGentooDotPkgmetadataPlusXml,
    /// vnd.gentoo.xpak
    VndDotGentooDotXpak,
    /// vnd.geo+json
    VndDotGeoPlusJson,
    /// vnd.geocube+xml
    VndDotGeocubePlusXml,
    /// vnd.geogebra.file
    VndDotGeogebraDotFile,
    /// vnd.geogebra.pinboard
    VndDotGeogebraDotPinboard,
    /// vnd.geogebra.slides
    VndDotGeogebraDotSlides,
    /// vnd.geogebra.tool
    VndDotGeogebraDotTool,
    /// vnd.geometry-explorer
    VndDotGeometryExplorer,
    /// vnd.geonext
    VndDotGeonext,
    /// vnd.geoplan
    VndDotGeoplan,
    /// vnd.geospace
    VndDotGeospace,
    /// vnd.gerber
    VndDotGerber,
    /// vnd.globalplatform.card-content-mgt
    VndDotGlobalplatformDotCardContentMgt,
    /// vnd.globalplatform.card-content-mgt-response
    VndDotGlobalplatformDotCardContentMgtResponse,
    /// vnd.gmx
    VndDotGmxDeprecated,
    /// vnd.gnu.taler.exchange+json
    VndDotGnuDotTalerDotExchangePlusJson,
    /// vnd.gnu.taler.merchant+json
    VndDotGnuDotTalerDotMerchantPlusJson,
    /// vnd.google-earth.kml+xml
    VndDotGoogleEarthDotKmlPlusXml,
    /// vnd.google-earth.kmz
    VndDotGoogleEarthDotKmz,
    /// vnd.gov.sk.e-form+xml
    VndDotGovDotSkDotEFormPlusXml,
    /// vnd.gov.sk.e-form+zip
    VndDotGovDotSkDotEFormPlusZip,
    /// vnd.gov.sk.xmldatacontainer+xml
    VndDotGovDotSkDotXmldatacontainerPlusXml,
    /// vnd.gp3
    VndDotGp3,
    /// vnd.gpxsee.map+xml
    VndDotGpxseeDotMapPlusXml,
    /// vnd.grafeq
    VndDotGrafeq,
    /// vnd.gridmp
    VndDotGridmp,
    /// vnd.groove-account
    VndDotGrooveAccount,
    /// vnd.groove-help
    VndDotGrooveHelp,
    /// vnd.groove-identity-message
    VndDotGrooveIdentityMessage,
    /// vnd.groove-injector
    VndDotGrooveInjector,
    /// vnd.groove-tool-message
    VndDotGrooveToolMessage,
    /// vnd.groove-tool-template
    VndDotGrooveToolTemplate,
    /// vnd.groove-vcard
    VndDotGrooveVcard,
    /// vnd.hal+json
    VndDotHalPlusJson,
    /// vnd.hal+xml
    VndDotHalPlusXml,
    /// vnd.HandHeld-Entertainment+xml
    VndDotHandHeldEntertainmentPlusXml,
    /// vnd.hbci
    VndDotHbci,
    /// vnd.hc+json
    VndDotHcPlusJson,
    /// vnd.hcl-bireports
    VndDotHclBireports,
    /// vnd.hdfgroup.hdf4
    VndDotHdfgroupDotHdf4,
    /// vnd.hdfgroup.hdf5
    VndDotHdfgroupDotHdf5,
    /// vnd.hdt
    VndDotHdt,
    /// vnd.heroku+json
    VndDotHerokuPlusJson,
    /// vnd.hhe.lesson-player
    VndDotHheDotLessonPlayer,
    /// vnd.hp-HPGL
    VndDotHpHpgl,
    /// vnd.hp-hpid
    VndDotHpHpid,
    /// vnd.hp-hps
    VndDotHpHps,
    /// vnd.hp-jlyt
    VndDotHpJlyt,
    /// vnd.hp-PCL
    VndDotHpPcl,
    /// vnd.hp-PCLXL
    VndDotHpPclxl,
    /// vnd.hsl
    VndDotHsl,
    /// vnd.httphone
    VndDotHttphone,
    /// vnd.hydrostatix.sof-data
    VndDotHydrostatixDotSofData,
    /// vnd.hyper+json
    VndDotHyperPlusJson,
    /// vnd.hyper-item+json
    VndDotHyperItemPlusJson,
    /// vnd.hyperdrive+json
    VndDotHyperdrivePlusJson,
    /// vnd.hzn-3d-crossword
    VndDotHzn3DCrossword,
    /// vnd.ibm.afplinedata
    VndDotIbmDotAfplinedata,
    /// vnd.ibm.electronic-media
    VndDotIbmDotElectronicMedia,
    /// vnd.ibm.MiniPay
    VndDotIbmDotMiniPay,
    /// vnd.ibm.modcap
    VndDotIbmDotModcap,
    /// vnd.ibm.rights-management
    VndDotIbmDotRightsManagement,
    /// vnd.ibm.secure-container
    VndDotIbmDotSecureContainer,
    /// vnd.iccprofile
    VndDotIccprofile,
    /// vnd.ieee.1905
    VndDotIeeeDot1905,
    /// vnd.igloader
    VndDotIgloader,
    /// vnd.imagemeter.folder+zip
    VndDotImagemeterDotFolderPlusZip,
    /// vnd.imagemeter.image+zip
    VndDotImagemeterDotImagePlusZip,
    /// vnd.immervision-ivp
    VndDotImmervisionIvp,
    /// vnd.immervision-ivu
    VndDotImmervisionIvu,
    /// vnd.ims.imsccv1p1
    VndDotImsDotImsccv1P1,
    /// vnd.ims.imsccv1p2
    VndDotImsDotImsccv1P2,
    /// vnd.ims.imsccv1p3
    VndDotImsDotImsccv1P3,
    /// vnd.ims.lis.v2.result+json
    VndDotImsDotLisDotV2DotResultPlusJson,
    /// vnd.ims.lti.v2.toolconsumerprofile+json
    VndDotImsDotLtiDotV2DotToolconsumerprofilePlusJson,
    /// vnd.ims.lti.v2.toolproxy+json
    VndDotImsDotLtiDotV2DotToolproxyPlusJson,
    /// vnd.ims.lti.v2.toolproxy.id+json
    VndDotImsDotLtiDotV2DotToolproxyDotIdPlusJson,
    /// vnd.ims.lti.v2.toolsettings+json
    VndDotImsDotLtiDotV2DotToolsettingsPlusJson,
    /// vnd.ims.lti.v2.toolsettings.simple+json
    VndDotImsDotLtiDotV2DotToolsettingsDotSimplePlusJson,
    /// vnd.informedcontrol.rms+xml
    VndDotInformedcontrolDotRmsPlusXml,
    /// vnd.informix-visionary
    VndDotInformixVisionary,
    /// vnd.infotech.project
    VndDotInfotechDotProject,
    /// vnd.infotech.project+xml
    VndDotInfotechDotProjectPlusXml,
    /// vnd.innopath.wamp.notification
    VndDotInnopathDotWampDotNotification,
    /// vnd.insors.igm
    VndDotInsorsDotIgm,
    /// vnd.intercon.formnet
    VndDotInterconDotFormnet,
    /// vnd.intergeo
    VndDotIntergeo,
    /// vnd.intertrust.digibox
    VndDotIntertrustDotDigibox,
    /// vnd.intertrust.nncp
    VndDotIntertrustDotNncp,
    /// vnd.intu.qbo
    VndDotIntuDotQbo,
    /// vnd.intu.qfx
    VndDotIntuDotQfx,
    /// vnd.ipfs.ipns-record
    VndDotIpfsDotIpnsRecord,
    /// vnd.ipld.car
    VndDotIpldDotCar,
    /// vnd.ipld.dag-cbor
    VndDotIpldDotDagCbor,
    /// vnd.ipld.dag-json
    VndDotIpldDotDagJson,
    /// vnd.ipld.raw
    VndDotIpldDotRaw,
    /// vnd.iptc.g2.catalogitem+xml
    VndDotIptcDotG2DotCatalogitemPlusXml,
    /// vnd.iptc.g2.conceptitem+xml
    VndDotIptcDotG2DotConceptitemPlusXml,
    /// vnd.iptc.g2.knowledgeitem+xml
    VndDotIptcDotG2DotKnowledgeitemPlusXml,
    /// vnd.iptc.g2.newsitem+xml
    VndDotIptcDotG2DotNewsitemPlusXml,
    /// vnd.iptc.g2.newsmessage+xml
    VndDotIptcDotG2DotNewsmessagePlusXml,
    /// vnd.iptc.g2.packageitem+xml
    VndDotIptcDotG2DotPackageitemPlusXml,
    /// vnd.iptc.g2.planningitem+xml
    VndDotIptcDotG2DotPlanningitemPlusXml,
    /// vnd.ipunplugged.rcprofile
    VndDotIpunpluggedDotRcprofile,
    /// vnd.irepository.package+xml
    VndDotIrepositoryDotPackagePlusXml,
    /// vnd.is-xpr
    VndDotIsXpr,
    /// vnd.isac.fcs
    VndDotIsacDotFcs,
    /// vnd.iso11783-10+zip
    VndDotIso1178310PlusZip,
    /// vnd.jam
    VndDotJam,
    /// vnd.japannet-directory-service
    VndDotJapannetDirectoryService,
    /// vnd.japannet-jpnstore-wakeup
    VndDotJapannetJpnstoreWakeup,
    /// vnd.japannet-payment-wakeup
    VndDotJapannetPaymentWakeup,
    /// vnd.japannet-registration
    VndDotJapannetRegistration,
    /// vnd.japannet-registration-wakeup
    VndDotJapannetRegistrationWakeup,
    /// vnd.japannet-setstore-wakeup
    VndDotJapannetSetstoreWakeup,
    /// vnd.japannet-verification
    VndDotJapannetVerification,
    /// vnd.japannet-verification-wakeup
    VndDotJapannetVerificationWakeup,
    /// vnd.jcp.javame.midlet-rms
    VndDotJcpDotJavameDotMidletRms,
    /// vnd.jisp
    VndDotJisp,
    /// vnd.joost.joda-archive
    VndDotJoostDotJodaArchive,
    /// vnd.jsk.isdn-ngn
    VndDotJskDotIsdnNgn,
    /// vnd.kahootz
    VndDotKahootz,
    /// vnd.kde.karbon
    VndDotKdeDotKarbon,
    /// vnd.kde.kchart
    VndDotKdeDotKchart,
    /// vnd.kde.kformula
    VndDotKdeDotKformula,
    /// vnd.kde.kivio
    VndDotKdeDotKivio,
    /// vnd.kde.kontour
    VndDotKdeDotKontour,
    /// vnd.kde.kpresenter
    VndDotKdeDotKpresenter,
    /// vnd.kde.kspread
    VndDotKdeDotKspread,
    /// vnd.kde.kword
    VndDotKdeDotKword,
    /// vnd.kdl
    VndDotKdl,
    /// vnd.kenameaapp
    VndDotKenameaapp,
    /// vnd.keyman.kmp+zip
    VndDotKeymanDotKmpPlusZip,
    /// vnd.keyman.kmx
    VndDotKeymanDotKmx,
    /// vnd.kidspiration
    VndDotKidspiration,
    /// vnd.Kinar
    VndDotKinar,
    /// vnd.koan
    VndDotKoan,
    /// vnd.kodak-descriptor
    VndDotKodakDescriptor,
    /// vnd.las
    VndDotLas,
    /// vnd.las.las+json
    VndDotLasDotLasPlusJson,
    /// vnd.las.las+xml
    VndDotLasDotLasPlusXml,
    /// vnd.laszip
    VndDotLaszip,
    /// vnd.ldev.productlicensing
    VndDotLdevDotProductlicensing,
    /// vnd.leap+json
    VndDotLeapPlusJson,
    /// vnd.liberty-request+xml
    VndDotLibertyRequestPlusXml,
    /// vnd.llamagraphics.life-balance.desktop
    VndDotLlamagraphicsDotLifeBalanceDotDesktop,
    /// vnd.llamagraphics.life-balance.exchange+xml
    VndDotLlamagraphicsDotLifeBalanceDotExchangePlusXml,
    /// vnd.logipipe.circuit+zip
    VndDotLogipipeDotCircuitPlusZip,
    /// vnd.loom
    VndDotLoom,
    /// vnd.lotus-1-2-3
    VndDotLotus123,
    /// vnd.lotus-approach
    VndDotLotusApproach,
    /// vnd.lotus-freelance
    VndDotLotusFreelance,
    /// vnd.lotus-notes
    VndDotLotusNotes,
    /// vnd.lotus-organizer
    VndDotLotusOrganizer,
    /// vnd.lotus-screencam
    VndDotLotusScreencam,
    /// vnd.lotus-wordpro
    VndDotLotusWordpro,
    /// vnd.macports.portpkg
    VndDotMacportsDotPortpkg,
    /// vnd.majikah.bundle
    VndDotMajikahDotBundle,
    /// vnd.maml
    VndDotMaml,
    /// vnd.mapbox-vector-tile
    VndDotMapboxVectorTile,
    /// vnd.marlin.drm.actiontoken+xml
    VndDotMarlinDotDrmDotActiontokenPlusXml,
    /// vnd.marlin.drm.conftoken+xml
    VndDotMarlinDotDrmDotConftokenPlusXml,
    /// vnd.marlin.drm.license+xml
    VndDotMarlinDotDrmDotLicensePlusXml,
    /// vnd.marlin.drm.mdcf
    VndDotMarlinDotDrmDotMdcf,
    /// vnd.mason+json
    VndDotMasonPlusJson,
    /// vnd.maxar.archive.3tz+zip
    VndDotMaxarDotArchiveDot3TzPlusZip,
    /// vnd.maxmind.maxmind-db
    VndDotMaxmindDotMaxmindDb,
    /// vnd.mcd
    VndDotMcd,
    /// vnd.mdl
    VndDotMdl,
    /// vnd.mdl-mbsdf
    VndDotMdlMbsdf,
    /// vnd.medcalcdata
    VndDotMedcalcdata,
    /// vnd.mediastation.cdkey
    VndDotMediastationDotCdkey,
    /// vnd.medicalholodeck.recordxr
    VndDotMedicalholodeckDotRecordxr,
    /// vnd.meridian-slingshot
    VndDotMeridianSlingshot,
    /// vnd.mermaid
    VndDotMermaid,
    /// vnd.MFER
    VndDotMfer,
    /// vnd.mfmp
    VndDotMfmp,
    /// vnd.micro+json
    VndDotMicroPlusJson,
    /// vnd.micrografx.flo
    VndDotMicrografxDotFlo,
    /// vnd.micrografx.igx
    VndDotMicrografxDotIgx,
    /// vnd.microsoft.portable-executable
    VndDotMicrosoftDotPortableExecutable,
    /// vnd.microsoft.windows.thumbnail-cache
    VndDotMicrosoftDotWindowsDotThumbnailCache,
    /// vnd.miele+json
    VndDotMielePlusJson,
    /// vnd.mif
    VndDotMif,
    /// vnd.minisoft-hp3000-save
    VndDotMinisoftHp3000Save,
    /// vnd.mitsubishi.misty-guard.trustweb
    VndDotMitsubishiDotMistyGuardDotTrustweb,
    /// vnd.Mobius.DAF
    VndDotMobiusDotDaf,
    /// vnd.Mobius.DIS
    VndDotMobiusDotDis,
    /// vnd.Mobius.MBK
    VndDotMobiusDotMbk,
    /// vnd.Mobius.MQY
    VndDotMobiusDotMqy,
    /// vnd.Mobius.MSL
    VndDotMobiusDotMsl,
    /// vnd.Mobius.PLC
    VndDotMobiusDotPlc,
    /// vnd.Mobius.TXF
    VndDotMobiusDotTxf,
    /// vnd.modl
    VndDotModl,
    /// vnd.mophun.application
    VndDotMophunDotApplication,
    /// vnd.mophun.certificate
    VndDotMophunDotCertificate,
    /// vnd.motorola.flexsuite
    VndDotMotorolaDotFlexsuite,
    /// vnd.motorola.flexsuite.adsi
    VndDotMotorolaDotFlexsuiteDotAdsi,
    /// vnd.motorola.flexsuite.fis
    VndDotMotorolaDotFlexsuiteDotFis,
    /// vnd.motorola.flexsuite.gotap
    VndDotMotorolaDotFlexsuiteDotGotap,
    /// vnd.motorola.flexsuite.kmr
    VndDotMotorolaDotFlexsuiteDotKmr,
    /// vnd.motorola.flexsuite.ttc
    VndDotMotorolaDotFlexsuiteDotTtc,
    /// vnd.motorola.flexsuite.wem
    VndDotMotorolaDotFlexsuiteDotWem,
    /// vnd.motorola.iprm
    VndDotMotorolaDotIprm,
    /// vnd.mozilla.xul+xml
    VndDotMozillaDotXulPlusXml,
    /// vnd.ms-3mfdocument
    VndDotMs3Mfdocument,
    /// vnd.ms-artgalry
    VndDotMsArtgalry,
    /// vnd.ms-asf
    VndDotMsAsf,
    /// vnd.ms-cab-compressed
    VndDotMsCabCompressed,
    /// vnd.ms-excel
    VndDotMsExcel,
    /// vnd.ms-excel.addin.macroEnabled.12
    VndDotMsExcelDotAddinDotMacroEnabledDot12,
    /// vnd.ms-excel.sheet.binary.macroEnabled.12
    VndDotMsExcelDotSheetDotBinaryDotMacroEnabledDot12,
    /// vnd.ms-excel.sheet.macroEnabled.12
    VndDotMsExcelDotSheetDotMacroEnabledDot12,
    /// vnd.ms-excel.template.macroEnabled.12
    VndDotMsExcelDotTemplateDotMacroEnabledDot12,
    /// vnd.ms-fontobject
    VndDotMsFontobject,
    /// vnd.ms-htmlhelp
    VndDotMsHtmlhelp,
    /// vnd.ms-ims
    VndDotMsIms,
    /// vnd.ms-lrm
    VndDotMsLrm,
    /// vnd.ms-office.activeX+xml
    VndDotMsOfficeDotActiveXPlusXml,
    /// vnd.ms-officetheme
    VndDotMsOfficetheme,
    /// vnd.ms-playready.initiator+xml
    VndDotMsPlayreadyDotInitiatorPlusXml,
    /// vnd.ms-powerpoint
    VndDotMsPowerpoint,
    /// vnd.ms-powerpoint.addin.macroEnabled.12
    VndDotMsPowerpointDotAddinDotMacroEnabledDot12,
    /// vnd.ms-powerpoint.presentation.macroEnabled.12
    VndDotMsPowerpointDotPresentationDotMacroEnabledDot12,
    /// vnd.ms-powerpoint.slide.macroEnabled.12
    VndDotMsPowerpointDotSlideDotMacroEnabledDot12,
    /// vnd.ms-powerpoint.slideshow.macroEnabled.12
    VndDotMsPowerpointDotSlideshowDotMacroEnabledDot12,
    /// vnd.ms-powerpoint.template.macroEnabled.12
    VndDotMsPowerpointDotTemplateDotMacroEnabledDot12,
    /// vnd.ms-PrintDeviceCapabilities+xml
    VndDotMsPrintDeviceCapabilitiesPlusXml,
    /// vnd.ms-PrintSchemaTicket+xml
    VndDotMsPrintSchemaTicketPlusXml,
    /// vnd.ms-project
    VndDotMsProject,
    /// vnd.ms-tnef
    VndDotMsTnef,
    /// vnd.ms-windows.devicepairing
    VndDotMsWindowsDotDevicepairing,
    /// vnd.ms-windows.nwprinting.oob
    VndDotMsWindowsDotNwprintingDotOob,
    /// vnd.ms-windows.printerpairing
    VndDotMsWindowsDotPrinterpairing,
    /// vnd.ms-windows.wsd.oob
    VndDotMsWindowsDotWsdDotOob,
    /// vnd.ms-wmdrm.lic-chlg-req
    VndDotMsWmdrmDotLicChlgReq,
    /// vnd.ms-wmdrm.lic-resp
    VndDotMsWmdrmDotLicResp,
    /// vnd.ms-wmdrm.meter-chlg-req
    VndDotMsWmdrmDotMeterChlgReq,
    /// vnd.ms-wmdrm.meter-resp
    VndDotMsWmdrmDotMeterResp,
    /// vnd.ms-word.document.macroEnabled.12
    VndDotMsWordDotDocumentDotMacroEnabledDot12,
    /// vnd.ms-word.template.macroEnabled.12
    VndDotMsWordDotTemplateDotMacroEnabledDot12,
    /// vnd.ms-works
    VndDotMsWorks,
    /// vnd.ms-wpl
    VndDotMsWpl,
    /// vnd.ms-xpsdocument
    VndDotMsXpsdocument,
    /// vnd.msa-disk-image
    VndDotMsaDiskImage,
    /// vnd.mseq
    VndDotMseq,
    /// vnd.msgpack
    VndDotMsgpack,
    /// vnd.msign
    VndDotMsign,
    /// vnd.multiad.creator
    VndDotMultiadDotCreator,
    /// vnd.multiad.creator.cif
    VndDotMultiadDotCreatorDotCif,
    /// vnd.music-niff
    VndDotMusicNiff,
    /// vnd.musician
    VndDotMusician,
    /// vnd.muvee.style
    VndDotMuveeDotStyle,
    /// vnd.mynfc
    VndDotMynfc,
    /// vnd.nacamar.ybrid+json
    VndDotNacamarDotYbridPlusJson,
    /// vnd.nato.bindingdataobject+cbor
    VndDotNatoDotBindingdataobjectPlusCbor,
    /// vnd.nato.bindingdataobject+json
    VndDotNatoDotBindingdataobjectPlusJson,
    /// vnd.nato.bindingdataobject+xml
    VndDotNatoDotBindingdataobjectPlusXml,
    /// vnd.nato.openxmlformats-package.iepd+zip
    VndDotNatoDotOpenxmlformatsPackageDotIepdPlusZip,
    /// vnd.ncd.control
    VndDotNcdDotControl,
    /// vnd.ncd.reference
    VndDotNcdDotReference,
    /// vnd.nearst.inv+json
    VndDotNearstDotInvPlusJson,
    /// vnd.nebumind.line
    VndDotNebumindDotLine,
    /// vnd.nervana
    VndDotNervana,
    /// vnd.netfpx
    VndDotNetfpx,
    /// vnd.neurolanguage.nlu
    VndDotNeurolanguageDotNlu,
    /// vnd.nimn
    VndDotNimn,
    /// vnd.nintendo.nitro.rom
    VndDotNintendoDotNitroDotRom,
    /// vnd.nintendo.snes.rom
    VndDotNintendoDotSnesDotRom,
    /// vnd.nitf
    VndDotNitf,
    /// vnd.noblenet-directory
    VndDotNoblenetDirectory,
    /// vnd.noblenet-sealer
    VndDotNoblenetSealer,
    /// vnd.noblenet-web
    VndDotNoblenetWeb,
    /// vnd.nokia.catalogs
    VndDotNokiaDotCatalogs,
    /// vnd.nokia.conml+wbxml
    VndDotNokiaDotConmlPlusWbxml,
    /// vnd.nokia.conml+xml
    VndDotNokiaDotConmlPlusXml,
    /// vnd.nokia.iptv.config+xml
    VndDotNokiaDotIptvDotConfigPlusXml,
    /// vnd.nokia.iSDS-radio-presets
    VndDotNokiaDotISdsRadioPresets,
    /// vnd.nokia.landmark+wbxml
    VndDotNokiaDotLandmarkPlusWbxml,
    /// vnd.nokia.landmark+xml
    VndDotNokiaDotLandmarkPlusXml,
    /// vnd.nokia.landmarkcollection+xml
    VndDotNokiaDotLandmarkcollectionPlusXml,
    /// vnd.nokia.n-gage.ac+xml
    VndDotNokiaDotNGageDotAcPlusXml,
    /// vnd.nokia.n-gage.data
    VndDotNokiaDotNGageDotData,
    /// vnd.nokia.n-gage.symbian.install
    VndDotNokiaDotNGageDotSymbianDotInstall,
    /// vnd.nokia.ncd
    VndDotNokiaDotNcd,
    /// vnd.nokia.pcd+wbxml
    VndDotNokiaDotPcdPlusWbxml,
    /// vnd.nokia.pcd+xml
    VndDotNokiaDotPcdPlusXml,
    /// vnd.nokia.radio-preset
    VndDotNokiaDotRadioPreset,
    /// vnd.nokia.radio-presets
    VndDotNokiaDotRadioPresets,
    /// vnd.novadigm.EDM
    VndDotNovadigmDotEdm,
    /// vnd.novadigm.EDX
    VndDotNovadigmDotEdx,
    /// vnd.novadigm.EXT
    VndDotNovadigmDotExt,
    /// vnd.ntt-local.content-share
    VndDotNttLocalDotContentShare,
    /// vnd.ntt-local.file-transfer
    VndDotNttLocalDotFileTransfer,
    /// vnd.ntt-local.ogw_remote-access
    VndDotNttLocalDotOgwRemoteAccess,
    /// vnd.ntt-local.sip-ta_remote
    VndDotNttLocalDotSipTaRemote,
    /// vnd.ntt-local.sip-ta_tcp_stream
    VndDotNttLocalDotSipTaTcpStream,
    /// vnd.nubaltec.nudoku-game
    VndDotNubaltecDotNudokuGame,
    /// vnd.oai.workflows
    VndDotOaiDotWorkflows,
    /// vnd.oai.workflows+json
    VndDotOaiDotWorkflowsPlusJson,
    /// vnd.oai.workflows+yaml
    VndDotOaiDotWorkflowsPlusYaml,
    /// vnd.oasis.opendocument.base
    VndDotOasisDotOpendocumentDotBase,
    /// vnd.oasis.opendocument.chart
    VndDotOasisDotOpendocumentDotChart,
    /// vnd.oasis.opendocument.chart-template
    VndDotOasisDotOpendocumentDotChartTemplate,
    /// vnd.oasis.opendocument.database
    VndDotOasisDotOpendocumentDotDatabase,
    /// vnd.oasis.opendocument.formula
    VndDotOasisDotOpendocumentDotFormula,
    /// vnd.oasis.opendocument.formula-template
    VndDotOasisDotOpendocumentDotFormulaTemplate,
    /// vnd.oasis.opendocument.graphics
    VndDotOasisDotOpendocumentDotGraphics,
    /// vnd.oasis.opendocument.graphics-template
    VndDotOasisDotOpendocumentDotGraphicsTemplate,
    /// vnd.oasis.opendocument.image
    VndDotOasisDotOpendocumentDotImage,
    /// vnd.oasis.opendocument.image-template
    VndDotOasisDotOpendocumentDotImageTemplate,
    /// vnd.oasis.opendocument.presentation
    VndDotOasisDotOpendocumentDotPresentation,
    /// vnd.oasis.opendocument.presentation-template
    VndDotOasisDotOpendocumentDotPresentationTemplate,
    /// vnd.oasis.opendocument.spreadsheet
    VndDotOasisDotOpendocumentDotSpreadsheet,
    /// vnd.oasis.opendocument.spreadsheet-template
    VndDotOasisDotOpendocumentDotSpreadsheetTemplate,
    /// vnd.oasis.opendocument.text
    VndDotOasisDotOpendocumentDotText,
    /// vnd.oasis.opendocument.text-master
    VndDotOasisDotOpendocumentDotTextMaster,
    /// vnd.oasis.opendocument.text-master-template
    VndDotOasisDotOpendocumentDotTextMasterTemplate,
    /// vnd.oasis.opendocument.text-template
    VndDotOasisDotOpendocumentDotTextTemplate,
    /// vnd.oasis.opendocument.text-web
    VndDotOasisDotOpendocumentDotTextWeb,
    /// vnd.obn
    VndDotObn,
    /// vnd.ocf+cbor
    VndDotOcfPlusCbor,
    /// vnd.oci.image.manifest.v1+json
    VndDotOciDotImageDotManifestDotV1PlusJson,
    /// vnd.oftn.l10n+json
    VndDotOftnDotL10NPlusJson,
    /// vnd.oipf.contentaccessdownload+xml
    VndDotOipfDotContentaccessdownloadPlusXml,
    /// vnd.oipf.contentaccessstreaming+xml
    VndDotOipfDotContentaccessstreamingPlusXml,
    /// vnd.oipf.cspg-hexbinary
    VndDotOipfDotCspgHexbinary,
    /// vnd.oipf.dae.svg+xml
    VndDotOipfDotDaeDotSvgPlusXml,
    /// vnd.oipf.dae.xhtml+xml
    VndDotOipfDotDaeDotXhtmlPlusXml,
    /// vnd.oipf.mippvcontrolmessage+xml
    VndDotOipfDotMippvcontrolmessagePlusXml,
    /// vnd.oipf.pae.gem
    VndDotOipfDotPaeDotGem,
    /// vnd.oipf.spdiscovery+xml
    VndDotOipfDotSpdiscoveryPlusXml,
    /// vnd.oipf.spdlist+xml
    VndDotOipfDotSpdlistPlusXml,
    /// vnd.oipf.ueprofile+xml
    VndDotOipfDotUeprofilePlusXml,
    /// vnd.oipf.userprofile+xml
    VndDotOipfDotUserprofilePlusXml,
    /// vnd.olpc-sugar
    VndDotOlpcSugar,
    /// vnd.oma-scws-config
    VndDotOmaScwsConfig,
    /// vnd.oma-scws-http-request
    VndDotOmaScwsHttpRequest,
    /// vnd.oma-scws-http-response
    VndDotOmaScwsHttpResponse,
    /// vnd.oma.bcast.associated-procedure-parameter+xml
    VndDotOmaDotBcastDotAssociatedProcedureParameterPlusXml,
    /// vnd.oma.bcast.drm-trigger+xml
    VndDotOmaDotBcastDotDrmTriggerPlusXml,
    /// vnd.oma.bcast.imd+xml
    VndDotOmaDotBcastDotImdPlusXml,
    /// vnd.oma.bcast.ltkm
    VndDotOmaDotBcastDotLtkm,
    /// vnd.oma.bcast.notification+xml
    VndDotOmaDotBcastDotNotificationPlusXml,
    /// vnd.oma.bcast.provisioningtrigger
    VndDotOmaDotBcastDotProvisioningtrigger,
    /// vnd.oma.bcast.sgboot
    VndDotOmaDotBcastDotSgboot,
    /// vnd.oma.bcast.sgdd+xml
    VndDotOmaDotBcastDotSgddPlusXml,
    /// vnd.oma.bcast.sgdu
    VndDotOmaDotBcastDotSgdu,
    /// vnd.oma.bcast.simple-symbol-container
    VndDotOmaDotBcastDotSimpleSymbolContainer,
    /// vnd.oma.bcast.smartcard-trigger+xml
    VndDotOmaDotBcastDotSmartcardTriggerPlusXml,
    /// vnd.oma.bcast.sprov+xml
    VndDotOmaDotBcastDotSprovPlusXml,
    /// vnd.oma.bcast.stkm
    VndDotOmaDotBcastDotStkm,
    /// vnd.oma.cab-address-book+xml
    VndDotOmaDotCabAddressBookPlusXml,
    /// vnd.oma.cab-feature-handler+xml
    VndDotOmaDotCabFeatureHandlerPlusXml,
    /// vnd.oma.cab-pcc+xml
    VndDotOmaDotCabPccPlusXml,
    /// vnd.oma.cab-subs-invite+xml
    VndDotOmaDotCabSubsInvitePlusXml,
    /// vnd.oma.cab-user-prefs+xml
    VndDotOmaDotCabUserPrefsPlusXml,
    /// vnd.oma.dcd
    VndDotOmaDotDcd,
    /// vnd.oma.dcdc
    VndDotOmaDotDcdc,
    /// vnd.oma.dd2+xml
    VndDotOmaDotDd2PlusXml,
    /// vnd.oma.drm.risd+xml
    VndDotOmaDotDrmDotRisdPlusXml,
    /// vnd.oma.group-usage-list+xml
    VndDotOmaDotGroupUsageListPlusXml,
    /// vnd.oma.lwm2m+cbor
    VndDotOmaDotLwm2MPlusCbor,
    /// vnd.oma.lwm2m+json
    VndDotOmaDotLwm2MPlusJson,
    /// vnd.oma.lwm2m+tlv
    VndDotOmaDotLwm2MPlusTlv,
    /// vnd.oma.pal+xml
    VndDotOmaDotPalPlusXml,
    /// vnd.oma.poc.detailed-progress-report+xml
    VndDotOmaDotPocDotDetailedProgressReportPlusXml,
    /// vnd.oma.poc.final-report+xml
    VndDotOmaDotPocDotFinalReportPlusXml,
    /// vnd.oma.poc.groups+xml
    VndDotOmaDotPocDotGroupsPlusXml,
    /// vnd.oma.poc.invocation-descriptor+xml
    VndDotOmaDotPocDotInvocationDescriptorPlusXml,
    /// vnd.oma.poc.optimized-progress-report+xml
    VndDotOmaDotPocDotOptimizedProgressReportPlusXml,
    /// vnd.oma.push
    VndDotOmaDotPush,
    /// vnd.oma.scidm.messages+xml
    VndDotOmaDotScidmDotMessagesPlusXml,
    /// vnd.oma.xcap-directory+xml
    VndDotOmaDotXcapDirectoryPlusXml,
    /// vnd.omads-email+xml
    VndDotOmadsEmailPlusXml,
    /// vnd.omads-file+xml
    VndDotOmadsFilePlusXml,
    /// vnd.omads-folder+xml
    VndDotOmadsFolderPlusXml,
    /// vnd.omaloc-supl-init
    VndDotOmalocSuplInit,
    /// vnd.oms.cellular-cose-content+cbor
    VndDotOmsDotCellularCoseContentPlusCbor,
    /// vnd.onepager
    VndDotOnepager,
    /// vnd.onepagertamp
    VndDotOnepagertamp,
    /// vnd.onepagertamx
    VndDotOnepagertamx,
    /// vnd.onepagertat
    VndDotOnepagertat,
    /// vnd.onepagertatp
    VndDotOnepagertatp,
    /// vnd.onepagertatx
    VndDotOnepagertatx,
    /// vnd.onvif.metadata
    VndDotOnvifDotMetadata,
    /// vnd.openblox.game+xml
    VndDotOpenbloxDotGamePlusXml,
    /// vnd.openblox.game-binary
    VndDotOpenbloxDotGameBinary,
    /// vnd.openeye.oeb
    VndDotOpeneyeDotOeb,
    /// vnd.openprinttag
    VndDotOpenprinttag,
    /// vnd.openstreetmap.data+xml
    VndDotOpenstreetmapDotDataPlusXml,
    /// vnd.opentimestamps.ots
    VndDotOpentimestampsDotOts,
    /// vnd.openvpi.dspx+json
    VndDotOpenvpiDotDspxPlusJson,
    /// vnd.openxmlformats-officedocument.custom-properties+xml
    VndDotOpenxmlformatsOfficedocumentDotCustomPropertiesPlusXml,
    /// vnd.openxmlformats-officedocument.customXmlProperties+xml
    VndDotOpenxmlformatsOfficedocumentDotCustomXmlPropertiesPlusXml,
    /// vnd.openxmlformats-officedocument.drawing+xml
    VndDotOpenxmlformatsOfficedocumentDotDrawingPlusXml,
    /// vnd.openxmlformats-officedocument.drawingml.chart+xml
    VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotChartPlusXml,
    /// vnd.openxmlformats-officedocument.drawingml.chartshapes+xml
    VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotChartshapesPlusXml,
    /// vnd.openxmlformats-officedocument.drawingml.diagramColors+xml
    VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotDiagramColorsPlusXml,
    /// vnd.openxmlformats-officedocument.drawingml.diagramData+xml
    VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotDiagramDataPlusXml,
    /// vnd.openxmlformats-officedocument.drawingml.diagramLayout+xml
    VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotDiagramLayoutPlusXml,
    /// vnd.openxmlformats-officedocument.drawingml.diagramStyle+xml
    VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotDiagramStylePlusXml,
    /// vnd.openxmlformats-officedocument.extended-properties+xml
    VndDotOpenxmlformatsOfficedocumentDotExtendedPropertiesPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.commentAuthors+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotCommentAuthorsPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.comments+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotCommentsPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.handoutMaster+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotHandoutMasterPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.notesMaster+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotNotesMasterPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.notesSlide+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotNotesSlidePlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.presentation
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotPresentation,
    /// vnd.openxmlformats-officedocument.presentationml.presentation.main+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotPresentationDotMainPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.presProps+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotPresPropsPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.slide
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlide,
    /// vnd.openxmlformats-officedocument.presentationml.slide+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlidePlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.slideLayout+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideLayoutPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.slideMaster+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideMasterPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.slideshow
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideshow,
    /// vnd.openxmlformats-officedocument.presentationml.slideshow.main+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideshowDotMainPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.slideUpdateInfo+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideUpdateInfoPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.tableStyles+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotTableStylesPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.tags+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotTagsPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.template
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotTemplate,
    /// vnd.openxmlformats-officedocument.presentationml.template.main+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotTemplateDotMainPlusXml,
    /// vnd.openxmlformats-officedocument.presentationml.viewProps+xml
    VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotViewPropsPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.calcChain+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotCalcChainPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotChartsheetPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.comments+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotCommentsPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.connections+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotConnectionsPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.dialogsheet+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotDialogsheetPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.externalLink+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotExternalLinkPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheDefinition+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotPivotCacheDefinitionPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheRecords+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotPivotCacheRecordsPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.pivotTable+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotPivotTablePlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.queryTable+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotQueryTablePlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.revisionHeaders+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotRevisionHeadersPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.revisionLog+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotRevisionLogPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotSharedStringsPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.sheet
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotSheet,
    /// vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotSheetDotMainPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.sheetMetadata+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotSheetMetadataPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.styles+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotStylesPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.table+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotTablePlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.tableSingleCells+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotTableSingleCellsPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.template
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotTemplate,
    /// vnd.openxmlformats-officedocument.spreadsheetml.template.main+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotTemplateDotMainPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.userNames+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotUserNamesPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.volatileDependencies+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotVolatileDependenciesPlusXml,
    /// vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml
    VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotWorksheetPlusXml,
    /// vnd.openxmlformats-officedocument.theme+xml
    VndDotOpenxmlformatsOfficedocumentDotThemePlusXml,
    /// vnd.openxmlformats-officedocument.themeOverride+xml
    VndDotOpenxmlformatsOfficedocumentDotThemeOverridePlusXml,
    /// vnd.openxmlformats-officedocument.vmlDrawing
    VndDotOpenxmlformatsOfficedocumentDotVmlDrawing,
    /// vnd.openxmlformats-officedocument.wordprocessingml.comments+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotCommentsPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.document
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotDocument,
    /// vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotDocumentDotGlossaryPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotDocumentDotMainPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotEndnotesPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotFontTablePlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.footer+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotFooterPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotFootnotesPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotNumberingPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.settings+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotSettingsPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.styles+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotStylesPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.template
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotTemplate,
    /// vnd.openxmlformats-officedocument.wordprocessingml.template.main+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotTemplateDotMainPlusXml,
    /// vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml
    VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotWebSettingsPlusXml,
    /// vnd.openxmlformats-package.core-properties+xml
    VndDotOpenxmlformatsPackageDotCorePropertiesPlusXml,
    /// vnd.openxmlformats-package.digital-signature-xmlsignature+xml
    VndDotOpenxmlformatsPackageDotDigitalSignatureXmlsignaturePlusXml,
    /// vnd.openxmlformats-package.relationships+xml
    VndDotOpenxmlformatsPackageDotRelationshipsPlusXml,
    /// vnd.oracle.resource+json
    VndDotOracleDotResourcePlusJson,
    /// vnd.orange.indata
    VndDotOrangeDotIndata,
    /// vnd.osa.netdeploy
    VndDotOsaDotNetdeploy,
    /// vnd.osgeo.mapguide.package
    VndDotOsgeoDotMapguideDotPackage,
    /// vnd.osgi.bundle
    VndDotOsgiDotBundle,
    /// vnd.osgi.dp
    VndDotOsgiDotDp,
    /// vnd.osgi.subsystem
    VndDotOsgiDotSubsystem,
    /// vnd.otps.ct-kip+xml
    VndDotOtpsDotCtKipPlusXml,
    /// vnd.oxli.countgraph
    VndDotOxliDotCountgraph,
    /// vnd.pagerduty+json
    VndDotPagerdutyPlusJson,
    /// vnd.palm
    VndDotPalm,
    /// vnd.panoply
    VndDotPanoply,
    /// vnd.paos.xml
    VndDotPaosDotXml,
    /// vnd.patentdive
    VndDotPatentdive,
    /// vnd.patientecommsdoc
    VndDotPatientecommsdoc,
    /// vnd.pawaafile
    VndDotPawaafile,
    /// vnd.pcos
    VndDotPcos,
    /// vnd.pg.format
    VndDotPgDotFormat,
    /// vnd.pg.osasli
    VndDotPgDotOsasli,
    /// vnd.phbk+xml
    VndDotPhbkPlusXml,
    /// vnd.piaccess.application-licence
    VndDotPiaccessDotApplicationLicence,
    /// vnd.picsel
    VndDotPicsel,
    /// vnd.pmi.widget
    VndDotPmiDotWidget,
    /// vnd.pmtiles
    VndDotPmtiles,
    /// vnd.poc.group-advertisement+xml
    VndDotPocDotGroupAdvertisementPlusXml,
    /// vnd.pocketlearn
    VndDotPocketlearn,
    /// vnd.powerbuilder6
    VndDotPowerbuilder6,
    /// vnd.powerbuilder6-s
    VndDotPowerbuilder6S,
    /// vnd.powerbuilder7
    VndDotPowerbuilder7,
    /// vnd.powerbuilder7-s
    VndDotPowerbuilder7S,
    /// vnd.powerbuilder75
    VndDotPowerbuilder75,
    /// vnd.powerbuilder75-s
    VndDotPowerbuilder75S,
    /// vnd.pp.systemverify+xml
    VndDotPpDotSystemverifyPlusXml,
    /// vnd.preminet
    VndDotPreminet,
    /// vnd.previewsystems.box
    VndDotPreviewsystemsDotBox,
    /// vnd.project-graph
    VndDotProjectGraph,
    /// vnd.proteus.magazine
    VndDotProteusDotMagazine,
    /// vnd.psfs
    VndDotPsfs,
    /// vnd.pt.mundusmundi
    VndDotPtDotMundusmundi,
    /// vnd.publishare-delta-tree
    VndDotPublishareDeltaTree,
    /// vnd.pvi.ptid1
    VndDotPviDotPtid1,
    /// vnd.pwg-multiplexed
    VndDotPwgMultiplexed,
    /// vnd.pwg-xhtml-print+xml
    VndDotPwgXhtmlPrintPlusXml,
    /// vnd.pyon+json
    VndDotPyonPlusJson,
    /// vnd.qualcomm.brew-app-res
    VndDotQualcommDotBrewAppRes,
    /// vnd.quarantainenet
    VndDotQuarantainenet,
    /// vnd.Quark.QuarkXPress
    VndDotQuarkDotQuarkXPress,
    /// vnd.quobject-quoxdocument
    VndDotQuobjectQuoxdocument,
    /// vnd.R74n.sandboxels+json
    VndDotR74NDotSandboxelsPlusJson,
    /// vnd.radisys.moml+xml
    VndDotRadisysDotMomlPlusXml,
    /// vnd.radisys.msml+xml
    VndDotRadisysDotMsmlPlusXml,
    /// vnd.radisys.msml-audit+xml
    VndDotRadisysDotMsmlAuditPlusXml,
    /// vnd.radisys.msml-audit-conf+xml
    VndDotRadisysDotMsmlAuditConfPlusXml,
    /// vnd.radisys.msml-audit-conn+xml
    VndDotRadisysDotMsmlAuditConnPlusXml,
    /// vnd.radisys.msml-audit-dialog+xml
    VndDotRadisysDotMsmlAuditDialogPlusXml,
    /// vnd.radisys.msml-audit-stream+xml
    VndDotRadisysDotMsmlAuditStreamPlusXml,
    /// vnd.radisys.msml-conf+xml
    VndDotRadisysDotMsmlConfPlusXml,
    /// vnd.radisys.msml-dialog+xml
    VndDotRadisysDotMsmlDialogPlusXml,
    /// vnd.radisys.msml-dialog-base+xml
    VndDotRadisysDotMsmlDialogBasePlusXml,
    /// vnd.radisys.msml-dialog-fax-detect+xml
    VndDotRadisysDotMsmlDialogFaxDetectPlusXml,
    /// vnd.radisys.msml-dialog-fax-sendrecv+xml
    VndDotRadisysDotMsmlDialogFaxSendrecvPlusXml,
    /// vnd.radisys.msml-dialog-group+xml
    VndDotRadisysDotMsmlDialogGroupPlusXml,
    /// vnd.radisys.msml-dialog-speech+xml
    VndDotRadisysDotMsmlDialogSpeechPlusXml,
    /// vnd.radisys.msml-dialog-transform+xml
    VndDotRadisysDotMsmlDialogTransformPlusXml,
    /// vnd.rainstor.data
    VndDotRainstorDotData,
    /// vnd.rapid
    VndDotRapid,
    /// vnd.rar
    VndDotRar,
    /// vnd.realvnc.bed
    VndDotRealvncDotBed,
    /// vnd.recordare.musicxml
    VndDotRecordareDotMusicxml,
    /// vnd.recordare.musicxml+xml
    VndDotRecordareDotMusicxmlPlusXml,
    /// vnd.rego
    VndDotRego,
    /// vnd.relpipe
    VndDotRelpipe,
    /// vnd.RenLearn.rlprint
    VndDotRenLearnDotRlprint,
    /// vnd.resilient.logic
    VndDotResilientDotLogic,
    /// vnd.restful+json
    VndDotRestfulPlusJson,
    /// vnd.rig.cryptonote
    VndDotRigDotCryptonote,
    /// vnd.route66.link66+xml
    VndDotRoute66DotLink66PlusXml,
    /// vnd.rs-274x
    VndDotRs274X,
    /// vnd.ruckus.download
    VndDotRuckusDotDownload,
    /// vnd.s3sms
    VndDotS3Sms,
    /// vnd.sailingtracker.track
    VndDotSailingtrackerDotTrack,
    /// vnd.sar
    VndDotSar,
    /// vnd.sbm.cid
    VndDotSbmDotCid,
    /// vnd.sbm.mid2
    VndDotSbmDotMid2,
    /// vnd.scribus
    VndDotScribus,
    /// vnd.sealed.3df
    VndDotSealedDot3Df,
    /// vnd.sealed.csf
    VndDotSealedDotCsf,
    /// vnd.sealed.doc
    VndDotSealedDotDoc,
    /// vnd.sealed.eml
    VndDotSealedDotEml,
    /// vnd.sealed.mht
    VndDotSealedDotMht,
    /// vnd.sealed.net
    VndDotSealedDotNet,
    /// vnd.sealed.ppt
    VndDotSealedDotPpt,
    /// vnd.sealed.tiff
    VndDotSealedDotTiff,
    /// vnd.sealed.xls
    VndDotSealedDotXls,
    /// vnd.sealedmedia.softseal.html
    VndDotSealedmediaDotSoftsealDotHtml,
    /// vnd.sealedmedia.softseal.pdf
    VndDotSealedmediaDotSoftsealDotPdf,
    /// vnd.seemail
    VndDotSeemail,
    /// vnd.seis+json
    VndDotSeisPlusJson,
    /// vnd.sema
    VndDotSema,
    /// vnd.semd
    VndDotSemd,
    /// vnd.semf
    VndDotSemf,
    /// vnd.shade-save-file
    VndDotShadeSaveFile,
    /// vnd.shana.informed.formdata
    VndDotShanaDotInformedDotFormdata,
    /// vnd.shana.informed.formtemplate
    VndDotShanaDotInformedDotFormtemplate,
    /// vnd.shana.informed.interchange
    VndDotShanaDotInformedDotInterchange,
    /// vnd.shana.informed.package
    VndDotShanaDotInformedDotPackage,
    /// vnd.shootproof+json
    VndDotShootproofPlusJson,
    /// vnd.shopkick+json
    VndDotShopkickPlusJson,
    /// vnd.shp
    VndDotShp,
    /// vnd.shx
    VndDotShx,
    /// vnd.sigrok.session
    VndDotSigrokDotSession,
    /// vnd.SimTech-MindMapper
    VndDotSimTechMindMapper,
    /// vnd.siren+json
    VndDotSirenPlusJson,
    /// vnd.sirtx.vmv0
    VndDotSirtxDotVmv0,
    /// vnd.sketchometry
    VndDotSketchometry,
    /// vnd.smaf
    VndDotSmaf,
    /// vnd.smart.notebook
    VndDotSmartDotNotebook,
    /// vnd.smart.teacher
    VndDotSmartDotTeacher,
    /// vnd.smintio.portals.archive
    VndDotSmintioDotPortalsDotArchive,
    /// vnd.snesdev-page-table
    VndDotSnesdevPageTable,
    /// vnd.software602.filler.form+xml
    VndDotSoftware602DotFillerDotFormPlusXml,
    /// vnd.software602.filler.form-xml-zip
    VndDotSoftware602DotFillerDotFormXmlZip,
    /// vnd.solent.sdkm+xml
    VndDotSolentDotSdkmPlusXml,
    /// vnd.spotfire.dxp
    VndDotSpotfireDotDxp,
    /// vnd.spotfire.sfs
    VndDotSpotfireDotSfs,
    /// vnd.sqlite3
    VndDotSqlite3,
    /// vnd.sri
    VndDotSri,
    /// vnd.sss-cod
    VndDotSssCod,
    /// vnd.sss-dtf
    VndDotSssDtf,
    /// vnd.sss-ntf
    VndDotSssNtf,
    /// vnd.stepmania.package
    VndDotStepmaniaDotPackage,
    /// vnd.stepmania.stepchart
    VndDotStepmaniaDotStepchart,
    /// vnd.street-stream
    VndDotStreetStream,
    /// vnd.sun.wadl+xml
    VndDotSunDotWadlPlusXml,
    /// vnd.superfile.super
    VndDotSuperfileDotSuper,
    /// vnd.sus-calendar
    VndDotSusCalendar,
    /// vnd.svd
    VndDotSvd,
    /// vnd.swiftview-ics
    VndDotSwiftviewIcs,
    /// vnd.sybyl.mol2
    VndDotSybylDotMol2,
    /// vnd.sycle+xml
    VndDotSyclePlusXml,
    /// vnd.syft+json
    VndDotSyftPlusJson,
    /// vnd.syncml+xml
    VndDotSyncmlPlusXml,
    /// vnd.syncml.dm+wbxml
    VndDotSyncmlDotDmPlusWbxml,
    /// vnd.syncml.dm+xml
    VndDotSyncmlDotDmPlusXml,
    /// vnd.syncml.dm.notification
    VndDotSyncmlDotDmDotNotification,
    /// vnd.syncml.dmddf+wbxml
    VndDotSyncmlDotDmddfPlusWbxml,
    /// vnd.syncml.dmddf+xml
    VndDotSyncmlDotDmddfPlusXml,
    /// vnd.syncml.dmtnds+wbxml
    VndDotSyncmlDotDmtndsPlusWbxml,
    /// vnd.syncml.dmtnds+xml
    VndDotSyncmlDotDmtndsPlusXml,
    /// vnd.syncml.ds.notification
    VndDotSyncmlDotDsDotNotification,
    /// vnd.tableschema+json
    VndDotTableschemaPlusJson,
    /// vnd.tao.intent-module-archive
    VndDotTaoDotIntentModuleArchive,
    /// vnd.tcpdump.pcap
    VndDotTcpdumpDotPcap,
    /// vnd.think-cell.ppttc+json
    VndDotThinkCellDotPpttcPlusJson,
    /// vnd.tmd.mediaflex.api+xml
    VndDotTmdDotMediaflexDotApiPlusXml,
    /// vnd.tml
    VndDotTml,
    /// vnd.tmobile-livetv
    VndDotTmobileLivetv,
    /// vnd.tri.onesource
    VndDotTriDotOnesource,
    /// vnd.trid.tpt
    VndDotTridDotTpt,
    /// vnd.triscape.mxs
    VndDotTriscapeDotMxs,
    /// vnd.trueapp
    VndDotTrueapp,
    /// vnd.truedoc
    VndDotTruedoc,
    /// vnd.ubisoft.webplayer
    VndDotUbisoftDotWebplayer,
    /// vnd.ufdl
    VndDotUfdl,
    /// vnd.uic.dosipas.v1
    VndDotUicDotDosipasDotV1,
    /// vnd.uic.dosipas.v2
    VndDotUicDotDosipasDotV2,
    /// vnd.uic.osdm+json
    VndDotUicDotOsdmPlusJson,
    /// vnd.uic.tlb-fcb
    VndDotUicDotTlbFcb,
    /// vnd.uiq.theme
    VndDotUiqDotTheme,
    /// vnd.umajin
    VndDotUmajin,
    /// vnd.unity
    VndDotUnity,
    /// vnd.uoml+xml
    VndDotUomlPlusXml,
    /// vnd.uplanet.alert
    VndDotUplanetDotAlert,
    /// vnd.uplanet.alert-wbxml
    VndDotUplanetDotAlertWbxml,
    /// vnd.uplanet.bearer-choice
    VndDotUplanetDotBearerChoice,
    /// vnd.uplanet.bearer-choice-wbxml
    VndDotUplanetDotBearerChoiceWbxml,
    /// vnd.uplanet.cacheop
    VndDotUplanetDotCacheop,
    /// vnd.uplanet.cacheop-wbxml
    VndDotUplanetDotCacheopWbxml,
    /// vnd.uplanet.channel
    VndDotUplanetDotChannel,
    /// vnd.uplanet.channel-wbxml
    VndDotUplanetDotChannelWbxml,
    /// vnd.uplanet.list
    VndDotUplanetDotList,
    /// vnd.uplanet.list-wbxml
    VndDotUplanetDotListWbxml,
    /// vnd.uplanet.listcmd
    VndDotUplanetDotListcmd,
    /// vnd.uplanet.listcmd-wbxml
    VndDotUplanetDotListcmdWbxml,
    /// vnd.uplanet.signal
    VndDotUplanetDotSignal,
    /// vnd.uri-map
    VndDotUriMap,
    /// vnd.valve.source.material
    VndDotValveDotSourceDotMaterial,
    /// vnd.vcx
    VndDotVcx,
    /// vnd.vd-study
    VndDotVdStudy,
    /// vnd.vectorworks
    VndDotVectorworks,
    /// vnd.vel+json
    VndDotVelPlusJson,
    /// vnd.veraison.tsm-report+cbor
    VndDotVeraisonDotTsmReportPlusCbor,
    /// vnd.veraison.tsm-report+json
    VndDotVeraisonDotTsmReportPlusJson,
    /// vnd.verifier-attestation+jwt
    VndDotVerifierAttestationPlusJwt,
    /// vnd.verimatrix.vcas
    VndDotVerimatrixDotVcas,
    /// vnd.veritone.aion+json
    VndDotVeritoneDotAionPlusJson,
    /// vnd.vertifile.pvf
    VndDotVertifileDotPvf,
    /// vnd.veryant.thin
    VndDotVeryantDotThin,
    /// vnd.ves.encrypted
    VndDotVesDotEncrypted,
    /// vnd.vidsoft.vidconference
    VndDotVidsoftDotVidconference,
    /// vnd.visio
    VndDotVisio,
    /// vnd.visionary
    VndDotVisionary,
    /// vnd.vividence.scriptfile
    VndDotVividenceDotScriptfile,
    /// vnd.vocalshaper.vsp4
    VndDotVocalshaperDotVsp4,
    /// vnd.vsf
    VndDotVsf,
    /// vnd.vuq
    VndDotVuq,
    /// vnd.wantverse
    VndDotWantverse,
    /// vnd.wap.sic
    VndDotWapDotSic,
    /// vnd.wap.slc
    VndDotWapDotSlc,
    /// vnd.wap.wbxml
    VndDotWapDotWbxml,
    /// vnd.wap.wmlc
    VndDotWapDotWmlc,
    /// vnd.wap.wmlscriptc
    VndDotWapDotWmlscriptc,
    /// vnd.wasmflow.wafl
    VndDotWasmflowDotWafl,
    /// vnd.webturbo
    VndDotWebturbo,
    /// vnd.wfa.dpp
    VndDotWfaDotDpp,
    /// vnd.wfa.p2p
    VndDotWfaDotP2P,
    /// vnd.wfa.wsc
    VndDotWfaDotWsc,
    /// vnd.windows.devicepairing
    VndDotWindowsDotDevicepairing,
    /// vnd.wmap
    VndDotWmap,
    /// vnd.wmc
    VndDotWmc,
    /// vnd.wmf.bootstrap
    VndDotWmfDotBootstrap,
    /// vnd.wolfram.mathematica
    VndDotWolframDotMathematica,
    /// vnd.wolfram.mathematica.package
    VndDotWolframDotMathematicaDotPackage,
    /// vnd.wolfram.player
    VndDotWolframDotPlayer,
    /// vnd.wordlift
    VndDotWordlift,
    /// vnd.wordperfect
    VndDotWordperfect,
    /// vnd.wqd
    VndDotWqd,
    /// vnd.wrq-hp3000-labelled
    VndDotWrqHp3000Labelled,
    /// vnd.wt.stf
    VndDotWtDotStf,
    /// vnd.wv.csp+wbxml
    VndDotWvDotCspPlusWbxml,
    /// vnd.wv.csp+xml
    VndDotWvDotCspPlusXml,
    /// vnd.wv.ssp+xml
    VndDotWvDotSspPlusXml,
    /// vnd.xacml+json
    VndDotXacmlPlusJson,
    /// vnd.xara
    VndDotXara,
    /// vnd.xarin.cpj
    VndDotXarinDotCpj,
    /// vnd.xcdn
    VndDotXcdn,
    /// vnd.xecrets-encrypted
    VndDotXecretsEncrypted,
    /// vnd.xfdl
    VndDotXfdl,
    /// vnd.xfdl.webform
    VndDotXfdlDotWebform,
    /// vnd.xmi+xml
    VndDotXmiPlusXml,
    /// vnd.xmpie.cpkg
    VndDotXmpieDotCpkg,
    /// vnd.xmpie.dpkg
    VndDotXmpieDotDpkg,
    /// vnd.xmpie.plan
    VndDotXmpieDotPlan,
    /// vnd.xmpie.ppkg
    VndDotXmpieDotPpkg,
    /// vnd.xmpie.xlim
    VndDotXmpieDotXlim,
    /// vnd.yamaha.hv-dic
    VndDotYamahaDotHvDic,
    /// vnd.yamaha.hv-script
    VndDotYamahaDotHvScript,
    /// vnd.yamaha.hv-voice
    VndDotYamahaDotHvVoice,
    /// vnd.yamaha.openscoreformat
    VndDotYamahaDotOpenscoreformat,
    /// vnd.yamaha.openscoreformat.osfpvg+xml
    VndDotYamahaDotOpenscoreformatDotOsfpvgPlusXml,
    /// vnd.yamaha.remote-setup
    VndDotYamahaDotRemoteSetup,
    /// vnd.yamaha.smaf-audio
    VndDotYamahaDotSmafAudio,
    /// vnd.yamaha.smaf-phrase
    VndDotYamahaDotSmafPhrase,
    /// vnd.yamaha.through-ngn
    VndDotYamahaDotThroughNgn,
    /// vnd.yamaha.tunnel-udpencap
    VndDotYamahaDotTunnelUdpencap,
    /// vnd.yaoweme
    VndDotYaoweme,
    /// vnd.yellowriver-custom-menu
    VndDotYellowriverCustomMenu,
    /// vnd.youtube.yt
    VndDotYoutubeDotYt,
    /// vnd.zoho-document.writer
    VndDotZohoDocumentDotWriter,
    /// vnd.zoho-presentation.show
    VndDotZohoPresentationDotShow,
    /// vnd.zoho.spreadsheetml.sheet
    VndDotZohoDotSpreadsheetmlDotSheet,
    /// vnd.zul
    VndDotZul,
    /// vnd.zzazz.deck+xml
    VndDotZzazzDotDeckPlusXml,
    /// voicexml+xml
    VoicexmlPlusXml,
    /// voucher-cms+json
    VoucherCmsPlusJson,
    /// voucher-jws+json
    VoucherJwsPlusJson,
    /// vp
    Vp,
    /// vp+cose
    VpPlusCose,
    /// vp+jwt
    VpPlusJwt,
    /// vp+sd-jwt
    VpPlusSdJwt,
    /// vq-rtcpxr
    VqRtcpxr,
    /// wasm
    Wasm,
    /// watcherinfo+xml
    WatcherinfoPlusXml,
    /// webpush-options+json
    WebpushOptionsPlusJson,
    /// whoispp-query
    WhoisppQuery,
    /// whoispp-response
    WhoisppResponse,
    /// widget
    Widget,
    /// wita
    Wita,
    /// wordperfect5.1
    Wordperfect5Dot1,
    /// wsdl+xml
    WsdlPlusXml,
    /// wspolicy+xml
    WspolicyPlusXml,
    /// x-pki-message
    XPkiMessage,
    /// x-www-form-urlencoded
    XWwwFormUrlencoded,
    /// x-x509-ca-cert
    XX509CaCert,
    /// x-x509-ca-ra-cert
    XX509CaRaCert,
    /// x-x509-next-ca-cert
    XX509NextCaCert,
    /// x400-bp
    X400Bp,
    /// xacml+xml
    XacmlPlusXml,
    /// xcap-att+xml
    XcapAttPlusXml,
    /// xcap-caps+xml
    XcapCapsPlusXml,
    /// xcap-diff+xml
    XcapDiffPlusXml,
    /// xcap-el+xml
    XcapElPlusXml,
    /// xcap-error+xml
    XcapErrorPlusXml,
    /// xcap-ns+xml
    XcapNsPlusXml,
    /// xcon-conference-info+xml
    XconConferenceInfoPlusXml,
    /// xcon-conference-info-diff+xml
    XconConferenceInfoDiffPlusXml,
    /// xenc+xml
    XencPlusXml,
    /// xfdf
    Xfdf,
    /// xhtml+xml
    XhtmlPlusXml,
    /// xliff+xml
    XliffPlusXml,
    /// xml
    Xml,
    /// xml-dtd
    XmlDtd,
    /// xml-external-parsed-entity
    XmlExternalParsedEntity,
    /// xml-patch+xml
    XmlPatchPlusXml,
    /// xmpp+xml
    XmppPlusXml,
    /// xop+xml
    XopPlusXml,
    /// xslt+xml
    XsltPlusXml,
    /// xv+xml
    XvPlusXml,
    /// yaml
    Yaml,
    /// yang
    Yang,
    /// yang-data+cbor
    YangDataPlusCbor,
    /// yang-data+json
    YangDataPlusJson,
    /// yang-data+xml
    YangDataPlusXml,
    /// yang-patch+json
    YangPatchPlusJson,
    /// yang-patch+xml
    YangPatchPlusXml,
    /// yang-sid+json
    YangSidPlusJson,
    /// yin+xml
    YinPlusXml,
    /// zip
    Zip,
    /// zlib
    Zlib,
    /// zstd
    Zstd,
    /// private
    Private(PrivateSubtype),
}
const APPLICATION_SUBTYPE_VLIST: &[ApplicationSubtype] = &[
    ApplicationSubtype::_1DInterleavedParityfec,
    ApplicationSubtype::_3GpdashQoeReportPlusXml,
    ApplicationSubtype::_3GppImsPlusXml,
    ApplicationSubtype::_3GppMbsObjectManifestPlusJson,
    ApplicationSubtype::_3GppMbsUserServiceDescriptionsPlusJson,
    ApplicationSubtype::_3GppMediaDeliveryMetricsReportPlusJson,
    ApplicationSubtype::_3GppHalPlusJson,
    ApplicationSubtype::_3GppHalFormsPlusJson,
    ApplicationSubtype::A2L,
    ApplicationSubtype::AasPlusZip,
    ApplicationSubtype::AcePlusCbor,
    ApplicationSubtype::AcePlusJson,
    ApplicationSubtype::AceGroupcommPlusCbor,
    ApplicationSubtype::AceTrlPlusCbor,
    ApplicationSubtype::Activemessage,
    ApplicationSubtype::ActivityPlusJson,
    ApplicationSubtype::AifPlusCbor,
    ApplicationSubtype::AifPlusJson,
    ApplicationSubtype::AltoCdniPlusJson,
    ApplicationSubtype::AltoCdnifilterPlusJson,
    ApplicationSubtype::AltoCostmapPlusJson,
    ApplicationSubtype::AltoCostmapfilterPlusJson,
    ApplicationSubtype::AltoDirectoryPlusJson,
    ApplicationSubtype::AltoEndpointcostPlusJson,
    ApplicationSubtype::AltoEndpointcostparamsPlusJson,
    ApplicationSubtype::AltoEndpointpropPlusJson,
    ApplicationSubtype::AltoEndpointpropparamsPlusJson,
    ApplicationSubtype::AltoErrorPlusJson,
    ApplicationSubtype::AltoNetworkmapPlusJson,
    ApplicationSubtype::AltoNetworkmapfilterPlusJson,
    ApplicationSubtype::AltoPropmapPlusJson,
    ApplicationSubtype::AltoPropmapparamsPlusJson,
    ApplicationSubtype::AltoTipsPlusJson,
    ApplicationSubtype::AltoTipsparamsPlusJson,
    ApplicationSubtype::AltoUpdatestreamcontrolPlusJson,
    ApplicationSubtype::AltoUpdatestreamparamsPlusJson,
    ApplicationSubtype::Aml,
    ApplicationSubtype::AndrewInset,
    ApplicationSubtype::Applefile,
    ApplicationSubtype::AsyncapiPlusJson,
    ApplicationSubtype::AsyncapiPlusYaml,
    ApplicationSubtype::AtPlusJwt,
    ApplicationSubtype::Atf,
    ApplicationSubtype::Atfx,
    ApplicationSubtype::AtomPlusXml,
    ApplicationSubtype::AtomcatPlusXml,
    ApplicationSubtype::AtomdeletedPlusXml,
    ApplicationSubtype::Atomicmail,
    ApplicationSubtype::AtomsvcPlusXml,
    ApplicationSubtype::AtscDwdPlusXml,
    ApplicationSubtype::AtscDynamicEventMessage,
    ApplicationSubtype::AtscHeldPlusXml,
    ApplicationSubtype::AtscRdtPlusJson,
    ApplicationSubtype::AtscRsatPlusXml,
    ApplicationSubtype::Atxml,
    ApplicationSubtype::AuthPolicyPlusXml,
    ApplicationSubtype::AutomationmlAmlPlusXml,
    ApplicationSubtype::AutomationmlAmlxPlusZip,
    ApplicationSubtype::BacnetXddPlusZip,
    ApplicationSubtype::BatchSmtp,
    ApplicationSubtype::BeepPlusXml,
    ApplicationSubtype::Bufr,
    ApplicationSubtype::C2Pa,
    ApplicationSubtype::CalendarPlusJson,
    ApplicationSubtype::CalendarPlusXml,
    ApplicationSubtype::CallCompletion,
    ApplicationSubtype::Cals1840,
    ApplicationSubtype::CaptivePlusJson,
    ApplicationSubtype::Cbor,
    ApplicationSubtype::CborSeq,
    ApplicationSubtype::Cccex,
    ApplicationSubtype::CcmpPlusXml,
    ApplicationSubtype::CcxmlPlusXml,
    ApplicationSubtype::CdaPlusXml,
    ApplicationSubtype::CdfxPlusXml,
    ApplicationSubtype::CdmiCapability,
    ApplicationSubtype::CdmiContainer,
    ApplicationSubtype::CdmiDomain,
    ApplicationSubtype::CdmiObject,
    ApplicationSubtype::CdmiQueue,
    ApplicationSubtype::Cdni,
    ApplicationSubtype::CePlusCbor,
    ApplicationSubtype::Cea,
    ApplicationSubtype::Cea2018PlusXml,
    ApplicationSubtype::CellmlPlusXml,
    ApplicationSubtype::Cfw,
    ApplicationSubtype::Cid,
    ApplicationSubtype::CidEdhocPlusCborSeq,
    ApplicationSubtype::CityPlusJson,
    ApplicationSubtype::CityPlusJsonSeq,
    ApplicationSubtype::Clr,
    ApplicationSubtype::CluePlusXml,
    ApplicationSubtype::ClueInfoPlusXml,
    ApplicationSubtype::Cms,
    ApplicationSubtype::CmwPlusCbor,
    ApplicationSubtype::CmwPlusCose,
    ApplicationSubtype::CmwPlusJson,
    ApplicationSubtype::CmwPlusJws,
    ApplicationSubtype::CnrpPlusXml,
    ApplicationSubtype::CoapEap,
    ApplicationSubtype::CoapGroupPlusJson,
    ApplicationSubtype::CoapPayload,
    ApplicationSubtype::Commonground,
    ApplicationSubtype::ConciseProblemDetailsPlusCbor,
    ApplicationSubtype::ConferenceInfoPlusXml,
    ApplicationSubtype::Cose,
    ApplicationSubtype::CoseKey,
    ApplicationSubtype::CoseKeySet,
    ApplicationSubtype::CoseX509,
    ApplicationSubtype::CplPlusXml,
    ApplicationSubtype::Csrattrs,
    ApplicationSubtype::CstaPlusXml,
    ApplicationSubtype::CstAdataPlusXml,
    ApplicationSubtype::CsvmPlusJson,
    ApplicationSubtype::Cwl,
    ApplicationSubtype::CwlPlusJson,
    ApplicationSubtype::CwlPlusYaml,
    ApplicationSubtype::Cwt,
    ApplicationSubtype::Cybercash,
    ApplicationSubtype::DashPlusXml,
    ApplicationSubtype::DashPatchPlusXml,
    ApplicationSubtype::Dashdelta,
    ApplicationSubtype::DavmountPlusXml,
    ApplicationSubtype::DcaRft,
    ApplicationSubtype::Dcd,
    ApplicationSubtype::DecDx,
    ApplicationSubtype::DialogInfoPlusXml,
    ApplicationSubtype::Dicom,
    ApplicationSubtype::DicomPlusJson,
    ApplicationSubtype::DicomPlusXml,
    ApplicationSubtype::Did,
    ApplicationSubtype::Dii,
    ApplicationSubtype::Dit,
    ApplicationSubtype::Dns,
    ApplicationSubtype::DnsPlusJson,
    ApplicationSubtype::DnsMessage,
    ApplicationSubtype::DotsPlusCbor,
    ApplicationSubtype::DpopPlusJwt,
    ApplicationSubtype::DskppPlusXml,
    ApplicationSubtype::DsscPlusDer,
    ApplicationSubtype::DsscPlusXml,
    ApplicationSubtype::Dvcs,
    ApplicationSubtype::EatPlusCwt,
    ApplicationSubtype::EatPlusJwt,
    ApplicationSubtype::EatBunPlusCbor,
    ApplicationSubtype::EatBunPlusJson,
    ApplicationSubtype::EatUcsPlusCbor,
    ApplicationSubtype::EatUcsPlusJson,
    ApplicationSubtype::Ecmascript,
    ApplicationSubtype::EdhocPlusCborSeq,
    ApplicationSubtype::EdiConsent,
    ApplicationSubtype::EdiX12,
    ApplicationSubtype::Edifact,
    ApplicationSubtype::Efi,
    ApplicationSubtype::ElmPlusJson,
    ApplicationSubtype::ElmPlusXml,
    ApplicationSubtype::EmergencyCallDataDotCapPlusXml,
    ApplicationSubtype::EmergencyCallDataDotCommentPlusXml,
    ApplicationSubtype::EmergencyCallDataDotControlPlusXml,
    ApplicationSubtype::EmergencyCallDataDotDeviceInfoPlusXml,
    ApplicationSubtype::EmergencyCallDataDotECallDotMsd,
    ApplicationSubtype::EmergencyCallDataDotLegacyEsnPlusJson,
    ApplicationSubtype::EmergencyCallDataDotProviderInfoPlusXml,
    ApplicationSubtype::EmergencyCallDataDotServiceInfoPlusXml,
    ApplicationSubtype::EmergencyCallDataDotSubscriberInfoPlusXml,
    ApplicationSubtype::EmergencyCallDataDotVedsPlusXml,
    ApplicationSubtype::EmmaPlusXml,
    ApplicationSubtype::EmotionmlPlusXml,
    ApplicationSubtype::Encaprtp,
    ApplicationSubtype::EntityStatementPlusJwt,
    ApplicationSubtype::EppPlusXml,
    ApplicationSubtype::EpubPlusZip,
    ApplicationSubtype::Eshop,
    ApplicationSubtype::Example,
    ApplicationSubtype::Exi,
    ApplicationSubtype::ExpectCtReportPlusJson,
    ApplicationSubtype::ExplicitRegistrationResponsePlusJwt,
    ApplicationSubtype::Express,
    ApplicationSubtype::Fastinfoset,
    ApplicationSubtype::Fastsoap,
    ApplicationSubtype::Fdf,
    ApplicationSubtype::FdtPlusXml,
    ApplicationSubtype::FhirPlusJson,
    ApplicationSubtype::FhirPlusXml,
    ApplicationSubtype::Fits,
    ApplicationSubtype::Flexfec,
    ApplicationSubtype::FontSfntDeprecatedInFavorOfFontSfnt,
    ApplicationSubtype::FontTdpfr,
    ApplicationSubtype::FontWoffDeprecatedInFavorOfFontWoff,
    ApplicationSubtype::FrameworkAttributesPlusXml,
    ApplicationSubtype::GeoPlusJson,
    ApplicationSubtype::GeoPlusJsonSeq,
    ApplicationSubtype::GeofeedPlusCsv,
    ApplicationSubtype::GeopackagePlusSqlite3,
    ApplicationSubtype::GeoposePlusJson,
    ApplicationSubtype::GeoxacmlPlusJson,
    ApplicationSubtype::GeoxacmlPlusXml,
    ApplicationSubtype::GltfBuffer,
    ApplicationSubtype::GmlPlusXml,
    ApplicationSubtype::GnapBindingJws,
    ApplicationSubtype::GnapBindingJwsd,
    ApplicationSubtype::GnapBindingRotationJws,
    ApplicationSubtype::GnapBindingRotationJwsd,
    ApplicationSubtype::Grib,
    ApplicationSubtype::Gzip,
    ApplicationSubtype::H224,
    ApplicationSubtype::HeldPlusXml,
    ApplicationSubtype::Hl7V2PlusXml,
    ApplicationSubtype::Http,
    ApplicationSubtype::Hyperstudio,
    ApplicationSubtype::IbeKeyRequestPlusXml,
    ApplicationSubtype::IbePkgReplyPlusXml,
    ApplicationSubtype::IbePpData,
    ApplicationSubtype::Iges,
    ApplicationSubtype::ImIscomposingPlusXml,
    ApplicationSubtype::Index,
    ApplicationSubtype::IndexDotCmd,
    ApplicationSubtype::IndexDotObj,
    ApplicationSubtype::IndexDotResponse,
    ApplicationSubtype::IndexDotVnd,
    ApplicationSubtype::InkmlPlusXml,
    ApplicationSubtype::Iotp,
    ApplicationSubtype::Ipfix,
    ApplicationSubtype::Ipp,
    ApplicationSubtype::Isup,
    ApplicationSubtype::ItsPlusXml,
    ApplicationSubtype::JavaArchive,
    ApplicationSubtype::Javascript,
    ApplicationSubtype::Jf2FeedPlusJson,
    ApplicationSubtype::Jose,
    ApplicationSubtype::JosePlusJson,
    ApplicationSubtype::JrdPlusJson,
    ApplicationSubtype::JscalendarPlusJson,
    ApplicationSubtype::JscontactPlusJson,
    ApplicationSubtype::Json,
    ApplicationSubtype::JsonPatchPlusJson,
    ApplicationSubtype::JsonPatchQueryPlusJson,
    ApplicationSubtype::JsonSeq,
    ApplicationSubtype::Jsonpath,
    ApplicationSubtype::JwkPlusJson,
    ApplicationSubtype::JwkSetPlusJson,
    ApplicationSubtype::JwkSetPlusJwt,
    ApplicationSubtype::Jwt,
    ApplicationSubtype::KbPlusJwt,
    ApplicationSubtype::KblPlusXml,
    ApplicationSubtype::KpmlRequestPlusXml,
    ApplicationSubtype::KpmlResponsePlusXml,
    ApplicationSubtype::LdPlusJson,
    ApplicationSubtype::LgrPlusXml,
    ApplicationSubtype::LinkFormat,
    ApplicationSubtype::Linkset,
    ApplicationSubtype::LinksetPlusJson,
    ApplicationSubtype::LoadControlPlusXml,
    ApplicationSubtype::LogoutPlusJwt,
    ApplicationSubtype::LostPlusXml,
    ApplicationSubtype::LostsyncPlusXml,
    ApplicationSubtype::LpfPlusZip,
    ApplicationSubtype::Lxf,
    ApplicationSubtype::MacBinhex40,
    ApplicationSubtype::Macwriteii,
    ApplicationSubtype::MadsPlusXml,
    ApplicationSubtype::ManifestPlusJson,
    ApplicationSubtype::Marc,
    ApplicationSubtype::MarcxmlPlusXml,
    ApplicationSubtype::Mathematica,
    ApplicationSubtype::MathmlPlusXml,
    ApplicationSubtype::MathmlContentPlusXml,
    ApplicationSubtype::MathmlPresentationPlusXml,
    ApplicationSubtype::MbmsAssociatedProcedureDescriptionPlusXml,
    ApplicationSubtype::MbmsDeregisterPlusXml,
    ApplicationSubtype::MbmsEnvelopePlusXml,
    ApplicationSubtype::MbmsMskPlusXml,
    ApplicationSubtype::MbmsMskResponsePlusXml,
    ApplicationSubtype::MbmsProtectionDescriptionPlusXml,
    ApplicationSubtype::MbmsReceptionReportPlusXml,
    ApplicationSubtype::MbmsRegisterPlusXml,
    ApplicationSubtype::MbmsRegisterResponsePlusXml,
    ApplicationSubtype::MbmsSchedulePlusXml,
    ApplicationSubtype::MbmsUserServiceDescriptionPlusXml,
    ApplicationSubtype::Mbox,
    ApplicationSubtype::MeasuredComponentPlusCbor,
    ApplicationSubtype::MeasuredComponentPlusJson,
    ApplicationSubtype::MediaPolicyDatasetPlusXml,
    ApplicationSubtype::MediaControlPlusXml,
    ApplicationSubtype::MediaservercontrolPlusXml,
    ApplicationSubtype::MergePatchPlusJson,
    ApplicationSubtype::Metalink4PlusXml,
    ApplicationSubtype::MetsPlusXml,
    ApplicationSubtype::Mf4,
    ApplicationSubtype::Mikey,
    ApplicationSubtype::Mipc,
    ApplicationSubtype::MissingBlocksPlusCborSeq,
    ApplicationSubtype::MmtAeiPlusXml,
    ApplicationSubtype::MmtUsdPlusXml,
    ApplicationSubtype::ModsPlusXml,
    ApplicationSubtype::MossKeys,
    ApplicationSubtype::MossSignature,
    ApplicationSubtype::MosskeyData,
    ApplicationSubtype::MosskeyRequest,
    ApplicationSubtype::Mp21,
    ApplicationSubtype::Mp4,
    ApplicationSubtype::Mpeg4Generic,
    ApplicationSubtype::Mpeg4Iod,
    ApplicationSubtype::Mpeg4IodXmt,
    ApplicationSubtype::MrbConsumerPlusXml,
    ApplicationSubtype::MrbPublishPlusXml,
    ApplicationSubtype::MscIvrPlusXml,
    ApplicationSubtype::MscMixerPlusXml,
    ApplicationSubtype::Msword,
    ApplicationSubtype::MudPlusJson,
    ApplicationSubtype::MultipartCore,
    ApplicationSubtype::Mxf,
    ApplicationSubtype::NQuads,
    ApplicationSubtype::NTriples,
    ApplicationSubtype::Nasdata,
    ApplicationSubtype::NewsCheckgroups,
    ApplicationSubtype::NewsGroupinfo,
    ApplicationSubtype::NewsTransmission,
    ApplicationSubtype::NlsmlPlusXml,
    ApplicationSubtype::Node,
    ApplicationSubtype::Nss,
    ApplicationSubtype::OauthAuthzReqPlusJwt,
    ApplicationSubtype::ObliviousDnsMessage,
    ApplicationSubtype::OcspRequest,
    ApplicationSubtype::OcspResponse,
    ApplicationSubtype::OctetStream,
    ApplicationSubtype::Oda,
    ApplicationSubtype::OdmPlusXml,
    ApplicationSubtype::Odx,
    ApplicationSubtype::OebpsPackagePlusXml,
    ApplicationSubtype::Ogg,
    ApplicationSubtype::OhttpKeys,
    ApplicationSubtype::OpcNodesetPlusXml,
    ApplicationSubtype::Oscore,
    ApplicationSubtype::Oxps,
    ApplicationSubtype::P21,
    ApplicationSubtype::P21PlusZip,
    ApplicationSubtype::P2POverlayPlusXml,
    ApplicationSubtype::Parityfec,
    ApplicationSubtype::Passport,
    ApplicationSubtype::PatchOpsErrorPlusXml,
    ApplicationSubtype::Pdf,
    ApplicationSubtype::Pdx,
    ApplicationSubtype::PemCertificateChain,
    ApplicationSubtype::PgpEncrypted,
    ApplicationSubtype::PgpKeys,
    ApplicationSubtype::PgpSignature,
    ApplicationSubtype::PidfPlusXml,
    ApplicationSubtype::PidfDiffPlusXml,
    ApplicationSubtype::Pkcs10,
    ApplicationSubtype::Pkcs12,
    ApplicationSubtype::Pkcs7Mime,
    ApplicationSubtype::Pkcs7Signature,
    ApplicationSubtype::Pkcs8,
    ApplicationSubtype::Pkcs8Encrypted,
    ApplicationSubtype::PkixAttrCert,
    ApplicationSubtype::PkixCert,
    ApplicationSubtype::PkixCrl,
    ApplicationSubtype::PkixPkipath,
    ApplicationSubtype::Pkixcmp,
    ApplicationSubtype::PlsPlusXml,
    ApplicationSubtype::PocSettingsPlusXml,
    ApplicationSubtype::Postscript,
    ApplicationSubtype::PpspTrackerPlusJson,
    ApplicationSubtype::PrivateTokenIssuerDirectory,
    ApplicationSubtype::PrivateTokenRequest,
    ApplicationSubtype::PrivateTokenResponse,
    ApplicationSubtype::ProblemPlusJson,
    ApplicationSubtype::ProblemPlusXml,
    ApplicationSubtype::Protobuf,
    ApplicationSubtype::ProtobufPlusJson,
    ApplicationSubtype::ProvenancePlusXml,
    ApplicationSubtype::ProvidedClaimsPlusJwt,
    ApplicationSubtype::PrsDotAlvestrandDotTitraxSheet,
    ApplicationSubtype::PrsDotBwtc32Key,
    ApplicationSubtype::PrsDotCww,
    ApplicationSubtype::PrsDotCyn,
    ApplicationSubtype::PrsDotHpubPlusZip,
    ApplicationSubtype::PrsDotImpliedDocumentPlusXml,
    ApplicationSubtype::PrsDotImpliedExecutable,
    ApplicationSubtype::PrsDotImpliedObjectPlusJson,
    ApplicationSubtype::PrsDotImpliedObjectPlusJsonSeq,
    ApplicationSubtype::PrsDotImpliedObjectPlusYaml,
    ApplicationSubtype::PrsDotImpliedStructure,
    ApplicationSubtype::PrsDotMayfile,
    ApplicationSubtype::PrsDotNprend,
    ApplicationSubtype::PrsDotPlucker,
    ApplicationSubtype::PrsDotRdfXmlCrypt,
    ApplicationSubtype::PrsDotSclt,
    ApplicationSubtype::PrsDotVcfbzip2,
    ApplicationSubtype::PrsDotXsfPlusXml,
    ApplicationSubtype::PskcPlusXml,
    ApplicationSubtype::PvdPlusJson,
    ApplicationSubtype::Qsig,
    ApplicationSubtype::Raptorfec,
    ApplicationSubtype::RdapPlusJson,
    ApplicationSubtype::RdfPlusXml,
    ApplicationSubtype::ReginfoPlusXml,
    ApplicationSubtype::RelaxNgCompactSyntax,
    ApplicationSubtype::RemotePrinting,
    ApplicationSubtype::ReputonPlusJson,
    ApplicationSubtype::ResolveResponsePlusJwt,
    ApplicationSubtype::ResourceListsPlusXml,
    ApplicationSubtype::ResourceListsDiffPlusXml,
    ApplicationSubtype::RfcPlusXml,
    ApplicationSubtype::Riscos,
    ApplicationSubtype::RlmiPlusXml,
    ApplicationSubtype::RlsServicesPlusXml,
    ApplicationSubtype::RoughtimeMalfeasancePlusJson,
    ApplicationSubtype::RoughtimeServerPlusJson,
    ApplicationSubtype::RouteApdPlusXml,
    ApplicationSubtype::RouteSTsidPlusXml,
    ApplicationSubtype::RouteUsdPlusXml,
    ApplicationSubtype::RpkiChecklist,
    ApplicationSubtype::RpkiGhostbusters,
    ApplicationSubtype::RpkiManifest,
    ApplicationSubtype::RpkiPublication,
    ApplicationSubtype::RpkiRoa,
    ApplicationSubtype::RpkiSignedTal,
    ApplicationSubtype::RpkiUpdown,
    ApplicationSubtype::RsMetadataPlusXml,
    ApplicationSubtype::Rtf,
    ApplicationSubtype::Rtploopback,
    ApplicationSubtype::Rtx,
    ApplicationSubtype::SamlassertionPlusXml,
    ApplicationSubtype::SamlmetadataPlusXml,
    ApplicationSubtype::SarifPlusJson,
    ApplicationSubtype::SarifExternalPropertiesPlusJson,
    ApplicationSubtype::Sbe,
    ApplicationSubtype::SbmlPlusXml,
    ApplicationSubtype::ScaipPlusXml,
    ApplicationSubtype::ScimPlusJson,
    ApplicationSubtype::ScittReceiptPlusCose,
    ApplicationSubtype::ScittStatementPlusCose,
    ApplicationSubtype::ScvpCvRequest,
    ApplicationSubtype::ScvpCvResponse,
    ApplicationSubtype::ScvpVpRequest,
    ApplicationSubtype::ScvpVpResponse,
    ApplicationSubtype::SdJwt,
    ApplicationSubtype::SdJwtPlusJson,
    ApplicationSubtype::SdfPlusJson,
    ApplicationSubtype::Sdp,
    ApplicationSubtype::SeceventPlusJwt,
    ApplicationSubtype::SenmlPlusCbor,
    ApplicationSubtype::SenmlPlusJson,
    ApplicationSubtype::SenmlPlusXml,
    ApplicationSubtype::SenmlEtchPlusCbor,
    ApplicationSubtype::SenmlEtchPlusJson,
    ApplicationSubtype::SenmlExi,
    ApplicationSubtype::SensmlPlusCbor,
    ApplicationSubtype::SensmlPlusJson,
    ApplicationSubtype::SensmlPlusXml,
    ApplicationSubtype::SensmlExi,
    ApplicationSubtype::SepPlusXml,
    ApplicationSubtype::SepExi,
    ApplicationSubtype::SessionInfo,
    ApplicationSubtype::SetPayment,
    ApplicationSubtype::SetPaymentInitiation,
    ApplicationSubtype::SetRegistration,
    ApplicationSubtype::SetRegistrationInitiation,
    ApplicationSubtype::Sgml,
    ApplicationSubtype::SgmlOpenCatalog,
    ApplicationSubtype::ShfPlusXml,
    ApplicationSubtype::Sieve,
    ApplicationSubtype::SimpleFilterPlusXml,
    ApplicationSubtype::SimpleMessageSummary,
    ApplicationSubtype::SimpleSymbolContainer,
    ApplicationSubtype::Sipc,
    ApplicationSubtype::Slate,
    ApplicationSubtype::Smil,
    ApplicationSubtype::SmilPlusXml,
    ApplicationSubtype::Smpte336M,
    ApplicationSubtype::SoapPlusFastinfoset,
    ApplicationSubtype::SoapPlusXml,
    ApplicationSubtype::SparqlQuery,
    ApplicationSubtype::SparqlResultsPlusXml,
    ApplicationSubtype::SpdxPlusJson,
    ApplicationSubtype::SpiritsEventPlusXml,
    ApplicationSubtype::Sql,
    ApplicationSubtype::Srgs,
    ApplicationSubtype::SrgsPlusXml,
    ApplicationSubtype::SruPlusXml,
    ApplicationSubtype::Sslkeylogfile,
    ApplicationSubtype::SsmlPlusXml,
    ApplicationSubtype::St211041,
    ApplicationSubtype::StixPlusJson,
    ApplicationSubtype::Stratum,
    ApplicationSubtype::SuitEnvelopePlusCose,
    ApplicationSubtype::SuitReportPlusCose,
    ApplicationSubtype::SwidPlusCbor,
    ApplicationSubtype::SwidPlusXml,
    ApplicationSubtype::SyslogMsg,
    ApplicationSubtype::TampApexUpdate,
    ApplicationSubtype::TampApexUpdateConfirm,
    ApplicationSubtype::TampCommunityUpdate,
    ApplicationSubtype::TampCommunityUpdateConfirm,
    ApplicationSubtype::TampError,
    ApplicationSubtype::TampSequenceAdjust,
    ApplicationSubtype::TampSequenceAdjustConfirm,
    ApplicationSubtype::TampStatusQuery,
    ApplicationSubtype::TampStatusResponse,
    ApplicationSubtype::TampUpdate,
    ApplicationSubtype::TampUpdateConfirm,
    ApplicationSubtype::TaxiiPlusJson,
    ApplicationSubtype::TdPlusJson,
    ApplicationSubtype::TeepPlusCbor,
    ApplicationSubtype::TeiPlusXml,
    ApplicationSubtype::TetraIsi,
    ApplicationSubtype::Texinfo,
    ApplicationSubtype::ThraudPlusXml,
    ApplicationSubtype::TimestampQuery,
    ApplicationSubtype::TimestampReply,
    ApplicationSubtype::TimestampedData,
    ApplicationSubtype::TlsrptPlusGzip,
    ApplicationSubtype::TlsrptPlusJson,
    ApplicationSubtype::TmPlusJson,
    ApplicationSubtype::Tnauthlist,
    ApplicationSubtype::TocPlusCbor,
    ApplicationSubtype::TokenIntrospectionPlusJwt,
    ApplicationSubtype::Toml,
    ApplicationSubtype::TrickleIceSdpfrag,
    ApplicationSubtype::Trig,
    ApplicationSubtype::TrustChainPlusJson,
    ApplicationSubtype::TrustMarkPlusJwt,
    ApplicationSubtype::TrustMarkDelegationPlusJwt,
    ApplicationSubtype::TrustMarkStatusResponsePlusJwt,
    ApplicationSubtype::TtmlPlusXml,
    ApplicationSubtype::TveTrigger,
    ApplicationSubtype::Tzif,
    ApplicationSubtype::TzifLeap,
    ApplicationSubtype::UccsPlusCbor,
    ApplicationSubtype::UjcsPlusJson,
    ApplicationSubtype::Ulpfec,
    ApplicationSubtype::UrcGrpsheetPlusXml,
    ApplicationSubtype::UrcRessheetPlusXml,
    ApplicationSubtype::UrcTargetdescPlusXml,
    ApplicationSubtype::UrcUisocketdescPlusXml,
    ApplicationSubtype::V3C,
    ApplicationSubtype::Vc,
    ApplicationSubtype::VcPlusCose,
    ApplicationSubtype::VcPlusJwt,
    ApplicationSubtype::VcPlusSdJwt,
    ApplicationSubtype::VcardPlusJson,
    ApplicationSubtype::VcardPlusXml,
    ApplicationSubtype::VecPlusXml,
    ApplicationSubtype::VecPackagePlusGzip,
    ApplicationSubtype::VecPackagePlusZip,
    ApplicationSubtype::Vemmi,
    ApplicationSubtype::VndDot1000MindsDotDecisionModelPlusXml,
    ApplicationSubtype::VndDot1Ob,
    ApplicationSubtype::VndDot3GppProsePlusXml,
    ApplicationSubtype::VndDot3GppProsePc3APlusXml,
    ApplicationSubtype::VndDot3GppProsePc3AchPlusXml,
    ApplicationSubtype::VndDot3GppProsePc3ChPlusXml,
    ApplicationSubtype::VndDot3GppProsePc8PlusXml,
    ApplicationSubtype::VndDot3GppV2XLocalServiceInformation,
    ApplicationSubtype::VndDot3GppDot5Gnas,
    ApplicationSubtype::VndDot3GppDot5Gsa2X,
    ApplicationSubtype::VndDot3GppDot5Gsa2XLocalServiceInformation,
    ApplicationSubtype::VndDot3GppDot5Gsv2X,
    ApplicationSubtype::VndDot3GppDot5Gsv2XLocalServiceInformation,
    ApplicationSubtype::VndDot3GppDotAccessTransferEventsPlusXml,
    ApplicationSubtype::VndDot3GppDotBsfPlusXml,
    ApplicationSubtype::VndDot3GppDotCrsPlusXml,
    ApplicationSubtype::VndDot3GppDotCurrentLocationDiscoveryPlusXml,
    ApplicationSubtype::VndDot3GppDotGmopPlusXml,
    ApplicationSubtype::VndDot3GppDotGtpc,
    ApplicationSubtype::VndDot3GppDotInterworkingData,
    ApplicationSubtype::VndDot3GppDotLpp,
    ApplicationSubtype::VndDot3GppDotMcSignallingEar,
    ApplicationSubtype::VndDot3GppDotMcdataAffiliationCommandPlusXml,
    ApplicationSubtype::VndDot3GppDotMcdataInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotMcdataMsgstoreCtrlRequestPlusXml,
    ApplicationSubtype::VndDot3GppDotMcdataPayload,
    ApplicationSubtype::VndDot3GppDotMcdataRegroupPlusXml,
    ApplicationSubtype::VndDot3GppDotMcdataServiceConfigPlusXml,
    ApplicationSubtype::VndDot3GppDotMcdataSignalling,
    ApplicationSubtype::VndDot3GppDotMcdataUeConfigPlusXml,
    ApplicationSubtype::VndDot3GppDotMcdataUserProfilePlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttAffiliationCommandPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttFloorRequestPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttLocationInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttMbmsUsageInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttRegroupPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttServiceConfigPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttSignedPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttUeConfigPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttUeInitConfigPlusXml,
    ApplicationSubtype::VndDot3GppDotMcpttUserProfilePlusXml,
    ApplicationSubtype::VndDot3GppDotMcsLocationUserConfigPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoAffiliationCommandPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoAffiliationInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoLocationInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoMbmsUsageInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoRegroupPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoServiceConfigPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoTransmissionRequestPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoUeConfigPlusXml,
    ApplicationSubtype::VndDot3GppDotMcvideoUserProfilePlusXml,
    ApplicationSubtype::VndDot3GppDotMidCallPlusXml,
    ApplicationSubtype::VndDot3GppDotNgap,
    ApplicationSubtype::VndDot3GppDotPfcp,
    ApplicationSubtype::VndDot3GppDotPicBwLarge,
    ApplicationSubtype::VndDot3GppDotPicBwSmall,
    ApplicationSubtype::VndDot3GppDotPicBwVar,
    ApplicationSubtype::VndDot3GppDotPinappInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotS1Ap,
    ApplicationSubtype::VndDot3GppDotSealAppCommRequirementsInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSealDataDeliveryInfoPlusCbor,
    ApplicationSubtype::VndDot3GppDotSealDataDeliveryInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSealGroupDocPlusXml,
    ApplicationSubtype::VndDot3GppDotSealInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSealLocationInfoPlusCbor,
    ApplicationSubtype::VndDot3GppDotSealLocationInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSealMbmsUsageInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSealMbsUsageInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSealNetworkQoSManagementInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSealNetworkResourceInfoPlusCbor,
    ApplicationSubtype::VndDot3GppDotSealUeConfigInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSealUnicastInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSealUserProfileInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotSms,
    ApplicationSubtype::VndDot3GppDotSmsPlusXml,
    ApplicationSubtype::VndDot3GppDotSrvccExtPlusXml,
    ApplicationSubtype::VndDot3GppDotSrvccInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotStateAndEventInfoPlusXml,
    ApplicationSubtype::VndDot3GppDotUssdPlusXml,
    ApplicationSubtype::VndDot3GppDotV2X,
    ApplicationSubtype::VndDot3GppDotVaeInfoPlusXml,
    ApplicationSubtype::VndDot3Gpp2DotBcmcsinfoPlusXml,
    ApplicationSubtype::VndDot3Gpp2DotSms,
    ApplicationSubtype::VndDot3Gpp2DotTcap,
    ApplicationSubtype::VndDot3LightssoftwareDotImagescal,
    ApplicationSubtype::VndDot3MDotPostItNotes,
    ApplicationSubtype::VndDotAccpacDotSimplyDotAso,
    ApplicationSubtype::VndDotAccpacDotSimplyDotImp,
    ApplicationSubtype::VndDotAcmDotAddressxferPlusJson,
    ApplicationSubtype::VndDotAcmDotChatbotPlusJson,
    ApplicationSubtype::VndDotAcucobol,
    ApplicationSubtype::VndDotAcucorp,
    ApplicationSubtype::VndDotAdobeDotFlashDotMovie,
    ApplicationSubtype::VndDotAdobeDotFormscentralDotFcdt,
    ApplicationSubtype::VndDotAdobeDotFxp,
    ApplicationSubtype::VndDotAdobeDotPartialUpload,
    ApplicationSubtype::VndDotAdobeDotXdpPlusXml,
    ApplicationSubtype::VndDotAetherDotImp,
    ApplicationSubtype::VndDotAfpcDotAfplinedata,
    ApplicationSubtype::VndDotAfpcDotAfplinedataPagedef,
    ApplicationSubtype::VndDotAfpcDotCmocaCmresource,
    ApplicationSubtype::VndDotAfpcDotFocaCharset,
    ApplicationSubtype::VndDotAfpcDotFocaCodedfont,
    ApplicationSubtype::VndDotAfpcDotFocaCodepage,
    ApplicationSubtype::VndDotAfpcDotModca,
    ApplicationSubtype::VndDotAfpcDotModcaCmtable,
    ApplicationSubtype::VndDotAfpcDotModcaFormdef,
    ApplicationSubtype::VndDotAfpcDotModcaMediummap,
    ApplicationSubtype::VndDotAfpcDotModcaObjectcontainer,
    ApplicationSubtype::VndDotAfpcDotModcaOverlay,
    ApplicationSubtype::VndDotAfpcDotModcaPagesegment,
    ApplicationSubtype::VndDotAge,
    ApplicationSubtype::VndDotAhBarcode,
    ApplicationSubtype::VndDotAheadDotSpace,
    ApplicationSubtype::VndDotAia,
    ApplicationSubtype::VndDotAirzipDotFilesecureDotAzf,
    ApplicationSubtype::VndDotAirzipDotFilesecureDotAzs,
    ApplicationSubtype::VndDotAmadeusPlusJson,
    ApplicationSubtype::VndDotAmazonDotMobi8Ebook,
    ApplicationSubtype::VndDotAmericandynamicsDotAcc,
    ApplicationSubtype::VndDotAmigaDotAmi,
    ApplicationSubtype::VndDotAmundsenDotMazePlusXml,
    ApplicationSubtype::VndDotAndroidDotOta,
    ApplicationSubtype::VndDotAnki,
    ApplicationSubtype::VndDotAnserWebCertificateIssueInitiation,
    ApplicationSubtype::VndDotAntixDotGameComponent,
    ApplicationSubtype::VndDotApacheDotArrowDotFile,
    ApplicationSubtype::VndDotApacheDotArrowDotStream,
    ApplicationSubtype::VndDotApacheDotParquet,
    ApplicationSubtype::VndDotApacheDotThriftDotBinary,
    ApplicationSubtype::VndDotApacheDotThriftDotCompact,
    ApplicationSubtype::VndDotApacheDotThriftDotJson,
    ApplicationSubtype::VndDotApexlang,
    ApplicationSubtype::VndDotApiPlusJson,
    ApplicationSubtype::VndDotAplextorDotWarrpPlusJson,
    ApplicationSubtype::VndDotApothekendeDotReservationPlusJson,
    ApplicationSubtype::VndDotAppleDotInstallerPlusXml,
    ApplicationSubtype::VndDotAppleDotKeynote,
    ApplicationSubtype::VndDotAppleDotMpegurl,
    ApplicationSubtype::VndDotAppleDotNumbers,
    ApplicationSubtype::VndDotAppleDotPages,
    ApplicationSubtype::VndDotArastraDotSwi,
    ApplicationSubtype::VndDotAristanetworksDotSwi,
    ApplicationSubtype::VndDotArtisanPlusJson,
    ApplicationSubtype::VndDotArtsquare,
    ApplicationSubtype::VndDotAs207960DotVasDotConfigPlusJer,
    ApplicationSubtype::VndDotAs207960DotVasDotConfigPlusUper,
    ApplicationSubtype::VndDotAs207960DotVasDotTapPlusJer,
    ApplicationSubtype::VndDotAs207960DotVasDotTapPlusUper,
    ApplicationSubtype::VndDotAstraeaSoftwareDotIota,
    ApplicationSubtype::VndDotAudiograph,
    ApplicationSubtype::VndDotAutopackage,
    ApplicationSubtype::VndDotAvalonPlusJson,
    ApplicationSubtype::VndDotAvistarPlusXml,
    ApplicationSubtype::VndDotBalsamiqDotBmmlPlusXml,
    ApplicationSubtype::VndDotBalsamiqDotBmpr,
    ApplicationSubtype::VndDotBananaAccounting,
    ApplicationSubtype::VndDotBbfDotUspDotError,
    ApplicationSubtype::VndDotBbfDotUspDotMsg,
    ApplicationSubtype::VndDotBbfDotUspDotMsgPlusJson,
    ApplicationSubtype::VndDotBekitzurStechPlusJson,
    ApplicationSubtype::VndDotBelightsoftDotLhzdPlusZip,
    ApplicationSubtype::VndDotBelightsoftDotLhzlPlusZip,
    ApplicationSubtype::VndDotBintDotMedContent,
    ApplicationSubtype::VndDotBiopaxDotRdfPlusXml,
    ApplicationSubtype::VndDotBlinkIdbValueWrapper,
    ApplicationSubtype::VndDotBlueiceDotMultipass,
    ApplicationSubtype::VndDotBluetoothDotEpDotOob,
    ApplicationSubtype::VndDotBluetoothDotLeDotOob,
    ApplicationSubtype::VndDotBmi,
    ApplicationSubtype::VndDotBpf,
    ApplicationSubtype::VndDotBpf3,
    ApplicationSubtype::VndDotBusinessobjects,
    ApplicationSubtype::VndDotByuDotUapiPlusJson,
    ApplicationSubtype::VndDotBzip3,
    ApplicationSubtype::VndDotC3VocDotSchedulePlusXml,
    ApplicationSubtype::VndDotCabJscript,
    ApplicationSubtype::VndDotCanonCpdl,
    ApplicationSubtype::VndDotCanonLips,
    ApplicationSubtype::VndDotCapasystemsPgPlusJson,
    ApplicationSubtype::VndDotCel,
    ApplicationSubtype::VndDotCendioDotThinlincDotClientconf,
    ApplicationSubtype::VndDotCenturySystemsDotTcpStream,
    ApplicationSubtype::VndDotChemdrawPlusXml,
    ApplicationSubtype::VndDotChessPgn,
    ApplicationSubtype::VndDotChipnutsDotKaraokeMmd,
    ApplicationSubtype::VndDotCiedi,
    ApplicationSubtype::VndDotCinderella,
    ApplicationSubtype::VndDotCirpackDotIsdnExt,
    ApplicationSubtype::VndDotCitationstylesDotStylePlusXml,
    ApplicationSubtype::VndDotClaymore,
    ApplicationSubtype::VndDotCloantoDotRp9,
    ApplicationSubtype::VndDotClonkDotC4Group,
    ApplicationSubtype::VndDotCluetrustDotCartomobileConfig,
    ApplicationSubtype::VndDotCluetrustDotCartomobileConfigPkg,
    ApplicationSubtype::VndDotCmmfConfigurationInformationPlusJson,
    ApplicationSubtype::VndDotCmmfEfdPlusXml,
    ApplicationSubtype::VndDotCmmfEncoderConfigurationPlusJson,
    ApplicationSubtype::VndDotCncfDotHelmDotChartDotContentDotV1DotTarPlusGzip,
    ApplicationSubtype::VndDotCncfDotHelmDotChartDotProvenanceDotV1DotProv,
    ApplicationSubtype::VndDotCncfDotHelmDotConfigDotV1PlusJson,
    ApplicationSubtype::VndDotCoffeescript,
    ApplicationSubtype::VndDotCollabioDotXodocumentsDotDocument,
    ApplicationSubtype::VndDotCollabioDotXodocumentsDotDocumentTemplate,
    ApplicationSubtype::VndDotCollabioDotXodocumentsDotPresentation,
    ApplicationSubtype::VndDotCollabioDotXodocumentsDotPresentationTemplate,
    ApplicationSubtype::VndDotCollabioDotXodocumentsDotSpreadsheet,
    ApplicationSubtype::VndDotCollabioDotXodocumentsDotSpreadsheetTemplate,
    ApplicationSubtype::VndDotCollectionPlusJson,
    ApplicationSubtype::VndDotCollectionDotDocPlusJson,
    ApplicationSubtype::VndDotCollectionDotNextPlusJson,
    ApplicationSubtype::VndDotComicbookPlusZip,
    ApplicationSubtype::VndDotComicbookRar,
    ApplicationSubtype::VndDotCommerceBattelle,
    ApplicationSubtype::VndDotCommonspace,
    ApplicationSubtype::VndDotContactDotCmsg,
    ApplicationSubtype::VndDotCoreosDotIgnitionPlusJson,
    ApplicationSubtype::VndDotCosmocaller,
    ApplicationSubtype::VndDotCrickDotClicker,
    ApplicationSubtype::VndDotCrickDotClickerDotKeyboard,
    ApplicationSubtype::VndDotCrickDotClickerDotPalette,
    ApplicationSubtype::VndDotCrickDotClickerDotTemplate,
    ApplicationSubtype::VndDotCrickDotClickerDotWordbank,
    ApplicationSubtype::VndDotCriticaltoolsDotWbsPlusXml,
    ApplicationSubtype::VndDotCryptiiDotPipePlusJson,
    ApplicationSubtype::VndDotCryptoShadeFile,
    ApplicationSubtype::VndDotCryptomatorDotEncrypted,
    ApplicationSubtype::VndDotCryptomatorDotVault,
    ApplicationSubtype::VndDotCtcPosml,
    ApplicationSubtype::VndDotCtctDotWsPlusXml,
    ApplicationSubtype::VndDotCupsPdf,
    ApplicationSubtype::VndDotCupsPostscript,
    ApplicationSubtype::VndDotCupsPpd,
    ApplicationSubtype::VndDotCupsRaster,
    ApplicationSubtype::VndDotCupsRaw,
    ApplicationSubtype::VndDotCurl,
    ApplicationSubtype::VndDotCyanDotDeanDotRootPlusXml,
    ApplicationSubtype::VndDotCybank,
    ApplicationSubtype::VndDotCyclonedxPlusJson,
    ApplicationSubtype::VndDotCyclonedxPlusXml,
    ApplicationSubtype::VndDotD2LDotCoursepackage1P0PlusZip,
    ApplicationSubtype::VndDotD3MDataset,
    ApplicationSubtype::VndDotD3MProblem,
    ApplicationSubtype::VndDotDart,
    ApplicationSubtype::VndDotDataVisionDotRdz,
    ApplicationSubtype::VndDotDatalog,
    ApplicationSubtype::VndDotDatapackagePlusJson,
    ApplicationSubtype::VndDotDataresourcePlusJson,
    ApplicationSubtype::VndDotDbf,
    ApplicationSubtype::VndDotDcmpPlusXml,
    ApplicationSubtype::VndDotDebianDotBinaryPackage,
    ApplicationSubtype::VndDotDeceDotData,
    ApplicationSubtype::VndDotDeceDotTtmlPlusXml,
    ApplicationSubtype::VndDotDeceDotUnspecified,
    ApplicationSubtype::VndDotDeceDotZip,
    ApplicationSubtype::VndDotDenovoDotFcselayoutLink,
    ApplicationSubtype::VndDotDesmumeDotMovie,
    ApplicationSubtype::VndDotDeutPlusJson,
    ApplicationSubtype::VndDotDirBiDotPlateDlNosuffix,
    ApplicationSubtype::VndDotDmDotDelegationPlusXml,
    ApplicationSubtype::VndDotDna,
    ApplicationSubtype::VndDotDocumentPlusJson,
    ApplicationSubtype::VndDotDolbyDotMobileDot1,
    ApplicationSubtype::VndDotDolbyDotMobileDot2,
    ApplicationSubtype::VndDotDoremirDotScorecloudBinaryDocument,
    ApplicationSubtype::VndDotDpgraph,
    ApplicationSubtype::VndDotDreamfactory,
    ApplicationSubtype::VndDotDrivePlusJson,
    ApplicationSubtype::VndDotDtgDotLocal,
    ApplicationSubtype::VndDotDtgDotLocalDotFlash,
    ApplicationSubtype::VndDotDtgDotLocalDotHtml,
    ApplicationSubtype::VndDotDvbDotAit,
    ApplicationSubtype::VndDotDvbDotDvbislPlusXml,
    ApplicationSubtype::VndDotDvbDotDvbj,
    ApplicationSubtype::VndDotDvbDotEsgcontainer,
    ApplicationSubtype::VndDotDvbDotIpdcdftnotifaccess,
    ApplicationSubtype::VndDotDvbDotIpdcesgaccess,
    ApplicationSubtype::VndDotDvbDotIpdcesgaccess2,
    ApplicationSubtype::VndDotDvbDotIpdcesgpdd,
    ApplicationSubtype::VndDotDvbDotIpdcroaming,
    ApplicationSubtype::VndDotDvbDotIptvDotAlfecBase,
    ApplicationSubtype::VndDotDvbDotIptvDotAlfecEnhancement,
    ApplicationSubtype::VndDotDvbDotNotifAggregateRootPlusXml,
    ApplicationSubtype::VndDotDvbDotNotifContainerPlusXml,
    ApplicationSubtype::VndDotDvbDotNotifGenericPlusXml,
    ApplicationSubtype::VndDotDvbDotNotifIaMsglistPlusXml,
    ApplicationSubtype::VndDotDvbDotNotifIaRegistrationRequestPlusXml,
    ApplicationSubtype::VndDotDvbDotNotifIaRegistrationResponsePlusXml,
    ApplicationSubtype::VndDotDvbDotNotifInitPlusXml,
    ApplicationSubtype::VndDotDvbDotPfr,
    ApplicationSubtype::VndDotDvbDotService,
    ApplicationSubtype::VndDotDxr,
    ApplicationSubtype::VndDotDynageo,
    ApplicationSubtype::VndDotDzr,
    ApplicationSubtype::VndDotEasykaraokeDotCdgdownload,
    ApplicationSubtype::VndDotEcdisUpdate,
    ApplicationSubtype::VndDotEcipDotRlp,
    ApplicationSubtype::VndDotEclipseDotDittoPlusJson,
    ApplicationSubtype::VndDotEcowinDotChart,
    ApplicationSubtype::VndDotEcowinDotFilerequest,
    ApplicationSubtype::VndDotEcowinDotFileupdate,
    ApplicationSubtype::VndDotEcowinDotSeries,
    ApplicationSubtype::VndDotEcowinDotSeriesrequest,
    ApplicationSubtype::VndDotEcowinDotSeriesupdate,
    ApplicationSubtype::VndDotEdulithDotEduxPlusJson,
    ApplicationSubtype::VndDotEfiDotImg,
    ApplicationSubtype::VndDotEfiDotIso,
    ApplicationSubtype::VndDotElnPlusZip,
    ApplicationSubtype::VndDotEmclientDotAccessrequestPlusXml,
    ApplicationSubtype::VndDotEnliven,
    ApplicationSubtype::VndDotEnphaseDotEnvoy,
    ApplicationSubtype::VndDotEprintsDotDataPlusXml,
    ApplicationSubtype::VndDotEpsonDotEsf,
    ApplicationSubtype::VndDotEpsonDotMsf,
    ApplicationSubtype::VndDotEpsonDotQuickanime,
    ApplicationSubtype::VndDotEpsonDotSalt,
    ApplicationSubtype::VndDotEpsonDotSsf,
    ApplicationSubtype::VndDotEricssonDotQuickcall,
    ApplicationSubtype::VndDotErofs,
    ApplicationSubtype::VndDotEspassEspassPlusZip,
    ApplicationSubtype::VndDotEszigno3PlusXml,
    ApplicationSubtype::VndDotEtsiDotAocPlusXml,
    ApplicationSubtype::VndDotEtsiDotAsicEPlusZip,
    ApplicationSubtype::VndDotEtsiDotAsicSPlusZip,
    ApplicationSubtype::VndDotEtsiDotCugPlusXml,
    ApplicationSubtype::VndDotEtsiDotIptvcommandPlusXml,
    ApplicationSubtype::VndDotEtsiDotIptvdiscoveryPlusXml,
    ApplicationSubtype::VndDotEtsiDotIptvprofilePlusXml,
    ApplicationSubtype::VndDotEtsiDotIptvsadBcPlusXml,
    ApplicationSubtype::VndDotEtsiDotIptvsadCodPlusXml,
    ApplicationSubtype::VndDotEtsiDotIptvsadNpvrPlusXml,
    ApplicationSubtype::VndDotEtsiDotIptvservicePlusXml,
    ApplicationSubtype::VndDotEtsiDotIptvsyncPlusXml,
    ApplicationSubtype::VndDotEtsiDotIptvueprofilePlusXml,
    ApplicationSubtype::VndDotEtsiDotMcidPlusXml,
    ApplicationSubtype::VndDotEtsiDotMheg5,
    ApplicationSubtype::VndDotEtsiDotOverloadControlPolicyDatasetPlusXml,
    ApplicationSubtype::VndDotEtsiDotPstnPlusXml,
    ApplicationSubtype::VndDotEtsiDotSciPlusXml,
    ApplicationSubtype::VndDotEtsiDotSimservsPlusXml,
    ApplicationSubtype::VndDotEtsiDotTimestampToken,
    ApplicationSubtype::VndDotEtsiDotTslPlusXml,
    ApplicationSubtype::VndDotEtsiDotTslDotDer,
    ApplicationSubtype::VndDotEuDotKasparianDotCarPlusJson,
    ApplicationSubtype::VndDotEudoraDotData,
    ApplicationSubtype::VndDotEvolvDotEcigDotProfile,
    ApplicationSubtype::VndDotEvolvDotEcigDotSettings,
    ApplicationSubtype::VndDotEvolvDotEcigDotTheme,
    ApplicationSubtype::VndDotExstreamEmpowerPlusZip,
    ApplicationSubtype::VndDotExstreamPackage,
    ApplicationSubtype::VndDotEzpixAlbum,
    ApplicationSubtype::VndDotEzpixPackage,
    ApplicationSubtype::VndDotFSecureDotMobile,
    ApplicationSubtype::VndDotFafPlusYaml,
    ApplicationSubtype::VndDotFamilysearchDotGedcomPlusZip,
    ApplicationSubtype::VndDotFastcopyDiskImage,
    ApplicationSubtype::VndDotFdsnDotMseed,
    ApplicationSubtype::VndDotFdsnDotSeed,
    ApplicationSubtype::VndDotFdsnDotStationxmlPlusXml,
    ApplicationSubtype::VndDotFfsns,
    ApplicationSubtype::VndDotFgb,
    ApplicationSubtype::VndDotFiclabDotFlbPlusZip,
    ApplicationSubtype::VndDotFilmitDotZfc,
    ApplicationSubtype::VndDotFints,
    ApplicationSubtype::VndDotFiremonkeysDotCloudcell,
    ApplicationSubtype::VndDotFloGraphIt,
    ApplicationSubtype::VndDotFluxtimeDotClip,
    ApplicationSubtype::VndDotFontFontforgeSfd,
    ApplicationSubtype::VndDotForitechDotContainer,
    ApplicationSubtype::VndDotFramemaker,
    ApplicationSubtype::VndDotFreelogDotComic,
    ApplicationSubtype::VndDotFrogansDotFnc,
    ApplicationSubtype::VndDotFrogansDotLtf,
    ApplicationSubtype::VndDotFscDotWeblaunch,
    ApplicationSubtype::VndDotFujifilmDotFbDotDocuworks,
    ApplicationSubtype::VndDotFujifilmDotFbDotDocuworksDotBinder,
    ApplicationSubtype::VndDotFujifilmDotFbDotDocuworksDotContainer,
    ApplicationSubtype::VndDotFujifilmDotFbDotJfiPlusXml,
    ApplicationSubtype::VndDotFujitsuDotOasys,
    ApplicationSubtype::VndDotFujitsuDotOasys2,
    ApplicationSubtype::VndDotFujitsuDotOasys3,
    ApplicationSubtype::VndDotFujitsuDotOasysgp,
    ApplicationSubtype::VndDotFujitsuDotOasysprs,
    ApplicationSubtype::VndDotFujixeroxDotArtEx,
    ApplicationSubtype::VndDotFujixeroxDotArt4,
    ApplicationSubtype::VndDotFujixeroxDotDdd,
    ApplicationSubtype::VndDotFujixeroxDotDocuworks,
    ApplicationSubtype::VndDotFujixeroxDotDocuworksDotBinder,
    ApplicationSubtype::VndDotFujixeroxDotDocuworksDotContainer,
    ApplicationSubtype::VndDotFujixeroxDotHbpl,
    ApplicationSubtype::VndDotFutMisnet,
    ApplicationSubtype::VndDotFutoinPlusCbor,
    ApplicationSubtype::VndDotFutoinPlusJson,
    ApplicationSubtype::VndDotFuzzysheet,
    ApplicationSubtype::VndDotG3PixDotG3Fc,
    ApplicationSubtype::VndDotGa4GhDotPassportPlusJwt,
    ApplicationSubtype::VndDotGenomatixDotTuxedo,
    ApplicationSubtype::VndDotGenozip,
    ApplicationSubtype::VndDotGenticsDotGrdPlusJson,
    ApplicationSubtype::VndDotGentooDotCatmetadataPlusXml,
    ApplicationSubtype::VndDotGentooDotEbuild,
    ApplicationSubtype::VndDotGentooDotEclass,
    ApplicationSubtype::VndDotGentooDotGpkg,
    ApplicationSubtype::VndDotGentooDotManifest,
    ApplicationSubtype::VndDotGentooDotPkgmetadataPlusXml,
    ApplicationSubtype::VndDotGentooDotXpak,
    ApplicationSubtype::VndDotGeoPlusJson,
    ApplicationSubtype::VndDotGeocubePlusXml,
    ApplicationSubtype::VndDotGeogebraDotFile,
    ApplicationSubtype::VndDotGeogebraDotPinboard,
    ApplicationSubtype::VndDotGeogebraDotSlides,
    ApplicationSubtype::VndDotGeogebraDotTool,
    ApplicationSubtype::VndDotGeometryExplorer,
    ApplicationSubtype::VndDotGeonext,
    ApplicationSubtype::VndDotGeoplan,
    ApplicationSubtype::VndDotGeospace,
    ApplicationSubtype::VndDotGerber,
    ApplicationSubtype::VndDotGlobalplatformDotCardContentMgt,
    ApplicationSubtype::VndDotGlobalplatformDotCardContentMgtResponse,
    ApplicationSubtype::VndDotGmxDeprecated,
    ApplicationSubtype::VndDotGnuDotTalerDotExchangePlusJson,
    ApplicationSubtype::VndDotGnuDotTalerDotMerchantPlusJson,
    ApplicationSubtype::VndDotGoogleEarthDotKmlPlusXml,
    ApplicationSubtype::VndDotGoogleEarthDotKmz,
    ApplicationSubtype::VndDotGovDotSkDotEFormPlusXml,
    ApplicationSubtype::VndDotGovDotSkDotEFormPlusZip,
    ApplicationSubtype::VndDotGovDotSkDotXmldatacontainerPlusXml,
    ApplicationSubtype::VndDotGp3,
    ApplicationSubtype::VndDotGpxseeDotMapPlusXml,
    ApplicationSubtype::VndDotGrafeq,
    ApplicationSubtype::VndDotGridmp,
    ApplicationSubtype::VndDotGrooveAccount,
    ApplicationSubtype::VndDotGrooveHelp,
    ApplicationSubtype::VndDotGrooveIdentityMessage,
    ApplicationSubtype::VndDotGrooveInjector,
    ApplicationSubtype::VndDotGrooveToolMessage,
    ApplicationSubtype::VndDotGrooveToolTemplate,
    ApplicationSubtype::VndDotGrooveVcard,
    ApplicationSubtype::VndDotHalPlusJson,
    ApplicationSubtype::VndDotHalPlusXml,
    ApplicationSubtype::VndDotHandHeldEntertainmentPlusXml,
    ApplicationSubtype::VndDotHbci,
    ApplicationSubtype::VndDotHcPlusJson,
    ApplicationSubtype::VndDotHclBireports,
    ApplicationSubtype::VndDotHdfgroupDotHdf4,
    ApplicationSubtype::VndDotHdfgroupDotHdf5,
    ApplicationSubtype::VndDotHdt,
    ApplicationSubtype::VndDotHerokuPlusJson,
    ApplicationSubtype::VndDotHheDotLessonPlayer,
    ApplicationSubtype::VndDotHpHpgl,
    ApplicationSubtype::VndDotHpHpid,
    ApplicationSubtype::VndDotHpHps,
    ApplicationSubtype::VndDotHpJlyt,
    ApplicationSubtype::VndDotHpPcl,
    ApplicationSubtype::VndDotHpPclxl,
    ApplicationSubtype::VndDotHsl,
    ApplicationSubtype::VndDotHttphone,
    ApplicationSubtype::VndDotHydrostatixDotSofData,
    ApplicationSubtype::VndDotHyperPlusJson,
    ApplicationSubtype::VndDotHyperItemPlusJson,
    ApplicationSubtype::VndDotHyperdrivePlusJson,
    ApplicationSubtype::VndDotHzn3DCrossword,
    ApplicationSubtype::VndDotIbmDotAfplinedata,
    ApplicationSubtype::VndDotIbmDotElectronicMedia,
    ApplicationSubtype::VndDotIbmDotMiniPay,
    ApplicationSubtype::VndDotIbmDotModcap,
    ApplicationSubtype::VndDotIbmDotRightsManagement,
    ApplicationSubtype::VndDotIbmDotSecureContainer,
    ApplicationSubtype::VndDotIccprofile,
    ApplicationSubtype::VndDotIeeeDot1905,
    ApplicationSubtype::VndDotIgloader,
    ApplicationSubtype::VndDotImagemeterDotFolderPlusZip,
    ApplicationSubtype::VndDotImagemeterDotImagePlusZip,
    ApplicationSubtype::VndDotImmervisionIvp,
    ApplicationSubtype::VndDotImmervisionIvu,
    ApplicationSubtype::VndDotImsDotImsccv1P1,
    ApplicationSubtype::VndDotImsDotImsccv1P2,
    ApplicationSubtype::VndDotImsDotImsccv1P3,
    ApplicationSubtype::VndDotImsDotLisDotV2DotResultPlusJson,
    ApplicationSubtype::VndDotImsDotLtiDotV2DotToolconsumerprofilePlusJson,
    ApplicationSubtype::VndDotImsDotLtiDotV2DotToolproxyPlusJson,
    ApplicationSubtype::VndDotImsDotLtiDotV2DotToolproxyDotIdPlusJson,
    ApplicationSubtype::VndDotImsDotLtiDotV2DotToolsettingsPlusJson,
    ApplicationSubtype::VndDotImsDotLtiDotV2DotToolsettingsDotSimplePlusJson,
    ApplicationSubtype::VndDotInformedcontrolDotRmsPlusXml,
    ApplicationSubtype::VndDotInformixVisionary,
    ApplicationSubtype::VndDotInfotechDotProject,
    ApplicationSubtype::VndDotInfotechDotProjectPlusXml,
    ApplicationSubtype::VndDotInnopathDotWampDotNotification,
    ApplicationSubtype::VndDotInsorsDotIgm,
    ApplicationSubtype::VndDotInterconDotFormnet,
    ApplicationSubtype::VndDotIntergeo,
    ApplicationSubtype::VndDotIntertrustDotDigibox,
    ApplicationSubtype::VndDotIntertrustDotNncp,
    ApplicationSubtype::VndDotIntuDotQbo,
    ApplicationSubtype::VndDotIntuDotQfx,
    ApplicationSubtype::VndDotIpfsDotIpnsRecord,
    ApplicationSubtype::VndDotIpldDotCar,
    ApplicationSubtype::VndDotIpldDotDagCbor,
    ApplicationSubtype::VndDotIpldDotDagJson,
    ApplicationSubtype::VndDotIpldDotRaw,
    ApplicationSubtype::VndDotIptcDotG2DotCatalogitemPlusXml,
    ApplicationSubtype::VndDotIptcDotG2DotConceptitemPlusXml,
    ApplicationSubtype::VndDotIptcDotG2DotKnowledgeitemPlusXml,
    ApplicationSubtype::VndDotIptcDotG2DotNewsitemPlusXml,
    ApplicationSubtype::VndDotIptcDotG2DotNewsmessagePlusXml,
    ApplicationSubtype::VndDotIptcDotG2DotPackageitemPlusXml,
    ApplicationSubtype::VndDotIptcDotG2DotPlanningitemPlusXml,
    ApplicationSubtype::VndDotIpunpluggedDotRcprofile,
    ApplicationSubtype::VndDotIrepositoryDotPackagePlusXml,
    ApplicationSubtype::VndDotIsXpr,
    ApplicationSubtype::VndDotIsacDotFcs,
    ApplicationSubtype::VndDotIso1178310PlusZip,
    ApplicationSubtype::VndDotJam,
    ApplicationSubtype::VndDotJapannetDirectoryService,
    ApplicationSubtype::VndDotJapannetJpnstoreWakeup,
    ApplicationSubtype::VndDotJapannetPaymentWakeup,
    ApplicationSubtype::VndDotJapannetRegistration,
    ApplicationSubtype::VndDotJapannetRegistrationWakeup,
    ApplicationSubtype::VndDotJapannetSetstoreWakeup,
    ApplicationSubtype::VndDotJapannetVerification,
    ApplicationSubtype::VndDotJapannetVerificationWakeup,
    ApplicationSubtype::VndDotJcpDotJavameDotMidletRms,
    ApplicationSubtype::VndDotJisp,
    ApplicationSubtype::VndDotJoostDotJodaArchive,
    ApplicationSubtype::VndDotJskDotIsdnNgn,
    ApplicationSubtype::VndDotKahootz,
    ApplicationSubtype::VndDotKdeDotKarbon,
    ApplicationSubtype::VndDotKdeDotKchart,
    ApplicationSubtype::VndDotKdeDotKformula,
    ApplicationSubtype::VndDotKdeDotKivio,
    ApplicationSubtype::VndDotKdeDotKontour,
    ApplicationSubtype::VndDotKdeDotKpresenter,
    ApplicationSubtype::VndDotKdeDotKspread,
    ApplicationSubtype::VndDotKdeDotKword,
    ApplicationSubtype::VndDotKdl,
    ApplicationSubtype::VndDotKenameaapp,
    ApplicationSubtype::VndDotKeymanDotKmpPlusZip,
    ApplicationSubtype::VndDotKeymanDotKmx,
    ApplicationSubtype::VndDotKidspiration,
    ApplicationSubtype::VndDotKinar,
    ApplicationSubtype::VndDotKoan,
    ApplicationSubtype::VndDotKodakDescriptor,
    ApplicationSubtype::VndDotLas,
    ApplicationSubtype::VndDotLasDotLasPlusJson,
    ApplicationSubtype::VndDotLasDotLasPlusXml,
    ApplicationSubtype::VndDotLaszip,
    ApplicationSubtype::VndDotLdevDotProductlicensing,
    ApplicationSubtype::VndDotLeapPlusJson,
    ApplicationSubtype::VndDotLibertyRequestPlusXml,
    ApplicationSubtype::VndDotLlamagraphicsDotLifeBalanceDotDesktop,
    ApplicationSubtype::VndDotLlamagraphicsDotLifeBalanceDotExchangePlusXml,
    ApplicationSubtype::VndDotLogipipeDotCircuitPlusZip,
    ApplicationSubtype::VndDotLoom,
    ApplicationSubtype::VndDotLotus123,
    ApplicationSubtype::VndDotLotusApproach,
    ApplicationSubtype::VndDotLotusFreelance,
    ApplicationSubtype::VndDotLotusNotes,
    ApplicationSubtype::VndDotLotusOrganizer,
    ApplicationSubtype::VndDotLotusScreencam,
    ApplicationSubtype::VndDotLotusWordpro,
    ApplicationSubtype::VndDotMacportsDotPortpkg,
    ApplicationSubtype::VndDotMajikahDotBundle,
    ApplicationSubtype::VndDotMaml,
    ApplicationSubtype::VndDotMapboxVectorTile,
    ApplicationSubtype::VndDotMarlinDotDrmDotActiontokenPlusXml,
    ApplicationSubtype::VndDotMarlinDotDrmDotConftokenPlusXml,
    ApplicationSubtype::VndDotMarlinDotDrmDotLicensePlusXml,
    ApplicationSubtype::VndDotMarlinDotDrmDotMdcf,
    ApplicationSubtype::VndDotMasonPlusJson,
    ApplicationSubtype::VndDotMaxarDotArchiveDot3TzPlusZip,
    ApplicationSubtype::VndDotMaxmindDotMaxmindDb,
    ApplicationSubtype::VndDotMcd,
    ApplicationSubtype::VndDotMdl,
    ApplicationSubtype::VndDotMdlMbsdf,
    ApplicationSubtype::VndDotMedcalcdata,
    ApplicationSubtype::VndDotMediastationDotCdkey,
    ApplicationSubtype::VndDotMedicalholodeckDotRecordxr,
    ApplicationSubtype::VndDotMeridianSlingshot,
    ApplicationSubtype::VndDotMermaid,
    ApplicationSubtype::VndDotMfer,
    ApplicationSubtype::VndDotMfmp,
    ApplicationSubtype::VndDotMicroPlusJson,
    ApplicationSubtype::VndDotMicrografxDotFlo,
    ApplicationSubtype::VndDotMicrografxDotIgx,
    ApplicationSubtype::VndDotMicrosoftDotPortableExecutable,
    ApplicationSubtype::VndDotMicrosoftDotWindowsDotThumbnailCache,
    ApplicationSubtype::VndDotMielePlusJson,
    ApplicationSubtype::VndDotMif,
    ApplicationSubtype::VndDotMinisoftHp3000Save,
    ApplicationSubtype::VndDotMitsubishiDotMistyGuardDotTrustweb,
    ApplicationSubtype::VndDotMobiusDotDaf,
    ApplicationSubtype::VndDotMobiusDotDis,
    ApplicationSubtype::VndDotMobiusDotMbk,
    ApplicationSubtype::VndDotMobiusDotMqy,
    ApplicationSubtype::VndDotMobiusDotMsl,
    ApplicationSubtype::VndDotMobiusDotPlc,
    ApplicationSubtype::VndDotMobiusDotTxf,
    ApplicationSubtype::VndDotModl,
    ApplicationSubtype::VndDotMophunDotApplication,
    ApplicationSubtype::VndDotMophunDotCertificate,
    ApplicationSubtype::VndDotMotorolaDotFlexsuite,
    ApplicationSubtype::VndDotMotorolaDotFlexsuiteDotAdsi,
    ApplicationSubtype::VndDotMotorolaDotFlexsuiteDotFis,
    ApplicationSubtype::VndDotMotorolaDotFlexsuiteDotGotap,
    ApplicationSubtype::VndDotMotorolaDotFlexsuiteDotKmr,
    ApplicationSubtype::VndDotMotorolaDotFlexsuiteDotTtc,
    ApplicationSubtype::VndDotMotorolaDotFlexsuiteDotWem,
    ApplicationSubtype::VndDotMotorolaDotIprm,
    ApplicationSubtype::VndDotMozillaDotXulPlusXml,
    ApplicationSubtype::VndDotMs3Mfdocument,
    ApplicationSubtype::VndDotMsArtgalry,
    ApplicationSubtype::VndDotMsAsf,
    ApplicationSubtype::VndDotMsCabCompressed,
    ApplicationSubtype::VndDotMsExcel,
    ApplicationSubtype::VndDotMsExcelDotAddinDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsExcelDotSheetDotBinaryDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsExcelDotSheetDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsExcelDotTemplateDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsFontobject,
    ApplicationSubtype::VndDotMsHtmlhelp,
    ApplicationSubtype::VndDotMsIms,
    ApplicationSubtype::VndDotMsLrm,
    ApplicationSubtype::VndDotMsOfficeDotActiveXPlusXml,
    ApplicationSubtype::VndDotMsOfficetheme,
    ApplicationSubtype::VndDotMsPlayreadyDotInitiatorPlusXml,
    ApplicationSubtype::VndDotMsPowerpoint,
    ApplicationSubtype::VndDotMsPowerpointDotAddinDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsPowerpointDotPresentationDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsPowerpointDotSlideDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsPowerpointDotSlideshowDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsPowerpointDotTemplateDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsPrintDeviceCapabilitiesPlusXml,
    ApplicationSubtype::VndDotMsPrintSchemaTicketPlusXml,
    ApplicationSubtype::VndDotMsProject,
    ApplicationSubtype::VndDotMsTnef,
    ApplicationSubtype::VndDotMsWindowsDotDevicepairing,
    ApplicationSubtype::VndDotMsWindowsDotNwprintingDotOob,
    ApplicationSubtype::VndDotMsWindowsDotPrinterpairing,
    ApplicationSubtype::VndDotMsWindowsDotWsdDotOob,
    ApplicationSubtype::VndDotMsWmdrmDotLicChlgReq,
    ApplicationSubtype::VndDotMsWmdrmDotLicResp,
    ApplicationSubtype::VndDotMsWmdrmDotMeterChlgReq,
    ApplicationSubtype::VndDotMsWmdrmDotMeterResp,
    ApplicationSubtype::VndDotMsWordDotDocumentDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsWordDotTemplateDotMacroEnabledDot12,
    ApplicationSubtype::VndDotMsWorks,
    ApplicationSubtype::VndDotMsWpl,
    ApplicationSubtype::VndDotMsXpsdocument,
    ApplicationSubtype::VndDotMsaDiskImage,
    ApplicationSubtype::VndDotMseq,
    ApplicationSubtype::VndDotMsgpack,
    ApplicationSubtype::VndDotMsign,
    ApplicationSubtype::VndDotMultiadDotCreator,
    ApplicationSubtype::VndDotMultiadDotCreatorDotCif,
    ApplicationSubtype::VndDotMusicNiff,
    ApplicationSubtype::VndDotMusician,
    ApplicationSubtype::VndDotMuveeDotStyle,
    ApplicationSubtype::VndDotMynfc,
    ApplicationSubtype::VndDotNacamarDotYbridPlusJson,
    ApplicationSubtype::VndDotNatoDotBindingdataobjectPlusCbor,
    ApplicationSubtype::VndDotNatoDotBindingdataobjectPlusJson,
    ApplicationSubtype::VndDotNatoDotBindingdataobjectPlusXml,
    ApplicationSubtype::VndDotNatoDotOpenxmlformatsPackageDotIepdPlusZip,
    ApplicationSubtype::VndDotNcdDotControl,
    ApplicationSubtype::VndDotNcdDotReference,
    ApplicationSubtype::VndDotNearstDotInvPlusJson,
    ApplicationSubtype::VndDotNebumindDotLine,
    ApplicationSubtype::VndDotNervana,
    ApplicationSubtype::VndDotNetfpx,
    ApplicationSubtype::VndDotNeurolanguageDotNlu,
    ApplicationSubtype::VndDotNimn,
    ApplicationSubtype::VndDotNintendoDotNitroDotRom,
    ApplicationSubtype::VndDotNintendoDotSnesDotRom,
    ApplicationSubtype::VndDotNitf,
    ApplicationSubtype::VndDotNoblenetDirectory,
    ApplicationSubtype::VndDotNoblenetSealer,
    ApplicationSubtype::VndDotNoblenetWeb,
    ApplicationSubtype::VndDotNokiaDotCatalogs,
    ApplicationSubtype::VndDotNokiaDotConmlPlusWbxml,
    ApplicationSubtype::VndDotNokiaDotConmlPlusXml,
    ApplicationSubtype::VndDotNokiaDotIptvDotConfigPlusXml,
    ApplicationSubtype::VndDotNokiaDotISdsRadioPresets,
    ApplicationSubtype::VndDotNokiaDotLandmarkPlusWbxml,
    ApplicationSubtype::VndDotNokiaDotLandmarkPlusXml,
    ApplicationSubtype::VndDotNokiaDotLandmarkcollectionPlusXml,
    ApplicationSubtype::VndDotNokiaDotNGageDotAcPlusXml,
    ApplicationSubtype::VndDotNokiaDotNGageDotData,
    ApplicationSubtype::VndDotNokiaDotNGageDotSymbianDotInstall,
    ApplicationSubtype::VndDotNokiaDotNcd,
    ApplicationSubtype::VndDotNokiaDotPcdPlusWbxml,
    ApplicationSubtype::VndDotNokiaDotPcdPlusXml,
    ApplicationSubtype::VndDotNokiaDotRadioPreset,
    ApplicationSubtype::VndDotNokiaDotRadioPresets,
    ApplicationSubtype::VndDotNovadigmDotEdm,
    ApplicationSubtype::VndDotNovadigmDotEdx,
    ApplicationSubtype::VndDotNovadigmDotExt,
    ApplicationSubtype::VndDotNttLocalDotContentShare,
    ApplicationSubtype::VndDotNttLocalDotFileTransfer,
    ApplicationSubtype::VndDotNttLocalDotOgwRemoteAccess,
    ApplicationSubtype::VndDotNttLocalDotSipTaRemote,
    ApplicationSubtype::VndDotNttLocalDotSipTaTcpStream,
    ApplicationSubtype::VndDotNubaltecDotNudokuGame,
    ApplicationSubtype::VndDotOaiDotWorkflows,
    ApplicationSubtype::VndDotOaiDotWorkflowsPlusJson,
    ApplicationSubtype::VndDotOaiDotWorkflowsPlusYaml,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotBase,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotChart,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotChartTemplate,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotDatabase,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotFormula,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotFormulaTemplate,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotGraphics,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotGraphicsTemplate,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotImage,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotImageTemplate,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotPresentation,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotPresentationTemplate,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotSpreadsheet,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotSpreadsheetTemplate,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotText,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotTextMaster,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotTextMasterTemplate,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotTextTemplate,
    ApplicationSubtype::VndDotOasisDotOpendocumentDotTextWeb,
    ApplicationSubtype::VndDotObn,
    ApplicationSubtype::VndDotOcfPlusCbor,
    ApplicationSubtype::VndDotOciDotImageDotManifestDotV1PlusJson,
    ApplicationSubtype::VndDotOftnDotL10NPlusJson,
    ApplicationSubtype::VndDotOipfDotContentaccessdownloadPlusXml,
    ApplicationSubtype::VndDotOipfDotContentaccessstreamingPlusXml,
    ApplicationSubtype::VndDotOipfDotCspgHexbinary,
    ApplicationSubtype::VndDotOipfDotDaeDotSvgPlusXml,
    ApplicationSubtype::VndDotOipfDotDaeDotXhtmlPlusXml,
    ApplicationSubtype::VndDotOipfDotMippvcontrolmessagePlusXml,
    ApplicationSubtype::VndDotOipfDotPaeDotGem,
    ApplicationSubtype::VndDotOipfDotSpdiscoveryPlusXml,
    ApplicationSubtype::VndDotOipfDotSpdlistPlusXml,
    ApplicationSubtype::VndDotOipfDotUeprofilePlusXml,
    ApplicationSubtype::VndDotOipfDotUserprofilePlusXml,
    ApplicationSubtype::VndDotOlpcSugar,
    ApplicationSubtype::VndDotOmaScwsConfig,
    ApplicationSubtype::VndDotOmaScwsHttpRequest,
    ApplicationSubtype::VndDotOmaScwsHttpResponse,
    ApplicationSubtype::VndDotOmaDotBcastDotAssociatedProcedureParameterPlusXml,
    ApplicationSubtype::VndDotOmaDotBcastDotDrmTriggerPlusXml,
    ApplicationSubtype::VndDotOmaDotBcastDotImdPlusXml,
    ApplicationSubtype::VndDotOmaDotBcastDotLtkm,
    ApplicationSubtype::VndDotOmaDotBcastDotNotificationPlusXml,
    ApplicationSubtype::VndDotOmaDotBcastDotProvisioningtrigger,
    ApplicationSubtype::VndDotOmaDotBcastDotSgboot,
    ApplicationSubtype::VndDotOmaDotBcastDotSgddPlusXml,
    ApplicationSubtype::VndDotOmaDotBcastDotSgdu,
    ApplicationSubtype::VndDotOmaDotBcastDotSimpleSymbolContainer,
    ApplicationSubtype::VndDotOmaDotBcastDotSmartcardTriggerPlusXml,
    ApplicationSubtype::VndDotOmaDotBcastDotSprovPlusXml,
    ApplicationSubtype::VndDotOmaDotBcastDotStkm,
    ApplicationSubtype::VndDotOmaDotCabAddressBookPlusXml,
    ApplicationSubtype::VndDotOmaDotCabFeatureHandlerPlusXml,
    ApplicationSubtype::VndDotOmaDotCabPccPlusXml,
    ApplicationSubtype::VndDotOmaDotCabSubsInvitePlusXml,
    ApplicationSubtype::VndDotOmaDotCabUserPrefsPlusXml,
    ApplicationSubtype::VndDotOmaDotDcd,
    ApplicationSubtype::VndDotOmaDotDcdc,
    ApplicationSubtype::VndDotOmaDotDd2PlusXml,
    ApplicationSubtype::VndDotOmaDotDrmDotRisdPlusXml,
    ApplicationSubtype::VndDotOmaDotGroupUsageListPlusXml,
    ApplicationSubtype::VndDotOmaDotLwm2MPlusCbor,
    ApplicationSubtype::VndDotOmaDotLwm2MPlusJson,
    ApplicationSubtype::VndDotOmaDotLwm2MPlusTlv,
    ApplicationSubtype::VndDotOmaDotPalPlusXml,
    ApplicationSubtype::VndDotOmaDotPocDotDetailedProgressReportPlusXml,
    ApplicationSubtype::VndDotOmaDotPocDotFinalReportPlusXml,
    ApplicationSubtype::VndDotOmaDotPocDotGroupsPlusXml,
    ApplicationSubtype::VndDotOmaDotPocDotInvocationDescriptorPlusXml,
    ApplicationSubtype::VndDotOmaDotPocDotOptimizedProgressReportPlusXml,
    ApplicationSubtype::VndDotOmaDotPush,
    ApplicationSubtype::VndDotOmaDotScidmDotMessagesPlusXml,
    ApplicationSubtype::VndDotOmaDotXcapDirectoryPlusXml,
    ApplicationSubtype::VndDotOmadsEmailPlusXml,
    ApplicationSubtype::VndDotOmadsFilePlusXml,
    ApplicationSubtype::VndDotOmadsFolderPlusXml,
    ApplicationSubtype::VndDotOmalocSuplInit,
    ApplicationSubtype::VndDotOmsDotCellularCoseContentPlusCbor,
    ApplicationSubtype::VndDotOnepager,
    ApplicationSubtype::VndDotOnepagertamp,
    ApplicationSubtype::VndDotOnepagertamx,
    ApplicationSubtype::VndDotOnepagertat,
    ApplicationSubtype::VndDotOnepagertatp,
    ApplicationSubtype::VndDotOnepagertatx,
    ApplicationSubtype::VndDotOnvifDotMetadata,
    ApplicationSubtype::VndDotOpenbloxDotGamePlusXml,
    ApplicationSubtype::VndDotOpenbloxDotGameBinary,
    ApplicationSubtype::VndDotOpeneyeDotOeb,
    ApplicationSubtype::VndDotOpenprinttag,
    ApplicationSubtype::VndDotOpenstreetmapDotDataPlusXml,
    ApplicationSubtype::VndDotOpentimestampsDotOts,
    ApplicationSubtype::VndDotOpenvpiDotDspxPlusJson,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotCustomPropertiesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotCustomXmlPropertiesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotDrawingPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotChartPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotChartshapesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotDiagramColorsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotDiagramDataPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotDiagramLayoutPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotDrawingmlDotDiagramStylePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotExtendedPropertiesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotCommentAuthorsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotCommentsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotHandoutMasterPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotNotesMasterPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotNotesSlidePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotPresentation,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotPresentationDotMainPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotPresPropsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlide,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlidePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideLayoutPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideMasterPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideshow,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideshowDotMainPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotSlideUpdateInfoPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotTableStylesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotTagsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotTemplate,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotTemplateDotMainPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotPresentationmlDotViewPropsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotCalcChainPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotChartsheetPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotCommentsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotConnectionsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotDialogsheetPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotExternalLinkPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotPivotCacheDefinitionPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotPivotCacheRecordsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotPivotTablePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotQueryTablePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotRevisionHeadersPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotRevisionLogPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotSharedStringsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotSheet,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotSheetDotMainPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotSheetMetadataPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotStylesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotTablePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotTableSingleCellsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotTemplate,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotTemplateDotMainPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotUserNamesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotVolatileDependenciesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotSpreadsheetmlDotWorksheetPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotThemePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotThemeOverridePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotVmlDrawing,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotCommentsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotDocument,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotDocumentDotGlossaryPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotDocumentDotMainPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotEndnotesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotFontTablePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotFooterPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotFootnotesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotNumberingPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotSettingsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotStylesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotTemplate,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotTemplateDotMainPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsOfficedocumentDotWordprocessingmlDotWebSettingsPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsPackageDotCorePropertiesPlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsPackageDotDigitalSignatureXmlsignaturePlusXml,
    ApplicationSubtype::VndDotOpenxmlformatsPackageDotRelationshipsPlusXml,
    ApplicationSubtype::VndDotOracleDotResourcePlusJson,
    ApplicationSubtype::VndDotOrangeDotIndata,
    ApplicationSubtype::VndDotOsaDotNetdeploy,
    ApplicationSubtype::VndDotOsgeoDotMapguideDotPackage,
    ApplicationSubtype::VndDotOsgiDotBundle,
    ApplicationSubtype::VndDotOsgiDotDp,
    ApplicationSubtype::VndDotOsgiDotSubsystem,
    ApplicationSubtype::VndDotOtpsDotCtKipPlusXml,
    ApplicationSubtype::VndDotOxliDotCountgraph,
    ApplicationSubtype::VndDotPagerdutyPlusJson,
    ApplicationSubtype::VndDotPalm,
    ApplicationSubtype::VndDotPanoply,
    ApplicationSubtype::VndDotPaosDotXml,
    ApplicationSubtype::VndDotPatentdive,
    ApplicationSubtype::VndDotPatientecommsdoc,
    ApplicationSubtype::VndDotPawaafile,
    ApplicationSubtype::VndDotPcos,
    ApplicationSubtype::VndDotPgDotFormat,
    ApplicationSubtype::VndDotPgDotOsasli,
    ApplicationSubtype::VndDotPhbkPlusXml,
    ApplicationSubtype::VndDotPiaccessDotApplicationLicence,
    ApplicationSubtype::VndDotPicsel,
    ApplicationSubtype::VndDotPmiDotWidget,
    ApplicationSubtype::VndDotPmtiles,
    ApplicationSubtype::VndDotPocDotGroupAdvertisementPlusXml,
    ApplicationSubtype::VndDotPocketlearn,
    ApplicationSubtype::VndDotPowerbuilder6,
    ApplicationSubtype::VndDotPowerbuilder6S,
    ApplicationSubtype::VndDotPowerbuilder7,
    ApplicationSubtype::VndDotPowerbuilder7S,
    ApplicationSubtype::VndDotPowerbuilder75,
    ApplicationSubtype::VndDotPowerbuilder75S,
    ApplicationSubtype::VndDotPpDotSystemverifyPlusXml,
    ApplicationSubtype::VndDotPreminet,
    ApplicationSubtype::VndDotPreviewsystemsDotBox,
    ApplicationSubtype::VndDotProjectGraph,
    ApplicationSubtype::VndDotProteusDotMagazine,
    ApplicationSubtype::VndDotPsfs,
    ApplicationSubtype::VndDotPtDotMundusmundi,
    ApplicationSubtype::VndDotPublishareDeltaTree,
    ApplicationSubtype::VndDotPviDotPtid1,
    ApplicationSubtype::VndDotPwgMultiplexed,
    ApplicationSubtype::VndDotPwgXhtmlPrintPlusXml,
    ApplicationSubtype::VndDotPyonPlusJson,
    ApplicationSubtype::VndDotQualcommDotBrewAppRes,
    ApplicationSubtype::VndDotQuarantainenet,
    ApplicationSubtype::VndDotQuarkDotQuarkXPress,
    ApplicationSubtype::VndDotQuobjectQuoxdocument,
    ApplicationSubtype::VndDotR74NDotSandboxelsPlusJson,
    ApplicationSubtype::VndDotRadisysDotMomlPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlAuditPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlAuditConfPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlAuditConnPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlAuditDialogPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlAuditStreamPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlConfPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlDialogPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlDialogBasePlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlDialogFaxDetectPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlDialogFaxSendrecvPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlDialogGroupPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlDialogSpeechPlusXml,
    ApplicationSubtype::VndDotRadisysDotMsmlDialogTransformPlusXml,
    ApplicationSubtype::VndDotRainstorDotData,
    ApplicationSubtype::VndDotRapid,
    ApplicationSubtype::VndDotRar,
    ApplicationSubtype::VndDotRealvncDotBed,
    ApplicationSubtype::VndDotRecordareDotMusicxml,
    ApplicationSubtype::VndDotRecordareDotMusicxmlPlusXml,
    ApplicationSubtype::VndDotRego,
    ApplicationSubtype::VndDotRelpipe,
    ApplicationSubtype::VndDotRenLearnDotRlprint,
    ApplicationSubtype::VndDotResilientDotLogic,
    ApplicationSubtype::VndDotRestfulPlusJson,
    ApplicationSubtype::VndDotRigDotCryptonote,
    ApplicationSubtype::VndDotRoute66DotLink66PlusXml,
    ApplicationSubtype::VndDotRs274X,
    ApplicationSubtype::VndDotRuckusDotDownload,
    ApplicationSubtype::VndDotS3Sms,
    ApplicationSubtype::VndDotSailingtrackerDotTrack,
    ApplicationSubtype::VndDotSar,
    ApplicationSubtype::VndDotSbmDotCid,
    ApplicationSubtype::VndDotSbmDotMid2,
    ApplicationSubtype::VndDotScribus,
    ApplicationSubtype::VndDotSealedDot3Df,
    ApplicationSubtype::VndDotSealedDotCsf,
    ApplicationSubtype::VndDotSealedDotDoc,
    ApplicationSubtype::VndDotSealedDotEml,
    ApplicationSubtype::VndDotSealedDotMht,
    ApplicationSubtype::VndDotSealedDotNet,
    ApplicationSubtype::VndDotSealedDotPpt,
    ApplicationSubtype::VndDotSealedDotTiff,
    ApplicationSubtype::VndDotSealedDotXls,
    ApplicationSubtype::VndDotSealedmediaDotSoftsealDotHtml,
    ApplicationSubtype::VndDotSealedmediaDotSoftsealDotPdf,
    ApplicationSubtype::VndDotSeemail,
    ApplicationSubtype::VndDotSeisPlusJson,
    ApplicationSubtype::VndDotSema,
    ApplicationSubtype::VndDotSemd,
    ApplicationSubtype::VndDotSemf,
    ApplicationSubtype::VndDotShadeSaveFile,
    ApplicationSubtype::VndDotShanaDotInformedDotFormdata,
    ApplicationSubtype::VndDotShanaDotInformedDotFormtemplate,
    ApplicationSubtype::VndDotShanaDotInformedDotInterchange,
    ApplicationSubtype::VndDotShanaDotInformedDotPackage,
    ApplicationSubtype::VndDotShootproofPlusJson,
    ApplicationSubtype::VndDotShopkickPlusJson,
    ApplicationSubtype::VndDotShp,
    ApplicationSubtype::VndDotShx,
    ApplicationSubtype::VndDotSigrokDotSession,
    ApplicationSubtype::VndDotSimTechMindMapper,
    ApplicationSubtype::VndDotSirenPlusJson,
    ApplicationSubtype::VndDotSirtxDotVmv0,
    ApplicationSubtype::VndDotSketchometry,
    ApplicationSubtype::VndDotSmaf,
    ApplicationSubtype::VndDotSmartDotNotebook,
    ApplicationSubtype::VndDotSmartDotTeacher,
    ApplicationSubtype::VndDotSmintioDotPortalsDotArchive,
    ApplicationSubtype::VndDotSnesdevPageTable,
    ApplicationSubtype::VndDotSoftware602DotFillerDotFormPlusXml,
    ApplicationSubtype::VndDotSoftware602DotFillerDotFormXmlZip,
    ApplicationSubtype::VndDotSolentDotSdkmPlusXml,
    ApplicationSubtype::VndDotSpotfireDotDxp,
    ApplicationSubtype::VndDotSpotfireDotSfs,
    ApplicationSubtype::VndDotSqlite3,
    ApplicationSubtype::VndDotSri,
    ApplicationSubtype::VndDotSssCod,
    ApplicationSubtype::VndDotSssDtf,
    ApplicationSubtype::VndDotSssNtf,
    ApplicationSubtype::VndDotStepmaniaDotPackage,
    ApplicationSubtype::VndDotStepmaniaDotStepchart,
    ApplicationSubtype::VndDotStreetStream,
    ApplicationSubtype::VndDotSunDotWadlPlusXml,
    ApplicationSubtype::VndDotSuperfileDotSuper,
    ApplicationSubtype::VndDotSusCalendar,
    ApplicationSubtype::VndDotSvd,
    ApplicationSubtype::VndDotSwiftviewIcs,
    ApplicationSubtype::VndDotSybylDotMol2,
    ApplicationSubtype::VndDotSyclePlusXml,
    ApplicationSubtype::VndDotSyftPlusJson,
    ApplicationSubtype::VndDotSyncmlPlusXml,
    ApplicationSubtype::VndDotSyncmlDotDmPlusWbxml,
    ApplicationSubtype::VndDotSyncmlDotDmPlusXml,
    ApplicationSubtype::VndDotSyncmlDotDmDotNotification,
    ApplicationSubtype::VndDotSyncmlDotDmddfPlusWbxml,
    ApplicationSubtype::VndDotSyncmlDotDmddfPlusXml,
    ApplicationSubtype::VndDotSyncmlDotDmtndsPlusWbxml,
    ApplicationSubtype::VndDotSyncmlDotDmtndsPlusXml,
    ApplicationSubtype::VndDotSyncmlDotDsDotNotification,
    ApplicationSubtype::VndDotTableschemaPlusJson,
    ApplicationSubtype::VndDotTaoDotIntentModuleArchive,
    ApplicationSubtype::VndDotTcpdumpDotPcap,
    ApplicationSubtype::VndDotThinkCellDotPpttcPlusJson,
    ApplicationSubtype::VndDotTmdDotMediaflexDotApiPlusXml,
    ApplicationSubtype::VndDotTml,
    ApplicationSubtype::VndDotTmobileLivetv,
    ApplicationSubtype::VndDotTriDotOnesource,
    ApplicationSubtype::VndDotTridDotTpt,
    ApplicationSubtype::VndDotTriscapeDotMxs,
    ApplicationSubtype::VndDotTrueapp,
    ApplicationSubtype::VndDotTruedoc,
    ApplicationSubtype::VndDotUbisoftDotWebplayer,
    ApplicationSubtype::VndDotUfdl,
    ApplicationSubtype::VndDotUicDotDosipasDotV1,
    ApplicationSubtype::VndDotUicDotDosipasDotV2,
    ApplicationSubtype::VndDotUicDotOsdmPlusJson,
    ApplicationSubtype::VndDotUicDotTlbFcb,
    ApplicationSubtype::VndDotUiqDotTheme,
    ApplicationSubtype::VndDotUmajin,
    ApplicationSubtype::VndDotUnity,
    ApplicationSubtype::VndDotUomlPlusXml,
    ApplicationSubtype::VndDotUplanetDotAlert,
    ApplicationSubtype::VndDotUplanetDotAlertWbxml,
    ApplicationSubtype::VndDotUplanetDotBearerChoice,
    ApplicationSubtype::VndDotUplanetDotBearerChoiceWbxml,
    ApplicationSubtype::VndDotUplanetDotCacheop,
    ApplicationSubtype::VndDotUplanetDotCacheopWbxml,
    ApplicationSubtype::VndDotUplanetDotChannel,
    ApplicationSubtype::VndDotUplanetDotChannelWbxml,
    ApplicationSubtype::VndDotUplanetDotList,
    ApplicationSubtype::VndDotUplanetDotListWbxml,
    ApplicationSubtype::VndDotUplanetDotListcmd,
    ApplicationSubtype::VndDotUplanetDotListcmdWbxml,
    ApplicationSubtype::VndDotUplanetDotSignal,
    ApplicationSubtype::VndDotUriMap,
    ApplicationSubtype::VndDotValveDotSourceDotMaterial,
    ApplicationSubtype::VndDotVcx,
    ApplicationSubtype::VndDotVdStudy,
    ApplicationSubtype::VndDotVectorworks,
    ApplicationSubtype::VndDotVelPlusJson,
    ApplicationSubtype::VndDotVeraisonDotTsmReportPlusCbor,
    ApplicationSubtype::VndDotVeraisonDotTsmReportPlusJson,
    ApplicationSubtype::VndDotVerifierAttestationPlusJwt,
    ApplicationSubtype::VndDotVerimatrixDotVcas,
    ApplicationSubtype::VndDotVeritoneDotAionPlusJson,
    ApplicationSubtype::VndDotVertifileDotPvf,
    ApplicationSubtype::VndDotVeryantDotThin,
    ApplicationSubtype::VndDotVesDotEncrypted,
    ApplicationSubtype::VndDotVidsoftDotVidconference,
    ApplicationSubtype::VndDotVisio,
    ApplicationSubtype::VndDotVisionary,
    ApplicationSubtype::VndDotVividenceDotScriptfile,
    ApplicationSubtype::VndDotVocalshaperDotVsp4,
    ApplicationSubtype::VndDotVsf,
    ApplicationSubtype::VndDotVuq,
    ApplicationSubtype::VndDotWantverse,
    ApplicationSubtype::VndDotWapDotSic,
    ApplicationSubtype::VndDotWapDotSlc,
    ApplicationSubtype::VndDotWapDotWbxml,
    ApplicationSubtype::VndDotWapDotWmlc,
    ApplicationSubtype::VndDotWapDotWmlscriptc,
    ApplicationSubtype::VndDotWasmflowDotWafl,
    ApplicationSubtype::VndDotWebturbo,
    ApplicationSubtype::VndDotWfaDotDpp,
    ApplicationSubtype::VndDotWfaDotP2P,
    ApplicationSubtype::VndDotWfaDotWsc,
    ApplicationSubtype::VndDotWindowsDotDevicepairing,
    ApplicationSubtype::VndDotWmap,
    ApplicationSubtype::VndDotWmc,
    ApplicationSubtype::VndDotWmfDotBootstrap,
    ApplicationSubtype::VndDotWolframDotMathematica,
    ApplicationSubtype::VndDotWolframDotMathematicaDotPackage,
    ApplicationSubtype::VndDotWolframDotPlayer,
    ApplicationSubtype::VndDotWordlift,
    ApplicationSubtype::VndDotWordperfect,
    ApplicationSubtype::VndDotWqd,
    ApplicationSubtype::VndDotWrqHp3000Labelled,
    ApplicationSubtype::VndDotWtDotStf,
    ApplicationSubtype::VndDotWvDotCspPlusWbxml,
    ApplicationSubtype::VndDotWvDotCspPlusXml,
    ApplicationSubtype::VndDotWvDotSspPlusXml,
    ApplicationSubtype::VndDotXacmlPlusJson,
    ApplicationSubtype::VndDotXara,
    ApplicationSubtype::VndDotXarinDotCpj,
    ApplicationSubtype::VndDotXcdn,
    ApplicationSubtype::VndDotXecretsEncrypted,
    ApplicationSubtype::VndDotXfdl,
    ApplicationSubtype::VndDotXfdlDotWebform,
    ApplicationSubtype::VndDotXmiPlusXml,
    ApplicationSubtype::VndDotXmpieDotCpkg,
    ApplicationSubtype::VndDotXmpieDotDpkg,
    ApplicationSubtype::VndDotXmpieDotPlan,
    ApplicationSubtype::VndDotXmpieDotPpkg,
    ApplicationSubtype::VndDotXmpieDotXlim,
    ApplicationSubtype::VndDotYamahaDotHvDic,
    ApplicationSubtype::VndDotYamahaDotHvScript,
    ApplicationSubtype::VndDotYamahaDotHvVoice,
    ApplicationSubtype::VndDotYamahaDotOpenscoreformat,
    ApplicationSubtype::VndDotYamahaDotOpenscoreformatDotOsfpvgPlusXml,
    ApplicationSubtype::VndDotYamahaDotRemoteSetup,
    ApplicationSubtype::VndDotYamahaDotSmafAudio,
    ApplicationSubtype::VndDotYamahaDotSmafPhrase,
    ApplicationSubtype::VndDotYamahaDotThroughNgn,
    ApplicationSubtype::VndDotYamahaDotTunnelUdpencap,
    ApplicationSubtype::VndDotYaoweme,
    ApplicationSubtype::VndDotYellowriverCustomMenu,
    ApplicationSubtype::VndDotYoutubeDotYt,
    ApplicationSubtype::VndDotZohoDocumentDotWriter,
    ApplicationSubtype::VndDotZohoPresentationDotShow,
    ApplicationSubtype::VndDotZohoDotSpreadsheetmlDotSheet,
    ApplicationSubtype::VndDotZul,
    ApplicationSubtype::VndDotZzazzDotDeckPlusXml,
    ApplicationSubtype::VoicexmlPlusXml,
    ApplicationSubtype::VoucherCmsPlusJson,
    ApplicationSubtype::VoucherJwsPlusJson,
    ApplicationSubtype::Vp,
    ApplicationSubtype::VpPlusCose,
    ApplicationSubtype::VpPlusJwt,
    ApplicationSubtype::VpPlusSdJwt,
    ApplicationSubtype::VqRtcpxr,
    ApplicationSubtype::Wasm,
    ApplicationSubtype::WatcherinfoPlusXml,
    ApplicationSubtype::WebpushOptionsPlusJson,
    ApplicationSubtype::WhoisppQuery,
    ApplicationSubtype::WhoisppResponse,
    ApplicationSubtype::Widget,
    ApplicationSubtype::Wita,
    ApplicationSubtype::Wordperfect5Dot1,
    ApplicationSubtype::WsdlPlusXml,
    ApplicationSubtype::WspolicyPlusXml,
    ApplicationSubtype::XPkiMessage,
    ApplicationSubtype::XWwwFormUrlencoded,
    ApplicationSubtype::XX509CaCert,
    ApplicationSubtype::XX509CaRaCert,
    ApplicationSubtype::XX509NextCaCert,
    ApplicationSubtype::X400Bp,
    ApplicationSubtype::XacmlPlusXml,
    ApplicationSubtype::XcapAttPlusXml,
    ApplicationSubtype::XcapCapsPlusXml,
    ApplicationSubtype::XcapDiffPlusXml,
    ApplicationSubtype::XcapElPlusXml,
    ApplicationSubtype::XcapErrorPlusXml,
    ApplicationSubtype::XcapNsPlusXml,
    ApplicationSubtype::XconConferenceInfoPlusXml,
    ApplicationSubtype::XconConferenceInfoDiffPlusXml,
    ApplicationSubtype::XencPlusXml,
    ApplicationSubtype::Xfdf,
    ApplicationSubtype::XhtmlPlusXml,
    ApplicationSubtype::XliffPlusXml,
    ApplicationSubtype::Xml,
    ApplicationSubtype::XmlDtd,
    ApplicationSubtype::XmlExternalParsedEntity,
    ApplicationSubtype::XmlPatchPlusXml,
    ApplicationSubtype::XmppPlusXml,
    ApplicationSubtype::XopPlusXml,
    ApplicationSubtype::XsltPlusXml,
    ApplicationSubtype::XvPlusXml,
    ApplicationSubtype::Yaml,
    ApplicationSubtype::Yang,
    ApplicationSubtype::YangDataPlusCbor,
    ApplicationSubtype::YangDataPlusJson,
    ApplicationSubtype::YangDataPlusXml,
    ApplicationSubtype::YangPatchPlusJson,
    ApplicationSubtype::YangPatchPlusXml,
    ApplicationSubtype::YangSidPlusJson,
    ApplicationSubtype::YinPlusXml,
    ApplicationSubtype::Zip,
    ApplicationSubtype::Zlib,
    ApplicationSubtype::Zstd,
];
const APPLICATION_SUBTYPE_SLIST: &[&str] = &[
    "1d-interleaved-parityfec",
    "3gpdash-qoe-report+xml",
    "3gpp-ims+xml",
    "3gpp-mbs-object-manifest+json",
    "3gpp-mbs-user-service-descriptions+json",
    "3gpp-media-delivery-metrics-report+json",
    "3gppHal+json",
    "3gppHalForms+json",
    "A2L",
    "aas+zip",
    "ace+cbor",
    "ace+json",
    "ace-groupcomm+cbor",
    "ace-trl+cbor",
    "activemessage",
    "activity+json",
    "aif+cbor",
    "aif+json",
    "alto-cdni+json",
    "alto-cdnifilter+json",
    "alto-costmap+json",
    "alto-costmapfilter+json",
    "alto-directory+json",
    "alto-endpointcost+json",
    "alto-endpointcostparams+json",
    "alto-endpointprop+json",
    "alto-endpointpropparams+json",
    "alto-error+json",
    "alto-networkmap+json",
    "alto-networkmapfilter+json",
    "alto-propmap+json",
    "alto-propmapparams+json",
    "alto-tips+json",
    "alto-tipsparams+json",
    "alto-updatestreamcontrol+json",
    "alto-updatestreamparams+json",
    "AML",
    "andrew-inset",
    "applefile",
    "asyncapi+json",
    "asyncapi+yaml",
    "at+jwt",
    "ATF",
    "ATFX",
    "atom+xml",
    "atomcat+xml",
    "atomdeleted+xml",
    "atomicmail",
    "atomsvc+xml",
    "atsc-dwd+xml",
    "atsc-dynamic-event-message",
    "atsc-held+xml",
    "atsc-rdt+json",
    "atsc-rsat+xml",
    "ATXML",
    "auth-policy+xml",
    "automationml-aml+xml",
    "automationml-amlx+zip",
    "bacnet-xdd+zip",
    "batch-SMTP",
    "beep+xml",
    "bufr",
    "c2pa",
    "calendar+json",
    "calendar+xml",
    "call-completion",
    "CALS-1840",
    "captive+json",
    "cbor",
    "cbor-seq",
    "cccex",
    "ccmp+xml",
    "ccxml+xml",
    "cda+xml",
    "CDFX+XML",
    "cdmi-capability",
    "cdmi-container",
    "cdmi-domain",
    "cdmi-object",
    "cdmi-queue",
    "cdni",
    "ce+cbor",
    "CEA",
    "cea-2018+xml",
    "cellml+xml",
    "cfw",
    "cid",
    "cid-edhoc+cbor-seq",
    "city+json",
    "city+json-seq",
    "clr",
    "clue+xml",
    "clue_info+xml",
    "cms",
    "cmw+cbor",
    "cmw+cose",
    "cmw+json",
    "cmw+jws",
    "cnrp+xml",
    "coap-eap",
    "coap-group+json",
    "coap-payload",
    "commonground",
    "concise-problem-details+cbor",
    "conference-info+xml",
    "cose",
    "cose-key",
    "cose-key-set",
    "cose-x509",
    "cpl+xml",
    "csrattrs",
    "csta+xml",
    "CSTAdata+xml",
    "csvm+json",
    "cwl",
    "cwl+json",
    "cwl+yaml",
    "cwt",
    "cybercash",
    "dash+xml",
    "dash-patch+xml",
    "dashdelta",
    "davmount+xml",
    "dca-rft",
    "DCD",
    "dec-dx",
    "dialog-info+xml",
    "dicom",
    "dicom+json",
    "dicom+xml",
    "did",
    "DII",
    "DIT",
    "dns",
    "dns+json",
    "dns-message",
    "dots+cbor",
    "dpop+jwt",
    "dskpp+xml",
    "dssc+der",
    "dssc+xml",
    "dvcs",
    "eat+cwt",
    "eat+jwt",
    "eat-bun+cbor",
    "eat-bun+json",
    "eat-ucs+cbor",
    "eat-ucs+json",
    "ecmascript",
    "edhoc+cbor-seq",
    "EDI-consent",
    "EDI-X12",
    "EDIFACT",
    "efi",
    "elm+json",
    "elm+xml",
    "EmergencyCallData.cap+xml",
    "EmergencyCallData.Comment+xml",
    "EmergencyCallData.Control+xml",
    "EmergencyCallData.DeviceInfo+xml",
    "EmergencyCallData.eCall.MSD",
    "EmergencyCallData.LegacyESN+json",
    "EmergencyCallData.ProviderInfo+xml",
    "EmergencyCallData.ServiceInfo+xml",
    "EmergencyCallData.SubscriberInfo+xml",
    "EmergencyCallData.VEDS+xml",
    "emma+xml",
    "emotionml+xml",
    "encaprtp",
    "entity-statement+jwt",
    "epp+xml",
    "epub+zip",
    "eshop",
    "example",
    "exi",
    "expect-ct-report+json",
    "explicit-registration-response+jwt",
    "express",
    "fastinfoset",
    "fastsoap",
    "fdf",
    "fdt+xml",
    "fhir+json",
    "fhir+xml",
    "fits",
    "flexfec",
    "font-sfnt",
    "font-tdpfr",
    "font-woff",
    "framework-attributes+xml",
    "geo+json",
    "geo+json-seq",
    "geofeed+csv",
    "geopackage+sqlite3",
    "geopose+json",
    "geoxacml+json",
    "geoxacml+xml",
    "gltf-buffer",
    "gml+xml",
    "gnap-binding-jws",
    "gnap-binding-jwsd",
    "gnap-binding-rotation-jws",
    "gnap-binding-rotation-jwsd",
    "grib",
    "gzip",
    "H224",
    "held+xml",
    "hl7v2+xml",
    "http",
    "hyperstudio",
    "ibe-key-request+xml",
    "ibe-pkg-reply+xml",
    "ibe-pp-data",
    "iges",
    "im-iscomposing+xml",
    "index",
    "index.cmd",
    "index.obj",
    "index.response",
    "index.vnd",
    "inkml+xml",
    "IOTP",
    "ipfix",
    "ipp",
    "ISUP",
    "its+xml",
    "java-archive",
    "javascript",
    "jf2feed+json",
    "jose",
    "jose+json",
    "jrd+json",
    "jscalendar+json",
    "jscontact+json",
    "json",
    "json-patch+json",
    "json-patch-query+json",
    "json-seq",
    "jsonpath",
    "jwk+json",
    "jwk-set+json",
    "jwk-set+jwt",
    "jwt",
    "kb+jwt",
    "kbl+xml",
    "kpml-request+xml",
    "kpml-response+xml",
    "ld+json",
    "lgr+xml",
    "link-format",
    "linkset",
    "linkset+json",
    "load-control+xml",
    "logout+jwt",
    "lost+xml",
    "lostsync+xml",
    "lpf+zip",
    "LXF",
    "mac-binhex40",
    "macwriteii",
    "mads+xml",
    "manifest+json",
    "marc",
    "marcxml+xml",
    "mathematica",
    "mathml+xml",
    "mathml-content+xml",
    "mathml-presentation+xml",
    "mbms-associated-procedure-description+xml",
    "mbms-deregister+xml",
    "mbms-envelope+xml",
    "mbms-msk+xml",
    "mbms-msk-response+xml",
    "mbms-protection-description+xml",
    "mbms-reception-report+xml",
    "mbms-register+xml",
    "mbms-register-response+xml",
    "mbms-schedule+xml",
    "mbms-user-service-description+xml",
    "mbox",
    "measured-component+cbor",
    "measured-component+json",
    "media-policy-dataset+xml",
    "media_control+xml",
    "mediaservercontrol+xml",
    "merge-patch+json",
    "metalink4+xml",
    "mets+xml",
    "MF4",
    "mikey",
    "mipc",
    "missing-blocks+cbor-seq",
    "mmt-aei+xml",
    "mmt-usd+xml",
    "mods+xml",
    "moss-keys",
    "moss-signature",
    "mosskey-data",
    "mosskey-request",
    "mp21",
    "mp4",
    "mpeg4-generic",
    "mpeg4-iod",
    "mpeg4-iod-xmt",
    "mrb-consumer+xml",
    "mrb-publish+xml",
    "msc-ivr+xml",
    "msc-mixer+xml",
    "msword",
    "mud+json",
    "multipart-core",
    "mxf",
    "n-quads",
    "n-triples",
    "nasdata",
    "news-checkgroups",
    "news-groupinfo",
    "news-transmission",
    "nlsml+xml",
    "node",
    "nss",
    "oauth-authz-req+jwt",
    "oblivious-dns-message",
    "ocsp-request",
    "ocsp-response",
    "octet-stream",
    "ODA",
    "odm+xml",
    "ODX",
    "oebps-package+xml",
    "ogg",
    "ohttp-keys",
    "opc-nodeset+xml",
    "oscore",
    "oxps",
    "p21",
    "p21+zip",
    "p2p-overlay+xml",
    "parityfec",
    "passport",
    "patch-ops-error+xml",
    "pdf",
    "PDX",
    "pem-certificate-chain",
    "pgp-encrypted",
    "pgp-keys",
    "pgp-signature",
    "pidf+xml",
    "pidf-diff+xml",
    "pkcs10",
    "pkcs12",
    "pkcs7-mime",
    "pkcs7-signature",
    "pkcs8",
    "pkcs8-encrypted",
    "pkix-attr-cert",
    "pkix-cert",
    "pkix-crl",
    "pkix-pkipath",
    "pkixcmp",
    "pls+xml",
    "poc-settings+xml",
    "postscript",
    "ppsp-tracker+json",
    "private-token-issuer-directory",
    "private-token-request",
    "private-token-response",
    "problem+json",
    "problem+xml",
    "protobuf",
    "protobuf+json",
    "provenance+xml",
    "provided-claims+jwt",
    "prs.alvestrand.titrax-sheet",
    "prs.bwtc32key",
    "prs.cww",
    "prs.cyn",
    "prs.hpub+zip",
    "prs.implied-document+xml",
    "prs.implied-executable",
    "prs.implied-object+json",
    "prs.implied-object+json-seq",
    "prs.implied-object+yaml",
    "prs.implied-structure",
    "prs.mayfile",
    "prs.nprend",
    "prs.plucker",
    "prs.rdf-xml-crypt",
    "prs.sclt",
    "prs.vcfbzip2",
    "prs.xsf+xml",
    "pskc+xml",
    "pvd+json",
    "QSIG",
    "raptorfec",
    "rdap+json",
    "rdf+xml",
    "reginfo+xml",
    "relax-ng-compact-syntax",
    "remote-printing",
    "reputon+json",
    "resolve-response+jwt",
    "resource-lists+xml",
    "resource-lists-diff+xml",
    "rfc+xml",
    "riscos",
    "rlmi+xml",
    "rls-services+xml",
    "roughtime-malfeasance+json",
    "roughtime-server+json",
    "route-apd+xml",
    "route-s-tsid+xml",
    "route-usd+xml",
    "rpki-checklist",
    "rpki-ghostbusters",
    "rpki-manifest",
    "rpki-publication",
    "rpki-roa",
    "rpki-signed-tal",
    "rpki-updown",
    "rs-metadata+xml",
    "rtf",
    "rtploopback",
    "rtx",
    "samlassertion+xml",
    "samlmetadata+xml",
    "sarif+json",
    "sarif-external-properties+json",
    "sbe",
    "sbml+xml",
    "scaip+xml",
    "scim+json",
    "scitt-receipt+cose",
    "scitt-statement+cose",
    "scvp-cv-request",
    "scvp-cv-response",
    "scvp-vp-request",
    "scvp-vp-response",
    "sd-jwt",
    "sd-jwt+json",
    "sdf+json",
    "sdp",
    "secevent+jwt",
    "senml+cbor",
    "senml+json",
    "senml+xml",
    "senml-etch+cbor",
    "senml-etch+json",
    "senml-exi",
    "sensml+cbor",
    "sensml+json",
    "sensml+xml",
    "sensml-exi",
    "sep+xml",
    "sep-exi",
    "session-info",
    "set-payment",
    "set-payment-initiation",
    "set-registration",
    "set-registration-initiation",
    "SGML",
    "sgml-open-catalog",
    "shf+xml",
    "sieve",
    "simple-filter+xml",
    "simple-message-summary",
    "simpleSymbolContainer",
    "sipc",
    "slate",
    "smil",
    "smil+xml",
    "smpte336m",
    "soap+fastinfoset",
    "soap+xml",
    "sparql-query",
    "sparql-results+xml",
    "spdx+json",
    "spirits-event+xml",
    "sql",
    "srgs",
    "srgs+xml",
    "sru+xml",
    "sslkeylogfile",
    "ssml+xml",
    "ST2110-41",
    "stix+json",
    "stratum",
    "suit-envelope+cose",
    "suit-report+cose",
    "swid+cbor",
    "swid+xml",
    "syslog-msg",
    "tamp-apex-update",
    "tamp-apex-update-confirm",
    "tamp-community-update",
    "tamp-community-update-confirm",
    "tamp-error",
    "tamp-sequence-adjust",
    "tamp-sequence-adjust-confirm",
    "tamp-status-query",
    "tamp-status-response",
    "tamp-update",
    "tamp-update-confirm",
    "taxii+json",
    "td+json",
    "teep+cbor",
    "tei+xml",
    "TETRA_ISI",
    "texinfo",
    "thraud+xml",
    "timestamp-query",
    "timestamp-reply",
    "timestamped-data",
    "tlsrpt+gzip",
    "tlsrpt+json",
    "tm+json",
    "tnauthlist",
    "toc+cbor",
    "token-introspection+jwt",
    "toml",
    "trickle-ice-sdpfrag",
    "trig",
    "trust-chain+json",
    "trust-mark+jwt",
    "trust-mark-delegation+jwt",
    "trust-mark-status-response+jwt",
    "ttml+xml",
    "tve-trigger",
    "tzif",
    "tzif-leap",
    "uccs+cbor",
    "ujcs+json",
    "ulpfec",
    "urc-grpsheet+xml",
    "urc-ressheet+xml",
    "urc-targetdesc+xml",
    "urc-uisocketdesc+xml",
    "v3c",
    "vc",
    "vc+cose",
    "vc+jwt",
    "vc+sd-jwt",
    "vcard+json",
    "vcard+xml",
    "vec+xml",
    "vec-package+gzip",
    "vec-package+zip",
    "vemmi",
    "vnd.1000minds.decision-model+xml",
    "vnd.1ob",
    "vnd.3gpp-prose+xml",
    "vnd.3gpp-prose-pc3a+xml",
    "vnd.3gpp-prose-pc3ach+xml",
    "vnd.3gpp-prose-pc3ch+xml",
    "vnd.3gpp-prose-pc8+xml",
    "vnd.3gpp-v2x-local-service-information",
    "vnd.3gpp.5gnas",
    "vnd.3gpp.5gsa2x",
    "vnd.3gpp.5gsa2x-local-service-information",
    "vnd.3gpp.5gsv2x",
    "vnd.3gpp.5gsv2x-local-service-information",
    "vnd.3gpp.access-transfer-events+xml",
    "vnd.3gpp.bsf+xml",
    "vnd.3gpp.crs+xml",
    "vnd.3gpp.current-location-discovery+xml",
    "vnd.3gpp.GMOP+xml",
    "vnd.3gpp.gtpc",
    "vnd.3gpp.interworking-data",
    "vnd.3gpp.lpp",
    "vnd.3gpp.mc-signalling-ear",
    "vnd.3gpp.mcdata-affiliation-command+xml",
    "vnd.3gpp.mcdata-info+xml",
    "vnd.3gpp.mcdata-msgstore-ctrl-request+xml",
    "vnd.3gpp.mcdata-payload",
    "vnd.3gpp.mcdata-regroup+xml",
    "vnd.3gpp.mcdata-service-config+xml",
    "vnd.3gpp.mcdata-signalling",
    "vnd.3gpp.mcdata-ue-config+xml",
    "vnd.3gpp.mcdata-user-profile+xml",
    "vnd.3gpp.mcptt-affiliation-command+xml",
    "vnd.3gpp.mcptt-floor-request+xml",
    "vnd.3gpp.mcptt-info+xml",
    "vnd.3gpp.mcptt-location-info+xml",
    "vnd.3gpp.mcptt-mbms-usage-info+xml",
    "vnd.3gpp.mcptt-regroup+xml",
    "vnd.3gpp.mcptt-service-config+xml",
    "vnd.3gpp.mcptt-signed+xml",
    "vnd.3gpp.mcptt-ue-config+xml",
    "vnd.3gpp.mcptt-ue-init-config+xml",
    "vnd.3gpp.mcptt-user-profile+xml",
    "vnd.3gpp.mcs-location-user-config+xml",
    "vnd.3gpp.mcvideo-affiliation-command+xml",
    "vnd.3gpp.mcvideo-affiliation-info+xml",
    "vnd.3gpp.mcvideo-info+xml",
    "vnd.3gpp.mcvideo-location-info+xml",
    "vnd.3gpp.mcvideo-mbms-usage-info+xml",
    "vnd.3gpp.mcvideo-regroup+xml",
    "vnd.3gpp.mcvideo-service-config+xml",
    "vnd.3gpp.mcvideo-transmission-request+xml",
    "vnd.3gpp.mcvideo-ue-config+xml",
    "vnd.3gpp.mcvideo-user-profile+xml",
    "vnd.3gpp.mid-call+xml",
    "vnd.3gpp.ngap",
    "vnd.3gpp.pfcp",
    "vnd.3gpp.pic-bw-large",
    "vnd.3gpp.pic-bw-small",
    "vnd.3gpp.pic-bw-var",
    "vnd.3gpp.pinapp-info+xml",
    "vnd.3gpp.s1ap",
    "vnd.3gpp.seal-app-comm-requirements-info+xml",
    "vnd.3gpp.seal-data-delivery-info+cbor",
    "vnd.3gpp.seal-data-delivery-info+xml",
    "vnd.3gpp.seal-group-doc+xml",
    "vnd.3gpp.seal-info+xml",
    "vnd.3gpp.seal-location-info+cbor",
    "vnd.3gpp.seal-location-info+xml",
    "vnd.3gpp.seal-mbms-usage-info+xml",
    "vnd.3gpp.seal-mbs-usage-info+xml",
    "vnd.3gpp.seal-network-QoS-management-info+xml",
    "vnd.3gpp.seal-network-resource-info+cbor",
    "vnd.3gpp.seal-ue-config-info+xml",
    "vnd.3gpp.seal-unicast-info+xml",
    "vnd.3gpp.seal-user-profile-info+xml",
    "vnd.3gpp.sms",
    "vnd.3gpp.sms+xml",
    "vnd.3gpp.srvcc-ext+xml",
    "vnd.3gpp.SRVCC-info+xml",
    "vnd.3gpp.state-and-event-info+xml",
    "vnd.3gpp.ussd+xml",
    "vnd.3gpp.v2x",
    "vnd.3gpp.vae-info+xml",
    "vnd.3gpp2.bcmcsinfo+xml",
    "vnd.3gpp2.sms",
    "vnd.3gpp2.tcap",
    "vnd.3lightssoftware.imagescal",
    "vnd.3M.Post-it-Notes",
    "vnd.accpac.simply.aso",
    "vnd.accpac.simply.imp",
    "vnd.acm.addressxfer+json",
    "vnd.acm.chatbot+json",
    "vnd.acucobol",
    "vnd.acucorp",
    "vnd.adobe.flash.movie",
    "vnd.adobe.formscentral.fcdt",
    "vnd.adobe.fxp",
    "vnd.adobe.partial-upload",
    "vnd.adobe.xdp+xml",
    "vnd.aether.imp",
    "vnd.afpc.afplinedata",
    "vnd.afpc.afplinedata-pagedef",
    "vnd.afpc.cmoca-cmresource",
    "vnd.afpc.foca-charset",
    "vnd.afpc.foca-codedfont",
    "vnd.afpc.foca-codepage",
    "vnd.afpc.modca",
    "vnd.afpc.modca-cmtable",
    "vnd.afpc.modca-formdef",
    "vnd.afpc.modca-mediummap",
    "vnd.afpc.modca-objectcontainer",
    "vnd.afpc.modca-overlay",
    "vnd.afpc.modca-pagesegment",
    "vnd.age",
    "vnd.ah-barcode",
    "vnd.ahead.space",
    "vnd.aia",
    "vnd.airzip.filesecure.azf",
    "vnd.airzip.filesecure.azs",
    "vnd.amadeus+json",
    "vnd.amazon.mobi8-ebook",
    "vnd.americandynamics.acc",
    "vnd.amiga.ami",
    "vnd.amundsen.maze+xml",
    "vnd.android.ota",
    "vnd.anki",
    "vnd.anser-web-certificate-issue-initiation",
    "vnd.antix.game-component",
    "vnd.apache.arrow.file",
    "vnd.apache.arrow.stream",
    "vnd.apache.parquet",
    "vnd.apache.thrift.binary",
    "vnd.apache.thrift.compact",
    "vnd.apache.thrift.json",
    "vnd.apexlang",
    "vnd.api+json",
    "vnd.aplextor.warrp+json",
    "vnd.apothekende.reservation+json",
    "vnd.apple.installer+xml",
    "vnd.apple.keynote",
    "vnd.apple.mpegurl",
    "vnd.apple.numbers",
    "vnd.apple.pages",
    "vnd.arastra.swi",
    "vnd.aristanetworks.swi",
    "vnd.artisan+json",
    "vnd.artsquare",
    "vnd.as207960.vas.config+jer",
    "vnd.as207960.vas.config+uper",
    "vnd.as207960.vas.tap+jer",
    "vnd.as207960.vas.tap+uper",
    "vnd.astraea-software.iota",
    "vnd.audiograph",
    "vnd.autopackage",
    "vnd.avalon+json",
    "vnd.avistar+xml",
    "vnd.balsamiq.bmml+xml",
    "vnd.balsamiq.bmpr",
    "vnd.banana-accounting",
    "vnd.bbf.usp.error",
    "vnd.bbf.usp.msg",
    "vnd.bbf.usp.msg+json",
    "vnd.bekitzur-stech+json",
    "vnd.belightsoft.lhzd+zip",
    "vnd.belightsoft.lhzl+zip",
    "vnd.bint.med-content",
    "vnd.biopax.rdf+xml",
    "vnd.blink-idb-value-wrapper",
    "vnd.blueice.multipass",
    "vnd.bluetooth.ep.oob",
    "vnd.bluetooth.le.oob",
    "vnd.bmi",
    "vnd.bpf",
    "vnd.bpf3",
    "vnd.businessobjects",
    "vnd.byu.uapi+json",
    "vnd.bzip3",
    "vnd.c3voc.schedule+xml",
    "vnd.cab-jscript",
    "vnd.canon-cpdl",
    "vnd.canon-lips",
    "vnd.capasystems-pg+json",
    "vnd.cel",
    "vnd.cendio.thinlinc.clientconf",
    "vnd.century-systems.tcp_stream",
    "vnd.chemdraw+xml",
    "vnd.chess-pgn",
    "vnd.chipnuts.karaoke-mmd",
    "vnd.ciedi",
    "vnd.cinderella",
    "vnd.cirpack.isdn-ext",
    "vnd.citationstyles.style+xml",
    "vnd.claymore",
    "vnd.cloanto.rp9",
    "vnd.clonk.c4group",
    "vnd.cluetrust.cartomobile-config",
    "vnd.cluetrust.cartomobile-config-pkg",
    "vnd.cmmf-configuration-information+json",
    "vnd.cmmf-efd+xml",
    "vnd.cmmf-encoder-configuration+json",
    "vnd.cncf.helm.chart.content.v1.tar+gzip",
    "vnd.cncf.helm.chart.provenance.v1.prov",
    "vnd.cncf.helm.config.v1+json",
    "vnd.coffeescript",
    "vnd.collabio.xodocuments.document",
    "vnd.collabio.xodocuments.document-template",
    "vnd.collabio.xodocuments.presentation",
    "vnd.collabio.xodocuments.presentation-template",
    "vnd.collabio.xodocuments.spreadsheet",
    "vnd.collabio.xodocuments.spreadsheet-template",
    "vnd.collection+json",
    "vnd.collection.doc+json",
    "vnd.collection.next+json",
    "vnd.comicbook+zip",
    "vnd.comicbook-rar",
    "vnd.commerce-battelle",
    "vnd.commonspace",
    "vnd.contact.cmsg",
    "vnd.coreos.ignition+json",
    "vnd.cosmocaller",
    "vnd.crick.clicker",
    "vnd.crick.clicker.keyboard",
    "vnd.crick.clicker.palette",
    "vnd.crick.clicker.template",
    "vnd.crick.clicker.wordbank",
    "vnd.criticaltools.wbs+xml",
    "vnd.cryptii.pipe+json",
    "vnd.crypto-shade-file",
    "vnd.cryptomator.encrypted",
    "vnd.cryptomator.vault",
    "vnd.ctc-posml",
    "vnd.ctct.ws+xml",
    "vnd.cups-pdf",
    "vnd.cups-postscript",
    "vnd.cups-ppd",
    "vnd.cups-raster",
    "vnd.cups-raw",
    "vnd.curl",
    "vnd.cyan.dean.root+xml",
    "vnd.cybank",
    "vnd.cyclonedx+json",
    "vnd.cyclonedx+xml",
    "vnd.d2l.coursepackage1p0+zip",
    "vnd.d3m-dataset",
    "vnd.d3m-problem",
    "vnd.dart",
    "vnd.data-vision.rdz",
    "vnd.datalog",
    "vnd.datapackage+json",
    "vnd.dataresource+json",
    "vnd.dbf",
    "vnd.dcmp+xml",
    "vnd.debian.binary-package",
    "vnd.dece.data",
    "vnd.dece.ttml+xml",
    "vnd.dece.unspecified",
    "vnd.dece.zip",
    "vnd.denovo.fcselayout-link",
    "vnd.desmume.movie",
    "vnd.deut+json",
    "vnd.dir-bi.plate-dl-nosuffix",
    "vnd.dm.delegation+xml",
    "vnd.dna",
    "vnd.document+json",
    "vnd.dolby.mobile.1",
    "vnd.dolby.mobile.2",
    "vnd.doremir.scorecloud-binary-document",
    "vnd.dpgraph",
    "vnd.dreamfactory",
    "vnd.drive+json",
    "vnd.dtg.local",
    "vnd.dtg.local.flash",
    "vnd.dtg.local.html",
    "vnd.dvb.ait",
    "vnd.dvb.dvbisl+xml",
    "vnd.dvb.dvbj",
    "vnd.dvb.esgcontainer",
    "vnd.dvb.ipdcdftnotifaccess",
    "vnd.dvb.ipdcesgaccess",
    "vnd.dvb.ipdcesgaccess2",
    "vnd.dvb.ipdcesgpdd",
    "vnd.dvb.ipdcroaming",
    "vnd.dvb.iptv.alfec-base",
    "vnd.dvb.iptv.alfec-enhancement",
    "vnd.dvb.notif-aggregate-root+xml",
    "vnd.dvb.notif-container+xml",
    "vnd.dvb.notif-generic+xml",
    "vnd.dvb.notif-ia-msglist+xml",
    "vnd.dvb.notif-ia-registration-request+xml",
    "vnd.dvb.notif-ia-registration-response+xml",
    "vnd.dvb.notif-init+xml",
    "vnd.dvb.pfr",
    "vnd.dvb.service",
    "vnd.dxr",
    "vnd.dynageo",
    "vnd.dzr",
    "vnd.easykaraoke.cdgdownload",
    "vnd.ecdis-update",
    "vnd.ecip.rlp",
    "vnd.eclipse.ditto+json",
    "vnd.ecowin.chart",
    "vnd.ecowin.filerequest",
    "vnd.ecowin.fileupdate",
    "vnd.ecowin.series",
    "vnd.ecowin.seriesrequest",
    "vnd.ecowin.seriesupdate",
    "vnd.edulith.edux+json",
    "vnd.efi.img",
    "vnd.efi.iso",
    "vnd.eln+zip",
    "vnd.emclient.accessrequest+xml",
    "vnd.enliven",
    "vnd.enphase.envoy",
    "vnd.eprints.data+xml",
    "vnd.epson.esf",
    "vnd.epson.msf",
    "vnd.epson.quickanime",
    "vnd.epson.salt",
    "vnd.epson.ssf",
    "vnd.ericsson.quickcall",
    "vnd.erofs",
    "vnd.espass-espass+zip",
    "vnd.eszigno3+xml",
    "vnd.etsi.aoc+xml",
    "vnd.etsi.asic-e+zip",
    "vnd.etsi.asic-s+zip",
    "vnd.etsi.cug+xml",
    "vnd.etsi.iptvcommand+xml",
    "vnd.etsi.iptvdiscovery+xml",
    "vnd.etsi.iptvprofile+xml",
    "vnd.etsi.iptvsad-bc+xml",
    "vnd.etsi.iptvsad-cod+xml",
    "vnd.etsi.iptvsad-npvr+xml",
    "vnd.etsi.iptvservice+xml",
    "vnd.etsi.iptvsync+xml",
    "vnd.etsi.iptvueprofile+xml",
    "vnd.etsi.mcid+xml",
    "vnd.etsi.mheg5",
    "vnd.etsi.overload-control-policy-dataset+xml",
    "vnd.etsi.pstn+xml",
    "vnd.etsi.sci+xml",
    "vnd.etsi.simservs+xml",
    "vnd.etsi.timestamp-token",
    "vnd.etsi.tsl+xml",
    "vnd.etsi.tsl.der",
    "vnd.eu.kasparian.car+json",
    "vnd.eudora.data",
    "vnd.evolv.ecig.profile",
    "vnd.evolv.ecig.settings",
    "vnd.evolv.ecig.theme",
    "vnd.exstream-empower+zip",
    "vnd.exstream-package",
    "vnd.ezpix-album",
    "vnd.ezpix-package",
    "vnd.f-secure.mobile",
    "vnd.faf+yaml",
    "vnd.familysearch.gedcom+zip",
    "vnd.fastcopy-disk-image",
    "vnd.fdsn.mseed",
    "vnd.fdsn.seed",
    "vnd.fdsn.stationxml+xml",
    "vnd.ffsns",
    "vnd.fgb",
    "vnd.ficlab.flb+zip",
    "vnd.filmit.zfc",
    "vnd.fints",
    "vnd.firemonkeys.cloudcell",
    "vnd.FloGraphIt",
    "vnd.fluxtime.clip",
    "vnd.font-fontforge-sfd",
    "vnd.foritech.container",
    "vnd.framemaker",
    "vnd.freelog.comic",
    "vnd.frogans.fnc",
    "vnd.frogans.ltf",
    "vnd.fsc.weblaunch",
    "vnd.fujifilm.fb.docuworks",
    "vnd.fujifilm.fb.docuworks.binder",
    "vnd.fujifilm.fb.docuworks.container",
    "vnd.fujifilm.fb.jfi+xml",
    "vnd.fujitsu.oasys",
    "vnd.fujitsu.oasys2",
    "vnd.fujitsu.oasys3",
    "vnd.fujitsu.oasysgp",
    "vnd.fujitsu.oasysprs",
    "vnd.fujixerox.ART-EX",
    "vnd.fujixerox.ART4",
    "vnd.fujixerox.ddd",
    "vnd.fujixerox.docuworks",
    "vnd.fujixerox.docuworks.binder",
    "vnd.fujixerox.docuworks.container",
    "vnd.fujixerox.HBPL",
    "vnd.fut-misnet",
    "vnd.futoin+cbor",
    "vnd.futoin+json",
    "vnd.fuzzysheet",
    "vnd.g3pix.g3fc",
    "vnd.ga4gh.passport+jwt",
    "vnd.genomatix.tuxedo",
    "vnd.genozip",
    "vnd.gentics.grd+json",
    "vnd.gentoo.catmetadata+xml",
    "vnd.gentoo.ebuild",
    "vnd.gentoo.eclass",
    "vnd.gentoo.gpkg",
    "vnd.gentoo.manifest",
    "vnd.gentoo.pkgmetadata+xml",
    "vnd.gentoo.xpak",
    "vnd.geo+json",
    "vnd.geocube+xml",
    "vnd.geogebra.file",
    "vnd.geogebra.pinboard",
    "vnd.geogebra.slides",
    "vnd.geogebra.tool",
    "vnd.geometry-explorer",
    "vnd.geonext",
    "vnd.geoplan",
    "vnd.geospace",
    "vnd.gerber",
    "vnd.globalplatform.card-content-mgt",
    "vnd.globalplatform.card-content-mgt-response",
    "vnd.gmx",
    "vnd.gnu.taler.exchange+json",
    "vnd.gnu.taler.merchant+json",
    "vnd.google-earth.kml+xml",
    "vnd.google-earth.kmz",
    "vnd.gov.sk.e-form+xml",
    "vnd.gov.sk.e-form+zip",
    "vnd.gov.sk.xmldatacontainer+xml",
    "vnd.gp3",
    "vnd.gpxsee.map+xml",
    "vnd.grafeq",
    "vnd.gridmp",
    "vnd.groove-account",
    "vnd.groove-help",
    "vnd.groove-identity-message",
    "vnd.groove-injector",
    "vnd.groove-tool-message",
    "vnd.groove-tool-template",
    "vnd.groove-vcard",
    "vnd.hal+json",
    "vnd.hal+xml",
    "vnd.HandHeld-Entertainment+xml",
    "vnd.hbci",
    "vnd.hc+json",
    "vnd.hcl-bireports",
    "vnd.hdfgroup.hdf4",
    "vnd.hdfgroup.hdf5",
    "vnd.hdt",
    "vnd.heroku+json",
    "vnd.hhe.lesson-player",
    "vnd.hp-HPGL",
    "vnd.hp-hpid",
    "vnd.hp-hps",
    "vnd.hp-jlyt",
    "vnd.hp-PCL",
    "vnd.hp-PCLXL",
    "vnd.hsl",
    "vnd.httphone",
    "vnd.hydrostatix.sof-data",
    "vnd.hyper+json",
    "vnd.hyper-item+json",
    "vnd.hyperdrive+json",
    "vnd.hzn-3d-crossword",
    "vnd.ibm.afplinedata",
    "vnd.ibm.electronic-media",
    "vnd.ibm.MiniPay",
    "vnd.ibm.modcap",
    "vnd.ibm.rights-management",
    "vnd.ibm.secure-container",
    "vnd.iccprofile",
    "vnd.ieee.1905",
    "vnd.igloader",
    "vnd.imagemeter.folder+zip",
    "vnd.imagemeter.image+zip",
    "vnd.immervision-ivp",
    "vnd.immervision-ivu",
    "vnd.ims.imsccv1p1",
    "vnd.ims.imsccv1p2",
    "vnd.ims.imsccv1p3",
    "vnd.ims.lis.v2.result+json",
    "vnd.ims.lti.v2.toolconsumerprofile+json",
    "vnd.ims.lti.v2.toolproxy+json",
    "vnd.ims.lti.v2.toolproxy.id+json",
    "vnd.ims.lti.v2.toolsettings+json",
    "vnd.ims.lti.v2.toolsettings.simple+json",
    "vnd.informedcontrol.rms+xml",
    "vnd.informix-visionary",
    "vnd.infotech.project",
    "vnd.infotech.project+xml",
    "vnd.innopath.wamp.notification",
    "vnd.insors.igm",
    "vnd.intercon.formnet",
    "vnd.intergeo",
    "vnd.intertrust.digibox",
    "vnd.intertrust.nncp",
    "vnd.intu.qbo",
    "vnd.intu.qfx",
    "vnd.ipfs.ipns-record",
    "vnd.ipld.car",
    "vnd.ipld.dag-cbor",
    "vnd.ipld.dag-json",
    "vnd.ipld.raw",
    "vnd.iptc.g2.catalogitem+xml",
    "vnd.iptc.g2.conceptitem+xml",
    "vnd.iptc.g2.knowledgeitem+xml",
    "vnd.iptc.g2.newsitem+xml",
    "vnd.iptc.g2.newsmessage+xml",
    "vnd.iptc.g2.packageitem+xml",
    "vnd.iptc.g2.planningitem+xml",
    "vnd.ipunplugged.rcprofile",
    "vnd.irepository.package+xml",
    "vnd.is-xpr",
    "vnd.isac.fcs",
    "vnd.iso11783-10+zip",
    "vnd.jam",
    "vnd.japannet-directory-service",
    "vnd.japannet-jpnstore-wakeup",
    "vnd.japannet-payment-wakeup",
    "vnd.japannet-registration",
    "vnd.japannet-registration-wakeup",
    "vnd.japannet-setstore-wakeup",
    "vnd.japannet-verification",
    "vnd.japannet-verification-wakeup",
    "vnd.jcp.javame.midlet-rms",
    "vnd.jisp",
    "vnd.joost.joda-archive",
    "vnd.jsk.isdn-ngn",
    "vnd.kahootz",
    "vnd.kde.karbon",
    "vnd.kde.kchart",
    "vnd.kde.kformula",
    "vnd.kde.kivio",
    "vnd.kde.kontour",
    "vnd.kde.kpresenter",
    "vnd.kde.kspread",
    "vnd.kde.kword",
    "vnd.kdl",
    "vnd.kenameaapp",
    "vnd.keyman.kmp+zip",
    "vnd.keyman.kmx",
    "vnd.kidspiration",
    "vnd.Kinar",
    "vnd.koan",
    "vnd.kodak-descriptor",
    "vnd.las",
    "vnd.las.las+json",
    "vnd.las.las+xml",
    "vnd.laszip",
    "vnd.ldev.productlicensing",
    "vnd.leap+json",
    "vnd.liberty-request+xml",
    "vnd.llamagraphics.life-balance.desktop",
    "vnd.llamagraphics.life-balance.exchange+xml",
    "vnd.logipipe.circuit+zip",
    "vnd.loom",
    "vnd.lotus-1-2-3",
    "vnd.lotus-approach",
    "vnd.lotus-freelance",
    "vnd.lotus-notes",
    "vnd.lotus-organizer",
    "vnd.lotus-screencam",
    "vnd.lotus-wordpro",
    "vnd.macports.portpkg",
    "vnd.majikah.bundle",
    "vnd.maml",
    "vnd.mapbox-vector-tile",
    "vnd.marlin.drm.actiontoken+xml",
    "vnd.marlin.drm.conftoken+xml",
    "vnd.marlin.drm.license+xml",
    "vnd.marlin.drm.mdcf",
    "vnd.mason+json",
    "vnd.maxar.archive.3tz+zip",
    "vnd.maxmind.maxmind-db",
    "vnd.mcd",
    "vnd.mdl",
    "vnd.mdl-mbsdf",
    "vnd.medcalcdata",
    "vnd.mediastation.cdkey",
    "vnd.medicalholodeck.recordxr",
    "vnd.meridian-slingshot",
    "vnd.mermaid",
    "vnd.MFER",
    "vnd.mfmp",
    "vnd.micro+json",
    "vnd.micrografx.flo",
    "vnd.micrografx.igx",
    "vnd.microsoft.portable-executable",
    "vnd.microsoft.windows.thumbnail-cache",
    "vnd.miele+json",
    "vnd.mif",
    "vnd.minisoft-hp3000-save",
    "vnd.mitsubishi.misty-guard.trustweb",
    "vnd.Mobius.DAF",
    "vnd.Mobius.DIS",
    "vnd.Mobius.MBK",
    "vnd.Mobius.MQY",
    "vnd.Mobius.MSL",
    "vnd.Mobius.PLC",
    "vnd.Mobius.TXF",
    "vnd.modl",
    "vnd.mophun.application",
    "vnd.mophun.certificate",
    "vnd.motorola.flexsuite",
    "vnd.motorola.flexsuite.adsi",
    "vnd.motorola.flexsuite.fis",
    "vnd.motorola.flexsuite.gotap",
    "vnd.motorola.flexsuite.kmr",
    "vnd.motorola.flexsuite.ttc",
    "vnd.motorola.flexsuite.wem",
    "vnd.motorola.iprm",
    "vnd.mozilla.xul+xml",
    "vnd.ms-3mfdocument",
    "vnd.ms-artgalry",
    "vnd.ms-asf",
    "vnd.ms-cab-compressed",
    "vnd.ms-excel",
    "vnd.ms-excel.addin.macroEnabled.12",
    "vnd.ms-excel.sheet.binary.macroEnabled.12",
    "vnd.ms-excel.sheet.macroEnabled.12",
    "vnd.ms-excel.template.macroEnabled.12",
    "vnd.ms-fontobject",
    "vnd.ms-htmlhelp",
    "vnd.ms-ims",
    "vnd.ms-lrm",
    "vnd.ms-office.activeX+xml",
    "vnd.ms-officetheme",
    "vnd.ms-playready.initiator+xml",
    "vnd.ms-powerpoint",
    "vnd.ms-powerpoint.addin.macroEnabled.12",
    "vnd.ms-powerpoint.presentation.macroEnabled.12",
    "vnd.ms-powerpoint.slide.macroEnabled.12",
    "vnd.ms-powerpoint.slideshow.macroEnabled.12",
    "vnd.ms-powerpoint.template.macroEnabled.12",
    "vnd.ms-PrintDeviceCapabilities+xml",
    "vnd.ms-PrintSchemaTicket+xml",
    "vnd.ms-project",
    "vnd.ms-tnef",
    "vnd.ms-windows.devicepairing",
    "vnd.ms-windows.nwprinting.oob",
    "vnd.ms-windows.printerpairing",
    "vnd.ms-windows.wsd.oob",
    "vnd.ms-wmdrm.lic-chlg-req",
    "vnd.ms-wmdrm.lic-resp",
    "vnd.ms-wmdrm.meter-chlg-req",
    "vnd.ms-wmdrm.meter-resp",
    "vnd.ms-word.document.macroEnabled.12",
    "vnd.ms-word.template.macroEnabled.12",
    "vnd.ms-works",
    "vnd.ms-wpl",
    "vnd.ms-xpsdocument",
    "vnd.msa-disk-image",
    "vnd.mseq",
    "vnd.msgpack",
    "vnd.msign",
    "vnd.multiad.creator",
    "vnd.multiad.creator.cif",
    "vnd.music-niff",
    "vnd.musician",
    "vnd.muvee.style",
    "vnd.mynfc",
    "vnd.nacamar.ybrid+json",
    "vnd.nato.bindingdataobject+cbor",
    "vnd.nato.bindingdataobject+json",
    "vnd.nato.bindingdataobject+xml",
    "vnd.nato.openxmlformats-package.iepd+zip",
    "vnd.ncd.control",
    "vnd.ncd.reference",
    "vnd.nearst.inv+json",
    "vnd.nebumind.line",
    "vnd.nervana",
    "vnd.netfpx",
    "vnd.neurolanguage.nlu",
    "vnd.nimn",
    "vnd.nintendo.nitro.rom",
    "vnd.nintendo.snes.rom",
    "vnd.nitf",
    "vnd.noblenet-directory",
    "vnd.noblenet-sealer",
    "vnd.noblenet-web",
    "vnd.nokia.catalogs",
    "vnd.nokia.conml+wbxml",
    "vnd.nokia.conml+xml",
    "vnd.nokia.iptv.config+xml",
    "vnd.nokia.iSDS-radio-presets",
    "vnd.nokia.landmark+wbxml",
    "vnd.nokia.landmark+xml",
    "vnd.nokia.landmarkcollection+xml",
    "vnd.nokia.n-gage.ac+xml",
    "vnd.nokia.n-gage.data",
    "vnd.nokia.n-gage.symbian.install",
    "vnd.nokia.ncd",
    "vnd.nokia.pcd+wbxml",
    "vnd.nokia.pcd+xml",
    "vnd.nokia.radio-preset",
    "vnd.nokia.radio-presets",
    "vnd.novadigm.EDM",
    "vnd.novadigm.EDX",
    "vnd.novadigm.EXT",
    "vnd.ntt-local.content-share",
    "vnd.ntt-local.file-transfer",
    "vnd.ntt-local.ogw_remote-access",
    "vnd.ntt-local.sip-ta_remote",
    "vnd.ntt-local.sip-ta_tcp_stream",
    "vnd.nubaltec.nudoku-game",
    "vnd.oai.workflows",
    "vnd.oai.workflows+json",
    "vnd.oai.workflows+yaml",
    "vnd.oasis.opendocument.base",
    "vnd.oasis.opendocument.chart",
    "vnd.oasis.opendocument.chart-template",
    "vnd.oasis.opendocument.database",
    "vnd.oasis.opendocument.formula",
    "vnd.oasis.opendocument.formula-template",
    "vnd.oasis.opendocument.graphics",
    "vnd.oasis.opendocument.graphics-template",
    "vnd.oasis.opendocument.image",
    "vnd.oasis.opendocument.image-template",
    "vnd.oasis.opendocument.presentation",
    "vnd.oasis.opendocument.presentation-template",
    "vnd.oasis.opendocument.spreadsheet",
    "vnd.oasis.opendocument.spreadsheet-template",
    "vnd.oasis.opendocument.text",
    "vnd.oasis.opendocument.text-master",
    "vnd.oasis.opendocument.text-master-template",
    "vnd.oasis.opendocument.text-template",
    "vnd.oasis.opendocument.text-web",
    "vnd.obn",
    "vnd.ocf+cbor",
    "vnd.oci.image.manifest.v1+json",
    "vnd.oftn.l10n+json",
    "vnd.oipf.contentaccessdownload+xml",
    "vnd.oipf.contentaccessstreaming+xml",
    "vnd.oipf.cspg-hexbinary",
    "vnd.oipf.dae.svg+xml",
    "vnd.oipf.dae.xhtml+xml",
    "vnd.oipf.mippvcontrolmessage+xml",
    "vnd.oipf.pae.gem",
    "vnd.oipf.spdiscovery+xml",
    "vnd.oipf.spdlist+xml",
    "vnd.oipf.ueprofile+xml",
    "vnd.oipf.userprofile+xml",
    "vnd.olpc-sugar",
    "vnd.oma-scws-config",
    "vnd.oma-scws-http-request",
    "vnd.oma-scws-http-response",
    "vnd.oma.bcast.associated-procedure-parameter+xml",
    "vnd.oma.bcast.drm-trigger+xml",
    "vnd.oma.bcast.imd+xml",
    "vnd.oma.bcast.ltkm",
    "vnd.oma.bcast.notification+xml",
    "vnd.oma.bcast.provisioningtrigger",
    "vnd.oma.bcast.sgboot",
    "vnd.oma.bcast.sgdd+xml",
    "vnd.oma.bcast.sgdu",
    "vnd.oma.bcast.simple-symbol-container",
    "vnd.oma.bcast.smartcard-trigger+xml",
    "vnd.oma.bcast.sprov+xml",
    "vnd.oma.bcast.stkm",
    "vnd.oma.cab-address-book+xml",
    "vnd.oma.cab-feature-handler+xml",
    "vnd.oma.cab-pcc+xml",
    "vnd.oma.cab-subs-invite+xml",
    "vnd.oma.cab-user-prefs+xml",
    "vnd.oma.dcd",
    "vnd.oma.dcdc",
    "vnd.oma.dd2+xml",
    "vnd.oma.drm.risd+xml",
    "vnd.oma.group-usage-list+xml",
    "vnd.oma.lwm2m+cbor",
    "vnd.oma.lwm2m+json",
    "vnd.oma.lwm2m+tlv",
    "vnd.oma.pal+xml",
    "vnd.oma.poc.detailed-progress-report+xml",
    "vnd.oma.poc.final-report+xml",
    "vnd.oma.poc.groups+xml",
    "vnd.oma.poc.invocation-descriptor+xml",
    "vnd.oma.poc.optimized-progress-report+xml",
    "vnd.oma.push",
    "vnd.oma.scidm.messages+xml",
    "vnd.oma.xcap-directory+xml",
    "vnd.omads-email+xml",
    "vnd.omads-file+xml",
    "vnd.omads-folder+xml",
    "vnd.omaloc-supl-init",
    "vnd.oms.cellular-cose-content+cbor",
    "vnd.onepager",
    "vnd.onepagertamp",
    "vnd.onepagertamx",
    "vnd.onepagertat",
    "vnd.onepagertatp",
    "vnd.onepagertatx",
    "vnd.onvif.metadata",
    "vnd.openblox.game+xml",
    "vnd.openblox.game-binary",
    "vnd.openeye.oeb",
    "vnd.openprinttag",
    "vnd.openstreetmap.data+xml",
    "vnd.opentimestamps.ots",
    "vnd.openvpi.dspx+json",
    "vnd.openxmlformats-officedocument.custom-properties+xml",
    "vnd.openxmlformats-officedocument.customXmlProperties+xml",
    "vnd.openxmlformats-officedocument.drawing+xml",
    "vnd.openxmlformats-officedocument.drawingml.chart+xml",
    "vnd.openxmlformats-officedocument.drawingml.chartshapes+xml",
    "vnd.openxmlformats-officedocument.drawingml.diagramColors+xml",
    "vnd.openxmlformats-officedocument.drawingml.diagramData+xml",
    "vnd.openxmlformats-officedocument.drawingml.diagramLayout+xml",
    "vnd.openxmlformats-officedocument.drawingml.diagramStyle+xml",
    "vnd.openxmlformats-officedocument.extended-properties+xml",
    "vnd.openxmlformats-officedocument.presentationml.commentAuthors+xml",
    "vnd.openxmlformats-officedocument.presentationml.comments+xml",
    "vnd.openxmlformats-officedocument.presentationml.handoutMaster+xml",
    "vnd.openxmlformats-officedocument.presentationml.notesMaster+xml",
    "vnd.openxmlformats-officedocument.presentationml.notesSlide+xml",
    "vnd.openxmlformats-officedocument.presentationml.presentation",
    "vnd.openxmlformats-officedocument.presentationml.presentation.main+xml",
    "vnd.openxmlformats-officedocument.presentationml.presProps+xml",
    "vnd.openxmlformats-officedocument.presentationml.slide",
    "vnd.openxmlformats-officedocument.presentationml.slide+xml",
    "vnd.openxmlformats-officedocument.presentationml.slideLayout+xml",
    "vnd.openxmlformats-officedocument.presentationml.slideMaster+xml",
    "vnd.openxmlformats-officedocument.presentationml.slideshow",
    "vnd.openxmlformats-officedocument.presentationml.slideshow.main+xml",
    "vnd.openxmlformats-officedocument.presentationml.slideUpdateInfo+xml",
    "vnd.openxmlformats-officedocument.presentationml.tableStyles+xml",
    "vnd.openxmlformats-officedocument.presentationml.tags+xml",
    "vnd.openxmlformats-officedocument.presentationml.template",
    "vnd.openxmlformats-officedocument.presentationml.template.main+xml",
    "vnd.openxmlformats-officedocument.presentationml.viewProps+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.calcChain+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.comments+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.connections+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.dialogsheet+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.externalLink+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheDefinition+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheRecords+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.pivotTable+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.queryTable+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.revisionHeaders+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.revisionLog+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.sheetMetadata+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.styles+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.table+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.tableSingleCells+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.template",
    "vnd.openxmlformats-officedocument.spreadsheetml.template.main+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.userNames+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.volatileDependencies+xml",
    "vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml",
    "vnd.openxmlformats-officedocument.theme+xml",
    "vnd.openxmlformats-officedocument.themeOverride+xml",
    "vnd.openxmlformats-officedocument.vmlDrawing",
    "vnd.openxmlformats-officedocument.wordprocessingml.comments+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.document",
    "vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.footer+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.settings+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.styles+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.template",
    "vnd.openxmlformats-officedocument.wordprocessingml.template.main+xml",
    "vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml",
    "vnd.openxmlformats-package.core-properties+xml",
    "vnd.openxmlformats-package.digital-signature-xmlsignature+xml",
    "vnd.openxmlformats-package.relationships+xml",
    "vnd.oracle.resource+json",
    "vnd.orange.indata",
    "vnd.osa.netdeploy",
    "vnd.osgeo.mapguide.package",
    "vnd.osgi.bundle",
    "vnd.osgi.dp",
    "vnd.osgi.subsystem",
    "vnd.otps.ct-kip+xml",
    "vnd.oxli.countgraph",
    "vnd.pagerduty+json",
    "vnd.palm",
    "vnd.panoply",
    "vnd.paos.xml",
    "vnd.patentdive",
    "vnd.patientecommsdoc",
    "vnd.pawaafile",
    "vnd.pcos",
    "vnd.pg.format",
    "vnd.pg.osasli",
    "vnd.phbk+xml",
    "vnd.piaccess.application-licence",
    "vnd.picsel",
    "vnd.pmi.widget",
    "vnd.pmtiles",
    "vnd.poc.group-advertisement+xml",
    "vnd.pocketlearn",
    "vnd.powerbuilder6",
    "vnd.powerbuilder6-s",
    "vnd.powerbuilder7",
    "vnd.powerbuilder7-s",
    "vnd.powerbuilder75",
    "vnd.powerbuilder75-s",
    "vnd.pp.systemverify+xml",
    "vnd.preminet",
    "vnd.previewsystems.box",
    "vnd.project-graph",
    "vnd.proteus.magazine",
    "vnd.psfs",
    "vnd.pt.mundusmundi",
    "vnd.publishare-delta-tree",
    "vnd.pvi.ptid1",
    "vnd.pwg-multiplexed",
    "vnd.pwg-xhtml-print+xml",
    "vnd.pyon+json",
    "vnd.qualcomm.brew-app-res",
    "vnd.quarantainenet",
    "vnd.Quark.QuarkXPress",
    "vnd.quobject-quoxdocument",
    "vnd.R74n.sandboxels+json",
    "vnd.radisys.moml+xml",
    "vnd.radisys.msml+xml",
    "vnd.radisys.msml-audit+xml",
    "vnd.radisys.msml-audit-conf+xml",
    "vnd.radisys.msml-audit-conn+xml",
    "vnd.radisys.msml-audit-dialog+xml",
    "vnd.radisys.msml-audit-stream+xml",
    "vnd.radisys.msml-conf+xml",
    "vnd.radisys.msml-dialog+xml",
    "vnd.radisys.msml-dialog-base+xml",
    "vnd.radisys.msml-dialog-fax-detect+xml",
    "vnd.radisys.msml-dialog-fax-sendrecv+xml",
    "vnd.radisys.msml-dialog-group+xml",
    "vnd.radisys.msml-dialog-speech+xml",
    "vnd.radisys.msml-dialog-transform+xml",
    "vnd.rainstor.data",
    "vnd.rapid",
    "vnd.rar",
    "vnd.realvnc.bed",
    "vnd.recordare.musicxml",
    "vnd.recordare.musicxml+xml",
    "vnd.rego",
    "vnd.relpipe",
    "vnd.RenLearn.rlprint",
    "vnd.resilient.logic",
    "vnd.restful+json",
    "vnd.rig.cryptonote",
    "vnd.route66.link66+xml",
    "vnd.rs-274x",
    "vnd.ruckus.download",
    "vnd.s3sms",
    "vnd.sailingtracker.track",
    "vnd.sar",
    "vnd.sbm.cid",
    "vnd.sbm.mid2",
    "vnd.scribus",
    "vnd.sealed.3df",
    "vnd.sealed.csf",
    "vnd.sealed.doc",
    "vnd.sealed.eml",
    "vnd.sealed.mht",
    "vnd.sealed.net",
    "vnd.sealed.ppt",
    "vnd.sealed.tiff",
    "vnd.sealed.xls",
    "vnd.sealedmedia.softseal.html",
    "vnd.sealedmedia.softseal.pdf",
    "vnd.seemail",
    "vnd.seis+json",
    "vnd.sema",
    "vnd.semd",
    "vnd.semf",
    "vnd.shade-save-file",
    "vnd.shana.informed.formdata",
    "vnd.shana.informed.formtemplate",
    "vnd.shana.informed.interchange",
    "vnd.shana.informed.package",
    "vnd.shootproof+json",
    "vnd.shopkick+json",
    "vnd.shp",
    "vnd.shx",
    "vnd.sigrok.session",
    "vnd.SimTech-MindMapper",
    "vnd.siren+json",
    "vnd.sirtx.vmv0",
    "vnd.sketchometry",
    "vnd.smaf",
    "vnd.smart.notebook",
    "vnd.smart.teacher",
    "vnd.smintio.portals.archive",
    "vnd.snesdev-page-table",
    "vnd.software602.filler.form+xml",
    "vnd.software602.filler.form-xml-zip",
    "vnd.solent.sdkm+xml",
    "vnd.spotfire.dxp",
    "vnd.spotfire.sfs",
    "vnd.sqlite3",
    "vnd.sri",
    "vnd.sss-cod",
    "vnd.sss-dtf",
    "vnd.sss-ntf",
    "vnd.stepmania.package",
    "vnd.stepmania.stepchart",
    "vnd.street-stream",
    "vnd.sun.wadl+xml",
    "vnd.superfile.super",
    "vnd.sus-calendar",
    "vnd.svd",
    "vnd.swiftview-ics",
    "vnd.sybyl.mol2",
    "vnd.sycle+xml",
    "vnd.syft+json",
    "vnd.syncml+xml",
    "vnd.syncml.dm+wbxml",
    "vnd.syncml.dm+xml",
    "vnd.syncml.dm.notification",
    "vnd.syncml.dmddf+wbxml",
    "vnd.syncml.dmddf+xml",
    "vnd.syncml.dmtnds+wbxml",
    "vnd.syncml.dmtnds+xml",
    "vnd.syncml.ds.notification",
    "vnd.tableschema+json",
    "vnd.tao.intent-module-archive",
    "vnd.tcpdump.pcap",
    "vnd.think-cell.ppttc+json",
    "vnd.tmd.mediaflex.api+xml",
    "vnd.tml",
    "vnd.tmobile-livetv",
    "vnd.tri.onesource",
    "vnd.trid.tpt",
    "vnd.triscape.mxs",
    "vnd.trueapp",
    "vnd.truedoc",
    "vnd.ubisoft.webplayer",
    "vnd.ufdl",
    "vnd.uic.dosipas.v1",
    "vnd.uic.dosipas.v2",
    "vnd.uic.osdm+json",
    "vnd.uic.tlb-fcb",
    "vnd.uiq.theme",
    "vnd.umajin",
    "vnd.unity",
    "vnd.uoml+xml",
    "vnd.uplanet.alert",
    "vnd.uplanet.alert-wbxml",
    "vnd.uplanet.bearer-choice",
    "vnd.uplanet.bearer-choice-wbxml",
    "vnd.uplanet.cacheop",
    "vnd.uplanet.cacheop-wbxml",
    "vnd.uplanet.channel",
    "vnd.uplanet.channel-wbxml",
    "vnd.uplanet.list",
    "vnd.uplanet.list-wbxml",
    "vnd.uplanet.listcmd",
    "vnd.uplanet.listcmd-wbxml",
    "vnd.uplanet.signal",
    "vnd.uri-map",
    "vnd.valve.source.material",
    "vnd.vcx",
    "vnd.vd-study",
    "vnd.vectorworks",
    "vnd.vel+json",
    "vnd.veraison.tsm-report+cbor",
    "vnd.veraison.tsm-report+json",
    "vnd.verifier-attestation+jwt",
    "vnd.verimatrix.vcas",
    "vnd.veritone.aion+json",
    "vnd.vertifile.pvf",
    "vnd.veryant.thin",
    "vnd.ves.encrypted",
    "vnd.vidsoft.vidconference",
    "vnd.visio",
    "vnd.visionary",
    "vnd.vividence.scriptfile",
    "vnd.vocalshaper.vsp4",
    "vnd.vsf",
    "vnd.vuq",
    "vnd.wantverse",
    "vnd.wap.sic",
    "vnd.wap.slc",
    "vnd.wap.wbxml",
    "vnd.wap.wmlc",
    "vnd.wap.wmlscriptc",
    "vnd.wasmflow.wafl",
    "vnd.webturbo",
    "vnd.wfa.dpp",
    "vnd.wfa.p2p",
    "vnd.wfa.wsc",
    "vnd.windows.devicepairing",
    "vnd.wmap",
    "vnd.wmc",
    "vnd.wmf.bootstrap",
    "vnd.wolfram.mathematica",
    "vnd.wolfram.mathematica.package",
    "vnd.wolfram.player",
    "vnd.wordlift",
    "vnd.wordperfect",
    "vnd.wqd",
    "vnd.wrq-hp3000-labelled",
    "vnd.wt.stf",
    "vnd.wv.csp+wbxml",
    "vnd.wv.csp+xml",
    "vnd.wv.ssp+xml",
    "vnd.xacml+json",
    "vnd.xara",
    "vnd.xarin.cpj",
    "vnd.xcdn",
    "vnd.xecrets-encrypted",
    "vnd.xfdl",
    "vnd.xfdl.webform",
    "vnd.xmi+xml",
    "vnd.xmpie.cpkg",
    "vnd.xmpie.dpkg",
    "vnd.xmpie.plan",
    "vnd.xmpie.ppkg",
    "vnd.xmpie.xlim",
    "vnd.yamaha.hv-dic",
    "vnd.yamaha.hv-script",
    "vnd.yamaha.hv-voice",
    "vnd.yamaha.openscoreformat",
    "vnd.yamaha.openscoreformat.osfpvg+xml",
    "vnd.yamaha.remote-setup",
    "vnd.yamaha.smaf-audio",
    "vnd.yamaha.smaf-phrase",
    "vnd.yamaha.through-ngn",
    "vnd.yamaha.tunnel-udpencap",
    "vnd.yaoweme",
    "vnd.yellowriver-custom-menu",
    "vnd.youtube.yt",
    "vnd.zoho-document.writer",
    "vnd.zoho-presentation.show",
    "vnd.zoho.spreadsheetml.sheet",
    "vnd.zul",
    "vnd.zzazz.deck+xml",
    "voicexml+xml",
    "voucher-cms+json",
    "voucher-jws+json",
    "vp",
    "vp+cose",
    "vp+jwt",
    "vp+sd-jwt",
    "vq-rtcpxr",
    "wasm",
    "watcherinfo+xml",
    "webpush-options+json",
    "whoispp-query",
    "whoispp-response",
    "widget",
    "wita",
    "wordperfect5.1",
    "wsdl+xml",
    "wspolicy+xml",
    "x-pki-message",
    "x-www-form-urlencoded",
    "x-x509-ca-cert",
    "x-x509-ca-ra-cert",
    "x-x509-next-ca-cert",
    "x400-bp",
    "xacml+xml",
    "xcap-att+xml",
    "xcap-caps+xml",
    "xcap-diff+xml",
    "xcap-el+xml",
    "xcap-error+xml",
    "xcap-ns+xml",
    "xcon-conference-info+xml",
    "xcon-conference-info-diff+xml",
    "xenc+xml",
    "xfdf",
    "xhtml+xml",
    "xliff+xml",
    "xml",
    "xml-dtd",
    "xml-external-parsed-entity",
    "xml-patch+xml",
    "xmpp+xml",
    "xop+xml",
    "xslt+xml",
    "xv+xml",
    "yaml",
    "yang",
    "yang-data+cbor",
    "yang-data+json",
    "yang-data+xml",
    "yang-patch+json",
    "yang-patch+xml",
    "yang-sid+json",
    "yin+xml",
    "zip",
    "zlib",
    "zstd",
];
impl ApplicationSubtype {
    pub fn to_str(&self) -> &str {
        if let Self::Private(sub) = self {
            return sub.to_str();
        }
        let p = APPLICATION_SUBTYPE_VLIST.binary_search(self).unwrap();
        APPLICATION_SUBTYPE_SLIST[p]
    }
}
impl FromStr for ApplicationSubtype {
    type Err = MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_ascii_lowercase();
        if let Ok(pos) =
            APPLICATION_SUBTYPE_SLIST.binary_search_by_key(&lower, |t| t.to_ascii_lowercase())
        {
            return Ok(APPLICATION_SUBTYPE_VLIST[pos].clone());
        }
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AudioSubtype {
    /// 1d-interleaved-parityfec
    _1DInterleavedParityfec,
    /// 32kadpcm
    _32Kadpcm,
    /// 3gpp
    _3Gpp,
    /// 3gpp2
    _3Gpp2,
    /// aac
    Aac,
    /// ac3
    Ac3,
    /// AMR
    Amr,
    /// AMR-WB
    AmrWb,
    /// amr-wb+
    AmrWbPlus,
    /// aptx
    Aptx,
    /// asc
    Asc,
    /// ATRAC-ADVANCED-LOSSLESS
    AtracAdvancedLossless,
    /// ATRAC-X
    AtracX,
    /// ATRAC3
    Atrac3,
    /// basic
    Basic,
    /// BV16
    Bv16,
    /// BV32
    Bv32,
    /// clearmode
    Clearmode,
    /// CN
    Cn,
    /// DAT12
    Dat12,
    /// dls
    Dls,
    /// dsr-es201108
    DsrEs201108,
    /// dsr-es202050
    DsrEs202050,
    /// dsr-es202211
    DsrEs202211,
    /// dsr-es202212
    DsrEs202212,
    /// DV
    Dv,
    /// DVI4
    Dvi4,
    /// eac3
    Eac3,
    /// encaprtp
    Encaprtp,
    /// EVRC
    Evrc,
    /// EVRC-QCP
    EvrcQcp,
    /// EVRC0
    Evrc0,
    /// EVRC1
    Evrc1,
    /// EVRCB
    Evrcb,
    /// EVRCB0
    Evrcb0,
    /// EVRCB1
    Evrcb1,
    /// EVRCNW
    Evrcnw,
    /// EVRCNW0
    Evrcnw0,
    /// EVRCNW1
    Evrcnw1,
    /// EVRCWB
    Evrcwb,
    /// EVRCWB0
    Evrcwb0,
    /// EVRCWB1
    Evrcwb1,
    /// EVS
    Evs,
    /// example
    Example,
    /// flac
    Flac,
    /// flexfec
    Flexfec,
    /// fwdred
    Fwdred,
    /// G711-0
    G7110,
    /// G719
    G719,
    /// G722
    G722,
    /// G7221
    G7221,
    /// G723
    G723,
    /// G726-16
    G72616,
    /// G726-24
    G72624,
    /// G726-32
    G72632,
    /// G726-40
    G72640,
    /// G728
    G728,
    /// G729
    G729,
    /// G7291
    G7291,
    /// G729D
    G729D,
    /// G729E
    G729E,
    /// GSM
    Gsm,
    /// GSM-EFR
    GsmEfr,
    /// GSM-HR-08
    GsmHr08,
    /// iLBC
    ILbc,
    /// ip-mr_v2.5
    IpMrV2Dot5,
    /// L16
    L16,
    /// L20
    L20,
    /// L24
    L24,
    /// L8
    L8,
    /// LPC
    Lpc,
    /// matroska
    Matroska,
    /// MELP
    Melp,
    /// MELP1200
    Melp1200,
    /// MELP2400
    Melp2400,
    /// MELP600
    Melp600,
    /// mhas
    Mhas,
    /// midi-clip
    MidiClip,
    /// mobile-xmf
    MobileXmf,
    /// mp4
    Mp4,
    /// MP4A-LATM
    Mp4ALatm,
    /// MPA
    Mpa,
    /// mpa-robust
    MpaRobust,
    /// mpeg
    Mpeg,
    /// mpeg4-generic
    Mpeg4Generic,
    /// ogg
    Ogg,
    /// opus
    Opus,
    /// parityfec
    Parityfec,
    /// PCMA
    Pcma,
    /// PCMA-WB
    PcmaWb,
    /// PCMU
    Pcmu,
    /// PCMU-WB
    PcmuWb,
    /// prs.sid
    PrsDotSid,
    /// QCELP
    Qcelp,
    /// raptorfec
    Raptorfec,
    /// RED
    Red,
    /// rtp-enc-aescm128
    RtpEncAescm128,
    /// rtp-midi
    RtpMidi,
    /// rtploopback
    Rtploopback,
    /// rtx
    Rtx,
    /// scip
    Scip,
    /// SMV
    Smv,
    /// SMV-QCP
    SmvQcp,
    /// SMV0
    Smv0,
    /// sofa
    Sofa,
    /// soundfont
    Soundfont,
    /// sp-midi
    SpMidi,
    /// speex
    Speex,
    /// t140c
    T140C,
    /// t38
    T38,
    /// telephone-event
    TelephoneEvent,
    /// TETRA_ACELP
    TetraAcelp,
    /// TETRA_ACELP_BB
    TetraAcelpBb,
    /// tone
    Tone,
    /// TSVCIS
    Tsvcis,
    /// UEMCLIP
    Uemclip,
    /// ulpfec
    Ulpfec,
    /// usac
    Usac,
    /// VDVI
    Vdvi,
    /// VMR-WB
    VmrWb,
    /// vnd.3gpp.iufp
    VndDot3GppDotIufp,
    /// vnd.4SB
    VndDot4Sb,
    /// vnd.audiokoz
    VndDotAudiokoz,
    /// vnd.blockfact.facta
    VndDotBlockfactDotFacta,
    /// vnd.CELP
    VndDotCelp,
    /// vnd.cisco.nse
    VndDotCiscoDotNse,
    /// vnd.cmles.radio-events
    VndDotCmlesDotRadioEvents,
    /// vnd.cns.anp1
    VndDotCnsDotAnp1,
    /// vnd.cns.inf1
    VndDotCnsDotInf1,
    /// vnd.dece.audio
    VndDotDeceDotAudio,
    /// vnd.digital-winds
    VndDotDigitalWinds,
    /// vnd.dlna.adts
    VndDotDlnaDotAdts,
    /// vnd.dolby.heaac.1
    VndDotDolbyDotHeaacDot1,
    /// vnd.dolby.heaac.2
    VndDotDolbyDotHeaacDot2,
    /// vnd.dolby.mlp
    VndDotDolbyDotMlp,
    /// vnd.dolby.mps
    VndDotDolbyDotMps,
    /// vnd.dolby.pl2
    VndDotDolbyDotPl2,
    /// vnd.dolby.pl2x
    VndDotDolbyDotPl2X,
    /// vnd.dolby.pl2z
    VndDotDolbyDotPl2Z,
    /// vnd.dolby.pulse.1
    VndDotDolbyDotPulseDot1,
    /// vnd.dra
    VndDotDra,
    /// vnd.dts
    VndDotDts,
    /// vnd.dts.hd
    VndDotDtsDotHd,
    /// vnd.dts.uhd
    VndDotDtsDotUhd,
    /// vnd.dvb.file
    VndDotDvbDotFile,
    /// vnd.everad.plj
    VndDotEveradDotPlj,
    /// vnd.hns.audio
    VndDotHnsDotAudio,
    /// vnd.lucent.voice
    VndDotLucentDotVoice,
    /// vnd.ms-playready.media.pya
    VndDotMsPlayreadyDotMediaDotPya,
    /// vnd.nokia.mobile-xmf
    VndDotNokiaDotMobileXmf,
    /// vnd.nortel.vbk
    VndDotNortelDotVbk,
    /// vnd.nuera.ecelp4800
    VndDotNueraDotEcelp4800,
    /// vnd.nuera.ecelp7470
    VndDotNueraDotEcelp7470,
    /// vnd.nuera.ecelp9600
    VndDotNueraDotEcelp9600,
    /// vnd.octel.sbc
    VndDotOctelDotSbc,
    /// vnd.presonus.multitrack
    VndDotPresonusDotMultitrack,
    /// vnd.qcelp
    VndDotQcelpDeprecatedInFavorOfAudioQcelp,
    /// vnd.rhetorex.32kadpcm
    VndDotRhetorexDot32Kadpcm,
    /// vnd.rip
    VndDotRip,
    /// vnd.sealedmedia.softseal.mpeg
    VndDotSealedmediaDotSoftsealDotMpeg,
    /// vnd.vmx.cvsd
    VndDotVmxDotCvsd,
    /// vorbis
    Vorbis,
    /// vorbis-config
    VorbisConfig,
    /// private
    Private(PrivateSubtype),
}
const AUDIO_SUBTYPE_VLIST: &[AudioSubtype] = &[
    AudioSubtype::_1DInterleavedParityfec,
    AudioSubtype::_32Kadpcm,
    AudioSubtype::_3Gpp,
    AudioSubtype::_3Gpp2,
    AudioSubtype::Aac,
    AudioSubtype::Ac3,
    AudioSubtype::Amr,
    AudioSubtype::AmrWb,
    AudioSubtype::AmrWbPlus,
    AudioSubtype::Aptx,
    AudioSubtype::Asc,
    AudioSubtype::AtracAdvancedLossless,
    AudioSubtype::AtracX,
    AudioSubtype::Atrac3,
    AudioSubtype::Basic,
    AudioSubtype::Bv16,
    AudioSubtype::Bv32,
    AudioSubtype::Clearmode,
    AudioSubtype::Cn,
    AudioSubtype::Dat12,
    AudioSubtype::Dls,
    AudioSubtype::DsrEs201108,
    AudioSubtype::DsrEs202050,
    AudioSubtype::DsrEs202211,
    AudioSubtype::DsrEs202212,
    AudioSubtype::Dv,
    AudioSubtype::Dvi4,
    AudioSubtype::Eac3,
    AudioSubtype::Encaprtp,
    AudioSubtype::Evrc,
    AudioSubtype::EvrcQcp,
    AudioSubtype::Evrc0,
    AudioSubtype::Evrc1,
    AudioSubtype::Evrcb,
    AudioSubtype::Evrcb0,
    AudioSubtype::Evrcb1,
    AudioSubtype::Evrcnw,
    AudioSubtype::Evrcnw0,
    AudioSubtype::Evrcnw1,
    AudioSubtype::Evrcwb,
    AudioSubtype::Evrcwb0,
    AudioSubtype::Evrcwb1,
    AudioSubtype::Evs,
    AudioSubtype::Example,
    AudioSubtype::Flac,
    AudioSubtype::Flexfec,
    AudioSubtype::Fwdred,
    AudioSubtype::G7110,
    AudioSubtype::G719,
    AudioSubtype::G722,
    AudioSubtype::G7221,
    AudioSubtype::G723,
    AudioSubtype::G72616,
    AudioSubtype::G72624,
    AudioSubtype::G72632,
    AudioSubtype::G72640,
    AudioSubtype::G728,
    AudioSubtype::G729,
    AudioSubtype::G7291,
    AudioSubtype::G729D,
    AudioSubtype::G729E,
    AudioSubtype::Gsm,
    AudioSubtype::GsmEfr,
    AudioSubtype::GsmHr08,
    AudioSubtype::ILbc,
    AudioSubtype::IpMrV2Dot5,
    AudioSubtype::L16,
    AudioSubtype::L20,
    AudioSubtype::L24,
    AudioSubtype::L8,
    AudioSubtype::Lpc,
    AudioSubtype::Matroska,
    AudioSubtype::Melp,
    AudioSubtype::Melp1200,
    AudioSubtype::Melp2400,
    AudioSubtype::Melp600,
    AudioSubtype::Mhas,
    AudioSubtype::MidiClip,
    AudioSubtype::MobileXmf,
    AudioSubtype::Mp4,
    AudioSubtype::Mp4ALatm,
    AudioSubtype::Mpa,
    AudioSubtype::MpaRobust,
    AudioSubtype::Mpeg,
    AudioSubtype::Mpeg4Generic,
    AudioSubtype::Ogg,
    AudioSubtype::Opus,
    AudioSubtype::Parityfec,
    AudioSubtype::Pcma,
    AudioSubtype::PcmaWb,
    AudioSubtype::Pcmu,
    AudioSubtype::PcmuWb,
    AudioSubtype::PrsDotSid,
    AudioSubtype::Qcelp,
    AudioSubtype::Raptorfec,
    AudioSubtype::Red,
    AudioSubtype::RtpEncAescm128,
    AudioSubtype::RtpMidi,
    AudioSubtype::Rtploopback,
    AudioSubtype::Rtx,
    AudioSubtype::Scip,
    AudioSubtype::Smv,
    AudioSubtype::SmvQcp,
    AudioSubtype::Smv0,
    AudioSubtype::Sofa,
    AudioSubtype::Soundfont,
    AudioSubtype::SpMidi,
    AudioSubtype::Speex,
    AudioSubtype::T140C,
    AudioSubtype::T38,
    AudioSubtype::TelephoneEvent,
    AudioSubtype::TetraAcelp,
    AudioSubtype::TetraAcelpBb,
    AudioSubtype::Tone,
    AudioSubtype::Tsvcis,
    AudioSubtype::Uemclip,
    AudioSubtype::Ulpfec,
    AudioSubtype::Usac,
    AudioSubtype::Vdvi,
    AudioSubtype::VmrWb,
    AudioSubtype::VndDot3GppDotIufp,
    AudioSubtype::VndDot4Sb,
    AudioSubtype::VndDotAudiokoz,
    AudioSubtype::VndDotBlockfactDotFacta,
    AudioSubtype::VndDotCelp,
    AudioSubtype::VndDotCiscoDotNse,
    AudioSubtype::VndDotCmlesDotRadioEvents,
    AudioSubtype::VndDotCnsDotAnp1,
    AudioSubtype::VndDotCnsDotInf1,
    AudioSubtype::VndDotDeceDotAudio,
    AudioSubtype::VndDotDigitalWinds,
    AudioSubtype::VndDotDlnaDotAdts,
    AudioSubtype::VndDotDolbyDotHeaacDot1,
    AudioSubtype::VndDotDolbyDotHeaacDot2,
    AudioSubtype::VndDotDolbyDotMlp,
    AudioSubtype::VndDotDolbyDotMps,
    AudioSubtype::VndDotDolbyDotPl2,
    AudioSubtype::VndDotDolbyDotPl2X,
    AudioSubtype::VndDotDolbyDotPl2Z,
    AudioSubtype::VndDotDolbyDotPulseDot1,
    AudioSubtype::VndDotDra,
    AudioSubtype::VndDotDts,
    AudioSubtype::VndDotDtsDotHd,
    AudioSubtype::VndDotDtsDotUhd,
    AudioSubtype::VndDotDvbDotFile,
    AudioSubtype::VndDotEveradDotPlj,
    AudioSubtype::VndDotHnsDotAudio,
    AudioSubtype::VndDotLucentDotVoice,
    AudioSubtype::VndDotMsPlayreadyDotMediaDotPya,
    AudioSubtype::VndDotNokiaDotMobileXmf,
    AudioSubtype::VndDotNortelDotVbk,
    AudioSubtype::VndDotNueraDotEcelp4800,
    AudioSubtype::VndDotNueraDotEcelp7470,
    AudioSubtype::VndDotNueraDotEcelp9600,
    AudioSubtype::VndDotOctelDotSbc,
    AudioSubtype::VndDotPresonusDotMultitrack,
    AudioSubtype::VndDotQcelpDeprecatedInFavorOfAudioQcelp,
    AudioSubtype::VndDotRhetorexDot32Kadpcm,
    AudioSubtype::VndDotRip,
    AudioSubtype::VndDotSealedmediaDotSoftsealDotMpeg,
    AudioSubtype::VndDotVmxDotCvsd,
    AudioSubtype::Vorbis,
    AudioSubtype::VorbisConfig,
];
const AUDIO_SUBTYPE_SLIST: &[&str] = &[
    "1d-interleaved-parityfec",
    "32kadpcm",
    "3gpp",
    "3gpp2",
    "aac",
    "ac3",
    "AMR",
    "AMR-WB",
    "amr-wb+",
    "aptx",
    "asc",
    "ATRAC-ADVANCED-LOSSLESS",
    "ATRAC-X",
    "ATRAC3",
    "basic",
    "BV16",
    "BV32",
    "clearmode",
    "CN",
    "DAT12",
    "dls",
    "dsr-es201108",
    "dsr-es202050",
    "dsr-es202211",
    "dsr-es202212",
    "DV",
    "DVI4",
    "eac3",
    "encaprtp",
    "EVRC",
    "EVRC-QCP",
    "EVRC0",
    "EVRC1",
    "EVRCB",
    "EVRCB0",
    "EVRCB1",
    "EVRCNW",
    "EVRCNW0",
    "EVRCNW1",
    "EVRCWB",
    "EVRCWB0",
    "EVRCWB1",
    "EVS",
    "example",
    "flac",
    "flexfec",
    "fwdred",
    "G711-0",
    "G719",
    "G722",
    "G7221",
    "G723",
    "G726-16",
    "G726-24",
    "G726-32",
    "G726-40",
    "G728",
    "G729",
    "G7291",
    "G729D",
    "G729E",
    "GSM",
    "GSM-EFR",
    "GSM-HR-08",
    "iLBC",
    "ip-mr_v2.5",
    "L16",
    "L20",
    "L24",
    "L8",
    "LPC",
    "matroska",
    "MELP",
    "MELP1200",
    "MELP2400",
    "MELP600",
    "mhas",
    "midi-clip",
    "mobile-xmf",
    "mp4",
    "MP4A-LATM",
    "MPA",
    "mpa-robust",
    "mpeg",
    "mpeg4-generic",
    "ogg",
    "opus",
    "parityfec",
    "PCMA",
    "PCMA-WB",
    "PCMU",
    "PCMU-WB",
    "prs.sid",
    "QCELP",
    "raptorfec",
    "RED",
    "rtp-enc-aescm128",
    "rtp-midi",
    "rtploopback",
    "rtx",
    "scip",
    "SMV",
    "SMV-QCP",
    "SMV0",
    "sofa",
    "soundfont",
    "sp-midi",
    "speex",
    "t140c",
    "t38",
    "telephone-event",
    "TETRA_ACELP",
    "TETRA_ACELP_BB",
    "tone",
    "TSVCIS",
    "UEMCLIP",
    "ulpfec",
    "usac",
    "VDVI",
    "VMR-WB",
    "vnd.3gpp.iufp",
    "vnd.4SB",
    "vnd.audiokoz",
    "vnd.blockfact.facta",
    "vnd.CELP",
    "vnd.cisco.nse",
    "vnd.cmles.radio-events",
    "vnd.cns.anp1",
    "vnd.cns.inf1",
    "vnd.dece.audio",
    "vnd.digital-winds",
    "vnd.dlna.adts",
    "vnd.dolby.heaac.1",
    "vnd.dolby.heaac.2",
    "vnd.dolby.mlp",
    "vnd.dolby.mps",
    "vnd.dolby.pl2",
    "vnd.dolby.pl2x",
    "vnd.dolby.pl2z",
    "vnd.dolby.pulse.1",
    "vnd.dra",
    "vnd.dts",
    "vnd.dts.hd",
    "vnd.dts.uhd",
    "vnd.dvb.file",
    "vnd.everad.plj",
    "vnd.hns.audio",
    "vnd.lucent.voice",
    "vnd.ms-playready.media.pya",
    "vnd.nokia.mobile-xmf",
    "vnd.nortel.vbk",
    "vnd.nuera.ecelp4800",
    "vnd.nuera.ecelp7470",
    "vnd.nuera.ecelp9600",
    "vnd.octel.sbc",
    "vnd.presonus.multitrack",
    "vnd.qcelp",
    "vnd.rhetorex.32kadpcm",
    "vnd.rip",
    "vnd.sealedmedia.softseal.mpeg",
    "vnd.vmx.cvsd",
    "vorbis",
    "vorbis-config",
];
impl AudioSubtype {
    pub fn to_str(&self) -> &str {
        if let Self::Private(sub) = self {
            return sub.to_str();
        }
        let p = AUDIO_SUBTYPE_VLIST.binary_search(self).unwrap();
        AUDIO_SUBTYPE_SLIST[p]
    }
}
impl FromStr for AudioSubtype {
    type Err = MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_ascii_lowercase();
        if let Ok(pos) =
            AUDIO_SUBTYPE_SLIST.binary_search_by_key(&lower, |t| t.to_ascii_lowercase())
        {
            return Ok(AUDIO_SUBTYPE_VLIST[pos].clone());
        }
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExampleSubtype {
    /// private
    Private(PrivateSubtype),
}
impl ExampleSubtype {
    pub fn to_str(&self) -> &str {
        match self {
            Self::Private(sub) => sub.to_str(),
        }
    }
}
impl FromStr for ExampleSubtype {
    type Err = crate::MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FontSubtype {
    /// collection
    Collection,
    /// otf
    Otf,
    /// sfnt
    Sfnt,
    /// ttf
    Ttf,
    /// woff
    Woff,
    /// woff2
    Woff2,
    /// private
    Private(PrivateSubtype),
}
impl FontSubtype {
    pub fn to_str(&self) -> &str {
        match self {
            Self::Collection => "collection",
            Self::Otf => "otf",
            Self::Sfnt => "sfnt",
            Self::Ttf => "ttf",
            Self::Woff => "woff",
            Self::Woff2 => "woff2",
            Self::Private(sub) => sub.to_str(),
        }
    }
}
impl FromStr for FontSubtype {
    type Err = crate::MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("collection") {
            return Ok(Self::Collection);
        }
        if s.eq_ignore_ascii_case("otf") {
            return Ok(Self::Otf);
        }
        if s.eq_ignore_ascii_case("sfnt") {
            return Ok(Self::Sfnt);
        }
        if s.eq_ignore_ascii_case("ttf") {
            return Ok(Self::Ttf);
        }
        if s.eq_ignore_ascii_case("woff") {
            return Ok(Self::Woff);
        }
        if s.eq_ignore_ascii_case("woff2") {
            return Ok(Self::Woff2);
        }
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum HapticsSubtype {
    /// hjif
    Hjif,
    /// hmpg
    Hmpg,
    /// ivs
    Ivs,
    /// private
    Private(PrivateSubtype),
}
impl HapticsSubtype {
    pub fn to_str(&self) -> &str {
        match self {
            Self::Hjif => "hjif",
            Self::Hmpg => "hmpg",
            Self::Ivs => "ivs",
            Self::Private(sub) => sub.to_str(),
        }
    }
}
impl FromStr for HapticsSubtype {
    type Err = crate::MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("hjif") {
            return Ok(Self::Hjif);
        }
        if s.eq_ignore_ascii_case("hmpg") {
            return Ok(Self::Hmpg);
        }
        if s.eq_ignore_ascii_case("ivs") {
            return Ok(Self::Ivs);
        }
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ImageSubtype {
    /// aces
    Aces,
    /// apng
    Apng,
    /// avci
    Avci,
    /// avcs
    Avcs,
    /// avif
    Avif,
    /// bmp
    Bmp,
    /// cgm
    Cgm,
    /// dicom-rle
    DicomRle,
    /// dpx
    Dpx,
    /// emf
    Emf,
    /// example
    Example,
    /// fits
    Fits,
    /// g3fax
    G3Fax,
    /// gif
    Gif,
    /// heic
    Heic,
    /// heic-sequence
    HeicSequence,
    /// heif
    Heif,
    /// heif-sequence
    HeifSequence,
    /// hej2k
    Hej2K,
    /// hsj2
    Hsj2,
    /// ief
    Ief,
    /// j2c
    J2C,
    /// jaii
    Jaii,
    /// jais
    Jais,
    /// jls
    Jls,
    /// jp2
    Jp2,
    /// jpeg
    Jpeg,
    /// jph
    Jph,
    /// jphc
    Jphc,
    /// jpm
    Jpm,
    /// jpx
    Jpx,
    /// jxl
    Jxl,
    /// jxr
    Jxr,
    /// jxrA
    JxrA,
    /// jxrS
    JxrS,
    /// jxs
    Jxs,
    /// jxsc
    Jxsc,
    /// jxsi
    Jxsi,
    /// jxss
    Jxss,
    /// ktx
    Ktx,
    /// ktx2
    Ktx2,
    /// naplps
    Naplps,
    /// png
    Png,
    /// prs.btif
    PrsDotBtif,
    /// prs.pti
    PrsDotPti,
    /// pwg-raster
    PwgRaster,
    /// svg+xml
    SvgPlusXml,
    /// t38
    T38,
    /// tiff
    Tiff,
    /// tiff-fx
    TiffFx,
    /// vnd.adobe.photoshop
    VndDotAdobeDotPhotoshop,
    /// vnd.airzip.accelerator.azv
    VndDotAirzipDotAcceleratorDotAzv,
    /// vnd.blockfact.facti
    VndDotBlockfactDotFacti,
    /// vnd.clip
    VndDotClip,
    /// vnd.cns.inf2
    VndDotCnsDotInf2,
    /// vnd.dece.graphic
    VndDotDeceDotGraphic,
    /// vnd.djvu
    VndDotDjvu,
    /// vnd.dvb.subtitle
    VndDotDvbDotSubtitle,
    /// vnd.dwg
    VndDotDwg,
    /// vnd.dxf
    VndDotDxf,
    /// vnd.fastbidsheet
    VndDotFastbidsheet,
    /// vnd.fpx
    VndDotFpx,
    /// vnd.fst
    VndDotFst,
    /// vnd.fujixerox.edmics-mmr
    VndDotFujixeroxDotEdmicsMmr,
    /// vnd.fujixerox.edmics-rlc
    VndDotFujixeroxDotEdmicsRlc,
    /// vnd.globalgraphics.pgb
    VndDotGlobalgraphicsDotPgb,
    /// vnd.microsoft.icon
    VndDotMicrosoftDotIcon,
    /// vnd.mix
    VndDotMix,
    /// vnd.mozilla.apng
    VndDotMozillaDotApng,
    /// vnd.ms-modi
    VndDotMsModi,
    /// vnd.net-fpx
    VndDotNetFpx,
    /// vnd.pco.b16
    VndDotPcoDotB16,
    /// vnd.radiance
    VndDotRadiance,
    /// vnd.sealed.png
    VndDotSealedDotPng,
    /// vnd.sealedmedia.softseal.gif
    VndDotSealedmediaDotSoftsealDotGif,
    /// vnd.sealedmedia.softseal.jpg
    VndDotSealedmediaDotSoftsealDotJpg,
    /// vnd.sld
    VndDotSld,
    /// vnd.svf
    VndDotSvf,
    /// vnd.tencent.tap
    VndDotTencentDotTap,
    /// vnd.valve.source.texture
    VndDotValveDotSourceDotTexture,
    /// vnd.wap.wbmp
    VndDotWapDotWbmp,
    /// vnd.xiff
    VndDotXiff,
    /// vnd.zbrush.pcx
    VndDotZbrushDotPcx,
    /// webp
    Webp,
    /// wmf
    Wmf,
    /// x-emf
    XEmfDeprecatedInFavorOfImageEmf,
    /// x-wmf
    XWmfDeprecatedInFavorOfImageWmf,
    /// private
    Private(PrivateSubtype),
}
const IMAGE_SUBTYPE_VLIST: &[ImageSubtype] = &[
    ImageSubtype::Aces,
    ImageSubtype::Apng,
    ImageSubtype::Avci,
    ImageSubtype::Avcs,
    ImageSubtype::Avif,
    ImageSubtype::Bmp,
    ImageSubtype::Cgm,
    ImageSubtype::DicomRle,
    ImageSubtype::Dpx,
    ImageSubtype::Emf,
    ImageSubtype::Example,
    ImageSubtype::Fits,
    ImageSubtype::G3Fax,
    ImageSubtype::Gif,
    ImageSubtype::Heic,
    ImageSubtype::HeicSequence,
    ImageSubtype::Heif,
    ImageSubtype::HeifSequence,
    ImageSubtype::Hej2K,
    ImageSubtype::Hsj2,
    ImageSubtype::Ief,
    ImageSubtype::J2C,
    ImageSubtype::Jaii,
    ImageSubtype::Jais,
    ImageSubtype::Jls,
    ImageSubtype::Jp2,
    ImageSubtype::Jpeg,
    ImageSubtype::Jph,
    ImageSubtype::Jphc,
    ImageSubtype::Jpm,
    ImageSubtype::Jpx,
    ImageSubtype::Jxl,
    ImageSubtype::Jxr,
    ImageSubtype::JxrA,
    ImageSubtype::JxrS,
    ImageSubtype::Jxs,
    ImageSubtype::Jxsc,
    ImageSubtype::Jxsi,
    ImageSubtype::Jxss,
    ImageSubtype::Ktx,
    ImageSubtype::Ktx2,
    ImageSubtype::Naplps,
    ImageSubtype::Png,
    ImageSubtype::PrsDotBtif,
    ImageSubtype::PrsDotPti,
    ImageSubtype::PwgRaster,
    ImageSubtype::SvgPlusXml,
    ImageSubtype::T38,
    ImageSubtype::Tiff,
    ImageSubtype::TiffFx,
    ImageSubtype::VndDotAdobeDotPhotoshop,
    ImageSubtype::VndDotAirzipDotAcceleratorDotAzv,
    ImageSubtype::VndDotBlockfactDotFacti,
    ImageSubtype::VndDotClip,
    ImageSubtype::VndDotCnsDotInf2,
    ImageSubtype::VndDotDeceDotGraphic,
    ImageSubtype::VndDotDjvu,
    ImageSubtype::VndDotDvbDotSubtitle,
    ImageSubtype::VndDotDwg,
    ImageSubtype::VndDotDxf,
    ImageSubtype::VndDotFastbidsheet,
    ImageSubtype::VndDotFpx,
    ImageSubtype::VndDotFst,
    ImageSubtype::VndDotFujixeroxDotEdmicsMmr,
    ImageSubtype::VndDotFujixeroxDotEdmicsRlc,
    ImageSubtype::VndDotGlobalgraphicsDotPgb,
    ImageSubtype::VndDotMicrosoftDotIcon,
    ImageSubtype::VndDotMix,
    ImageSubtype::VndDotMozillaDotApng,
    ImageSubtype::VndDotMsModi,
    ImageSubtype::VndDotNetFpx,
    ImageSubtype::VndDotPcoDotB16,
    ImageSubtype::VndDotRadiance,
    ImageSubtype::VndDotSealedDotPng,
    ImageSubtype::VndDotSealedmediaDotSoftsealDotGif,
    ImageSubtype::VndDotSealedmediaDotSoftsealDotJpg,
    ImageSubtype::VndDotSld,
    ImageSubtype::VndDotSvf,
    ImageSubtype::VndDotTencentDotTap,
    ImageSubtype::VndDotValveDotSourceDotTexture,
    ImageSubtype::VndDotWapDotWbmp,
    ImageSubtype::VndDotXiff,
    ImageSubtype::VndDotZbrushDotPcx,
    ImageSubtype::Webp,
    ImageSubtype::Wmf,
    ImageSubtype::XEmfDeprecatedInFavorOfImageEmf,
    ImageSubtype::XWmfDeprecatedInFavorOfImageWmf,
];
const IMAGE_SUBTYPE_SLIST: &[&str] = &[
    "aces",
    "apng",
    "avci",
    "avcs",
    "avif",
    "bmp",
    "cgm",
    "dicom-rle",
    "dpx",
    "emf",
    "example",
    "fits",
    "g3fax",
    "gif",
    "heic",
    "heic-sequence",
    "heif",
    "heif-sequence",
    "hej2k",
    "hsj2",
    "ief",
    "j2c",
    "jaii",
    "jais",
    "jls",
    "jp2",
    "jpeg",
    "jph",
    "jphc",
    "jpm",
    "jpx",
    "jxl",
    "jxr",
    "jxrA",
    "jxrS",
    "jxs",
    "jxsc",
    "jxsi",
    "jxss",
    "ktx",
    "ktx2",
    "naplps",
    "png",
    "prs.btif",
    "prs.pti",
    "pwg-raster",
    "svg+xml",
    "t38",
    "tiff",
    "tiff-fx",
    "vnd.adobe.photoshop",
    "vnd.airzip.accelerator.azv",
    "vnd.blockfact.facti",
    "vnd.clip",
    "vnd.cns.inf2",
    "vnd.dece.graphic",
    "vnd.djvu",
    "vnd.dvb.subtitle",
    "vnd.dwg",
    "vnd.dxf",
    "vnd.fastbidsheet",
    "vnd.fpx",
    "vnd.fst",
    "vnd.fujixerox.edmics-mmr",
    "vnd.fujixerox.edmics-rlc",
    "vnd.globalgraphics.pgb",
    "vnd.microsoft.icon",
    "vnd.mix",
    "vnd.mozilla.apng",
    "vnd.ms-modi",
    "vnd.net-fpx",
    "vnd.pco.b16",
    "vnd.radiance",
    "vnd.sealed.png",
    "vnd.sealedmedia.softseal.gif",
    "vnd.sealedmedia.softseal.jpg",
    "vnd.sld",
    "vnd.svf",
    "vnd.tencent.tap",
    "vnd.valve.source.texture",
    "vnd.wap.wbmp",
    "vnd.xiff",
    "vnd.zbrush.pcx",
    "webp",
    "wmf",
    "x-emf",
    "x-wmf",
];
impl ImageSubtype {
    pub fn to_str(&self) -> &str {
        if let Self::Private(sub) = self {
            return sub.to_str();
        }
        let p = IMAGE_SUBTYPE_VLIST.binary_search(self).unwrap();
        IMAGE_SUBTYPE_SLIST[p]
    }
}
impl FromStr for ImageSubtype {
    type Err = MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_ascii_lowercase();
        if let Ok(pos) =
            IMAGE_SUBTYPE_SLIST.binary_search_by_key(&lower, |t| t.to_ascii_lowercase())
        {
            return Ok(IMAGE_SUBTYPE_VLIST[pos].clone());
        }
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MessageSubtype {
    /// bhttp
    Bhttp,
    /// CPIM
    Cpim,
    /// delivery-status
    DeliveryStatus,
    /// disposition-notification
    DispositionNotification,
    /// example
    Example,
    /// external-body
    ExternalBody,
    /// feedback-report
    FeedbackReport,
    /// global
    Global,
    /// global-delivery-status
    GlobalDeliveryStatus,
    /// global-disposition-notification
    GlobalDispositionNotification,
    /// global-headers
    GlobalHeaders,
    /// http
    Http,
    /// imdn+xml
    ImdnPlusXml,
    /// mls
    Mls,
    /// news
    News,
    /// ohttp-chunked-req
    OhttpChunkedReq,
    /// ohttp-chunked-res
    OhttpChunkedRes,
    /// ohttp-req
    OhttpReq,
    /// ohttp-res
    OhttpRes,
    /// partial
    Partial,
    /// rfc822
    Rfc822,
    /// s-http
    SHttp,
    /// sip
    Sip,
    /// sipfrag
    Sipfrag,
    /// tracking-status
    TrackingStatus,
    /// vnd.si.simp
    VndDotSiDotSimp,
    /// vnd.wfa.wsc
    VndDotWfaDotWsc,
    /// private
    Private(PrivateSubtype),
}
const MESSAGE_SUBTYPE_VLIST: &[MessageSubtype] = &[
    MessageSubtype::Bhttp,
    MessageSubtype::Cpim,
    MessageSubtype::DeliveryStatus,
    MessageSubtype::DispositionNotification,
    MessageSubtype::Example,
    MessageSubtype::ExternalBody,
    MessageSubtype::FeedbackReport,
    MessageSubtype::Global,
    MessageSubtype::GlobalDeliveryStatus,
    MessageSubtype::GlobalDispositionNotification,
    MessageSubtype::GlobalHeaders,
    MessageSubtype::Http,
    MessageSubtype::ImdnPlusXml,
    MessageSubtype::Mls,
    MessageSubtype::News,
    MessageSubtype::OhttpChunkedReq,
    MessageSubtype::OhttpChunkedRes,
    MessageSubtype::OhttpReq,
    MessageSubtype::OhttpRes,
    MessageSubtype::Partial,
    MessageSubtype::Rfc822,
    MessageSubtype::SHttp,
    MessageSubtype::Sip,
    MessageSubtype::Sipfrag,
    MessageSubtype::TrackingStatus,
    MessageSubtype::VndDotSiDotSimp,
    MessageSubtype::VndDotWfaDotWsc,
];
const MESSAGE_SUBTYPE_SLIST: &[&str] = &[
    "bhttp",
    "CPIM",
    "delivery-status",
    "disposition-notification",
    "example",
    "external-body",
    "feedback-report",
    "global",
    "global-delivery-status",
    "global-disposition-notification",
    "global-headers",
    "http",
    "imdn+xml",
    "mls",
    "news",
    "ohttp-chunked-req",
    "ohttp-chunked-res",
    "ohttp-req",
    "ohttp-res",
    "partial",
    "rfc822",
    "s-http",
    "sip",
    "sipfrag",
    "tracking-status",
    "vnd.si.simp",
    "vnd.wfa.wsc",
];
impl MessageSubtype {
    pub fn to_str(&self) -> &str {
        if let Self::Private(sub) = self {
            return sub.to_str();
        }
        let p = MESSAGE_SUBTYPE_VLIST.binary_search(self).unwrap();
        MESSAGE_SUBTYPE_SLIST[p]
    }
}
impl FromStr for MessageSubtype {
    type Err = MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_ascii_lowercase();
        if let Ok(pos) =
            MESSAGE_SUBTYPE_SLIST.binary_search_by_key(&lower, |t| t.to_ascii_lowercase())
        {
            return Ok(MESSAGE_SUBTYPE_VLIST[pos].clone());
        }
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ModelSubtype {
    /// 3mf
    _3Mf,
    /// e57
    E57,
    /// example
    Example,
    /// gltf+json
    GltfPlusJson,
    /// gltf-binary
    GltfBinary,
    /// iges
    Iges,
    /// JT
    Jt,
    /// mesh
    Mesh,
    /// mtl
    Mtl,
    /// obj
    Obj,
    /// prc
    Prc,
    /// step
    Step,
    /// step+xml
    StepPlusXml,
    /// step+zip
    StepPlusZip,
    /// step-xml+zip
    StepXmlPlusZip,
    /// stl
    Stl,
    /// u3d
    U3D,
    /// vnd.bary
    VndDotBary,
    /// vnd.cld
    VndDotCld,
    /// vnd.collada+xml
    VndDotColladaPlusXml,
    /// vnd.dwf
    VndDotDwf,
    /// vnd.flatland.3dml
    VndDotFlatlandDot3Dml,
    /// vnd.gdl
    VndDotGdl,
    /// vnd.gs-gdl
    VndDotGsGdl,
    /// vnd.gtw
    VndDotGtw,
    /// vnd.moml+xml
    VndDotMomlPlusXml,
    /// vnd.mts
    VndDotMts,
    /// vnd.opengex
    VndDotOpengex,
    /// vnd.parasolid.transmit.binary
    VndDotParasolidDotTransmitDotBinary,
    /// vnd.parasolid.transmit.text
    VndDotParasolidDotTransmitDotText,
    /// vnd.pytha.pyox
    VndDotPythaDotPyox,
    /// vnd.rosette.annotated-data-model
    VndDotRosetteDotAnnotatedDataModel,
    /// vnd.sap.vds
    VndDotSapDotVds,
    /// vnd.usda
    VndDotUsda,
    /// vnd.usdz+zip
    VndDotUsdzPlusZip,
    /// vnd.valve.source.compiled-map
    VndDotValveDotSourceDotCompiledMap,
    /// vnd.vtu
    VndDotVtu,
    /// vrml
    Vrml,
    /// x3d+fastinfoset
    X3DPlusFastinfoset,
    /// x3d+xml
    X3DPlusXml,
    /// x3d-vrml
    X3DVrml,
    /// private
    Private(PrivateSubtype),
}
const MODEL_SUBTYPE_VLIST: &[ModelSubtype] = &[
    ModelSubtype::_3Mf,
    ModelSubtype::E57,
    ModelSubtype::Example,
    ModelSubtype::GltfPlusJson,
    ModelSubtype::GltfBinary,
    ModelSubtype::Iges,
    ModelSubtype::Jt,
    ModelSubtype::Mesh,
    ModelSubtype::Mtl,
    ModelSubtype::Obj,
    ModelSubtype::Prc,
    ModelSubtype::Step,
    ModelSubtype::StepPlusXml,
    ModelSubtype::StepPlusZip,
    ModelSubtype::StepXmlPlusZip,
    ModelSubtype::Stl,
    ModelSubtype::U3D,
    ModelSubtype::VndDotBary,
    ModelSubtype::VndDotCld,
    ModelSubtype::VndDotColladaPlusXml,
    ModelSubtype::VndDotDwf,
    ModelSubtype::VndDotFlatlandDot3Dml,
    ModelSubtype::VndDotGdl,
    ModelSubtype::VndDotGsGdl,
    ModelSubtype::VndDotGtw,
    ModelSubtype::VndDotMomlPlusXml,
    ModelSubtype::VndDotMts,
    ModelSubtype::VndDotOpengex,
    ModelSubtype::VndDotParasolidDotTransmitDotBinary,
    ModelSubtype::VndDotParasolidDotTransmitDotText,
    ModelSubtype::VndDotPythaDotPyox,
    ModelSubtype::VndDotRosetteDotAnnotatedDataModel,
    ModelSubtype::VndDotSapDotVds,
    ModelSubtype::VndDotUsda,
    ModelSubtype::VndDotUsdzPlusZip,
    ModelSubtype::VndDotValveDotSourceDotCompiledMap,
    ModelSubtype::VndDotVtu,
    ModelSubtype::Vrml,
    ModelSubtype::X3DPlusFastinfoset,
    ModelSubtype::X3DPlusXml,
    ModelSubtype::X3DVrml,
];
const MODEL_SUBTYPE_SLIST: &[&str] = &[
    "3mf",
    "e57",
    "example",
    "gltf+json",
    "gltf-binary",
    "iges",
    "JT",
    "mesh",
    "mtl",
    "obj",
    "prc",
    "step",
    "step+xml",
    "step+zip",
    "step-xml+zip",
    "stl",
    "u3d",
    "vnd.bary",
    "vnd.cld",
    "vnd.collada+xml",
    "vnd.dwf",
    "vnd.flatland.3dml",
    "vnd.gdl",
    "vnd.gs-gdl",
    "vnd.gtw",
    "vnd.moml+xml",
    "vnd.mts",
    "vnd.opengex",
    "vnd.parasolid.transmit.binary",
    "vnd.parasolid.transmit.text",
    "vnd.pytha.pyox",
    "vnd.rosette.annotated-data-model",
    "vnd.sap.vds",
    "vnd.usda",
    "vnd.usdz+zip",
    "vnd.valve.source.compiled-map",
    "vnd.vtu",
    "vrml",
    "x3d+fastinfoset",
    "x3d+xml",
    "x3d-vrml",
];
impl ModelSubtype {
    pub fn to_str(&self) -> &str {
        if let Self::Private(sub) = self {
            return sub.to_str();
        }
        let p = MODEL_SUBTYPE_VLIST.binary_search(self).unwrap();
        MODEL_SUBTYPE_SLIST[p]
    }
}
impl FromStr for ModelSubtype {
    type Err = MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_ascii_lowercase();
        if let Ok(pos) =
            MODEL_SUBTYPE_SLIST.binary_search_by_key(&lower, |t| t.to_ascii_lowercase())
        {
            return Ok(MODEL_SUBTYPE_VLIST[pos].clone());
        }
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MultipartSubtype {
    /// alternative
    Alternative,
    /// appledouble
    Appledouble,
    /// byteranges
    Byteranges,
    /// digest
    Digest,
    /// encrypted
    Encrypted,
    /// example
    Example,
    /// form-data
    FormData,
    /// header-set
    HeaderSet,
    /// mixed
    Mixed,
    /// multilingual
    Multilingual,
    /// parallel
    Parallel,
    /// related
    Related,
    /// report
    Report,
    /// signed
    Signed,
    /// vnd.bint.med-plus
    VndDotBintDotMedPlus,
    /// voice-message
    VoiceMessage,
    /// x-mixed-replace
    XMixedReplace,
    /// private
    Private(PrivateSubtype),
}
impl MultipartSubtype {
    pub fn to_str(&self) -> &str {
        match self {
            Self::Alternative => "alternative",
            Self::Appledouble => "appledouble",
            Self::Byteranges => "byteranges",
            Self::Digest => "digest",
            Self::Encrypted => "encrypted",
            Self::Example => "example",
            Self::FormData => "form-data",
            Self::HeaderSet => "header-set",
            Self::Mixed => "mixed",
            Self::Multilingual => "multilingual",
            Self::Parallel => "parallel",
            Self::Related => "related",
            Self::Report => "report",
            Self::Signed => "signed",
            Self::VndDotBintDotMedPlus => "vnd.bint.med-plus",
            Self::VoiceMessage => "voice-message",
            Self::XMixedReplace => "x-mixed-replace",
            Self::Private(sub) => sub.to_str(),
        }
    }
}
impl FromStr for MultipartSubtype {
    type Err = crate::MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("alternative") {
            return Ok(Self::Alternative);
        }
        if s.eq_ignore_ascii_case("appledouble") {
            return Ok(Self::Appledouble);
        }
        if s.eq_ignore_ascii_case("byteranges") {
            return Ok(Self::Byteranges);
        }
        if s.eq_ignore_ascii_case("digest") {
            return Ok(Self::Digest);
        }
        if s.eq_ignore_ascii_case("encrypted") {
            return Ok(Self::Encrypted);
        }
        if s.eq_ignore_ascii_case("example") {
            return Ok(Self::Example);
        }
        if s.eq_ignore_ascii_case("form-data") {
            return Ok(Self::FormData);
        }
        if s.eq_ignore_ascii_case("header-set") {
            return Ok(Self::HeaderSet);
        }
        if s.eq_ignore_ascii_case("mixed") {
            return Ok(Self::Mixed);
        }
        if s.eq_ignore_ascii_case("multilingual") {
            return Ok(Self::Multilingual);
        }
        if s.eq_ignore_ascii_case("parallel") {
            return Ok(Self::Parallel);
        }
        if s.eq_ignore_ascii_case("related") {
            return Ok(Self::Related);
        }
        if s.eq_ignore_ascii_case("report") {
            return Ok(Self::Report);
        }
        if s.eq_ignore_ascii_case("signed") {
            return Ok(Self::Signed);
        }
        if s.eq_ignore_ascii_case("vnd.bint.med-plus") {
            return Ok(Self::VndDotBintDotMedPlus);
        }
        if s.eq_ignore_ascii_case("voice-message") {
            return Ok(Self::VoiceMessage);
        }
        if s.eq_ignore_ascii_case("x-mixed-replace") {
            return Ok(Self::XMixedReplace);
        }
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TextSubtype {
    /// 1d-interleaved-parityfec
    _1DInterleavedParityfec,
    /// cache-manifest
    CacheManifest,
    /// calendar
    Calendar,
    /// cql
    Cql,
    /// cql-expression
    CqlExpression,
    /// cql-identifier
    CqlIdentifier,
    /// css
    Css,
    /// csv
    Csv,
    /// csv-schema
    CsvSchema,
    /// directory
    DirectoryDeprecatedByRfc6350,
    /// dns
    Dns,
    /// ecmascript
    Ecmascript,
    /// encaprtp
    Encaprtp,
    /// enriched
    Enriched,
    /// example
    Example,
    /// fhirpath
    Fhirpath,
    /// flexfec
    Flexfec,
    /// fwdred
    Fwdred,
    /// gff3
    Gff3,
    /// grammar-ref-list
    GrammarRefList,
    /// hl7v2
    Hl7V2,
    /// html
    Html,
    /// javascript
    Javascript,
    /// jcr-cnd
    JcrCnd,
    /// markdown
    Markdown,
    /// mizar
    Mizar,
    /// n3
    N3,
    /// org
    Org,
    /// parameters
    Parameters,
    /// parityfec
    Parityfec,
    /// plain
    Plain,
    /// provenance-notation
    ProvenanceNotation,
    /// prs.fallenstein.rst
    PrsDotFallensteinDotRst,
    /// prs.lines.tag
    PrsDotLinesDotTag,
    /// prs.prop.logic
    PrsDotPropDotLogic,
    /// prs.texi
    PrsDotTexi,
    /// raptorfec
    Raptorfec,
    /// RED
    Red,
    /// rfc822-headers
    Rfc822Headers,
    /// richtext
    Richtext,
    /// rtf
    Rtf,
    /// rtp-enc-aescm128
    RtpEncAescm128,
    /// rtploopback
    Rtploopback,
    /// rtx
    Rtx,
    /// SGML
    Sgml,
    /// shaclc
    Shaclc,
    /// shex
    Shex,
    /// spdx
    Spdx,
    /// strings
    Strings,
    /// t140
    T140,
    /// tab-separated-values
    TabSeparatedValues,
    /// troff
    Troff,
    /// turtle
    Turtle,
    /// ulpfec
    Ulpfec,
    /// uri-list
    UriList,
    /// vcard
    Vcard,
    /// vnd.a
    VndDotA,
    /// vnd.abc
    VndDotAbc,
    /// vnd.ascii-art
    VndDotAsciiArt,
    /// vnd.curl
    VndDotCurl,
    /// vnd.debian.copyright
    VndDotDebianDotCopyright,
    /// vnd.DMClientScript
    VndDotDmClientScript,
    /// vnd.dvb.subtitle
    VndDotDvbDotSubtitle,
    /// vnd.esmertec.theme-descriptor
    VndDotEsmertecDotThemeDescriptor,
    /// vnd.exchangeable
    VndDotExchangeable,
    /// vnd.familysearch.gedcom
    VndDotFamilysearchDotGedcom,
    /// vnd.ficlab.flt
    VndDotFiclabDotFlt,
    /// vnd.fly
    VndDotFly,
    /// vnd.fmi.flexstor
    VndDotFmiDotFlexstor,
    /// vnd.gml
    VndDotGml,
    /// vnd.graphviz
    VndDotGraphviz,
    /// vnd.hans
    VndDotHans,
    /// vnd.hgl
    VndDotHgl,
    /// vnd.in3d.3dml
    VndDotIn3DDot3Dml,
    /// vnd.in3d.spot
    VndDotIn3DDotSpot,
    /// vnd.IPTC.NewsML
    VndDotIptcDotNewsMl,
    /// vnd.IPTC.NITF
    VndDotIptcDotNitf,
    /// vnd.latex-z
    VndDotLatexZ,
    /// vnd.longform
    VndDotLongform,
    /// vnd.motorola.reflex
    VndDotMotorolaDotReflex,
    /// vnd.ms-mediapackage
    VndDotMsMediapackage,
    /// vnd.net2phone.commcenter.command
    VndDotNet2PhoneDotCommcenterDotCommand,
    /// vnd.radisys.msml-basic-layout
    VndDotRadisysDotMsmlBasicLayout,
    /// vnd.senx.warpscript
    VndDotSenxDotWarpscript,
    /// vnd.si.uricatalogue
    VndDotSiDotUricatalogue,
    /// vnd.sosi
    VndDotSosi,
    /// vnd.sun.j2me.app-descriptor
    VndDotSunDotJ2MeDotAppDescriptor,
    /// vnd.tps
    VndDotTps,
    /// vnd.trolltech.linguist
    VndDotTrolltechDotLinguist,
    /// vnd.typst
    VndDotTypst,
    /// vnd.vcf
    VndDotVcf,
    /// vnd.vri
    VndDotVri,
    /// vnd.wap.si
    VndDotWapDotSi,
    /// vnd.wap.sl
    VndDotWapDotSl,
    /// vnd.wap.wml
    VndDotWapDotWml,
    /// vnd.wap.wmlscript
    VndDotWapDotWmlscript,
    /// vnd.zoo.kcl
    VndDotZooDotKcl,
    /// vtt
    Vtt,
    /// wgsl
    Wgsl,
    /// xml
    Xml,
    /// xml-external-parsed-entity
    XmlExternalParsedEntity,
    /// private
    Private(PrivateSubtype),
}
const TEXT_SUBTYPE_VLIST: &[TextSubtype] = &[
    TextSubtype::_1DInterleavedParityfec,
    TextSubtype::CacheManifest,
    TextSubtype::Calendar,
    TextSubtype::Cql,
    TextSubtype::CqlExpression,
    TextSubtype::CqlIdentifier,
    TextSubtype::Css,
    TextSubtype::Csv,
    TextSubtype::CsvSchema,
    TextSubtype::DirectoryDeprecatedByRfc6350,
    TextSubtype::Dns,
    TextSubtype::Ecmascript,
    TextSubtype::Encaprtp,
    TextSubtype::Enriched,
    TextSubtype::Example,
    TextSubtype::Fhirpath,
    TextSubtype::Flexfec,
    TextSubtype::Fwdred,
    TextSubtype::Gff3,
    TextSubtype::GrammarRefList,
    TextSubtype::Hl7V2,
    TextSubtype::Html,
    TextSubtype::Javascript,
    TextSubtype::JcrCnd,
    TextSubtype::Markdown,
    TextSubtype::Mizar,
    TextSubtype::N3,
    TextSubtype::Org,
    TextSubtype::Parameters,
    TextSubtype::Parityfec,
    TextSubtype::Plain,
    TextSubtype::ProvenanceNotation,
    TextSubtype::PrsDotFallensteinDotRst,
    TextSubtype::PrsDotLinesDotTag,
    TextSubtype::PrsDotPropDotLogic,
    TextSubtype::PrsDotTexi,
    TextSubtype::Raptorfec,
    TextSubtype::Red,
    TextSubtype::Rfc822Headers,
    TextSubtype::Richtext,
    TextSubtype::Rtf,
    TextSubtype::RtpEncAescm128,
    TextSubtype::Rtploopback,
    TextSubtype::Rtx,
    TextSubtype::Sgml,
    TextSubtype::Shaclc,
    TextSubtype::Shex,
    TextSubtype::Spdx,
    TextSubtype::Strings,
    TextSubtype::T140,
    TextSubtype::TabSeparatedValues,
    TextSubtype::Troff,
    TextSubtype::Turtle,
    TextSubtype::Ulpfec,
    TextSubtype::UriList,
    TextSubtype::Vcard,
    TextSubtype::VndDotA,
    TextSubtype::VndDotAbc,
    TextSubtype::VndDotAsciiArt,
    TextSubtype::VndDotCurl,
    TextSubtype::VndDotDebianDotCopyright,
    TextSubtype::VndDotDmClientScript,
    TextSubtype::VndDotDvbDotSubtitle,
    TextSubtype::VndDotEsmertecDotThemeDescriptor,
    TextSubtype::VndDotExchangeable,
    TextSubtype::VndDotFamilysearchDotGedcom,
    TextSubtype::VndDotFiclabDotFlt,
    TextSubtype::VndDotFly,
    TextSubtype::VndDotFmiDotFlexstor,
    TextSubtype::VndDotGml,
    TextSubtype::VndDotGraphviz,
    TextSubtype::VndDotHans,
    TextSubtype::VndDotHgl,
    TextSubtype::VndDotIn3DDot3Dml,
    TextSubtype::VndDotIn3DDotSpot,
    TextSubtype::VndDotIptcDotNewsMl,
    TextSubtype::VndDotIptcDotNitf,
    TextSubtype::VndDotLatexZ,
    TextSubtype::VndDotLongform,
    TextSubtype::VndDotMotorolaDotReflex,
    TextSubtype::VndDotMsMediapackage,
    TextSubtype::VndDotNet2PhoneDotCommcenterDotCommand,
    TextSubtype::VndDotRadisysDotMsmlBasicLayout,
    TextSubtype::VndDotSenxDotWarpscript,
    TextSubtype::VndDotSiDotUricatalogue,
    TextSubtype::VndDotSosi,
    TextSubtype::VndDotSunDotJ2MeDotAppDescriptor,
    TextSubtype::VndDotTps,
    TextSubtype::VndDotTrolltechDotLinguist,
    TextSubtype::VndDotTypst,
    TextSubtype::VndDotVcf,
    TextSubtype::VndDotVri,
    TextSubtype::VndDotWapDotSi,
    TextSubtype::VndDotWapDotSl,
    TextSubtype::VndDotWapDotWml,
    TextSubtype::VndDotWapDotWmlscript,
    TextSubtype::VndDotZooDotKcl,
    TextSubtype::Vtt,
    TextSubtype::Wgsl,
    TextSubtype::Xml,
    TextSubtype::XmlExternalParsedEntity,
];
const TEXT_SUBTYPE_SLIST: &[&str] = &[
    "1d-interleaved-parityfec",
    "cache-manifest",
    "calendar",
    "cql",
    "cql-expression",
    "cql-identifier",
    "css",
    "csv",
    "csv-schema",
    "directory",
    "dns",
    "ecmascript",
    "encaprtp",
    "enriched",
    "example",
    "fhirpath",
    "flexfec",
    "fwdred",
    "gff3",
    "grammar-ref-list",
    "hl7v2",
    "html",
    "javascript",
    "jcr-cnd",
    "markdown",
    "mizar",
    "n3",
    "org",
    "parameters",
    "parityfec",
    "plain",
    "provenance-notation",
    "prs.fallenstein.rst",
    "prs.lines.tag",
    "prs.prop.logic",
    "prs.texi",
    "raptorfec",
    "RED",
    "rfc822-headers",
    "richtext",
    "rtf",
    "rtp-enc-aescm128",
    "rtploopback",
    "rtx",
    "SGML",
    "shaclc",
    "shex",
    "spdx",
    "strings",
    "t140",
    "tab-separated-values",
    "troff",
    "turtle",
    "ulpfec",
    "uri-list",
    "vcard",
    "vnd.a",
    "vnd.abc",
    "vnd.ascii-art",
    "vnd.curl",
    "vnd.debian.copyright",
    "vnd.DMClientScript",
    "vnd.dvb.subtitle",
    "vnd.esmertec.theme-descriptor",
    "vnd.exchangeable",
    "vnd.familysearch.gedcom",
    "vnd.ficlab.flt",
    "vnd.fly",
    "vnd.fmi.flexstor",
    "vnd.gml",
    "vnd.graphviz",
    "vnd.hans",
    "vnd.hgl",
    "vnd.in3d.3dml",
    "vnd.in3d.spot",
    "vnd.IPTC.NewsML",
    "vnd.IPTC.NITF",
    "vnd.latex-z",
    "vnd.longform",
    "vnd.motorola.reflex",
    "vnd.ms-mediapackage",
    "vnd.net2phone.commcenter.command",
    "vnd.radisys.msml-basic-layout",
    "vnd.senx.warpscript",
    "vnd.si.uricatalogue",
    "vnd.sosi",
    "vnd.sun.j2me.app-descriptor",
    "vnd.tps",
    "vnd.trolltech.linguist",
    "vnd.typst",
    "vnd.vcf",
    "vnd.vri",
    "vnd.wap.si",
    "vnd.wap.sl",
    "vnd.wap.wml",
    "vnd.wap.wmlscript",
    "vnd.zoo.kcl",
    "vtt",
    "wgsl",
    "xml",
    "xml-external-parsed-entity",
];
impl TextSubtype {
    pub fn to_str(&self) -> &str {
        if let Self::Private(sub) = self {
            return sub.to_str();
        }
        let p = TEXT_SUBTYPE_VLIST.binary_search(self).unwrap();
        TEXT_SUBTYPE_SLIST[p]
    }
}
impl FromStr for TextSubtype {
    type Err = MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_ascii_lowercase();
        if let Ok(pos) = TEXT_SUBTYPE_SLIST.binary_search_by_key(&lower, |t| t.to_ascii_lowercase())
        {
            return Ok(TEXT_SUBTYPE_VLIST[pos].clone());
        }
        Ok(Self::Private(s.parse()?))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum VideoSubtype {
    /// 1d-interleaved-parityfec
    _1DInterleavedParityfec,
    /// 3gpp
    _3Gpp,
    /// 3gpp-tt
    _3GppTt,
    /// 3gpp2
    _3Gpp2,
    /// AV1
    Av1,
    /// BMPEG
    Bmpeg,
    /// BT656
    Bt656,
    /// CelB
    CelB,
    /// DV
    Dv,
    /// encaprtp
    Encaprtp,
    /// evc
    Evc,
    /// example
    Example,
    /// FFV1
    Ffv1,
    /// flexfec
    Flexfec,
    /// H261
    H261,
    /// H263
    H263,
    /// H263-1998
    H2631998,
    /// H263-2000
    H2632000,
    /// H264
    H264,
    /// H264-RCDO
    H264Rcdo,
    /// H264-SVC
    H264Svc,
    /// H265
    H265,
    /// H266
    H266,
    /// iso.segment
    IsoDotSegment,
    /// JPEG
    Jpeg,
    /// jpeg2000
    Jpeg2000,
    /// jpeg2000-scl
    Jpeg2000Scl,
    /// jxsv
    Jxsv,
    /// lottie+json
    LottiePlusJson,
    /// matroska
    Matroska,
    /// matroska-3d
    Matroska3D,
    /// mj2
    Mj2,
    /// MP1S
    Mp1S,
    /// MP2P
    Mp2P,
    /// MP2T
    Mp2T,
    /// mp4
    Mp4,
    /// MP4V-ES
    Mp4VEs,
    /// mpeg
    Mpeg,
    /// mpeg4-generic
    Mpeg4Generic,
    /// MPV
    Mpv,
    /// nv
    Nv,
    /// ogg
    Ogg,
    /// parityfec
    Parityfec,
    /// pointer
    Pointer,
    /// quicktime
    Quicktime,
    /// raptorfec
    Raptorfec,
    /// raw
    Raw,
    /// rtp-enc-aescm128
    RtpEncAescm128,
    /// rtploopback
    Rtploopback,
    /// rtx
    Rtx,
    /// scip
    Scip,
    /// smpte291
    Smpte291,
    /// SMPTE292M
    Smpte292M,
    /// ulpfec
    Ulpfec,
    /// vc1
    Vc1,
    /// vc2
    Vc2,
    /// vnd.blockfact.factv
    VndDotBlockfactDotFactv,
    /// vnd.CCTV
    VndDotCctv,
    /// vnd.dece.hd
    VndDotDeceDotHd,
    /// vnd.dece.mobile
    VndDotDeceDotMobile,
    /// vnd.dece.mp4
    VndDotDeceDotMp4,
    /// vnd.dece.pd
    VndDotDeceDotPd,
    /// vnd.dece.sd
    VndDotDeceDotSd,
    /// vnd.dece.video
    VndDotDeceDotVideo,
    /// vnd.directv.mpeg
    VndDotDirectvDotMpeg,
    /// vnd.directv.mpeg-tts
    VndDotDirectvDotMpegTts,
    /// vnd.dlna.mpeg-tts
    VndDotDlnaDotMpegTts,
    /// vnd.dvb.file
    VndDotDvbDotFile,
    /// vnd.fvt
    VndDotFvt,
    /// vnd.hns.video
    VndDotHnsDotVideo,
    /// vnd.iptvforum.1dparityfec-1010
    VndDotIptvforumDot1Dparityfec1010,
    /// vnd.iptvforum.1dparityfec-2005
    VndDotIptvforumDot1Dparityfec2005,
    /// vnd.iptvforum.2dparityfec-1010
    VndDotIptvforumDot2Dparityfec1010,
    /// vnd.iptvforum.2dparityfec-2005
    VndDotIptvforumDot2Dparityfec2005,
    /// vnd.iptvforum.ttsavc
    VndDotIptvforumDotTtsavc,
    /// vnd.iptvforum.ttsmpeg2
    VndDotIptvforumDotTtsmpeg2,
    /// vnd.motorola.video
    VndDotMotorolaDotVideo,
    /// vnd.motorola.videop
    VndDotMotorolaDotVideop,
    /// vnd.mpegurl
    VndDotMpegurl,
    /// vnd.ms-playready.media.pyv
    VndDotMsPlayreadyDotMediaDotPyv,
    /// vnd.nokia.interleaved-multimedia
    VndDotNokiaDotInterleavedMultimedia,
    /// vnd.nokia.mp4vr
    VndDotNokiaDotMp4Vr,
    /// vnd.nokia.videovoip
    VndDotNokiaDotVideovoip,
    /// vnd.objectvideo
    VndDotObjectvideo,
    /// vnd.planar
    VndDotPlanar,
    /// vnd.radgamettools.bink
    VndDotRadgamettoolsDotBink,
    /// vnd.radgamettools.smacker
    VndDotRadgamettoolsDotSmacker,
    /// vnd.sealed.mpeg1
    VndDotSealedDotMpeg1,
    /// vnd.sealed.mpeg4
    VndDotSealedDotMpeg4,
    /// vnd.sealed.swf
    VndDotSealedDotSwf,
    /// vnd.sealedmedia.softseal.mov
    VndDotSealedmediaDotSoftsealDotMov,
    /// vnd.uvvu.mp4
    VndDotUvvuDotMp4,
    /// vnd.vivo
    VndDotVivo,
    /// vnd.youtube.yt
    VndDotYoutubeDotYt,
    /// VP8
    Vp8,
    /// VP9
    Vp9,
    /// private
    Private(PrivateSubtype),
}
const VIDEO_SUBTYPE_VLIST: &[VideoSubtype] = &[
    VideoSubtype::_1DInterleavedParityfec,
    VideoSubtype::_3Gpp,
    VideoSubtype::_3GppTt,
    VideoSubtype::_3Gpp2,
    VideoSubtype::Av1,
    VideoSubtype::Bmpeg,
    VideoSubtype::Bt656,
    VideoSubtype::CelB,
    VideoSubtype::Dv,
    VideoSubtype::Encaprtp,
    VideoSubtype::Evc,
    VideoSubtype::Example,
    VideoSubtype::Ffv1,
    VideoSubtype::Flexfec,
    VideoSubtype::H261,
    VideoSubtype::H263,
    VideoSubtype::H2631998,
    VideoSubtype::H2632000,
    VideoSubtype::H264,
    VideoSubtype::H264Rcdo,
    VideoSubtype::H264Svc,
    VideoSubtype::H265,
    VideoSubtype::H266,
    VideoSubtype::IsoDotSegment,
    VideoSubtype::Jpeg,
    VideoSubtype::Jpeg2000,
    VideoSubtype::Jpeg2000Scl,
    VideoSubtype::Jxsv,
    VideoSubtype::LottiePlusJson,
    VideoSubtype::Matroska,
    VideoSubtype::Matroska3D,
    VideoSubtype::Mj2,
    VideoSubtype::Mp1S,
    VideoSubtype::Mp2P,
    VideoSubtype::Mp2T,
    VideoSubtype::Mp4,
    VideoSubtype::Mp4VEs,
    VideoSubtype::Mpeg,
    VideoSubtype::Mpeg4Generic,
    VideoSubtype::Mpv,
    VideoSubtype::Nv,
    VideoSubtype::Ogg,
    VideoSubtype::Parityfec,
    VideoSubtype::Pointer,
    VideoSubtype::Quicktime,
    VideoSubtype::Raptorfec,
    VideoSubtype::Raw,
    VideoSubtype::RtpEncAescm128,
    VideoSubtype::Rtploopback,
    VideoSubtype::Rtx,
    VideoSubtype::Scip,
    VideoSubtype::Smpte291,
    VideoSubtype::Smpte292M,
    VideoSubtype::Ulpfec,
    VideoSubtype::Vc1,
    VideoSubtype::Vc2,
    VideoSubtype::VndDotBlockfactDotFactv,
    VideoSubtype::VndDotCctv,
    VideoSubtype::VndDotDeceDotHd,
    VideoSubtype::VndDotDeceDotMobile,
    VideoSubtype::VndDotDeceDotMp4,
    VideoSubtype::VndDotDeceDotPd,
    VideoSubtype::VndDotDeceDotSd,
    VideoSubtype::VndDotDeceDotVideo,
    VideoSubtype::VndDotDirectvDotMpeg,
    VideoSubtype::VndDotDirectvDotMpegTts,
    VideoSubtype::VndDotDlnaDotMpegTts,
    VideoSubtype::VndDotDvbDotFile,
    VideoSubtype::VndDotFvt,
    VideoSubtype::VndDotHnsDotVideo,
    VideoSubtype::VndDotIptvforumDot1Dparityfec1010,
    VideoSubtype::VndDotIptvforumDot1Dparityfec2005,
    VideoSubtype::VndDotIptvforumDot2Dparityfec1010,
    VideoSubtype::VndDotIptvforumDot2Dparityfec2005,
    VideoSubtype::VndDotIptvforumDotTtsavc,
    VideoSubtype::VndDotIptvforumDotTtsmpeg2,
    VideoSubtype::VndDotMotorolaDotVideo,
    VideoSubtype::VndDotMotorolaDotVideop,
    VideoSubtype::VndDotMpegurl,
    VideoSubtype::VndDotMsPlayreadyDotMediaDotPyv,
    VideoSubtype::VndDotNokiaDotInterleavedMultimedia,
    VideoSubtype::VndDotNokiaDotMp4Vr,
    VideoSubtype::VndDotNokiaDotVideovoip,
    VideoSubtype::VndDotObjectvideo,
    VideoSubtype::VndDotPlanar,
    VideoSubtype::VndDotRadgamettoolsDotBink,
    VideoSubtype::VndDotRadgamettoolsDotSmacker,
    VideoSubtype::VndDotSealedDotMpeg1,
    VideoSubtype::VndDotSealedDotMpeg4,
    VideoSubtype::VndDotSealedDotSwf,
    VideoSubtype::VndDotSealedmediaDotSoftsealDotMov,
    VideoSubtype::VndDotUvvuDotMp4,
    VideoSubtype::VndDotVivo,
    VideoSubtype::VndDotYoutubeDotYt,
    VideoSubtype::Vp8,
    VideoSubtype::Vp9,
];
const VIDEO_SUBTYPE_SLIST: &[&str] = &[
    "1d-interleaved-parityfec",
    "3gpp",
    "3gpp-tt",
    "3gpp2",
    "AV1",
    "BMPEG",
    "BT656",
    "CelB",
    "DV",
    "encaprtp",
    "evc",
    "example",
    "FFV1",
    "flexfec",
    "H261",
    "H263",
    "H263-1998",
    "H263-2000",
    "H264",
    "H264-RCDO",
    "H264-SVC",
    "H265",
    "H266",
    "iso.segment",
    "JPEG",
    "jpeg2000",
    "jpeg2000-scl",
    "jxsv",
    "lottie+json",
    "matroska",
    "matroska-3d",
    "mj2",
    "MP1S",
    "MP2P",
    "MP2T",
    "mp4",
    "MP4V-ES",
    "mpeg",
    "mpeg4-generic",
    "MPV",
    "nv",
    "ogg",
    "parityfec",
    "pointer",
    "quicktime",
    "raptorfec",
    "raw",
    "rtp-enc-aescm128",
    "rtploopback",
    "rtx",
    "scip",
    "smpte291",
    "SMPTE292M",
    "ulpfec",
    "vc1",
    "vc2",
    "vnd.blockfact.factv",
    "vnd.CCTV",
    "vnd.dece.hd",
    "vnd.dece.mobile",
    "vnd.dece.mp4",
    "vnd.dece.pd",
    "vnd.dece.sd",
    "vnd.dece.video",
    "vnd.directv.mpeg",
    "vnd.directv.mpeg-tts",
    "vnd.dlna.mpeg-tts",
    "vnd.dvb.file",
    "vnd.fvt",
    "vnd.hns.video",
    "vnd.iptvforum.1dparityfec-1010",
    "vnd.iptvforum.1dparityfec-2005",
    "vnd.iptvforum.2dparityfec-1010",
    "vnd.iptvforum.2dparityfec-2005",
    "vnd.iptvforum.ttsavc",
    "vnd.iptvforum.ttsmpeg2",
    "vnd.motorola.video",
    "vnd.motorola.videop",
    "vnd.mpegurl",
    "vnd.ms-playready.media.pyv",
    "vnd.nokia.interleaved-multimedia",
    "vnd.nokia.mp4vr",
    "vnd.nokia.videovoip",
    "vnd.objectvideo",
    "vnd.planar",
    "vnd.radgamettools.bink",
    "vnd.radgamettools.smacker",
    "vnd.sealed.mpeg1",
    "vnd.sealed.mpeg4",
    "vnd.sealed.swf",
    "vnd.sealedmedia.softseal.mov",
    "vnd.uvvu.mp4",
    "vnd.vivo",
    "vnd.youtube.yt",
    "VP8",
    "VP9",
];
impl VideoSubtype {
    pub fn to_str(&self) -> &str {
        if let Self::Private(sub) = self {
            return sub.to_str();
        }
        let p = VIDEO_SUBTYPE_VLIST.binary_search(self).unwrap();
        VIDEO_SUBTYPE_SLIST[p]
    }
}
impl FromStr for VideoSubtype {
    type Err = MediaTypeError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lower = s.to_ascii_lowercase();
        if let Ok(pos) =
            VIDEO_SUBTYPE_SLIST.binary_search_by_key(&lower, |t| t.to_ascii_lowercase())
        {
            return Ok(VIDEO_SUBTYPE_VLIST[pos].clone());
        }
        Ok(Self::Private(s.parse()?))
    }
}
