0.5.0.0
=======

Breaking changes:

-   Moved `Header` records into `Include` and `Namespace` types.
-   Moved `ConstDefinition` record into separate type, `Const`.
-   Moved `ServiceDefinition` record into separate type, `Service`.
-   Moved `Type` records into separate types: `Typedef`, `Enum`, `Struct`,
    `Union`, `Exception`, `Senum`.
-   Renamed `FieldType` to `TypeReference`.
-   Moved type annotations for defined types into the records for the types
    themselves.
-   Hide `function` parser and pretty printer.

Other changes:

-   Parsing will fail if the end of the document is not reached when the parser
    stops. This fixes the bug where the parser would stop half way through a
    file when it saw a recoverable error.
-   Added source annotations to headers, type references (`DefinedType`) and
    constant value references (`ConstIdentifer`).
-   Added `i8` as an alias for `byte`.
-   Type annotations are now allowed to have no associated value.
-   Expose parsers and pretty printers for different headers and definitions.

0.4.0.0
=======

-   Add pretty printing module.
-   Parsers of different constructors are no longer exported by the parsing
    module; instead only the parsers for their corresponding types are
    exported.
-   Rename record for field requiredness from `fieldRequiredNess` to
    `fieldRequiredness`.

0.3.0.0
=======

-   Allow changing the underlying parser to any parser that implements the
    `TokenParsing` class from `parsers`.
-   Add `thriftIDLParser` for standard use cases.
-   Add `Language.Thrift.Parser.Trifecta` with a standard Trifecta-based
    parser.

0.2.0.0
=======

-   Track starting positions in source annotations.
-   Move docs to a separate field.

0.1.0.1
=======

-   Allow `base` 4.9.

0.1.0.0
=======

-   Initial release.
