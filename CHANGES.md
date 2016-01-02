0.6.0.0
=======

-   Added a pretty printer based on `ansi-wl-pprint`.
-   Both pretty printing modules now export instances of `Pretty` for relevant
    elements of the AST. These instances use `defaultConfig` for printing.

0.5.0.0
=======

The AST representation has been overhauled to be more consistent and
accessible. As a result, this release contains a number of breaking changes:

-   Moved `Header` records into `Include` and `Namespace` types.
-   Moved `ConstDefinition` record into separate type, `Const`.
-   Moved `ServiceDefinition` record into separate type, `Service`.
-   Moved `Type` records into separate types: `Typedef`, `Enum`, `Struct`,
    `Union`, `Exception`, `Senum`.
-   Renamed `FieldType` to `TypeReference`.
-   Renamed parser and pretty printer for `TypeReference` to `typeReference`.
-   Renamed the following record fields: `constType` to `constValueType`,
    `typedefType` to `typedefTargetType`, `fieldType` to `fieldValueType`, and
    `fieldDefault` to `fieldDefaultValue`.
-   Hide `function` parser and pretty printer.
-   Moved type annotations for defined types into the records for the types
    themselves.

Other changes:

-   Added lenses and prisms for AST types where appropriate.
-   Parsing will fail if the end of the document is not reached when the parser
    stops. This fixes the bug where the parser would stop half way through a
    file when it saw a recoverable error.
-   Added source annotations to headers, type references (`DefinedType`) and
    constant value references (`ConstIdentifer`).
-   Added `i8` as an alias for `byte`.
-   Type annotations are now allowed to have no associated value.
-   Expose parsers and pretty printers for different headers and definitions.
-   Fixed a bug which would cause parsing to fail if a definition ended with a
    semicolon or a comma.
-   Drop dependency on mtl.

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
