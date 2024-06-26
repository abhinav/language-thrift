0.13.0.0 (2024-04-05)
=====================

-   ([#18](https://github.com/abhinav/language-thrift/pull/18))
    Migrate pretty printer to `prettyprinter`.

0.12.0.1 (2021-12-04)
=====================

-   Allow semigroups 0.20

0.12.0.0 (2020-12-31)
=====================

-   Port to Megaparsec 9.

0.11.0.0 (2019-10-01)
=====================

-   Port to Megaparsec 7.

0.10.0.0 (2016-09-25)
=====================

-   Breaking: Consolidate struct, union, and exception AST types into a single
    data type: `Struct`. Whether the object is a struct, union, or exception is
    now determined by the `StructKind` attribute.
-   Breaking: Deprecated module `Language.Thrift.Types` has now been removed.

0.9.0.2 (2016-08-31)
====================

-   Disallow reserved keywords from being used as identifier names.

0.9.0.1 (2016-05-26)
====================

-   Build with GHC 8.

0.9.0.0 (2016-05-15)
====================

- Deprecate the `Language.Thrift.Types` in favor of `Language.Thrift.AST`.
- Upgrade to `megaparsec` 5.0.

0.8.0.2 (2016-08-31)
====================

-   Disallow reserved keywords from being used as identifier names.

0.8.0.1 (2016-05-24)
====================

-   Build with GHC 8.

0.8.0.0 (2016-02-09)
====================

This release contains breaking changes. The number of transitive dependencies
has been reduced significantly.

-   Switched parser to `megaparsec`. `trifecta` and `parsers` bring too many
    dependencies with them.
-   Drop support for `wl-pprint`. Only `ansi-wl-pprint` is supported now.
-   Drop dependency on `lens`. Lenses for fields of the AST elements are still
    provided but prisms are not. Use `Control.Lens.makePrisms` to derive your
    own if needed.

0.7.0.1 (2016-01-27)
====================

-   Drop use of `TemplateHaskell`.

0.7.0.0 (2016-01-15)
====================

-   Added source annotations to all `TypeReference` and
    `ConstValue` constructors.
-   Added `name` and `srcAnnot` lenses for `Type` and `Definition`.

0.6.2.0 (2016-01-05)
====================

-   Use more concrete types for lenses which don't need overloading.

0.6.1.0 (2016-01-03)
====================

-   Output generated by the `ansi-wl-pprint` pretty printer will be colored
    using ANSI escape codes.

0.6.0.1 (2016-01-02)
====================

-   Export `Pretty` instances from `Language.Thrift.Types` instead of requiring
    a separate import of the corresponding `Pretty` module.

0.6.0.0 (2016-01-02)
====================

-   Added a pretty printer based on `ansi-wl-pprint`.
-   Both pretty printing modules now export instances of `Pretty` for relevant
    elements of the AST. These instances use `defaultConfig` for printing.

0.5.0.0 (2015-12-27)
====================

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
-   Moved type annotations for defined types into the records for the
    types themselves.

Other changes:

-   Added lenses and prisms for AST types where appropriate.
-   Parsing will fail if the end of the document is not reached when the
    parser stops. This fixes the bug where the parser would stop half way
    through a file when it saw a recoverable error.
-   Added source annotations to headers, type references (`DefinedType`) and
    constant value references (`ConstIdentifer`).
-   Added `i8` as an alias for `byte`.
-   Type annotations are now allowed to have no associated value.
-   Expose parsers and pretty printers for different headers and definitions.
-   Fixed a bug which would cause parsing to fail if a definition ended with a
    semicolon or a comma.
-   Drop dependency on mtl.

0.4.0.0 (2015-08-02)
====================

-   Add pretty printing module.
-   Parsers of different constructors are no longer exported by the parsing
    module; instead only the parsers for their corresponding types
    are exported.
-   Rename record for field requiredness from `fieldRequiredNess` to
    `fieldRequiredness`.

0.3.0.0 (2015-07-31)
====================

-   Allow changing the underlying parser to any parser that implements the
    `TokenParsing` class from `parsers`.
-   Add `thriftIDLParser` for standard use cases.
-   Add `Language.Thrift.Parser.Trifecta` with a standard
    Trifecta-based parser.

0.2.0.0 (2015-06-12)
====================

-   Track starting positions in source annotations.
-   Move docs to a separate field.

0.1.0.1 (2015-06-02)
====================

-   Allow `base` 4.9.

0.1.0.0 (2015-04-05)
====================

-   Initial release.
