`language-thrift` provides a parser for the [Thrift IDL format]. In addition to
parsing the IDL, it keeps track of Javadoc-style comments (`/** ... */`) and
attaches them to the type, service, function, or field, above which they were
added.

The parser uses [`parsers`] to allow plugging in the underlying parser. A
default [`trifecta`] based parser is provided.

Haddock-generated docs are available on [Hackage] and [here].

  [Thrift IDL format]: http://thrift.apache.org/docs/idl
  [`parsers`]: http://hackage.haskell.org/package/parsers
  [`trifecta`]: http://hackage.haskell.org/package/trifecta
  [Hackage]: http://hackage.haskell.org/package/language-thrift
  [here]: http://abhinavg.net/language-thrift/
