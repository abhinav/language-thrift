`language-thrift` provides a parser for the [Thrift IDL format][]. The parser
is written using [`trifecta`][] to provide helpful error messages when parsing
fails. The parser keeps track of Javadoc-style comments (`/** ... */`) and
associates them with the type, service, function, or field above which they
were added.

Haddock-generated docs are available on [Hackage][] and [here][].

  [Thrift IDL format]: http://thrift.apache.org/docs/idl
  [`trifecta`]: http://hackage.haskell.org/package/trifecta
  [Hackage]: http://hackage.haskell.org/package/language-thrift
  [here]: http://abhinavg.net/language-thrift/
