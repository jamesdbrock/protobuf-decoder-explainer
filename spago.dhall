{-
-}
{ name = "protobuf-decode"
, dependencies =
  [ "parsing"
  , "protobuf"
  , "halogen"
  , "aff"
  , "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "arrays"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "int64"
  , "maybe"
  , "newtype"
  , "parsing-dataview"
  , "prelude"
  , "strings"
  , "web-encoding"
  , "transformers"
  , "tuples"
  , "uint"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/jamesdbrock/protobuf-decode"
}
