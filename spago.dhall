{-
-}
{ name = "protobuf-decode"
, dependencies =
  [ "parsing"
  , "protobuf"
  , "halogen"
  , "unicode"
  , "debug"
  , "aff"
  , "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "arrays"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "longs"
  , "maybe"
  , "newtype"
  , "parsing-dataview"
  , "prelude"
  , "strings"
  , "text-encoding"
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
