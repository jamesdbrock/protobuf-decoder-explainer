module Main where

import Prelude

import Data.Array (many)
import Data.ArrayBuffer.ArrayBuffer (empty)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (buffer, fromArray, toArray, whole)
import Data.ArrayBuffer.Types (ArrayBuffer, DataView, Uint8ClampedArray)
import Data.Either (Either(..))
import Data.Long.Internal as Long
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Char
import Data.Tuple (Tuple(..))
import Data.UInt (UInt, fromInt)
import Data.UInt as UInt
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Protobuf.Common (Bytes(..), WireType(..))
import Protobuf.Decode as Decode
import Protobuf.Runtime (UnknownField(..), parseFieldUnknown)
import Text.Parsing.Parser (ParseError(..), Parser, ParserT, fail, runParser, runParserT)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.DataView (takeN)
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (anyChar, string)
import Text.Parsing.Parser.Token (digit, octDigit)

type State =
  -- { enabled :: Boolean
  { protobufString :: String
  }

data Action = Parse String


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  -- runUI Button.component unit body
  runUI component unit body

-- component :: forall q i o m. H.Component q i o m
-- component =
--   H.mkComponent
--     { unit
--     , render
--     , eval: H.mkEval $ H.defaultEval { }
--     }
--
-- render :: forall m. Unit -> H.ComponentHTML Unit () m
-- render _ =
--   HH.h1 [] [HH.text "Protobuf Decoder"]
--
component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  -- { enabled: false
  { protobufString: ""
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  -- let
  --   label = if state.enabled then "On" else "Off"
  -- in HH.div []
  HH.div []
    [ HH.h1 [] [HH.text "Protobuf Decoder"]
    -- , HH.button
    --   [ HP.title label
    --   -- , HE.onClick \_ -> Just Toggle
    --   ]
    --   [ HH.text label ]
    , HH.input
      [ HP.style "width: 100%"
      , HE.onValueInput \value -> Just $ Parse value
      ]
    , HH.div
      []
      [ HH.text state.protobufString ]
    ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  -- Toggle ->
  --   H.modify_ \st -> st { enabled = not st.enabled }
  Parse value -> H.modify_ \st -> st { protobufString = valueParsed }
   where
    valueParsed = case fromOctString of
      Left (ParseError message position) -> "Parse error " <> message <> show position
      Right xs ->
        -- let xs' = unsafePerformEffect $ toArray =<< (whole xs :: Effect Uint8ClampedArray) :: Array UInt
        -- in
        -- show xs'
        show $ unsafePerformEffect $ runParserT (DV.whole xs) parseProtobuf


    -- Inverse of ToOctString
    -- https://github.com/protocolbuffers/protobuf/blob/ab5b61bf2f0fb1ac485be1b82fffca153c2509ed/conformance/conformance_test.cc#L57
    -- https://github.com/protocolbuffers/protobuf/blob/d16bf914bc5ba569d2b70376051d15f68ce4322d/conformance/conformance_test.cc#L57
    -- Also it seems like it can contain C++ escape sequences?
    -- https://en.cppreference.com/w/cpp/language/escape
    -- For example
    -- Expect: \202\007\014\022\012\010\001\020\001\310\005\001\310\005\001, but got: \202\007\007\022\005\020\001\310\005\001. request=protobuf_payload: "\202\007\t\022\007\010\001\020\001\310\005\001\202\007\007\022\005\020\001\310\005\001"
    -- request=protobuf_payload: "\202\007\t\022\007\010\001\020\001\310\005\001\202\007\007\022\005\020\001\310\005\001" requested_output_format: PROTOBUF message_type: "protobuf_test_messages.proto3.TestAllTypesProto3" test_category: BINARY_TEST, response=protobuf_payload: "\202\007\007\022\005\020\001\310\005\001"
    fromOctString :: Either ParseError ArrayBuffer
    fromOctString = runParser value do
      -- pure $ unsafePerformEffect $ empty 0
      xs <- many parseByte
      let buf = unsafePerformEffect (fromArray xs) :: Uint8ClampedArray
      pure $ buffer buf
     where
      parseByte :: Parser String UInt
      parseByte = do
        void $ string "\\"
        fromInt <$> choice
          [ string "'" *> pure 0x27
          , string "\"" *> pure 0x22
          , string "?" *> pure 0x3f
          , string "\\" *> pure 0x5c
          , string "a" *> pure 0x07
          , string "b" *> pure 0x08
          , string "f" *> pure 0x0c
          , string "n" *> pure 0x0a
          , string "r" *> pure 0x0d
          , string "t" *> pure 0x09
          , string "v" *> pure 0x0b
          , do
              d0 <- parseOctalDigit
              d1 <- parseOctalDigit
              d2 <- parseOctalDigit
              let d = (d0 * 64) + (d1 * 8) + d2
              if d <= 255
                then pure d
                else fail $ "Octal byte overflow > 255: " <> show d
          ]
      -- See also octDigitToInt
      -- https://pursuit.purescript.org/packages/purescript-unicode/5.0.0/docs/Data.CodePoint.Unicode#v:octDigitToInt
      parseOctalDigit :: Parser String Int
      parseOctalDigit = anyChar >>= case _ of
        '0' -> pure 0
        '1' -> pure 1
        '2' -> pure 2
        '3' -> pure 3
        '4' -> pure 4
        '5' -> pure 5
        '6' -> pure 6
        '7' -> pure 7
        x   -> fail $ "Expecting octal digit but got " <> Char.singleton x

    parseProtobuf :: ParserT DataView Effect (Array UnknownField)
    parseProtobuf = many do
      Tuple fieldNumber wireType <- Decode.tag32
      -- parseFieldUnknown (UInt.toInt fieldNumber) wireType
      case wireType of
        VarInt ->  UnknownVarInt fieldNumber <$> Decode.uint64
        Bits64 -> UnknownBits64 fieldNumber <$> Decode.fixed64
        LenDel -> do
          len <- Long.toInt <$> Decode.varint64
          case len of
            Nothing -> fail "Length-delimited value of unknown field was too long."
            Just l -> do
              dv <- takeN l
              pure $ UnknownLenDel fieldNumber $ Bytes $ AB.slice (DV.byteOffset dv) (DV.byteLength dv) (DV.buffer dv)
        Bits32 -> UnknownBits32 fieldNumber <$> Decode.fixed32