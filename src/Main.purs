module Main where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, many, zipWith)
import Data.ArrayBuffer.ArrayBuffer (empty)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed (buffer, fromArray, whole)
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer.Types (ArrayBuffer, DataView, Uint8ClampedArray)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (hexadecimal, toStringAs)
import Data.Long.Internal as Long
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as Char
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), snd)
import Data.UInt (UInt, fromInt)
import Data.UInt as UInt
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class as Effect.Class
import Effect.Unsafe (unsafePerformEffect)
import Halogen (ClassName(..), get)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (HTML, object)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Protobuf.Common (Bytes(..), WireType(..), FieldNumber)
import Protobuf.Decode as Decode
import Protobuf.Runtime (UnknownField(..), parseFieldUnknown)
import Text.Parsing.Parser (ParseError(..), ParseState(..), Parser, ParserT, fail, runParser, runParserT)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.DataView (takeN)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (anyChar, string)
import Text.Parsing.Parser.Token (digit, octDigit)
import Web.DOM.Element (getAttribute)
import Web.DOM.NodeList (toArray)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (HTMLElement, window)
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLDocument (toDocument, toParentNode)
import Web.HTML.HTMLElement (fromElement, fromNode, toElement)
import Web.HTML.Window (document)

type State =
  -- { enabled :: Boolean
  -- { protobufString :: String
  { result :: Either String Message
  }

data Action = Parse String


main :: Effect Unit
main = HA.runHalogenAff do
  -- runUI Button.component unit body
  -- body <- HA.awaitBody
  -- runUI component unit body

  -- appMay <- do
  --   element <- getElementById "root" =<< toNonElementParentNode <$> toDocument <$> (document =<< window)
  --   pure $ fromElement =<< element
  -- case appMay of
  --   Nothing -> pure unit
  --   Just app -> runUI component unit app

  elements <- awaitSelectAll
    { query: QuerySelector "div#app"
    , attr: ""
    }
  for_ (head elements) \e -> runUI component unit e.element


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

initialInput :: String
initialInput = "\\202\\007\\t\\022\\007\\010\\001\\020\\001\\310\\005\\001\\202\\007\\007\\022\\005\\020\\001\\310\\005\\001"

initialState :: forall i. i -> State
initialState _ =
  -- { enabled: false
  -- { protobufString: ""
  { result: parseInput initialInput
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  -- let
  --   label = if state.enabled then "On" else "Off"
  -- in HH.div []
  HH.div []
    [ HH.h1 [] [HH.text "Protobuf Decoder Explainer"]
    -- , HH.button
    --   [ HP.title label
    --   -- , HE.onClick \_ -> Just Toggle
    --   ]
    --   [ HH.text label ]
    -- , HH.p_ [ HH.text "Paste a Protocol Buffers byte array into the input box."]
    , HH.input
      -- [ HP.style "width: 100%"
      [ HP.value "\\202\\007\\t\\022\\007\\010\\001\\020\\001\\310\\005\\001\\202\\007\\007\\022\\005\\020\\001\\310\\005\\001"
      , HE.onValueInput \value -> Just $ Parse value
      ]
    , HH.div
      []
      [ case state.result of
          Left error -> HH.text error
          Right message -> renderMessage message
      ]
    ]


renderMessage :: forall w i. Message -> HTML w i
renderMessage message = HH.table [ HP.class_ $ ClassName "messagetable" ]
  [ HH.tbody_ $ message <#> \(Tuple part field) ->
      HH.tr_ $ case field of
        Scalar (UnknownBits32 fieldNumber value) ->
          [ HH.td [HP.class_ $ ClassName "bytes"] [renderDataView part]
          , HH.td [HP.class_ $ ClassName "fnum"] [HH.text $ UInt.toString fieldNumber]
          , HH.td [HP.class_ $ ClassName "ftype"] [HH.text "Bits32"]
          , HH.td [HP.class_ $ ClassName "fval"] [HH.text $ UInt.toString value]
          ]
        Scalar (UnknownBits64 fieldNumber value) ->
          [ HH.td [HP.class_ $ ClassName "bytes"] [renderDataView part]
          , HH.td [HP.class_ $ ClassName "fnum"] [HH.text $ UInt.toString fieldNumber]
          , HH.td [HP.class_ $ ClassName "ftype"] [HH.text "Bits64"]
          , HH.td [HP.class_ $ ClassName "fval"] [HH.text $ Long.toString value]
          ]
        Scalar (UnknownVarInt fieldNumber value) ->
          [ HH.td [HP.class_ $ ClassName "bytes"] [renderDataView part]
          , HH.td [HP.class_ $ ClassName "fnum"] [HH.text $ UInt.toString fieldNumber]
          , HH.td [HP.class_ $ ClassName "ftype"] [HH.text "VarInt"]
          , HH.td [HP.class_ $ ClassName "fval"] [HH.text $ Long.toString value]
          ]
        Scalar (UnknownLenDel fieldNumber value) ->
          [ HH.td [HP.class_ $ ClassName "bytes"] [renderDataView part]
          , HH.td [HP.class_ $ ClassName "fnum"] [HH.text $ UInt.toString fieldNumber]
          , HH.td [HP.class_ $ ClassName "ftype"] [HH.text $ "Length Delimited " <> show value]
          , HH.td [HP.class_ $ ClassName "fval"] [HH.text "(uninterpretable)" ]
          ]
        Nested fieldNumber nestedMessage ->
          [ HH.td [HP.class_ $ ClassName "bytes"] [renderDataView part]
          , HH.td [HP.class_ $ ClassName "fnum"] [HH.text $ UInt.toString fieldNumber]
          , HH.td [HP.class_ $ ClassName "ftype"] [HH.text "Message "]
          , HH.td [HP.class_ $ ClassName "fval"] [renderMessage nestedMessage ]
          ]
  ]

renderDataView :: forall w i. DataView -> HTML w i
renderDataView dv = HH.div
  [ HP.class_ $ ClassName "dataview"]
  (bytearray <#> \b -> HH.div_ [HH.text $ toStringAs hexadecimal $ UInt.toInt b])
 where
  offset = DV.byteOffset dv
  len = DV.byteLength dv
  buf = DV.buffer dv
  bytearray = unsafePerformEffect $ AT.toArray =<< (AT.part buf offset len :: Effect Uint8ClampedArray) :: Array UInt


handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Parse value -> H.modify_ \st -> st { result = parseInput value }

parseInput :: String -> Either String Message
parseInput input = case fromOctString input of
  Left (ParseError message position@(Position {line,column})) -> Left $ "Parse failure in text input " <> show position <> " " <> message
  Right buf ->
    -- let xs' = unsafePerformEffect $ toArray =<< (whole xs :: Effect Uint8ClampedArray) :: Array UInt
    -- in
    -- show xs'
    -- show $ unsafePerformEffect $ runParserT (DV.whole xs) $ fix $ \p -> parseMessage p
    case unsafePerformEffect $ runParserT (DV.whole buf) $ fix $ \p -> parseMessage p of
      Left (ParseError message (Position {line,column})) -> Left $ "Parse failure at byte offset " <> show (column-1) <> ": " <> message
      Right message -> Right message


-- Inverse of ToOctString
-- https://github.com/protocolbuffers/protobuf/blob/ab5b61bf2f0fb1ac485be1b82fffca153c2509ed/conformance/conformance_test.cc#L57
-- https://github.com/protocolbuffers/protobuf/blob/d16bf914bc5ba569d2b70376051d15f68ce4322d/conformance/conformance_test.cc#L57
-- Also it seems like it can contain C++ escape sequences?
-- https://en.cppreference.com/w/cpp/language/escape
-- For example
-- Expect: \202\007\014\022\012\010\001\020\001\310\005\001\310\005\001, but got: \202\007\007\022\005\020\001\310\005\001. request=protobuf_payload: "\202\007\t\022\007\010\001\020\001\310\005\001\202\007\007\022\005\020\001\310\005\001"
-- request=protobuf_payload: "\202\007\t\022\007\010\001\020\001\310\005\001\202\007\007\022\005\020\001\310\005\001" requested_output_format: PROTOBUF message_type: "protobuf_test_messages.proto3.TestAllTypesProto3" test_category: BINARY_TEST, response=protobuf_payload: "\202\007\007\022\005\020\001\310\005\001"
fromOctString :: String -> Either ParseError ArrayBuffer
fromOctString value = runParser value do
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


-- See https://github.com/Thimoteus/SandScript/wiki/2.-Parsing-recursively
parseMessage :: ParserT DataView Effect Message -> ParserT DataView Effect Message
parseMessage recurse = many $ match $ parseField recurse

parseField :: ParserT DataView Effect Message -> ParserT DataView Effect Field
parseField parseMessage' = do
  Tuple fieldNumber wireType <- Decode.tag32
  -- parseFieldUnknown (UInt.toInt fieldNumber) wireType
  case wireType of
    VarInt -> Scalar <$> UnknownVarInt fieldNumber <$> Decode.uint64
    Bits64 -> Scalar <$> UnknownBits64 fieldNumber <$> Decode.fixed64
    LenDel -> do
      len <- Long.toInt <$> Decode.varint64
      case len of
        Nothing -> fail "Length-delimited value of unknown field was too long."
        Just l -> do
          dv <- takeN l
          case (unsafePerformEffect (runParserT dv parseMessage')) of
            Left _ -> pure $ Scalar <$> UnknownLenDel fieldNumber $ Bytes $ AB.slice (DV.byteOffset dv) (DV.byteLength dv) (DV.buffer dv)
            Right m -> pure $ Nested fieldNumber m
    Bits32 -> Scalar <$> UnknownBits32 fieldNumber <$> Decode.fixed32

data Field
  = Scalar UnknownField
  | Nested FieldNumber Message
-- derive instance eqField :: Eq Field
-- derive instance showField :: Show Field
-- derive instance genericField :: Generic Field _
-- instance showField :: Show Field where show = genericShow
instance showField :: Show Field where
  show (Scalar x) = show x
  show (Nested fieldNumber xs) = show fieldNumber <> " " <> show (map snd xs)

-- array of each parsed field, plus the matched DataView
type Message = Array (Tuple DataView Field)


-----------
-- Selection Helpers
-- https://github.com/citizennet/purescript-halogen-select/blob/37fdb24d3e86cccca51d52290ed26048494a7351/examples/Main.purs#L64-L81
-- We use these so that we can “inject” the application at the <div id="app"/>
-- element instead of at the end of the body.

awaitSelectAll
  :: { query :: QuerySelector, attr :: String }
  -> Aff (Array { element :: HTMLElement, attr :: String })
awaitSelectAll ask@{ query } = HA.awaitLoad >>= \_ -> selectElements ask

selectElements
  :: { query :: QuerySelector, attr :: String }
  -> Aff (Array { element :: HTMLElement, attr :: String })
selectElements { query, attr } = do
  nodeArray <- liftEffect do
    toArray =<< querySelectorAll query <<< toParentNode =<< document =<< window
  let
    elems = fromMaybe [] <<< sequence $ fromNode <$> nodeArray
  attrs <- liftEffect $ traverse (getAttribute attr <<< toElement) elems
  pure $ zipWith ({ element: _, attr: _ }) elems (fromMaybe "" <$> attrs)

-- | ParserT DataView match combinator
match :: forall a m. MonadEffect m => ParserT DataView m a -> ParserT DataView m (Tuple DataView a)
match p = do
  ParseState input (Position {column:column0}) _ <- get
  x <- p
  ParseState _ (Position {column:column1}) _ <- get
  part <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + (column0-1)) (column1-column0)
  pure $ Tuple part x

