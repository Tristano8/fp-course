{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.JsonParser where

import Course.Core
import Course.Parser
import Course.MoreParser
import Course.JsonValue
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.List
import Course.Optional

-- $setup
-- >>> :set -XOverloadedStrings

-- A special character is one of the following:
-- * \b  Backspace (ascii code 08)
-- * \f  Form feed (ascii code 0C)
-- * \n  New line
-- * \r  Carriage return
-- * \t  Tab
-- * \v  Vertical tab
-- * \'  Apostrophe or single quote (only valid in single quoted json strings)
-- * \"  Double quote (only valid in double quoted json strings)
-- * \\  Backslash character
data SpecialCharacter =
  BackSpace
  | FormFeed
  | NewLine
  | CarriageReturn
  | Tab
  | VerticalTab
  | SingleQuote
  | DoubleQuote
  | Backslash
  deriving (Eq, Ord, Show)

-- NOTE: This is not inverse to @toSpecialCharacter@.
fromSpecialCharacter ::
  SpecialCharacter
  -> Char
fromSpecialCharacter BackSpace =
  chr 0x08
fromSpecialCharacter FormFeed =
  chr 0x0C
fromSpecialCharacter NewLine =
  '\n'
fromSpecialCharacter CarriageReturn =
  '\r'
fromSpecialCharacter Tab =
  '\t'
fromSpecialCharacter VerticalTab =
  '\v'
fromSpecialCharacter SingleQuote =
  '\''
fromSpecialCharacter DoubleQuote =
  '"'
fromSpecialCharacter Backslash =
  '\\'

-- NOTE: This is not inverse to @fromSpecialCharacter@.
toSpecialCharacter ::
  Char
  -> Optional SpecialCharacter
toSpecialCharacter c =
  let table = ('b', BackSpace) :.
              ('f', FormFeed) :.
              ('n', NewLine) :.
              ('r', CarriageReturn) :.
              ('t', Tab) :.
              ('v', VerticalTab) :.
              ('\'', SingleQuote) :.
              ('"' , DoubleQuote) :.
              ('\\', Backslash) :.
              Nil
  in snd <$> find ((==) c . fst) table
  
-- | Parse a JSON string. Handle double-quotes, special characters, hexadecimal characters. See http://json.org for the full list of control characters in JSON.
--
-- /Tip:/ Use `hex`, `fromSpecialCharacter`, `between`, `is`, `charTok`, `toSpecialCharacter`.
--
-- >>> parse jsonString "\" abc\""
-- Result >< " abc"
--
-- >>> parse jsonString "\"abc\"def"
-- Result >def< "abc"
--
-- >>> parse jsonString "\"\\babc\"def"
-- Result >def< "\babc"
--
-- >>> parse jsonString "\"\\u00abc\"def"
-- Result >def< "\171c"
--
-- >>> parse jsonString "\"\\u00ffabc\"def"
-- Result >def< "\255abc"
--
-- >>> parse jsonString "\"\\u00faabc\"def"
-- Result >def< "\250abc"
--
-- >>> isErrorResult (parse jsonString "abc")
-- True
--
-- >>> isErrorResult (parse jsonString "\"\\abc\"def")
-- True
jsonString ::
  Parser Chars
jsonString = between (charTok '"') (charTok '"') (list jsonStringChar)

jsonStringChar :: Parser Char
jsonStringChar = do
  c <- character
  if c == '\\'
  then do
    c' <- character
    if c' == 'u'
    then hex
    else foldOptional (pure . fromSpecialCharacter) (unexpectedCharParser c') (toSpecialCharacter c')
  else
    if c == '"'
    then unexpectedCharParser c
    else pure c where
      foldOptional f _ (Full x) = f x
      foldOptional _ z Empty = z

-- | Parse a JSON rational.
--
-- /Tip:/ Use @readFloats@.
--
-- >>> parse jsonNumber "234"
-- Result >< 234 % 1
--
-- >>> parse jsonNumber "-234"
-- Result >< (-234) % 1
--
-- >>> parse jsonNumber "123.45"
-- Result >< 2469 % 20
--
-- >>> parse jsonNumber "-123"
-- Result >< (-123) % 1
--
-- >>> parse jsonNumber "-123.45"
-- Result >< (-2469) % 20
--
-- >>> isErrorResult (parse jsonNumber "-")
-- True
--
-- >>> isErrorResult (parse jsonNumber "abc")
-- True
jsonNumber ::
  Parser Rational
jsonNumber = P (\i -> case readFloats i of
                            Empty -> UnexpectedString i
                            Full (n, cs) -> Result cs n)

-- | Parse a JSON true literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonTrue "true"
-- Result >< "true"
--
-- >>> isErrorResult (parse jsonTrue "TRUE")
-- True
jsonTrue ::
  Parser Chars
jsonTrue = stringTok "true"

-- | Parse a JSON false literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonFalse "false"
-- Result >< "false"
--
-- >>> isErrorResult (parse jsonFalse "FALSE")
-- True
jsonFalse ::
  Parser Chars
jsonFalse = stringTok "false"

-- | Parse a JSON null literal.
--
-- /Tip:/ Use `stringTok`.
--
-- >>> parse jsonNull "null"
-- Result >< "null"
--
-- >>> isErrorResult (parse jsonNull "NULL")
-- True
jsonNull ::
  Parser Chars
jsonNull = stringTok "null"

-- | Parse a JSON array.
--
-- /Tip:/ Use `betweenSepbyComma` and `jsonValue`.
--
-- >>> parse jsonArray "[]"
-- Result >< []
--
-- >>> parse jsonArray "[true]"
-- Result >< [JsonTrue]
--
-- >>> parse jsonArray "[true, \"abc\"]"
-- Result >< [JsonTrue,JsonString "abc"]
--
-- >>> parse jsonArray "[true, \"abc\", []]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray []]
--
-- >>> parse jsonArray "[true, \"abc\", [false]]"
-- Result >< [JsonTrue,JsonString "abc",JsonArray [JsonFalse]]
jsonArray ::
  Parser (List JsonValue)
jsonArray = betweenSepbyComma '[' ']' jsonValue

-- | Parse a JSON object.
--
-- /Tip:/ Use `jsonString`, `charTok`, `betweenSepbyComma` and `jsonValue`.
--
-- >>> parse jsonObject "{}"
-- Result >< []
--
-- >>> parse jsonObject "{ \"key1\" : true }"
-- Result >< [("key1",JsonTrue)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false }"
-- Result >< [("key1",JsonTrue),("key2",JsonFalse)]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : false } xyz"
-- Result >xyz< [("key1",JsonTrue),("key2",JsonFalse)]
jsonObject ::
  Parser Assoc
-- jsonObject = betweenSepbyComma '{' '}' $ jsonString >>= \key -> charTok ':' >> jsonValue >>= \val -> pure (key, val)
jsonObject = betweenSepbyComma '{' '}' $ lift3 (\k _ v -> (k, v)) jsonString (charTok ':') jsonValue
-- | Parse a JSON value.
--
-- /Tip:/ Use `spaces`, `jsonNull`, `jsonTrue`, `jsonFalse`, `jsonArray`, `jsonString`, `jsonObject` and `jsonNumber`.
--
-- >>> parse jsonValue "true"
-- Result >< JsonTrue
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse])]
--
-- >>> parse jsonObject "{ \"key1\" : true , \"key2\" : [7, false] , \"key3\" : { \"key4\" : null } }"
-- Result >< [("key1",JsonTrue),("key2",JsonArray [JsonRational (7 % 1),JsonFalse]),("key3",JsonObject [("key4",JsonNull)])]
jsonValue ::
  Parser JsonValue
jsonValue = spaces *>
              (JsonTrue <$ jsonTrue
          ||| JsonFalse <$ jsonFalse
          ||| JsonNull <$ jsonNull
          ||| JsonArray <$> jsonArray
          ||| JsonString <$> jsonString
          ||| JsonObject <$> jsonObject
          ||| JsonRational <$> jsonNumber)

-- | Read a file into a JSON value.
--
-- /Tip:/ Use @System.IO#readFile@ and `jsonValue`.
readJsonValue ::
  FilePath
  -> IO (ParseResult JsonValue)
readJsonValue fp = readFile fp >>= \cs -> pure (parse jsonValue cs) 
