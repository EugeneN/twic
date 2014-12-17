{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module BL.Parser (parseTweet) where

import           BL.Types                       (TweetElement(..))
import           Data.Text                      (Text, unpack)
import           Text.Parsec
import           Text.Parsec.Text               (Parser)



urlAlphabet = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),;:/?&="

usernameAlphabet = ['a'..'z']++['A'..'Z']++['0'..'9']++"_"

hashtagAlphabet = ['a'..'z']++['A'..'Z']++['0'..'9']++"_"

whitespace = many $ oneOf " \n\t"

username = do
  char '@'
  u <- try (many1 $ oneOf usernameAlphabet)
  return $ AtUsername u

link = do
  proto <- try (string "http://") <|> try (string "https://")
  url <- many1 (oneOf urlAlphabet)
  return $ Link $ proto ++ url


hashtag = do
  char '#'
  t <- try (many1 $ oneOf hashtagAlphabet)
  return $ Hashtag t 

retweet = do
  string "RT"
  return Retweet

plaintext = do
  notFollowedBy link
  t <- many1 (noneOf " ")

  return $ PlainText t

spcs = do
    s <- many1 (oneOf " \n\t")
    return $ Spaces s

textOrLink = (try link) <|> plaintext

chunk = username <|> hashtag <|> link <|> plaintext <|> spcs

tweet :: Parser [TweetElement]
tweet = do
    t <- many chunk
    return t


parseTweet :: Text -> [TweetElement]
parseTweet t = case parse tweet "tweet" t of
    Left err -> [Unparsable $ unpack t] -- error $ (show err) ++ ("  >>>> " :: String) ++ (show t)
    Right ts -> ts
