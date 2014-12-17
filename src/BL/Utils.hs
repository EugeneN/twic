module BL.Utils where

import qualified Data.ByteString               as B

-- https://mutelight.org/generating-a-permalink-slug-in-haskell
regexReplace :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
regexReplace regex replacement text = go text []
  where 
    go str res =
      if B.null str
      then B.concat . reverse $ res
      else 
        case (str =~~ regex) :: Maybe (B.ByteString, B.ByteString, B.ByteString) of
          Nothing -> B.concat . reverse $ (str:res)
          Just (bef, _ , aft) -> go aft (replacement:bef:res)