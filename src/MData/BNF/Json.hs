{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module MData.BNF.Json where

import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC

-- from Oreilly chart 5

data JValue =
  JString BS.ByteString
  -- | JNumber Double
  | JInt Int
  | JBool Bool
  | JNull
  | JObject [(BS.ByteString, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)


-- getString :: JValue -> Maybe BS.ByteString
-- getString (JString s) = Just s
-- getString _ = Nothing



