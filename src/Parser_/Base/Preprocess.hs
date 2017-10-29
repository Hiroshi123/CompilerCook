{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}


module Parser_.Base.Preprocess where


import qualified Data.ByteString as BS --(ByteString,unpack)
import qualified Data.ByteString.Char8 as BC



comment_rem :: BC.ByteString -> BC.ByteString
comment_rem "" = ""
comment_rem x =
  let h  = BS.head x
      t  = BS.tail x
      th = BS.head t
  in case ( h , th ) of
       ( 47 ,47 ) -> ""
       otherwise  -> BS.cons h (comment_rem t)

