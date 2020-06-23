{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JSON
  ( stripPrefix
  , module A
  )
where

import           Data.Aeson
import qualified Data.Aeson                    as A
import           Data.Aeson.Types               ( Parser )
import           Data.Char
import           GHC.Generics                   ( Generic
                                                , Rep
                                                )
import           Type.Reflection                ( Typeable
                                                , typeRep
                                                )
import           Control.Monad                  ( (>=>) )

stripPrefix :: String -> A.Options
stripPrefix prefix =
  A.defaultOptions { A.fieldLabelModifier = headToLower . stripLongestPrefix prefix }

stripLongestPrefix :: Eq a => [a] -> [a] -> [a]
stripLongestPrefix (x : xs) (y : ys) | x == y = stripLongestPrefix xs ys
stripLongestPrefix _ ys                       = ys


{-@ ignore headToLower @-}
headToLower :: String -> String
headToLower []       = error "field became empty"
headToLower (x : xs) = toLower x : xs
