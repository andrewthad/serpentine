module Serpentine.PathPiece 
  ( PathPiece(..)
  ) where

import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Data.Text as Text

class PathPiece a where
  toPathPiece :: a -> Text
  fromPathPiece :: Text -> Maybe a

instance PathPiece Int where
  toPathPiece = Text.pack . show
  fromPathPiece = readMaybe . Text.unpack

instance PathPiece Char where
  toPathPiece = Text.singleton
  fromPathPiece t = if Text.length t == 1
    then Just (Text.head t)
    else Nothing

instance PathPiece Text where
  toPathPiece = id
  fromPathPiece = Just

