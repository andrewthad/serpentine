{-# LANGUAGE GADTs, TypeFamilies,DataKinds,PolyKinds,KindSignatures #-}
{-# LANGUAGE RankNTypes,TypeOperators,OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Serpentine.Playground where

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Data.Vinyl.Core
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits

data Piece x = Static Symbol | Capture x

data instance Sing (k :: Piece a) where
  SStatic :: (x ~ 'Static s) => SSymbol s -> Sing x
  SCapture :: (x ~ 'Capture b) => Sing b -> Sing x

newtype Attr (f :: TyFun k * -> *) (attr :: k) =
  Attr {getAttr :: Apply f attr}

type StaticSym1 s = 'Static s
type CaptureSym1 x = 'Capture x

data StaticSym0 :: (TyFun Symbol (Piece k) -> *)
type instance Apply StaticSym0 s = StaticSym1 s

data CaptureSym0 :: (TyFun k (Piece k) -> *)
type instance Apply CaptureSym0 s = CaptureSym1 s

$(singletons [d|
  captures :: [Piece x] -> [x]
  captures [] = []
  captures (Static _ : xs) = captures xs
  captures (Capture c : xs) = c : captures xs
  |])


------------------------
-- Route rendering
------------------------
render :: 
     Proxy f
  -> (forall route1. Sing route1 -> Sing (Apply f route1))
  -> (forall x. Sing x -> Attr e x -> Text)
  -> Sing route
  -> Rec (Attr e) (Captures (Apply f route))
  -> [Text]
render _ routeToPieces renderFunc r pdata = 
  renderPieces renderFunc (routeToPieces r) pdata

renderPieces :: forall (e :: TyFun item * -> *) (pieces :: [Piece item]).
  (forall x. Sing x -> Attr e x -> Text)
  -> SList pieces
  -> Rec (Attr e) (Captures pieces)
  -> [Text]
renderPieces renderFunc spieces attrs = 
  case spieces of
    SNil -> []
    SCons spiece spiecesNext -> 
      case spiece of
        SStatic sym -> renderSymbol sym : renderPieces renderFunc spiecesNext attrs
        SCapture item -> 
          case attrs of
            x :& xs -> renderFunc item x : renderPieces renderFunc spiecesNext xs

renderSymbol :: forall s. SSymbol s -> Text
renderSymbol SSym = Text.pack (symbolVal (Proxy :: Proxy s))

------------------------
-- Route parsing
------------------------
-- data SomeRoutePieces (f :: TyFun k [Piece *] -> *) where
--   SomeRoutePieces :: Sing (a :: k) -> Sing (Apply g a) 
--                   -> Rec (Attre (PiecesNestedTuple (Apply g a))
--                   -> SomeRoutePieces g
-- 
-- parsePiecesMulti :: forall e (pieces :: [Piece *]).
--   SList 
--   -> (forall x. Sing x -> Text -> Maybe (Attr e x))
--   -> [Text] 
--   -> Maybe (Rec (Attr e) (Captures pieces))
-- parsePiecesMulti = 

parsePieces :: forall e (pieces :: [Piece *]).
  (forall x. Sing x -> Text -> Maybe (Attr e x))
  -> SList pieces -> [Text] -> Maybe (Rec (Attr e) (Captures pieces))
parsePieces parsePiece s pieces = 
  case s of
    SNil -> if null pieces then Just RNil else Nothing
    SCons spiece snext -> 
      case pieces of
        [] -> Nothing
        (piece:piecesNext) -> 
          case spiece of
            SStatic sym -> 
              if renderSymbol sym == piece
                then parsePieces parsePiece snext piecesNext
                else Nothing
            SCapture sitem -> (:&)
              <$> parsePiece sitem piece 
              <*> parsePieces parsePiece snext piecesNext

-- parseSingle :: 
--   (forall x. Sing x -> Text -> Maybe (Attr e x))
--   -> SPiece item -> Text -> Maybe (Attr e item)
-- parseSingle 


