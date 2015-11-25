{-# LANGUAGE GADTs, TypeFamilies,DataKinds,PolyKinds,KindSignatures #-}
{-# LANGUAGE RankNTypes,TypeOperators,OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Serpentine where

import Prelude
import Data.Text (Text)
import Text.Read (readMaybe)
import Data.Typeable 
import Data.Singletons.TH
import Data.Singletons.Prelude
import GHC.Exts
import GHC.TypeLits
import qualified Data.Text as Text

data Piece x = Static Symbol | Capture x
type StaticStarH (a :: Piece *) = a
type StaticStar (s :: Symbol) = StaticStarH ('Static s)

class PathPiece a where
  toPathPiece :: a -> Text
  fromPathPiece :: Text -> Maybe a

instance PathPiece Int where
  toPathPiece = Text.pack . show
  fromPathPiece = readMaybe . Text.unpack

instance PathPiece Text where
  toPathPiece = id
  fromPathPiece = Just

type family GetStatic (k :: Piece a) where
  GetStatic ('Static s) = s

type family GetCapture (k :: Piece *) where
  GetCapture ('Capture a) = a

type family PiecesHavePathPiece (k :: [Piece *]) :: Constraint where
  PiecesHavePathPiece '[] = ()
  PiecesHavePathPiece (p ': ps) = (PieceHasPathPiece p, PiecesHavePathPiece ps)

type family PieceHasPathPiece (k :: Piece *) :: Constraint where
  PieceHasPathPiece ('Static s) = ()
  PieceHasPathPiece ('Capture a) = PathPiece a

type family PieceData (k :: Piece *) where 
  PieceData ('Static s) = ()
  PieceData ('Capture a) = a

type family PiecesNestedTuple (ks :: [Piece *]) :: * where
  PiecesNestedTuple '[] = ()
  PiecesNestedTuple ('Static s ': ks) = PiecesNestedTuple ks
  PiecesNestedTuple ('Capture x ': ks) = (x, PiecesNestedTuple ks)
genDefunSymbols [''PiecesNestedTuple]

data instance Sing (k :: Piece *) where
  SStatic  :: KnownSymbol s => Sing (StaticStar s)
  SCapture :: forall (x :: *). (Typeable x, PathPiece x) => Sing ('Capture x)
type SPiece (k :: Piece *) = Sing k

class DefPieces (t :: [Piece *]) where
  defPieces :: SList t

instance DefPieces '[] where
  defPieces = SNil

instance (DefPieces ps, KnownSymbol s) => DefPieces ('Static s ': ps) where
  defPieces = SCons SStatic defPieces

instance (DefPieces ps, PathPiece t, Typeable t) => DefPieces ('Capture t ': ps) where
  defPieces = SCons SCapture defPieces

sEnumAll :: forall (a :: [k]) (b :: KProxy k). 
  ( a ~ EnumFromTo MinBound MaxBound
  , SBounded b, SEnum b
  )
  => SList a 
sEnumAll = sEnumFromTo sMinBound sMaxBound

mapValue :: forall (rs :: [k]) b. 
  (forall (r :: k). Sing r -> b) -> SList rs -> [b] 
mapValue f s = case s of
  SNil -> []
  SCons sr snext -> (f sr) : mapValue f snext

data SomeSingWith (f :: TyFun k [Piece *] -> *) where
  SomeSingWith :: Sing (a :: k) -> Sing (Apply g a) -> PiecesNestedTuple (Apply g a) -> SomeSingWith g

instance ( Show (DemoteRep ('KProxy :: KProxy k)) 
         , SingKind ('KProxy :: KProxy k)
         )
  => Show (SomeSingWith (f :: TyFun k [Piece *] -> *)) where
    show (SomeSingWith sroute spieces pnt) = ""
      ++ show (fromSing sroute) 
      ++ " " 
      ++ Text.unpack (renderCaptures spieces pnt)

renderCaptures :: forall (a :: [Piece *]). SList a -> PiecesNestedTuple a -> Text
renderCaptures s pnt = case s of
  SNil -> ""
  SCons spiece snext -> case spiece of
    SStatic -> renderCaptures snext pnt
    SCapture -> case pnt of
      (a,anext) -> Text.concat 
        [ "("
        , toPathPiece a
        , " :: "
        , Text.pack (show (typeOf a))
        , ")"
        , " "
        , renderCaptures snext anext
        ]

parseAllRoutes :: forall (kp :: KProxy k) (f :: TyFun k [Piece *] -> *). 
  (SEnum kp, SBounded kp)
  => Proxy f 
  -> (forall (rt :: k). Sing rt -> Sing (Apply f rt)) 
  -> [Text] 
  -> Maybe (SomeSingWith f)
parseAllRoutes p f pieces = parseManyRoutes p f sEnumAll pieces

parseManyRoutes :: forall (routes :: [k]) (f :: TyFun k [Piece *] -> *). 
     Proxy f 
  -> (forall (rt :: k). Sing rt -> Sing (Apply f rt)) 
  -> SList routes 
  -> [Text] 
  -> Maybe (SomeSingWith f)
parseManyRoutes p f routes pieces = case routes of
  SNil -> Nothing
  SCons sroute snext -> case parseOneRoute p f sroute pieces of
    Nothing -> parseManyRoutes p f snext pieces
    Just pnt -> Just (SomeSingWith sroute (f sroute) pnt)

parseOneRoute :: forall (r :: k) (f :: TyFun k [Piece *] -> *). 
     Proxy f
  -> (forall (rt :: k). Sing rt -> Sing (Apply f rt)) 
  -> Sing r -> [Text] -> Maybe (PiecesNestedTuple (Apply f r))
parseOneRoute _ f r pieces = parseOne (f r) pieces

parseOne :: forall (pieces :: [Piece *]).
  SList pieces -> [Text] -> Maybe (PiecesNestedTuple pieces)
parseOne s pieces = case s of
  SNil -> if null pieces then Just () else Nothing
  SCons spiece snext -> case pieces of
    [] -> Nothing
    (piece:piecesNext) -> case spiece of
      SStatic -> if renderPiece spiece () == piece
        then parseOne snext piecesNext
        else Nothing
      SCapture -> (,) 
        <$> parseCapturePiece spiece piece 
        <*> parseOne snext piecesNext

render :: forall (f :: TyFun rk [Piece *] -> *) (route :: rk).
     Proxy f 
  -> (forall r. Sing r -> Sing (Apply f r)) 
  -> Sing route 
  -> PiecesNestedTuple (Apply f route)
  -> [Text] -- Sing (Apply f route)
render _ f r pdata = renderPieces (f r) pdata

renderExample :: forall (f :: TyFun rk [Piece *] -> *) (route :: rk).
     Proxy f 
  -> (forall r. Sing r -> Sing (Apply f r)) 
  -> Sing route 
  -> [Text] 
renderExample _ f r = renderExamplePieces (f r)

renderExamplePieces :: forall (pieces :: [Piece *]).
     SList pieces
  -> [Text]
renderExamplePieces spieces = case spieces of
  SNil -> []
  SCons (spiece :: SPiece a) spiecesNext -> case spiece of
    SStatic  -> renderPiece spiece () : renderExamplePieces spiecesNext
    SCapture -> Text.cons '#' (Text.pack (show (typeRep (Proxy :: Proxy (GetCapture a))))) : renderExamplePieces spiecesNext

renderPieces :: forall (pieces :: [Piece *]).
     SList pieces
  -> PiecesNestedTuple pieces
  -> [Text]
renderPieces spieces pdata = case spieces of
  SNil  -> []
  SCons spiece spiecesNext -> case spiece of
    SStatic -> renderPiece spiece () : renderPieces spiecesNext pdata
    SCapture -> case pdata of
      (x,xs) -> renderPiece spiece x : renderPieces spiecesNext xs

parseCapturePiece :: (PathPiece a, piece ~ 'Capture a)
  => SPiece piece -> Text -> Maybe a
parseCapturePiece SCapture t = fromPathPiece t
parseCapturePiece _ _        = error "impossible"

-- Maybe this shouldn't be its own function
renderPiece :: forall (piece :: Piece *). 
     PieceHasPathPiece piece
  => SPiece piece 
  -> PieceData piece
  -> Text
renderPiece p d = case p of
  SStatic  -> Text.pack (symbolVal (Proxy :: Proxy (GetStatic piece)))
  SCapture -> toPathPiece d

