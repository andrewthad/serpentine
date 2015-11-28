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
import Data.Typeable 
import Data.Singletons.TH
import Data.Singletons.Prelude
import GHC.TypeLits
import Serpentine.PathPiece
import qualified Data.Text as Text
import Language.Haskell.TH
import Control.Monad

iterConstructors :: Name -> Q Exp
iterConstructors name = stringE . show =<< reify name

constConstructors :: Name -> Name -> Name -> Q Exp
constConstructors vname name val = do
  TyConI (DataD _ _ _ ctors _) <- reify name
  matches <- forM ctors $ \(NormalC cname args) -> case args of
    [] -> return $ Match (ConP (sketchyNameSingletonize cname) []) (NormalB $ VarE val) []
    [(_,ConT tname)] -> do
      let vnameNext = mkName "j"
      r <- constConstructors vnameNext tname val
      return $ Match (ConP (sketchyNameSingletonize cname) [VarP vnameNext]) (NormalB r) []
  return $ CaseE (VarE vname) matches

sketchyNameSingletonize :: Name -> Name
sketchyNameSingletonize = id
  . mkName . ('S':) . reverse 
  . takeWhile (/= '.') . reverse . show

-- constConstructors :: Name -> Name -> Name -> Q Exp
-- constConstructors vname name val = do
--   TyConI (DataD _ _ _ ctors _) <- reify name
--   matches <- forM ctors $ \(NormalC cname args) -> case args of
--     [] -> return $ Match (ConP cname []) (NormalB $ VarE val) []
--     [(_,ConT tname)] -> do
--       let vnameNext = mkName "j"
--       r <- constConstructors vnameNext tname val
--       return $ Match (ConP cname [VarP vnameNext]) (NormalB r) []
--   return $ CaseE (VarE vname) matches


data IRec :: [*] -> * where
  IRNil :: IRec '[]
  (:&)  :: !r -> !(IRec rs) -> IRec (r ': rs)
infixr :&

data Piece x = Static Symbol | Capture x
type StaticStarH (a :: Piece *) = a
type StaticStar (s :: Symbol) = StaticStarH ('Static s)

type family PiecesNestedTuple (ks :: [Piece *]) :: [*] where
  PiecesNestedTuple '[] = '[]
  PiecesNestedTuple ('Static s ': ks) = PiecesNestedTuple ks
  PiecesNestedTuple ('Capture x ': ks) = x ': PiecesNestedTuple ks
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

data SomeRoutePieces (f :: TyFun k [Piece *] -> *) where
  SomeRoutePieces :: Sing (a :: k) -> Sing (Apply g a) 
                  -> IRec (PiecesNestedTuple (Apply g a))
                  -> SomeRoutePieces g

instance ( Show (DemoteRep ('KProxy :: KProxy k)) 
         , SingKind ('KProxy :: KProxy k)
         )
  => Show (SomeRoutePieces (f :: TyFun k [Piece *] -> *)) where
    show (SomeRoutePieces sroute spieces pnt) = ""
      ++ show (fromSing sroute) 
      ++ " " 
      ++ Text.unpack (renderCaptures spieces pnt)

renderCaptures :: forall (a :: [Piece *]). SList a 
  -> IRec (PiecesNestedTuple a) -> Text
renderCaptures s pnt = case s of
  SNil -> ""
  SCons spiece snext -> case spiece of
    SStatic -> renderCaptures snext pnt
    SCapture -> case pnt of
      a :& anext -> Text.concat 
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
  -> Maybe (SomeRoutePieces f)
parseAllRoutes p f pieces = parseManyRoutes p f sEnumAll pieces

parseManyRoutes :: forall (routes :: [k]) (f :: TyFun k [Piece *] -> *). 
     Proxy f 
  -> (forall (rt :: k). Sing rt -> Sing (Apply f rt)) 
  -> SList routes 
  -> [Text] 
  -> Maybe (SomeRoutePieces f)
parseManyRoutes p f routes pieces = case routes of
  SNil -> Nothing
  SCons sroute snext -> case parseOneRoute p f sroute pieces of
    Nothing -> parseManyRoutes p f snext pieces
    Just pnt -> Just (SomeRoutePieces sroute (f sroute) pnt)

parseOneRoute :: forall (r :: k) (f :: TyFun k [Piece *] -> *). 
     Proxy f
  -> (forall (rt :: k). Sing rt -> Sing (Apply f rt)) 
  -> Sing r -> [Text] -> Maybe (IRec (PiecesNestedTuple (Apply f r)))
parseOneRoute _ f r pieces = parseOne (f r) pieces

parseOne :: forall (pieces :: [Piece *]).
  SList pieces -> [Text] -> Maybe (IRec (PiecesNestedTuple pieces))
parseOne s pieces = case s of
  SNil -> if null pieces then Just IRNil else Nothing
  SCons spiece snext -> case pieces of
    [] -> Nothing
    (piece:piecesNext) -> case spiece of
      SStatic -> if renderStaticPiece spiece == piece
        then parseOne snext piecesNext
        else Nothing
      SCapture -> (:&)
        <$> parseCapturePiece spiece piece 
        <*> parseOne snext piecesNext

render :: forall (f :: TyFun rk [Piece *] -> *) (route :: rk).
     Proxy f 
  -> (forall r. Sing r -> Sing (Apply f r)) 
  -> Sing route 
  -> IRec (PiecesNestedTuple (Apply f route))
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
  SCons spiece spiecesNext -> case spiece of
    SStatic  -> renderStaticPiece spiece : renderExamplePieces spiecesNext
    SCapture -> Text.cons '#' (renderCapturePieceType spiece) : renderExamplePieces spiecesNext

renderPieces :: forall (pieces :: [Piece *]).
     SList pieces
  -> IRec (PiecesNestedTuple pieces)
  -> [Text]
renderPieces spieces pdata = case spieces of
  SNil  -> []
  SCons spiece spiecesNext -> case spiece of
    SStatic -> renderStaticPiece spiece : renderPieces spiecesNext pdata
    SCapture -> case pdata of
      x :& xs -> renderCapturePieceValue spiece x : renderPieces spiecesNext xs

parseCapturePiece :: (PathPiece a, piece ~ 'Capture a)
  => SPiece piece -> Text -> Maybe a
parseCapturePiece SCapture t = fromPathPiece t
parseCapturePiece _ _        = error "impossible"

renderStaticPiece :: forall static s. (static ~ 'Static s)
  => SPiece static -> Text
renderStaticPiece SStatic = Text.pack (symbolVal (Proxy :: Proxy s))
renderStaticPiece _       = error "renderStaticPiece: impossible"

renderCapturePieceValue :: (piece ~ 'Capture a)
  => SPiece piece -> a -> Text
renderCapturePieceValue SCapture t = toPathPiece t
renderCapturePieceValue _ _        = error "impossible"

renderCapturePieceType :: forall a piece. 
  (piece ~ 'Capture a) => SPiece piece -> Text
renderCapturePieceType SCapture = 
  Text.pack (show (typeRep (Proxy :: Proxy a)))
renderCapturePieceType _        = error "impossible"

