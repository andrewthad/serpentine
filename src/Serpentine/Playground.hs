{-# LANGUAGE GADTs, TypeFamilies,DataKinds,PolyKinds,KindSignatures #-}
{-# LANGUAGE RankNTypes,TypeOperators,OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Serpentine.Playground where

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude

type SSymbol = (Sing :: Symbol -> *)

data Piece x = Static Symbol | Capture x

data instance Sing (k :: Piece a) where
  SStatic :: (x ~ 'Static s) => SSymbol s -> Sing x
  SCapture :: (x ~ 'Capture b) => Sing b -> Sing x

type StaticSym1 s = 'Static s
type CaptureSym1 x = 'Capture x

data StaticSym0 :: (TyFun Symbol (Piece k) -> *)
type instance Apply StaticSym0 s = StaticSym1 s

data CaptureSym0 :: (TyFun k (Piece k) -> *)
type instance Apply CaptureSym0 s = CaptureSym1 s

$(singletons [d|
  data Item = ItemInt | ItemString | ItemBool

  data MyRoute = UsersR 
               | ProfileR 
               | HomeR
  |])

$(singletonsOnly [d|
  plan :: MyRoute -> [Piece Item]
  plan x = case x of
    UsersR   -> [Static "user", Static "index"]
    ProfileR -> [Static "profile", Capture ItemInt]
    HomeR    -> []
  |])



