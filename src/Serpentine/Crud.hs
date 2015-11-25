{-# LANGUAGE GADTs, TypeFamilies,DataKinds,PolyKinds,KindSignatures #-}
{-# LANGUAGE UndecidableInstances, TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs, FlexibleContexts #-}

module Serpentine.Crud where

import Prelude
import Serpentine
import Serpentine.PathPiece
import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Typeable (Typeable)

$(singletons [d|
  data CrudRoute = AddR | EditR | DeleteR | ViewR
    deriving (Eq,Ord,Enum,Bounded,Show)
  |])

type family PlanCrudRoute (key :: *) (r :: CrudRoute) :: [Piece *] where
  PlanCrudRoute key 'AddR    = '[ 'Static "add"]
  PlanCrudRoute key 'EditR   = '[ 'Static "edit", 'Capture key]
  PlanCrudRoute key 'DeleteR = '[ 'Static "delete", 'Capture key]
  PlanCrudRoute key 'ViewR   = '[ 'Static "view", 'Capture key]
genDefunSymbols [''PlanCrudRoute]

sPlanCrudRoute :: (Typeable i, PathPiece i)
  => Proxy i -> SCrudRoute route -> SList (PlanCrudRoute i route)
sPlanCrudRoute _ r = case r of
  SAddR -> defPieces
  SEditR -> defPieces
  SDeleteR -> defPieces
  SViewR -> defPieces

