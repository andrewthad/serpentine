{-# LANGUAGE GADTs, TypeFamilies,DataKinds,PolyKinds,KindSignatures #-}
{-# LANGUAGE RankNTypes,TypeOperators,OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import Prelude
import Data.Text (Text)
import Data.Typeable 
import Data.Singletons.TH
import Data.Singletons.Prelude
import Serpentine
import Serpentine.Crud
import Serpentine.PathPiece
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data Foo   = Bar | Baz
data Thing = This | That Foo
data Color = Red | Green | Blue Thing

$(singletons [d|
  data MyRoute = UsersR 
               | ProfileR 
               | DogR CrudRoute 
               | HouseR CrudRoute 
               | RenameR
    deriving (Eq,Ord,Show)
  instance Enum MyRoute where
    toEnum i
      | i == 0            = UsersR
      | i == 1            = ProfileR
      | i >= 2 && i <= 5  = DogR (toEnum (i - 2))
      | i >= 6 && i <= 9  = HouseR (toEnum (i - 6))
      | i == 10           = RenameR
    fromEnum a = go a
      where
      go UsersR = 0
      go ProfileR = 1
      go RenameR = 10
      go (DogR c) = 2 + fromEnum c
      go (HouseR c) = 6 + fromEnum c
  instance Bounded MyRoute where
    minBound = UsersR
    maxBound = RenameR
  |])

-- myChar :: Char
-- myChar = 'T'
-- 
-- colorToChar :: Color -> Char
-- colorToChar c = $(constConstructors 'c ''Color 'myChar)

newtype DogId = DogId { getDogId :: Int }
  deriving (Eq,Ord,Typeable,PathPiece)

type family PlanMyRoute (k :: MyRoute) :: [Piece *] where
  PlanMyRoute 'UsersR        = '[ 'Static "user", 'Static "index"]
  PlanMyRoute 'ProfileR      = '[ 'Static "user", 'Static "profile", 'Capture Int]
  PlanMyRoute 'RenameR       = '[ 'Static "user", 'Static "edit", 'Capture Int, 'Capture Text]
  PlanMyRoute ('DogR crud)   = 'Static "dog"   ': PlanCrudRoute DogId crud
  PlanMyRoute ('HouseR crud) = 'Static "house" ': PlanCrudRoute Int crud
genDefunSymbols [''PlanMyRoute]

-- sPlanMyRoute :: SMyRoute route -> SList (PlanMyRoute route)
-- sPlanMyRoute r = case r of
--   SUsersR   -> defPieces
--   SProfileR -> defPieces
--   SRenameR  -> defPieces
--   SDogR c   -> SCons (SStatic :: SPiece ('Static "dog")) (sPlanCrudRoute (Proxy :: Proxy DogId) c)
--   SHouseR c -> SCons (SStatic :: SPiece ('Static "house")) (sPlanCrudRoute (Proxy :: Proxy Int) c)

sPlanMyRoute :: SMyRoute route -> SList (PlanMyRoute route)
sPlanMyRoute r = $(constConstructors 'r ''MyRoute 'defPieces)

parseMyRoute :: [Text] -> Maybe (SomeRoutePieces PlanMyRouteSym0)
parseMyRoute = parseAllRoutes (Proxy :: Proxy PlanMyRouteSym0) sPlanMyRoute

renderMyRoute :: Sing route 
              -> IRec (PiecesNestedTuple (PlanMyRoute route))
              -> [Text] 
renderMyRoute = render (Proxy :: Proxy PlanMyRouteSym0) sPlanMyRoute

main :: IO ()
main = do
  r SProfileR     (44 :& IRNil)
  r SRenameR      (33 :& "Bob" :& IRNil)
  r (SDogR SAddR) (IRNil)
  print (p "/bad/path")
  print (p "/user/edit/12/Jonathon")
  print (p "/house/edit/66")
  print (p "/dog/add")
  where 
  r a = Text.putStrLn 
      . Text.cons '/'
      . Text.intercalate "/"
      . renderMyRoute a
  p   = parseMyRoute
      . filter (not . Text.null) 
      . Text.splitOn "/"

