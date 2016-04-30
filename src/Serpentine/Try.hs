{-# LANGUAGE GADTs, TypeFamilies,DataKinds,PolyKinds,KindSignatures #-}
{-# LANGUAGE RankNTypes,TypeOperators,OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Serpentine.Try where

import Data.Singletons
import Data.Singletons.TH
import Data.Singletons.Prelude
import Serpentine.Playground
import Data.Text (Text)
import Data.Vinyl.Core
import qualified Data.Text as Text

$(singletons [d|
  data Item = ItemInt | ItemText | ItemBool
  data Crud = AddR | EditR | DeleteR | ViewR
  data MyRoute = UsersR 
               | ProfileR 
               | HomeR
               | DogR Crud
  |])


$(singletonsOnly [d|
  planCrud :: n -> Crud -> [Piece n]
  planCrud n x = 
    case x of
      AddR    -> [Static "add"]
      EditR   -> [Static "edit", Capture n]
      DeleteR -> [Static "delete", Capture n]
      ViewR   -> [Static "view", Capture n]

  plan :: MyRoute -> [Piece Item]
  plan x = 
    case x of
      UsersR    -> [Static "user", Static "index"]
      ProfileR  -> [Static "profile", Capture ItemInt]
      HomeR     -> []
      DogR crud -> Static "dog" : planCrud ItemInt crud
  |])

type family ItemType (x :: Item) where
  ItemType ItemInt  = Int
  ItemType ItemText = Text
  ItemType ItemBool = Bool

genDefunSymbols [''ItemType]

renderAnyItem :: SItem x -> Attr ItemTypeSym0 x -> Text
renderAnyItem x attr = case x of
  SItemInt -> Text.pack $ show $ getAttr attr
  SItemText -> getAttr attr
  SItemBool -> if getAttr attr then "yes" else "no"

renderMyRoute :: SMyRoute x 
              -> Rec (Attr ItemTypeSym0) (Captures (Plan x)) 
              -> [Text]
renderMyRoute = render (Proxy :: Proxy PlanSym0) sPlan renderAnyItem 

test1, test2, test3 :: [Text]
test1 = renderMyRoute SProfileR (Attr 33 :& RNil)
test2 = renderMyRoute SUsersR RNil
test3 = renderMyRoute (SDogR SViewR) (Attr 12 :& RNil)

