module Test.Main where

import Prelude
import Data.Foldable (foldr)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.Set.NonEmpty as NE
import Data.Set (fromFoldable, size)

import Main (Action(..), OrderedAction(..), actionsSinceLastState, foldy, getState, indexFromSituation, newestActionIndex, openingStore)

item1 :: OrderedAction
item1 
  = OrderedAction (Tuple 1 (SetFirstName "egg"))

item2 :: OrderedAction
item2 
  = OrderedAction (Tuple 2 ToUpper)

item3 :: OrderedAction
item3 
  = OrderedAction (Tuple 3 (SetSurname "horse"))

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Foldy Boy" do
    describe "Folds in order" do
      it "Works with one item" do
        let store = foldy item1 openingStore
            newState = getState store
        size store.actions `shouldEqual` 1
        newState.firstName `shouldEqual` "egg"
      it "Works with three items" do
         let store = (foldy item1 >>> foldy item2 >>> foldy item3) openingStore
             newState = getState store
         size store.actions `shouldEqual` 3
         newState.firstName `shouldEqual` "EGG"
         newState.surname `shouldEqual` "horse"
      it "Gets actions since last state" do
         let actions = fromFoldable [ item1, item2, item3 ]
             store = openingStore { actions = actions }
         size (actionsSinceLastState store) `shouldEqual` 3
         indexFromSituation (NE.max store.states) `shouldEqual` 0
      it "Gets actions after a fold" do
         let store = foldy item2 openingStore
             store2 = store { actions = fromFoldable [ item1, item2, item3 ] }
         size (actionsSinceLastState store2) `shouldEqual` 1
         indexFromSituation (NE.max store2.states) `shouldEqual` 2 
      it "Newest action index" do
         let store = openingStore { actions = fromFoldable [ item2, item1,
                                  item2, item1 ] }
         newestActionIndex store `shouldEqual` 2
      it "Order does not matter" do
         let store1 = (foldy item3 >>> foldy item2 >>> foldy item1) openingStore
             store2 = (foldy item2 >>> foldy item1 >>> foldy item3) openingStore
             store3 = (foldy item1 >>> foldy item3 >>> foldy item2) openingStore
         getState store1 `shouldEqual` getState store2
         getState store3 `shouldEqual` getState store1
         store1.actions `shouldEqual` store2.actions
      it "Ends up with some states" do
         let store = foldr foldy openingStore [ item1, item2, item3, item2,
                                              item3, item1, item3, item1 ]
         NE.size (store.states) `shouldEqual` 2
