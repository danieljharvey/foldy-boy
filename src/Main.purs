module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.String (toLower, toUpper)
import Data.Set (Set, empty, filter, findMax, insert)
import Data.Set.NonEmpty as NE
import Data.Tuple (Tuple(..))

main :: Effect Unit
main = do
  log "oo"

data Action
  = SetFirstName String
  | SetSurname String
  | SetAge Int
  | ToUpper
  | ToLower

derive instance eqAction :: Eq Action
instance showAction :: Show Action where
  show (SetFirstName s) = "SetFirstName " <> s
  show (SetSurname s)   = "SetSurname " <> s
  show (SetAge a )      = "SetAge "<> show a
  show ToUpper          = "ToUpper"
  show ToLower          = "ToLower"

newtype OrderedAction
  = OrderedAction (Tuple Int Action)

derive newtype instance eqOrderedAction :: Eq OrderedAction

instance orderedActionOrd :: Ord OrderedAction where
  compare (OrderedAction (Tuple a _)) (OrderedAction (Tuple b _)) 
    = compare a b

instance showOrderedAction :: Show OrderedAction where
  show (OrderedAction (Tuple a action))
    = "[" <> show a <> ": " <> show action <> "]"

type State
  = { firstName :: String
  , surname :: String
  , age :: Int
  }

reducer :: Action -> State -> State
reducer action state = case action of
  SetFirstName name -> state {firstName = name}
  SetSurname name -> state {surname = name}
  SetAge age -> state {age = age}
  ToUpper ->
    state
      { firstName = toUpper state.firstName
      , surname = toUpper state.surname
      }
  ToLower ->
    state
      { firstName = toLower state.firstName
      , surname = toLower state.surname
      }

newtype Situation
  = Situation (Tuple Int State)

derive newtype instance eqSituation :: Eq Situation

instance situationOrd :: Ord Situation where
  compare (Situation (Tuple a _)) (Situation (Tuple b _)) 
    = compare a b 

instance showSituation :: Show Situation where
  show (Situation (Tuple i state))
    = "[" <> show i <> ": " <> show state <> "]"

type Store
  = { states  :: NE.NonEmptySet Situation
    , actions :: Set OrderedAction
    }

getState :: Store -> State
getState store
  = let (Situation (Tuple _ state)) = NE.max store.states
    in state

createStore :: State -> Store
createStore initialState
  = let states = NE.singleton (Situation (Tuple 0 initialState))
        actions = empty
    in { states, actions } 

openingStore :: Store
openingStore 
  = createStore { firstName: "John", surname: "Horse", age: 100 }
   

indexFromSituation :: Situation -> Int
indexFromSituation (Situation (Tuple i _)) = i

stateFromSituation :: Situation -> State
stateFromSituation (Situation (Tuple _ s)) = s

actionFromOrderedAction :: OrderedAction -> Action
actionFromOrderedAction (OrderedAction (Tuple _ a)) = a

-- fold is a cheeky boy here
-- it puts the new action in the list, deletes states after it
-- then adds most recent state to the list
foldy :: OrderedAction -> Store -> Store
foldy (OrderedAction (Tuple actionIndex action)) store
  = let states    = filterFrom store.states actionIndex
        actions   = insert (OrderedAction (Tuple actionIndex action)) store.actions 
    in updateStoreState { states, actions }

filterFrom 
  :: NE.NonEmptySet Situation 
  -> Int 
  -> NE.NonEmptySet Situation
filterFrom items newIndex
  = let filtered = NE.filter (\(Situation (Tuple i _)) -> i < newIndex) items
        first    = NE.min items
    in NE.cons first filtered

actionsSinceLastState :: Store -> Set OrderedAction
actionsSinceLastState { actions, states }
  = let (Situation (Tuple mostRecentStateIndex _)) = NE.max states
        actionSet = filter (\(OrderedAction (Tuple i _)) ->
                    mostRecentStateIndex < i) actions
    in actionSet

newestActionIndex :: Store -> Int
newestActionIndex store
  = fromMaybe 0 $ (\(OrderedAction (Tuple i _)) -> i) <$> (findMax store.actions)


-- fold over actions from last good state to
-- current state to get current state
updateStoreState :: Store -> Store
updateStoreState store
  = store { states = newStates }
  where
    newestIndex
      = newestActionIndex store

    newStates
      = NE.insert (Situation (Tuple newestIndex newState)) store.states
    
    newState
      = foldl (\s -> \a -> reducer (actionFromOrderedAction a) s) (stateFromSituation lastGoodState) rawActions
    
    rawActions
      = actionsSinceLastState store

    lastGoodState
      = NE.max store.states
    
