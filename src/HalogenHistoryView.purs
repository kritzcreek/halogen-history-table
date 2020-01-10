module HalogenHistoryView where

import Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Const (Const(..))
import Data.Foldable (fold, foldl)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (un)
import Debug.Trace (spy, traceM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type PersonR f = ( birthday :: f String, name :: f String, rating :: f Int )

type Person = Record (PersonR Identity)
type PersonUpdate = Record (date :: String | PersonR Maybe)
type Filters = Record (PersonR (Const Boolean))

applyUpdate :: Person -> PersonUpdate -> Person
applyUpdate person update =
  { birthday: maybe person.birthday Identity update.birthday
  , name: maybe person.name Identity update.name
  , rating: maybe person.rating Identity update.rating
  }

applyUpdates :: Person -> Array PersonUpdate -> Person
applyUpdates p us = foldl applyUpdate p (Array.reverse us)

isFiltered :: Filters -> PersonUpdate -> Boolean
isFiltered filters update =
  let
    filterName = un Const filters.name
    filterBirthday = un Const filters.birthday
    filterRating = un Const filters.rating
  in
    if not (filterName || filterBirthday || filterRating)
    then false
    else
      not ((filterName && isJust update.name)
           || (filterBirthday && isJust update.birthday)
           || (filterRating && isJust update.rating))

initialPerson :: Person
initialPerson = { birthday: Identity "01.01.1970", name: Identity "Carl", rating: Identity 1 }

emptyUpdate :: String -> PersonUpdate
emptyUpdate date = { date, birthday: Nothing, name: Nothing, rating: Nothing }

updates :: Array PersonUpdate
updates =
  [ (emptyUpdate "09.01.2020") { name = Just "Christoph" }
  , (emptyUpdate "07.01.2020") { rating = Just 10 }
  , (emptyUpdate "05.01.2020") { birthday = Just "20.02.1992" }
  , (emptyUpdate "03.01.2020") { name = Just "Carlotta" }
  ]

type Html m = H.ComponentHTML Action () m

type State =
  { initial :: Person
  , updates :: Array PersonUpdate
  , hovered :: Maybe Int
  , filters :: Filters
  }

data Action
  = HoverRow Int
  | LeaveTable
  | ToggleFilter String

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { initial: initialPerson
  , updates
  , hovered: Nothing
  , filters:
      { name: Const false
      , birthday: Const false
      , rating: Const false
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "container") ]
    [ table state ]

table :: forall m. State -> H.ComponentHTML Action () m
table state =
  HH.table
    [ HP.class_ (HH.ClassName "hhv-table")
    , HE.onMouseLeave (\_ -> Just LeaveTable)
    ]
    ([tableHeader state, personRow "Current" (applyUpdates state.initial state.updates)]
      <> Array.mapWithIndex updateRow state.updates
      <> [personRow "Initial" state.initial])
  where
    updateRow :: Int -> PersonUpdate -> Html m
    updateRow ix update =
      let active = Just ix == state.hovered in
      let userInTime = applyUpdates state.initial (Array.takeEnd ((Array.length state.updates) - ix) state.updates) in
      let filtered = isFiltered state.filters update in
      HH.tr
          [ HP.classes $ fold
              [ pure (HH.ClassName "hhv-TableRow")
              , guard filtered $> HH.ClassName "hhv-TableRow--filtered"
              ]
          , HE.onMouseEnter (\_ -> Just (HoverRow ix))
          ]
          [ HH.td
              [ HP.class_ (HH.ClassName "hhv-TableElement") ]
              [ HH.text update.date ]
          , HH.td
              [ HP.classes $ fold
                [ pure (HH.ClassName "hhv-TableElement")
                , guard (isNothing update.name) $> HH.ClassName "hhv-TableElement--shadow"
                ]
              ]
              [ HH.text if active then un Identity userInTime.name else fromMaybe "" update.name ]
          , HH.td
              [ HP.classes $ fold
                [ pure (HH.ClassName "hhv-TableElement")
                , guard (isNothing update.birthday) $> HH.ClassName "hhv-TableElement--shadow"
                ]
              ]
              [ HH.text if active then un Identity userInTime.birthday else fromMaybe "" update.birthday ]
          , HH.td
              [ HP.classes $ fold
                [ pure (HH.ClassName "hhv-TableElement")
                , guard (isNothing update.rating) $> HH.ClassName "hhv-TableElement--shadow"
                ]
              ]
              [ HH.text if active then show (un Identity userInTime.rating) else maybe "" show update.rating ]
          ]

personRow :: forall m. String -> Person -> Html m
personRow label person =
  HH.tr
      [ HP.class_ (HH.ClassName "hhv-TableRow") ]
      [ HH.td
        [ HP.class_ (HH.ClassName "hhv-TableElement") ]
        [ HH.text label ]
      , HH.td
        [ HP.class_ (HH.ClassName "hhv-TableElement") ]
        [ HH.text (un Identity person.name) ]
      , HH.td
          [ HP.class_ (HH.ClassName "hhv-TableElement") ]
          [ HH.text (un Identity person.birthday) ]
      , HH.td
          [ HP.class_ (HH.ClassName "hhv-TableElement") ]
          [ HH.text (show (un Identity person.rating)) ]
      ]

tableHeader :: forall m. State -> Html m
tableHeader state =
  HH.thead_
    [ HH.tr_
        [ HH.th
          [ HP.class_ (H.ClassName "hhv-TableHeader") ]
          [ HH.text "" ]
        , HH.th
            [ HP.classes $ fold
                [ pure (H.ClassName "hhv-TableHeader")
                , guard (un Const state.filters.name) $> H.ClassName "hhv-TableHeader--filtered"
                ]
            , HE.onClick (\_ -> Just (ToggleFilter "name"))
            ]
            [ HH.text "Name" ]
        , HH.th
            [ HP.classes $ fold
                [ pure (H.ClassName "hhv-TableHeader")
                , guard (un Const state.filters.birthday) $> H.ClassName "hhv-TableHeader--filtered"
                ]
            , HE.onClick (\_ -> Just (ToggleFilter "birthday"))
            ]
            [ HH.text "Birthday" ]
        , HH.th
            [ HP.classes $ fold
                [ pure (H.ClassName "hhv-TableHeader")
                , guard (un Const state.filters.rating) $> H.ClassName "hhv-TableHeader--filtered"
                ]
            , HE.onClick (\_ -> Just (ToggleFilter "rating"))
            ]
            [ HH.text "Rating" ]
        ]
  ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  HoverRow ix -> do
    { hovered } <- H.get
    unless (hovered == Just ix) do
      H.modify_ (_ { hovered = Just ix })
  LeaveTable -> do
    H.modify_ (_ { hovered = Nothing })
  ToggleFilter lbl -> do
    H.modify_
      (\prev ->
        prev { filters = case lbl of
                  "name" -> prev.filters { name = not prev.filters.name }
                  "birthday" -> prev.filters { birthday = not prev.filters.birthday }
                  "rating" -> prev.filters { rating = not prev.filters.rating }
                  _ -> prev.filters
             })
