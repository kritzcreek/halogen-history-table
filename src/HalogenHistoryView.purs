module HalogenHistoryView where

import Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Const (Const(..))
import Data.Foldable (fold, foldl)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (un)
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type PersonR f =
  ( birthday :: f String
  , name :: f String
  , rating :: f Int
  )

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
initialPerson =
  { birthday: Identity "01.01.1970"
  , name: Identity "Carl"
  , rating: Identity 1
  }

updates :: Array PersonUpdate
updates =
  [ (emptyUpdate "09.01.2020") { name = Just "Christoph" }
  , (emptyUpdate "07.01.2020") { rating = Just 10 }
  , (emptyUpdate "05.01.2020") { birthday = Just "20.02.1992" }
  , (emptyUpdate "03.01.2020") { name = Just "Carlotta" }
  ]
  where
    emptyUpdate date =
      { date, birthday: Nothing, name: Nothing, rating: Nothing }

type Html m = H.ComponentHTML Action () m

type State =
  { initial :: Person
  , updates :: Array PersonUpdate
  , hoveredRow :: Maybe Int
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
  , hoveredRow: Nothing
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
    [ HE.onMouseLeave (\_ -> Just LeaveTable) ] $
    [ tableHeader state
    , personRow "Current" (applyUpdates state.initial state.updates)
    ]
    <> Array.mapWithIndex updateRow state.updates
    <> [personRow "Initial" state.initial]
  where
    updateRow :: Int -> PersonUpdate -> Html m
    updateRow ix update =
      let
        active = Just ix == state.hoveredRow
        userInTime =
          applyUpdates
            state.initial
            (Array.takeEnd
              (Array.length state.updates - ix)
              state.updates)
        filtered = isFiltered state.filters update
        mkCell :: forall a. (a -> String) -> Identity a -> Maybe a -> Html m
        mkCell display shadowVal val =
          HH.td
            [ HP.classes $ fold
                [ pure (HH.ClassName "TableElement")
                , guard (isNothing val) $> HH.ClassName "TableElement--shadow"
                ]
            ]
            [ HH.text
                if active
                then display (un Identity shadowVal)
                else maybe "" display val
            ] in
      HH.tr
          [ HP.classes $ fold
              [ pure (HH.ClassName "TableRow")
              , guard filtered $> HH.ClassName "TableRow--filtered"
              ]
          , HE.onMouseEnter (\_ -> Just (HoverRow ix))
          , HE.onMouseLeave (\_ -> Just LeaveTable)
          ]
          [ HH.td
              [ HP.class_ (HH.ClassName "TableElement") ]
              [ HH.text update.date ]
          , mkCell identity userInTime.name update.name
          , mkCell identity userInTime.birthday update.birthday
          , mkCell show userInTime.rating update.rating
          ]

personRow :: forall m. String -> Person -> Html m
personRow label person =
  HH.tr
      [ HP.class_ (HH.ClassName "TableRow") ]
      [ mkCell identity (Identity label)
      , mkCell identity person.name
      , mkCell identity person.birthday
      , mkCell show person.rating
      ]
  where
    mkCell :: forall a. (a -> String) -> Identity a -> Html m
    mkCell display val =
      HH.td
        [ HP.class_ (HH.ClassName "TableElement") ]
        [ HH.text (display (un Identity val)) ]

tableHeader :: forall m. State -> Html m
tableHeader state =
  HH.thead_
    [ HH.tr_
        [ HH.th
            [ HP.class_ (H.ClassName "TableHeader") ]
            [ HH.text "" ]
        , mkCell state.filters.name "Name"
        , mkCell state.filters.birthday "Birthday"
        , mkCell state.filters.rating "Rating"
        ]
    ]
  where
    mkCell :: forall a. Const Boolean a -> String -> Html m
    mkCell filtered label =
      HH.th
        [ HP.classes $ fold
            [ pure (H.ClassName "TableHeader")
            , guard (un Const filtered) $> H.ClassName "TableHeader--filtered"
            ]
        , HE.onClick (\_ -> Just (ToggleFilter (String.toLower label)))
        ]
        [ HH.text label ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  HoverRow ix ->
    H.modify_ (_ { hoveredRow = Just ix })
  LeaveTable ->
    H.modify_ (_ { hoveredRow = Nothing })
  ToggleFilter lbl ->
    H.modify_
      (\prev ->
        prev { filters = case lbl of
                  "name" -> prev.filters { name = not prev.filters.name }
                  "birthday" -> prev.filters { birthday = not prev.filters.birthday }
                  "rating" -> prev.filters { rating = not prev.filters.rating }
                  _ -> prev.filters
             })
