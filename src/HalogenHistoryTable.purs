module HalogenHistoryTable where

import Prelude

import Control.MonadPlus (guard)
import Data.Array as Array
import Data.Const (Const(..))
import Data.Foldable (fold, foldl)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Newtype (un)
import Data.String as String
import Data.Variant (Variant)
import Data.Variant as V
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row as Row
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

type PersonR f =
  ( birthday :: f String
  , name :: f String
  , rating :: f Int
  )

type Begone a = a
type Person = Record (PersonR Begone)
type PersonUpdate = Record (date :: String | PersonR Maybe)
type Filters = Record (PersonR (Const Boolean))
type FilterToggle = Variant (PersonR (Const Boolean))

applyUpdate :: Person -> PersonUpdate -> Person
applyUpdate person update =
  { birthday: fromMaybe person.birthday update.birthday
  , name: fromMaybe person.name update.name
  , rating: fromMaybe person.rating update.rating
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
  { birthday: "01.01.1970"
  , name: "Carl"
  , rating: 1
  }

updates :: Array PersonUpdate
updates =
  [ (emptyUpdate "09.01.2020") { rating = Just 9 }
  , (emptyUpdate "09.01.2020") { name = Just "Christoph" }
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
  | ToggleFilter FilterToggle

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
        mkCell :: forall a. (a -> String) -> a -> Maybe a -> Html m
        mkCell display shadowVal val =
          HH.td
            [ HP.classes $ fold
                [ pure (HH.ClassName "TableElement")
                , guard (isNothing val) $> HH.ClassName "TableElement--shadow"
                ]
            ]
            [ HH.text
                if active
                then display shadowVal
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
      [ mkCell label
      , mkCell person.name
      , mkCell person.birthday
      , mkCell (show person.rating)
      ]
  where
    mkCell text =
      HH.td
        [ HP.class_ (HH.ClassName "TableElement") ]
        [ HH.text text ]

tableHeader :: forall m. State -> Html m
tableHeader state =
  HH.thead_
    [ HH.tr_
        [ HH.th
            [ HP.class_ (H.ClassName "TableHeader") ]
            [ HH.text "" ]
        , mkCell state.filters.name (SProxy::_"name")
        , mkCell state.filters.birthday (SProxy::_"birthday")
        , mkCell state.filters.rating (SProxy::_"rating")
        ]
    ]
  where
    mkCell
      :: forall a _b sym
      . IsSymbol sym
      => Row.Cons sym (Const Boolean a) _b (PersonR (Const Boolean))
      => Const Boolean a
      -> SProxy sym
      -> Html m
    mkCell filtered label =
      HH.th
        [ HP.classes $ fold
            [ pure (H.ClassName "TableHeader")
            , guard (un Const filtered) $> H.ClassName "TableHeader--filtered"
            ]
        , HE.onClick (\_ -> Just (ToggleFilter (V.inj label (not filtered))))
        ]
        [ HH.text (String.toUpper (reflectSymbol label)) ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  HoverRow ix ->
    H.modify_ (_ { hoveredRow = Just ix })
  LeaveTable ->
    H.modify_ (_ { hoveredRow = Nothing })
  ToggleFilter f -> do
    { filters } <- H.get
    let
      newFilters = V.match
        { name: filters { name = _ }
        , birthday: filters { birthday = _ }
        , rating: filters { rating = _ }
        } f
    H.modify_ (_ { filters = newFilters})
