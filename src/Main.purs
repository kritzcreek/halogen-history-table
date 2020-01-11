module Main where

import Prelude

import Effect (Effect)
import HalogenHistoryTable as HHT
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI HHT.component unit body
