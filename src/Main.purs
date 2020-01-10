module Main where

import Prelude

import Effect (Effect)
import HalogenHistoryView as HHV
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI HHV.component unit body
