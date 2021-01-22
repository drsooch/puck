module Puck.Utils
    ( strToPos
    , strToHand
    , strToPer
    , toiToDouble
    , doubleToToi
    ) where



import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           Puck.Types

--strToDiv :: Text -> Division
--strToDiv "Atlantic"     = Atlantic
--strToDiv "Metropolitan" = Metropolitan
--strToDiv "Central"      = Central
--strToDiv "Pacific"      = Pacific
--strToDiv _              = error "Failed String to Divison conversion"

--strToConf :: Text -> Conference
--strToConf "Eastern" = Eastern
--strToConf "Western" = Western
--strToConf _         = error "Failed String to Conference conversion"

strToPos :: Text -> Position
strToPos "L"          = Forward LW
strToPos "Left Wing"  = Forward LW
strToPos "R"          = Forward RW
strToPos "Right Wing" = Forward RW
strToPos "C"          = Forward C
strToPos "Center"     = Forward C
strToPos "D"          = Defenseman
strToPos "Defenseman" = Defenseman
strToPos "G"          = Goalie
strToPos "Goalie"     = Goalie
strToPos "N/A"        = UnknownPos
strToPos "Unknown"    = UnknownPos
strToPos x = error $ "Failed String to Position conversion: " <> show x

strToHand :: Text -> Handedness
strToHand "L"     = LeftH
strToHand "Left"  = LeftH
strToHand "R"     = RightH
strToHand "Right" = RightH
strToHand x       = error $ "Failed String to Hand conversion: " <> show x

strToPer :: Text -> Period
strToPer "1" = First
strToPer "2" = Second
strToPer "3" = Third
strToPer "4" = OT Nothing
strToPer x   = error $ "Failed String to Period conversion: " <> show x

toiToDouble :: Text -> Double
toiToDouble txt =
    let (min, sec) = T.breakOn ":" txt
    in  (read $ T.unpack min :: Double)
            + ((read $ T.unpack $ T.tail sec :: Double) / 60.0)

doubleToToi :: Double -> Text
doubleToToi num =
    -- stupid way to convert decimal number containing Minutes to a text form
    let (min, sec) = quotRem (round $ 60.0 * num) 60
    in  T.pack (show min) <> ":" <> T.pack (show sec)
