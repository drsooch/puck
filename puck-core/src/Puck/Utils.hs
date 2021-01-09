module Puck.Utils where

import           Data.Text                      ( Text )
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
strToPos "LW" = Forward LW
strToPos "RW" = Forward RW
strToPos "C"  = Forward C
strToPos "D"  = Defenseman
strToPos "G"  = Goalie
strToPos _    = error "Failed String to Position conversion"

strToHand :: Text -> Handedness
strToHand "L"     = LeftH
strToHand "Left"  = LeftH
strToHand "R"     = RightH
strToHand "Right" = RightH
strToHand x       = error $ "Failed String to Position conversion: " ++ show x

strToPer :: Text -> Period
strToPer "1" = First
strToPer "2" = Second
strToPer "3" = Third
strToPer "4" = OT Nothing
strToPer x   = error $ "Failed String to Period conversion: " ++ show x
