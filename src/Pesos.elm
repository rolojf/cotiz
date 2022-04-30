module Pesos exposing
    ( DeciCentavo(..)
    , deciCentavos
    , enDeciCentavos
    , enPesos
    , monto
    , pesos
    )

import Quantity exposing (Quantity(..))


type DeciCentavo
    = DeciCentavo


monto : Int -> Int -> Quantity Int DeciCentavo
monto numPesos numCents =
    Quantity (numPesos * 1000 + numCents)


deciCentavos : Int -> Quantity Int DeciCentavo
deciCentavos numCents =
    Quantity numCents


enDeciCentavos : Quantity Int DeciCentavo -> Int
enDeciCentavos (Quantity numDeciCents) =
    numDeciCents


pesos : Int -> Quantity Int DeciCentavo
pesos numPesos =
    Quantity (1000 * numPesos)


enPesos : Quantity Int DeciCentavo -> Float
enPesos (Quantity numDeciCents) =
    toFloat numDeciCents / 1000
