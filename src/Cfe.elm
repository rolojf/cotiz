module Cfe exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Energy exposing (Energy, kilowattHours)
import List.Extra as List
import Pesos as Pesos exposing (DeciCentavo)
import Quantity exposing (Quantity(..))
import Time exposing (Month(..))


type alias TarifaDeEstimulo =
    List ( Energy, Quantity Int DeciCentavo )


tarifasVerano1c : TarifaDeEstimulo
tarifasVerano1c =
    [ ( kilowattHours 0, Pesos.deciCentavos 887 )
    , ( kilowattHours 150, Pesos.deciCentavos 1079 )
    , ( kilowattHours 300, Pesos.deciCentavos 1198 )
    , ( kilowattHours 450, Pesos.deciCentavos 3191 )
    ]


tarifasInvierno1c : TarifaDeEstimulo
tarifasInvierno1c =
    [ ( kilowattHours 0, Pesos.deciCentavos 887 )
    , ( kilowattHours 75, Pesos.deciCentavos 1079 )
    , ( kilowattHours 175, Pesos.deciCentavos 3153 )
    ]


tarifasMultiplica : { factor : Float } -> TarifaDeEstimulo -> TarifaDeEstimulo
tarifasMultiplica { factor } tarifas =
    tarifas
        |> List.map
            (\( ener, cant ) ->
                ( Quantity.multiplyBy factor ener
                , cant
                )
            )


cobroPorDia : { consumo : Energy, tarifas : TarifaDeEstimulo } -> Quantity Int DeciCentavo
cobroPorDia { consumo, tarifas } =
    let
        extraeRevisa : Float -> TarifaDeEstimulo -> Float
        extraeRevisa talConsumo queTarifas =
            case List.head queTarifas of
                Nothing ->
                    0

                Just ( energia, dCentavos ) ->
                    (if talConsumo >= Energy.inKilowattHours energia then
                        (talConsumo
                            - Energy.inKilowattHours energia
                        )
                            * (toFloat <|
                                Pesos.enDeciCentavos dCentavos
                              )

                     else
                        0.0
                    )
                        + (case List.tail queTarifas of
                            Nothing ->
                                0.0

                            Just colaDelListado ->
                                extraeRevisa
                                    (if Energy.inKilowattHours energia <= talConsumo then
                                        Energy.inKilowattHours energia

                                     else
                                        talConsumo
                                    )
                                    colaDelListado
                          )
    in
    tarifas
        |> List.reverse
        |> extraeRevisa (Energy.inKilowattHours consumo)
        |> round
        |> Pesos.deciCentavos


esVeranoEsteMes : Month -> Month -> Bool
esVeranoEsteMes primerMesDeVerano mes =
    case mes of
        Jan ->
            False

        Feb ->
            False

        Mar ->
            if primerMesDeVerano == Mar then
                True

            else
                False

        Apr ->
            if primerMesDeVerano == Mar || primerMesDeVerano == Apr then
                True

            else
                False

        May ->
            True

        Jun ->
            True

        Jul ->
            True

        Aug ->
            True

        Sep ->
            if primerMesDeVerano == Mar then
                False

            else
                True

        Oct ->
            if primerMesDeVerano == Apr || primerMesDeVerano == Mar then
                False

            else
                True

        Nov ->
            False

        Dec ->
            False


type alias Periodo =
    ( Date, Date )


diaCuenta : Periodo -> List Date
diaCuenta dias =
    Date.range
        Date.Day
        1
        (Tuple.first dias)
        (Tuple.second dias)


repartirEnergiaEnDias : Energy -> List Date -> List ( Int, Float )
repartirEnergiaEnDias energia listaDias =
    listaDias
        |> List.map
            (\dia ->
                ( Date.monthNumber dia
                , energia
                    |> Quantity.divideBy (toFloat (List.length listaDias))
                    |> Energy.inKilowattHours
                )
            )


consumoPorPeriodo : List ( Int, Float ) -> List Float
consumoPorPeriodo dias =
    let
        funcionFM : Int -> ( Int, Float ) -> Maybe Float
        funcionFM mesProbar ( mes, leToca ) =
            if mesProbar == mes then
                Just leToca

            else
                Nothing
    in
    List.range 1 12
        |> List.map
            (\mesNum ->
                List.filterMap
                    (funcionFM mesNum)
                    dias
                    |> List.sum
            )


diasPorPeriodo : List Date -> List Int
diasPorPeriodo dias =
    let
        funcionFM : Int -> Date -> Maybe Int
        funcionFM mesProbar fecha =
            if mesProbar == Date.monthNumber fecha then
                Just 1

            else
                Nothing
    in
    List.range 1 12
        |> List.map
            (\mesNum ->
                List.filterMap
                    (funcionFM mesNum)
                    dias
                    |> List.sum
            )


dictDeConsumo : List Int -> List Float -> Dict Int ( Int, Float )
dictDeConsumo diasQueConsumio consumoDelMes =
    List.zip diasQueConsumio consumoDelMes
        |> List.indexedMap
            (\mesNumero ( dias, consumo ) ->
                ( mesNumero + 1, ( dias, consumo ) )
            )
        |> Dict.fromList


diasConsumoVerano : Dict Int ( Int, Float ) -> Dict Int ( Int, Float )
diasConsumoVerano reparticionesDeMeses =
    reparticionesDeMeses
        |> Dict.filter
            (\llave valor ->
                esVeranoEsteMes Apr (Date.numberToMonth llave)
            )


diasConsumoInvierno : Dict Int ( Int, Float ) -> Dict Int ( Int, Float )
diasConsumoInvierno reparticionesDeMeses =
    reparticionesDeMeses
        |> Dict.filter
            (\llave valor ->
                not (esVeranoEsteMes Apr (Date.numberToMonth llave))
            )


sumaDiasConsumo : Dict Int ( Int, Float ) -> ( Int, Float )
sumaDiasConsumo consumoTemporada =
    let
        --(k -> v -> b -> b)
        reductor : Int -> ( Int, Float ) -> ( Int, Float ) -> ( Int, Float )
        reductor _ valor acumulante =
            ( Tuple.first valor + Tuple.first acumulante
            , Tuple.second valor + Tuple.second acumulante
            )
    in
    Dict.foldl
        reductor
        ( 0, 0.0 )
        consumoTemporada


mesesAcumulado : Dict Int ( Int, Float ) -> List TarifaDeEstimulo
mesesAcumulado consu =
    let
        verano =
            diasConsumoVerano consu |> sumaDiasConsumo

        invierno =
            diasConsumoInvierno consu |> sumaDiasConsumo
    in
    case ( verano, invierno ) of
        ( ( 0, _ ), ( _, _ ) ) ->
            [ tarifasMultiplica { factor = 2 } tarifasInvierno1c ]

        ( ( _, _ ), ( 0, _ ) ) ->
            [ tarifasMultiplica { factor = 2 } tarifasVerano1c ]

        ( ( _, _ ), ( _, _ ) ) ->
            let
                proporcionVerano =
                    toFloat (Tuple.first verano)
                        / toFloat (Tuple.first verano + Tuple.first invierno)
            in
            [ tarifasMultiplica { factor = proporcionVerano } tarifasVerano1c
            , tarifasMultiplica { factor = 1 - proporcionVerano } tarifasInvierno1c
            ]
