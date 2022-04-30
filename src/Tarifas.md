---
elm:
  dependencies:
    justinmimbs/date: latest
    elm/time: latest
    ianmackenzie/elm-units: latest
    elm-community/list-extra: latest
  source-directories:
    - ./
---

```elm {l=hidden}
import Cfe
import Date exposing (Date)
import Dict exposing (Dict)
import Energy exposing (Energy, kilowattHours)
import List.Extra as List
import Pesos as Pesos exposing (DeciCentavo)
import Quantity exposing (Quantity(..))
import Time exposing (Month(..))
import TranslateTime exposing (..)



{-
   pagaPorConsumo =
       Cfe.cobroPorDia
           { consumo = kilowattHours 525
           , dia = Date.fromCalendarDate 2022 Aug 26
           , diasDelMes = 1
           }
-}
```

# Sobre Tenera las Tarfifas a la mano

Pago por el consumo es

Y finalmente validando manualmente el cálculo de cuanto pagar:

```elm {l=hidden}
type alias Periodo =
    ( Date, Date )


diasTotales : Periodo -> Int
diasTotales periodo =
    Date.ordinalDay (Tuple.second periodo)
        - Date.ordinalDay (Tuple.first periodo)


type alias ConsumoPorMes =
    ( Month, Int )


diaCuenta : Periodo -> List Date
diaCuenta dias =
    Date.range
        Date.Day
        1
        (Tuple.first dias)
        (Tuple.second dias)


diaCuentaEner : Energy -> List Date -> List ( Int, Float )
diaCuentaEner energia listaDias =
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
                Cfe.esVeranoEsteMes Apr (Date.numberToMonth llave)
            )


diasConsumoInvierno : Dict Int ( Int, Float ) -> Dict Int ( Int, Float )
diasConsumoInvierno reparticionesDeMeses =
    reparticionesDeMeses
        |> Dict.filter
            (\llave valor ->
                not (Cfe.esVeranoEsteMes Apr (Date.numberToMonth llave))
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


mesesAcumulado : Dict Int ( Int, Float ) -> List ( Energy, Quantity Int DeciCentavo )
mesesAcumulado consu =
    let
        verano =
            diasConsumoVerano consu |> sumaDiasConsumo

        invierno =
            diasConsumoInvierno consu |> sumaDiasConsumo
    in
    case ( verano, invierno ) of
        ( ( 0, _ ), ( _, _ ) ) ->
            [ Cfe.tarifasMultiplica Cfe.tarifasInvierno1c { factor = 1 } ]

        ( ( _, _ ), ( 0, _ ) ) ->
            [ Cfe.tarifasMultiplica Cfe.tarifasVerano1c { factor = 1 } ]

        ( ( _, _ ), ( _, _ ) ) ->
            let
                proporcionVerano =
                    toFloat (Tuple.first verano)
                        / toFloat (Tuple.first verano + Tuple.first invierno)
            in
            [ Cfe.tarifasMultiplica Cfe.tarifasVerano1c { factor = proporcionVerano }
            , Cfe.tarifasMultiplica Cfe.tarifasInvierno1c { factor = 1 - proporcionVerano }
            ]



-- |> Quantity.sum
-- |> Pesos.enPesos
-- |> List.sum
```

### A ver si pude calcuar el gasto de CFE

```elm {r}
consumoDelPeriodo1 =
    kilowattHours 942


periodo1 =
    ( Date.fromCalendarDate 2021 Oct 21
    , Date.fromCalendarDate 2021 Dec 23
    )


queDias =
    diaCuenta periodo1



-- listadoIntermedio : List ( Int, Float )


listadoIntermedio =
    diaCuentaEner consumoDelPeriodo1 queDias



-- listadoQueCobrar : List Float


listadoQueCobrar =
    consumoPorPeriodo listadoIntermedio



-- listadoDiasDelMes : List Int


listadoDiasDelMes =
    diasPorPeriodo queDias



--cobro : Float
--dictCobro : Dict Int ( Int, Float )


dictCobro =
    dictDeConsumo
        listadoDiasDelMes
        listadoQueCobrar


mesesCobrar : List ( Float, Float )
mesesCobrar =
    mesesAcumulado dictCobro
        |> List.map
            (\( ener, money ) ->
                ( Energy.inKilowattHours ener
                , Pesos.enPesos money
                )
            )
```

La cantidad de días es ^^^elm m=List.length(queDias)^^^.
