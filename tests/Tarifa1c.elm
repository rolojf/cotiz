module Tarifa1c exposing (..)

import Cfe
import Date exposing (Date)
import Debug
import Energy exposing (Energy, kilowattHours)
import Expect
import Fuzz exposing (Fuzzer, int, list, string)
import Pesos as Pesos exposing (DeciCentavo)
import Quantity exposing (Quantity(..))
import Test exposing (..)
import Time exposing (Month(..))
import TranslateTime exposing (..)


uno =
    test
        "Precio de Verano"
        (\_ ->
            Cfe.cobroPorDia
                { consumo = kilowattHours 525
                , tarifas = Cfe.tarifasVerano1c
                }
                |> Pesos.enPesos
                |> Expect.within
                    (Expect.Absolute 0.02)
                    ((525 - 450) * 3.191 + 150 * 1.198 + 150 * 1.079 + 150 * 0.887)
        )


dos =
    test
        "Precio de Invierno"
        (\_ ->
            Cfe.cobroPorDia
                { consumo = kilowattHours 525
                , tarifas = Cfe.tarifasInvierno1c
                }
                |> Pesos.enPesos
                |> Expect.within
                    (Expect.Absolute 0.02)
                    ((525 - 175) * 3.153 + 100 * 1.079 + 75 * 0.887)
        )


tres =
    test
        "Meses de Verano empezando Marzo"
        (\_ ->
            [ ( Mar, Feb )
            , ( Mar, Mar )
            , ( Mar, Apr )
            , ( Mar, May )
            , ( Mar, Sep )
            , ( Mar, Oct )
            , ( Mar, Nov )
            , ( Apr, Mar )
            , ( Apr, Apr )
            , ( Apr, May )
            , ( Apr, Sep )
            , ( Apr, Oct )
            , ( May, Mar )
            , ( May, Apr )
            , ( May, May )
            , ( May, Sep )
            , ( May, Oct )
            , ( May, Nov )
            ]
                |> List.map (\( a, b ) -> Cfe.esVeranoEsteMes a b)
                |> Expect.equalLists
                    [ False
                    , True
                    , True
                    , True
                    , False
                    , False
                    , False

                    -- Abril
                    , False
                    , True
                    , True
                    , True
                    , False

                    -- Mayo
                    , False
                    , False
                    , True
                    , True
                    , True
                    , False
                    ]
        )


consPer1 =
    kilowattHours 942


per1 =
    ( Date.fromCalendarDate 2021 Jun 12
    , Date.fromCalendarDate 2021 Aug 10
    )


per2 =
    ( Date.fromCalendarDate 2021 Aug 29
    , Date.fromCalendarDate 2021 Nov 2
    )


per3 =
    ( Date.fromCalendarDate 2021 Oct 21
    , Date.fromCalendarDate 2021 Dec 23
    )


per4 =
    ( Date.fromCalendarDate 2021 Dec 12
    , Date.fromCalendarDate 2022 Feb 10
    )


per5 =
    ( Date.fromCalendarDate 2022 Mar 14
    , Date.fromCalendarDate 2022 May 16
    )


periodos =
    [ per1, per2, per3, per4, per5 ]


cuatro =
    let
        variasDC =
            List.map
                Cfe.diaCuenta
                periodos

        diasPer =
            List.map
                List.length
                variasDC
    in
    test
        "Dias del periodo"
        (\_ ->
            diasPer
                |> Expect.equal
                    [ 59, 65, 63, 60, 63 ]
        )


cinco =
    let
        energia =
            consPer1

        diasCta =
            Cfe.diaCuenta per2

        reparticion =
            diasCta
                |> Cfe.repartirEnergiaEnDias
                    energia

        consPorMes =
            reparticion
                |> Cfe.consumoPorPeriodo

        diasQueConsumio =
            diasCta
                |> Cfe.diasPorPeriodo

        dictDConsumo =
            Cfe.dictDeConsumo
                diasQueConsumio
                consPorMes

        verFacil listado =
            listado
                |> List.map
                    (\( energ, precio ) ->
                        ( Energy.inKilowattHours energ
                        , Pesos.enPesos precio
                        )
                    )

        mesesAcum =
            Cfe.mesesAcumulado dictDConsumo

        mesesAcumPP =
            List.map
                verFacil
                mesesAcum
                |> Debug.log "Tarifas acumuladas "

        sumDConsVer =
            dictDConsumo
                |> Cfe.diasConsumoVerano
                |> Cfe.sumaDiasConsumo

        sumDConsInv =
            dictDConsumo
                |> Cfe.diasConsumoInvierno
                |> Cfe.sumaDiasConsumo

        _ =
            Debug.log
                "sumDConsumo"
                sumDConsVer
    in
    describe "Ver consolidados Verano e Invierno"
        [ test "que cuadren los dias"
            (\_ ->
                (Tuple.first sumDConsVer
                    + Tuple.first sumDConsInv
                )
                    |> Expect.equal (List.length diasCta)
            )
        , test "que cuadre la energia"
            (\_ ->
                (Tuple.second sumDConsVer
                    + Tuple.second sumDConsInv
                )
                    |> Expect.within
                        (Expect.Absolute 0.1)
                        (Energy.inKilowattHours energia)
            )
        ]


seis =
    test
        "validar tarifas multiplica"
        (\_ ->
            Cfe.tarifasVerano1c
                |> Cfe.tarifasMultiplica
                    { factor = 2 }
                |> List.map
                    (\( energia, tarifaTal ) ->
                        ( Energy.inKilowattHours energia
                        , Pesos.enPesos tarifaTal
                        )
                    )
                |> List.drop 2
                |> List.head
                |> (\valPasado ->
                        case valPasado of
                            Just ( valorado, _ ) ->
                                valorado

                            Nothing ->
                                0
                   )
                |> Expect.within
                    (Expect.Absolute 0.2)
                    600
        )


suite : Test
suite =
    describe "Comprobando bien calculado el cobro por día"
        [ uno
        , dos
        , tres
        , cuatro
        , cinco
        , seis
        ]
