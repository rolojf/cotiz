module TranslateTime exposing (..)

import Time exposing (Month(..))


toSpanishWeekday : Time.Weekday -> String
toSpanishWeekday weekday =
    case weekday of
        Time.Mon ->
            "Lun"

        Time.Tue ->
            "Mar"

        Time.Wed ->
            "Mie"

        Time.Thu ->
            "Jue"

        Time.Fri ->
            "Vie"

        Time.Sat ->
            "Sab"

        Time.Sun ->
            "Dom"


toSpanishMonth : Month -> String
toSpanishMonth month =
    case month of
        Jan ->
            "Ene"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Abr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Ago"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dic"
