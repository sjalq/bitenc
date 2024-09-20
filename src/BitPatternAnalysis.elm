module BitPatternAnalysis exposing (..)

import Array exposing (Array)
import Bitwise


type alias BitCounter =
    { zeros : Int, ones : Int }


type alias BitMapping =
    Array Char


createBitCounters : Int -> String -> Array BitCounter
createBitCounters indexBitsLength bitSeries =
    let
        updateCounter bit counter =
            case bit of
                Just '0' ->
                    { counter | zeros = counter.zeros + 1 }

                Just '1' ->
                    { counter | ones = counter.ones + 1 }

                _ ->
                    counter

        folder index acc =
            if index + indexBitsLength <= String.length bitSeries then
                let
                    pattern =
                        String.slice index (index + indexBitsLength) bitSeries
                            |> bitsToInt

                    nextBit =
                        String.slice (index + indexBitsLength) (index + indexBitsLength + 1) bitSeries
                            |> String.uncons
                            |> Maybe.map Tuple.first
                in
                Array.set pattern
                    (Array.get pattern acc
                        |> Maybe.withDefault { zeros = 0, ones = 0 }
                        |> updateCounter nextBit
                    )
                    acc

            else
                acc

        counters =
            List.range 0 (String.length bitSeries - indexBitsLength)
                |> List.foldl folder (Array.repeat (2 ^ indexBitsLength) { zeros = 0, ones = 0 })
    in
    counters


createMapping : Int -> String -> BitMapping
createMapping indexBitsLength bitSeries =
    createBitCounters indexBitsLength bitSeries
        |> Array.map
            (\counter ->
                if counter.ones > counter.zeros then
                    '1'

                else
                    '0'
            )


encodeWithMapping : Int -> String -> BitMapping -> String
encodeWithMapping indexBitsLength bitSeries mapping =
    let
        encode index =
            if index < indexBitsLength then
                String.slice index (index + 1) bitSeries

            else
                let
                    pattern =
                        String.slice (index - indexBitsLength) index bitSeries
                            |> bitsToInt

                    prediction =
                        Array.get pattern mapping |> Maybe.withDefault '0'

                    actual =
                        String.slice index (index + 1) bitSeries
                in
                if actual == String.fromChar prediction then
                    "1"

                else
                    "0"
    in
    List.range 0 (String.length bitSeries - 1)
        |> List.map encode
        |> String.concat


iterativeEncode : Int -> String -> Int -> String
iterativeEncode indexBitsLength bitSeries iterations =
    let
        iterate series count =
            if count <= 0 then
                series

            else
                let
                    mapping =
                        createMapping indexBitsLength series

                    encoded =
                        encodeWithMapping indexBitsLength series mapping
                in
                iterate encoded (count - 1)
    in
    iterate bitSeries iterations


bitsToInt : String -> Int
bitsToInt bits =
    String.foldl
        (\c acc ->
            Bitwise.shiftLeftBy 1 acc
                |> Bitwise.or
                    (if c == '1' then
                        1

                     else
                        0
                    )
        )
        0
        bits


decodeWithMapping : String -> BitMapping -> String
decodeWithMapping encodedString mapping =
    let
        indexBitsLength =
            Array.length mapping
                |> toFloat
                |> logBase 2
                |> ceiling

        initialBits =
            String.left indexBitsLength encodedString

        decodeStep : Int -> String -> String
        decodeStep index acc =
            if index >= String.length encodedString then
                acc

            else
                let
                    pattern =
                        String.right indexBitsLength acc
                            |> bitsToInt

                    prediction =
                        Array.get pattern mapping
                            |> Maybe.withDefault '0'

                    encodedBit =
                        String.slice index (index + 1) encodedString

                    nextBit =
                        if encodedBit == "1" then
                            String.fromChar prediction

                        else if prediction == '0' then
                            "1"

                        else
                            "0"
                in
                decodeStep (index + 1) (acc ++ nextBit)
    in
    decodeStep indexBitsLength initialBits
