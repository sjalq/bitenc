module BitPatternAnalysisTest exposing (..)

import Array
import BitPatternAnalysis exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "BitPatternAnalysis"
        [ testCreateBitCounters
        , testCreateMapping
        , testEncodeWithMapping
        , testDecodeWithMapping
        , testIterativeEncode
        ]


testCreateBitCounters : Test
testCreateBitCounters =
    test "createBitCounters creates correct bit counters" <|
        \_ ->
            let
                indexBitsLength =
                    2

                bitSeries =
                    "01101001"

                expectedCounters =
                    Array.fromList
                        [ { zeros = 0, ones = 1 } -- 00
                        , { zeros = 1, ones = 1 } -- 01
                        , { zeros = 1, ones = 1 } -- 10
                        , { zeros = 1, ones = 0 } -- 11
                        ]
            in
            Expect.equal expectedCounters (createBitCounters indexBitsLength bitSeries)


testCreateMapping : Test
testCreateMapping =
    test "createMapping creates correct mapping" <|
        \_ ->
            let
                indexBitsLength =
                    2

                bitSeries =
                    "01101001"

                --ind -> 0s1s
                -- 00 -> 0 1 -> 1
                -- 01 -> 1 1 -> 0
                -- 10 -> 1 1 -> 0
                -- 11 -> 1 0 -> 0
                expectedMapping =
                    Array.fromList [ '1', '0', '0', '0' ]
            in
            Expect.equal expectedMapping (createMapping indexBitsLength bitSeries)


testEncodeWithMapping : Test
testEncodeWithMapping =
    test "encodeWithMapping encodes correctly" <|
        \_ ->
            let
                indexBitsLength =
                    2

                bitSeries =
                    "01101001"

                --  "01010111"
                mapping =
                    Array.fromList [ '1', '0', '0', '0' ]

                expectedEncoded =
                    "01010111"
            in
            Expect.equal expectedEncoded (encodeWithMapping indexBitsLength bitSeries mapping)


testDecodeWithMapping : Test
testDecodeWithMapping =
    test "decodeWithMapping decodes correctly" <|
        \_ ->
            let
                encodedSeries =
                    "01010111"

                mapping =
                    Array.fromList [ '1', '0', '0', '0' ]

                expectedDecoded =
                    "01101001"
            in
            Expect.equal expectedDecoded (decodeWithMapping encodedSeries mapping)


testIterativeEncode : Test
testIterativeEncode =
    test "iterativeEncode encodes correctly for multiple iterations" <|
        \_ ->
            let
                indexBitsLength =
                    2

                bitSeries =
                    "01101001"

                iterations =
                    2

                expectedEncoded =
                    "01010111"
            in
            Expect.equal expectedEncoded (iterativeEncode indexBitsLength bitSeries iterations)
