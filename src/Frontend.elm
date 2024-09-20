module Frontend exposing (..)

import Array
import BitPatternAnalysis
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Lamdera
import Random
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    ( { key = key
      , indexLength = 8
      , bitStringLength = 100
      , bitString = ""
      , processedBits = ""
      , indexPrediction = ""
      , decodedBits = ""
      , iterativeRounds = 1
      }
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case msg of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        UpdateIndexLength str ->
            ( { model | indexLength = Maybe.withDefault 8 (String.toInt str) |> clamp 0 16 }, Cmd.none )

        UpdateTestStringLength str ->
            ( { model | bitStringLength = Maybe.withDefault 100 (String.toInt str) |> clamp 1 1000 }, Cmd.none )

        RandomBitsGenerated bits ->
            ( { model | bitString = bits }, Cmd.none )

        ProcessBits ->
            let
                mapping =
                    BitPatternAnalysis.createMapping model.indexLength model.bitString

                processed =
                    List.foldl
                        (\_ acc -> BitPatternAnalysis.encodeWithMapping model.indexLength acc mapping)
                        model.bitString
                        (List.range 1 model.iterativeRounds)

                prediction =
                    Array.toList mapping |> String.fromList

                decoded =
                    List.foldl
                        (\_ acc -> BitPatternAnalysis.decodeWithMapping acc mapping)
                        processed
                        (List.range 1 model.iterativeRounds)
            in
            ( { model
                | processedBits = processed
                , indexPrediction = prediction
                , decodedBits = decoded
              }
            , Cmd.none
            )

        UpdateIterativeRounds str ->
            ( { model | iterativeRounds = Maybe.withDefault 1 (String.toInt str) |> clamp 1 10 }, Cmd.none )

        UpdateBitString str ->
            ( { model | bitString = str }, Cmd.none )

        UpdateBitStringLength string ->
            case String.toInt string of
                Just length ->
                    ( { model | bitStringLength = length }
                    , Random.generate RandomBitsGenerated (randomBitString length)
                    )

                Nothing ->
                    ( { model | bitStringLength = 0 }, Cmd.none )


randomBitString : Int -> Random.Generator String
randomBitString length =
    Random.list length (Random.uniform '0' [ '1' ])
        |> Random.map String.fromList


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Bit Pattern Analysis"
    , body =
        [ div [ Attr.style "padding" "20px", Attr.style "font-family" "sans-serif" ]
            [ h1 [] [ text "Bit Pattern Analysis" ]
            , div []
                [ label [] [ text "Bit String Length: " ]
                , input
                    [ Attr.value (String.fromInt model.bitStringLength)
                    , Events.onInput UpdateBitStringLength
                    , Attr.style "width" "300px"
                    ]
                    []
                ]
            , div [ Attr.style "margin-top" "10px" ]
                [ label [] [ text "Index Length: " ]
                , input
                    [ Attr.type_ "number"
                    , Attr.min "1"
                    , Attr.max "8"
                    , Attr.value (String.fromInt model.indexLength)
                    , Events.onInput UpdateIndexLength
                    ]
                    []
                ]
            , div [ Attr.style "margin-top" "10px" ]
                [ label [] [ text "Iterative Rounds: " ]
                , input
                    [ Attr.type_ "number"
                    , Attr.min "1"
                    , Attr.max "10"
                    , Attr.value (String.fromInt model.iterativeRounds)
                    , Events.onInput UpdateIterativeRounds
                    ]
                    []
                ]
            , button [ Events.onClick ProcessBits, Attr.style "margin-top" "10px" ] [ text "Process" ]
            , div [ Attr.style "margin-top" "20px" ]
                [ h3 [] [ text "Original Bits:" ]
                , viewBits model.bitString
                , h3 [] [ text "Processed Bits:" ]
                , viewBits model.processedBits
                ]
            , div [ Attr.style "margin-top" "20px" ]
                [ h3 [] [ text "Index Prediction:" ]
                , viewBits model.indexPrediction
                ]
            , div [ Attr.style "margin-top" "20px" ]
                [ h3 [] [ text "Decoded Bits:" ]
                , viewBits model.decodedBits
                ]
            ]
        ]
    }


viewBits : String -> Html FrontendMsg
viewBits bits =
    div []
        [ div [ Attr.style "font-family" "monospace", Attr.style "font-size" "14px", Attr.style "letter-spacing" "2px" ]
            (String.toList bits
                |> List.map
                    (\c ->
                        span
                            [ Attr.style "color"
                                (if c == '0' then
                                    "green"
                                 else
                                    "red"
                                )
                            ]
                            [ text (String.fromChar c) ]
                    )
            )
        , div [ Attr.style "margin-top" "5px" ]
            [ text ("Number of zeros: " ++ String.fromInt (countZeros bits)) ]
        ]

countZeros : String -> Int
countZeros str =
    String.filter ((==) '0') str |> String.length
