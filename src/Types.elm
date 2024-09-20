module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , indexLength : Int
    , bitStringLength : Int
    , bitString : String
    , processedBits : String
    , indexPrediction : String
    , decodedBits : String
    , iterativeRounds : Int
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | UpdateIndexLength String
    | UpdateTestStringLength String
    | RandomBitsGenerated String
    | ProcessBits
    | NoOpFrontendMsg
    | UpdateBitString String
    | UpdateBitStringLength String
    | UpdateIterativeRounds String


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
