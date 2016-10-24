module ErrorRecordsDecoder exposing(..)
-- import Dict exposing (Dict)
import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import ErrorRecord exposing (..)

decodeErrorDetails : Json.Decode.Decoder ErrorDetails
decodeErrorDetails = 
    Json.Decode.object1 ErrorDetails ("timestamp" := Json.Decode.int)

decodeError : Json.Decode.Decoder ErrorRecord
decodeError =
    Json.Decode.succeed ErrorRecord
        |: ("id" := Json.Decode.string)
        |: ("lastOccurrence" := Json.Decode.string)
        |: ("occurrences" := Json.Decode.int)
        |: ("details" := Json.Decode.list decodeErrorDetails)
        |: ("message" := Json.Decode.string)
        |: (Json.Decode.maybe ("stack" := Json.Decode.string))
        |: ("type" := Json.Decode.string)
        |: ("pid" := Json.Decode.string)
        |: ("host" := Json.Decode.string)
        |: ("cwd" := Json.Decode.string)
        |: ("app" := Json.Decode.string)
        |: ("env" := Json.Decode.string)
