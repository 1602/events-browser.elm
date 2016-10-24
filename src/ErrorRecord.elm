module ErrorRecord exposing (..)


type alias ErrorRecordId =
    String


type alias ErrorRecord =
    { id : ErrorRecordId
    , lastOccurrence : String
    , occurrences : Int
    , details : List ErrorDetails
    , message : String
    , stack : Maybe String
    , kind : String
    , pid : String
    , host : String
    , cwd : String
    , app : String
    , env : String
    }


type alias ErrorDetails =
    { timestamp : Int
    }
