module ErrorRecord exposing (Id, Model, ErrorDetails, nextId, prevId)

-- import Dict exposing (Dict)


type alias Id =
    String


type alias Model =
    { id : Id
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


nextId : Id -> List Model -> Id
nextId id list =
    let
        nextItem =
            next list id
    in
        case nextItem of
            Just nextItem ->
                nextItem.id

            Nothing ->
                case list of
                    head :: tail ->
                        head.id

                    _ ->
                        ""


prevId : Id -> List Model -> Id
prevId id list =
    let
        prevItem =
            prev list id
    in
        case prevItem of
            Just prevItem ->
                prevItem.id

            Nothing ->
                case lastElem list of
                    Just item ->
                        item.id

                    Nothing ->
                        ""


next : List Model -> Id -> Maybe Model
next list id =
    case list of
        head :: target :: tail ->
            if head.id == id then
                Just target
            else
                next (target :: tail) id

        _ ->
            Nothing


lastElem : List a -> Maybe a
lastElem =
    List.foldl (Just >> always) Nothing


prev : List Model -> Id -> Maybe Model
prev list id =
    case list of
        head :: target :: tail ->
            if target.id == id then
                Just head
            else
                prev (target :: tail) id

        _ ->
            Nothing
