module Main exposing (..)

import Dom
import Char
import Util exposing (..)
import Html exposing (Html, div, button, text, pre, ul, li, span, a, input)
import Html.Events exposing (onClick, onInput, onFocus)
import Html.Attributes as Attr exposing (style, class, value, property)
import Html.App
import Http
import Task exposing (Task)
import Json.Decode as Decode
import ErrorRecord exposing (..)
import ErrorRecordsDecoder exposing (..)
import Components.ErrorRecordOverview exposing (viewErrorRecord, ViewErrorRecordProps)
import Components.Filters as Filters exposing (filters, visibleRecords)
import Messages exposing (Msg(..), FilterType(..))
import Keyboard


-- MODEL


type alias Model =
    { records : List ErrorRecord
    , error : String
    , selectedRecordId : ErrorRecordId
    , filter : Filters.Model
    , focus : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model []
        ""
        ""
        Filters.init
        ""
    , fetchCmd
    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        listStyle =
            [ "padding" => "0", "margin" => "0" ]

        records =
            visibleRecords model.filter model.records

        listItems =
            records
                |> List.map
                    (\record ->
                        viewErrorRecord
                            { isActive = record.id == model.selectedRecordId
                            , aboutToDelete = False
                            }
                            record
                    )
    in
        div []
            [ div [ style [ "display" => "flex", "align-items" => "center" ] ]
                [ button [ onClick Fetch ] [ text "Fetch" ]
                , text model.error
                ]
            , filters model.filter records UpdateFilters
            , ul [ style listStyle ] listItems
            ]


decode : Decode.Decoder (List ErrorRecord)
decode =
    Decode.at [ "errors" ] (Decode.list decodeError)


url : String
url =
    "http://errors.ub.io/errors"


fetchTask : Task Http.Error (List ErrorRecord)
fetchTask =
    Http.get decode url


fetchCmd : Cmd Msg
fetchCmd =
    Task.perform FetchError FetchSuccess fetchTask



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            model ! []

        Fetch ->
            model ! [ fetchCmd ]

        FetchSuccess records ->
            { model | records = records } ! []

        FetchError error ->
            { model | error = (toString error) } ! []

        FocusOn id ->
            let
                focus =
                    Dom.focus id
            in
                model ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) focus ]

        UpdateFilters filterType msg ->
            { model | filter = Filters.update filterType msg model.filter } ! []

        SelectErrorRecord errorRecordId ->
            update (FocusOn errorRecordId) { model | selectedRecordId = errorRecordId }

        KeyMsg code ->
            if model.filter.app.expanded || model.filter.env.expanded || model.filter.message.expanded then
                model ! []
            else
                case Char.fromCode code of
                    'J' ->
                        let
                            id =
                                visibleRecords model.filter model.records
                                    |> nextId model.selectedRecordId
                        in
                            update (SelectErrorRecord id) model

                    'K' ->
                        let
                            id =
                                visibleRecords model.filter model.records
                                    |> prevId model.selectedRecordId
                        in
                            update (SelectErrorRecord id) model

                    _ ->
                        let
                            a =
                                Debug.log "key pressed" code
                        in
                            model ! []


nextId : ErrorRecordId -> List ErrorRecord -> ErrorRecordId
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



-- Autocomplete.update


prevId : ErrorRecordId -> List ErrorRecord -> ErrorRecordId
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


next : List ErrorRecord -> ErrorRecordId -> Maybe ErrorRecord
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


prev : List ErrorRecord -> ErrorRecordId -> Maybe ErrorRecord
prev list id =
    case list of
        head :: target :: tail ->
            if target.id == id then
                Just head
            else
                prev (target :: tail) id

        _ ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg
        ]



-- MAIN


main : Program Never
main =
    --TimeTravel.program
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
