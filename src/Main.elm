module Main exposing (..)

import Dom
import Char
import Util exposing (..)
import Html exposing (Html, div, button, text, pre, ul, li, span, a, input)
import Html.Events exposing (onClick, onInput, onFocus)
import Html.Attributes as Attr exposing (style, class, value, property)
import Html.App
import Task exposing (Task)
import Components.ErrorRecordList as ErrorRecordList
import Components.Filters as Filters exposing (filters, visibleRecords)
import Messages exposing (Msg(..), FilterType(..))
import Keyboard


-- MODEL


type alias Model =
    { records : ErrorRecordList.Model
    , error : String
    , filter : Filters.Model
    , focus : String
    }


init : ( Model, Cmd Msg )
init =
    let
        ( list, command )
    ( Model ErrorRecordList.init
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


fetchCmd : Cmd Msg
fetchCmd =
    Task.perform FetchError FetchSuccess ErrorRecord.fetchErrors



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
                                    |> ErrorRecord.nextId model.selectedRecordId
                        in
                            update (SelectErrorRecord id) model

                    'K' ->
                        let
                            id =
                                visibleRecords model.filter model.records
                                    |> ErrorRecord.prevId model.selectedRecordId
                        in
                            update (SelectErrorRecord id) model

                    _ ->
                        let
                            a =
                                Debug.log "key pressed" code
                        in
                            model ! []



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
