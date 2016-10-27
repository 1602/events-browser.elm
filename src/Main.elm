module Main exposing (..)

import Html exposing (Html, div, button, text, pre, ul, li, span, a, input)
import Html.App
import Components.ErrorRecordList as ErrorRecordList
import Components.Filters as Filters exposing (filters, visibleRecords)
import Components.Autocomplete as Autocomplete
import Keyboard


-- MODEL


type alias Model =
    { records : ErrorRecordList.Model
    , filter : Filters.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( list, command ) =
            ErrorRecordList.init

        filter =
            Filters.init
    in
        ( Model list filter
        , Cmd.map ListMessages command
        )



-- VIEW


view : Model -> Html Msg
view model =
    let
        records =
            ErrorRecordList.applyFilter (visibleRecords model.filter) model.records

        list =
            Html.App.map ListMessages (ErrorRecordList.render model.records records)
    in
        div []
            [ Filters.filters model.filter records FiltersMessages
            , list
            ]



-- UPDATE


type Msg
    = NoOp
    | ListMessages ErrorRecordList.Msg
    | FiltersMessages Filters.FilterType Autocomplete.Msg
    | KeyMsg Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            model ! []

        ListMessages msg ->
            let
                ( records, cmd ) =
                    ErrorRecordList.update msg model.records
            in
                { model | records = records } ! [ Cmd.map ListMessages cmd ]

        FiltersMessages filterType msg ->
            { model | filter = Filters.update filterType msg model.filter } ! []

        KeyMsg code ->
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
