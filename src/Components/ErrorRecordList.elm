module Components.ErrorRecordList
    exposing
        ( Model
        , init
        , render
        , update
        , applyFilter
        , Msg
        )

-- import Char
-- import Keyboard

import Util exposing (..)
import Task exposing (Task)
import Util exposing ((=>))
import Html exposing (Html, div, button, text, pre, ul, li, span, a, input)
import Html.Events exposing (onClick)
import Html.Attributes as Attr exposing (style, property, id, tabindex, class, value, property)
import Html.App
import Http
import Dom
import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import ErrorRecord


--import Components.Filters as Filters

import Components.ErrorRecordOverview as ErrorRecordOverview


type alias Model =
    { list : List ErrorRecord.Model
    , selectedItemId : ErrorRecord.Id
    , error : String
    }


type Msg
    = NoOp
    | Fetch
    | FetchSuccess (List ErrorRecord.Model)
    | FetchError Http.Error
    | Select ErrorRecord.Id
    | FocusOn String



-- | ErrorRecordOverviewMessage ErrorRecordOverview.Msg
--| KeyMsg Keyboard.KeyCode


init : ( Model, Cmd Msg )
init =
    ( { list = []
      , selectedItemId = ""
      , error = ""
      }
    , fetchCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        NoOp ->
            model ! []

        Fetch ->
            model ! [ fetchCmd ]

        FetchSuccess list ->
            { model | list = list } ! []

        FetchError error ->
            { model | error = (toString error) } ! []

        Select errorRecordId ->
            update (FocusOn errorRecordId) { model | selectedItemId = errorRecordId }

        FocusOn id ->
            let
                focus =
                    Dom.focus id
            in
                model ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) focus ]



-- ErrorRecordOverviewMessage msg ->
--model ! []
{-
   KeyMsg code ->
       -- if model.filter.app.expanded || model.filter.env.expanded || model.filter.message.expanded then
       -- model ! []
       -- else
       case Char.fromCode code of
           'J' ->
               let
                   id =
                       Filters.visibleRecords model.filter model.records.list
                           |> ErrorRecord.nextId model.selectedItemId
               in
                   update (SelectErrorRecord id) model

           'K' ->
               let
                   id =
                       Filters.visibleRecords model.filter model.records.list
                           |> ErrorRecord.prevId model.selectedItemId
               in
                   update (SelectErrorRecord id) model

           _ ->
               let
                   a =
                       Debug.log "key pressed" code
               in
                   model ! []

   _ ->
       model ! []
-}


applyFilter : (List ErrorRecord.Model -> List ErrorRecord.Model) -> Model -> List ErrorRecord.Model
applyFilter filter model =
    model.list
        |> filter


render : Model -> List ErrorRecord.Model -> Html Msg
render model records =
    let
        listStyle =
            [ "padding" => "0", "margin" => "0" ]

        listItems =
            records
                |> List.map
                    (\record ->
                        li
                            [ id record.id, tabindex 1, style css.listItem, onClick (Select record.id) ]
                            [ --Html.App.map ErrorRecordOverviewMessage
                              (ErrorRecordOverview.viewErrorRecord
                                { isActive = record.id == model.selectedItemId
                                , aboutToDelete = False
                                }
                                record
                              )
                            ]
                    )
    in
        div []
            [ div [ style [ "display" => "flex", "align-items" => "center" ] ]
                [ button [ onClick Fetch ] [ text "Fetch" ]
                , text model.error
                ]
            , ul [ style listStyle ] listItems
            ]


css :
    { listItem : InlineStyle
    }
css =
    { listItem =
        [ "outline" => "0"
        , "position" => "relative"
        , "list-style" => "none"
        , "margin" => "5px 0 5px 5px"
        , "display" => "flex"
        ]
    }


fetchCmd : Cmd Msg
fetchCmd =
    Task.perform FetchError FetchSuccess fetchErrors


decode : Json.Decode.Decoder (List ErrorRecord.Model)
decode =
    Json.Decode.at [ "errors" ] (Json.Decode.list decodeError)


fetchErrors : Task Http.Error (List ErrorRecord.Model)
fetchErrors =
    Http.get
        (Json.Decode.at [ "errors" ] (Json.Decode.list decodeError))
        "http://errors.ub.io/errors"


decodeErrorDetails : Json.Decode.Decoder ErrorRecord.ErrorDetails
decodeErrorDetails =
    Json.Decode.object1 ErrorRecord.ErrorDetails ("timestamp" := Json.Decode.int)


decodeError : Json.Decode.Decoder ErrorRecord.Model
decodeError =
    Json.Decode.succeed ErrorRecord.Model
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
