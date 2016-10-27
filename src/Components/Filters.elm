module Components.Filters exposing (filters, Model, init, update, visibleRecords, FilterType)

import Set
import String
import Util exposing (..)
import Html exposing (Html, div)
import ErrorRecord
import Html.Attributes as Attr exposing (style)
import Components.Autocomplete as Autocomplete exposing (autocomplete)


type alias Model =
    { app : Autocomplete.Model
    , message : Autocomplete.Model
    , env : Autocomplete.Model
    }


type FilterType
    = FilterApp
    | FilterMessage
    | FilterEnvironment


init : Model
init =
    { app = Autocomplete.init
    , message = Autocomplete.init
    , env = Autocomplete.init
    }


visibleRecords : Model -> List ErrorRecord.Model -> List ErrorRecord.Model
visibleRecords filter collection =
    let
        app =
            String.toLower filter.app.value

        message =
            String.toLower filter.message.value

        env =
            String.toLower filter.env.value
    in
        collection
            |> List.filter
                (\r ->
                    String.contains message (String.toLower r.message)
                        && String.contains app (String.toLower r.app)
                        && String.contains env (String.toLower r.env)
                )


update : FilterType -> Autocomplete.Msg -> Model -> Model
update filterType msg oldFilter =
    case filterType of
        FilterApp ->
            { oldFilter | app = Autocomplete.update msg oldFilter.app }

        FilterMessage ->
            { oldFilter | message = Autocomplete.update msg oldFilter.message }

        FilterEnvironment ->
            { oldFilter | env = Autocomplete.update msg oldFilter.env }


filters : Model -> List ErrorRecord.Model -> (FilterType -> Autocomplete.Msg -> a) -> Html a
filters filter records msg =
    let
        collect fn =
            records
                |> List.map fn
                |> Set.fromList
                |> Set.toList
                |> List.take 10

        { app, message, env } =
            filter
    in
        div [ style [ "display" => "flex", "align-items" => "center" ] ]
            [ autocomplete app (collect .app) (msg FilterApp)
            , autocomplete message (collect .message) (msg FilterMessage)
            , autocomplete env (collect .env) (msg FilterEnvironment)
            ]
