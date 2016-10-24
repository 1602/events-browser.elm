module Messages exposing (..)

import ErrorRecord exposing (..)
import Components.Autocomplete exposing (ActionType)
import Http
import Keyboard


type FilterType
    = FilterApp
    | FilterMessage
    | FilterEnvironment


type Msg
    = NoOp
    | Fetch
    | FetchSuccess (List ErrorRecord)
    | FetchError Http.Error
    | UpdateFilters FilterType ActionType
    | SelectErrorRecord ErrorRecordId
    | KeyMsg Keyboard.KeyCode
    | FocusOn String
