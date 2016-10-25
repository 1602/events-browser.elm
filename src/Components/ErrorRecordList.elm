module Components.ErrorRecordList exposing(Model, init)

import ErrorRecord
import Components.ErrorRecordOverview exposing (viewErrorRecord, ViewErrorRecordProps)


type alias Model =
    { list : List ErrorRecord.Model
    , selectedItemId : ErrorRecord.Id
    }

init : Model
init
