import Html exposing (Html, text)

main = 
    Html.beginnerProgram { model = model, view = view, update = update }

-- model : Model
model = "Hello, world"

-- update : Msg -> Model -> Model
update msg model =
    model

-- view : Model -> Html Msg
view model =
    text model
