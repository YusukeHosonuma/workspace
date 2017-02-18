import Html exposing (Html, h1, button, div, text)
import Html.Events exposing (onClick)

main =
    Html.beginnerProgram { model = 0, view = view, update = update}

type Msg = Increment | Decrement

update msg model =
    case msg of
        Increment ->
            model + 1
        Decrement -> 
            model - 1

view model =
    div []
        [ h1 [] [ text "Hello, Elm" ] -- original
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (strong (toString model)) ]
        , button [ onClick Increment ] [ text "+" ]
        ]

-- original
strong : String -> String
strong s = s ++ "!"