import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Regex exposing (..)
import Char exposing (..)
import List.Extra exposing (last)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

type alias Model =
  { message : String
  }

model : Model
model =
  Model ""

-- UPDATE

type Msg
  = Update String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Update message ->
      { model | message = message }
  
-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ textarea [ placeholder "Paste Objective-C code", onInput Update ] []
    , viewResult model
    ]

{-- 
NSUserDefaults *ud = [NSUserDefaults standardUserDefaults];
[ud setBool:YES forKey:@"KEY_B"];

var ud = R.NSClassFromString('NSUserDefaults').call('standardUserDefaults');
ud.call('setBool:forKey', true, 'KEY_B');
--}

viewResult : Model -> Html Msg
viewResult model =
  let xs = List.map (\x -> li [] [ text x ]) (convert model.message)
  in ul [] xs

-- Rollout

convert : String -> List String
convert text =
  String.lines text |> List.map (expr << removeComment)

expr : String -> String
expr text =
  let
    xs = String.split "=" text
  in
  -- TODO: Refactor - 共通化
    if List.length xs == 1
      then
        exprExpression <| String.trim <| Maybe.withDefault "" <| List.head xs
      else
        let
          declaration = String.trim <| Maybe.withDefault "" <| List.head xs
          expression = String.trim <| Maybe.withDefault "" <| List.Extra.last xs
        in
          exprDeclaration declaration ++ " = " ++ (exprExpression expression)

exprDeclaration : String -> String
exprDeclaration text =
  case List.Extra.last << String.split " " << remove "\\*" <| text of
    Nothing -> ""
    Just variable -> "var " ++ variable

exprExpression : String -> String
exprExpression text =
  let
    xs = (remove "\\[" >> remove "\\]" >> remove ";" >> String.split " ") text
    a = case pattern xs of
      Nothing -> Nothing
      Just (v, ts) ->
        let receiver = if isType v
          then "R.NSClassFromString('" ++ v ++ "')"
          else v
        in
          if (List.length ts == 1)
            && String.contains ":" (Maybe.withDefault "" (List.head ts)) == False
            then
              let
                selector = Maybe.withDefault "" <| List.head ts
              in
                Just <| receiver ++ ".call('" ++ selector ++ "');"
            else
              let
                selector = String.concat (List.filterMap getSelector ts)
                values = List.filterMap (\x -> last (String.split ":" x)) ts
                jsValues = List.map toJSValue values
                value = String.join ", " jsValues
              in 
                Just <| receiver ++ ".call('" ++ selector ++ "', " ++ value ++ ");"
  in
    case a of
      Nothing -> "Error"
      Just message -> message

getSelector : String -> Maybe String
getSelector token =
  let append s = s ++ ":"
  in Maybe.map append (List.head (String.split ":" token))

toJSValue : String -> String
toJSValue s =
  case s of
    "YES" -> "true"
    "NO"  -> "false"
    _ ->
      (remove "@" >> replace "\"" "'") s

isType : String -> Bool
isType token =
  Maybe.withDefault False <| Maybe.map Char.isUpper <| firstChar token

removeComment : String -> String
removeComment = remove "\\s*//.*$"

-- List Utility

last : List a -> Maybe a
last xs
  = List.head (List.reverse xs)

pattern : List a -> Maybe (a, List a)
pattern xs =
  let
    head = List.head xs
    tail = List.tail xs
  in
    Maybe.map2 (,) head tail

-- String Utility

firstChar : String -> Maybe Char
firstChar text = 
  case String.uncons text of
    Nothing -> Nothing
    Just (c, s) -> Just c

remove : String -> String -> String
remove target text =
  replace target "" text

replace : String -> String -> String -> String
replace from to text =
  Regex.replace All (Regex.regex from) (\_ -> to) text
