import Browser
import Html exposing (Html, Attribute, div, input, text, h1, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ 
        h1 [] [text ("HEI, VERDEN")]
      , h2 [] [text ("Dette skal bli en blogg.")]
      , text ("Uten en backend.")
    ]