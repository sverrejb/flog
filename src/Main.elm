import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getBlogList)



-- UPDATE


type Msg
  = MorePlease
  | GotBlogList (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getBlogList)

    GotBlogList result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ 
      div [] [ h1 [] [ text "Header" ] ]
    , div [] [ h2 [] [ text "Content" ], viewBlogList model ]
    , div [] [ h2 [] [text "Footer"]]
    ]


viewBlogList : Model -> Html Msg
viewBlogList model =
  case model of
    Failure ->
      div []
        [ text "Unable to load blog posts."
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading blog posts"

    Success url ->
      div []
        [ 
        text url
        ]

-- HTTP

getBlogList : Cmd Msg
getBlogList =
  Http.get
    { url = "https://www.googleapis.com/drive/v3/files?q=%271pOJUeCNvFbHEbPxM4FkL8fePbZ4Ct_Ak%27%20in%20parents&key=AIzaSyDOw0EmUh-dNvg3qXvJ7ewkZNJgTIxtK_o"
    , expect = Http.expectJson GotBlogList blogListDecoder
    }


blogListDecoder : Decoder String
blogListDecoder =
  field "kind" string