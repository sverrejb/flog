import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, string, list)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias BlogPost
  = { name : String,
      id : String}


type Model
  = Failure String
  | Loading
  | Success (List BlogPost)


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getBlogList)



-- UPDATE


type Msg
  = MorePlease
  | GotBlogList (Result Http.Error (List BlogPost))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getBlogList)

    GotBlogList result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)

        Err err ->
          (Failure (Debug.toString err), Cmd.none)



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
    Failure err ->
      div []
        [ text err
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading blog posts"

    Success blogPosts ->
      text (Debug.toString blogPosts)

-- HTTP

getBlogList : Cmd Msg
getBlogList =
  Http.get
    { url = "https://www.googleapis.com/drive/v3/files?q=%271pOJUeCNvFbHEbPxM4FkL8fePbZ4Ct_Ak%27%20in%20parents&key=AIzaSyDOw0EmUh-dNvg3qXvJ7ewkZNJgTIxtK_o"
    , expect = Http.expectJson GotBlogList blogDecoder
    }

blogDecoder : Decoder (List BlogPost)
blogDecoder = Decode.at ["files"] (Decode.list blogPostDecoder)

blogPostDecoder : Decoder BlogPost
blogPostDecoder =
    Decode.map2
        BlogPost
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "id" ] Decode.string)