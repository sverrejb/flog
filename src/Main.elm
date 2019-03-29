module Main exposing (BlogPost, Model(..), Msg(..), backendUrl, blogDecoder, blogPostDecoder, getBlogList, init, main, renderList, subscriptions, update, view, viewBlogList, viewSpinner)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, list, string)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )



-- MAIN


backendUrl =
    "https://www.googleapis.com/drive/v3/files?q=%271pOJUeCNvFbHEbPxM4FkL8fePbZ4Ct_Ak%27%20in%20parents&key=AIzaSyDOw0EmUh-dNvg3qXvJ7ewkZNJgTIxtK_o"


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias BlogPost =
    { name : String
    , id : String
    }


type Model
    = Failure Http.Error
    | Loading
    | Success (List BlogPost)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getBlogList )



-- UPDATE


type Msg
    = FetchBlogposts
    | GotBlogList (Result Http.Error (List BlogPost))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchBlogposts ->
            ( Loading, getBlogList )

        GotBlogList result ->
            case result of
                Ok url ->
                    ( Success url, Cmd.none )

                Err err ->
                    ( Failure err, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


renderList : List BlogPost -> Html msg
renderList lst =
    ul []
        (List.map (\l -> li [] [ text l.name ]) lst)


view : Model -> Html Msg
view model =
    div []
        [ div [] [ h1 [] [ text "Clever Blog Title" ] ]
        , div [] [ h2 [] [ text "Interesting ramblings" ], viewBlogList model ]
        , div [] [ h2 [] [ text "Footer? Footer." ] ]
        ]


viewSpinner : Html Msg
viewSpinner =
    div []
        [ Loading.render
            Spinner
            { defaultConfig | color = "#000000" }
            Loading.On
        ]


viewBlogList : Model -> Html Msg
viewBlogList model =
    case model of
        Failure err ->
            div []
                [ text "Unable to load blogposts"
                , button [ onClick FetchBlogposts ] [ text "Try Again!" ]
                ]

        Loading ->
            viewSpinner

        Success blogPosts ->
            renderList blogPosts



-- HTTP


getBlogList : Cmd Msg
getBlogList =
    Http.get
        { url = backendUrl
        , expect = Http.expectJson GotBlogList blogDecoder
        }


blogDecoder : Decoder (List BlogPost)
blogDecoder =
    Decode.at [ "files" ] (Decode.list blogPostDecoder)


blogPostDecoder : Decoder BlogPost
blogPostDecoder =
    Decode.map2
        BlogPost
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "id" ] Decode.string)
