module Main exposing (BlogPost, Model(..), Msg(..), backendUrl, blogDecoder, blogPostDecoder, getBlogList, init, main, viewBlogpostList, subscriptions, update, view, viewMainContent, viewSpinner)

--import Html.Attributes exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, li, text, ul)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, list, string)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )



-- MAIN


backendUrl : String
backendUrl =
    "https://www.googleapis.com/drive/v3/files?q=%271pOJUeCNvFbHEbPxM4FkL8fePbZ4Ct_Ak%27%20in%20parents&key=AIzaSyDOw0EmUh-dNvg3qXvJ7ewkZNJgTIxtK_o"


main : Program () Model Msg
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
    | Success (List BlogPost)


type alias BlogPost =
    { name : String
    , id : String
    }


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
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewBlogpostList : List BlogPost -> Html msg
viewBlogpostList lst =
    ul []
        (List.map (\l -> li [] [ text l.name ]) lst)


view : Model -> Html Msg
view model =
    div []
        [ div [] [ h1 [] [ text "Clever Blog Title" ] ]
        , div [] [ h2 [] [ text "Interesting ramblings" ], viewMainContent model ]
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


viewMainContent : Model -> Html Msg
viewMainContent model =
    case model of
        Failure ->
            div []
                [ text "Unable to load blogposts"
                , button [ onClick FetchBlogposts ] [ text "Try Again!" ]
                ]

        Loading ->
            viewSpinner

        Success blogPosts ->
            viewBlogpostList blogPosts



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
