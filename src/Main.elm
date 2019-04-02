module Main exposing (BlogPost, Model, Msg(..), blogDecoder, blogPostDecoder, blogPostListUrl, getBlogList, init, main, subscriptions, update, view, viewBlogpostList, viewMainContent, viewSpinner)

import Browser
import Browser.Navigation as Nav
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
import String.Interpolate exposing (interpolate)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


blogTitle =
    "Blog"



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , content : BlogContent
    }


type BlogContent
    = Failure
    | Loading
    | Success (List BlogPost)


type alias BlogPost =
    { name : String
    , id : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url Loading, getBlogList )



-- UPDATE


type Msg
    = FetchBlogposts
    | GotBlogList (Result Http.Error (List BlogPost))
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchBlogposts ->
            ( { model | content = Loading }, getBlogList )

        GotBlogList result ->
            case result of
                Ok blogPostList ->
                    ( { model | content = Success blogPostList }, Cmd.none )

                Err err ->
                    ( { model | content = Failure }, Cmd.none )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewBlogpostList : List BlogPost -> Html msg
viewBlogpostList lst =
    ul []
        (List.map (\l -> li [] [ text l.name ]) lst)


view : Model -> Browser.Document Msg
view model =
    { title = blogTitle
    , body =
        [ div []
            [ div [] [ h1 [] [ text "Clever Blog Title" ] ]
            , div [] [ h2 [] [ text "Interesting ramblings" ], viewMainContent model ]
            , div [] [ h2 [] [ text "Footer? Footer." ] ]
            ]
        ]
    }


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
    case model.content of
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


apiString : String
apiString =
    "https://www.googleapis.com/drive/v3/files?orderBy=createdTime desc&q={0} in parents&key={1}"


apiKey : String
apiKey =
    "AIzaSyDOw0EmUh-dNvg3qXvJ7ewkZNJgTIxtK_o"


blogRootDirectoryId : String
blogRootDirectoryId =
    "'1pOJUeCNvFbHEbPxM4FkL8fePbZ4Ct_Ak'"


blogPostListUrl : String
blogPostListUrl =
    interpolate apiString [ blogRootDirectoryId, apiKey ]


getBlogList : Cmd Msg
getBlogList =
    Http.get
        { url = blogPostListUrl
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
