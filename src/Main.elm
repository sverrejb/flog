module Main exposing (BlogIndexItem, Model, Msg(..), blogDecoder, blogPostDecoder, blogPostListURI, getBlogList, init, main, update, view, viewBlogpostList, viewMainContent, viewSpinner)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h1, h2, li, pre, text, ul)
import Html.Attributes exposing (href)
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
import Url.Parser as UrlParser exposing ((</>), (<?>), Parser, int, map, oneOf, s, string, top)
import Url.Parser.Query as Query



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


blogTitle : String
blogTitle =
    "Blog"



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Maybe Route
    , blogIndex : BlogIndex
    , currentBlogPost : String
    }


type BlogIndex
    = Failure
    | Loading
    | Success (List BlogIndexItem)


type alias BlogIndexItem =
    { name : String
    , id : String
    }


type Route
    = BlogPostRoute String
    | RootRoute


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map RootRoute top
        , map BlogPostRoute (s "post" </> string)
        ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key (UrlParser.parse routeParser url) Loading "", getBlogList )



-- UPDATE


type Msg
    = FetchBlogpostsIndex
    | FetchBlogpostContent String
    | GotBlogList (Result Http.Error (List BlogIndexItem))
    | GotBlogPost (Result Http.Error String)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchBlogpostsIndex ->
            ( { model | blogIndex = Loading }, getBlogList )

        FetchBlogpostContent id ->
            ( model, getBlogPost id )

        GotBlogList result ->
            case result of
                Ok blogPostList ->
                    ( { model | blogIndex = Success blogPostList }, Cmd.none )

                Err err ->
                    ( { model | blogIndex = Failure }, Cmd.none )

        GotBlogPost result ->
            case result of
                Ok blogPostContent ->
                    ( { model | currentBlogPost = blogPostContent }, Cmd.none )

                Err _ ->
                    ( { model | currentBlogPost = "error fetching blogpost" }, Cmd.none )

        UrlChanged url ->
            ( { model | url = UrlParser.parse routeParser url }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = blogTitle
    , body =
        [ div []
            [ div [] [ h1 [] [ a [ href "/" ] [ text "Blag" ] ] ]
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


viewBlogIndex : Model -> Html Msg
viewBlogIndex model =
    case model.blogIndex of
        Failure ->
            div []
                [ text "Unable to load blogposts"
                , button [ onClick FetchBlogpostsIndex ] [ text "Try Again!" ]
                ]

        Loading ->
            viewSpinner

        Success blogPosts ->
            viewBlogpostList blogPosts


viewBlogpostList : List BlogIndexItem -> Html Msg
viewBlogpostList lst =
    ul []
        (List.map (\l -> viewBlogListItem l.name l.id) lst)


viewBlogListItem : String -> String -> Html Msg
viewBlogListItem name id =
    li [] [ a [ href ("/post/" ++ id), onClick (FetchBlogpostContent id) ] [ text name ] ]


viewBlogPost : Model -> Html msg
viewBlogPost model =
    pre [] [ text model.currentBlogPost ]


viewMainContent : Model -> Html Msg
viewMainContent model =
    case model.url of
        Just RootRoute ->
            viewBlogIndex model

        Just (BlogPostRoute _) ->
            viewBlogPost model

        Nothing ->
            text "404"



-- HTTP


getBlogList : Cmd Msg
getBlogList =
    Http.get
        { url = blogPostListURI
        , expect = Http.expectJson GotBlogList blogDecoder
        }


getBlogPost : String -> Cmd Msg
getBlogPost id =
    Http.get
        { url = interpolate blogPostURI [ id, apiKey ]
        , expect = Http.expectString GotBlogPost
        }


apiKey : String
apiKey =
    "AIzaSyDOw0EmUh-dNvg3qXvJ7ewkZNJgTIxtK_o"


blogRootDirectoryId : String
blogRootDirectoryId =
    "'1pOJUeCNvFbHEbPxM4FkL8fePbZ4Ct_Ak'"


googleDriveFilesURI : String
googleDriveFilesURI =
    "https://www.googleapis.com/drive/v3/files"


blogDirectoryURI : String
blogDirectoryURI =
    googleDriveFilesURI ++ "?orderBy=createdTime desc&q={0} in parents&key={1}"


blogPostURI : String
blogPostURI =
    googleDriveFilesURI ++ "/{0}/export?key={1}&mimeType=text/plain"


blogPostListURI : String
blogPostListURI =
    interpolate blogDirectoryURI [ blogRootDirectoryId, apiKey ]


blogDecoder : Decoder (List BlogIndexItem)
blogDecoder =
    Decode.at [ "files" ] (Decode.list blogPostDecoder)


blogPostDecoder : Decoder BlogIndexItem
blogPostDecoder =
    Decode.map2
        BlogIndexItem
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "id" ] Decode.string)
