module Main exposing (BlogIndexItem, Model, Msg(..), blogDecoder, blogPostDecoder, blogPostListURI, getBlogList, init, main, update, view, viewBlogpostList, viewMainContent, viewSpinner)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h1, h2, li, pre, text, ul)
import Html.Attributes exposing (href, id)
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
    "Sverres blog"



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Maybe Route
    , blogIndex : BlogIndex
    , currentBlogPost : BlogItemContent
    }


type BlogIndex
    = Failure
    | Loading
    | Success (List BlogIndexItem)


type BlogItemContent
    = ContentFailure
    | ContentLoading
    | ContentSuccess String


type alias BlogIndexItem =
    { name : String
    , id : String
    , date : String
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
    ( Model key (UrlParser.parse routeParser url) Loading ContentLoading, getBlogList )



-- UPDATE


type Msg
    = FetchBlogpostsIndex
    | FetchBlogpostContent Url.Url
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
            ( { model | currentBlogPost = ContentLoading }, getBlogPost model.url )

        GotBlogList result ->
            case result of
                Ok blogPostList ->
                    ( { model | blogIndex = Success blogPostList }, getBlogPost model.url )

                Err err ->
                    ( { model | blogIndex = Failure }, Cmd.none )

        GotBlogPost result ->
            case result of
                Ok blogPostContent ->
                    ( { model | currentBlogPost = ContentSuccess blogPostContent }, Cmd.none )

                Err _ ->
                    ( { model | currentBlogPost = ContentFailure }, Cmd.none )

        UrlChanged url ->
            let
                newUrl =
                    UrlParser.parse routeParser url
            in
            ( { model | url = newUrl, currentBlogPost = ContentLoading }, getBlogPost newUrl )

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
            [ div [] [ h1 [ id "header" ] [ a [ href "/" ] [ text "Blog" ] ] ]
            , div [] [ viewMainContent model ]
            , div [] [ h2 [] [ text "Footer? Footer." ] ]
            ]
        ]
    }


viewSpinner : Html Msg
viewSpinner =
    div []
        [ Loading.render
            Spinner
            { defaultConfig | color = "#444444" }
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
            div [] [ h2 [] [ text "Interesting ramblings" ], viewBlogpostList blogPosts ]


viewBlogpostList : List BlogIndexItem -> Html Msg
viewBlogpostList lst =
    ul []
        (List.map (\l -> viewBlogListItem l.name l.id l.date) lst)


viewBlogListItem : String -> String -> String -> Html Msg
viewBlogListItem name id date =
    li [] [ a [ href ("/post/" ++ id) ] [ text (name ++ " - " ++ date) ] ]


viewBlogPost : Model -> String -> Html Msg
viewBlogPost model id =
    case model.blogIndex of
        Loading ->
            viewSpinner

        Failure ->
            div []
                [ text "Unable to load blogposts"
                , button [ onClick FetchBlogpostsIndex ] [ text "Try Again!" ]
                ]

        Success blogPosts ->
            let
                title =
                    getTitleFromId id blogPosts
            in
            case model.currentBlogPost of
                ContentFailure ->
                    div [] [ text "Error loading blogpost" ]

                ContentLoading ->
                    viewSpinner

                ContentSuccess content ->
                    div [] [ h2 [] [ text title ], pre [] [ text content ] ]


viewMainContent : Model -> Html Msg
viewMainContent model =
    case model.url of
        Just RootRoute ->
            viewBlogIndex model

        Just (BlogPostRoute route) ->
            viewBlogPost model route

        Nothing ->
            text "404"



-- HTTP


getBlogList : Cmd Msg
getBlogList =
    Http.get
        { url = blogPostListURI
        , expect = Http.expectJson GotBlogList blogDecoder
        }


getBlogPost : Maybe Route -> Cmd Msg
getBlogPost url =
    case url of
        Just (BlogPostRoute path) ->
            let
                id =
                    getIDfromUrl path
            in
            case id of
                Just "" ->
                    Cmd.none

                Just blogId ->
                    Http.get
                        { url = interpolate blogPostURI [ blogId, apiKey ]
                        , expect = Http.expectString GotBlogPost
                        }

                Nothing ->
                    Cmd.none

        Just RootRoute ->
            Cmd.none

        Nothing ->
            Cmd.none


getIDfromUrl : String -> Maybe String
getIDfromUrl url =
    List.head (List.reverse (String.split "/" url))


getTitleFromId : String -> List BlogIndexItem -> String
getTitleFromId id blogIndex =
    case List.head (List.filter (\b -> b.id == id) blogIndex) of
        Just currentBlogPost ->
            currentBlogPost.name

        Nothing ->
            "Unable to load title"


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
    googleDriveFilesURI ++ "?orderBy=createdTime desc&q={0} in parents&key={1}&fields=files(name,id,createdTime)"


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
    Decode.map3
        BlogIndexItem
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "id" ] Decode.string)
        (Decode.at [ "createdTime" ] Decode.string)
