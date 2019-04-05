module Main exposing (BlogItem, Model, Msg(..), blogDecoder, blogPostDecoder, blogPostListURI, getBlogList, init, main, update, view, viewBlogpostList, viewMainContent, viewSpinner)

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
    , currentBlogPost : BlogItemContent
    }


type BlogIndex
    = Failure
    | Loading
    | Success (List BlogItem)

type BlogItemContent
    = ContentFailure
    | ContentLoading
    | ContentSuccess String


type alias BlogItem =
    { name : String
    , id : String
    , date : String
    , content : String
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
    ( Model key (UrlParser.parse routeParser url) Loading ContentLoading, Cmd.batch [getBlogList, getBlogPost url] )



-- UPDATE


type Msg
    = FetchBlogpostsIndex
    | FetchBlogpostContent Url.Url
    | GotBlogList (Result Http.Error (List BlogItem))
    | GotBlogPost (Result Http.Error String)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchBlogpostsIndex ->
            ( { model | blogIndex = Loading }, getBlogList )

        FetchBlogpostContent id ->
            ( { model | currentBlogPost = ContentLoading }, getBlogPost id )

        GotBlogList result ->
            case result of
                Ok blogPostList ->
                    ( { model | blogIndex = Success blogPostList }, Cmd.none )

                Err err ->
                    ( { model | blogIndex = Failure }, Cmd.none )

        GotBlogPost result ->
            case result of
                Ok blogPostContent ->
                    ( { model | currentBlogPost = ContentSuccess blogPostContent }, Cmd.none )

                Err _ ->
                    ( { model | currentBlogPost = ContentFailure }, Cmd.none )

        UrlChanged url ->
            ( { model | url = UrlParser.parse routeParser url }, getBlogPost url )

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


viewBlogpostList : List BlogItem -> Html Msg
viewBlogpostList lst =
    ul []
        (List.map (\l -> viewBlogListItem l.name l.id l.date) lst)


viewBlogListItem : String -> String -> String -> Html Msg
viewBlogListItem name id date =
    li [] [ a [ href ("/post/" ++ id) ] [ text (name ++ " - " ++ date) ] ]


viewBlogPost : Model -> Html Msg
viewBlogPost model =
    case model.currentBlogPost of
        ContentFailure ->
            div [] [ text "Error loading blogpost"]
        ContentLoading ->
            viewSpinner
        ContentSuccess content ->
            pre [] [ text content ]


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


getBlogPost : Url.Url -> Cmd Msg
getBlogPost url =
    let id = getIDfromUrl url in
    case id of
        Just blogId ->
            case blogId of
            "" -> Cmd.none
            _ -> Http.get
                    { url = interpolate blogPostURI [ blogId, apiKey ]
                    , expect = Http.expectString GotBlogPost
                    }
        Nothing -> Cmd.none

getIDfromUrl : Url.Url -> Maybe String
getIDfromUrl url =
    List.head (List.reverse (String.split "/" (Url.toString url)))

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


blogDecoder : Decoder (List BlogItem)
blogDecoder =
    Decode.at [ "files" ] (Decode.list blogPostDecoder)


blogPostDecoder : Decoder BlogItem
blogPostDecoder =
    Decode.map4
        BlogItem
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "id" ] Decode.string)
        (Decode.at [ "createdTime" ] Decode.string)
        (Decode.at [ "id" ] Decode.string)
