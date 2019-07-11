module Main exposing (BlogIndexItem, Model, Msg(..), blogDecoder, blogPostDecoder, blogPostListURI, getBlogList, init, main, update, view, viewBlogpostList, viewMainContent, viewSpinner)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, br, button, div, h1, h2, h3, i, li, p, span, text, ul)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Http
import Iso8601 as Iso
import Json.Decode as Decode exposing (Decoder, list, string)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )
import String.Interpolate exposing (interpolate)
import Time exposing (Month(..), Posix, utc)
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
    , date : Posix
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
        [ div [ class "grid-container" ]
            [ div [ class "header" ] [ h1 [] [ a [ href "/" ] [ text "Blog" ] ], h3 [] [ text "Interesting ramblings" ] ]
            , div [ class "content" ] [ viewMainContent model ]
            , div [ class "footer" ] [ text "2019 © Sverre" ]
            ]
        ]
    }


viewSpinner : Html Msg
viewSpinner =
    div []
        [ br [] []
        , Loading.render
            Spinner
            { defaultConfig | color = "#454545" }
            Loading.On
        ]


viewError : String -> String -> Html Msg
viewError statusCode message =
    div [ class "error" ] [ div [ class "jumbotron" ] [ text statusCode ], div [] [ text message ] ]


viewBlogIndex : Model -> Html Msg
viewBlogIndex model =
    case model.blogIndex of
        Failure ->
            viewError "500" "Error loading blog"

        Loading ->
            viewSpinner

        Success blogPosts ->
            div [] [ viewBlogpostList blogPosts ]


viewBlogpostList : List BlogIndexItem -> Html Msg
viewBlogpostList lst =
    ul []
        (List.map (\l -> viewBlogListItem l.name l.id l.date) lst)


viewBlogListItem : String -> String -> Posix -> Html Msg
viewBlogListItem name id date =
    li [] [ a [ href ("/post/" ++ id) ] [ span [ class "blogpost-title" ] [ text name, span [ class "blogpost-date" ] [ text (" | " ++ formatDate date) ] ] ] ]


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
                    viewError "404" "Blogpost not found"

                ContentLoading ->
                    viewSpinner

                ContentSuccess content ->
                    div [] [ h3 [] [ text title ], div [] (List.map (\paragraph -> p [] [ text paragraph ]) <| List.filter (\x -> String.length x /= 1) <| String.split "\n" content) ]


viewMainContent : Model -> Html Msg
viewMainContent model =
    case model.url of
        Just RootRoute ->
            viewBlogIndex model

        Just (BlogPostRoute route) ->
            viewBlogPost model route

        Nothing ->
            viewError "404" "Not Found"



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
    String.split "/" url |> List.reverse |> List.head 


formatDate : Posix -> String
formatDate date =
    let
        year =
            String.fromInt (Time.toYear utc date)

        month =
            toMonthNumber (Time.toMonth utc date)

        day =
            String.fromInt (Time.toDay utc date)
    in
    day ++ "." ++ month ++ "." ++ year


toMonthNumber : Month -> String
toMonthNumber month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


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
        (Decode.at [ "createdTime" ] Iso.decoder)
