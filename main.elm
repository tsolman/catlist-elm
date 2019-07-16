import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Html.Attributes exposing (style)
import Json.Decode as JD exposing (Decoder, map2, field, string, int ,list)



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
  | Success (List Cat)

type alias Cat =
  { id : String
    ,url: String
    ,width: Int
    ,height: Int
  }

type alias AppStatus =
  {
    msg : String
    ,isModalOpen : Bool
  }

initialAppStatus : AppStatus
initialAppStatus =
  { msg = "Catz"
  , isModalOpen = False
  }

init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getCatList)


api_key = "bdb0f0b8-7f00-4e07-a6e9-e06c10f8bc63"
-- UPDATE


type Msg
  = MorePlease
  | GotList (Result Http.Error (List Cat))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getCatList)

    GotList result ->
      case result of
        Ok catlist ->
          (Success catlist, Cmd.none)

        Err _ ->
                (Failure, Cmd.none)


      -- handle error message


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Cute catz list" ]
    , viewGif model
    -- , div [ maskStyle ]
    --     [ div [ modalStyle ]
    --       [ text "Hey look, a modal!" ]
    --     ]
    ]


-- maskStyle : Attribute msg
-- maskStyle =
--   style
--     [ ("background-color", "rgba(0,0,0,0.3)")
--     , ("position", "fixed")
--     , ("top", "0")
--     , ("left", "0")
--     , ("width", "100%")
--     , ("height", "100%")
--     ]
--
-- modalStyle : Attribute msg
-- modalStyle =
--   style
--     [ ("background-color", "rgba(255,255,255,1.0)")
--     , ("position", "absolute")
--     , ("top", "50%")
--     , ("left", "50%")
--     , ("height", "auto")
--     , ("max-height", "80%")
--     , ("width", "700px")
--     , ("max-width", "95%")
--     , ("padding", "10px")
--     , ("border-radius", "3px")
--     , ("box-shadow", "1px 1px 5px rgba(0,0,0,0.5)")
--     , ("transform", "translate(-50%, -50%)")
--     ]


renderImages : Cat -> Html Msg
renderImages lst =
      li[ (style "list-style" "none") ]
      [
      div [ ] [
      img [src lst.url , width 300 ] [] ]
      ]-- [text lst.url]
--
-- openModal : String
-- openModal  =
--     Debug.log "clicked"

viewGif : Model -> Html Msg
viewGif model =
  case model of
    Failure ->
      div []
        [ text "I could not load the catz for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]
    Loading ->
      text "Loading..."

    Success catlist ->
      div []
        [ul [] (List.map renderImages catlist)
        ,button [ onClick MorePlease ] [ text "More Catz please" ]]

-- HTTP

getCatList : Cmd Msg
getCatList =
  let headers  = [ Http.header "x-api-key" api_key ]
  in
  Http.request
    { body = Http.emptyBody
    , method="GET"
    , url = "https://api.thecatapi.com/v1/images/search?limit=10"
    , expect = Http.expectJson GotList catListDecoder
    , headers = headers
    , timeout = Nothing
    , tracker = Nothing
  }

catItemDecoder: Decoder Cat
catItemDecoder =
  JD.map4 Cat
    (field "id" string)
    (field "url" string)
    (field "width" int)
    (field "height" int)

catListDecoder: Decoder (List Cat)
catListDecoder =
  JD.list catItemDecoder
