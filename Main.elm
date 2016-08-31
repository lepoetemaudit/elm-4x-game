module Main exposing (..)

import Html exposing (Html, button, div, text, p)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Mouse exposing (moves)

main : Program Never
main =
    Html.program
        { init = ( model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    moves (\{ x, y } -> MouseMove ( x, y ))

type alias Vec2 = (Float, Float)

add : Vec2 -> Vec2 -> Vec2
add (x1, y1) (x2, y2) =
  (x1 + x2, y1 + y2)

sub : Vec2 -> Vec2 -> Vec2
sub (x1, y1) (x2, y2) =
  (x1 - x2, y1 - y2)

intsToVec2 : (Int, Int) -> Vec2
intsToVec2 (x, y) =
    (toFloat x, toFloat y)

-- MODEL


type alias Model =
    { dragState : DragState
    , pos : Vec2
    , lastMousePos : Maybe Vec2
    }

model : Model
model =
    { dragState = NotDragging
    , pos = (0, 0)
    , lastMousePos = Nothing
    }


type DragState
    = Dragging
    | NotDragging



-- UPDATE


type Msg
    = DragStart
    | MouseMove ( Int, Int )
    | DragStop


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        MouseMove newPos ->
            let vecNewPos = intsToVec2 newPos in
            (case (model.lastMousePos, model.dragState)  of                
                (Just lastPos, Dragging) -> 
                    { model | pos = sub model.pos ( lastPos `sub` vecNewPos )
                    , lastMousePos = Just vecNewPos }
                _ -> { model | lastMousePos = Just vecNewPos })
            ! []

        DragStart ->
            { model | dragState = Dragging } ! []

        DragStop ->
            { model | dragState = NotDragging } ! []



-- VIEW


posString : Float -> Float -> String
posString x y =
    toString (x) ++ "px " ++ toString (y) ++ "px "


background : Vec2 -> DragState -> Html Msg
background (x, y) dragState =
    div []
        [ div
            [ class "background"
            , style
                [ ( "backgroundImage", "url(img/basic.jpg)" )
                , ( "backgroundSize", "768px" )
                , ( "backgroundPosition", posString (x * 1.62) (y * 1.62) )
                ]
            ]
            []
        , div
            [ class "background"
            , style
                [ ( "backgroundImage", "url(img/blue.jpg)" )
                , ( "backgroundSize", "768px" )
                , ( "opacity", "0.5" )
                , ( "backgroundPosition", posString x y )
                ]
            ]
            []
        , div
            [ class "background"
            , style
                [ ( "backgroundImage", "url(img/red.jpg)" )
                , ( "backgroundSize", "1024x" )
                , ( "opacity", "0.4" )
                , ( "backgroundPosition", posString (x * 0.7) (y * 0.7) )
                ]
            ]
            []
        , div [ (case dragState of
                    NotDragging ->
                        onMouseDown DragStart

                    Dragging ->
                        onMouseUp DragStop
                )
             , class "full" ] [ (case dragState of
                    NotDragging ->
                        p [] [ text "no" ]

                    Dragging ->
                        p [] [ text "yo" ])  ]
        ]


view : Model -> Html Msg
view model =
    div [] [ background model.pos model.dragState ]
