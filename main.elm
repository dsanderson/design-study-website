import Html exposing (..)


-- TYPE ALIASES to help the model

type alias CombinatorNode = { operation : String, id: Int, children: List Node }
type alias LeafNode = { label: String, node_type: String, id: Int, data: String }
type Tree a = Empty | Node a (Tree a) (Tree a)

-- MODEL

type alias Model = Tree

default_model : Model
default_model = { operation = "product", id = 0, children = [
  { operation = "sum", id = 1, children = [
    { label = "Shaft Diameter (mm)", node_type = "range", id = 2, data = "1.0|2.0|5" },
    { label = "Shaft Diameter (mm)", node_type = "range", id = 3, data = "3.0|4.0|5" }
    ] },
  { label = "Color", node_type = "set", id = 4, data = "a|b|c" }
  ] }

-- UPDATE

type Msg = Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset -> default_model


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text (toString model) ] ]
