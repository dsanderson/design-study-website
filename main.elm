import Html exposing ( .. )
import Html.App
import String
import List

-- TYPE ALIASES to help the model

type alias Label = String
type alias Node_id = Int
type Operation = Sum | Product
type alias Data = String
type Node_type = Range | Set | Search
type alias ListTree = List Tree
type Tree = LeafNode Label Node_type Node_id Data | CombinatorNode Operation Node_id ListTree

main = Html.App.beginnerProgram { model = default_model, view = view, update = update }

-- MODEL

type alias Model = Tree

default_model : Model
default_model =
  CombinatorNode Product 1 [ LeafNode "Shaft Type" Set 2 "Keyed|D|Round",
    CombinatorNode Sum 3 [
      LeafNode "Shaft Diameter" Range 4 "1.0|2.0|4",
      LeafNode "Shaft Diameter" Range 5 "3.0|5.0|4"
    ]
  ]

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
    [ table [] [ tbody [] [ render_table model ] ] ]

render_table : Model -> Html Msg
render_table model =
  case model of
    LeafNode label node_type _ data ->
      case node_type of
        Range ->
          tr [] [ td [] [ text (label ++ ": " ++ (parse_range data)) ] ]
        Set ->
          tr [] [ td [] [ text (label ++ ": " ++ (parse_set data)) ] ]
        Search ->
          tr [] [ td [] [ text (label ++ ": " ++ "Not Implemented") ] ]
    CombinatorNode operation _ children ->
      case operation of
        Product ->
          tr [] [ ( td [] [ (text "Product") ] ), (td [] (List.map render_table children ) ) ]
        Sum ->
          tr [] [ ( td [] [ (text "Sum") ] ), (td [] (List.map render_table children ) ) ]

parse_range : Data -> String
parse_range data =
  let
    data_list = String.split "|" data
    npts = case (List.head (List.drop 2 data_list)) of
      Just a -> a
      Nothing -> " "
    lowpt = case (List.head (List.drop 0 data_list)) of
      Just a -> a
      Nothing -> " "
    highpt = case (List.head (List.drop 1 data_list)) of
      Just a -> a
      Nothing -> " "
    out_string = npts ++ " points between " ++ lowpt ++ " and " ++ highpt
  in
    case (String.length out_string) > 60 of
      True ->
        let
          out_string = String.slice 0 17 out_string
        in
          out_string ++ "..."
      False ->
        out_string

parse_set : Data -> String
parse_set data =
  let
    data_list = String.split "|" data
    out_string = String.join ", " data_list
  in
    case (String.length out_string) > 60 of
      True ->
        let
          out_string = String.slice 0 17 out_string
        in
          out_string ++ "..."
      False ->
        out_string
