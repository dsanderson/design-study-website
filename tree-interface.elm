import main exposing (..)
import Svg

type alias Dim = {x:Int, y:Int}

render_svg : Dim Model -> Dim Svg Msg
render_svg model =
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

render_leaf Model
