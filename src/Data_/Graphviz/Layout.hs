
module Data_.Graphviz.Layout where

graph_layout =
  [
    "graph [",
    "ordering=\"out\";",
    "charset = \"UTF-8\";",
    "label = \"noddd\",",
    "labelloc = \"t\",",
    "labeljust = \"c\",",
    "bgcolor = \"#343434\",",
    "fontcolor = white,",
    "fontsize = 18,",
    "style = \"filled\",",
    "rankdir = TB,",
    "margin = 0.2,",
    "nodesep = 0.5,",
    "ranksep = 0.8,",
    "compound = true",
    "];"
  ]
  
node_layout =
  [
    "node [",
    "colorscheme = rdylgn11,",
    "fontname = \"Migu 1M\",",
    "color = 7,",
    "fontsize = 12,",
    "fontcolor = 6,",
    "fillcolor = 11,",
    "margin = 0.05,",
    "style = \"solid,filled\"",
    "];"
  ]
  
edge_layout = 
  [
    "edge [",
    "  color = white",
    "];"
  ]

graph_layout' =
  [
    "graph [",
    "charset = \"UTF-8\"",
    "bgcolor = \"#EDEDED\"",
    "rankdir = TB",
    "nodesep = 1.1",
    "ranksep = 1.05",
    "];"
  ];
  
node_layout' =
  [
    "node [",
    "shape = record",
    "fontname = \"Migu 1M\"",
    "fontsize = 12",
    "];"
  ];
  
  
--attribute :: [String] -> String
--attribute fields = x
--  where x = foldr (\a b -> (ff b) ++ a ) "" fields

                   
-- "[label = "
--  \" { <pc> AttributeProto \

-- " [label =
--   	\" { <pc> AttributeProto \
-- 	| { optional | { string | float | bottom } | {name | f | a } }  \
-- 	| { <pl> repeated | { float  | int64 | bytes  } | {name | f | a } } \ 
-- 	| { <pr> repeated | { <tens> TensorProto | GraphProto} }  \
-- 	} \
-- 	\"];"



