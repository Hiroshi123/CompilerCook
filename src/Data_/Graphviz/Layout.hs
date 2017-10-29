
module MData.Graphviz.Layout where

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





