
library(igraph)
library(ggraph)
library(dplyr)
library(viridis)
library(animation)

graph <- igraph::barabasi.game(n=6, power=5)

V(graph)$color <- ovl[2]
V(graph)[[1]]$color  <-ovl[12]
g1 <-   ggraph(graph) +
      geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE, width=2, colour="white") +
      geom_node_point(color=V(graph)$color, size=25) +
      theme_graph(foreground = 'steelblue', fg_text_colour = 'white', background = 'black')

V(graph)$color <- ovl[3]
V(graph)[[1]]$color  <-ovl[11]
g2 <-   ggraph(graph) +
      geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE, width=2, colour="white") +
      geom_node_point(color=V(graph)$color, size=25) +
      theme_graph(foreground = 'steelblue', fg_text_colour = 'white', background = 'black')
V(graph)$color <- ovl[4]
V(graph)[[1]]$color  <-ovl[10]
g3 <-   ggraph(graph) +
      geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE, width=2, colour="white") +
      geom_node_point(color=V(graph)$color, size=25) +
      theme_graph(foreground = 'steelblue', fg_text_colour = 'white', background = 'black')
V(graph)$color <- ovl[5]
V(graph)[[1]]$color  <-ovl[9]
g4 <-   ggraph(graph) +
      geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE, width=2, colour="white") +
      geom_node_point(color=V(graph)$color, size=25) +
      theme_graph(foreground = 'steelblue', fg_text_colour = 'white', background = 'black')
V(graph)$color <- ovl[6]
V(graph)[[1]]$color  <-ovl[8]
g5 <-   ggraph(graph) +
      geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE, width=2, colour="white") +
      geom_node_point(color=V(graph)$color, size=25) +
      theme_graph(foreground = 'steelblue', fg_text_colour = 'white', background = 'black')


gif <- function() {
   lapply(list(g1, g2, g3, g4, g5), function(i) {
     print(i)
   })
 }
saveGIF(gif(), interval = .8, movie.name="diffusion.gif")
