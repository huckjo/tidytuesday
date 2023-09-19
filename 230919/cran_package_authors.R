library(tidyverse)

tt <- tidytuesdayR::tt_load('2023-09-19')
packages <- tt$cran_20230905
package_authors <- tt$package_authors
cran_graph_nodes <- tt$cran_graph_nodes
cran_graph_edges <- tt$cran_graph_edges
rm(tt)