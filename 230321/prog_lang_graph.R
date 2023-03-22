###############################################################################
# TIDYTUESDAY, MARCH 21, 2023: PROGRAMMING LANGUAGES
###############################################################################

library(pacman)
p_load(tidyverse, tidygraph, ggraph)

languages <- read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv'
)

###############################################################################
# Process data to get all pairs of languages

languages$title[languages$title == "Assembly language"] <- "Assembly"

edges1 <- languages |>
  select(pldb_id, wikipedia_related, title, appeared) |>
  separate_rows(wikipedia_related, sep = " ") |>
  filter(wikipedia_related %in% pldb_id) |>
  unique() 

edges2 <- edges1 |>
  bind_rows(
    edges1 |>
      rename(pldb_id = wikipedia_related,
             wikipedia_related = pldb_id) |>
      anti_join(edges1, by = c("pldb_id", "wikipedia_related")) |>
      unique()
  ) |>
  inner_join(
    select(languages, pldb_id, title) |>
      rename(wikipedia_related = pldb_id,
             related_title = title),
    by = "wikipedia_related"
  )

# filter top n languages
n <- 20
filtered_edges <- edges2 |>
  group_by(pldb_id) |>
  summarize(degree = n()) |>
  slice_max(order_by = degree, n = n)

top_n_edges <- edges2 |>
  filter(pldb_id %in% filtered_edges$pldb_id & 
           wikipedia_related %in% filtered_edges$pldb_id) |> 
  inner_join(filtered_edges, by = "pldb_id") |> 
  filter(title != related_title) |> 
  select(title, related_title, degree)

# nodes and edges for ggraph
nodes <- top_n_edges |> 
  select(title, degree) |> 
  unique()

edges <- top_n_edges |> 
  select(title, related_title) |> 
  rename(from = title, to = related_title) 

###############################################################################
# Graph network

graph <- tbl_graph(nodes = nodes, edges = edges, 
                   directed = FALSE, node_key = "title")

ggraph(graph, 'kk') +
  geom_edge_link(alpha = .7) +
  geom_node_label(aes(label = title, size = degree), 
                  repel = FALSE, fill='white') +
  labs(title = "The Most Influential Programming Languages",
       subtitle = "Relationships among the 20 most influential programming languages, as determined by \nthe number of links between all languages in the Programming Language Database",
       caption = "Source: pldb.com | github.com/huckjo") +
  # adjust relative node size
  scale_size_continuous(range = c(4, 10)) +
  # adjust plot area so nodes aren't truncated
  coord_cartesian(xlim=c(-2.1, 2.2), ylim=c(-2.4, 2.3)) +
  theme_void() +
  theme(
    aspect.ratio = 1:1,
    plot.background = element_rect(fill = "#EFEFEF"),
    plot.title = element_text(size=18, face="bold"),
    plot.subtitle = element_text(size=12, face="italic"),
    plot.caption = element_text(size=10),
    plot.margin = margin(.7, .7, .7, .7, "cm"),
    legend.position = "none"
  )