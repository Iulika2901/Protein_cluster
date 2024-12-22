library(igraph)
library(dplyr)
library(openxlsx)
library(threejs)
library(htmlwidgets)
library(zoom)
library(threejs)
#library(graphjs)
library(visNetwork)
#install.packages("devtools")
#install.packages("visNetwork")

data <- load(file.choose())  
work_mat3 <- get("work_mat2")  

# Creare graf
edges <- which(work_mat3 == 1, arr.ind = TRUE)
g <- graph_from_edgelist(as.matrix(edges), directed = FALSE)

# Atribuie culori
methods <- list()

node_features <- data.frame(
  degree = degree(g),
  closeness = closeness(g),
  betweenness = betweenness(g)
)
set.seed(123)
scaled_features <- scale(node_features)
kmeans_result <- kmeans(scaled_features, centers = 4, nstart = 25)
methods$kmeans <- as.numeric(kmeans_result$cluster)

methods$leading_eigen <- as.numeric(membership(cluster_leading_eigen(g)))
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
methods$fast_greedy <- as.numeric(membership(cluster_fast_greedy(g)))
methods$louvain <- as.numeric(membership(cluster_louvain(g)))
methods$walktrap <- as.numeric(membership(cluster_walktrap(g)))
methods$label_propagation <- as.numeric(membership(cluster_label_prop(g)))
methods$infomap <- as.numeric(membership(cluster_infomap(g)))

if (!is.null(colnames(work_mat3))) {
  V(g)$name <- colnames(work_mat3)
} else {
  V(g)$name <- as.character(1:length(V(g))) # Default dacă nu există etichete
}

# metode clustering
node_intersections <- data.frame(
  node = V(g)$name,
  kmeans = methods$kmeans,
  leading_eigen = methods$leading_eigen,
  fast_greedy = methods$fast_greedy,
  louvain = methods$louvain,
  walktrap = methods$walktrap,
  label_propagation = methods$label_propagation,
  infomap = methods$infomap
)

# Intersectie + frecventa
node_intersections <- node_intersections %>%
  rowwise() %>%
  mutate(
    intersection_count = n_distinct(c_across(-node)),
    most_frequent = names(sort(table(c_across(-node)), decreasing = TRUE)[1])
  ) %>%
  ungroup()

# Creare excel
write.xlsx(node_intersections, "node_intersections2.xlsx")

# Atribuire culori 
color_scale <- colorRampPalette(c("yellow", "orange", "red"))
node_colors <- color_scale(max(node_intersections$intersection_count))[
  node_intersections$intersection_count
]
V(g)$color <- node_colors

# Vizualizare grafic
gjs <- graphjs(g, main = "Network!", bg = "gray", showLabels = F, stroke = F, 
               curvature = 0.1, attraction = 0.9, repulsion = 0.8, opacity = 0.9)

print(gjs)
saveWidget(gjs, file = "Media-Network-gjs.html")
browseURL("Media-Network-gjs.html")

gjs.an <- graphjs(g, bg = "white", showLabels = F, stroke = F, 
                  layout = list(layout_randomly(g, dim = 3),
                                layout_with_fr(g, dim = 3),
                                layout_with_drl(g, dim = 3),
                                layout_on_sphere(g)),
                  clickCallback = htmlwidgets::JS(
                    "function(node) {
      if (node !== null) {
        let nodeName = this.graph.nodes[node].name || 'Unknown Node';
        console.log('Node clicked:', nodeName);
        alert('You clicked on node: ' + nodeName);
      } else {
        console.log('No node clicked.');
      }
    }"
                  ),
                  vertex.color = list(V(g)$color, "gray", "orange", V(g)$color),
                  main = list("Random Layout",
                  , "Fruchterman-Reingold",  "DrL layout", "Sphere"))
print(gjs.an)
saveWidget(gjs.an, file = "Media-Network-gjs-an.html")
browseURL("Media-Network-gjs-an.html")

# Excel
#openXL("node_intersections2.xlsx")
