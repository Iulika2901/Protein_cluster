library(igraph)
library(dplyr)
library(openxlsx)
install.packages("zoom")
library(zoom)

data <- load(file.choose())  

work_mat3 <- get("work_mat2")  

edges <- which(work_mat3 == 1, arr.ind = TRUE)
g <- graph_from_edgelist(as.matrix(edges), directed = FALSE)

methods <- list()

# 1. K-means clustering
node_features <- data.frame(
  degree = degree(g),
  closeness = closeness(g),
  betweenness = betweenness(g)
)
set.seed(123)
scaled_features <- scale(node_features)
kmeans_result <- kmeans(scaled_features, centers = 4, nstart = 25)
methods$kmeans <- as.numeric(kmeans_result$cluster)

# 2. Leading Eigenvector
methods$leading_eigen <- as.numeric(membership(cluster_leading_eigen(g)))

# 3. Edge Betweenness
#methods$edge_betweenness <- as.numeric(membership(cluster_edge_betweenness(g)))

# 4. Fast Greedy
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
methods$fast_greedy <- as.numeric(membership(cluster_fast_greedy(g)))

# 5. Louvain
methods$louvain <- as.numeric(membership(cluster_louvain(g)))

# 6. Multi-level
#methods$multilevel <- as.numeric(membership(cluster_multilevel(g)))

# 7. Walktrap
methods$walktrap <- as.numeric(membership(cluster_walktrap(g)))

# 8. Label Propagation
methods$label_propagation <- as.numeric(membership(cluster_label_prop(g)))

# 9. Infomap
methods$infomap <- as.numeric(membership(cluster_infomap(g)))

if (is.null(V(g)$name)) {
  V(g)$name <- as.character(1:length(V(g)))
}

# Verrify correct lenght for data frame
print(length(methods$kmeans))
print(length(methods$leading_eigen))
print(length(methods$fast_greedy))
print(length(methods$louvain))
print(length(methods$walktrap))
print(length(methods$label_propagation))
print(length(methods$infomap))


# combine in dataframe

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
     
# intersection
node_intersections <- node_intersections %>%
  rowwise() %>%
  mutate(
    intersection_count = n_distinct(c_across(-node)),
    most_frequent = names(sort(table(c_across(-node)), decreasing = TRUE)[1])
  ) %>%
  ungroup()

# create excel
write.xlsx(node_intersections, "node_intersections2.xlsx")

# intersections
color_scale <- colorRampPalette(c("yellow", "orange", "red"))

# colors
node_colors <- color_scale(max(node_intersections$intersection_count))[
  node_intersections$intersection_count
]
V(g)$color <- node_colors

# plot graf
plot(g, 
     vertex.color = V(g)$color,
     vertex.size = 8,
     vertex.label = NA,
     edge.color = "gray",
     main = "Intersectia maximala")

V(g)$label <- V(g)$name  #number for vertex

plot(
  g,
  vertex.color = V(g)$color,  
  vertex.size = 12,           
  vertex.label = V(g)$name,   
  vertex.label.cex = 1.2,     
  vertex.label.color = "black",
  edge.color = "gray",        
  edge.width = 0.5,           
  layout = layout_with_fr(g), 
  main = "Intersectia maximala (Zoom)"
)

openXL("node_intersections2.xlsx")

# create excel
write.xlsx(node_intersections_kl, "node_intersections2.xlsx")

#maximal intersection for each two cluster methods

# Creare graf
edges <- which(work_mat3 == 1, arr.ind = TRUE)
g <- graph_from_edgelist(as.matrix(edges), directed = FALSE)

# Aplicare metode clustering
methods <- list(
  kmeans = as.numeric(kmeans(scale(data.frame(
    degree = degree(g),
    closeness = closeness(g),
    betweenness = betweenness(g)
  )), centers = 4, nstart = 25)$cluster),
  leading_eigen = as.numeric(membership(cluster_leading_eigen(g))),
  fast_greedy = as.numeric(membership(cluster_fast_greedy(simplify(g)))),
  louvain = as.numeric(membership(cluster_louvain(g))),
  walktrap = as.numeric(membership(cluster_walktrap(g))),
  label_propagation = as.numeric(membership(cluster_label_prop(g))),
  infomap = as.numeric(membership(cluster_infomap(g)))
)

# Generare combinații de metode pentru perechi
method_names <- names(methods)
combinations <- combn(method_names, 2)

# Creare Excel cu intersecțiile pentru toate combinațiile
excel_filename <- "pairwise_intersections.xlsx"
wb <- createWorkbook()

for (i in 1:ncol(combinations)) {
  # Extrage metodele curente
  method1 <- combinations[1, i]
  method2 <- combinations[2, i]
  
  # Creare dataframe pentru pereche
  pairwise_intersection <- data.frame(
    node = V(g)$name,
    method1 = methods[[method1]],
    method2 = methods[[method2]]
  )
  
  # Calcul intersecții
  pairwise_intersection <- pairwise_intersection %>%
    rowwise() %>%
    mutate(
      intersection_count = n_distinct(c_across(-node)),
      most_frequent = names(sort(table(c_across(-node)), decreasing = TRUE)[1])
    ) %>%
    ungroup()
  
  # Adaugare în Excel
  addWorksheet(wb, sheetName = paste(method1, method2, sep = "_"))
  writeData(wb, sheet = paste(method1, method2, sep = "_"), pairwise_intersection)
}

# Salvare fișier Excel
saveWorkbook(wb, file = excel_filename, overwrite = TRUE)

# Deschidere fișier Excel
openXL(excel_filename)
