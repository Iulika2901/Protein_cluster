library(shiny)
library(bslib)
library(igraph)
library(mclust)
library(ggplot2)
library(shinybusy)
library(promises)
library(future)
library(threejs)
library(RColorBrewer)
library(htmltools)
library(gprofiler2)
library(dplyr)
library(corrplot)
library(visNetwork)
library(rpart) 
library(rpart.plot)

plan(sequential)

attach.entregene_gen <- function (x, map, x_name, map_name, map_target) {
  output <- merge(x, map, by.x=as.character(x_name), by.y=as.character(map_name), all.x=TRUE, sort=FALSE)
  output <- output[!is.na(output$map_gprofiler.target), ]
  return(output)
}

load_matrix_file <- function(datapath, filename) {
  ext <- tools::file_ext(filename)
  df <- NULL
  if (ext == "csv") df <- read.csv(datapath, row.names = 1)
  if (ext == "rds") df <- readRDS(datapath)
  if (ext == "RData") {
    tmp_env <- new.env()
    load(datapath, envir = tmp_env)
    if(exists("work_mat2", envir = tmp_env)) df <- get("work_mat2", envir = tmp_env)
    else {
      objs <- mget(ls(tmp_env), envir = tmp_env)
      for (o in objs) if (is.data.frame(o) || is.matrix(o)) df <- as.data.frame(o)
    }
  }
  return(as.matrix(df))
}

ui <- page_sidebar(
  title = "Graph Bioinformatics & Clustering Expert",
  sidebar = sidebar(
    h5("1. Data Loading (Single Source)"),
    fileInput("file", "Choose primary file (csv, rds, RData):", accept = c(".csv", ".RData", ".rds")),
    
    hr(),
    h5("1b. Multi-View Data Integration (Partially Shared)"),
    helpText("Upload different biological views. The algorithm will extract the consensus and preserve specific particularities."),
    fileInput("file_ppi", "1. PPI Network:", accept = c(".csv", ".RData", ".rds")),
    fileInput("file_coexp", "2. Gene Co-expression:", accept = c(".csv", ".RData", ".rds")),
    fileInput("file_go", "3. Gene Ontology (GO):", accept = c(".csv", ".RData", ".rds")),
    fileInput("file_loc", "4. Localization:", accept = c(".csv", ".RData", ".rds")),
    sliderInput("alpha_specific", "Specific Edge Weight (Particularities):", min = 0.1, max = 1, value = 0.5, step = 0.1),
    actionButton("btn_multiview", "Integrate Multi-View Data", class = "btn-success"),
    
    hr(),
    h5("2. Must-Link & Quality Control"),
    fileInput("file_ml", "Upload Must-Link CSV (Optional):", accept = c(".csv")),
    numericInput("ml_boost", "Must-Link Weight Boost:", min = 0, max = 1000, value = 10, step = 1),
    sliderInput("noise_level", "Noise Level (Jitter):", min = 0, max = 1, value = 0, step = 0.05),
    checkboxInput("filter_isolated", "Remove isolated clusters (<3 nodes)", value = FALSE),
    numericInput("max_betweenness", "Remove nodes with Betweenness > : (0 = ignore)", value = 0, min = 0),
    radioButtons("graph_type", "Workflow Type:",
                 choices = c("Simple Undirected" = "undirected",
                             "Biological Directed" = "directed"),
                 selected = "undirected"),
    
    hr(),
    h5("3. Clustering Algorithms"),
    checkboxInput("auto_k", "Auto Optimal K (for K-Means/Spectral)", value = FALSE),
    numericInput("k", "Manual K (nr. clusters):", min = 1, max = 10, value = 3),
    div(style="display: flex; flex-direction: column; gap: 5px;",
        actionButton("btn_kmeans", "Run K-Means", class = "btn-info"),
        actionButton("btn_spectral", "Run Spectral", class = "btn-info"),
        actionButton("btn_louvain", "Run Louvain"),
        actionButton("btn_leiden", "Run Leiden"),
        actionButton("btn_walktrap", "Run Walktrap"),
        actionButton("btn_fastgreedy", "Run Fast Greedy"),
        actionButton("btn_eigen", "Run Eigenvector"),
        actionButton("btn_label", "Label Propagation"),
        actionButton("btn_infomap", "Run Infomap")
    ),
    
    hr(),
    h5("4. Ensemble Methods"),
    selectInput("ensemble_combo", "Choose Combinations:",
                choices = c("Modularity (Louvain+Leiden+Greedy)" = "modularity",
                            "Random Walks (Walktrap+Infomap)" = "walks",
                            "Geometric (K-Means+Spectral)" = "geometric",
                            "Robust Consensus (All 8 Methods)" = "all")),
    actionButton("btn_ensemble", "Run Ensemble Consensus", class = "btn-primary"),
    
    hr(),
    h5("5. Machine Learning (Contrast Patterns)"),
    actionButton("btn_ml_patterns", "Extract ML Patterns & Scan", class = "btn-warning"),
    hr(),
    downloadButton("download_table", "Download Results Table"),
    verbatimTextOutput("log")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("2D Graph", visNetworkOutput("graphPlot", height = "650px")),
      tabPanel("3D Graph", scatterplotThreeOutput("graphPlot3D", height = "650px")),
      tabPanel("Multi-View Consensus", 
               helpText("Aceasta este esența inovației Partially Shared: Tabelul arată legăturile care sunt confirmate de multiple surse de date (Consensul Biologic)."),
               tableOutput("consensusTable")),
      tabPanel("Hubs", tableOutput("hubTable")),
      tabPanel("Method Comparison", tableOutput("comparisonTable")),
      tabPanel("Stability QA (ARI)", plotOutput("ariPlot")),
      tabPanel("ML Contrast Patterns",
               verbatimTextOutput("ml_rules_text"),
               hr(),
               tableOutput("ml_predictions_table"))
    )
  )
)

server <- function(input, output, session) {
  log_msgs <- reactiveVal(character())
  clustering_res <- reactiveVal(NULL)
  all_results <- reactiveValues()
  ml_patterns_res <- reactiveVal(list(rules = "", predictions = data.frame()))
  
   working_matrix <- reactiveVal(NULL)
  consensus_data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    req(input$file)
    mat <- load_matrix_file(input$file$datapath, input$file$name)
    working_matrix(mat)
    log_msgs(c(log_msgs(), paste(Sys.time(), "- Single data source loaded.")))
  })
  
  observeEvent(input$btn_multiview, {
    show_modal_spinner(text = "Integrating Multi-View Data (Partially Shared)...")
    
     mats <- list()
    if(!is.null(input$file_ppi)) mats[["PPI"]] <- load_matrix_file(input$file_ppi$datapath, input$file_ppi$name)
    if(!is.null(input$file_coexp)) mats[["CoExp"]] <- load_matrix_file(input$file_coexp$datapath, input$file_coexp$name)
    if(!is.null(input$file_go)) mats[["GO"]] <- load_matrix_file(input$file_go$datapath, input$file_go$name)
    if(!is.null(input$file_loc)) mats[["Loc"]] <- load_matrix_file(input$file_loc$datapath, input$file_loc$name)
    
    if(length(mats) < 2) {
      remove_modal_spinner()
      showNotification("Please upload at least 2 data sources for Multi-View Integration!", type="error")
      return()
    }
    
   all_nodes <- unique(unlist(lapply(mats, rownames)))
    n <- length(all_nodes)
    
     aligned_mats <- lapply(mats, function(m) {
      new_m <- matrix(0, n, n, dimnames=list(all_nodes, all_nodes))
      common_r <- intersect(rownames(m), all_nodes)
      common_c <- intersect(colnames(m), all_nodes)
      new_m[common_r, common_c] <- m[common_r, common_c]
      if(max(new_m, na.rm=TRUE) > 0) new_m <- new_m / max(new_m, na.rm=TRUE)
      return(new_m)
    })
    
    freq_mat <- matrix(0, n, n, dimnames=list(all_nodes, all_nodes))
    sum_mat <- matrix(0, n, n, dimnames=list(all_nodes, all_nodes))
    
    for(am in aligned_mats) {
      freq_mat <- freq_mat + (am > 0)
      sum_mat <- sum_mat + am
    }
    
   alpha <- input$alpha_specific
    final_mat <- matrix(0, n, n, dimnames=list(all_nodes, all_nodes))
    
    is_consensus <- freq_mat > 1
    is_specific <- freq_mat == 1
    
    final_mat[is_consensus] <- sum_mat[is_consensus] * freq_mat[is_consensus] 
    final_mat[is_specific] <- sum_mat[is_specific] * alpha                    
    
    working_matrix(final_mat)
    
    edges_idx <- which(is_consensus, arr.ind = TRUE)
    if(nrow(edges_idx) > 0) {
      cons_df <- data.frame(
        Node_1 = as.character(all_nodes[edges_idx[,1]]),
        Node_2 = as.character(all_nodes[edges_idx[,2]]),
        Sources_Confirmed = freq_mat[edges_idx],
        Consensus_Score = final_mat[edges_idx]
      ) %>% 
        filter(Node_1 != Node_2) %>% 
       mutate(
          MinNode = pmin(Node_1, Node_2),
          MaxNode = pmax(Node_1, Node_2)
        ) %>%
        distinct(MinNode, MaxNode, .keep_all = TRUE) %>%
        select(Node_1, Node_2, Sources_Confirmed, Consensus_Score) %>%
        arrange(desc(Sources_Confirmed), desc(Consensus_Score))
      
      consensus_data(head(cons_df, 50)) 
    }else {
      consensus_data(data.frame(Message="No consensus edges found across sources."))
    }
    
    remove_modal_spinner()
    log_msgs(c(log_msgs(), paste(Sys.time(), "- Multi-View Partially Shared Network Built (", length(mats), "sources).")))
  })
  
  output$consensusTable <- renderTable({ req(consensus_data()) })
  
  data_ml <- reactive({
    infile_ml <- input$file_ml
    if (is.null(infile_ml)) return(NULL)
    read.csv(infile_ml$datapath, sep = ";", header = FALSE, stringsAsFactors = FALSE)
  })
  
  observeEvent(input$btn_ml_patterns, {
    req(working_matrix())
    show_modal_spinner(text = "Machine Learning: Extracting Contrast Patterns...")
    curr_mat <- working_matrix()
    type <- input$graph_type
    
    g <- graph_from_adjacency_matrix(curr_mat, mode = ifelse(type == "directed", "directed", "undirected"), weighted = TRUE, diag = FALSE)
    g <- delete.edges(g, which(E(g)$weight <= 0))
    
    extract_topology <- function(graph, nodes_list, class_label) {
      df <- data.frame()
      for (nodes in nodes_list) {
        if(length(nodes) < 3) next
        sg <- induced_subgraph(graph, nodes)
        den <- edge_density(sg)
        trans <- transitivity(sg, type="global")
        if(is.na(trans)) trans <- 0
        diam <- diameter(sg, weights=NA)
        core <- max(coreness(sg))
        df <- rbind(df, data.frame(Density = den, Transitivity = trans, Diameter = diam, MaxCore = core, Class = class_label, Nodes = paste(nodes, collapse=",")))
      }
      return(df)
    }
    tryCatch({
      hubs <- names(sort(degree(g), decreasing = TRUE)[1:min(10, vcount(g))])
      real_complexes <- lapply(hubs, function(h) { unique(c(h, names(neighbors(g, h)))) })
      set.seed(42)
      random_complexes <- lapply(1:20, function(x) { sample(V(g)$name, size = sample(4:15, 1)) })
      
      train_real <- extract_topology(g, real_complexes, "Order")
      train_chaos <- extract_topology(g, random_complexes, "Chaos")
      train_data <- rbind(train_real, train_chaos)
      
      tree_model <- rpart(Class ~ Density + Transitivity + Diameter + MaxCore, data = train_data, method = "class", control = rpart.control(minsplit = 2, cp = 0.01))
      rules_text <- capture.output(rpart.rules(tree_model, cover = TRUE, nn = TRUE))
      rules_formatted <- paste("Discovered contrast patterns:\n", paste(rules_text, collapse = "\n"), "\n\n(Explanation: If an unknown subgraph follows an 'Order' rule, it is flagged as a new protein complex.)")
      
      candidate_clusters <- cluster_louvain(g)
      candidate_list <- split(V(g)$name, membership(candidate_clusters))
      candidates_df <- extract_topology(g, candidate_list, "Unknown")
      
      if(nrow(candidates_df) > 0) {
        predictions <- predict(tree_model, candidates_df, type = "class")
        candidates_df$Prediction <- predictions
        valid_complexes <- candidates_df %>% filter(Prediction == "Order") %>% select(Nodes, Density, Transitivity, Diameter, MaxCore) %>% mutate(Density = round(Density, 2), Transitivity = round(Transitivity, 2))
      } else {
        valid_complexes <- data.frame(Message="Not enough candidates generated.")
      }
      ml_patterns_res(list(rules = rules_formatted, predictions = valid_complexes))
      log_msgs(c(log_msgs(), paste(Sys.time(), "- ML Contrast Patterns successfully extracted.")))
    }, error = function(e) {
      ml_patterns_res(list(rules = paste("Error generating patterns:", e$message), predictions = data.frame()))
    })
    remove_modal_spinner()
  })
  
  output$ml_rules_text <- renderText({ ml_patterns_res()$rules })
  output$ml_predictions_table <- renderTable({ ml_patterns_res()$predictions })
  
   methods_map <- list(btn_kmeans="kmeans", btn_louvain="louvain", btn_walktrap="walktrap",
                      btn_fastgreedy="fastgreedy", btn_label="label", btn_infomap="infomap",
                      btn_leiden="leiden", btn_spectral="spectral", btn_eigen="eigen",
                      btn_ensemble="ensemble")
  
  lapply(names(methods_map), function(btn_id) {
    observeEvent(input[[btn_id]], {
      req(working_matrix())
      show_modal_spinner(text = paste("Parallel computing:", methods_map[[btn_id]], "..."))
      
      method_name <- methods_map[[btn_id]]
      curr_mat <- working_matrix()
      
      input_noise <- input$noise_level
      input_filter <- input$filter_isolated
      input_max_betw <- input$max_betweenness
      type <- input$graph_type
      k_val <- input$k
      auto_k <- input$auto_k
      combo_type <- input$ensemble_combo
      ml_data_val <- isolate(data_ml())
      ml_boost_val <- isolate(input$ml_boost)
      
      future({
        run_clustering <- function(met, gr, lay, k_v, a_k) {
          if (met %in% c("kmeans", "spectral")) {
            k_use <- k_v
            if (a_k) {
              mclust_model <- suppressWarnings(Mclust(lay, verbose = FALSE))
              if (!is.null(mclust_model$G)) k_use <- mclust_model$G
            }
            k_use <- max(2, k_use)
            if (met == "kmeans") {
              return(as.integer(kmeans(lay, centers = k_use)$cluster))
            } else {
              embed_res <- embed_laplacian_matrix(gr, no = k_use)$X
              return(as.integer(kmeans(embed_res, centers = k_use)$cluster))
            }
          } else {
            comm <- switch(met,
                           louvain = cluster_louvain(gr, weights = E(gr)$weight),
                           walktrap = cluster_walktrap(gr, weights = E(gr)$weight),
                           fastgreedy = cluster_fast_greedy(gr, weights = E(gr)$weight),
                           label = cluster_label_prop(gr, weights = E(gr)$weight),
                           infomap = cluster_infomap(gr, e.weights = E(gr)$weight),
                           leiden = cluster_leiden(gr, objective_function = "modularity", weights = E(gr)$weight),
                           eigen = cluster_leading_eigen(gr, weights = E(gr)$weight))
            return(as.integer(membership(comm)))
          }
        }
        
        if (input_noise > 0) {
          noise <- matrix(runif(length(curr_mat), -input_noise, input_noise), nrow = nrow(curr_mat))
          curr_mat <- curr_mat + noise
          curr_mat[curr_mat < 0] <- 0
        }
        
        if (!is.null(ml_data_val) && nrow(ml_data_val) > 0) {
          max_w <- max(curr_mat, na.rm = TRUE)
          if (max_w == 0) max_w <- 1
          for (i in 1:nrow(ml_data_val)) {
            n1 <- as.character(ml_data_val[i, 1])
            n2 <- as.character(ml_data_val[i, 2])
            if (n1 %in% rownames(curr_mat) && n2 %in% colnames(curr_mat)) {
              force_weight <- max_w * ml_boost_val * 100
              curr_mat[n1, n2] <- curr_mat[n1, n2] + force_weight
              if (type == "undirected" && n2 %in% rownames(curr_mat) && n1 %in% colnames(curr_mat)) {
                curr_mat[n2, n1] <- curr_mat[n2, n1] + force_weight
              }
            }
          }
        }
        
        g <- graph_from_adjacency_matrix(curr_mat, mode = ifelse(type == "directed", "directed", "undirected"), weighted = TRUE, diag = FALSE)
        g <- delete.edges(g, which(E(g)$weight <= 0))
        
        if (input_max_betw > 0) {
          betw_vals <- betweenness(g)
          g <- delete.vertices(g, V(g)[betw_vals > input_max_betw])
        }
        if (input_filter) {
          comps <- components(g)
          keep_nodes <- which(comps$membership %in% which(comps$csize >= 3))
          g <- subgraph(g, keep_nodes)
        }
        if (vcount(g) == 0) stop("The network is empty after filtering!")
        
        lay2d <- layout_with_fr(g, weights = E(g)$weight)
        lay3d <- layout_with_fr(g, dim = 3, weights = E(g)$weight)
        
        if (method_name == "ensemble") {
          methods_to_run <- switch(combo_type,
                                   "modularity" = c("louvain", "leiden", "fastgreedy"),
                                   "walks" = c("walktrap", "infomap"),
                                   "geometric" = c("kmeans", "spectral"),
                                   "all" = c("louvain", "leiden", "fastgreedy", "walktrap", "infomap", "kmeans", "spectral", "eigen"))
          cl_matrix <- matrix(0, nrow=vcount(g), ncol=length(methods_to_run))
          for(i in 1:length(methods_to_run)) {
            cl_matrix[, i] <- run_clustering(methods_to_run[i], g, lay2d, k_val, auto_k)
          }
          dist_mat <- matrix(0, nrow=vcount(g), ncol=vcount(g))
          for(m in 1:ncol(cl_matrix)) {
            dist_mat <- dist_mat + (outer(cl_matrix[,m], cl_matrix[,m], "!="))
          }
          dist_mat <- dist_mat / ncol(cl_matrix)
          hc <- hclust(as.dist(dist_mat), method="average")
          k_cons <- k_val
          if(auto_k) {
            mclust_model <- suppressWarnings(Mclust(lay2d, verbose = FALSE))
            if (!is.null(mclust_model$G)) k_cons <- mclust_model$G
          }
          k_cons <- max(2, k_cons)
          cl <- cutree(hc, k = k_cons)
        } else {
          cl <- run_clustering(method_name, g, lay2d, k_val, auto_k)
        }
        
        hub_df <- data.frame(
          Node = V(g)$name,
          Degree = degree(g),
          Betweenness = round(betweenness(g), 2)
        ) %>% arrange(desc(Degree))
        
        list(g = g, layout = lay2d, layout3d = lay3d, cluster = cl,
             method = method_name, node_names = V(g)$name, hubs = hub_df)
      }, seed = TRUE) %...>% (function(res) {
        remove_modal_spinner()
        clustering_res(res)
        all_results[[res$method]] <- data.frame(Node = res$node_names, Cluster = res$cluster)
        log_msgs(c(log_msgs(), paste(Sys.time(), "-", res$method, "finished (Nodes:", vcount(res$g), ")")))
      }) %...!% (function(e) {
        remove_modal_spinner()
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
  
  output$graphPlot <- renderVisNetwork({
    res <- clustering_res()
    req(res)
    palette <- brewer.pal(8, "Set2")
    color_idx <- as.numeric(res$cluster)
    node_colors <- palette[(color_idx - 1) %% 8 + 1]
    nodes <- data.frame(id = V(res$g)$name, label = V(res$g)$name, title = paste("Node:", V(res$g)$name, "<br>Cluster:", res$cluster), color = node_colors, x = res$layout[,1] * 200, y = res$layout[,2] * 200)
    edges <- igraph::as_data_frame(res$g, what = "edges")
    cluster_map <- setNames(res$cluster, V(res$g)$name)
    color_map <- setNames(node_colors, V(res$g)$name)
    from_cluster <- cluster_map[edges$from]
    to_cluster <- cluster_map[edges$to]
    edges$color <- ifelse(from_cluster == to_cluster, color_map[edges$from], "#e0e0e0")
    edges$width <- ifelse(from_cluster == to_cluster, 3, 0.5)
    visNetwork(nodes, edges, main = paste("Method:", res$method)) %>% visNodes(physics = FALSE, size = 20, font = list(size = 14)) %>% visEdges(smooth = FALSE) %>% visInteraction(zoomView = TRUE, dragView = TRUE, navigationButtons = TRUE) %>% visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE))
  })
  
  output$graphPlot3D <- renderScatterplotThree({
    res <- clustering_res()
    req(res)
    palette <- brewer.pal(8, "Set2")
    node_colors <- palette[as.numeric(res$cluster) %% 8 + 1]
    graphjs(res$g, layout = res$layout3d, vertex.color = node_colors, vertex.size = 0.5, main = paste("3D Projection -", res$method))
  })
  
  output$hubTable <- renderTable({ req(clustering_res()); head(clustering_res()$hubs, 15) }, digits = 2)
  
  output$comparisonTable <- renderTable({
    methods_active <- names(reactiveValuesToList(all_results))
    req(length(methods_active) > 0)
    df_list <- lapply(methods_active, function(m) { d <- all_results[[m]]; colnames(d)[2] <- m; d })
    Reduce(function(x, y) merge(x, y, by = "Node", all = TRUE), df_list)
  }, digits = 0)
  
  output$ariPlot <- renderPlot({
    methods <- names(reactiveValuesToList(all_results))
    req(length(methods) >= 2)
    n <- length(methods)
    ari_mat <- matrix(1, n, n, dimnames = list(methods, methods))
    for(i in 1:(n-1)) {
      for(j in (i+1):n) {
        merged <- merge(all_results[[methods[i]]], all_results[[methods[j]]], by = "Node")
        if(nrow(merged) > 1) { ari_mat[i,j] <- ari_mat[j,i] <- adjustedRandIndex(merged$Cluster.x, merged$Cluster.y) }
      }
    }
    corrplot(ari_mat, method = "pie", type = "upper", addCoef.col = "black", title = "Cluster Stability QA (ARI Index)", mar = c(0,0,2,0))
  })
  
  output$log <- renderText({ paste(log_msgs(), collapse = "\n") })
  output$download_table <- downloadHandler(
    filename = function() { paste("results_", Sys.Date(), ".csv", sep="") },
    content = function(file) { req(clustering_res()); write.csv(clustering_res()$hubs, file, row.names = FALSE) }
  )
}

shinyApp(ui, server)
