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

plan(multisession)

attach.entregene_gen <- function (x, map, x_name, map_name, map_target) {
  output <- merge(x, map, by.x=as.character(x_name), by.y=as.character(map_name), all.x=TRUE, sort=FALSE)
  output <- output[!is.na(output$map_gprofiler.target), ]
  return(output)
}

ui <- page_sidebar(
  title = "Graph Bioinformatics & Clustering Expert",
  sidebar = sidebar(
    h5("1. Data Loading"),
    fileInput("file", "Choose file (csv, rds, RData):", accept = c(".csv", ".RData", ".rds")),
    radioButtons("graph_type", "Workflow Type:",
                 choices = c("Simple Undirected" = "undirected", 
                             "Biological Directed" = "directed"),
                 selected = "undirected"),
    hr(),
    h5("2. Quality Control & Robustness"),
    sliderInput("noise_level", "Noise Level (Jitter):", min = 0, max = 1, value = 0, step = 0.05),
    checkboxInput("filter_isolated", "Remove isolated clusters (<3 nodes)", value = FALSE),
    numericInput("max_betweenness", "Remove nodes with Betweenness > : (0 = ignore)", value = 0, min = 0),
    hr(),
    h5("3. Clustering Algorithms"),
    checkboxInput("auto_k", "Auto Optimal K (for K-Means/Spectral/Ensemble)", value = FALSE),
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
    downloadButton("download_table", "Download Results Table"),
    verbatimTextOutput("log")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("2D Graph", 
               helpText("Tip: Scroll over the graph to ZOOM IN/OUT. Click and drag nodes or the background to pan."),
               visNetworkOutput("graphPlot", height = "650px")), 
      tabPanel("3D Graph", 
               helpText("Tip: Scroll with your mouse over the plot to ZOOM. Click and drag to ROTATE."),
               scatterplotThreeOutput("graphPlot3D", height = "650px")),
      tabPanel("Hubs", 
               helpText("Nodes with high degree (hubs) are essential for network stability."),
               tableOutput("hubTable")),
      tabPanel("Method Comparison", 
               helpText("Comparison of cluster membership across all executed methods."),
               tableOutput("comparisonTable")),
      tabPanel("Stability QA (ARI)", 
               helpText("ARI (Adjusted Rand Index) correlation matrix. Values over 0.7 indicate high stability."),
               plotOutput("ariPlot"))
    )
  )
)

server <- function(input, output, session) {
  
  log_msgs <- reactiveVal(character())
  clustering_res <- reactiveVal(NULL)
  all_results <- reactiveValues() 
  
  data_raw <- reactive({
    infile <- input$file
    req(infile)
    ext <- tools::file_ext(infile$name)
    df <- NULL
    if (ext == "csv") df <- read.csv(infile$datapath, row.names = 1)
    if (ext == "rds") df <- readRDS(infile$datapath)
    if (ext == "RData") {
      tmp_env <- new.env()
      load(infile$datapath, envir = tmp_env)
      if(exists("work_mat2", envir = tmp_env)) df <- get("work_mat2", envir = tmp_env)
      else {
        objs <- mget(ls(tmp_env), envir = tmp_env)
        for (o in objs) if (is.data.frame(o) || is.matrix(o)) df <- as.data.frame(o)
      }
    }
    return(as.matrix(df))
  })
  
  methods_map <- list(btn_kmeans="kmeans", btn_louvain="louvain", btn_walktrap="walktrap", 
                      btn_fastgreedy="fastgreedy", btn_label="label", btn_infomap="infomap",
                      btn_leiden="leiden", btn_spectral="spectral", btn_eigen="eigen",
                      btn_ensemble="ensemble")
  
  lapply(names(methods_map), function(btn_id) {
    observeEvent(input[[btn_id]], {
      req(data_raw())
      show_modal_spinner(text = paste("Parallel computing:", methods_map[[btn_id]], "..."))
      
      method_name <- methods_map[[btn_id]]
      curr_mat <- data_raw()
      input_noise <- input$noise_level
      input_filter <- input$filter_isolated
      input_max_betw <- input$max_betweenness
      type <- input$graph_type
      k_val <- input$k
      auto_k <- input$auto_k
      combo_type <- input$ensemble_combo
      
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
                           louvain = cluster_louvain(gr),
                           walktrap = cluster_walktrap(gr),
                           fastgreedy = cluster_fast_greedy(gr),
                           label = cluster_label_prop(gr),
                           infomap = cluster_infomap(gr),
                           leiden = cluster_leiden(gr, objective_function = "modularity"),
                           eigen = cluster_leading_eigen(gr))
            return(as.integer(membership(comm)))
          }
        }
        
        if (input_noise > 0) {
          noise <- matrix(runif(length(curr_mat), -input_noise, input_noise), nrow = nrow(curr_mat))
          curr_mat <- curr_mat + noise
          curr_mat[curr_mat < 0] <- 0
        }
        
        g <- graph_from_adjacency_matrix(curr_mat, 
                                         mode = ifelse(type == "directed", "directed", "undirected"), 
                                         weighted = TRUE, diag = FALSE)
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
        
        lay2d <- layout_with_fr(g)
        lay3d <- layout_with_fr(g, dim = 3)
        
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
    
    nodes <- data.frame(
      id = V(res$g)$name,
      label = V(res$g)$name,
      title = paste("Node:", V(res$g)$name, "<br>Cluster:", res$cluster), 
      color = node_colors,
      x = res$layout[,1] * 200, 
      y = res$layout[,2] * 200
    )
    
    edges <- igraph::as_data_frame(res$g, what = "edges")
    
    cluster_map <- setNames(res$cluster, V(res$g)$name)
    color_map <- setNames(node_colors, V(res$g)$name)
    
    from_cluster <- cluster_map[edges$from]
    to_cluster <- cluster_map[edges$to]
    
   edges$color <- ifelse(from_cluster == to_cluster, color_map[edges$from], "#e0e0e0")
    
    edges$width <- ifelse(from_cluster == to_cluster, 3, 0.5)
    
    visNetwork(nodes, edges, main = paste("Method:", res$method)) %>%
      visNodes(physics = FALSE, size = 20, font = list(size = 14)) %>%
      visEdges(smooth = FALSE) %>% 
      visInteraction(zoomView = TRUE, dragView = TRUE, navigationButtons = TRUE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE))
  })
  
  output$graphPlot3D <- renderScatterplotThree({
    res <- clustering_res()
    req(res)
    palette <- brewer.pal(8, "Set2")
    node_colors <- palette[as.numeric(res$cluster) %% 8 + 1]
    
    graphjs(res$g, 
            layout = res$layout3d, 
            vertex.color = node_colors, 
            vertex.size = 0.5, 
            main = paste("3D Projection -", res$method))
  })
  
  output$hubTable <- renderTable({
    res <- clustering_res()
    req(res)
    head(res$hubs, 15)
  }, digits = 2)
  
  output$comparisonTable <- renderTable({
    methods_active <- names(reactiveValuesToList(all_results))
    req(length(methods_active) > 0)
    
    df_list <- lapply(methods_active, function(m) {
      d <- all_results[[m]]
      colnames(d)[2] <- m
      d
    })
    
    comp_df <- Reduce(function(x, y) merge(x, y, by = "Node", all = TRUE), df_list)
    return(comp_df)
  }, digits = 0)
  
  output$ariPlot <- renderPlot({
    methods <- names(reactiveValuesToList(all_results))
    req(length(methods) >= 2)
    
    n <- length(methods)
    ari_mat <- matrix(1, n, n, dimnames = list(methods, methods))
    
    for(i in 1:(n-1)) {
      for(j in (i+1):n) {
        m1 <- all_results[[methods[i]]]
        m2 <- all_results[[methods[j]]]
        merged <- merge(m1, m2, by = "Node")
        if(nrow(merged) > 1) {
          ari_mat[i,j] <- ari_mat[j,i] <- adjustedRandIndex(merged$Cluster.x, merged$Cluster.y)
        }
      }
    }
    corrplot(ari_mat, method = "pie", type = "upper", addCoef.col = "black", 
             title = "Cluster Stability QA (ARI Index)", mar = c(0,0,2,0))
  })
  
  output$log <- renderText({ paste(log_msgs(), collapse = "\n") })
  
  output$download_table <- downloadHandler(
    filename = function() { paste("results_", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      res <- clustering_res()
      req(res)
      write.csv(res$hubs, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
