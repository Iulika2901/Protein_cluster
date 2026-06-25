<div align="center">
  
# 3 D &nbsp; N E T W O R K &nbsp; A N A L Y S I S

**Advanced Clustering & Interactive Graph Visualization in R**

<br>
  
> A comprehensive analytical toolset built in R designed to perform multi-algorithm community detection, evaluate topological node features, and render highly interactive, animated 3D network graphs.

<br>

</div>

&nbsp;

## Overview
<br>

This repository contains an advanced R-based pipeline for network analysis. The script processes adjacency matrices to construct complex undirected graphs, calculates critical node metrics (degree, closeness, betweenness), and applies multiple machine learning and community detection algorithms to identify structural clusters. 

Beyond backend analysis, the project emphasizes data visualization by exporting the mathematical consensus of these clustering methods into interactive 3D HTML widgets and structured Excel reports for further statistical review.

&nbsp;

---

&nbsp;

## Key Features
<br>

* **Multi-Algorithm Clustering:** Implements a robust community detection approach by utilizing seven distinct algorithms (K-Means, Leading Eigenvector, Fast Greedy, Louvain, Walktrap, Label Propagation, and Infomap).
* **Consensus Intersection Calculation:** Automatically evaluates the agreement across all clustering methods for each node, determining the distinct cluster count and the most frequent community assignment.
* **Interactive 3D Visualization:** Generates manipulable 3D graph representations with custom JavaScript callbacks for node-click events, zooming, and panning.
* **Dynamic Layout Animations:** Features animated transitions between multiple mathematical graph layouts (Random, Fruchterman-Reingold, DrL, and Spherical mappings) within the browser.
* **Automated Data Export:** Compiles the intersection data and feature metrics into a clean, formatted Excel spreadsheet for external reporting.

&nbsp;

---

&nbsp;

## Technology Stack
<br>

The pipeline leverages several high-performance R packages for graph mathematics, data manipulation, and web-based rendering.

| Library | Primary Function | Application Context |
| :--- | :--- | :--- |
| **`igraph`** | Network Mathematics | Constructs the edgelist, calculates centralities, and executes community detection. |
| **`threejs`** | 3D Rendering | Renders the `graphjs` objects into interactive WebGL-based HTML widgets. |
| **`dplyr` & `magrittr`** | Data Manipulation | Handles data frame mutations, row-wise operations, and pipeline (`%>%`) logic. |
| **`openxlsx`** | Data Export | Writes the final `node_intersections` data frames into Excel workbooks. |
| **`htmlwidgets`** | Web Integration | Binds R-generated JavaScript payloads into standalone HTML files. |

&nbsp;

---

&nbsp;

## System Methodology
<br>

The script operates through a structured analytical pipeline:

1. **Graph Construction:** Ingests an adjacency matrix (`work_mat3`), extracts valid edges, and builds an undirected `igraph` network object.
2. **Feature Scaling:** Extracts node-level topological features (Degree, Closeness, Betweenness), handles infinite/NA values, and standardizes the data using Z-score scaling.
3. **Community Detection:** Applies the scaled features to a K-Means algorithm ($k=4$), followed by six topological clustering methods provided by the `igraph` library.
4. **Data Aggregation:** Compiles the cluster assignments into a unified data frame, computing the intersection count (consensus) and frequency for each individual node.
5. **Color Mapping:** Maps the intersection count to a continuous color scale (Yellow -> Orange -> Red) to visually highlight nodes with high clustering variability.

&nbsp;

---

&nbsp;

## Getting Started
<br>

Follow these instructions to configure your R environment and execute the network analysis script.

### Prerequisites

Ensure you have a recent version of R or RStudio installed. The script will automatically attempt to install the required packages, but you can manually install them using the CRAN repository:

```R
install.packages(c("igraph", "dplyr", "magrittr", "openxlsx", "threejs", "htmlwidgets", "visNetwork", "stringi", "zoom"))
