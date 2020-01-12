# ...................................................
# Data transformation functions #####################

# Transform a list of labels (ie., sequence of activities) into an adjacent vector
trial_to_adjacent_vectors <- function(x){
  av <- x[,] # Global variable
  av <- droplevels(av)
  sapply(levels(droplevels(x[,])), function(y){
    factor_ <- as.character(y)
    factor_positions <- which(av==factor_)
    av <<- insert_values(as.vector(av),
                         factor_, 
                         factor_positions +
                           seq(1,
                               length(factor_positions)))
  })
  av <- as.factor(av)
  
  # Remove duplicate nodes at the begining and at the end
  av <- av[-1]
  av <- av[-length(av)]
  return(av)
}

# Transform a sequence of labels into a weighted dependency graph.
sequence_to_dependency_graph <- function(seq){
  # First, we transform the sequence into an adjacent vector.
  adjvect <- trial_to_adjacent_vectors(seq)
  
  # Next, we create the directed graph associated with the adjacent vector. 
  # By default, igraph creates a multiedge graph when there are repeated edges.
  dep_graph <- graph(edges=as.character(adjvect))
  
  # In order to transform the multiedge graph into a weighted graph, we add a weight of 1 to every edge in the graph.
  E(dep_graph)$weight <- 1
  
  # Finally, we merge the common edges by applying a summing their weights.
  dep_graph <- simplify(dep_graph, edge.attr.comb=list(weight="sum"))
  
  return(dep_graph)
}


# Graph Visualisation with visnetwork
igraph_to_visnetwork <- function(graph){
  visgraph <- toVisNetworkData(graph)
  visgraph$nodes["shape"] <- c("ellipse")
  visgraph$nodes["color"] <- c("#E8E6ED")
  visgraph$nodes["font.color"] <- c("#0C0C0D")
  visgraph$nodes[which(visgraph$nodes$id=="F"),"color"] <- c("#4F191F")
  visgraph$nodes[which(visgraph$nodes$id=="S"),"color"] <- c("#4F191F")
  visgraph$nodes[which(visgraph$nodes$id=="F"),"font.color"] <- c("#E8E6ED")
  visgraph$nodes[which(visgraph$nodes$id=="S"),"font.color"] <- c("#E8E6ED")
  visgraph$edges["color"] <- c("#4B4A4C")
  visgraph$edges["arrows"] <- c("to")
  visgraph$edges["label"] <- E(graph)$weight
  #visgraph$edges["label"] <-  paste("A", 0:(nrow(visgraph$edges)-1)) # Add the edge id label
  return(visgraph)
}

# My function to plot igraph graphs with visnetwork with a nice presentantion.
plot_visnetwork <- function(graph, name="", save=F){
  visgraph <- igraph_to_visnetwork(graph)
  visnetwork <-visNetwork(nodes=visgraph$nodes, edges=visgraph$edges, main=name)
  if(save == T){
    save_graph(visgraph, paste(name, collapse = "_"))
  }
  return(visnetwork)
}

# Export a graph in html file
save_graph <- function(visgraph, graph_name){
  
  filename <- paste(c(graph_name,".html"), collapse = "")
  
  network<- visNetwork(nodes = visgraph$nodes, 
                       edges = visgraph$edges, 
                       main=paste0("<p style='font-size: 25px;'><b>",graph_name,"</b></p>"),
                       width="100%")  %>%
    visPhysics(stabilization = TRUE,  barnesHut = list(avoidOverlap =  0, 
                                                       centralGravity =  0,
                                                       springLength =  100,
                                                       springConstant = 0.035)) %>%
    visEdges(smooth = TRUE) %>%
    visExport(type = "pdf", name = graph_name,
              float = "left", label = "Save network", background = "white", style= "")
  
  htmlwidgets::saveWidget(network,file.path(normalizePath(dirname(filename)),basename(filename)))
  
}
# ...................................................


# ...................................................
# Graph connectivity functions ##############

# Transform a graph into an array of adjacent edges. The adjacency is represented as "a|b", 
# where a and b are vertex labels.
get_edges_array <- function(g){
  return(apply(as_edgelist(g), 1, function(x) paste0(c(x[1],x[2]),collapse = "|")))
}

# ...................................................

# ...................................................
# Outputs in a file functions #######################

# Export the similarity matrices to Excel files.
results_to_excel_file <- function(results, filename){
  wb <- createWorkbook()
  
  # We create a sheet for each kernel result.
  sheetnames <- names(results)
  
  sheets <- lapply(sheetnames, createSheet, wb = wb)
  
  void <- Map(addDataFrame, results, sheets)
  
  saveWorkbook(wb, file = filename)
}

# ...................................................

# ...................................................
# Other functions ###################################

# Remove the isolated vertices (disconnected) from a graph
delete_isolates <- function(graph){
  return(igraph::delete.vertices(graph, which(igraph::degree(graph) == 0)))
}

# Evaluate if a set is empty or not (improve the code readability).
is_empty <- function(x){
  return(length(x) == 0)
}

insert_values <- function(x, value, index) {
  if (length(value) == 1) 
    value = rep(value, length(index))
  if (length(value) < length(index)) 
    stop("length of repacement does not match index")
  z <- numeric(length(x) + length(index))
  z[index] <- value
  z[-index] <- x
  return(z)
}

# 
format.summary.character <- function(x, ...) {
  s <- summary.default(as.character(x), ...)
  format(structure(as.character(s), names = names(s), dim = dim(s),
                   dimnames = dimnames(s)), ...)
}

# Extract the metadata from the graph and return a dataframe with that information.
add_row <- function(graph, dataframe){
  df <- rbind(dataframe, data.frame("iter" = graph$category,
                                    "exp"= graph$meta,
                                    "element" = "V",
                                    "type"= graph$type,
                                    "value" = graph$num_vertices,
                                    "name" = paste0(c(graph$meta,
                                                      "V",
                                                      graph$type),
                                                    collapse="-")))
  df <- rbind(df, data.frame("iter" = graph$category,
                             "exp"=   graph$meta,
                             "element" = "E",
                             "type"=  graph$type,
                             "value" = graph$num_edges,
                             "name"= paste0(c(graph$meta,
                                              "E",
                                              graph$type),
                                            collapse="-")))
  return(df)
}

# Create PDF files for plots
openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}
# -------------------------------------------------------

# =======================================================
# Functions regarding the mining backbone process
# =======================================================
# Transform a graph into an array of adjacent edges. The adjacency is represented as "a|b", 
# where a and b are vertex labels.
get_edges_array <- function(g){
  return(apply(as_edgelist(g), 1, function(x) paste0(c(x[1],x[2]),collapse = "|")))
}

# Remove the isolated vertices (disconnected) from a graph
delete_isolates <- function(graph){
  return(igraph::delete.vertices(graph, which(igraph::degree(graph) == 0)))
}

# Add the metadata to a graph
add_summary_data_to_graph <- function(graph, category="", type="", meta=""){
  
  graph$num_vertices <- length(V(delete_isolates(graph)))
  
  graph$num_edges <- length(E(delete_isolates(graph)))
  
  graph$max_weight <- max(E(graph)$weight)
  
  graph$threshold <- min(E(graph)$weight)
  
  graph$type <- type
  
  graph$category <- category
  
  graph$meta <- meta
  
  return(graph)
}  

# helper function (from: https://stackoverflow.com/a/31437336)
as.data.frame.igraph <- function(g) {
  # prepare data frame
  res= cbind(as.data.frame(get.edgelist(g)),
             asDF(g)$edges)[ , c(-3, -4)]
  # unfactorize
  res$V1= as.character(res$V1)
  res$V2= as.character(res$V2)
  # return df
  res
}

# =======================================================
