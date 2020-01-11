# ...................................................
# ALGORITHMS  ########################################
# Alternative Ways Mining Algorithm -----------------------------
MMDG_algorithm <- function(graphs){
  id_model_tags <- LETTERS # IDs for the mined models
  uncat_graphs <- graphs
  
  mined_models <- list()
  aggr_graphs <- list()
  
  # Data frame for graph categorisation results.
  df_cat_graphs <- data.frame(ID =  names(uncat_graphs), 
                              category = c("-"),
                              stringsAsFactors = FALSE)
  
  while(!is_empty(uncat_graphs)){
    
    # Get the type tag for the new mined plan
    id_model <- id_model_tags[1]
    
    # We added the dependency graphs not yet uncategorised.
    aggr_graph <- aggregate_graphs(uncat_graphs)
    
    # We provide some metadata to the aggregated graph. 
    aggr_graph <- add_summary_data_to_graph(aggr_graph, category=id_model, type="+", meta="") # Add the topology information to the graph as metadata
    
    # Calculate the threshold from aggregated graph applying the connectivity criteria.
    results <- refinement(aggr_graph, NULL, uncat_graphs)
    
    # Unlist the refinement results.
    model_graph <- results[[1]]
    cat_graphs <- results[[2]]
    
    # It creates some interesting metadata about the model obtained. Also, it adds the produced graphs to the global lists of results.
    model_graph <- add_summary_data_to_graph(model_graph, category=id_model, type="theta", meta="") # Add the topology information to the graph as metadata
    aggr_graphs[[as.character(paste(c("aggr", min(E(model_graph)$weight), id_model), collapse="_"))]] <- aggr_graph
    mined_models[[as.character(paste(c("thresh", min(E(model_graph)$weight), id_model), collapse="_"))]] <- model_graph
    
    # Add the current category to graphs which fulfil the connectivity condition
    df_cat_graphs$category[df_cat_graphs$ID %in% cat_graphs] <- id_model
    
    # Pop the trials that is possible to explain with the current mined plan
    uncat_graphs <- uncat_graphs[!(names(uncat_graphs) %in% cat_graphs)]
    id_model_tags <- id_model_tags[-1] # Pop current tag
  }
  
  output <- list()
  
  output$mined_models <- mined_models
  
  output$aggr_graphs <- aggr_graphs
  
  output$df_cat_graphs <- arrange(df_cat_graphs, (category),  ID)
  
  output$df_cat_graphs$category <-  as.factor(output$df_cat_graphs$category)
  
  return(output)
}

# Calculate the added dependency graph from a list of dependency graphs.
aggregate_graphs <- function(graphs){
  df_graphs <- lapply(graphs, as.data.frame)
  
  df_aggr = rbind_all(df_graphs) %>%
    group_by(V1, V2) %>%
    summarise(weight= sum(weight))
  
  aggr_graph = simplify(graph.data.frame(df_aggr, directed = T))
  E(aggr_graph)$weight=  df_aggr$weight
  E(aggr_graph)$label= E(aggr_graph)$weight
  return(aggr_graph)
}

# Calculate the appropriate threshold for obtaining a valid graph.
refinement <- function(aggr_g, threshold=NULL, graphs){
  # Initial threshold ................................
  if(is.null(threshold)){
    # First, we get the connected nodes to the start and the end vertices.
    vs <- grep(c("S"), as_ids(E(aggr_g)))
    vf <- grep(c("F"), as_ids(E(aggr_g)))
    # Then, we take the minimum weight from the maximum edges that connect with the start or the end.
    starting_max_weight <- max(edge_attr(aggr_g, "weight", index = E(aggr_g)[[vs]]))
    ending_max_weight <- max(edge_attr(aggr_g, "weight", index = E(aggr_g)[[vf]]))
    threshold <- min(c(starting_max_weight, ending_max_weight))
  }
  # ..................................................
  
  # Apply the obtained threshold and evaluate the connectivity.
  thresh_graph <- subgraph.edges(aggr_g,
                                      delete.vertices = FALSE,
                                      which(E(aggr_g)$weight >= threshold))
  
  # In case of the threshold "breaks" the graph, we decrease its value until
  # the thresholded graph is connected again.
  while(!is_valid(thresh_graph, "S", "F")){
    # Reduce the threshold until fulfilling the condition.
    threshold <- max(edge_attr(aggr_g, "weight", index = E(aggr_g)[[weight < threshold]]))
    
    # Apply the new threshold and evaluate again the connectivity
    thresh_graph <- subgraph.edges(aggr_g,
                                        delete.vertices = FALSE,
                                        which(E(aggr_g)$weight >= threshold))
  }
  
  # Check whether the valid thresh graph overlaps at least one of the input graphs.
  connectivity_results <- lapply(graphs,
                                 intersection,
                                 g2 = thresh_graph)
  ## We obtain which graph have completed the connectivity test after making the intersection with the thresholded graph.
  tested_graphs <- lapply(connectivity_results, "[[", 1)
  overlapped_graphs <- names(unlist(tested_graphs))[unlist(tested_graphs)==TRUE]
  
  ## If any graph meets the validity criterion after the overlap, we proceed with the recursive call by reducing the threshold. 
  if(!length(overlapped_graphs)){
    results <- refinement(aggr_g,
                          threshold=max(edge_attr(aggr_g, "weight", index = E(aggr_g)[[weight < threshold]])),
                          graphs)
    thresh_graph <- results[[1]]
    overlapped_graphs <- results[[2]]
  }
  
  # Return the threshold that satisfy our connectivity test.
  return(list(thresh_graph, overlapped_graphs))
}


# Check whether the set of nodes, which connect with the start and the ending, are the same.
is_valid <- function(graph, vertex_1="S", vertex_2="F"){
  # Take the vertices reachable from the start vertex and the vertices that reach the final vertex. 
  from_start <- names(ego(graph, 100, vertex_1, mode="out")[[1]])
  from_end <- names(ego(graph, 100, vertex_2, mode="in")[[1]])
  
  # Check whether the set of nodes connected with the start and the ending are the same.
  return(setequal(from_start, from_end))
}

# Check whether the set of nodes, which connect with the start and the ending, are the same.
connectivity_test <- function(graph, vertex_1="S", vertex_2="F"){
  # Take the vertices reachable from the start vertex and the vertices that reach the final vertex. 
  from_start <- names(ego(graph, 100, vertex_1, mode="out")[[1]])
  from_end <- names(ego(graph, 100, vertex_2, mode="in")[[1]])
  
  # Check whether the set of nodes connected with the start and the ending are the same.
  return(setequal(from_start, from_end))
}


# Evaluate whether the output graph still being connected after the intersection.
intersection <- function(g1, g2){
  diff_edges <- NULL
  
  # Get edge existing in the g1 but not in the g2
  diff_edges <- paste(as_ids(E(difference(g1, g2))), collapse=" ")
  
  # Remove the dismissed arrows from g1 
  # Same result as "intersection(g1, g2, byname = TRUE, keep.all.vertices = TRUE)"
  diff_graph <- g1 - edges(unlist(strsplit(as.character(diff_edges[1]), " ")))
  
  # Check whether the set of nodes connected with the start and the ending are the same.
  is_connected <- connectivity_test(diff_graph, "S", "F")
  
  # Return the test result and the graph generated by intersection
  return(list(is_connected, diff_graph))
}



# ------------------------------------------------------
# Labelling Algorithm ----------------------------------

apply_models_algorithm <- function(models, graphs){
  
  uncat_graphs <- graphs
  
  models_cpy <- models
  
  # Data frame for graph categorisation
  df_cat_graphs <- data.frame(ID =  names(uncat_graphs), 
                              category = c("-"),
                              stringsAsFactors = FALSE)
  
  while(!is_empty(models_cpy)){
    # Get the type tag for the new mined plan
    model <- models_cpy[1]
    
    # Get cat tag for the new group
    id_model <- unlist(strsplit(names(model), "_"))[3]
    
    # Check whether the valid thresh graph overlaps at least one of the input graphs.
    connectivity_results <- lapply(uncat_graphs,
                                   intersection,
                                   g2 = model[[1]])

    ## We obtain which graph have completed the connectivity test after making the intersection with the model.
    tested_graphs <- lapply(connectivity_results, "[[", 1)
    overlapped_graphs <- names(unlist(tested_graphs))[unlist(tested_graphs)==TRUE]
    
    # Add the current category to graphs which fulfil the connectivity condition
    df_cat_graphs$category[df_cat_graphs$ID %in% overlapped_graphs] <- id_model
    
    # Pop the trials that is possible to explain with the current mined plan
    uncat_graphs <- uncat_graphs[!(names(uncat_graphs) %in% overlapped_graphs)]
    models_cpy <- models_cpy[-1] # Pop current tag
  }
  
  output <- list()
  
  output$mined_models <- models_cpy
  
  output$df_cat_graphs <- arrange(df_cat_graphs, (category),  ID)
  
  output$df_cat_graphs$category <-  as.factor(output$df_cat_graphs$category)
  
  return(output)
}
# ------------------------------------------------------

# ...................................................

