# ...................................................
# Experiments #######################################

# 4 experiments:\\
# JIGSAWS (Suturing)
# Experiment 1: \\
#     Training -> Experts (E) \\
#     Test -> the rest (I + N) \\
# Experiment 2: \\
#     Training -> Q1 \\
#     Test -> Q2 + Q3 + Q4\\
# Experiment 3: \\
#   Training -> Q1 + Q2 \\
#   Test -> Q3 + Q4\\
# MMAC (Brownie)
#   Training -> ALL
# ===============================

# -------------------------------

## SETTINGS =======================================
experiments_config <- list()
experiments_config$exp1 <- list()
experiments_config$exp2 <- list()
experiments_config$exp3 <- list()


### Experiment 1 -----------------------------------
# Training -> E
# Test -> I + N
df_scores <- jigsaws_data$df_scores
experiments_config$exp1$df_training  <- df_scores[df_scores$self_level=="E",]
experiments_config$exp1$df_test <- df_scores[(df_scores$self_level=="I" | df_scores$self_level=="N"),]

### Experiment 2 -----------------------------------
# Training -> Q1
# Test -> Q2 + Q3 + Q4

experiments_config$exp2$df_training  <- df_scores[df_scores$Q=="Q1",]
experiments_config$exp2$df_test  <- df_scores[(df_scores$Q=="Q2" | df_scores$Q=="Q3" | df_scores$Q=="Q4"),]

### Experiment 3 -----------------------------------
# Training -> Q1 + Q2
# Test -> Q3 + Q4

experiments_config$exp3$df_training  <- df_scores[(df_scores$Q=="Q1" | df_scores$Q=="Q2"),]
experiments_config$exp3$df_test <- df_scores[(df_scores$Q=="Q3" | df_scores$Q=="Q4"),]

## ================================================


## EXPERIMENT CALCULATION ========================================
# Applying the algorithm to experiment samples
experiment_count <- 1
algorithms_results <- lapply(experiments_config, function(exp){
  output <- list()
  output$MMDG <- MMDG_algorithm(jigsaws_data$graphs_trials[exp$df_training$ID])
  output$MMDG$aggr_graphs <- lapply(output$MMDG$aggr_graphs, function(g) {
    g$meta<-paste0(c("Exp",experiment_count), collapse="_")
    return(g)})
  output$MMDG$mined_models <- lapply(output$MMDG$mined_models, function(g) {
    g$meta<-paste0(c("Exp",experiment_count), collapse="_")
    return(g)})
  output$apply_models <- apply_models_algorithm(output$MMDG$mined_models, jigsaws_data$graphs_trials[exp$df_test$ID])
  experiment_count <<- experiment_count + 1
  return(output)
})

### Experiment 4 -----------------------------------
# Training -> ALL
experiments_config$exp4 <- list()
experiments_config$exp4$df_training <- data.frame(ID = names(brownie_data$graphs_trials))
exp <- experiments_config$exp4
algorithms_results$exp4$MMDG <- MMDG_algorithm(brownie_data$graphs_trials[exp$df_training$ID])
algorithms_results$exp4$MMDG$aggr_graphs <- lapply(algorithms_results$exp4$MMDG$aggr_graphs, function(g) {
  g$meta<-paste0(c("Exp",experiment_count), collapse="_")
  return(g)})
algorithms_results$exp4$MMDG$mined_models <- lapply(algorithms_results$exp4$MMDG$mined_models, function(g) {
  g$meta<-paste0(c("Exp",experiment_count), collapse="_")
  return(g)})

## RESULTS ========================================
### Fitness results #################################
#### Training set ###################################
print("=== Fitness results (Training sets) ===")
print("Experiment 1")
print(summary(algorithms_results$exp1$MMDG$df_cat_graphs))
print("Experiment 2")
print(summary(algorithms_results$exp2$MMDG$df_cat_graphs))
print("Experiment 3")
print(summary(algorithms_results$exp3$MMDG$df_cat_graphs))
print("Experiment 4")
print(summary(algorithms_results$exp4$MMDG$df_cat_graphs))

#### Test set ###################################
print("=== Fitness results (Test sets) ===")
print("Experiment 1")
print(summary(algorithms_results$exp1$apply_models$df_cat_graphs))
print("Experiment 2")
print(summary(algorithms_results$exp2$apply_models$df_cat_graphs))
print("Experiment 3")
print(summary(algorithms_results$exp3$apply_models$df_cat_graphs))

### Simplicity results #############################
# Create a dataframe with the vertices and edges differences between aggregated graph and thresholded graph
df_vertices_edges_summary = data.frame(matrix(vector(), 0, 6,
                       dimnames=list(c(), c("iter", "exp", "element", "type", "value", "name"))),
                stringsAsFactors=T)
for(exp in algorithms_results){
  for(aggr in exp$MMDG$aggr_graphs){
    df_vertices_edges_summary <- add_row(aggr, df_vertices_edges_summary)
  }
  for(thresh in exp$MMDG$mined_models){
    df_vertices_edges_summary <- add_row(thresh, df_vertices_edges_summary)
  }
}

#### Bar plots ###################################
# Bar plot with the simplicity results for the Brownie dataset.
print(ggplot(df_vertices_edges_summary[df_vertices_edges_summary$exp=="Exp_4",], aes(iter, value, fill = exp, alpha = type)) + #linetype = type)) + 
        geom_bar(stat = "identity", colour = "black", position = "dodge", size = 0.5) + 
        facet_grid(element~., scales = "free") + scale_fill_brewer(palette = "Set2") +
        theme_bw() + 
        scale_alpha_discrete(range = c(0.4, 0.8)) + 
        scale_y_continuous(breaks = seq(0,50,5), minor_breaks = seq(0,50,1)) + 
        theme(legend.position = "bottom"))

# Bar plot with the simplicity results for the JIGSAWS dataset.
print(ggplot(df_vertices_edges_summary[df_vertices_edges_summary$exp!="Exp_4",], aes(iter, value, fill = exp, alpha = type)) + #linetype = type)) + 
  geom_bar(stat = "identity", colour = "black", position = "dodge", size = 0.5) + 
  facet_grid(element~., scales = "free") + scale_fill_brewer(palette = "Set1") +
  theme_bw() + 
  scale_alpha_discrete(range = c(0.4, 0.8)) + 
  scale_y_continuous(breaks = c(0,5,10,15,20), minor_breaks = seq(0,25,1)) + 
  theme(legend.position = "bottom"))

