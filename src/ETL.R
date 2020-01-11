# ...................................................
# Loading data ##################
## We load the activity sequences and vocabularies of each dataset (jigsaws and mmac/brownie)
data <- readRDS("data.rds")
jigsaws_data <- data$jigsaws[[TASK_NAME]]
brownie_data <- data$brownie

# ...................................................
# Data structures creation #############
## JIGSAWS DATASET
# Graphs (in igraph format) from the sequences.
jigsaws_data$graphs_trials <- lapply(jigsaws_data$sequences_trials,sequence_to_dependency_graph)

## BROWNIE DATASET
# Graphs (in igraph format) from the sequences.
brownie_data$graphs_trials <- lapply(brownie_data$sequences,sequence_to_dependency_graph)

# ...................................................


       



