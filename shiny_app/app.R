##########################################################
# COMMENT THIS BEFORE UPLOAD THE CODE TO THE SHINY SEVER
#-------------------------------------------------------

# Set the current file location as workspace directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries #####################
# .lib<- c("igraph", "visNetwork", "plyr", "dplyr", "shiny", "shinydashboard", "shinycssloaders", "DT", "ggplot2")
# 
# .inst <- .lib %in% installed.packages()
# if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
# lapply(.lib, require, character.only=TRUE)
# options(warn=-1)
##########################################################

library(igraph)
library(visNetwork)
library(shinydashboard)
library(DT)
library(plyr)
library(dplyr)
library(shiny)
library(shinycssloaders)
library(ggplot2)

# ...................................................


# Load the set of functions used during program
source("utils.R")

# Loading our algorithms
source("algorithms.R")

# Load the data resources from dataset
data <- readRDS("data.rds")

# Empty dataframe
datas <- data.frame()

# Vertex and edges difference between aggregated graph and thresholded graph
df_vertices_edges_summary = data.frame(matrix(vector(), 0, 6,
                                              dimnames=list(c(), c("iter", "exp", "element", "type", "value", "name"))),
                                       stringsAsFactors=T)

header <- dashboardHeader(title = "Multiple Workflow Miner")

sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput(inputId = 'si_task_type', label="Task", choices = names(data)),
    menuItem("Task information", tabName = "task_information", icon = icon("book")),
    menuItem("Trial visualisation", tabName = "trials_visualisation", icon = icon("eye")),
    menuItem("Learn workflows",  tabName = "learn_workflows", icon = icon("cogs"))
  )
)

#body <- dashboardBody()

body <- dashboardBody(class="main-content",
                      tags$head(includeCSS("www/style.css")),
                      #tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
                      tabItems(
                        tabItem(tabName = "task_information",
                                h2("Task Information"),
                                fluidRow(column(12,
                                                h3("Description"),
                                                wellPanel(textOutput("description")))),
                                fluidRow(column(12,
                                                fluidRow(column(5, align="center",
                                                                h3("Demonstration", align="left"), 
                                                                withSpinner(imageOutput("performance_example", height = 450))),
                                                         column(5, align="center",
                                                                h3("Possible Activities", align="left"),
                                                                withSpinner(DT::dataTableOutput("dt_required_activities")))
                                                         #, column(5, imageOutput("performance_graph_gif"))
                                                ))),
                                fluidRow(column(12,
                                                tags$div(class="alert-message alert-message-info",
                                                         tags$h4("JIGSAWS dataset citation"), 
                                                         tags$p("Yixin Gao, S. Swaroop Vedula, Carol E. Reiley, Narges Ahmidi, Balakrishnan Varadarajan, Henry C. Lin, Lingling Tao, Luca Zappella, Benjam??n B??jar, David D. Yuh, Chi Chiung Grace Chen, Ren?? Vidal, Sanjeev Khudanpur and Gregory D. Hager, The JHU-ISI Gesture and Skill Assessment Working Set (JIGSAWS): A Surgical Activity Dataset for Human Motion Modeling, In Modeling and Monitoring of Computer Assisted Interventions (M2CAI) ??? MICCAI Workshop, 2014. 
                                                                " )
                                                         )))),
                        
                        tabItem(tabName = "trials_visualisation",
                                h2("Trial visualisation"),
                                fluidRow(column(4,
                                                h3("Trials"),
                                                DT::dataTableOutput("dt_task_trials")),
                                         column(7, offset = 1,
                                                h3("Trial as activity sequence"),
                                                verbatimTextOutput("vt_sequence", placeholder = TRUE),
                                                h3("Trial as dependency graph"),
                                                fluidRow(wellPanel(visNetworkOutput("network")))))),
                        
                        tabItem(tabName = "learn_workflows",
                                h2("Extract Workflows"),
                                fluidRow(column(12, h3("Selection"))),
                                fluidRow(column(3,
                                                h4("Trials"),
                                                DT::dataTableOutput("dt_task_trials_2")),
                                         column(3, offset = 1,
                                                h4("Selected trials"),
                                                DT::dataTableOutput("dt_task_trials_training"),
                                                actionButton(inputId = "btn_learning", class="btn btn-primary", label = "Extract workflows")),
                                         column(3, offset = 1,
                                                h4("Mined Workflows"),
                                                DT::dataTableOutput("dt_workflows"))),
                                fluidRow(column(12, h3("Workflow analysis"))),
                                fluidRow(column(10, offset = 1,  
                                                h4("Mined workflows"),
                                                DT::dataTableOutput("dt_workflows_2"))),
                                fluidRow(column(5, offset = 1, 
                                                h4("Workflow dependency graph"),
                                                fluidRow(wellPanel(visNetworkOutput("workflow_network")))),
                                         column(5,
                                                h4("Aggregated Graph (G+)"),
                                                fluidRow(wellPanel(visNetworkOutput("aggr_network"))))),
                                fluidRow(column(12, h3("Evolution of filtering throughout the learning phase"))),
                                fluidRow(column(5, offset = 1,
                                                plotOutput("plot_vertices")),
                                         column(5,
                                                plotOutput("plot_edges"))),
                                fluidRow(column(12, h3("Coverage results of the workflows"))),
                                fluidRow(column(4, offset = 1, 
                                                h4("Learning phase"),
                                                plotOutput("plot_coverage_training")),
                                         column(4, offset = 1, 
                                                h4("Test phase"),
                                                plotOutput("plot_coverage_test"))),
                                fluidRow(column(4, offset = 1,
                                                DT::dataTableOutput("dt_coverage_training")),
                                         column(4, offset = 1,
                                                DT::dataTableOutput("dt_coverage_test"))))))

ui <- dashboardPage(
  header,
  sidebar,
  body, skin = "yellow")

server <- function(input, output, session) {
  # Task selection from header
  task_data <- reactive({data[[input$si_task_type]]})
  
  output$selected_task <- renderText({
    switch(input$si_task_type,
           suturing={
             return("Suturing")
           }, knot_tying={
             return("Knot Tying")
           }, needle_passing={
             return("Needle Passing")
           })
    return("")
  })
  
  # Task description
  output$description <- renderText({
    return(task_data()$info_resources$task_description)
  })
  
  output$dt_required_activities <- DT::renderDataTable(task_data()$df_required_gestures,
                                                       server = FALSE, selection = "none")
  
  output$performance_example <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.gif')
    
    # Return a list containing the filename
    #list(src = task_data()$info_resources$gif,
    list(src = paste0(c("img", task_data()$info_resources$gif), collapse = "/"),
         #list(src = data$needle_passing$info_resource$gif, 
         contentType = 'image/gif',
         width = 400,
         height = 441,
         alt = "This is alternate text",
         class="rcorners1")
  }, deleteFile = FALSE)
  
  # Plan Visualisation Part
  trial_id <- reactive({
    task_data()$df_scores[input$dt_task_trials_rows_selected,]$ID})
  
  trial_graph <- reactive({
    task_data()$igraph_trials[[trial_id()]]})
  
  trial_sequence <- reactive({
    task_data()$original_trials[[trial_id()]]$gestures})
  
  output$dt_task_trials <-  DT::renderDataTable(task_data()$df_scores,
                                                server = FALSE, selection = list(mode = 'single', selected = c(1)))
  
  output$vt_sequence <- renderPrint({
    if(length(input$dt_task_trials_rows_selected)){
      cat(as.character(trial_sequence()), sep = " -> ")}})
  
  output$network <- renderVisNetwork({
    if(length(input$dt_task_trials_rows_selected)){
      graph <- trial_graph()
      no_iso_graph <- delete_isolates(graph)
      df_gestures <- task_data()$df_gestures_meaning
      no_iso_df_gestures <- df_gestures[df_gestures$id %in% V(no_iso_graph)$name,]
      vis_graph <- plot_visnetwork(no_iso_graph, name = trial_id())
      vis_graph$x$nodes["title"] <- paste0("<p>", no_iso_df_gestures$id ,"<br>", no_iso_df_gestures$description,"</p>")
      vis_graph}})
  
  # Workflow Extraction Part
  #task_data_2 <- reactive({data[[selected_task()]]})
  workflow_id <- reactive({
    task_data()$df_scores[input$dt_task_trials_rows_selected,]$ID})
  
  PM_alg_result <- list()
  L_alg_result <- list()
  
  output$dt_task_trials_2 <- DT::renderDataTable(task_data()$df_scores,
                                                 server = FALSE, selection = list(mode = 'multiple', selected = c(1)))
  
  selected_trials <- reactive({task_data()$df_scores[input$dt_task_trials_2_rows_selected,]$ID})
  
  output$dt_task_trials_training <- DT::renderDataTable(task_data()$df_scores[input$dt_task_trials_2_rows_selected,],
                                                        server = FALSE,
                                                        selection = "none")
  
  observeEvent(input$btn_learning, {
    if(length(selected_trials())>0)
    {
      PM_alg_result <<- PM_2_algorithm(selected_trials(), LETTERS, task_data()$igraph_trials, task_data()$am_trials)
      
      L_alg_result <<- L_algorithm(PM_alg_result$mined_plans,
                                   task_data()$df_scores$ID[!(task_data()$df_scores$ID %in% selected_trials())],
                                   task_data()$igraph_trials)
      #df_workflows <- data.frame("workflow" = names(PM_alg_result$mined_plans))
      df_workflows <- do.call("rbind", lapply(PM_alg_result$mined_plans, function(x) data.frame(t(unlist(get.graph.attribute(x))),stringsAsFactors = F)))
      
      
      df_v_e <- df_vertices_edges_summary # Make a copy 
      
      for(aggr in PM_alg_result$aggr_plans){
        df_v_e <- add_row(aggr, df_v_e)
      }
      for(thresh in PM_alg_result$mined_plans){
        df_v_e <- add_row(thresh, df_v_e)
      }
      
      workflows_ID <- unique(PM_alg_result$df_cat_trials$category)
      
      comparison_plots <- create_comparison_plots(df_v_e, workflows_ID)
      
      output$dt_workflows <- DT::renderDataTable(df_workflows[,c(1,2)], server = FALSE, selection = "none")
      output$dt_workflows_2 <- DT::renderDataTable(df_workflows[,1:(ncol(df_workflows)-2)], server = FALSE, selection = list(mode = 'single', selected = c(1)))
      output$dt_coverage_training <- DT::renderDataTable(PM_alg_result$df_cat_trials, server = FALSE, selection = "none")
      output$dt_coverage_test <- DT::renderDataTable(L_alg_result$df_cat_trials, server = FALSE, selection = "none")
      
      df_coverage_training <- PM_alg_result$df_cat_trials
      
      output$plot_coverage_training <- renderPlot({ggplot(data=df_coverage_training, aes(x = category, fill = category)) + 
          geom_bar(stat="count") + 
          theme_bw() +
          scale_fill_brewer(palette = "Set1") +
          xlab("Workflow") +
          ylab("Number of covered trials") + 
          labs(fill = "Workflow") +
          scale_y_continuous(breaks = seq(0,40,5), minor_breaks = seq(0,40,1), limits = c(0, (max(table(df_coverage_training$category)) + 5))) + #(max(table(df_coverage_training$category)) + 5))) +
          geom_text( stat='count', aes(label=..count..), fontface="bold", size = 8, position=position_dodge(width=0.9), vjust=-0.75)
        })
      # Prepare the coverage dataframe in the test phase    
      # We get the workflow labels that were obtained
      possible_categories <- c(as.character(unique(PM_alg_result$df_cat_trials$category)), "-")
      
      # Count the workflows coverage using the labels from training (in case of one workflow was not covered any example)
      df_count <- plyr::count(match(L_alg_result$df_cat_trials$category, possible_categories, nomatch = F))
      df_count$x <-  possible_categories[df_count$x] # Add the workflow label to dataframe column
      
      # Create the dataframe with 
      df_coverage_test <- merge(data.frame( "x" = possible_categories), df_count, by.x = "x", by.y = "x", all.x = T)
      
      # Reordering the coverage results putting the workflows firstly to the character of uncategorised
      df_coverage_test$x <- ordered(df_coverage_test$x, possible_categories)
      df_coverage_test <- df_coverage_test[with(df_coverage_test, order(x, freq)),]
      
      # Ensure that we regard the workflows without coverage.
      df_coverage_test[is.na(df_coverage_test)] <- 0

      output$plot_coverage_test <- renderPlot({ggplot(data=df_coverage_test, aes(x = x, y = freq, fill = x)) + 
          geom_bar(stat="identity") + 
          theme_bw() +
          scale_fill_brewer(palette = "Set1") +
          xlab("Workflow") +
          ylab("Number of covered trials") + 
          labs(fill = "Workflow") +
          scale_y_continuous(breaks = seq(0,40,5), minor_breaks = seq(0,40,1), limits = c(0, (max(df_coverage_test$freq) + 5))) +
          geom_text(aes(label = freq),fontface="bold", size = 8, position=position_dodge(width=0.9), vjust=-0.75)
        })
      
      output$plot_vertices <- renderPlot({comparison_plots$vertices_plot})
      output$plot_edges <- renderPlot({comparison_plots$edges_plot})
    }
  })
  
  output$workflow_network <- renderVisNetwork({
    if(length(input$dt_workflows_2_rows_selected)){
      graph <- PM_alg_result$mined_plans[[input$dt_workflows_2_rows_selected]]
      no_iso_graph <- delete_isolates(graph)
      df_gestures <- task_data()$df_gestures_meaning
      no_iso_df_gestures <- df_gestures[df_gestures$id %in% V(no_iso_graph)$name,]
      vis_graph <- plot_visnetwork(no_iso_graph, name = get.graph.attribute(no_iso_graph)$name)
      vis_graph$x$nodes["title"] <- paste0("<p>",no_iso_df_gestures$id ,"<br>", no_iso_df_gestures$description,"</p>")
      vis_graph$x$edges$label <- NULL # Hide the edge weight
      vis_graph$x$edges$color = c("green") # Highlight the arrows with green colours
      vis_graph$x$nodes$color.border = c("green") # Highlight the nodes with green colours
      vis_graph}})
  
  output$aggr_network <- renderVisNetwork({
    if(length(input$dt_workflows_2_rows_selected)){
      graph <- PM_alg_result$aggr_plans[[input$dt_workflows_2_rows_selected]]
      no_iso_graph <- delete_isolates(graph)
      df_gestures <- task_data()$df_gestures_meaning
      no_iso_df_gestures <- df_gestures[df_gestures$id %in% V(no_iso_graph)$name,]
      
      vis_graph <- plot_visnetwork(no_iso_graph, name = get.graph.attribute(no_iso_graph)$name)
      
      # Highligthing the differences between the aggregated graph and the mined graph.
      # Get the mined graph
      vis_pattern <- plot_visnetwork(PM_alg_result$mined_plans[[input$dt_workflows_2_rows_selected]], "")
      
      # Highligth the shared nodes with green colour while the others are highlighted in red.
      vis_graph$x$nodes$color.border = c("red") 
      vis_graph$x$nodes[vis_graph$x$nodes$id %in% vis_pattern$x$nodes$id,]$color.border <- c("green") 

      # The same highlighting but with edges.
      vis_graph$x$edges$color = c("red") 
      vis_graph$x$edges$tmp <- paste(vis_graph$x$edges[,1],vis_graph$x$edges[,2],sep = "_")
      vis_pattern$x$edges$tmp <- paste(vis_pattern$x$edges[,1],vis_pattern$x$edges[,2],sep = "_")
      vis_graph$x$edges[vis_graph$x$edges$tmp %in% vis_pattern$x$edges$tmp,]$color  <- c("green") 

      vis_graph$x$nodes["title"] <- paste0("<p>",no_iso_df_gestures$id ,"<br>", no_iso_df_gestures$description,"</p>")
      vis_graph}})
  
  # Set this to "force" instead of TRUE for testing locally (without Shiny Server)
  session$allowReconnect(TRUE)
}

shinyApp(ui, server)
