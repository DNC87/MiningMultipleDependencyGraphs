# Learning Alternative Ways of Performing a Task

## Project
We present a novel tool for learning models (in dependency graph form) that is able to determine different ‘styles’ that cluster the multiple workflows for performing task. This is a new feature that other commercial systems lack,whereas our system is also open source and web-based (a Shiny application). The tool extracts the model from very few sequences of activities and can be used in many areas, from activity recognition to the clustering of human traces in ML/AI benchmarks. We illustrate its use on a surgical domain but it can be extended to other domains (e.g., cooking tasks).

## Software resources
The software resources we have programmed for this project are two: the code programmed to implement our mining method along with some experiments for testing it, and an interactive web application to be able to interact with our method for different use cases. 

+ `src/` Source code with the algorithms developed and the calculation of the experiments involved in testing them.
	+ ``main.R`` Entry point for the execution of our code related to the experiments. Basically, it contains the loading of the necessary R packages and the execution of the call pipeline to the other scripts involved in our development.
	+ ``conf.R`` Declaration of some configuration variables that will be used throughout the execution.
	+ ``utils.R`` Useful functions that make it easier to use certain recurring patterns in the program.
	+ ``ETL.R`` Takes the input data (i.e., sequences of activity labels) and converts them to a dependency graph formalism.
	+ ``algorithms.R`` Contains our MMDG mining algorithm implementation, as well as all the satellite functions involved in our mining process. 
	+ ``experiments.R`` Setup and execution of the experiments to test our mining algorithm. 
	+ ``data.rds`` Input data of our program.
 + `shiny_app/` Interactive web application developed with Shiny to demonstrate our mining method in different practical scenarios. All the code is contained in the `app.R` file.


## Prerequisites
The code has been developed under version 3.5.1 of R. It is recommended to execute the code through RStudio (version 1.1.456 or higher), through the execution of the __main.R__ script. A priori, it is not considered necessary to install any package manually since the code is in charge of automatically installing those that are required. 

Note that we use the following expression to automatically set the path where our files are located:

```R
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
``` 

This expression makes use of a proprietary RStudio package (i.e., ``rstudioapi``). In case you have problems loading or using it, please define your working directory with the following expression using the local path in your computer where the source files of our program are located:

```R
setwd('/path/to/files/')
```

## How to get the results of the experiments?
After the execution of the ``main.R`` file, the variable the results of the experiments will be collected in the ``algorithms_results`` variable. 

For example, for experiment 1 (i.e., models learned from the trials provided by expert surgeons for a suture exercise), we can consult the different results obtained from our algorithm during the training phase as follows:


```R
# Accessing the mined models #
# The models obtained are stored within the MMDG$mined_models structure. In order to plot the dependency graph of the first model obtained by our algorithm, we can use the following expression:

plot_visnetwork(algorithms_results$exp1$MMDG$mined_models$thresh_6_A)
```
![Model plot](/figures/model.png)

```R
# Accessing to aggregated graphs #
# The aggregate graphs generated in each iteration will be found within MMDG$aggr_graphs structure. In the same manner, we can plot one of these graphs by using this code:

plot_visnetwork(algorithms_results$exp1$MMDG$aggr_graphs$aggr_6_A)
```
![Aggr plot](/figures/aggr_graph.png)

```R
# Accessing to fitness results #
# The fitness results of the models obtained during the training phase can be consulted as follows:

algorithms_results$exp1$MMDG$df_cat_graphs
```

| Trial ID | Model |
| -------- | ----- |
| E001 |	A |
| E002 |	A |
| E003 |	A |
| E005 |	A |
| D003 |	B |
| D005 |	B |
| D001 |	C |
| D002 |	C | 
| D004 |	C |
| E004 |	C |

```R
# Fitness results on unseen examples #
# To access the fitness results on new trials you can do the following:

algorithms_results$exp1$apply_models$df_cat_graphs

```  
| Trial ID | Model |
| -------- | ----- |
| C002 | 	- |
| F001 |	- |
| F002 |	- |
| F003 |	- |
| F004 |	- |
| G001 |	- |
| G002 |	- |
| G003 |	- |
| G004 |	- |
| G005 |	- |
| H005 |	- |
| B003 |	A |
| B004 |	A |
| B001 |	C |
| B002 |	C |
| B005 |	C |
| C001 |	C |
| C003 |	C |
| C004 |	C |
| C005 |	C |
| F005 |	C |
| H001 |	C |
| H003 |	C |
| H004 |	C |
| I001 |	C |
| I002 |	C |
| I003 |	C |
| I004 |	C |
| I005 |	C |

## Shiny application
The application can be found at [shinyapp link](https://safe-tools.dsic.upv.es/shiny/SurgicalWorkflowMining). The functionalities available in this application are these: Task Description(1) shows the explanatory information of the task. Trial Visualisation(2) provides the view/detail of each recorded trial. Learn Workflow includes the selection of examples to extract models (3), view/detail of learned workflows (4), and statistics about coverage (5)and removal of edges and vertices (6) during the learning process.

![Shiny application](/figures/shiny_app.png)

## License
This project is licensed under the MIT License - see the [`License.md`](LICENSE.md) file for details

## Acknowledges
This work has been partially supported by the EU (FEDER) and the Spanish MINECO under grants TIN2014-61716-EXP (SUPERVASION) and RTI2018-094403-B-C32, and by Generalitat Valenciana under grant PROMETEO/2019/098. David Nieves is also supported by the Spanish MINECO under FPI grant (BES-2016-078863).
