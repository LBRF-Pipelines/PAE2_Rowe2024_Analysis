# Experiment Two: Prism Adaptation & Motor Imagery Anaylsis
Analysis code for a study examining the role of explicit knowledge of errors on updating internal models during motor imagery.

## Dependencies

To install the R packages required for running this analysis pipeline, run the following command at an R prompt: 

```r
install.packages(
c("data.table", "dplyr", "tidytable", "ggplot2", "afex", "emmeans", "performance", "car"))
```

## Running the pipeline
First, to run the pipeline, you will need to place the contents of the ```Data folder``` from the project's OSF repository (found [here](https://osf.io/za23p/?view_only=3737eb74368a4f6ebdf2984364640b42)) in the pipeline's ```_Data folder```. 

Then, set the working directory in R to the ```PAE2_Rowe2024_Analysis folder``` and run the following source commands in sequence. 

```r
# Import all task and demographic data for the study
source("./_Scripts/0_import.R")

# Run two-way ANOVA
source("./_Scripts/1_twoway_ANOVA.R")

# Run movement time
source("./_Scripts/2_movement_time.R")

# Summarize demographic data
source("./_Scripts/3_demographic_analysis.R")
