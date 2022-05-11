# ========================================================================================
# Use the 50 selected participants to do PCA, as an example data.
# Version 1
# Created on 05.10.2022 by Rie Sadohara
# ========================================================================================

# Use the only the 50 datapoints.
totals_QCed_sampled <- read.table("results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_input_PCs_50.txt", sep="\t", header=T)

# Define the input data to be used.
input_data <- totals_QCed_sampled
colnames(input_data)
dim(input_data)

# The columns specified as start.col, end.col, and all columns in between will be selected.
SubsetColumns(data=input_data, start.col="GRMS", end.col="NoOfItems")

# The output is a df called "subsetted".

# Pick up only the columns with non-zero variance, in order to run PCA, cluster analysis etc.
# The removed columns will be shown if any.
KeepNonZeroVarColumns(data = subsetted)
# The output is a df called "subsetted_non0var".

# Check the columns (variables) remained.
colnames(subsetted_non0var)  
dim(subsetted_non0var)  

# ---------------------------------------------------------------------------------------------------------------
# Collapse variables by correlation: take only one variables if they are highly correlated.
cbc_res <- CollapseByCorrelation(x = subsetted_non0var,
                                 min.cor = 0.75, 
                                 select.rep.fcn = 'mean', verbose = T)

# Filter out highly correlated variables from the original dataset.  
selected_variables <- subsetted_non0var[, cbc_res$reps]

# ***"selected_variables" is the dataframe to be used for PCA, cluster analyses etc.***

# Check to see the name of the original and filtered variables. 
# Among the variables in the same group, the one with the highest variance is kept 
#  (according to the explanation above.)
# filtered
head(selected_variables, 1)     
dim(selected_variables)     

# original
head(subsetted_non0var, 1)
dim(subsetted_non0var)

# ---------------------------------------------------------------------------------------------------------------
# Save the variables after removing correlated variables
write.table(selected_variables, 
            "results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_rv.txt", 
            sep="\t", row.names=F, quote=F)

# ---------------------------------------------------------------------------------------------------------------
# Save the correlation matrix for record in the results folder.
# cc is the correlation matrix produced when variables are collapsed by correlation. 
SaveCorrMatrix(x=cc,
               out.fn="results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_corr_mat.txt")

# ---------------------------------------------------------------------------------------------------------------
# Your input data should be a data frame with variables with non-zero variance. 
pca_input <- selected_variables

# Ensure your input file has the correct number of rows and columns.
dim(pca_input)

# Perform PCA with the subset data, scaled.
scaled_pca <- prcomp(x=pca_input, scale = TRUE)   

# Create a scree plot.
screep <- LineScreePlot(pca.data = pca_input, pca.result = scaled_pca)
screep
ggsave("results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_screep.pdf", screep, device="pdf", width=5, height=5, units="in")

# Create a biplot.
# A biplot with the individuals as black dots and variables labelled.
biplotdots <- BiplotDots(pca.result = scaled_pca, pca.data = pca_input, alpha = 0.5)
biplotdots
ggsave("results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_biplotdots.pdf", biplotdots, device="pdf", width=5, height=5, units="in")

# A biplot with the individuals labeled.
biplotlabeled <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label = T)
biplotlabeled
ggsave("results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_biplotlabeled.pdf", biplotlabeled, device="pdf", width=5, height=5, units="in")

# A biplot with the individuals labeled without the variables' arrows.
biplotlabeledwoarrows <- BiplotLabeledwoArrows(pca.result=scaled_pca, pca.data=pca_input, 
                                               individuals.label=T)
biplotlabeledwoarrows #+coord_cartesian(xlim=c(-0.1, 0.1), ylim=c(0.05, 0.1))   

ggsave("results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_biplotlabeledwoarrows.pdf", biplotlabeledwoarrows, device="pdf", width=5, height=5, units="in")

# Plot the directions of the variables.
directions <- BiplotLabeled(pca.result=scaled_pca, pca.data=pca_input, individuals.label=F)
directions
ggsave("results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_directions.pdf", directions, device="pdf", width=5, height=5, units="in")

# Plot the contribution of the variables to a given PC.
LoadingsPlot(pca.result=scaled_pca,  whichPC="PC1", 
             positive.color="green2", negative.color="grey70", sort.variables = T)
loadings_plot
ggsave("results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_loadings_PC1.pdf", loadings_plot, device="pdf", width=8, height=4.8, units="in")

# ---------------------------------------------------------------------------------------------------------------  
# Save the variance explained by each PC as a .txt file. 
# Change the file name as necessary.  
SaveVarExplained(pca.data = pca_input, pca.result = scaled_pca, 
                 out.fn = "results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_PC_var_explained.txt")

# ---------------------------------------------------------------------------------------------------------------  
# Calculate loadings of each PC to the variables and 
# save it as a txt file in the results folder.
# Change the file name as necessary.  
SaveLoadings(pca.result=scaled_pca, 
             out.fn="results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_PC_loadings.txt")

# ---------------------------------------------------------------------------------------------------------------  
# Save the PC values with the input which has the metadata and food codes, food names.  
# Input is your food input file before any prep for clustering, from which you derived the input for the PCA.
SaveInputAndPCs(input = "eg_data/NHANES/NHANES1516_total_d12_mean_QC_2_500sampled.txt",
                pca.results = scaled_pca, 
                out.fn = "results/PCA_results/temporary/eg 50/total_d12_mean_QC500_2_50_input_PCs.txt")


