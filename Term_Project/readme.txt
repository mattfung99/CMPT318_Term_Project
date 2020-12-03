Multiple R files were used to generate our results. Here is a summary of what each R file is responsible for:

1. remove_na.R
   - Removes all NA values from TermProjectData.txt and generates a new dataset TermProjectDataCleaned without NA values

2. pca_plots_and_time_windows.R
   - Generates the PCA biplots and PCA summary tables for all 42 distinct time periods (Appendix A and Appendix B in the report)

3. corr_matrix_and_univariate_hmm.R
   - Verifies the chosen time window from pca_plots_and_time_windows.R and generates a correlation heatmap (Variable Selection in the report)
   - Creates, trains, and tests the univariate HMM and outputs its results (Univariate Hidden Markov Model and Anomaly Detection in the report)

4. multivariate_hmm.R
   - Creates, trains, and tests the multivariate HMM and outputs its results (Multivariate Hidden Markov Model and Anomaly Detection in the report)

5. multivariate_hmm_alt.R
   - Creates, trains, and tests the multivariate HMM but with a different time window
   - Outputs the results of the multivariate HMM with different time window 
   (Slide "Anomaly Detection With Multivariate HMM and Different Time Periods" in presentation)