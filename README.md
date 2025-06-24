# AdultTBAntibodyOmics
Detailed code, associated datasets and documentation for LASSO-SVM machine learning analysis of Luminex antibody data on South Africa Cohort and Point of Care diagnostic platform data on South Africa and Validation Cohorts
The Rscripts folder contains the codes we used for the analysis

The data is contained in the dataset folder

The output folder contains results used for plotting

AdultTBAbOmic codes:

Lasso_SVM_Luminex.R -- This script calculates the AUC (ATB vs LTBI) for number of replication in a (LASSO+SVM) cross-validation framework on Luminex data from South Africa Cohort.
AdultTB_Luminex.xlxs --This Excel sheet contain Antibody profiling data obtained via Luminex bead based assay on South Africa Cohort 
Lasso_SVM_POC.R -- This script calculates the AUC (ATB vs LTBI) for number of replication in a (LASSO+SVM) cross-validation framework on Point-of-Care Diagnostic data from South Africa Cohort and validation(testing) Cohort.
AdultTB_POC_Train.xlxs --This Excel sheet contain Antibody profiling data obtained via point-of-care diagnosatic platform on South Africa Cohort 
AdultTB_POC_Test.xlxs --This Excel sheet contain Antibody profiling data obtained via point-of-care diagnosatic platform on validation Cohort 
