************************************************************************************
************************************************************************************
* REPLICATION INSTRUCTIONS for:							   *
* "Monetary Policy and Exchange Rate Dynamics  in a Behavioral Open Economy Model" *
* by Marcin Kolasa, Sahil Ravgotra, Pawel Zabczyk				   *
* published in Journal of International Economics				   *
************************************************************************************
************************************************************************************


****************
* REQUIREMENTS *
****************
To run the codes, you need to have MATLAB and Dynare installed. Please make sure that Dynare files are included in your MATLAB path. 
The codes were run using Dynare version 4.6.4 and MATLAB 2019a on Windows operating system. More recent versions of Dynare and MATLAB
should work as well, but we have not checked all the versions available as of today. 



***************
* REPLICATION *
***************

1. ESTIMATION

To estimate the Baseline, Rational and Inflated Prior versions of the model, run the following m-files:
Run_JIE_EST_Baseline.m
Run_JIE_EST_Rational.m
Run_JIE_EST_Inflated.m
This will produce the results reported in the tables reporting the estimation output.


2. SIMULATIONS

To produce the model simulation results, run:
Result_Replicator.m
This will generate all the remaining tables and figures presented in the text. 


3. DATA FOR ESTIMATION

Raw data and their transformations can be found in:
DATA_KRZ.xlsx
The final data used in estimation (sample 1972-2007) is saved in:
DATA_KRZ_7207.mat

