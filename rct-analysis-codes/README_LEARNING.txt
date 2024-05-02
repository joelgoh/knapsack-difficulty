########## README_LEARNING.txt ######
#
# Last updated: April 29, 2024
# By: JG
#
#####################################
1.  Run analyze_notimeout_for_learning_poly_agg.R to aggregate regions and estimate polynomial regression on half the sample of players
	Output file: data_no_timeout_for_estimation_reg_%REGION%.rds   -- contains subset of data used to perform polynomial regression
	Output file: learning_modelest_allreg_notimeout.RData          -- contains output of polynomial regression

2. 	Run process_learnbehavior.R to generate data for learning analysis. Eliminates players used to perform polynomial regression
	Input file : data_no_timeout_for_estimation_reg_%REGION%.rds
	Output file: df_notimeout_learning_reg%REGION%.rds 
	Output file: df_player_notimeout_learning_reg%REGION%.rds

3. 	Run analyze_full_learn_poly_agg_fw.R  to conduct the full and aggregated learning analysis
	Input files : df_notimeout_learning_reg%REGION%.rds AND df_player_notimeout_learning_reg%REGION%.rds AND learning_modelest_allreg_notimeout.RData
	Output files: full_learn_bydiff_allreg_s%TAU%.RDATA      where %TAU% is the learning horizon 
	
4.  Run plot_learning_results_agg_fw.R to generate the figure in the main paper
	Input files : full_learn_bydiff_allreg_s%TAU%.RDATA      where %TAU% is the learning horizon 
	Output files: fig_learning_all_fw.png