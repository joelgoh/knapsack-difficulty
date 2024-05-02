########## README_MAIN.txt ####
#
# Last updated: April 29, 2024
# By: JG
#
################################

1. 	Use bash script to combine raw webdata files into single files.
	Output file: raw_webdata_%REGION%.csv

2. 	Run process_playerchoice.R. This processes the sequence of playerchoices and counts number of undos, items taken etc.

	Input files : raw_webdata_%REGION%.csv
	Output files: webdata_%REGION%.csv

3. 	Run process_playerstats.R. This merges webgame data with player statistics.

	Input files : raw_webdata_%REGION%.csv AND raw data from /ingame_data/region_%REGION% subfolders
	Output files: playerstats_%REGION%.csv

4. 	Run run_all_sa.sh. This shell script runs all the sensitivity analyses. The base case model is part of it. 

	Input files : webdata_%REGION%.csv AND playerstats_%REGION%.csv AND knapsack_instances_with_complexity.csv
	
	4.1 Base case model with different model specifications 
		Call:         Rscript ./sa_corr_multispec.R
		Output files: sa_%REGION%_multispec.RData
		Called by:    prettyprint_main_results.R to generate main results table (using model s6 --> fully controlled)
		Called by: 	  prettyprint_sa_multispec_results.R to generate EC tables with different model specifications (models s2, s4, s5, s6 --> different levels of control)
   
	4.2 Sensitivity analysis of model with no timeouts
		Call: 	      Rscript ./sa_corr_no_timeout.R
		Output files: sa_%REGION%_notimeout_avg.RData
		Called by:    plot_sa_summary.R to generate summary plot in the main text
		Called by:    prettyprint_sa_altmodel_results.R to generate EC tables with detailed regression results
	   
	4.3 Sensitivity analysis of model with largest number of items when there are are non unique optimal solutions to knapsack instances
		Call: 	      Rscript ./sa_corr_maxitems.R
		Output files: sa_%REGION%_maxitems.RData
		Called by:    plot_sa_summary.R to generate summary plot in the main text
		Called by:    prettyprint_sa_altmodel_results.R to generate EC tables with detailed regression results
	   
	4.4 Sensitivity analysis of model with smallest number of items when there are are non unique optimal solutions to knapsack instances
		Call: 	      Rscript ./sa_corr_minitems.R
		Output files: sa_%REGION%_minitems.RData
		Called by:    plot_sa_summary.R to generate summary plot in the main text
		Called by:    prettyprint_sa_altmodel_results.R to generate EC tables with detailed regression results

	4.5 Sensitivity analysis of model with different cutoffs of initial modeling attempts. 
	    Call: 	      Rscript ./sa_corr_attempt_cutoff.R
	    Output files: sa_%REGION%_attempt%K%_avg.RData  	with %K% \in {4, 7, 10, 13}
	    Called by:    plot_sa_summary.R to generate summary plot in the main text
	    Called by:    prettyprint_sa_altmodel_results.R to generate EC tables with detailed regression results
	
	4.6 Sensitivity analysis of model with different cutoffs of recent player activity. 
	    Call: 	      Rscript ./sa_corr_recent_active.R
	    Output files: sa_%REGION%_recent_active_%K%_avg.RData  	with %K% \in {5, 10, 15}
	    Called by:    plot_sa_summary.R to generate summary plot in the main text
	    Called by:    prettyprint_sa_altmodel_results.R to generate EC tables with detailed regression results
	
	4.7 Sensitivity analysis of model with different cutoffs of total player activity. 
	    Call: 	      Rscript ./sa_corr_total_active.R
	    Output files: sa_%REGION%_total_active_%K%_avg.RData  	with %K% \in {30, 60, 90}
	    Called by:    plot_sa_summary.R to generate summary plot in the main text
	    Called by:    prettyprint_sa_altmodel_results.R to generate EC tables with detailed regression results


5. 	Run printing and plotting scripts
	5.1 prettyprint_main_results.R ------------ Generates main results table
	5.2 plot_sa_summary.R --------------------- Generates summary plot in the main text
	5.3 prettyprint_sa_multispec_results.R ---- Generates first set of tables in EC with different model specifications
	5.4 prettyprint_sa_altmodel_results.R ----- Generates second set of tables in EC with different data constructions
