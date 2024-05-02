########## README_POLYREG.txt ######
#
# Last updated: April 29, 2024
# By: JG
#
#####################################
1.  Generate vector of unique ids for each region. 
	Output file: unique_id_reg_%REGION%.RData

2. 	Run ./data_polyreg/get_id_batches_for_poly.R to generate random subsetting of account_ids into groups and match them into groups
	Input file : unique_id_reg_%REGION%.RData 
	Output file: unique_id_with_group_%REGION%.RData 

3. 	Run process_poly_groups.R. This breaks the data down into smaller files based on the groupings generated previously
	Input files : webdata_%REGION%.csv AND playerstats_%REGION%.csv AND knapsack_instances_with_complexity.csv 
				  AND unique_id_with_group_%REGION%.RData 
	Output files: ./data_polyreg/data_reg_%REGION%_%KKK%.rds  where %KKK% is a 3 digit index of subgroups, varying between regions 

4. 	Run sa_corr_poly_clust.R. This performs the actual polynomial regressions
	Input files : ./data_polyreg/data_reg_%REGION%_%KKK%.rds
	Output files: ./sa_%REGION%_poly%DEG%_lm_clust.RData   where %DEG% is the degree of the polynomial used

5.  Run plot_poly_results_single.R to generate the single-region figure (with confidence bands) depicted in the main text
	Output file: fig_reg_A_poly4_lm6_with_ci.png

6.  Run plot_poly_results_multi.R to generate the multi-region figure (with no confidence bands) depicted in the EC 
	Output file: fig_all_reg_poly4_lm6.png
	

NOTE: The only reason for doing the above is that the clustered SE estimates crashes the system memory for region E. The process above allows us to do the 50% subsampling of accounts when estimating the SE of the polynomial regression in that region, as mentioned in the EC. 