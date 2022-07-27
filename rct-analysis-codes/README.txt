### README ####
#
# Last updated: July 20, 2022
# By: JG
#
###############

1. Use bash script to combine raw webdata files into single files.
   Output file: raw_webdata_%REGION%.csv

2. Run process_playerchoice.R. This processes the sequence of playerchoices and counts number of undos, items taken etc.

   Input files : raw_webdata_%REGION%.csv
   Output files: webdata_%REGION%.csv

3. Run process_playerstats.R. This merges webgame data with FF player statistics.

   Input files : raw_webdata_%REGION%.csv AND raw data from /ingame_data/region_%REGION% subfolders
   Output files: playerstats_%REGION%.csv


4. Run analyze_corr.R/sa_corr.R. This merges all the files and runs regression models for the main model / sensitivity analyses..

   Input files : webdata_%REGION%.csv AND playerstats_%REGION%.csv AND knapsack_instances_with_complexity.csv
   Output files: lmoutput_%REGION%.RData

5. Run summarize_clustered_output.R/summarize_clustered_sa.R. This produces a table of results in a neater format to be read by Excel.

   Input files: lmoutput_%REGION%.RData
   Output files: lm_summary_%d.RData

