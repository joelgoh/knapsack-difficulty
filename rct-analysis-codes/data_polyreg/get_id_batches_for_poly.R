# get_id_batches_for_poly.R
# --------------------------
#  split data frame into batches of unique IDs for processing

NUM_BATCHES <- c(2, 10, 5, 5, 100)
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')
names(NUM_BATCHES) <- STR_REGION_ALL
for(STR_REGION in STR_REGION_ALL){
  num_batches <- NUM_BATCHES[STR_REGION]
  str_infile <- sprintf('./unique_id_reg_%s.RData', STR_REGION)
  load(str_infile)
  dt.id = data.table(unique_ids, 
                  id_group = sample(num_batches, length(unique_ids), replace=TRUE))
  save(dt.id, file = sprintf('./unique_id_with_group_%s.RData', STR_REGION))
}

