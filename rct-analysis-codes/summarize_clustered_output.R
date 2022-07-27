## summarize_clustered_output.R
rm(list = ls())
library('data.table')

## constant
STR_COEFF_COLNAMES <- c("est", "se", "tval", "pval")

## output file
STR_OUT_PREFIX <- 'lm_summary'

## results dir
STR_RESULTS_DIR <- '20220411_results_clustered'

## select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')

s1.all <- NULL
s2.all <- NULL

## iterate over all output
for (STR_REGION in STR_REGION_ALL) {
    ## read main file
    start.time <- Sys.time()
    cat(sprintf("Reading file from region %s\n", STR_REGION))
    load(file = sprintf('./%s/lmoutput_%s.RData', STR_RESULTS_DIR, STR_REGION))
    cat(sprintf("Read complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))

    ## Model 1
    colnames(s1) <- paste0(STR_COEFF_COLNAMES, "_", STR_REGION)
    s1[s1[, 4] < 1E-40, 4] <- 0 # for stability

    ## add regression stats
    s1.regstats <- matrix(c(attr(s1, "Rsq"), attr(s1, "Fstat"),
                            attr(s1, "nobs") - attr(s1, "df"),
                            attr(s1, "nobs"), rep(NA, 12)), 4, 4)
    rownames(s1.regstats) <- c("Rsq", "Fstat", "Np", "N")
    s1 <- rbind(s1, s1.regstats)    
    s1.all <- cbind(s1.all, s1)               
}

## generate summary
write.table(data.frame("H"=rownames(s1.all), s1.all),
            file = sprintf("./%s/%s_1.csv", STR_RESULTS_DIR, STR_OUT_PREFIX),
            row.names = FALSE, sep = ",")



