## summarize_clustered_output.R
rm(list = ls())
library('data.table')

## constant
STR_COEFF_COLNAMES <- c("est", "se", "tval", "pval")

## output file
STR_OUT_PREFIX <- 'lm_summary'

## results dir
STR_RESULTS_DIR <- '20221031_results_fe_test'
#STR_RESULTS_DIR <- '20220411_results_clustered'
#STR_RESULTS_DIR <- '20220215_results_isopt_clustered'

## select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')


s1.all <- NULL
s2.all <- NULL

## iterate over all output
for (STR_REGION in STR_REGION_ALL) {
    ## read main file
    start.time <- Sys.time()
    cat(sprintf("Reading file from region %s\n", STR_REGION))
    load(file = sprintf('./%s/lmoutput_%s_fe_allactions.RData', STR_RESULTS_DIR, STR_REGION))
    ## load(file = sprintf('./%s/sa_%s_maxitems.RData', STR_RESULTS_DIR, STR_REGION))
    ## s1 <- s6
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

    ## ## Model 2
    ## colnames(s2) <- paste0(STR_COEFF_COLNAMES, "_", STR_REGION)
    ## s2[s2[, 4] < 1E-40, 4] <- 0 # for stability

    ## ## add regression stats
    ## s2.regstats <- matrix(c(attr(s2, "Rsq"), attr(s2, "Fstat"),
    ##                         attr(s2, "nobs") - attr(s2, "df"),
    ##                         attr(s2, "nobs"), rep(NA, 12)), 4, 4)
    ## rownames(s2.regstats) <- c("Rsq", "Fstat", "Np", "N")
    ## s2 <- rbind(s2, s2.regstats)    
    ## s2.all <- cbind(s2.all, s2)                  
}

## generate summary
write.table(data.frame("H"=rownames(s1.all), s1.all),
            file = sprintf("./%s/%s_1_fe_allactions.csv", STR_RESULTS_DIR, STR_OUT_PREFIX),
            row.names = FALSE, sep = ",")
## write.table(data.frame("H"=rownames(s2.all), s2.all),
##             file = sprintf("./%s/%s_2.csv", STR_RESULTS_DIR, STR_OUT_PREFIX),
##             row.names = FALSE, sep = ",")




## ## For s1
## for (i in 1:nrow(s1.all)){
##     ## point est
##     out.str <- paste0(out.str, sprintf("\n%s,", str.labels[i]))
##     for (STR_REGION in STR_REGION_ALL) {           
##         out.str <- paste0(out.str, sprintf("%0.4f,%s,",
##                                            s1.all[i, sprintf("est_%s", STR_REGION)],
##                                            starstr(s1.all[i, sprintf("pval_%s", STR_REGION)])))
##     }
##     ## std. error
##     out.str <- paste0(out.str, sprintf("\n,"))
##     for (STR_REGION in STR_REGION_ALL) {        
##         out.str <- paste0(out.str, sprintf("\"=\"\"(%0.4f)\"\"\",,", s1.all[i, sprintf("se_%s", STR_REGION)]))
##     }
## }
## cat(out.str, file=STR_OUTFILE)




