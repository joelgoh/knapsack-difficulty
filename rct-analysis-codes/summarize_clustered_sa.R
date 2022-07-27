## summarize_clustered_sa.R
rm(list = ls())
library('data.table')

## constant
STR_COEFF_COLNAMES <- c("est", "se", "tval", "pval")

## change to get different files
STR_SA_POSTFIX <- "includetimeouts"

## output file
STR_OUT_PREFIX <- 'sa'

## results dir
STR_RESULTS_DIR <- '20220412_results_sa_clustered'

## select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')

## Number of distinct models
NUM_MODELS <- 6

# preallocate 
lst.summary <- vector(mode = 'list', NUM_MODELS)


process.one.model <- function(s){
    colnames(s) <- paste0(STR_COEFF_COLNAMES, "_", STR_REGION)
    s[s[, 4] < 1E-40, 4] <- 0 # for stability

    ## add regression stats
    s.regstats <- matrix(c(attr(s, "Rsq"), attr(s, "Fstat"),
                           attr(s, "nobs") - attr(s, "df"),
                           attr(s, "nobs"), rep(NA, 12)), 4, 4)
    rownames(s.regstats) <- c("Rsq", "Fstat", "Np", "N")
    s <- rbind(s, s.regstats)    
    return(s)
}


## iterate over all output
for (STR_REGION in STR_REGION_ALL) {
    ## read main file
    start.time <- Sys.time()
    cat(sprintf("Reading file from region %s\n", STR_REGION))
    load(file = sprintf('./%s/%s_%s_%s.RData', STR_RESULTS_DIR, STR_OUT_PREFIX, STR_REGION, STR_SA_POSTFIX))
    cat(sprintf("Read complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))

    ## testing
    lst.results <- list(s1, s2, s3, s4, s5, s6)

    ## Models
    for (i in 1:NUM_MODELS){
        lst.summary[[i]] <- cbind(lst.summary[[i]], process.one.model(lst.results[[i]]))
    }
}

## Write output
for (i in 1:NUM_MODELS){
    s <- lst.summary[[i]]
    str.outfile <- sprintf("./%s/%s_%d_%s.csv", STR_RESULTS_DIR, STR_OUT_PREFIX, i, STR_SA_POSTFIX)
    write.table(data.frame("H"=rownames(s), s),
                file = str.outfile, 
                row.names = FALSE, sep = ",")
    cat(sprintf('%s written successfully.\n', str.outfile))
}



