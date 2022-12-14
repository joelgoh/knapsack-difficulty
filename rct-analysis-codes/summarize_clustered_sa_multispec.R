## summarize_clustered_sa_multispec.R
rm(list = ls())
library('data.table')

## constant
STR_COEFF_COLNAMES <- c("est", "se", "tval", "pval")

## change to get different files
STR_SA_POSTFIX <- "multispec_avg"

## output file
STR_OUT_PREFIX <- 'sa'

## results dir
STR_RESULTS_DIR <- '20221102_results_avg'

## select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')

NUM_MODELS <- 7

# preallocate 
lst.summary <- vector(mode = 'list', NUM_MODELS)


process.one.model <- function(s){
    colnames(s) <- paste0(STR_COEFF_COLNAMES, "_", STR_REGION)
    s[s[, 4] < 1E-40, 4] <- 0 # for stability

    ## add regression stats
    s.regstats <- matrix(c(attr(s, "Rsq"), attr(s, "Fstat"),
                           attr(s, "Np"), attr(s, "N"),
                           rep(NA, 12)), 4, 4)
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
    lst.results <- list(s1, s2, s3, s4, s5, s6, s7)

    ## Models
    for (i in 1:NUM_MODELS){
        lst.summary[[i]] <- cbind(lst.summary[[i]], process.one.model(lst.results[[i]]))
    }
}


for (i in 1:NUM_MODELS){
    s <- lst.summary[[i]]
    str.outfile <- sprintf("./%s/%s_%d_%s.csv", STR_RESULTS_DIR, STR_OUT_PREFIX, i, STR_SA_POSTFIX)
    write.table(data.frame("H"=rownames(s), s),
                file = str.outfile, 
                row.names = FALSE, sep = ",")
    cat(sprintf('%s written successfully.\n', str.outfile))
}


## generate summary
## write.table(data.frame("H"=rownames(s1.all), s1.all),
##             file = sprintf("./%s/%s_1.csv", STR_RESULTS_DIR, STR_OUT_PREFIX),
##             row.names = FALSE, sep = ",")
## write.table(data.frame("H"=rownames(s2.all), s2.all),
##             file = sprintf("./%s/%s_2.csv", STR_RESULTS_DIR, STR_OUT_PREFIX),
##             row.names = FALSE, sep = ",")
## write.table(data.frame("H"=rownames(s3.all), s3.all),
##             file = sprintf("./%s/%s_3.csv", STR_RESULTS_DIR, STR_OUT_PREFIX),
##             row.names = FALSE, sep = ",")
## write.table(data.frame("H"=rownames(s4.all), s4.all),
##             file = sprintf("./%s/%s_4.csv", STR_RESULTS_DIR, STR_OUT_PREFIX),
##             row.names = FALSE, sep = ",")
## write.table(data.frame("H"=rownames(s5.all), s5.all),
##             file = sprintf("./%s/%s_5.csv", STR_RESULTS_DIR, STR_OUT_PREFIX),
##             row.names = FALSE, sep = ",")
## write.table(data.frame("H"=rownames(s6.all), s6.all),
##             file = sprintf("./%s/%s_6.csv", STR_RESULTS_DIR, STR_OUT_PREFIX),
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




