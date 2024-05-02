## sa_corr_poly_clust.R
rm(list = ls())
library('data.table')
library('sandwich')
library('lmtest')
library('fixest')
library('car')
library('biglm')
library('magrittr')
library('purrr')
library('biglmmapper')

STR_DATA_DIR <- './data_polyreg'
MAX_CNT <- 100

## select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')

## filter
ATTEMPT_CUTOFF = 4
ACTIVE_DAY_CUTOFF_30D = 0
ACTIVE_DAY_CUTOFF_TOTAL = 0


str.col.main <- c('wtval_corr_actual', 'sahni_complexity', 'opt_numitems', 
                  'min_sahni', 'max_sahni', 'greedyval_0_rel_opt_gap')


## output settings
options("width" = 200)

## read knapsack problem data
str_knapsack <- './knapsack_instances_with_complexity.csv';
df_ks <- fread(str_knapsack);

normalize <- function(x){
    return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}


## Degree of polynomial in regression
for (POLY_DEGREE in 1:1){

    ## output of regression
    for (STR_REGION in STR_REGION_ALL) {

        ## Read processed file and some quick calculations
        start.time <- Sys.time()
        cat(sprintf("\n\nProcessing region %s, ... \n", STR_REGION))

        ## generate file list
        rds_file_list <- list.files(path = STR_DATA_DIR,
                                    pattern = sprintf('data_reg_%s_\\d+.rds', STR_REGION))
        rds_file_list <- sprintf('%s/%s', STR_DATA_DIR, rds_file_list)

        ## get the full data, except for the case where too large (region E)
        if(STR_REGION == 'E' && POLY_DEGREE > 1){
            rds_file_list <- rds_file_list[1:floor(length(rds_file_list)/2)];
        }                    
        df <- do.call(rbind, lapply(rds_file_list, readRDS))
        lst.poly.coeffs <- lapply(df[, ..str.col.main],
                                  function(x) attr(poly(x, POLY_DEGREE, raw = F), "coefs"))
        
                         
        ## NOTE: f1 to f3 are unused
        f1 <- formula(sol_quality ~ PlayerAttempt)

        f2 <- formula(sol_quality ~ wtval_corr_actual + sahni_complexity)

        f3 <- formula(sol_quality ~ wtval_corr_actual + sahni_complexity + PlayerAttempt)

        ## ONLY f4 - f6 are used for the polyreg
        f4 <- formula(sol_quality ~ poly(wtval_corr_actual, POLY_DEGREE, raw = F)
                      + poly(sahni_complexity, POLY_DEGREE, raw = F)
                      + poly(opt_numitems, POLY_DEGREE, raw = F) 
                      + poly(min_sahni, POLY_DEGREE, raw = F) 
                      + poly(max_sahni, POLY_DEGREE, raw = F) 
                      + poly(greedyval_0_rel_opt_gap, POLY_DEGREE, raw = F)) 

        f5 <- formula(sol_quality ~ poly(wtval_corr_actual, POLY_DEGREE, raw = F)
                      + poly(sahni_complexity, POLY_DEGREE, raw = F)
                      + poly(opt_numitems, POLY_DEGREE, raw = F) 
                      + poly(min_sahni, POLY_DEGREE, raw = F) 
                      + poly(max_sahni, POLY_DEGREE, raw = F) 
                      + poly(greedyval_0_rel_opt_gap, POLY_DEGREE, raw = F)
                      + PlayerAttempt)

        f6 <- formula(sol_quality ~ poly(wtval_corr_actual, POLY_DEGREE, raw = F)
                      + poly(sahni_complexity, POLY_DEGREE, raw = F)
                      + poly(opt_numitems, POLY_DEGREE, raw = F) 
                      + poly(min_sahni, POLY_DEGREE, raw = F) 
                      + poly(max_sahni, POLY_DEGREE, raw = F)
                      + poly(greedyval_0_rel_opt_gap, POLY_DEGREE, raw = F)
                      + PlayerAttempt
                      + log(actives_day_in_30d) + log(avg_online_time_in_30d)
                      + log(total_active_days) + log(total_online_time)
                      + is_clan
                      + has.rank + is.max.rank + rank
                      + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                      + has.win.rate + win_rate
                      + has.mmr + log(mmr)
                      + has.match.num + log1p(match_num)
                      + has.gems.consume + log1p(gems_consume))        
        
        lm1 <- NULL 
        lm2 <- NULL
        lm3 <- NULL

        cat(sprintf("\nStart LM s4 ....\n"))
        lm4 <- NULL
        ## do lm and cluster test
        m <- lm(f4, df)
        cur.vcov <- vcovCL(m, ~account_id)
        lm4_orig <- list(ct = coeftest(m, vcov. = cur.vcov), vcov = cur.vcov)
        if(POLY_DEGREE == 1){
            ## add attributes
            s <- summary(m)
            lm4_orig$Rsq <-  s$r.squared
            lm4_orig$Fstat <-  s$fstatistic[1]
            lm4_orig$N <- s$nobs
            lm4_orig$Np <- s$nobs - s$df
            lm4_orig$VIF <- vif(m)
        }
       
   
        cat(sprintf("\nStart LM s5 ....\n")) 
        lm5 <- NULL
        
        cat(sprintf("\nStart LM s6 ....\n"))
        lm6 <- NULL

        ## do lm and cluster test
        m <- lm(f6, df)
        cur.vcov <- vcovCL(m, ~account_id)
        lm6_orig <- list(ct = coeftest(m, vcov. = cur.vcov), vcov = cur.vcov)
        if(POLY_DEGREE == 1){
            ## add attributes
            s <- summary(m)
            lm6_orig$Rsq <-  s$r.squared
            lm6_orig$Fstat <-  s$fstatistic[1]
            lm6_orig$N <- s$nobs
            lm6_orig$Np <- s$nobs - s$df
            lm6_orig$VIF <- vif(m)
        }

               
        ## save to file
        cat(sprintf("Saving begin ... "))

        save(lm1, lm2, lm3, lm4, lm5, lm6, lm4_orig, lm6_orig,
             lst.poly.coeffs,
             POLY_DEGREE, ATTEMPT_CUTOFF, ACTIVE_DAY_CUTOFF_30D, ACTIVE_DAY_CUTOFF_TOTAL,
             file = sprintf("./sa_%s_poly%d_lm_clust.RData", STR_REGION, POLY_DEGREE))

        cat(sprintf("Saving complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
    }
}






