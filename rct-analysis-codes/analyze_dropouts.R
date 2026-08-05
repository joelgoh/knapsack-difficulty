## analyze_dropouts.R
rm(list = ls())
library('data.table')
library('sandwich')
library('lmtest')
library('car')
library('ggplot2')

predict.poly <- function(cur.form, ct, newdata){
  ##if(missing(newdata)){ newdata <- x$model }
  m.mat <- model.matrix(terms(cur.form),data=newdata)
  m.coef <- coef(ct)
  fit <- as.vector(m.mat %*% m.coef)
  return(fit)
}

normalize <- function(x){
  return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}


POLY_DEGREE <- 4
SAVE_TO_FILE <- TRUE

## results directory
STR_INPUT_DIR <- '20240408_results'
STR_RESULTS_DIR <- '20250429_results'

ATTEMPT_CUTOFF <- 4
ACTIVE_DAY_CUTOFF_30D <- 0
ACTIVE_DAY_CUTOFF_TOTAL <- 0

## select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')
##STR_REGION_ALL <- c('A')

## get 6 variables corresponding to variable names
str_varnames <- c("(Intercept)", "wtval_corr_actual", "sahni_complexity",
                  "opt_numitems", "min_sahni", "max_sahni", "greedyval_0_rel_opt_gap")


## read rds and merge
rds_file_list <- sprintf('%s/df_player_notimeout_learning_reg%s.rds',STR_INPUT_DIR, STR_REGION_ALL)
df_player <- do.call(rbind, lapply(rds_file_list, readRDS))
df_player_dropouts <- df_player[,c("account_id", "max_player_attempt")]
if(SAVE_TO_FILE){
    cat(sprintf("Saving begin\n"))
    save(df_player_dropouts,
         file = sprintf("./%s/dropout_hist_allreg.RData", STR_RESULTS_DIR))
    cat(sprintf("Saving complete\n"))
}
rm(list = c("df_player", "df_player_dropouts"))


## # read rds and merge
## rds_file_list <- sprintf('%s/df_notimeout_learning_reg%s.rds',STR_INPUT_DIR, STR_REGION_ALL)
## df <- do.call(rbind, lapply(rds_file_list, readRDS))

## # Plot histogram 
## p <- ggplot(df_player, aes(x = max_player_attempt)) +
##         geom_histogram(fill = "skyblue", color = "black") +
##         labs(title = "Histogram of maximum minigame attempt",
##           x = "Maximum minigame attempt", 
##           y = "Counts")

for (STR_REGION in STR_REGION_ALL){
    str_infile <- sprintf('%s/df_player_notimeout_learning_reg%s.rds',STR_INPUT_DIR, STR_REGION)
    df_player <- readRDS(str_infile)

    m2.dropout <- lm(max_player_attempt ~ 
                         + log(actives_day_in_30d) + log(avg_online_time_in_30d) +
                         + log(total_active_days) + log(total_online_time)
                     + is_clan
                     + has.rank + is.max.rank + rank
                     + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                     + has.win.rate + win_rate
                     + has.mmr + log(mmr)
                     + has.match.num + log1p(match_num)
                     + has.gems.consume + log1p(gems_consume),
                     data = df_player)

    cur.vcov <- vcovCL(m2.dropout, ~account_id)
    s2.dropout  <- list(ct = coeftest(m2.dropout, vcov. = cur.vcov), vcov = cur.vcov)

    ## add attributes
    s <- summary(m2.dropout)
    s2.dropout$Rsq <-  s$r.squared
    s2.dropout$Fstat <-  s$fstatistic[1]
    s2.dropout$N <- s$nobs
    s2.dropout$Np <- s$nobs - s$df
    s2.dropout$VIF <- vif(m2.dropout)

    if(SAVE_TO_FILE){
    save(s2.dropout, 
         file = sprintf("./%s/dropout_results_reg_%s.RData", STR_RESULTS_DIR, STR_REGION))
    }

}

## if(SAVE_TO_FILE){
## save(s2.dropout, 
##      df_player_dropouts,
##      file = sprintf("./%s/dropout_results_allreg.RData", STR_RESULTS_DIR))
## }

## for (START_IND in 3:8){
##   ## START_IND <- 3  # for individual testing
##   END_IND   <- 25
##   TAU_WIN_LENGTH <- START_IND
  
##   ## window to learn over
##   tau_seq = seq(START_IND, 25, 1)
  
##   lst.s1.learn <- vector("list", length(tau_seq))
##   lst.s2.learn <- vector("list", length(tau_seq))
  
##   lst.s1.futurelearn <- vector("list", length(tau_seq))
##   lst.s2.futurelearn <- vector("list", length(tau_seq))
  
##   lst.s1.retain <- vector("list", length(tau_seq))
##   lst.s2.retain <- vector("list", length(tau_seq))
  
##   for (i in 1:length(tau_seq)){
##     ## Report
##     cat(sprintf("Working on tau = %d of %d ... \n",
##                 tau_seq[i], tau_seq[length(tau_seq)]))
    
##     ## tau
##     tau <- tau_seq[i]
##     tau_lb <- tau - TAU_WIN_LENGTH + 1
    
##     ## long to wide
##     df_tmp <- dcast(df[PlayerAttempt <= tau & PlayerAttempt >= tau_lb,],
##                     account_id ~ PlayerAttempt, value.var = "InstanceSolQual")
     
##     ## remove rows where tau > player attempts
##     df_tmp <- na.omit(df_tmp)
    
##     ## add player stats
##     df_tmp <- merge(df_tmp, df_player, by = "account_id", all = FALSE)
    
##     ## convert to numbers
##     str_colnames <- as.character(tau_lb:tau)
##     z <- lapply(df_tmp[,..str_colnames], function(x) as.numeric(as.character(x)))
##     df_tmp$learn_efficiency <- apply(as.data.table(z), MARGIN = 1, sum)
##     df_tmp[,str_colnames] <- NULL
    
##     ## merge max player attempts -- to get continuation flag
##     ##df_tmp <- merge(df_tmp, df_player[, .(account_id, max_player_attempt)], "account_id")
##     df_tmp <- df_tmp[account_id %in% df_player$account_id]
##     df_tmp <- df_tmp[, rem_attempts := max_player_attempt - tau][, is_cont := rem_attempts > 0]
    
##     ## merge player performance in next iteration
##     df_next_perf <- df[PlayerAttempt == tau + 1, .(account_id, sol_quality)]
##     df_tmp <- merge(df_tmp, df_next_perf, by = "account_id", all.x = TRUE)
    
##     ## merge player performance in future iterations
##     df_future_perf <- df[PlayerAttempt >= tau + 1, .(future_sol_quality = mean(sol_quality)), by = account_id]
##     df_tmp <- merge(df_tmp, df_future_perf, by = "account_id", all.x = TRUE)
    
##     ## regress
##     df_reg <- df_tmp
##     ## df_reg <- df_reg[, c("account_id", "max_player_attempt", "rem_attempts") := NULL]
    
##     m1.learn <- glm(sol_quality ~ learn_efficiency,
##                     data = df_reg[is_cont == TRUE,], family = "gaussian")
##     m2.learn <- glm(sol_quality ~ learn_efficiency
##                     + log(actives_day_in_30d) + log(avg_online_time_in_30d) +
##                       + log(total_active_days) + log(total_online_time)
##                     + is_clan
##                     + has.rank + is.max.rank + rank
##                     + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
##                     + has.win.rate + win_rate
##                     + has.mmr + log(mmr)
##                     + has.match.num + log1p(match_num)
##                     + has.gems.consume + log1p(gems_consume),
##                     data = df_reg[is_cont == TRUE,], family = "gaussian")
    
##     m1.futurelearn <- glm(future_sol_quality ~ learn_efficiency,
##                           data = df_reg[is_cont == TRUE,], family = "gaussian")
##     m2.futurelearn <- glm(future_sol_quality ~ learn_efficiency
##                           + log(actives_day_in_30d) + log(avg_online_time_in_30d) +
##                             + log(total_active_days) + log(total_online_time)
##                           + is_clan
##                           + has.rank + is.max.rank + rank
##                           + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
##                           + has.win.rate + win_rate
##                           + has.mmr + log(mmr)
##                           + has.match.num + log1p(match_num)
##                           + has.gems.consume + log1p(gems_consume),
##                           data = df_reg[is_cont == TRUE,], family = "gaussian")
    
##     m1.retain <- glm(is_cont ~ learn_efficiency,
##                      data = df_reg, family = "gaussian")
##     m2.retain <- glm(is_cont ~ learn_efficiency
##                      + log(actives_day_in_30d) + log(avg_online_time_in_30d) +
##                        + log(total_active_days) + log(total_online_time)
##                      + is_clan
##                      + has.rank + is.max.rank + rank
##                      + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
##                      + has.win.rate + win_rate
##                      + has.mmr + log(mmr)
##                      + has.match.num + log1p(match_num)
##                      + has.gems.consume + log1p(gems_consume),
##                      data = df_reg, family = "gaussian")
    
##     lst.s1.learn[[i]] <- coeftest(m1.learn)
##     lst.s2.learn[[i]] <- coeftest(m2.learn)
    
##     lst.s1.futurelearn[[i]] <- coeftest(m1.futurelearn)
##     lst.s2.futurelearn[[i]] <- coeftest(m2.futurelearn)
    
##     lst.s1.retain[[i]] <- coeftest(m1.retain)
##     lst.s2.retain[[i]] <- coeftest(m2.retain)
##   }
  
##   all.lst.s1.learn <- lst.s1.learn
##   all.lst.s2.learn <- lst.s2.learn
##   all.lst.s1.futurelearn <- lst.s1.futurelearn
##   all.lst.s2.futurelearn <- lst.s2.futurelearn
##   all.lst.s1.retain <- lst.s1.retain
##   all.lst.s2.retain <- lst.s2.retain
  
##   save(all.lst.s1.learn, all.lst.s2.learn,
##        all.lst.s1.futurelearn, all.lst.s2.futurelearn,
##        all.lst.s1.retain, all.lst.s2.retain,
##        tau_seq,
##        file = sprintf("./%s/full_learn_bydiff_allreg_s%d.RData", STR_RESULTS_DIR, START_IND))
## } 

