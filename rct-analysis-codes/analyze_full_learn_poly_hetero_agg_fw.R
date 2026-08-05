## analayze_full_learn_poly_hetero_agg_fw.R
## April 29, 2025
## - Revised notation to be consistent with paper (switched tau for T): Breaking change
## - Removed all. prefix in front of saved objects: Breaking change
## - Added heterogeneous analysis to study question from R4
rm(list = ls())
library('data.table')
library('sandwich')
library('lmtest')

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

## results directory
##STR_RESULTS_DIR <- '20240131_results'
##STR_RESULTS_DIR <- '20240408_results'
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

## read rds and merge
rds_file_list <- sprintf('%s/df_notimeout_learning_reg%s.rds',STR_INPUT_DIR, STR_REGION_ALL)
df <- do.call(rbind, lapply(rds_file_list, readRDS))

TAU_RANGE <- 3:8
for (TAU_WIN_LENGTH in TAU_RANGE){
    cat(sprintf('\n\nWorking on tau = %d\n\n', TAU_WIN_LENGTH))
    
    ## START_IND <- 3  # for individual testing
    END_IND   <- 25
    
    ## window to learn over
    T_seq = seq(TAU_WIN_LENGTH, END_IND, 1)
    
    lst.s1.learn <- vector("list", length(T_seq))
    lst.s2.learn <- vector("list", length(T_seq))
    lst.s2.learn.active30d <- vector("list", length(T_seq))
    lst.s2.learn.online30d <- vector("list", length(T_seq))
    lst.s2.learn.activetot <- vector("list", length(T_seq))
    lst.s2.learn.onlinetot <- vector("list", length(T_seq))
    lst.s2.learn.isclan <- vector("list", length(T_seq))
    lst.s2.learn.rank <- vector("list", length(T_seq))
    lst.s2.learn.avgkills <- vector("list", length(T_seq))
    lst.s2.learn.avgdmg <- vector("list", length(T_seq))
    lst.s2.learn.winrate <- vector("list", length(T_seq))
    lst.s2.learn.mmr <- vector("list", length(T_seq))
    lst.s2.learn.matchnum <- vector("list", length(T_seq))
    lst.s2.learn.gemsconsume <- vector("list", length(T_seq))
    
    
    lst.s1.futurelearn <- vector("list", length(T_seq))
    lst.s2.futurelearn <- vector("list", length(T_seq))
    lst.s2.futurelearn.active30d <- vector("list", length(T_seq))
    lst.s2.futurelearn.online30d <- vector("list", length(T_seq))
    lst.s2.futurelearn.activetot <- vector("list", length(T_seq))
    lst.s2.futurelearn.onlinetot <- vector("list", length(T_seq))
    lst.s2.futurelearn.isclan <- vector("list", length(T_seq))
    lst.s2.futurelearn.rank <- vector("list", length(T_seq))
    lst.s2.futurelearn.avgkills <- vector("list", length(T_seq))
    lst.s2.futurelearn.avgdmg <- vector("list", length(T_seq))
    lst.s2.futurelearn.winrate <- vector("list", length(T_seq))
    lst.s2.futurelearn.mmr <- vector("list", length(T_seq))
    lst.s2.futurelearn.matchnum <- vector("list", length(T_seq))
    lst.s2.futurelearn.gemsconsume <- vector("list", length(T_seq))

    
    lst.s1.retain <- vector("list", length(T_seq))
    lst.s2.retain <- vector("list", length(T_seq))
    lst.s2.retain.active30d <- vector("list", length(T_seq))
    lst.s2.retain.online30d <- vector("list", length(T_seq))
    lst.s2.retain.activetot <- vector("list", length(T_seq))
    lst.s2.retain.onlinetot <- vector("list", length(T_seq))
    lst.s2.retain.isclan <- vector("list", length(T_seq))
    lst.s2.retain.rank <- vector("list", length(T_seq))
    lst.s2.retain.avgkills <- vector("list", length(T_seq))
    lst.s2.retain.avgdmg <- vector("list", length(T_seq))
    lst.s2.retain.winrate <- vector("list", length(T_seq))
    lst.s2.retain.mmr <- vector("list", length(T_seq))
    lst.s2.retain.matchnum <- vector("list", length(T_seq))
    lst.s2.retain.gemsconsume <- vector("list", length(T_seq))

    
    for (i in 1:length(T_seq)){
        ## Report
        cat(sprintf("Working on T = %d of %d ... \n",
                    T_seq[i], T_seq[length(T_seq)]))
        
        ## T
        cur_T <- T_seq[i]
        cur_T_lb <- cur_T - TAU_WIN_LENGTH + 1
        
        ## long to wide
        df_tmp <- dcast(df[PlayerAttempt <= cur_T & PlayerAttempt >= cur_T_lb,],
                        account_id ~ PlayerAttempt, value.var = "InstanceSolQual")
        
        ## remove rows where tau > player attempts
        df_tmp <- na.omit(df_tmp)
        
        ## add player stats
        df_tmp <- merge(df_tmp, df_player, by = "account_id", all = FALSE)
        
        ## convert to numbers
        str_colnames <- as.character(cur_T_lb:cur_T)
        z <- lapply(df_tmp[,..str_colnames], function(x) as.numeric(as.character(x)))
        df_tmp$learn_efficiency <- apply(as.data.table(z), MARGIN = 1, sum)
        df_tmp[,str_colnames] <- NULL
        
        ## merge max player attempts -- to get continuation flag
        ##df_tmp <- merge(df_tmp, df_player[, .(account_id, max_player_attempt)], "account_id")
        df_tmp <- df_tmp[account_id %in% df_player$account_id]
        df_tmp <- df_tmp[, rem_attempts := max_player_attempt - cur_T][, is_cont := rem_attempts > 0]
        
        ## merge player performance in next iteration
        df_next_perf <- df[PlayerAttempt == cur_T + 1, .(account_id, sol_quality)]
        df_tmp <- merge(df_tmp, df_next_perf, by = "account_id", all.x = TRUE)
        
        ## merge player performance in future iterations
        df_future_perf <- df[PlayerAttempt >= cur_T + 1, .(future_sol_quality = mean(sol_quality)), by = account_id]
        df_tmp <- merge(df_tmp, df_future_perf, by = "account_id", all.x = TRUE)
        
        ## regress
        df_reg <- df_tmp
        ## df_reg <- df_reg[, c("account_id", "max_player_attempt", "rem_attempts") := NULL]
        
        m1.learn <- glm(sol_quality ~ learn_efficiency,
                        data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn <- glm(sol_quality ~ learn_efficiency
                        + log(actives_day_in_30d)
                        + log(avg_online_time_in_30d)
                        + log(total_active_days)
                        + log(total_online_time)
                        + is_clan
                        + has.rank + is.max.rank + rank
                        + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                        + has.win.rate + win_rate
                        + has.mmr + log(mmr)
                        + has.match.num + log1p(match_num)
                        + has.gems.consume + log1p(gems_consume),
                        data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.active30d <- glm(sol_quality ~ learn_efficiency
                                   + learn_efficiency : log(actives_day_in_30d)
                                   + log(actives_day_in_30d)
                                   + log(avg_online_time_in_30d)
                                   + log(total_active_days)
                                   + log(total_online_time)
                                   + is_clan
                                   + has.rank + is.max.rank + rank
                                   + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                   + has.win.rate + win_rate
                                   + has.mmr + log(mmr)
                                   + has.match.num + log1p(match_num)
                                   + has.gems.consume + log1p(gems_consume),
                                   data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.online30d <- glm(sol_quality ~ learn_efficiency
                                  + learn_efficiency : log(avg_online_time_in_30d)
                                  + log(actives_day_in_30d)
                                  + log(avg_online_time_in_30d)
                                  + log(total_active_days)
                                  + log(total_online_time)
                                  + is_clan
                                  + has.rank + is.max.rank + rank
                                  + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                  + has.win.rate + win_rate
                                  + has.mmr + log(mmr)
                                  + has.match.num + log1p(match_num)
                                  + has.gems.consume + log1p(gems_consume),
                                  data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.activetot <- glm(sol_quality ~ learn_efficiency
                                  + learn_efficiency : log(total_active_days)
                                  + log(actives_day_in_30d)
                                  + log(avg_online_time_in_30d)
                                  + log(total_active_days)
                                  + log(total_online_time)
                                  + is_clan
                                  + has.rank + is.max.rank + rank
                                  + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                  + has.win.rate + win_rate
                                  + has.mmr + log(mmr)
                                  + has.match.num + log1p(match_num)
                                  + has.gems.consume + log1p(gems_consume),
                                  data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.onlinetot <- glm(sol_quality ~ learn_efficiency
                                  + learn_efficiency : log(total_online_time)
                                  + log(actives_day_in_30d)
                                  + log(avg_online_time_in_30d)
                                  + log(total_active_days)
                                  + log(total_online_time)
                                  + is_clan
                                  + has.rank + is.max.rank + rank
                                  + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                  + has.win.rate + win_rate
                                  + has.mmr + log(mmr)
                                  + has.match.num + log1p(match_num)
                                  + has.gems.consume + log1p(gems_consume),
                                  data = df_reg[is_cont == TRUE,], family = "gaussian")
        
        m2.learn.isclan <- glm(sol_quality ~ learn_efficiency
                               + learn_efficiency : is_clan
                               + log(actives_day_in_30d)
                               + log(avg_online_time_in_30d)
                               + log(total_active_days)
                               + log(total_online_time)
                               + is_clan
                               + has.rank + is.max.rank + rank
                               + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                               + has.win.rate + win_rate
                               + has.mmr + log(mmr)
                               + has.match.num + log1p(match_num)
                               + has.gems.consume + log1p(gems_consume),
                               data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.rank <- glm(sol_quality ~ learn_efficiency
                             + learn_efficiency : rank
                             + log(actives_day_in_30d)
                             + log(avg_online_time_in_30d)
                             + log(total_active_days)
                             + log(total_online_time)
                             + is_clan
                             + has.rank + is.max.rank + rank
                             + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                             + has.win.rate + win_rate
                             + has.mmr + log(mmr)
                             + has.match.num + log1p(match_num)
                             + has.gems.consume + log1p(gems_consume),
                             data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.avgkills <- glm(sol_quality ~ learn_efficiency
                                 + learn_efficiency : log1p(avg_kills)
                                 + log(actives_day_in_30d)
                                 + log(avg_online_time_in_30d)
                                 + log(total_active_days)
                                 + log(total_online_time)
                                 + is_clan
                                 + has.rank + is.max.rank + rank
                                 + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                 + has.win.rate + win_rate
                                 + has.mmr + log(mmr)
                                 + has.match.num + log1p(match_num)
                                 + has.gems.consume + log1p(gems_consume),
                                 data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.avgdmg <- glm(sol_quality ~ learn_efficiency
                               + learn_efficiency : log1p(avg_damage)
                               + log(actives_day_in_30d)
                               + log(avg_online_time_in_30d)
                               + log(total_active_days)
                               + log(total_online_time)
                               + is_clan
                               + has.rank + is.max.rank + rank
                               + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                               + has.win.rate + win_rate
                               + has.mmr + log(mmr)
                               + has.match.num + log1p(match_num)
                               + has.gems.consume + log1p(gems_consume),
                               data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.winrate <- glm(sol_quality ~ learn_efficiency
                                + learn_efficiency : win_rate
                                + log(actives_day_in_30d)
                                + log(avg_online_time_in_30d)
                                + log(total_active_days)
                                + log(total_online_time)
                                + is_clan
                                + has.rank + is.max.rank + rank
                                + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                + has.win.rate + win_rate
                                + has.mmr + log(mmr)
                                + has.match.num + log1p(match_num)
                                + has.gems.consume + log1p(gems_consume),
                                data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.mmr <- glm(sol_quality ~ learn_efficiency
                            + learn_efficiency : log(mmr)
                            + log(actives_day_in_30d)
                            + log(avg_online_time_in_30d)
                            + log(total_active_days)
                            + log(total_online_time)
                            + is_clan
                            + has.rank + is.max.rank + rank
                            + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                            + has.win.rate + win_rate
                            + has.mmr + log(mmr)
                            + has.match.num + log1p(match_num)
                            + has.gems.consume + log1p(gems_consume),
                            data = df_reg[is_cont == TRUE,], family = "gaussian")
        
        m2.learn.matchnum <- glm(sol_quality ~ learn_efficiency
                                 + learn_efficiency : log1p(match_num)
                                 + log(actives_day_in_30d)
                                 + log(avg_online_time_in_30d)
                                 + log(total_active_days)
                                 + log(total_online_time)
                                 + is_clan
                                 + has.rank + is.max.rank + rank
                                 + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                 + has.win.rate + win_rate
                                 + has.mmr + log(mmr)
                                 + has.match.num + log1p(match_num)
                                 + has.gems.consume + log1p(gems_consume),
                                 data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.learn.gemsconsume <- glm(sol_quality ~ learn_efficiency
                                    + learn_efficiency : log1p(gems_consume)
                                    + log(actives_day_in_30d)
                                    + log(avg_online_time_in_30d)
                                    + log(total_active_days)
                                    + log(total_online_time)
                                    + is_clan
                                    + has.rank + is.max.rank + rank
                                    + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                    + has.win.rate + win_rate
                                    + has.mmr + log(mmr)
                                    + has.match.num + log1p(match_num)
                                    + has.gems.consume + log1p(gems_consume),
                                    data = df_reg[is_cont == TRUE,], family = "gaussian")

        ## Report
        cat(sprintf('Start Coeftest for Short-term learning\n'))
        
        lst.s1.learn[[i]] <- coeftest(m1.learn)
        lst.s2.learn[[i]] <- coeftest(m2.learn)
        lst.s2.learn.active30d[[i]] <- coeftest(m2.learn.active30d)
        lst.s2.learn.online30d[[i]] <- coeftest(m2.learn.online30d)
        lst.s2.learn.activetot[[i]] <- coeftest(m2.learn.activetot)
        lst.s2.learn.onlinetot[[i]] <- coeftest(m2.learn.onlinetot)
        lst.s2.learn.isclan[[i]] <- coeftest(m2.learn.isclan)
        lst.s2.learn.rank[[i]] <- coeftest(m2.learn.rank)
        lst.s2.learn.avgkills[[i]] <- coeftest(m2.learn.avgkills)
        lst.s2.learn.avgdmg[[i]] <- coeftest(m2.learn.avgdmg)
        lst.s2.learn.winrate[[i]] <- coeftest(m2.learn.winrate)
        lst.s2.learn.mmr[[i]] <-  coeftest(m2.learn.mmr)
        lst.s2.learn.matchnum[[i]] <- coeftest(m2.learn.matchnum)
        lst.s2.learn.gemsconsume[[i]] <- coeftest(m2.learn.gemsconsume)

        rm(m1.learn, m2.learn,
           m2.learn.active30d,
           m2.learn.online30d,
           m2.learn.activetot,
           m2.learn.onlinetot,
           m2.learn.isclan,
           m2.learn.rank,
           m2.learn.avgkills,
           m2.learn.avgdmg,
           m2.learn.winrate,
           m2.learn.mmr,
           m2.learn.matchnum,
           m2.learn.gemsconsume)

        ## Report
        cat(sprintf('Short-Term Learning Complete\n'))

        

        
        m1.futurelearn <- glm(future_sol_quality ~ learn_efficiency,
                              data = df_reg[is_cont == TRUE,], family = "gaussian")
        m2.futurelearn <- glm(future_sol_quality ~ learn_efficiency
                              + log(actives_day_in_30d)
                              + log(avg_online_time_in_30d) 
                              + log(total_active_days)
                              + log(total_online_time)
                              + is_clan
                              + has.rank + is.max.rank + rank
                              + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                              + has.win.rate + win_rate
                              + has.mmr + log(mmr)
                              + has.match.num + log1p(match_num)
                              + has.gems.consume + log1p(gems_consume),
                              data = df_reg[is_cont == TRUE,], family = "gaussian")


        m2.futurelearn.active30d <- glm(future_sol_quality ~ learn_efficiency
                                         + learn_efficiency : log(actives_day_in_30d)
                                         + log(actives_day_in_30d)
                                         + log(avg_online_time_in_30d)
                                         + log(total_active_days)
                                         + log(total_online_time)
                                         + is_clan
                                         + has.rank + is.max.rank + rank
                                         + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                         + has.win.rate + win_rate
                                         + has.mmr + log(mmr)
                                         + has.match.num + log1p(match_num)
                                         + has.gems.consume + log1p(gems_consume),
                                         data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.futurelearn.online30d <- glm(future_sol_quality ~ learn_efficiency
                                        + learn_efficiency : log(avg_online_time_in_30d)
                                        + log(actives_day_in_30d)
                                        + log(avg_online_time_in_30d)
                                        + log(total_active_days)
                                        + log(total_online_time)
                                        + is_clan
                                        + has.rank + is.max.rank + rank
                                        + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                        + has.win.rate + win_rate
                                        + has.mmr + log(mmr)
                                        + has.match.num + log1p(match_num)
                                        + has.gems.consume + log1p(gems_consume),
                                        data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.futurelearn.activetot <- glm(future_sol_quality ~ learn_efficiency
                                        + learn_efficiency : log(total_active_days)
                                        + log(actives_day_in_30d)
                                        + log(avg_online_time_in_30d)
                                        + log(total_active_days)
                                        + log(total_online_time)
                                        + is_clan
                                        + has.rank + is.max.rank + rank
                                        + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                        + has.win.rate + win_rate
                                        + has.mmr + log(mmr)
                                        + has.match.num + log1p(match_num)
                                        + has.gems.consume + log1p(gems_consume),
                                        data = df_reg[is_cont == TRUE,], family = "gaussian")
        
        m2.futurelearn.onlinetot <- glm(future_sol_quality ~ learn_efficiency
                                        + learn_efficiency : log(total_online_time)
                                        + log(actives_day_in_30d)
                                        + log(avg_online_time_in_30d)
                                        + log(total_active_days)
                                        + log(total_online_time)
                                        + is_clan
                                        + has.rank + is.max.rank + rank
                                        + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                        + has.win.rate + win_rate
                                        + has.mmr + log(mmr)
                                        + has.match.num + log1p(match_num)
                                        + has.gems.consume + log1p(gems_consume),
                                        data = df_reg[is_cont == TRUE,], family = "gaussian")
        
        m2.futurelearn.isclan <- glm(future_sol_quality ~ learn_efficiency
                                     + learn_efficiency : is_clan
                                     + log(actives_day_in_30d)
                                     + log(avg_online_time_in_30d)
                                     + log(total_active_days)
                                     + log(total_online_time)
                                     + is_clan
                                     + has.rank + is.max.rank + rank
                                     + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                     + has.win.rate + win_rate
                                     + has.mmr + log(mmr)
                                     + has.match.num + log1p(match_num)
                                     + has.gems.consume + log1p(gems_consume),
                                     data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.futurelearn.rank <- glm(future_sol_quality ~ learn_efficiency
                                   + learn_efficiency : rank
                                   + log(actives_day_in_30d)
                                   + log(avg_online_time_in_30d)
                                   + log(total_active_days)
                                   + log(total_online_time)
                                   + is_clan
                                   + has.rank + is.max.rank + rank
                                   + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                   + has.win.rate + win_rate
                                   + has.mmr + log(mmr)
                                   + has.match.num + log1p(match_num)
                                   + has.gems.consume + log1p(gems_consume),
                                   data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.futurelearn.avgkills <- glm(future_sol_quality ~ learn_efficiency
                                       + learn_efficiency : log1p(avg_kills)
                                       + log(actives_day_in_30d)
                                       + log(avg_online_time_in_30d)
                                       + log(total_active_days)
                                       + log(total_online_time)
                                       + is_clan
                                       + has.rank + is.max.rank + rank
                                       + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                       + has.win.rate + win_rate
                                       + has.mmr + log(mmr)
                                       + has.match.num + log1p(match_num)
                                       + has.gems.consume + log1p(gems_consume),
                                       data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.futurelearn.avgdmg <- glm(future_sol_quality ~ learn_efficiency
                                     + learn_efficiency : log1p(avg_damage)
                                     + log(actives_day_in_30d)
                                     + log(avg_online_time_in_30d)
                                     + log(total_active_days)
                                     + log(total_online_time)
                                     + is_clan
                                     + has.rank + is.max.rank + rank
                                     + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                     + has.win.rate + win_rate
                                     + has.mmr + log(mmr)
                                     + has.match.num + log1p(match_num)
                                     + has.gems.consume + log1p(gems_consume),
                                     data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.futurelearn.winrate <- glm(future_sol_quality ~ learn_efficiency
                                      + learn_efficiency : win_rate
                                      + log(actives_day_in_30d)
                                      + log(avg_online_time_in_30d)
                                      + log(total_active_days)
                                      + log(total_online_time)
                                      + is_clan
                                      + has.rank + is.max.rank + rank
                                      + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                      + has.win.rate + win_rate
                                      + has.mmr + log(mmr)
                                      + has.match.num + log1p(match_num)
                                      + has.gems.consume + log1p(gems_consume),
                                      data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.futurelearn.mmr <- glm(future_sol_quality ~ learn_efficiency
                                  + learn_efficiency : log(mmr)
                                  + log(actives_day_in_30d)
                                  + log(avg_online_time_in_30d)
                                  + log(total_active_days)
                                  + log(total_online_time)
                                  + is_clan
                                  + has.rank + is.max.rank + rank
                                  + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                  + has.win.rate + win_rate
                                  + has.mmr + log(mmr)
                                  + has.match.num + log1p(match_num)
                                  + has.gems.consume + log1p(gems_consume),
                                  data = df_reg[is_cont == TRUE,], family = "gaussian")
        
        m2.futurelearn.matchnum <- glm(future_sol_quality ~ learn_efficiency
                                       + learn_efficiency : log1p(match_num)
                                       + log(actives_day_in_30d)
                                       + log(avg_online_time_in_30d)
                                       + log(total_active_days)
                                       + log(total_online_time)
                                       + is_clan
                                       + has.rank + is.max.rank + rank
                                       + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                       + has.win.rate + win_rate
                                       + has.mmr + log(mmr)
                                       + has.match.num + log1p(match_num)
                                       + has.gems.consume + log1p(gems_consume),
                                       data = df_reg[is_cont == TRUE,], family = "gaussian")

        m2.futurelearn.gemsconsume <- glm(future_sol_quality ~ learn_efficiency
                                          + learn_efficiency : log1p(gems_consume)
                                          + log(actives_day_in_30d)
                                          + log(avg_online_time_in_30d)
                                          + log(total_active_days)
                                          + log(total_online_time)
                                          + is_clan
                                          + has.rank + is.max.rank + rank
                                          + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                          + has.win.rate + win_rate
                                          + has.mmr + log(mmr)
                                          + has.match.num + log1p(match_num)
                                          + has.gems.consume + log1p(gems_consume),
                                          data = df_reg[is_cont == TRUE,], family = "gaussian")

        ## Report
        cat(sprintf('Start Coeftest for Long-term learning\n'))

        lst.s1.futurelearn[[i]] <- coeftest(m1.futurelearn)
        lst.s2.futurelearn[[i]] <- coeftest(m2.futurelearn)
        lst.s2.futurelearn.active30d[[i]] <- coeftest(m2.futurelearn.active30d)
        lst.s2.futurelearn.online30d[[i]] <- coeftest(m2.futurelearn.online30d)
        lst.s2.futurelearn.activetot[[i]] <- coeftest(m2.futurelearn.activetot)
        lst.s2.futurelearn.onlinetot[[i]] <- coeftest(m2.futurelearn.onlinetot)
        lst.s2.futurelearn.isclan[[i]] <- coeftest(m2.futurelearn.isclan)
        lst.s2.futurelearn.rank[[i]] <- coeftest(m2.futurelearn.rank)
        lst.s2.futurelearn.avgkills[[i]] <- coeftest(m2.futurelearn.avgkills)
        lst.s2.futurelearn.avgdmg[[i]] <- coeftest(m2.futurelearn.avgdmg)
        lst.s2.futurelearn.winrate[[i]] <- coeftest(m2.futurelearn.winrate)
        lst.s2.futurelearn.mmr[[i]] <-  coeftest(m2.futurelearn.mmr)
        lst.s2.futurelearn.matchnum[[i]] <- coeftest(m2.futurelearn.matchnum)
        lst.s2.futurelearn.gemsconsume[[i]] <- coeftest(m2.futurelearn.gemsconsume)

        rm(m1.futurelearn, m2.futurelearn,
           m2.futurelearn.active30d,
           m2.futurelearn.online30d,
           m2.futurelearn.activetot,
           m2.futurelearn.onlinetot,
           m2.futurelearn.isclan,
           m2.futurelearn.rank,
           m2.futurelearn.avgkills,
           m2.futurelearn.avgdmg,
           m2.futurelearn.winrate,
           m2.futurelearn.mmr,
           m2.futurelearn.matchnum,
           m2.futurelearn.gemsconsume)



        ## Report
        cat(sprintf('Long-Term Learning Complete\n'))

        
        m1.retain <- glm(is_cont ~ learn_efficiency,
                         data = df_reg, family = "gaussian")

        m2.retain <- glm(is_cont ~ learn_efficiency
                         + log(actives_day_in_30d)
                         + log(avg_online_time_in_30d) 
                         + log(total_active_days)
                         + log(total_online_time)
                         + is_clan
                         + has.rank + is.max.rank + rank
                         + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                         + has.win.rate + win_rate
                         + has.mmr + log(mmr)
                         + has.match.num + log1p(match_num)
                         + has.gems.consume + log1p(gems_consume),
                         data = df_reg, family = "gaussian")

        m2.retain.active30d <- glm(is_cont ~ learn_efficiency
                                    + learn_efficiency : log(actives_day_in_30d)
                                    + log(actives_day_in_30d)
                                    + log(avg_online_time_in_30d)
                                    + log(total_active_days)
                                    + log(total_online_time)
                                    + is_clan
                                    + has.rank + is.max.rank + rank
                                    + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                    + has.win.rate + win_rate
                                    + has.mmr + log(mmr)
                                    + has.match.num + log1p(match_num)
                                    + has.gems.consume + log1p(gems_consume),
                                    data = df_reg, family = "gaussian")

        m2.retain.online30d <- glm(is_cont ~ learn_efficiency
                                   + learn_efficiency : log(avg_online_time_in_30d)
                                   + log(actives_day_in_30d)
                                   + log(avg_online_time_in_30d)
                                   + log(total_active_days)
                                   + log(total_online_time)
                                   + is_clan
                                   + has.rank + is.max.rank + rank
                                   + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                   + has.win.rate + win_rate
                                   + has.mmr + log(mmr)
                                   + has.match.num + log1p(match_num)
                                   + has.gems.consume + log1p(gems_consume),
                                   data = df_reg, family = "gaussian")

        m2.retain.activetot <- glm(is_cont ~ learn_efficiency
                                   + learn_efficiency : log(total_active_days)
                                   + log(actives_day_in_30d)
                                   + log(avg_online_time_in_30d)
                                   + log(total_active_days)
                                   + log(total_online_time)
                                   + is_clan
                                   + has.rank + is.max.rank + rank
                                   + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                   + has.win.rate + win_rate
                                   + has.mmr + log(mmr)
                                   + has.match.num + log1p(match_num)
                                   + has.gems.consume + log1p(gems_consume),
                                   data = df_reg, family = "gaussian")
        
        m2.retain.onlinetot <- glm(is_cont ~ learn_efficiency
                                   + learn_efficiency : log(total_online_time)
                                   + log(actives_day_in_30d)
                                   + log(avg_online_time_in_30d)
                                   + log(total_active_days)
                                   + log(total_online_time)
                                   + is_clan
                                   + has.rank + is.max.rank + rank
                                   + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                   + has.win.rate + win_rate
                                   + has.mmr + log(mmr)
                                   + has.match.num + log1p(match_num)
                                   + has.gems.consume + log1p(gems_consume),
                                   data = df_reg, family = "gaussian")
        
        m2.retain.isclan <- glm(is_cont ~ learn_efficiency
                                + learn_efficiency : is_clan
                                + log(actives_day_in_30d)
                                + log(avg_online_time_in_30d)
                                + log(total_active_days)
                                + log(total_online_time)
                                + is_clan
                                + has.rank + is.max.rank + rank
                                + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                + has.win.rate + win_rate
                                + has.mmr + log(mmr)
                                + has.match.num + log1p(match_num)
                                + has.gems.consume + log1p(gems_consume),
                                data = df_reg, family = "gaussian")

        m2.retain.rank <- glm(is_cont ~ learn_efficiency
                              + learn_efficiency : rank
                              + log(actives_day_in_30d)
                              + log(avg_online_time_in_30d)
                              + log(total_active_days)
                              + log(total_online_time)
                              + is_clan
                              + has.rank + is.max.rank + rank
                              + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                              + has.win.rate + win_rate
                              + has.mmr + log(mmr)
                              + has.match.num + log1p(match_num)
                              + has.gems.consume + log1p(gems_consume),
                              data = df_reg, family = "gaussian")

        m2.retain.avgkills <- glm(is_cont ~ learn_efficiency
                                  + learn_efficiency : log1p(avg_kills)
                                  + log(actives_day_in_30d)
                                  + log(avg_online_time_in_30d)
                                  + log(total_active_days)
                                  + log(total_online_time)
                                  + is_clan
                                  + has.rank + is.max.rank + rank
                                  + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                  + has.win.rate + win_rate
                                  + has.mmr + log(mmr)
                                  + has.match.num + log1p(match_num)
                                  + has.gems.consume + log1p(gems_consume),
                                  data = df_reg, family = "gaussian")

        m2.retain.avgdmg <- glm(is_cont ~ learn_efficiency
                                + learn_efficiency : log1p(avg_damage)
                                + log(actives_day_in_30d)
                                + log(avg_online_time_in_30d)
                                + log(total_active_days)
                                + log(total_online_time)
                                + is_clan
                                + has.rank + is.max.rank + rank
                                + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                + has.win.rate + win_rate
                                + has.mmr + log(mmr)
                                + has.match.num + log1p(match_num)
                                + has.gems.consume + log1p(gems_consume),
                                data = df_reg, family = "gaussian")

        m2.retain.winrate <- glm(is_cont ~ learn_efficiency
                                 + learn_efficiency : win_rate
                                 + log(actives_day_in_30d)
                                 + log(avg_online_time_in_30d)
                                 + log(total_active_days)
                                 + log(total_online_time)
                                 + is_clan
                                 + has.rank + is.max.rank + rank
                                 + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                 + has.win.rate + win_rate
                                 + has.mmr + log(mmr)
                                 + has.match.num + log1p(match_num)
                                 + has.gems.consume + log1p(gems_consume),
                                 data = df_reg, family = "gaussian")

        m2.retain.mmr <- glm(is_cont ~ learn_efficiency
                             + learn_efficiency : log(mmr)
                             + log(actives_day_in_30d)
                             + log(avg_online_time_in_30d)
                             + log(total_active_days)
                             + log(total_online_time)
                             + is_clan
                             + has.rank + is.max.rank + rank
                             + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                             + has.win.rate + win_rate
                             + has.mmr + log(mmr)
                             + has.match.num + log1p(match_num)
                             + has.gems.consume + log1p(gems_consume),
                             data = df_reg, family = "gaussian")
        
        m2.retain.matchnum <- glm(is_cont ~ learn_efficiency
                                  + learn_efficiency : log1p(match_num)
                                  + log(actives_day_in_30d)
                                  + log(avg_online_time_in_30d)
                                  + log(total_active_days)
                                  + log(total_online_time)
                                  + is_clan
                                  + has.rank + is.max.rank + rank
                                  + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                  + has.win.rate + win_rate
                                  + has.mmr + log(mmr)
                                  + has.match.num + log1p(match_num)
                                  + has.gems.consume + log1p(gems_consume),
                                  data = df_reg, family = "gaussian")

        m2.retain.gemsconsume <- glm(is_cont ~ learn_efficiency
                                     + learn_efficiency : log1p(gems_consume)
                                     + log(actives_day_in_30d)
                                     + log(avg_online_time_in_30d)
                                     + log(total_active_days)
                                     + log(total_online_time)
                                     + is_clan
                                     + has.rank + is.max.rank + rank
                                     + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                                     + has.win.rate + win_rate
                                     + has.mmr + log(mmr)
                                     + has.match.num + log1p(match_num)
                                     + has.gems.consume + log1p(gems_consume),
                                     data = df_reg, family = "gaussian")

        ## Report
        cat(sprintf('Start Coeftest for retention\n'))

        lst.s1.retain[[i]] <- coeftest(m1.retain)
        lst.s2.retain[[i]] <- coeftest(m2.retain)
        lst.s2.retain.active30d[[i]] <- coeftest(m2.retain.active30d)
        lst.s2.retain.online30d[[i]] <- coeftest(m2.retain.online30d)
        lst.s2.retain.activetot[[i]] <- coeftest(m2.retain.activetot)
        lst.s2.retain.onlinetot[[i]] <- coeftest(m2.retain.onlinetot)
        lst.s2.retain.isclan[[i]] <- coeftest(m2.retain.isclan)
        lst.s2.retain.rank[[i]] <- coeftest(m2.retain.rank)
        lst.s2.retain.avgkills[[i]] <- coeftest(m2.retain.avgkills)
        lst.s2.retain.avgdmg[[i]] <- coeftest(m2.retain.avgdmg)
        lst.s2.retain.winrate[[i]] <- coeftest(m2.retain.winrate)
        lst.s2.retain.mmr[[i]] <-  coeftest(m2.retain.mmr)
        lst.s2.retain.matchnum[[i]] <- coeftest(m2.retain.matchnum)
        lst.s2.retain.gemsconsume[[i]] <- coeftest(m2.retain.gemsconsume)
        
        rm(m1.retain, m2.retain,
           m2.retain.active30d,
           m2.retain.online30d,
           m2.retain.activetot,
           m2.retain.onlinetot,
           m2.retain.isclan,
           m2.retain.rank,
           m2.retain.avgkills,
           m2.retain.avgdmg,
           m2.retain.winrate,
           m2.retain.mmr,
           m2.retain.matchnum,
           m2.retain.gemsconsume)

        ## Report
        cat(sprintf('Retention Complete\n'))

               



        
    }
    
    ## NOTE: removed April 29, 2025 to avoid confusion
    ## all.lst.s1.learn <- lst.s1.learn
    ## all.lst.s2.learn <- lst.s2.learn
    ## all.lst.s1.futurelearn <- lst.s1.futurelearn
    ## all.lst.s2.futurelearn <- lst.s2.futurelearn
    ## all.lst.s1.retain <- lst.s1.retain
    ## all.lst.s2.retain <- lst.s2.retain
    
    ## Report
    cat(sprintf('Saving .... '))
    
    save(lst.s1.learn,
         lst.s2.learn,
         lst.s2.learn.active30d,
         lst.s2.learn.online30d,
         lst.s2.learn.activetot,
         lst.s2.learn.onlinetot,
         lst.s2.learn.isclan,
         lst.s2.learn.rank,
         lst.s2.learn.avgkills,
         lst.s2.learn.avgdmg,
         lst.s2.learn.winrate,
         lst.s2.learn.mmr,
         lst.s2.learn.matchnum,
         lst.s2.learn.gemsconsume,
         lst.s1.futurelearn,
         lst.s2.futurelearn,
         lst.s2.futurelearn.active30d,
         lst.s2.futurelearn.online30d,
         lst.s2.futurelearn.activetot,
         lst.s2.futurelearn.onlinetot,
         lst.s2.futurelearn.isclan,
         lst.s2.futurelearn.rank,
         lst.s2.futurelearn.avgkills,
         lst.s2.futurelearn.avgdmg,
         lst.s2.futurelearn.winrate,
         lst.s2.futurelearn.mmr,
         lst.s2.futurelearn.matchnum,
         lst.s2.futurelearn.gemsconsume,
         lst.s1.retain,
         lst.s2.retain,
         lst.s2.retain.active30d,
         lst.s2.retain.online30d,
         lst.s2.retain.activetot,
         lst.s2.retain.onlinetot,
         lst.s2.retain.isclan,
         lst.s2.retain.rank,
         lst.s2.retain.avgkills,
         lst.s2.retain.avgdmg,
         lst.s2.retain.winrate,
         lst.s2.retain.mmr,
         lst.s2.retain.matchnum,
         lst.s2.retain.gemsconsume,
         T_seq,
         file = sprintf("./%s/full_learn_bydiff_allreg_s%d_hetero.RData", STR_RESULTS_DIR, TAU_WIN_LENGTH))


    ## Report
    cat(sprintf('Saving Complete!\n'))
} 

