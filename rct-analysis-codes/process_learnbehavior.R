## process_learnbehavior.R
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
STR_RESULTS_DIR <- '20240408_results'

ATTEMPT_CUTOFF <- 4
ACTIVE_DAY_CUTOFF_30D <- 0
ACTIVE_DAY_CUTOFF_TOTAL <- 0

## select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')




## get 6 variables corresponding to variable names
str_varnames <- c("(Intercept)", "wtval_corr_actual", "sahni_complexity",
                  "opt_numitems", "min_sahni", "max_sahni", "greedyval_0_rel_opt_gap")

str_knapsack <- './knapsack_instances_with_complexity.csv';
df_ks <- fread(str_knapsack);
df_ks[,"(Intercept)":=1]

## build extra variables
df_ks$min_sahni <- apply(df_ks[, .(sahni_order_0_rel_opt_gap, sahni_order_1_rel_opt_gap, sahni_order_2_rel_opt_gap,
                                   sahni_order_3_rel_opt_gap, sahni_order_4_rel_opt_gap)],
                         MARGIN = 1,
                         FUN = min)
df_ks$max_sahni <- apply(df_ks[, .(sahni_order_0_rel_opt_gap, sahni_order_1_rel_opt_gap, sahni_order_2_rel_opt_gap,
                                   sahni_order_3_rel_opt_gap, sahni_order_4_rel_opt_gap)],
                         MARGIN = 1,
                         FUN = max)



## output of regression
for (STR_REGION in STR_REGION_ALL) {
  
  
  
  ## Read processed file and some quick calculations
  start.time <- Sys.time()
  cat(sprintf("\n\nProcessing region %s, reading webdata file ... \n", STR_REGION))
  df <- fread(sprintf('./webdata_%s.csv', STR_REGION))
  
  ## define new columns: player attempt and optimality gap, and filter by player attempt and timeoutx
  setkey(df, account_id, DateTime)
  df[, PlayerAttempt:=1:.N, by = account_id]
  cat(sprintf("Read complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
  
  
  ## Read playerstats file
  cat(sprintf("Reading player stats file ... \n"))
  df.stats <- fread(sprintf('./playerstats_%s.csv', STR_REGION))
  cat(sprintf("Read complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))

  
  ## filter playerstats
  cat(sprintf("Processing player stats ... \n"))
  df.stats <- df.stats[!is.na(actives_day_in_30d)
                       & !is.na(avg_online_time_in_30d) & avg_online_time_in_30d > 0
                       & !is.na(total_online_time) & total_online_time > 0
                       & actives_day_in_30d >= ACTIVE_DAY_CUTOFF_30D
                       & total_active_days >= ACTIVE_DAY_CUTOFF_TOTAL
                       ,]
  
  ## create dummies for NA vals
  df.stats[, `:=`(has.rank = !is.na(rank),
                  is.max.rank = (rank == max(rank, na.rm = TRUE)),
                  has.avg.kills = !is.na(avg_kills),
                  has.avg.damage = !is.na(avg_damage),
                  has.win.rate = !is.na(win_rate),
                  has.mmr = !is.na(mmr),
                  has.match.num = !is.na(match_num),
                  has.gems.consume = !is.na(gems_consume)),
  ]
  
  df.stats[has.rank == FALSE, `:=`(rank = 0, is.max.rank = FALSE)]
  df.stats[has.avg.kills == FALSE, avg_kills := 0]
  df.stats[has.avg.damage == FALSE, avg_damage := 0]
  df.stats[has.win.rate == FALSE, win_rate := 0]
  df.stats[has.mmr == FALSE, mmr := 6000]
  df.stats[has.match.num == FALSE, match_num := 0]
  df.stats[has.gems.consume == FALSE, gems_consume := 0]
  
  
  df.stats[, `:=`(norm_avg_kills = normalize(avg_kills),
                  norm_avg_damage = normalize(avg_damage),
                  norm_mmr = normalize(mmr),
                  norm_gems_consume = normalize(gems_consume))]

  ## NOTE:  has.avg.kills and has.damage are collinear, drop one of them
  df.stats[, has.avg.damage := NULL]
  cat(sprintf("Processing complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
  
  
  ## merge with game data
  cat('Merging with game data ... \n');
  setkey(df, MiniGameCode)
  setkey(df_ks, InstanceID)
  df <- df_ks[df, ]
  df[,PalyerId := NULL] ## drop unnecssary columns
  
  df[,ks_val := value1  * SolutionPos01_Boolean_flag +
       value2  * SolutionPos02_Boolean_flag +
       value3  * SolutionPos03_Boolean_flag +
       value4  * SolutionPos04_Boolean_flag +
       value5  * SolutionPos05_Boolean_flag +
       value6  * SolutionPos06_Boolean_flag +
       value7  * SolutionPos07_Boolean_flag +
       value8  * SolutionPos08_Boolean_flag +
       value9  * SolutionPos09_Boolean_flag +
       value10 * SolutionPos10_Boolean_flag +
       value11 * SolutionPos11_Boolean_flag +
       value12 * SolutionPos12_Boolean_flag]
  
  df[,ks_wt  := weight1  * SolutionPos01_Boolean_flag +
       weight2  * SolutionPos02_Boolean_flag +
       weight3  * SolutionPos03_Boolean_flag +
       weight4  * SolutionPos04_Boolean_flag +
       weight5  * SolutionPos05_Boolean_flag +
       weight6  * SolutionPos06_Boolean_flag +
       weight7  * SolutionPos07_Boolean_flag +
       weight8  * SolutionPos08_Boolean_flag +
       weight9  * SolutionPos09_Boolean_flag +
       weight10 * SolutionPos10_Boolean_flag +
       weight11 * SolutionPos11_Boolean_flag +
       weight12 * SolutionPos12_Boolean_flag]
  df[, `:=`(abs_opt_gap = optval - ks_val, rel_opt_gap = (optval - ks_val) / optval, sol_quality = ks_val / optval)]
  df[, c(sprintf('value%d', seq(1, 12))) := NULL]
  df[, c(sprintf('xsol%d', seq(1, 12))) := NULL]
  df[, c(sprintf('weight%d', seq(1, 12))) := NULL]
  
  
  ## load problem difficulty
  load(sprintf('%s/learning_modelest_allreg_notimeout.RData', STR_RESULTS_DIR))
  f4 <- formula(sol_quality ~ 
                  poly(wtval_corr_actual, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$wtval_corr_actual)
                + poly(sahni_complexity, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$sahni_complexity)
                + poly(opt_numitems, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$opt_numitems)
                + poly(min_sahni, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$min_sahni)
                + poly(max_sahni, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$max_sahni)
                + poly(greedyval_0_rel_opt_gap, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$greedyval_0_rel_opt_gap))
  df_new <- df_ks[,..str_varnames]
  df_new$sol_quality <- 0
  sol_qual_vec <- predict.poly(f4, s4$ct, df_new)
  df$InstanceSolQual <- sol_qual_vec[df$InstanceID]

  ## create data table where 1 row = 1 player, record min and max # attempts
  df_player = df[, .(min_player_attempt = min(PlayerAttempt),
                     max_player_attempt = max(PlayerAttempt)), by=account_id]
  df_player <- df_player[max_player_attempt>=ATTEMPT_CUTOFF]
  df_player <- merge(df_player, df.stats, by = "account_id", all = FALSE)

  ## REMOVE PLAYERS USED FOR TRAINING
  ## load file with account_id used for regression and only keep aggregate account id
  df_training_set <- readRDS(sprintf("%s/data_notimeout_for_estimation_reg_%s.rds", STR_RESULTS_DIR, STR_REGION))
  vec_acct_id <- unique(df_training_set[, agg_account_id])
  
  ## remove account_ids used for training
  df_player[,account_id:=paste0(STR_REGION, account_id)]
  setkey(df_player, account_id)
  df_player <- df_player[!vec_acct_id] 
  
  ## remove from instance data too
  df[, account_id := paste0(STR_REGION, account_id)]
  df <- df[account_id %in% df_player$account_id, ]
  
  ## save to rds
  saveRDS(df_player, sprintf('%s/df_player_notimeout_learning_reg%s.rds',STR_RESULTS_DIR, STR_REGION))
  saveRDS(df, sprintf('%s/df_notimeout_learning_reg%s.rds',STR_RESULTS_DIR, STR_REGION))
}



# 
# 
# 
# for (START_IND in 3:8){
#     ## START_IND <- 3  # for individual testing
#     END_IND   <- 25
#     TAU_WIN_LENGTH <- START_IND
# 
#     ## filter
#     ATTEMPT_CUTOFF = 4
#     ACTIVE_DAY_CUTOFF_30D = 0
#     ACTIVE_DAY_CUTOFF_TOTAL = 0
# 
#     ## read knapsack problem data
#     str_knapsack <- './knapsack_instances_with_complexity.csv';
#     df_ks <- fread(str_knapsack);
#     df_ks[,"(Intercept)":=1]
# 
#     # ## base results file to read from
#     # str_base_results_file <- sprintf('./%s/sa_4_multispec.csv', STR_RESULTS_DIR)
#     # df_base_res <- as.data.table(read.csv(str_base_results_file))
# 
#     ## get 6 variables corresponding to variable names
#     # str_varnames <- df_base_res[1:7,H]
# 
#     ## build extra variables
#     df_ks$min_sahni <- apply(df_ks[, .(sahni_order_0_rel_opt_gap, sahni_order_1_rel_opt_gap, sahni_order_2_rel_opt_gap,
#                                        sahni_order_3_rel_opt_gap, sahni_order_4_rel_opt_gap)],
#                              MARGIN = 1,
#                              FUN = min)
#     df_ks$max_sahni <- apply(df_ks[, .(sahni_order_0_rel_opt_gap, sahni_order_1_rel_opt_gap, sahni_order_2_rel_opt_gap,
#                                        sahni_order_3_rel_opt_gap, sahni_order_4_rel_opt_gap)],
#                              MARGIN = 1,
#                              FUN = max)
# 
# 
#     # sol_qual_vec <- as.matrix(df_ks[,..str_varnames]) %*%
#     #     as.matrix(df_base_res[1:7, list(est_A, est_B, est_C, est_D, est_E)])
# 
# 
# 
#     ## window to learn over
#     tau_seq = seq(START_IND, 25, 1)
# 
#     ## output variable
#     all.lst.template <- vector("list", length(STR_REGION_ALL))
#     names(all.lst.template) <- STR_REGION_ALL
# 
#     all.lst.s1.learn <- all.lst.template
#     all.lst.s2.learn <- all.lst.template
# 
#     all.lst.s1.futurelearn <- all.lst.template
#     all.lst.s2.futurelearn <- all.lst.template
# 
#     all.lst.s1.retain <- all.lst.template
#     all.lst.s2.retain <- all.lst.template
# 
#     ## TODO
# 
# }
    
    #     cat(sprintf("Merge with game info complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
    # 
    #     ## merge with player stats
    #     cat('Merging with game data ... \n');
    #     cat(sprintf("Merge with player stats complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
    # 
    #     ## delete uneeded
    #     ## df.stats <- NULL
    #     gc()
    #     cat('Learning Test Begin ... \n');
    #     
    #     ## load problem difficulty
    #     load(sprintf('%s/learning_modelest_%s_notimeout.RData', STR_RESULTS_DIR, STR_REGION))
    #     sol_qual_vec <- as.matrix(df_ks[,..str_varnames]) %*% coef(s4$ct)
    #     df$InstanceSolQual <- sol_qual_vec[df$InstanceID]
    #     
    #     ## df$InstanceSolQual <- sol_qual_vec[df$InstanceID, sprintf("est_%s", STR_REGION)]
    #     df$InstanceQuantile = cut(df$InstanceSolQual, 
    #                               breaks = quantile(df$InstanceSolQual, probs = c(0, 0.5, 1.0)), 
    #                               include.lowest = TRUE, 
    #                               labels = c(0, 1))
    #     
    #     ## TEST 1 focus on individual instances (performance)
    #     
    #     
    #     ## TEST 2 focus on individual players (retention) -- no results here
    #     ## consider: (1) adding player performance on past problem, (2) quartiles of difficulty
    #     
    #     ## create data table where 1 row = 1 player, record min and max # attempts
    #     df_player = df[, .(min_player_attempt = min(PlayerAttempt), 
    #                        max_player_attempt = max(PlayerAttempt)), by=account_id]
    #     df_player <- df_player[max_player_attempt>=ATTEMPT_CUTOFF]
    #     df_player <- merge(df_player, df.stats, by = "account_id", all = FALSE)
    #     
    #     ## remove players whose data were used for estimation. variable name = df.for.est
    #     load(sprintf('%s/data_notimeout_estimation_reg_%s.RData', STR_RESULTS_DIR, STR_REGION))
    #     setkey(df_player, account_id)
    #     df_player <- df_player[!df.for.est] 
    #     ## 
    #     
    #     
    #     lst.s1.learn <- vector("list", length(tau_seq))
    #     lst.s2.learn <- vector("list", length(tau_seq))
    #     
    #     lst.s1.futurelearn <- vector("list", length(tau_seq))
    #     lst.s2.futurelearn <- vector("list", length(tau_seq))
    #     
    #     lst.s1.retain <- vector("list", length(tau_seq))
    #     lst.s2.retain <- vector("list", length(tau_seq))
    #     
    #     for (i in 1:length(tau_seq)){ 
    #         ## Report
    #         cat(sprintf("Region %s, working on tau = %d of %d ... \n", 
    #                     STR_REGION, tau_seq[i], tau_seq[length(tau_seq)]))
    #         
    #         ## tau
    #         tau <- tau_seq[i]
    #         tau_lb <- tau - TAU_WIN_LENGTH + 1
    #         
    #         ## long to wide
    #         df_tmp <- dcast(df[PlayerAttempt <= tau & PlayerAttempt >= tau_lb,], 
    #                         account_id ~ PlayerAttempt, value.var = "InstanceSolQual")
    #         
    #         ## remove rows where tau > player attempts
    #         df_tmp <- na.omit(df_tmp)
    #         
    #         ## add player stats
    #         df_tmp <- merge(df_tmp, df.stats, by = "account_id", all = FALSE)
    #         
    #         ## convert to numbers
    #         str_colnames <- as.character(tau_lb:tau)
    #         z <- lapply(df_tmp[,..str_colnames], function(x) as.numeric(as.character(x)))
    #         df_tmp$learn_efficiency <- apply(as.data.table(z), MARGIN = 1, sum)
    #         df_tmp[,str_colnames] <- NULL
    #         
    #         ## merge max player attempts -- to get continuation flag
    #         df_tmp <- merge(df_tmp, df_player[, .(account_id, max_player_attempt)], "account_id")
    #         df_tmp <- df_tmp[, rem_attempts := max_player_attempt - tau][, is_cont := rem_attempts > 0]
    #         
    #         ## merge player performance in next iteration
    #         df_next_perf <- df[PlayerAttempt == tau + 1, .(account_id, sol_quality)]
    #         df_tmp <- merge(df_tmp, df_next_perf, by = "account_id", all.x = TRUE)
    #         
    #         ## merge player performance in future iterations
    #         df_future_perf <- df[PlayerAttempt >= tau + 1, .(future_sol_quality = mean(sol_quality)), by = account_id]
    #         df_tmp <- merge(df_tmp, df_future_perf, by = "account_id", all.x = TRUE)
    #         
    #         ## regress
    #         df_reg <- df_tmp
    #         ## df_reg <- df_reg[, c("account_id", "max_player_attempt", "rem_attempts") := NULL]
    #         
    #         m1.learn <- glm(sol_quality ~ learn_efficiency, 
    #                         data = df_reg[is_cont == TRUE,], family = "gaussian")
    #         m2.learn <- glm(sol_quality ~ learn_efficiency 
    #                         + log(actives_day_in_30d) + log(avg_online_time_in_30d) +
    #                         + log(total_active_days) + log(total_online_time)
    #                         + is_clan
    #                         + has.rank + is.max.rank + rank
    #                         + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
    #                         + has.win.rate + win_rate
    #                         + has.mmr + log(mmr)
    #                         + has.match.num + log1p(match_num)
    #                         + has.gems.consume + log1p(gems_consume), 
    #                         data = df_reg[is_cont == TRUE,], family = "gaussian")
    # 
    #         m1.futurelearn <- glm(future_sol_quality ~ learn_efficiency,
    #                                data = df_reg[is_cont == TRUE,], family = "gaussian")
    #         m2.futurelearn <- glm(future_sol_quality ~ learn_efficiency
    #                               + log(actives_day_in_30d) + log(avg_online_time_in_30d) +
    #                           + log(total_active_days) + log(total_online_time)
    #                         + is_clan
    #                         + has.rank + is.max.rank + rank
    #                         + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
    #                         + has.win.rate + win_rate
    #                         + has.mmr + log(mmr)
    #                         + has.match.num + log1p(match_num)
    #                         + has.gems.consume + log1p(gems_consume),
    #                         data = df_reg[is_cont == TRUE,], family = "gaussian")
    # 
    #         m1.retain <- glm(max_player_attempt ~ learn_efficiency,
    #                   data = df_reg, family = "gaussian")
    #         m2.retain <- glm(max_player_attempt ~ learn_efficiency
    #                   + log(actives_day_in_30d) + log(avg_online_time_in_30d) +
    #                   + log(total_active_days) + log(total_online_time)
    #                   + is_clan
    #                   + has.rank + is.max.rank + rank
    #                   + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
    #                   + has.win.rate + win_rate
    #                   + has.mmr + log(mmr)
    #                   + has.match.num + log1p(match_num)
    #                   + has.gems.consume + log1p(gems_consume),
    #                   data = df_reg, family = "gaussian")
    # 
    #         lst.s1.learn[[i]] <- coeftest(m1.learn)
    #         lst.s2.learn[[i]] <- coeftest(m2.learn)
    # 
    #         lst.s1.futurelearn[[i]] <- coeftest(m1.futurelearn)
    #         lst.s2.futurelearn[[i]] <- coeftest(m2.futurelearn)
    # 
    #         lst.s1.retain[[i]] <- coeftest(m1.retain)
    #         lst.s2.retain[[i]] <- coeftest(m2.retain)
    #     }
    #     all.lst.s1.learn[[STR_REGION]] <- lst.s1.learn
    #     all.lst.s2.learn[[STR_REGION]] <- lst.s2.learn
    # 
    #     all.lst.s1.futurelearn[[STR_REGION]] <- lst.s1.futurelearn
    #     all.lst.s2.futurelearn[[STR_REGION]] <- lst.s2.futurelearn
    # 
    #     all.lst.s1.retain[[STR_REGION]] <- lst.s1.retain
    #     all.lst.s2.retain[[STR_REGION]] <- lst.s2.retain
    # }
    # save(all.lst.s1.learn, all.lst.s2.learn,
    #      all.lst.s1.futurelearn, all.lst.s2.futurelearn,
    #      all.lst.s1.retain, all.lst.s2.retain,
    #      tau_seq,
    #      file = sprintf("./%s/full_learn_bydiff_allreg_s%d.RData", STR_RESULTS_DIR, START_IND))
    # 
    # 

