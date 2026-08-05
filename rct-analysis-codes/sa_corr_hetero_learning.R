## sa_corr_hetero_learning.R
## 20250520 Added to analyze learning results. 

rm(list = ls())
library('data.table')
library('sandwich')
library('lmtest')
library('fixest')
library('car')

## select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')

## filter
ATTEMPT_CUTOFF = 4
ACTIVE_DAY_CUTOFF_30D = 0
ACTIVE_DAY_CUTOFF_TOTAL = 0

## master list of variables
## STR_VARNAMES_MASTER <- c("wtval_corr_actual", "sahni_complexity", "opt_numitems",
##                          "sahni_order_0_rel_opt_gap", "sahni_order_1_rel_opt_gap", "sahni_order_2_rel_opt_gap",
##                          "sahni_order_3_rel_opt_gap", "sahni_order_4_rel_opt_gap", "greedyval_0_rel_opt_gap",
##                          "PlayerAttempt")

## read knapsack problem data
str_knapsack <- './knapsack_instances_with_complexity.csv';
df_ks <- fread(str_knapsack);

normalize <- function(x){
    return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

lm.helper <- function(cur.form, df){
    ## run linear model with clustering
    lm.result <- lm(cur.form, data = df)
    cur.vcov <- vcovCL(lm.result, ~account_id)
    lst.out  <- list(ct = coeftest(lm.result, vcov. = cur.vcov), vcov = cur.vcov)

    ## add attributes
    s <- summary(lm.result)
    lst.out$Rsq <-  s$r.squared
    lst.out$Fstat <-  s$fstatistic[1]
    lst.out$N <- s$nobs
    lst.out$Np <- s$nobs - s$df
    lst.out$VIF <- vif(lm.result)
 
    ## attr(s.ret, "Rsq") <-  s$r.squared
    ## attr(s.ret, "Fstat") <-  s$fstatistic[1]
    ## attr(s.ret, "N") <- s$nobs
    ## attr(s.ret, "Np") <- s$nobs - s$df
    ## attr(s.ret, "VIF") <- vif(lm.result)

    ## min/max of variables in list
    ## str_varnames <- intersect(names(lm.result$coefficients), STR_VARNAMES_MASTER)
    ## attr(s.ret, "var.range") <- apply(df[, ..str_varnames], MARGIN=2,FUN = function(x) c(min = min(x), max = max(x)))

    return(lst.out)
}

# output of regression
for (STR_REGION in STR_REGION_ALL) {

    ## Read processed file and some quick calculations
    start.time <- Sys.time()
    cat(sprintf("\n\nProcessing region %s, reading webdata file ... \n", STR_REGION))
    df <- fread(sprintf('./webdata_%s.csv', STR_REGION))

    ## define new columns: player attempt and optimality gap, and filter by player attempt and timeoutx
    setkey(df, account_id, DateTime)
    df[, PlayerAttempt:=1:.N, by = account_id]
    df <- df[PlayerAttempt >= ATTEMPT_CUTOFF & !WasTimeOut,]
    ##df <- df[PlayerAttempt >= ATTEMPT_CUTOFF,]
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

    ## remove unneccessary columns
    df.stats[, `:=`(register_ts = NULL, p = NULL)]
    
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
    if("PalyerId" %in% names(df)){
        df[,PalyerId := NULL] ## drop unnecssary columns
    }
    
    ## df <- merge(df[,.(MiniGameCode, account_id, DateTime, TimeTaken, WasTimeOut, PlayerAttempt,
    ##                   num.net.selected, num.unselect, num.undo, max.undo)],
    ##             df_ks, by.x = "MiniGameCode", by.y = "InstanceID", all.x = TRUE)

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

    
    cat(sprintf("Merge with game info complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))

    ## merge with player stats
    cat('Merging with game data ... \n');
    df <- merge(df, df.stats, by = "account_id", all = FALSE)
    cat(sprintf("Merge with player stats complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))

    ## delete uneeded
    df.stats <- NULL
    gc()
    
    
    cat('Starting LM ... \n');
    ## LINEAR MODEL CALCULATION

    ## TESTING START MAY 6 2022
    df[, num.actions := num.net.selected + 2*num.unselect]
    ## TESTING END

    ## df$opt_numitems <- df$opt_numitems_max
    tmp_numitems_min <- df$opt_numitems
    tmp_numitems_max <- df$opt_numitems_max
    df$opt_numitems <- 0.5 * tmp_numitems_min + 0.5 * tmp_numitems_max

    df$min_sahni <- apply(df[, .(sahni_order_0_rel_opt_gap, sahni_order_1_rel_opt_gap, sahni_order_2_rel_opt_gap,
                                 sahni_order_3_rel_opt_gap, sahni_order_4_rel_opt_gap)],
                          MARGIN = 1,
                          FUN = min)
    df$max_sahni <- apply(df[, .(sahni_order_0_rel_opt_gap, sahni_order_1_rel_opt_gap, sahni_order_2_rel_opt_gap,
                                 sahni_order_3_rel_opt_gap, sahni_order_4_rel_opt_gap)],
                          MARGIN = 1,
                          FUN = max)

    ## only keep f6 for this sanalysis
    f6 <- formula(sol_quality ~ wtval_corr_actual + sahni_complexity + opt_numitems
                  + min_sahni + max_sahni + greedyval_0_rel_opt_gap
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


    f6.active30d   <- update(f6, ~ . + PlayerAttempt:log(actives_day_in_30d))
    f6.online30d   <- update(f6, ~ . + PlayerAttempt:log(avg_online_time_in_30d))
    f6.activetot   <- update(f6, ~ . + PlayerAttempt:log(total_active_days))
    f6.onlinetot   <- update(f6, ~ . + PlayerAttempt:log(total_online_time))
    f6.isclan      <- update(f6, ~ . + PlayerAttempt:is_clan)
    f6.rank        <- update(f6, ~ . + PlayerAttempt:rank)  
    f6.avgkills    <- update(f6, ~ . + PlayerAttempt:log1p(avg_kills))
    f6.avgdmg      <- update(f6, ~ . + PlayerAttempt:log1p(avg_damage))
    f6.winrate     <- update(f6, ~ . + PlayerAttempt:win_rate)
    f6.mmr         <- update(f6, ~ . + PlayerAttempt:log(mmr))
    f6.matchnum    <- update(f6, ~ . + PlayerAttempt:log1p(match_num))
    f6.gemsconsume <- update(f6, ~ . + PlayerAttempt:log1p(gems_consume))
    
    s6.active30d   <- lm.helper(f6.active30d, df)
    s6.online30d   <- lm.helper(f6.online30d, df)
    s6.activetot   <- lm.helper(f6.activetot, df)
    s6.onlinetot   <- lm.helper(f6.onlinetot, df)
    s6.isclan      <- lm.helper(f6.isclan, df)
    s6.rank        <- lm.helper(f6.rank, df)
    s6.avgkills    <- lm.helper(f6.avgkills, df)
    s6.avgdmg      <- lm.helper(f6.avgdmg, df)
    s6.winrate     <- lm.helper(f6.winrate, df)
    s6.mmr         <- lm.helper(f6.mmr, df)
    s6.matchnum    <- lm.helper(f6.matchnum, df)
    s6.gemsconsume <- lm.helper(f6.gemsconsume, df)
    
    save(s6.active30d,
         s6.online30d,
         s6.activetot,
         s6.onlinetot, 
         s6.isclan,
         s6.rank, 
         s6.avgkills,
         s6.avgdmg, 
         s6.winrate, 
         s6.mmr, 
         s6.matchnum, 
         s6.gemsconsume, 
         ATTEMPT_CUTOFF, ACTIVE_DAY_CUTOFF_30D, ACTIVE_DAY_CUTOFF_TOTAL,
         file = sprintf("./sa_%s_hetero_learning.RData", STR_REGION))

    cat(sprintf("Saving complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
}






