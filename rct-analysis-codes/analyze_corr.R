## analyze_corr.R
rm(list = ls())
library('data.table')
library('sandwich')
library('lmtest')
        

# select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')
##STR_REGION_ALL <- c('A', 'B', 'C', 'D')
#STR_REGION_ALL <- c('A')


# filter
ATTEMPT_CUTOFF = 4
ACTIVE_DAY_CUTOFF_30D = 0
ACTIVE_DAY_CUTOFF_TOTAL = 0

# read knapsack problem data
str_knapsack <- './knapsack_instances_with_complexity.csv';
df_ks <- fread(str_knapsack);

normalize <- function(x){
    return ((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
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
    ## df.stats[, `:=`(register_ts = NULL, p = NULL)]
    
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

    ## df.stats[, `:=`(avg_kills = NULL,
    ##                 avg_damage = NULL,
    ##                 mmr = NULL,
    ##                 gems_consume = NULL)]


    ## NOTE:  has.avg.kills and has.damage are collinear, drop one of them
    df.stats[, has.avg.damage := NULL]
    cat(sprintf("Processing complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
    
    
    ## merge with game data
    cat('Merging with game data ... \n');
    setkey(df, MiniGameCode)
    setkey(df_ks, InstanceID)
    df <- df_ks[df, ]
    df[,PalyerId := NULL] ## drop unnecssary columns
    
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


    ## ## HACK: for summary stats
    ## fwrite(df, file = sprintf('./processed_webdata_%s.csv', STR_REGION))
    ## fwrite(df.stats, file = sprintf('./processed_playerstats_%s.csv', STR_REGION))
    ## next
    
    ## delete uneeded
    df.stats <- NULL
    ##df_ks <- NULL
    gc()
    
    
    ## ## CORRELATION CALCULATION
    ## ## calculate corr with actual wtvalcorr
    ## df[PlayerAttempt >= ATTEMPT_CUTOFF,
    ##             .(pcor=cor(rel_opt_gap, wtval_corr_actual, method = "pearson"),
    ##               scor=cor(rel_opt_gap, wtval_corr_actual, method = "spearman")),
    ##             by=wtval_corr_designed]

    ## ## calculate corr with sahni complexity
    ## df[PlayerAttempt >= ATTEMPT_CUTOFF,
    ##             .(pcor=cor(rel_opt_gap, sahni_complexity, method = "pearson"),
    ##               scor=cor(rel_opt_gap, sahni_complexity, method = "spearman")),
    ##             by=wtval_corr_designed]

    cat('Starting LM ... \n');
 
    ## LINEAR MODEL CALCULATION

    ## Testing May 6 2022
    ##df$opt_numitems <- df$opt_numitems_max
    ##df[, num.actions := num.net.selected + 2*num.unselect]
    
    lm.result = lm(sol_quality ~ wtval_corr_actual + sahni_complexity + PlayerAttempt + TimeTaken + opt_numitems
                   + sahni_order_0_rel_opt_gap + sahni_order_1_rel_opt_gap + sahni_order_2_rel_opt_gap
                   + sahni_order_3_rel_opt_gap + sahni_order_4_rel_opt_gap + greedyval_0_rel_opt_gap
                   + num.unselect + num.undo 
                   + log(actives_day_in_30d) + log(avg_online_time_in_30d)
                   + log(total_active_days) + log(total_online_time)
                   + is_clan
                   + has.rank + is.max.rank + rank
                   + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
                   + has.win.rate + win_rate
                   + has.mmr + log(mmr)
                   + has.match.num + log1p(match_num)
                   + has.gems.consume + log1p(gems_consume),
                   data = df)
    ## end testing
    
    ## lm.result = lm(sol_quality ~ wtval_corr_actual + sahni_complexity + PlayerAttempt + TimeTaken + opt_numitems
    ##                + sahni_order_0_rel_opt_gap + sahni_order_1_rel_opt_gap + sahni_order_2_rel_opt_gap
    ##                + sahni_order_3_rel_opt_gap + sahni_order_4_rel_opt_gap + greedyval_0_rel_opt_gap
    ##                + num.unselect + num.undo 
    ##                + log(actives_day_in_30d) + log(avg_online_time_in_30d)
    ##                + log(total_active_days) + log(total_online_time)
    ##                + is_clan
    ##                + has.rank + is.max.rank + rank
    ##                + has.avg.kills + log1p(avg_kills) + log1p(avg_damage)
    ##                + has.win.rate + win_rate
    ##                + has.mmr + log(mmr)
    ##                + has.match.num + log1p(match_num)
    ##                + has.gems.consume + log1p(gems_consume),
    ##                data = df)

    cat('Model 1 complete.')
    ## save to file
    s <- summary(lm.result)
    ## s1$residuals <- NULL
    s1 <- coeftest(lm.result, vcov. = vcovCL, cluster = ~account_id)
    ##s1 <- coeftest(lm.result, vcov. = vcovCL, )
    attr(s1, "Rsq") <-  s$r.squared
    attr(s1, "Fstat") <-  s$fstatistic[1]
    s <- NULL
    lm.result <- NULL
   
   

    ## lm.result.2 = lm(sol_quality ~ wtval_corr_actual + sahni_complexity + PlayerAttempt + TimeTaken + opt_numitems
    ##                + sahni_order_0_rel_opt_gap + sahni_order_1_rel_opt_gap + sahni_order_2_rel_opt_gap
    ##                + sahni_order_3_rel_opt_gap + sahni_order_4_rel_opt_gap + greedyval_0_rel_opt_gap
    ##                + num.unselect + num.undo 
    ##                + log(actives_day_in_30d) + log(avg_online_time_in_30d)
    ##                + log(total_active_days) + log(total_online_time)
    ##                + is_clan
    ##                + rank + is.max.rank 
    ##                + log1p(avg_kills) + log1p(avg_damage)
    ##                + win_rate
    ##                + log(mmr)
    ##                + log1p(match_num)
    ##                + has.gems.consume + log1p(gems_consume), 
    ##                data = df[has.rank & has.avg.kills & has.win.rate & has.mmr & has.match.num,])


    ## cat(sprintf("LM complete. Now Saving. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
    
    ## ## s <- summary(lm.result)
    ## ## tmp <- s$coefficients
    ## ## colnames(tmp) <- paste0(STR_REGION, "_", colnames(tmp))
    ## ## lm.out.all <- cbind(lm.out.all, tmp)

    ## s <- summary(lm.result.2)
    ## ##s2$residuals <- NULL
    ## ##s2 <- coeftest(lm.result.2)
    ## s2 <- coeftest(lm.result.2, vcov. = vcovCL, cluster = ~account_id)
    ## attr(s2, "Rsq") <-  s$r.squared
    ## attr(s2, "Fstat") <-  s$fstatistic[1]
    ## s <- NULL
    ## lm.result.2 <- NULL
    s2 <- NULL


    save(s1, s2, ATTEMPT_CUTOFF, ACTIVE_DAY_CUTOFF_30D, ACTIVE_DAY_CUTOFF_TOTAL,
         file = sprintf("./lmoutput_%s_optval.RData", STR_REGION))

    cat(sprintf("Saving complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
}
##save(lm.out.all, lst.results.all, STR_REGION_ALL, ATTEMPT_CUTOFF, str.call, file = "./lmoutput_all_regions.RData")




