## prettyprint_poly_results_multi.R
rm(list = ls())
library('data.table')
library('biglm')
library('lmtest')
library('ggplot2')
library('gridExtra')
library('ggpubr')

SAVE_TO_FILE <- TRUE
LM4_SELECT <- TRUE

STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')

str.col.main <- c('wtval_corr_actual', 'sahni_complexity', 'opt_numitems', 
                  'min_sahni', 'max_sahni', 'greedyval_0_rel_opt_gap')
str.col.aux <- c('PlayerAttempt',
                 'actives_day_in_30d', 'avg_online_time_in_30d',
                 'total_active_days', 'total_online_time',
                 'is_clan', 'has.rank', 'is.max.rank', 'rank',
                 'has.avg.kills', 'avg_kills', 'avg_damage',
                 'has.win.rate', 'win_rate',
                 'has.mmr', 'mmr',
                 'has.match.num', 'match_num',
                 'has.gems.consume', 'gems_consume')

str.xlabels <- c('Correlation between value and weight vectors',
                 'Sahni complexity measure',
                 'Optimal number of items in knapsack',
                 'Minimum gap from Sahni heuristics', 
                 'Maximum gap from Sahni heuristics', 
                 'Gap from greedy value heuristic')
names(str.xlabels) <- str.col.main

POLY_DEGREE <- 4
N_PTS <- 15

f4 <- formula(sol_quality ~ 
                poly(wtval_corr_actual, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$wtval_corr_actual)
              + poly(sahni_complexity, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$sahni_complexity)
              + poly(opt_numitems, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$opt_numitems)
              + poly(min_sahni, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$min_sahni)
              + poly(max_sahni, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$max_sahni)
              + poly(greedyval_0_rel_opt_gap, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$greedyval_0_rel_opt_gap))

f6 <- formula(sol_quality ~ 
                poly(wtval_corr_actual, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$wtval_corr_actual)
              + poly(sahni_complexity, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$sahni_complexity)
              + poly(opt_numitems, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$opt_numitems)
              + poly(min_sahni, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$min_sahni)
              + poly(max_sahni, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$max_sahni)
              + poly(greedyval_0_rel_opt_gap, POLY_DEGREE, raw = F, coefs = lst.poly.coeffs$greedyval_0_rel_opt_gap)
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


if(LM4_SELECT){
  cur.form <- f4
  str.cur.lm <- "lm4_orig"
  str.all.cols <- str.col.main
} else {
  cur.form <- f6
  str.cur.lm <- "lm6_orig"
  str.all.cols <- c(str.col.main, str.col.aux)
}

plot.list.allregions <- vector('list', length(STR_REGION_ALL))
names(plot.list.allregions) <- STR_REGION_ALL

for (STR_REGION in STR_REGION_ALL){
  load(sprintf('./sa_%s_poly%d_lm_clust.RData', STR_REGION, POLY_DEGREE))
  load(sprintf('./reg_%s_poly_sumstats.RData', STR_REGION))
  
  plot.list <- vector("list", length(str.col.main))
  names(plot.list) <- str.col.main
  
  for(cur.str.col in str.col.main){
    ## build base columns
    df.new <- as.data.frame(sum.stats[rep("mean", N_PTS),str.all.cols])
    
    ## target column
    x_min <- sum.stats["min",cur.str.col]
    x_max <- sum.stats["max",cur.str.col]
    
    # ## outlier 
    # if(cur.str.col == 'opt_numitems'){
    #   x_min <- 3
    # }
    
    x_var <- seq(x_min, x_max, length.out = N_PTS)
    df.new[, cur.str.col] <- x_var
    
    # new column for results
    df.new$sol_quality <- 0
    
    ## try with SE
    m.mat <- model.matrix(terms(cur.form),data=df.new)
    m.mod <- eval(sym(str.cur.lm))
    m.coef <- coef(m.mod$ct)
    y_pred <- as.vector(m.mat %*% m.coef)

    ## make plot 
    df.plot <- data.frame(x = x_var, y_pred = y_pred, region = rep(STR_REGION, N_PTS))
    plot.list[[cur.str.col]] <- df.plot
  }
  plot.list.allregions[[STR_REGION]] <- plot.list
}

## REASSEMBLE 
lst.plots <- list()
for(cur.str.col in str.col.main){
  ## extract data 
  df.curplot <- do.call(rbind, lapply(plot.list.allregions, function(df) df[[cur.str.col]]))
  rownames(df.curplot) <- NULL
  df.curplot <- as.data.table(df.curplot)
  
  ## raw plots
  p1 <- ggplot(data = df.curplot,
              aes(x = x, y = y_pred, group = region, color = region)) + 
              geom_line(linewidth = 0.5, aes(linetype = region)) +
              geom_point(size = 2, aes(shape = region, fill = region)) +
              scale_linetype_manual(values = c(1,1,1,1,1)) +
              scale_shape_manual(values=c(21,22,23,24,25)) +
              xlab(str.xlabels[cur.str.col]) + ylab("Solution quality") + 
              theme_bw(base_size = 8)
  
  ## percentage plot
  df.curplot[,y_perc := 100 * y_pred/.SD[1,y_pred], by = region]
  
  p2 <- ggplot(data = df.curplot,
              aes(x = x, y = y_perc, group = region, color = region)) + 
              geom_line(linewidth = 0.5, aes(linetype = region)) +
              geom_point(size = 2, aes(shape = region, fill = region)) +
              scale_linetype_manual(values = c(1,1,1,1,1)) +
              scale_shape_manual(values=c(21,22,23,24,25)) + 
              xlab(str.xlabels[cur.str.col]) + ylab("Relative solution quality (%)") +
              theme_bw(base_size = 8)
  
  if(SAVE_TO_FILE){
    lst.plots <- append(lst.plots, list(p1, p2))
  } else {
    plot(p1)
    plot(p2)
  }
}



if(SAVE_TO_FILE){
  p <- ggarrange(plotlist = lst.plots, nrow=6, ncol=2, common.legend = TRUE, legend = "bottom") 
  ggsave(sprintf('./fig_all_reg_poly%d_%s.png', POLY_DEGREE, substr(str.cur.lm, 1, 3)), 
         width = 10, height = 12, units = "in", dpi = 320,
         plot = p)
  # ggsave(sprintf('./fig_all_reg_poly%d_%s.png', POLY_DEGREE, substr(str.cur.lm, 1, 3)), 
  #         width = 8, height = 12, units = "in", dpi = 320,
  #         marrangeGrob(grobs=c(lst.plots1, lst.plots2), 
  #                      nrow = 6, ncol = 2,top = NULL))
}