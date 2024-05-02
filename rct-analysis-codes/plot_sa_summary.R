## plot_sa_summary.R
##
## Generates box-and-whisker plots of a summary of SA results
##
## Version: Feb 22, 2024

rm(list = ls())
library('data.table')
library('ggplot2')
library('gridExtra')


SAVE_TO_FILE <- TRUE

STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')
STR_POSTFIX_ALL <- c("attempt4_avg", "notimeout_avg", "minitems", "maxitems",
                     "attempt7_avg", "attempt10_avg", "attempt13_avg", 
                     "recent_active_5_avg", "recent_active_10_avg", "recent_active_15_avg",
                     "total_active_30_avg", "total_active_60_avg", "total_active_90_avg")


str.col.main <- c('wtval_corr_actual', 'sahni_complexity', 'opt_numitems', 
                  'min_sahni', 'max_sahni', 'greedyval_0_rel_opt_gap', 'PlayerAttempt')

str.pretty.labels <- c('Correlation between value and weight vectors',
                       'Sahni complexity measure',
                       'Optimal number of items in knapsack',
                       'Minimum gap from Sahni heuristics', 
                       'Maximum gap from Sahni heuristics', 
                       'Gap from greedy value heuristic',
                       'Num. minigame attempts per player')

names(str.pretty.labels) <- str.col.main

## main results
out.df <- data.table(rn = rep(str.col.main, length(STR_REGION_ALL)), 
                     reg = do.call(c, lapply(STR_REGION_ALL, FUN = function(x) rep(x, length(str.col.main)))))

## initialize
out.str <- ''

## iterate
for(STR_POSTFIX in STR_POSTFIX_ALL){
  lst_est = vector("list", length(STR_REGION_ALL))
  names(lst_est) <- STR_REGION_ALL
  for(STR_REGION in STR_REGION_ALL){
    ## load file
    load(sprintf('./sa_%s_%s.RData', STR_REGION, STR_POSTFIX))
    
    ## model object
    s <- s6
    
    ## get model and read
    df <- as.data.table(s$ct[,], keep.rownames=TRUE)
    
    ## get new df
    lst_est[[STR_REGION]] <- df[rn %in% str.col.main, Estimate]
  }
  
  ## add to vector
  out.df[,eval(STR_POSTFIX) := do.call(c, lst_est)]
}

lst.plots <- vector('list', length(str.col.main)-1)
names(lst.plots) <- str.col.main[1:length(str.col.main)-1]
for (str.feature in names(lst.plots)){
  ## get current feature
  df <- out.df[rn == str.feature,]
  
  ## prepare plotting dataframe
  plot.df <- data.table(reg = character(), val = numeric())
  for (STR_REGION in STR_REGION_ALL){
    tmp <- df[reg == STR_REGION, ..STR_POSTFIX_ALL]  
    plot.df <- rbind(plot.df, data.table(reg = rep(STR_REGION, length(STR_POSTFIX_ALL)),
                                val = unlist(tmp)))
  }
  
  ## use ggplot to plot density
  # p <- ggplot(plot.df, aes(x=val, fill=reg, colour=reg)) +
  #       geom_density(alpha = 0.3, adjust = 1, position = "identity", linewidth = 0.5) +
  #       xlab(str.pretty.labels[[str.feature]]) + 
  #       labs(fill='Region', colour='Region') +
  #       geom_vline(xintercept = 0, linetype = 2, linewidth = 0.5) +
  #       geom_hline(yintercept = 0, linetype = 1, linewidth = 0.5) +
  #       theme_bw(base_size = 8) + 
  #       theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  plot.df[, reg := as.factor(reg)]
  p <- ggplot(plot.df, aes(x = reg, y = val, colour = reg)) + 
          geom_boxplot(outlier.size = 0.5, staplewidth = 0.5, show.legend = FALSE) + 
          ggtitle(str.pretty.labels[[str.feature]]) +
          ylab('Estimated coefficient') + 
          xlab('Region') + 
          geom_hline(yintercept = 0, linetype = 2, linewidth = 0.5) +
          theme_bw(base_size = 8)
  
  
  if(str.feature %in% str.col.main[1:2]){
    p <- p + ylim(0, 1.2*max(plot.df$val))
  } else {
    p <- p + ylim(1.2*min(plot.df$val), 0)
  }
  
  if(SAVE_TO_FILE){
    lst.plots[[str.feature]] <- p
  } else {
    plot(p)  
  }
}
if(SAVE_TO_FILE){
  ggsave(sprintf('./fig_sa_summary_all.png'),
         width = 8, height = 6, units = "in", dpi = 320,
         marrangeGrob(grobs=lst.plots, 
                      nrow = 3, ncol = 2,top = NULL,
                      layout_matrix = matrix(1:6, 3, 2, TRUE)))
}