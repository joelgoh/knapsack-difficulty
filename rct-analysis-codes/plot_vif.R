## plot_vif.R
##
## plots VIF of main results
##
## Version: Feb 22, 2024

rm(list = ls())
library('data.table')
library('ggplot2')

SAVE_TO_FILE <- TRUE
STR_RESULTS_DIR <- '20240221_results'
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')

str.col.main <- c('(Intercept)', 'wtval_corr_actual', 'sahni_complexity', 'opt_numitems', 
                  'min_sahni', 'max_sahni', 'greedyval_0_rel_opt_gap', 'PlayerAttempt')
str.pretty.labels <- c('Intercept',
                       'Correlation between value and weight vectors',
                       'Sahni complexity measure',
                       'Optimal number of items in knapsack',
                       'Minimum gap from Sahni heuristics', 
                       'Maximum gap from Sahni heuristics', 
                       'Gap from greedy value heuristic',
                       'Num. minigame attempts per player')
names(str.pretty.labels) <- str.col.main

## main results
lst.results <- vector("list", length(STR_REGION_ALL))
names(lst.results) <- STR_REGION_ALL

for(STR_REGION in STR_REGION_ALL){
  ## load file
  load(sprintf('./%s/sa_%s_multispec.RData', STR_RESULTS_DIR, STR_REGION))
  
  ## get VIF results from s6 specification (fully controlled) with no intercept
  cur.VIF <- s6$VIF[str.col.main[-1]]
  
  lst.results[[STR_REGION]] <- data.table(reg = rep(STR_REGION, length(cur.VIF)), 
                                          lab = str.pretty.labels[names(cur.VIF)], 
                                          val = cur.VIF)
}

# make into data table
df <- do.call(rbind, lst.results)
df$lab <- factor(df$lab, ordered = TRUE, levels = rev(str.pretty.labels[-1]))


p <- ggplot(df, aes(x = reg, y = lab, fill=val)) + 
        geom_tile(color = "black") + coord_fixed() + 
        geom_text(aes(label=sprintf('%0.2f', val)), size = 3) +
        #scale_fill_distiller(palette = "YlOrBr", 
        #                     guide="legend", name = "VIF", limits = c(1, 2)) +
        scale_fill_viridis_c(limits = c(1, 2), guide = "legend", name = "VIF") + 
        xlab('Region') + ylab('Regressor') +
        scale_x_discrete(position = "top") + 
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text = element_text(size=10))
if(SAVE_TO_FILE){
  ggsave(filename = "fig_VIF.png", plot = p, 
         width = 7, height = 3, units = "in", dpi = 320)
} else {
  plot(p)
}
