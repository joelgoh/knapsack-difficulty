## plot_vif_all_Sahnis.R
##
## plots VIF of main results for extension with all Sahni in place
## In response to R4
##
## Version: Apr 14, 2025

rm(list = ls())
library('data.table')
library('ggplot2')

SAVE_TO_FILE <- TRUE
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')

str.col.main <- c('(Intercept)', 'wtval_corr_actual', 'sahni_complexity', 'opt_numitems', 
                  'sahni_order_0_rel_opt_gap', 
                  'sahni_order_1_rel_opt_gap', 
                  'sahni_order_2_rel_opt_gap', 
                  'sahni_order_3_rel_opt_gap',
                  'sahni_order_4_rel_opt_gap', 
                  'greedyval_0_rel_opt_gap', 'PlayerAttempt')
str.pretty.labels <- c('Intercept',
                       'Correlation between value and weight vectors',
                       'Sahni complexity measure',
                       'Optimal number of items in knapsack',
                       'Gap from Sahni-0 heuristic', 
                       'Gap from Sahni-1 heuristic', 
                       'Gap from Sahni-2 heuristic', 
                       'Gap from Sahni-3 heuristic', 
                       'Gap from Sahni-4 heuristic', 
                       'Gap from greedy value heuristic',
                       'Num. minigame attempts per player')
names(str.pretty.labels) <- str.col.main

## main results
lst.results <- vector("list", length(STR_REGION_ALL))
names(lst.results) <- STR_REGION_ALL

for(STR_REGION in STR_REGION_ALL){
  ## load file
  load(sprintf('./20250414_results/lmoutput_%s_optval_all_Sahnis.RData', STR_REGION))
  
  ## get VIF results from specification with no intercept
  cur.VIF <- attr(s1, "VIF")
  
  lst.results[[STR_REGION]] <- data.table(reg = rep(STR_REGION, length(str.col.main[-1])), 
                                          lab = str.pretty.labels[-1], 
                                          val = cur.VIF[str.col.main[-1]])
}

# make into data table
df <- do.call(rbind, lst.results)
df$lab <- factor(df$lab, ordered = TRUE, levels = rev(str.pretty.labels[-1]))


p <- ggplot(df, aes(x = reg, y = lab, fill=val)) + 
        geom_tile(color = "black") + coord_fixed() + 
        geom_text(aes(label=sprintf('%0.2f', val)), size = 3) +
        #scale_fill_distiller(palette = "YlOrBr", 
        #                     guide="legend", name = "VIF", limits = c(1, 5)) + 
        #labs(fill = "VIF") +
        scale_fill_viridis_c(limits = c(1, 5), guide = "legend", name = "VIF") + 
        xlab('Region') + ylab('Regressor') +
        scale_x_discrete(position = "top") + 
        theme(panel.background = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(), 
              axis.text = element_text(size=10))
if(SAVE_TO_FILE){
  ggsave(filename = "fig_VIF_all_Sahnis.png", plot = p, 
         width = 7, height = 4, units = "in", dpi = 320)
} else {
  plot(p)
}
