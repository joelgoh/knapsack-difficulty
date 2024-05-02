## plot_learning_results_agg_fw.R
rm(list = ls())
library('data.table')
library('ggplot2')
library('gridExtra')
library('ggpubr')
library('latex2exp')

## nice palette: https://coolors.co/134074-13315c-0b2545-8da9c4-eef4ed

STR_RESULTS_DIR <- '20240408_results'
SAVE_TO_FILE <- TRUE
GEOM_PT_SIZE <- 2

##STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')
STR_REGION_ALL <- c('E')
HEATMAP_COLORS <- c("#EEF4ED","#8DA9C4", "#134074", "#0B2545")

cat(sprintf('\n####################\n#'))
cat(sprintf('\n# LEARNING RESULTS:'))
cat(sprintf('\n#\n####################\n'))

lst.all.plots <- vector("list", 2)
lst.all.sigplots <- vector("list", 2)
lst.all.heatmaps <- vector("list", 2) 
lst.df.plots <- list()
for (START_IND in 3:8){
  str_infile <- sprintf('./%s/full_learn_bydiff_allreg_s%d.RData', STR_RESULTS_DIR, START_IND)
  load(str_infile)
  
  ## extract from list
  lst.s1 <- all.lst.s1.learn
  lst.s2 <- all.lst.s2.learn
  
  ## Print region
  cat(sprintf('\nAggregated Regions. START_IND = WIN_LENGTH = %d :\n----------------------------------\n', START_IND))
  
  
  ## extract coefficients and p_values from list
  df_s1_results <- data.table(tau = tau_seq, 
                              est = sapply(lst.s1, FUN = function(s) s['learn_efficiency','Estimate']),
                              pval = sapply(lst.s1, FUN = function(s) s['learn_efficiency','Pr(>|z|)']))
  
  df_s2_results <- data.table(tau = tau_seq, 
                              est = sapply(lst.s2, FUN = function(s) s['learn_efficiency','Estimate']),
                              pval = sapply(lst.s2, FUN = function(s) s['learn_efficiency','Pr(>|z|)']))
  
  
  ## PREPARE DATA for plotting
  df_s2_results[, is_est_neg := est<0][,is_est_signeg := is_est_neg & pval<0.05][,is_est_sigpos := !is_est_neg & pval<0.05]
  df_s2_results[, perc_est_neg := 100*cumsum(is_est_neg)/(tau - min(tau)+1)][,perc_est_signeg := 100*cumsum(is_est_signeg)/(tau - min(tau)+1)]
  df_s2_plot <- df_s2_results[, .(tau, perc_est_neg, perc_est_signeg, 
                                  -(2*is_est_neg - 1 + is_est_signeg - is_est_sigpos))]
  df_s2_plot[, end_window := tau][, tau := NULL]
  df_s2_plot <- melt(df_s2_plot, id.vars = "end_window", variable.name = "plot.type", value.name = "perc")
  levels(df_s2_plot$plot.type) <- c('Positive coefficient', 'Significantly positive coefficient', 'bool_signeg')
  df_s2_plot[,winlength := START_IND]
  lst.df.plots <- append(lst.df.plots, list(df_s2_plot))
}
df_s2_plot <- do.call(rbind, lst.df.plots)
df_s2_plot[, winlength := factor(winlength, ordered=TRUE)]


# make plot
p <- ggplot(data = df_s2_plot[plot.type == 'Positive coefficient',], 
            aes(x = end_window, y = perc, color = winlength, shape = winlength)) +
  geom_line(linewidth = 1) +
  geom_point(size = GEOM_PT_SIZE) +
  labs(title = "Proportion of runs in which ``average difficulty rating while learning'' positively affects short-term solution quality ",
       color="Learning window length", shape="Learning window length") +
  xlab("T") + ylab("Proportion  (%)") + ylim(0, 100) +
  theme_bw(base_size = 8) 
lst.all.plots[[1]] <- p

# make plot
p <- ggplot(data = df_s2_plot[plot.type == 'Significantly positive coefficient',], 
            aes(x = end_window, y = perc, color = winlength, shape = winlength)) +
  geom_line(linewidth = 1) +
  geom_point(size = GEOM_PT_SIZE) +
  labs(title = "Proportion of runs in which ``average difficulty rating while learning'' positively affects short-term solution quality",
       color="Learning window length", shape="Learning window length") +
  xlab("T") + ylab("Proportion  (%)") + ylim(0, 100) +
  theme_bw(base_size = 8)
lst.all.sigplots[[1]] <- p

## heatmap plot
df_heatmap <- df_s2_plot[plot.type == "bool_signeg"]
df_heatmap[, Result := factor(perc, levels = c(-2, -1, 1, 2))][,perc := NULL]
levels(df_heatmap$Result) <- c("Positive and significant", "Positive but insignificant", "Negative but insignificant", "Negative and significant")
p <- ggplot(data = df_heatmap, 
            aes(y = winlength, x = end_window, fill=Result)) + 
  geom_tile(color = "black") + coord_fixed() + 
  labs(title = "Relationship between average difficulty rating while learning and short-term solution quality") + 
  xlab(TeX('Last learning attempt, $T$')) + ylab(TeX('Length of learning window, $\\tau$')) + 
  scale_fill_manual(values=HEATMAP_COLORS) + 
  scale_y_discrete(limits=rev) + 
  scale_x_discrete(limits = factor(seq(1, 25))) + 
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
lst.all.heatmaps[[1]] <- p

## RESET 

cat(sprintf('\n########################\n#'))
cat(sprintf('\n# FUTURE LEARNING RESULTS:'))
cat(sprintf('\n#\n########################\n'))
lst.df.plots <- list()
for (START_IND in 3:8){
  str_infile <- sprintf('./%s/full_learn_bydiff_allreg_s%d.RData', STR_RESULTS_DIR, START_IND)
  load(str_infile)
  
  ## extract from list
  lst.s1 <- all.lst.s1.futurelearn
  lst.s2 <- all.lst.s2.futurelearn
  
  ## Print region
  cat(sprintf('\nAggreated Regions. START_IND = WIN_LENGTH = %d :\n----------------------------------\n', START_IND))
  
  
  ## extract coefficients and p_values from list
  df_s1_results <- data.table(tau = tau_seq, 
                              est = sapply(lst.s1, FUN = function(s) s['learn_efficiency','Estimate']),
                              pval = sapply(lst.s1, FUN = function(s) s['learn_efficiency','Pr(>|z|)']))
  
  df_s2_results <- data.table(tau = tau_seq, 
                              est = sapply(lst.s2, FUN = function(s) s['learn_efficiency','Estimate']),
                              pval = sapply(lst.s2, FUN = function(s) s['learn_efficiency','Pr(>|z|)']))
  
  
  ## PREPARE DATA for plotting
  df_s2_results[, is_est_neg := est<0][,is_est_signeg := is_est_neg & pval<0.05][,is_est_sigpos := !is_est_neg & pval<0.05]
  df_s2_results[, perc_est_neg := 100*cumsum(is_est_neg)/(tau - min(tau)+1)][,perc_est_signeg := 100*cumsum(is_est_signeg)/(tau - min(tau)+1)]
  df_s2_plot <- df_s2_results[, .(tau, perc_est_neg, perc_est_signeg, 
                                  -(2*is_est_neg - 1 + is_est_signeg - is_est_sigpos))]
  df_s2_plot[, end_window := tau][, tau := NULL]
  df_s2_plot <- melt(df_s2_plot, id.vars = "end_window", variable.name = "plot.type", value.name = "perc")
  levels(df_s2_plot$plot.type) <- c('Positive coefficient', 'Significantly positive coefficient', 'bool_signeg')
  
  df_s2_plot[,winlength := START_IND]
  lst.df.plots <- append(lst.df.plots, list(df_s2_plot))
}
df_s2_plot <- do.call(rbind, lst.df.plots)
df_s2_plot[, winlength := as.factor(winlength)]


# make plot
p <- ggplot(data = df_s2_plot[plot.type == 'Positive coefficient',], 
            aes(x = end_window, y = perc, color = winlength, shape = winlength)) +
  geom_line(linewidth = 1) +
  geom_point(size = GEOM_PT_SIZE) +
  labs(title = "Proportion of runs in which ``average difficulty rating while learning'' positively affects long-term solution quality",
       color="Learning window length", shape="Learning window length") +
  xlab("T") + ylab("Proportion  (%)") + ylim(0, 100) +
  theme_bw(base_size = 8)
lst.all.plots[[2]] <- p

# make plot
p <- ggplot(data = df_s2_plot[plot.type == 'Significantly positive coefficient',], 
            aes(x = end_window, y = perc, color = winlength, shape = winlength)) +
  geom_line(linewidth = 1) +
  geom_point(size = GEOM_PT_SIZE) +
  labs(title = TeX("Proportion of runs in which ``average difficulty rating while learning'' positively affects long-term solution quality"),
       color="Learning window length", shape="Learning window length") +
  xlab("T") + ylab("Proportion  (%)") + ylim(0, 100) +
  theme_bw(base_size = 8)
lst.all.sigplots[[2]] <- p
  

## heatmap plot
df_heatmap <- df_s2_plot[plot.type == "bool_signeg"]
df_heatmap[, Result := factor(perc, levels = c(-2, -1, 1, 2))][,perc := NULL]
levels(df_heatmap$Result) <- c("Positive and significant", "Positive but insignificant", "Negative but insignificant", "Negative and significant")
p <- ggplot(data = df_heatmap, 
            aes(y = winlength, x = end_window, fill=Result)) + 
  geom_tile(color = "black") + coord_fixed() + 
  labs(title = "Relationship between average difficulty rating while learning and long-term solution quality") + 
  xlab(TeX('Last learning attempt, $T$')) + ylab(TeX('Length of learning window, $\\tau$')) + 
  scale_fill_manual(values=HEATMAP_COLORS) +
  scale_y_discrete(limits=rev) + 
  scale_x_discrete(limits = factor(seq(1, 25))) + 
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
lst.all.heatmaps[[2]] <- p

# cat(sprintf('\n####################\n#'))
# cat(sprintf('\n# RETENTION RESULTS:'))
# cat(sprintf('\n#\n####################\n'))
# 
# lst.df.plots <- list()
# for (START_IND in 3:8){
#   str_infile <- sprintf('./%s/full_learn_bydiff_allreg_s%d.RData', STR_RESULTS_DIR, START_IND)
#   load(str_infile)
# 
#   ## extract from list
#   lst.s1 <- all.lst.s1.retain
#   lst.s2 <- all.lst.s2.retain
# 
#   ## Print region
#   cat(sprintf('\nAggregated Regions. START_IND = WIN_LENGTH = %d :\n----------------------------------\n', START_IND))
# 
#   ## extract coefficients and p_values from list
#   df_s1_results <- data.table(tau = tau_seq,
#                               est = sapply(lst.s1, FUN = function(s) s['learn_efficiency','Estimate']),
#                               pval = sapply(lst.s1, FUN = function(s) s['learn_efficiency','Pr(>|z|)']))
# 
#   df_s2_results <- data.table(tau = tau_seq,
#                               est = sapply(lst.s2, FUN = function(s) s['learn_efficiency','Estimate']),
#                               pval = sapply(lst.s2, FUN = function(s) s['learn_efficiency','Pr(>|z|)']))
# 
#   ## PREPARE DATA for plotting
#   df_s2_results[, is_est_neg := est<0][,is_est_signeg := is_est_neg & pval<0.05][,is_est_sigpos := !is_est_neg & pval<0.05]
#   df_s2_results[, perc_est_pos := 100*cumsum(!is_est_neg)/(tau - min(tau)+1)][,perc_est_sigpos := 100*cumsum(is_est_sigpos)/(tau - min(tau)+1)]
#   df_s2_plot <- df_s2_results[, .(tau, perc_est_pos, perc_est_sigpos,
#                                   -(2*is_est_neg - 1 + is_est_signeg - is_est_sigpos))]
# 
#   df_s2_plot[, end_window := tau][, tau := NULL]
#   df_s2_plot <- melt(df_s2_plot, id.vars = "end_window", variable.name = "plot.type", value.name = "perc")
#   levels(df_s2_plot$plot.type) <- c('Negative coefficient', 'Significantly negative coefficient', 'bool_signeg')
#   df_s2_plot[,winlength := START_IND]
#   lst.df.plots <- append(lst.df.plots, list(df_s2_plot))
# }
# df_s2_plot <- do.call(rbind, lst.df.plots)
# df_s2_plot[, winlength := as.factor(winlength)]
# 
# 
# # make plot
# p <- ggplot(data = df_s2_plot[plot.type == 'Negative coefficient',],
#             aes(x = end_window, y = perc, color = winlength, shape = winlength)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 3) +
#   xlab(TeX("Index of last attempt of learning window")) + ylab("Proportion  (%)") +
#   labs(title = TeX("Proportion of runs in which ``average difficulty rating while learning'' negatively affects number of remaining attempts"),
#        color="Learning window length", shape="Learning window length") +
#   theme_bw(base_size = 8)
# lst.all.plots[[3]] <- p
# 
# # make plot
# p <- ggplot(data = df_s2_plot[plot.type == 'Significantly negative coefficient',],
#             aes(x = end_window, y = perc, color = winlength, shape = winlength)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 3) +
#   xlab(TeX("Index of last attempt of learning window")) + ylab("Proportion  (%)") +
#   labs(title = TeX("Proportion of runs in which ``average difficulty rating while learning'' negatively affects number of remaining attempts"),
#        color="Learning window length", shape="Learning window length") +
#   theme_bw(base_size = 8)
# lst.all.sigplots[[3]] <- p
# 
# ## heatmap plot
# df_heatmap <- df_s2_plot[plot.type == "bool_signeg"]
# df_heatmap[, Result := factor(perc, levels = c(-2, -1, 1, 2))][,perc := NULL]
# levels(df_heatmap$Result) <- c("Positive and significant", "Positive but insignificant", "Negative but insignificant", "Negative and significant")
# p <- ggplot(data = df_heatmap,
#             aes(y = winlength, x = end_window, fill=Result)) +
#   geom_tile(color = "black") + coord_fixed() +
#   labs(title = "Relationship between average difficulty rating while learning and retention") +
#   xlab(TeX('Last learning attempt, $T$')) + ylab(TeX('Length of learning window, $\\tau$')) +
#   scale_fill_manual(values=HEATMAP_COLORS) +
#   scale_y_discrete(limits=rev) +
#   scale_x_discrete(limits = factor(seq(1, 25))) +
#   theme(panel.background = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank())
# plot(p)

## either plot all or save to plot
if(SAVE_TO_FILE){
  p <- ggarrange(plotlist = lst.all.heatmaps, nrow=2, ncol=1, common.legend = TRUE, legend = "bottom") 
  ggsave('./fig_learning_all_fw.png', 
         width = 10, height = 6, units = "in", dpi = 320,
         plot = p)
}
lapply(lst.all.heatmaps, plot)




