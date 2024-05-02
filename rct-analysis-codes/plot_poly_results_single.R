## prettyprint_poly_results_single.R
rm(list = ls())
library('data.table')
library('biglm')
library('lmtest')
library('ggplot2')
library('gridExtra')

SAVE_TO_FILE <- TRUE
LM4_SELECT <- TRUE
POLY_DEGREE <- 4
N_PTS <- 20
STR_REGION <- 'A'


predict.rob <- function(cur.form, ct, clcov, newdata){
  ##if(missing(newdata)){ newdata <- x$model }
  m.mat <- model.matrix(terms(cur.form),data=newdata)
  m.coef <- coef(ct)
  fit <- as.vector(m.mat %*% m.coef)
  se.fit <- sqrt(diag(m.mat %*% clcov %*% t(m.mat)))
  return(list(fit=fit,se.fit=se.fit))
}

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



load(sprintf('./sa_%s_poly%d_lm_clust.RData', STR_REGION, POLY_DEGREE))
load(sprintf('./reg_%s_poly_sumstats.RData', STR_REGION))

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


## choose
if(LM4_SELECT){
  cur.form <- f4
  str.cur.lm <- "lm4_orig"
  str.all.cols <- str.col.main
} else {
  cur.form <- f6
  str.cur.lm <- "lm6_orig"
  str.all.cols <- c(str.col.main, str.col.aux)
}

lst.plots <- vector('list', length(str.col.main))
names(lst.plots) <- str.col.main

for(cur.str.col in str.col.main){
  ## build base columns
  df.new <- as.data.frame(sum.stats[rep("mean", N_PTS),str.all.cols])
  
  ## target column
  x_min <- sum.stats["min",cur.str.col]
  x_max <- sum.stats["max",cur.str.col]
  x_var <- seq(x_min, x_max, length.out = N_PTS)
  df.new[, cur.str.col] <- x_var
  
  # new column for results
  df.new$sol_quality <- 0
  
  ## call helper to get point est and clustered covar matrix
  m.mod <- eval(sym(str.cur.lm))
  ll <- predict.rob(cur.form, m.mod$ct, m.mod$vcov, df.new)
  
  ## build point est and CI
  y_pred <- ll$fit
  y_se <- ll$se.fit
  y_ci_low <- y_pred + qnorm(0.025) * y_se
  y_ci_high <- y_pred + qnorm(0.975) * y_se
  
  # plotting data table
  df.plot = data.table(x_var = x_var,
                       y_pred = y_pred, y_se = y_se, 
                       y_ci_low = y_ci_low, y_ci_high = y_ci_high)
  
  # make plot
  p <- ggplot(data = df.plot,
              aes(x = x_var, y = y_pred, ymin = y_ci_low, ymax = y_ci_high)) +
          geom_line(linewidth = 1, color = "blue") +
          geom_ribbon(alpha = 0.2, fill = "blue") +
          xlab(str.xlabels[cur.str.col]) + ylab("Solution Quality") + theme_bw(base_size = 8)
          
  
  if(SAVE_TO_FILE){
    lst.plots[[cur.str.col]] <- p
  } else {
    plot(p)  
  }
}

if(SAVE_TO_FILE){
  ggsave(sprintf('./fig_reg_%s_poly%d_%s_with_ci.png', 
                 STR_REGION, POLY_DEGREE, substr(str.cur.lm,1,3)),
         width = 8, height = 6, units = "in", dpi = 320,
         marrangeGrob(grobs=lst.plots, 
                      nrow = 3, ncol = 2,top = NULL,
                      layout_matrix = matrix(1:6, 3, 2, TRUE)))
}