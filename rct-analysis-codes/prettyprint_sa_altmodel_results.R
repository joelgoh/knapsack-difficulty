## prettyprint_sa_altmodel_results.R
##
## Prints results of alternative model SA into nice tables (EC Tables). 
##
## Version: Feb 22, 2024

rm(list = ls())
library('data.table')
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')
STR_POSTFIX_ALL <- c("notimeout_avg", "minitems", "maxitems",
                     "attempt7_avg", "attempt10_avg", "attempt13_avg", 
                     "recent_active_5_avg", "recent_active_10_avg", "recent_active_15_avg",
                     "total_active_30_avg", "total_active_60_avg", "total_active_90_avg")

## helper to get significance stars
pstar <- function(pvec){
  sapply(pvec, function(p){
    if(p < 0.001) return('***')
    else if (p < 0.01) return('**')
    else if (p < 0.05) return('*')
    else return('')
  })
}


str.col.main <- c('(Intercept)', 'wtval_corr_actual', 'sahni_complexity', 'opt_numitems', 
                  'min_sahni', 'max_sahni', 'greedyval_0_rel_opt_gap', 'PlayerAttempt')

str.pretty.labels <- c('Intercept',
                       '\\quad Correlation between value and weight vectors',
                       '\\quad Sahni complexity measure',
                       '\\quad Optimal number of items in knapsack',
                       '\\quad Minimum gap from Sahni heuristics', 
                       '\\quad Maximum gap from Sahni heuristics', 
                       '\\quad Gap from greedy value heuristic',
                       '\\quad Num. minigame attempts per player')

stats.pretty.labels <- c('R$^2$', 'Num. observations', 'Num. covariates', '$F$-statistic')



str.col.labels <- STR_REGION_ALL

## main results
out.df <- data.table(strLab = str.pretty.labels)

## Rsq and other stats
stats.df <- data.table(strLab = stats.pretty.labels)

## initialize
out.str <- ''
for(STR_POSTFIX in STR_POSTFIX_ALL){
  for(STR_REGION in STR_REGION_ALL){
    ## load file
    load(sprintf('./sa_%s_%s.RData', STR_REGION, STR_POSTFIX))
    
    ## model object
    s <- s6
    
    ## get model, read, and calculate pstars
    df <- as.data.table(s$ct[,], keep.rownames=TRUE)
    df <- df[, sigstar := pstar(`Pr(>|t|)`)]
    
    # ensure we only pull out just enough rows
    num.out.rows <- min(nrow(df), nrow(out.df))
    df <- df[1:num.out.rows,]
    out.df[1:num.out.rows, eval(STR_REGION) := sprintf('%0.3f$^{%s}$ (%0.3f)',
                                                       df[, `Estimate`], 
                                                       df[, `sigstar`], 
                                                       df[,`Std. Error`])]
    
    stats.df[, STR_REGION] = c(sprintf('%0.4f', s$Rsq), 
                               formatC(attr(s$ct, "nobs"), format="f", big.mark=",", digits=0),
                               sprintf('%d', dim(s$ct)[1]),
                               formatC(s$Fstat, format="f", big.mark=",", digits=1))
  }
  
  ## process by removing na
  out.df[is.na(out.df)] <- ''
  
  ##build final string
  out.str <- paste(out.str, sprintf("\n\nProcessing SA mode: %s\n---------------------------------\n", STR_POSTFIX))
  for(i in 1:nrow(out.df)){
    out.str <- paste(out.str, paste(out.df[i, ], collapse = " & "), r"(\\)", "\n")
    if(i == 1){
      out.str <- paste(out.str, r'(\\ \multicolumn{6}{l}{\textbf{Instance-level complexity measures:}} \\\\)', '\n')
    } else if (i == nrow(out.df)-1) {
      out.str <- paste(out.str, r'(\\ \multicolumn{6}{l}{\textbf{Measure of player learning:}} \\\\)', '\n')
    }
  }
  
  ## second part
  out.str <- paste(out.str, r'(\\ \textbf{Control for primary game player stats?} & Yes & Yes & Yes & Yes & Yes \\\\)', '\n')
  
  for(i in 1:nrow(stats.df)){
    out.str <- paste(out.str, paste(stats.df[i, ], collapse = " & "), r"(\\)", "\n")
  }
}
cat(out.str)