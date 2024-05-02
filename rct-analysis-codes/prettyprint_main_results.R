## prettyprint_main_results.R
##
## Prints main results into a nice table (Table 3 of paper). 
##
## Version: Feb 22, 2024

rm(list = ls())
library('data.table')
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')

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

## main results
out.df <- data.table(strLab = str.pretty.labels)

## Rsq and other stats
stats.df <- data.table(strLab = stats.pretty.labels)
  
for(STR_REGION in STR_REGION_ALL){
  ## load file
  load(sprintf('./sa_%s_multispec.RData', STR_REGION))
  
  ## get results from s6 specification (fully controlled)
  df <- as.data.table(s6$ct[,], keep.rownames=TRUE)
  
  ## make stars
  df <- df[1:length(str.pretty.labels), ][, sigstar := pstar(`Pr(>|t|)`)]
  
  ## build output 
  out.df[, eval(STR_REGION) := sprintf('%0.3f$^{%s}$ (%0.3f)',df[, `Estimate`], df[, `sigstar`], df[,`Std. Error`])]
  
  stats.df[, eval(STR_REGION)] = c(sprintf('%0.4f', s6$Rsq), 
                                   formatC(attr(s6$ct, "nobs"), format="f", big.mark=",", digits=0),
                                   sprintf('%d', dim(s6$ct)[1]),
                                   formatC(s6$Fstat, format="f", big.mark=",", digits=1))
  
}

##build final string
out.str <- ''
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
cat(out.str)


