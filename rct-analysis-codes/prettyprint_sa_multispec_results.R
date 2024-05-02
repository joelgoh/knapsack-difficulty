## prettyprint_sa_multispec_results.R
##
## Prints results of multispec SA into nice tables (EC Tables). 
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



str.col.labels <- c('(a)', '(b)', '(c)', '(d)')
lst.s <- c("s2", "s4", "s5", "s6")
names(lst.s) <- str.col.labels
for(STR_REGION in STR_REGION_ALL){
  ## create separation
  cat(sprintf("\n\n RESULTS FOR REGION %s \n ----------------------- \n", STR_REGION))
  
  ## main results
  out.df <- data.table(strLab = str.pretty.labels)
  
  ## Rsq and other stats
  stats.df <- data.table(strLab = stats.pretty.labels)
  
  ## load file
  load(sprintf('./sa_%s_multispec.RData', STR_REGION))
  
  for(cur.col in str.col.labels){
    ## get model, read, and calcuate pstars
    s <- eval(parse(text = lst.s[[cur.col]]))
    df <- as.data.table(s$ct[,], keep.rownames=TRUE)
    df <- df[, sigstar := pstar(`Pr(>|t|)`)]
    
    # ensure we only pull out just enough rows
    num.out.rows <- min(nrow(df), nrow(out.df))
    df <- df[1:num.out.rows,]
    out.df[1:num.out.rows, eval(cur.col) := sprintf('%0.3f$^{%s}$ (%0.3f)',
                                                    df[, `Estimate`], 
                                                    df[, `sigstar`], 
                                                    df[,`Std. Error`])]
    
    stats.df[, eval(cur.col)] = c(sprintf('%0.4f', s$Rsq), 
                                  formatC(attr(s$ct, "nobs"), format="f", big.mark=",", digits=0),
                                  sprintf('%d', dim(s$ct)[1]),
                                  formatC(s$Fstat, format="f", big.mark=",", digits=1))
  }
  
  ## process by removing na
  out.df[is.na(out.df)] <- ''
  
  
  ##build final string
  out.str <- ''
  for(i in 1:nrow(out.df)){
    out.str <- paste(out.str, paste(out.df[i, ], collapse = " & "), r"(\\)", "\n")
    if(i == 1){
      out.str <- paste(out.str, r'(\\ \multicolumn{5}{l}{\textbf{Instance-level complexity measures:}} \\\\)', '\n')
    } else if (i == nrow(out.df)-1) {
      out.str <- paste(out.str, r'(\\ \multicolumn{5}{l}{\textbf{Measure of player learning:}} \\\\)', '\n')
    }
  }
  
  ## second part
  out.str <- paste(out.str, r'(\\ \textbf{Control for primary game player stats?} & No & No & No & Yes \\\\)', '\n')
  
  for(i in 1:nrow(stats.df)){
    out.str <- paste(out.str, paste(stats.df[i, ], collapse = " & "), r"(\\)", "\n")
  }
  cat(out.str)
}