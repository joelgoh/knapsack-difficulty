## process_playerchoice.R
rm(list = ls())
library('stringr')
library('data.table')

# select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')


for (STR_REGION in STR_REGION_ALL) {

    ## reporting
    cat(sprintf("Processing region %s\n", STR_REGION))
    
    ## Read and only retain a few columns
    start.time <- Sys.time()
    ##df <- readRDS(sprintf('./processed_webdata_%s.rds', STR_REGION))
    df <- fread(file = sprintf('../subset_data/web_data/raw_webdata_%s.csv', STR_REGION),
                select = c("MiniGameCode", "account_id", "DateTime", "ChoiceSequence_new"))
    ## df <- df[,.(MiniGameCode, account_id, DateTime, ChoiceSequence_new)]
    cat(sprintf("Read complete. Time taken = %0.2f minutes\n", difftime(Sys.time(), start.time, units='mins')))

    ## count numbers of each action
    start.time <- Sys.time()
    cat(sprintf('Counting ... \n'))
    for (ii in 1:12){
        df[, LETTERS[ii]:=str_count(ChoiceSequence_new, LETTERS[ii])]  
    }
    for (ii in 1:12){
        df[, letters[ii]:=str_count(ChoiceSequence_new, letters[ii])]  
    }
    cat(sprintf("Counting complete. Time taken = %0.2f minutes\n", difftime(Sys.time(), start.time, units='mins')))

    ## remove unneeded columns
    df[, ChoiceSequence_new := NULL]

    ## Processing counts
    start.time <- Sys.time()
    cat(sprintf('Processing counts ... \n'))
    
    ## count number of net selected
    df$num.net.selected <- rowSums(df[, LETTERS[1:12], with = FALSE]) 
                              - rowSums(df[, letters[1:12], with = FALSE])

    ## count number of unselects
    df$num.unselect <- rowSums(df[, letters[1:12], with = FALSE])

    cat(sprintf("First three process complete.\n")) 

   
    ## count total number of undos
    df$num.undo <- rowSums(as.data.table(pmax(as.matrix(df[, LETTERS[1:12], with = FALSE] - 1), 0)))

    ## count max number of undos
    df$max.undo <- apply(df[, LETTERS[1:12], with = FALSE], 1, max)-1
    df[num.net.selected == 0, max.undo := 0] # fix case with zero total selections
    cat(sprintf("Processing counts complete. Time taken = %0.2f minutes\n", difftime(Sys.time(), start.time, units='mins')))


    ## read main file
    cat(sprintf("Reading main file ... \n"))
    df.main <- fread(sprintf('../subset_data/web_data/raw_webdata_%s.csv', STR_REGION))
    cat(sprintf("Read complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
    
    ## merge and save to file
    cat(sprintf("Merging data tables ... \n"))
    setkey(df.main, MiniGameCode, account_id, DateTime)
    setkey(df, MiniGameCode, account_id, DateTime)
    df.main[df, `:=`(num.net.selected = i.num.net.selected,
                     num.unselect = i.num.unselect,
                     num.undo = i.num.undo,
                     max.undo = i.max.undo)]
    cat(sprintf("Merge complete. Time taken = %0.2f mins \n", difftime(Sys.time(), start.time, units = "mins")))
    df <- NULL

    ## filter out data from test accounts
    if(STR_REGION == "D"){
        cutoff <- as.POSIXct("2021-09-25 00:00:00", tz = "UTC")
        test_accts <- c("622e0664f354a12bdcf22ff142bdc661", "2f2770a1ac9d637eaed5d8fc613a9653", "4c656a0ee72d01679ece056166478b20")
        df.main <- df.main[!(account_id %in% test_accts) & DateTime > cutoff,]
    }
    if(STR_REGION %in% c("A", "B", "C")){
        cutoff <- as.POSIXct("2021-10-01 00:00:00", tz = "UTC")
        df.main <- df.main[DateTime > cutoff,]
    }
    if(STR_REGION == "E"){
        cutoff <- as.POSIXct("2021-10-04 06:00:00", tz = "UTC")
        df.main <- df.main[DateTime > cutoff,]
    }


    ## remove unncessary field
    df.main[, PalyerId := NULL] 
    
    ## save to file
    cat(sprintf('Saving file ... \n'))
    fwrite(x = df.main, file = sprintf('./webdata_%s.csv', STR_REGION))
    cat(sprintf("Saving file complete. Time taken = %0.2f minutes\n", difftime(Sys.time(), start.time, units='mins')))
}





