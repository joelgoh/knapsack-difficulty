## process_playerstats.R
rm(list = ls())
library('data.table')

# select region
STR_REGION_ALL <- c('A', 'B', 'C', 'D', 'E')


for (STR_REGION in STR_REGION_ALL) {

    ## reporting
    cat(sprintf("Processing region %s\n", STR_REGION))
    
    ## Read webdata file and only retain a few columns
    start.time <- Sys.time()
    cat(sprintf("Reading web file ... \n"))
    df.web <- fread(file = sprintf('../subset_data/web_data/raw_webdata_%s.csv', STR_REGION), select = "account_id")
    cat(sprintf("Read complete. Time taken = %0.2f minutes\n", difftime(Sys.time(), start.time, units='mins')))
    df.web <- unique(df.web, by = "account_id")
    setkey(df.web, account_id)

    ## filter out data from test accounts
    if(STR_REGION == "SG"){
        test_accts <- c("622e0664f354a12bdcf22ff142bdc661", "2f2770a1ac9d637eaed5d8fc613a9653", "4c656a0ee72d01679ece056166478b20")
        df.web <- df.web[!(account_id %in% test_accts),]
    }


    ## Read ingame player stats file 
    cat(sprintf("Reading player stats file ... \n"))
    df.ingame <- fread(file = sprintf('../subset_data/ingame_data/ingame_%s.csv', STR_REGION))
    cat(sprintf("Read complete. Time taken = %0.2f minutes\n", difftime(Sys.time(), start.time, units='mins')))
    setkey(df.ingame, account_id)

    ## Perform inner join
    df <- df.web[df.ingame, nomatch=0]

    ## remove unncessary fields
    df[, last_active_brand := NULL]
    df[, register_ts := NULL]
    df[, p := NULL]

    ## save to file
    cat(sprintf('Saving file playerstats_%s.csv ... \n', STR_REGION))
    fwrite(x = df, file = sprintf ('./playerstats_%s.csv', STR_REGION))
    cat(sprintf("Saving file complete. Time taken = %0.2f minutes\n", difftime(Sys.time(), start.time, units='mins')))
}





