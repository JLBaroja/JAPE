# Extracts data and builds different human-friendly datasets.
# Name of script, and of main .RData object, honors Dorothy Johnson Vaughan 1910-2008

rm(list=ls())
concurrent_data_folder <- '~/Documents/Luis/JAPE/ConcurrentData'
setwd(concurrent_data_folder)
japede_full <- read.csv('concurrent_full.csv',stringsAsFactors = F)

birdz <- sort(unique(japede_full$bird))
sessionz <- sort(unique(japede_full$session))

brd <- birdz[4]
# for(brd in birdz){
b_data <- subset(japede_full,bird==brd)
sssn <- sessionz[3]
# for(sssn in sessionz){
s_data <- subset(b_data,session==sssn)


reinf_in_session <- seq(1,sum(c(sum(s_data$event=='feeder_on_left'),sum(s_data$event=='feeder_on_right'))))





reinforcers <- data.frame(bird=brd,
                          session=sssn,
                          reinf_in_session)


# }}