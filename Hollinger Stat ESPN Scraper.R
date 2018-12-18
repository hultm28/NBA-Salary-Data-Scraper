# load libs
library(tidyverse)
library(rvest)
library(reshape2)
library(stringr)

# empty vector to hold urls
urls <- vector(mode="character")

# function to paste year and num of pages as inputs into url appending results
pasteNBA <- function(year, pages) {
  # http://insider.espn.com/nba/hollinger/statistics/_/page/#/year/####
  
  # loop through pages, paste into string, append to preexisting urls vector
  for (i in 1:pages) {
    x <- paste("http://insider.espn.com/nba/hollinger/statistics/_/page/", i, "/year/", year, sep="")
    urls <- c(urls, x)
  }
 return(urls)
}

# create df to loop through yrs & pages
yearsPagesDf <- data.frame("year" = 2003:2018, "pages" = c(7,7,7,7,7,7,7,7,7,8,7,7,8,7,8,8))

for (i in 1:nrow(yearsPagesDf)) {
  urls <- pasteNBA(year = yearsPagesDf[i, 1], pages = yearsPagesDf[i, 2])
}


# create empty df to store scraped data
hollingerStats <- data.frame(
  year = numeric(),
  player = character(),
  team = character(),
  rank = numeric(),
  gp = numeric(),
  mpg = numeric(),
  `ts%` = numeric(),
  ast = numeric(),
  to = numeric(),
  usg = numeric(),
  orr = numeric(),
  drr = numeric(),
  rebr = numeric(),
  per = numeric(),
  va = numeric(),
  ewa = numeric(),
  stringsAsFactors = FALSE
)

# write function to scrape data
for (i in urls) {
  # read in data from urls as vector 
  playerData <- read_html(i) %>% html_nodes("tr td") %>% html_text()
  # read in year data from urls
  year <- read_html(i) %>% html_nodes("div h1") %>% html_text()
  # extract out the yr to append to end df
  year <- substr(year[1], 1, 4) %>% as.numeric()
  # omit unnecessary items from beginning of urls vector
  playerData <- playerData[-(1:15)]
  # restructure data as df
  playerDataDf <- matrix(playerData, ncol = 14, byrow = TRUE) %>% as.data.frame()
  # omit every 11th row as they are headers not player data, some pages have less than 11 so use if statement
  if (nrow(playerDataDf) > 11) {
    playerDataDf <- playerDataDf[-seq(11, nrow(playerDataDf), by=11),]
  }
  # split player column into player and team column
  playerDataDf <- playerDataDf %>% cbind(str_split_fixed(playerDataDf$V2, ",", 2))
  # drop redundant player/team col
  playerDataDf <- playerDataDf[ ,-2]
  # add year column
  playerDataDf <- playerDataDf %>% mutate(year=year)
  # impute missing rank data
  playerDataDf$V1 <- ifelse(is.na(playerDataDf$V1),
                            playerDataDf$V1+1, 
                            playerDataDf$V1)
  # rename columns to append to hollingerStats column
  names(playerDataDf) <- c("rank", "gp", "mpg", "ts%", "ast", "to",
                   "usg", "orr", "drr", "rebr", "per", "va",
                   "ewa", "player", "team", "year")
  # append to hollingster
  hollingerStats <- hollingerStats %>% rbind(playerDataDf)
  # suspend time to avoid url pointer error
  runif(1, 3.1, 5.12) %>% Sys.sleep()
  # close connections to avoid error
  closeAllConnections()
  # run garbage collector to empty urls
  gc()
  # print url iteration to track url
  print(i)
}

