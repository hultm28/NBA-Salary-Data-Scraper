# load libs
library(tidyverse)
library(rvest)
library(reshape2)
library(stringr)

# empty vector to hold urls
urls <- vector(mode="character")

# the first page of each yr is different from the other pages...
# must be a disitnct url from the other yrs
for (i in 2003:2018)  {
  tmp <- paste("http://www.espn.com/nba/salaries/_/year/", i, sep="")
  urls <- c(urls, tmp)
}

# create df for additional yrs & pages
tmpDf <- data.frame(
  year = 2003:2018,
  pages = c(7,5,3,11,11,11,11,11,12,13,14,10,11,11,12,12)
)

pasteNBA <- function(year, pages) {
  # http://www.espn.com/nba/salaries/_/year/####/page/#
  
  # loop through pages, paste into string, append to preexisting urls vector
  for (i in 2:pages) {
    tmp <- paste("http://www.espn.com/nba/salaries/_/year/", year, "/page/", i, sep="")
    print(tmp)
    urls <- c(urls, tmp)
  }
  return(urls)
}

# loop through df and append apprpriate urls to urls
for (i in 1:nrow(tmpDf)) {
  urls <- pasteNBA(year = tmpDf[i, 1], pages = tmpDf[i, 2])
}

###### scrape data

# create empty data frame
playerSalaryDf <- data.frame(
  rank = numeric(),
  team = character(),
  salary = numeric(),
  season = character(),
  player = character(),
  position = character(),
  stringsAsFactors = FALSE
)

for (i in urls[152:length(urls)]) {
  # read url player salary data in
  tmp <- i %>% read_html() %>% html_nodes("tr td") %>% html_text()
  # read url in for the year
  yr <- i %>% read_html() %>% html_nodes("div h2") %>% html_text()
  # convert data into matrix
  tmp <- matrix(tmp, ncol=4, byrow=TRUE) %>% as.data.frame()
  # delete every 12th row
  if (nrow(tmp) > 11) {
    tmp <- tmp[-seq(1, nrow(tmp), by=11),]
  }
  # strip special characters from salary data
  tmp <- tmp
  tmp$V4 <- gsub("\\$|\\,", "", tmp$V4) %>% as.numeric()
  # seperate out position from player name and append
  tmp <- tmp %>% cbind(str_split_fixed(tmp$V2, ",", 2))
  # drop redundant variable
  tmp <- tmp[, -2]
  # pull out the year from the original string
  yr <- str_sub(yr, -9, -1)
  # append yr as new col
  tmp$year <- yr
  # rename cols
  names(tmp) <- c("rank", "team", "salary", "season", "player", "position")
  # append to prexisting df
  playerSalaryDf <- playerSalaryDf %>% rbind(tmp)
  # sleep
  Sys.sleep(1.12311)
  print(i)
}

# reorder data by year then rank
playerSalaryDf <- playerSalaryDf[order(playerSalaryDf$season, playerSalaryDf$rank), ]
# write out salary data as csv file
write.csv(playerSalaryDf, file="C:/Users/Mike/Desktop/NBASalaryData.csv")
