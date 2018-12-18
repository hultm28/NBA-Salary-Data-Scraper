# Objective: Scrape web data pertaining to grateful dead setlists

# import libraries
library(tidyverse)
library(rvest)
library(stringr)

# create charcter vector list to hold URLs
URLs <- vector(mode="character")

# create nested list object to hold all URLs of setlists for ea yr using for loop
for (i in seq(72,95,1)) {
  # set holder with concat URL name
  tmpURL <- paste("https://www.cs.cmu.edu/~./gdead/", i, ".html", sep="")
  # append tmpURL to pre existing songList vector
  URLs <- c(URLs, tmpURL)
}

# create empty dataframe to hold scraped data from URLs in songList
setListDf <- data.frame(
  setLocation = character(),
  city = character(),
  stateORcountry = character(),
  date = as.Date(character()),
  stringsAsFactors = FALSE
)

# loop through URLs, scrape data, append to preexisting df
# it will be done yr by yr with each yrs values appended iteratively
# for each yr html read, the cursor will read each obs as one long character vector
# regex operators will then be used to split this up and construct a df
for (i in URLs) {
  # read in current html info from 
  x <- read_html(i) %>% html_nodes("li a") %>% html_text

  # extract the location
  setLocationLoc <- regexpr("(\n.+?,)?|
                            (\n.+?\\()", x)
  setLocation <- regmatches(x, setLocationLoc) %>% str_sub(2, -2)
  
  # extract the city & clean
  cityLoc <- regexpr("([A-Z]?[a-z]?{1,}?\\.?\\s?[A-Z]{1}[a-z]{1,},\\s[A-Z]{2,})?|
                     ([A-Z]{1}[a-z]{1,}\\s[A-Z]{2})", x)
  city <- regmatches(x, cityLoc) %>% str_sub(1,-5) %>% trimws()
  
  # extract the state/country code
  stateLoc <- regexpr("(\\s[A-Z]{2,}\\s)?|
                      (,\\s.+?\\s\\()", x)
  state.con <- regmatches(x, stateLoc) %>% str_trim()

  # extract the date and convert to date type
  dateLoc <- regexpr("[0-9]{1,2}\\/[0-9]{1,2}\\/[0-9]{2}", x)
  date <- regmatches(x, dateLoc) %>% as.Date(format="%m/%d/%y")
  
  # create df of above vectors to append to preexisting df
  tmpDf <- data.frame(
    setLocation = setLocation,
    city = city,
    stateORcountry = state.con,
    date = date,
    stringsAsFactors = FALSE
    )
  
  # append tmpDf to setListDf
  setListDf <- rbind(setListDf, tmpDf)
} 
