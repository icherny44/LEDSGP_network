
library(visNetwork)
library(data.table)
library(stringr)

library(googlesheets)
library(RCurl)
library(httr)

# Fetch sheet registration from G-docs
if(!exists("gap_gsheet")){
  set_config( config( ssl_verifypeer = FALSE ) )
  sheet_url <- "https://docs.google.com/spreadsheets/d/1jKtc49RjMg9AtnSF89nAoGFzTKjGU628WrHfxH21dAs/edit?usp=sharing"
  gap_gsheet <- gs_url(sheet_url)
}

# Update sheet contents
gsheet <- data.table(gs_read(gap_gsheet),ws=1)

# Names of WGs and RPs
Names <- fread("Names.csv")