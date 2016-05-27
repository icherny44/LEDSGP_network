
library(visNetwork)
library(data.table)
library(stringr)

library(googlesheets)
library(RCurl)
library(httr)

set_config( config( ssl_verifypeer = FALSE ) )

# Fetch sheet registration from G-docs
if(!exists("gap_gsheet")){
  sheet_url <- "https://docs.google.com/spreadsheets/d/1jKtc49RjMg9AtnSF89nAoGFzTKjGU628WrHfxH21dAs"
  gap_gsheet <- gs_url(sheet_url)
}

# Update sheet contents
gsheet <- data.table(gs_read(gap_gsheet, ws=1))

gsheet_real <- data.table(gs_read(gap_gsheet, ws=2))

# Names of WGs and RPs
Names <- fread("Names.csv")