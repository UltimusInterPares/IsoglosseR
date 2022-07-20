### STYLE GUIDE ----------------------------------------------------------------
# 
# All function names are in CamelCase
# All variable names are lowercase_with_underscores
#
# Var.'s
  # phi_no
  # link
  # page
  # header
  # cities
  # date_after
  # date_before
  # text
  # ig_book
  # ig_no
  
### PACKAGES -------------------------------------------------------------------
library(dplyr)
library(stringr)
library(rvest)
library(ggplot2)
library(progress)
 
### DEFINING COMPONENTS FOR Scrape() -------------------------------------------
MakePage <- function(phi_no) {
  link <- paste('https://inscriptions.packhum.org/text/', phi_no, sep="")
  page <- read_html(link)
  return(page)
}

ReadHeader <- function(page) {
  header <- page %>% html_nodes('div.tildeinfo.light') %>% html_text()
  return(header)
}

CleanHeader <- function(header) {
  header <- gsub("[\\[\\]]", "", header, perl=T)
  header <- gsub("(\\-\\n{1,})", "", header, perl=T)
  header <- gsub("\\n{1,}", " ", header, perl=T)
  header <- gsub("\\s{1,}", " ", header, perl=T)
  # ̣COMBINING DOT BELOW Unicode: U+0323, UTF-8: CC A3
  header <- gsub("̣", "", header, perl=T)
  header <- gsub("#", "", header, perl=T)
  # ℎ PLANCK CONSTANT Unicode: U+210E, UTF-8: E2 84 8E
  header <- gsub("ℎ", "h", header, perl=T)
  
  # not bef. > after
  header <- gsub("not bef.", "after", header, perl=T)
  return(header)
}

cities <- "(Megara|Pagae|Aegosthena|Oropus|Tanagra|Eleusis|Athens)"

ReadLocation <- function(header) {
  header <- gsub("Ath.", "Athens", header, ignore.case = T, perl = T)
  location <- str_extract(header, cities)
  return(location)
}

################################################################################
# WIP

TranslateCentury <- function(header) {
  grepl("(\\d+)(?=\\w{2} c.)", header, perl = T)
  cen_start <- str_extract(header, "(\\d+)(?=\\w{2} c.)")
  cen_start <- paste(cen_start, "00", sep="") %>%
    as.integer()
  
  cen_end <- cen_start - 99
  
  if (grepl("(?<=early )(\\d+)(?=\\w{2} c.)", header, perl = T)) {
    header <- gsub("(?>early|mid|late) (\\d+)(?>\\w{2} c.)", cen_start, header, perl = T)
    return(header)
  } else if (grepl("(?<=late )(\\d+)(?=\\w{2} c.)", header, perl = T)) {
    cen_start <- cen_start - 99
    header <- gsub("(?>early|mid|late) (\\d+)(?>\\w{2} c.)", cen_start, header, perl = T)
    return(header)
    #return(cen_start)
  } else if (grepl("(?<=mid )(\\d+)(?=\\w{2} c.)", header, perl = T)) {
    cen_start <- cen_start - 50
    header <- gsub("(?>early|mid|late) (\\d+)(?>\\w{2} c.)", cen_start, header, perl = T)
    return(header)
    #return(cen_start)
  } else {
    cen <- paste(cen_start, cen_end, sep="-")
    return(cen)
  }
  
}

ReadDateAfter <- function(header) {
  if (grepl("\\d+-\\d+", header, perl = T)) {
    date_after <- str_extract(header, "\\d+(?=-)")
  } else if (grepl("(?<=after )\\d+", header, perl = T)) {
    date_after <- str_extract(header, "(?<=after )\\d+")
  }  else if (grepl("(?<=— )(\\d+)(?= BC)", header, perl = T)) {
    date_after <- str_extract(header, "(?<=— )(\\d+)(?= BC)")
  } else {date_after <- NA}
  return(date_after)
}

ReadDateBefore <- function(header) {
  if (grepl("\\d+-\\d+", header, perl = T)) {
    date_before <- str_extract(header, "(?<=-)\\d+")
  } else if (grepl("(?<=before )\\d+", header, perl = T)) {
    date_before <- str_extract(header, "(?<=before )\\d+")
  } else if (grepl("(?<=— )(\\d+)(?= BC)", header, perl = T)) {
    date_before <- str_extract(header, "(?<=— )(\\d+)(?= BC)")
  } else {date_before <- NA}
  return(date_before)
}

################################################################################

ReadText <- function(page) {
  text <- page %>% html_nodes('div.greek.text-nowrap.dblclk') %>% html_text()
  return(text)
}

Clean <- function(text) {
  text <- gsub("[\\d\\[\\]]", "", text, perl=T)
  text <- gsub("(\\-\\n{1,})", "", text, perl=T)
  text <- gsub("\\n{1,}", " ", text, perl=T)
  text <- gsub("\\s{1,}", " ", text, perl=T)
  # ̣COMBINING DOT BELOW Unicode: U+0323, UTF-8: CC A3
  text <- gsub("̣", "", text, perl=T)
  text <- gsub("#", "", text, perl=T)
  # ℎ PLANCK CONSTANT Unicode: U+210E, UTF-8: E2 84 8E
  text <- gsub("ℎ", "h", text, perl=T)
}

ReadBook <- function(page) {
  ig_book <- page %>% html_nodes("span.fullref > a") %>% html_text()
  ig_book <- gsub("\\n", "", ig_book)
  return(ig_book)
}

ReadNo <- function(page) {
  ig_no <- page %>% html_nodes("span.fullref > span") %>% html_text()
  #ig_no < gsub("\\n", "", ig_no)
}

MakeEntry <- function(ig_book, ig_no, phi_no, header, location, date_after, date_before, text) {
  entry <- c(ig_book, ig_no, phi_no, header, location, date_after, date_before, text)
  return(as.list(entry))
}

### Scrape() -------------------------------------------------------------------
ScrapeTest <- function(phi_no=1) {
  # Make page for scraping
  page <- MakePage(phi_no)
  
  # Scrape relevant data
  text <- ReadText(page)
  text <- Clean(text)
  
  # Scrape identifying materials
  ig_book <- ReadBook(page)
  ig_no <- ReadNo(page)
  
  # Location and Dates
  header <- ReadHeader(page)
  header <- CleanHeader(header)
  #header <- TranslateCentury(header)
  location <- ReadLocation(header)
  date_after <- ReadDateAfter(header)
  date_before <- ReadDateBefore(header)
  
  
  entry <- MakeEntry(ig_book,
                     ig_no,
                     phi_no,
                     header,
                     location,
                     date_after,
                     date_before,
                     text)
  return(entry)
}

### THE TIBBLE -----------------------------------------------------------------
test_df <- tibble(ig_book = NA,
                  ig_no = NA,
                  phi_no = NA,
                  header = NA,
                  location = NA,
                  date_after = NA,
                  date_before = NA,
                  text = NA)

### TESTING --------------------------------------------------------------------
scrape_total <- 50

pb <- pb <- progress_bar$new(
  format = "  scraping PackHum [:bar] :percent eta: :eta (:spin)",
  total = scrape_total, clear = FALSE, width= 60)

start_time <- Sys.time()
for (i in c(1:scrape_total)) {
  test_df[i,] <- ScrapeTest(phi_no=(i+143476))
  pb$tick()
  end_time <- Sys.time()
}

pb$terminate()

print(end_time-start_time)
