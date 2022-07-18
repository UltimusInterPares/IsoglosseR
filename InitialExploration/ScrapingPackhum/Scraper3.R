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
  # text
  # ig_book
  # ig_no
  
### PACKAGES -------------------------------------------------------------------
library(dplyr)
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
}
  

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

MakeEntry <- function(ig_book, ig_no, phi_no, header, text) {
  entry <- c(ig_book, ig_no, phi_no, header, text)
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
  
  header <- ReadHeader(page)
  header <- CleanHeader(header)
  
  entry <- MakeEntry(ig_book, ig_no, phi_no, header, text)
  return(entry)
}

### THE TIBBLE -----------------------------------------------------------------
test_df <- tibble(ig_book = NA, ig_no = NA, phi_no = NA, header = NA, text = NA)

### TESTING --------------------------------------------------------------------
scrape_total <- 10

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
