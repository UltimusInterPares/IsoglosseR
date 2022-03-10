library(dplyr)
library(rvest)

### GRAB PAGE ------------------------------------------------------------------
link <- 'https://inscriptions.packhum.org/text/1825'
page <- read_html(link)

### GRAB TEXT REF --------------------------------------------------------------
#textpage > div.hdr2 > span.fullref > a
IGbook <- page %>% html_nodes("span.fullref > a") %>% html_text()
IGbook <- gsub("\\n", "", IGbook)

#textpage > div.hdr2 > span.fullref > span  
IGno <- page %>% html_nodes("span.fullref > span") %>% html_text()
IGno <- gsub("\\n", "", IGno)

### GRAB PACKHUM REF -----------------------------------------------------------
#textpage > div.docref
PHIno <- page %>% html_nodes('div.docref') %>% html_text()
PHIno <- gsub("\\n", "", PHIno)

### GRAB TEXT ------------------------------------------------------------------
#textpage > div.text > div.greek.text-nowrap.dblclk > table
text <- page %>% html_nodes('div.greek.text-nowrap.dblclk') %>% html_text()
text <- gsub("\\n", "", text)
text <- gsub("\\d", "", text)
text <- gsub("-", "", text)
#ℎ PLANCK CONSTANT Unicode: U+210E, UTF-8: E2 84 8E
text <- gsub("ℎ", "h", text)

### PRINT OUT ------------------------------------------------------------------
print(IGbook)
print(IGno)
print(PHIno)
print(text)

### SCRAPBOOK ------------------------------------------------------------------

# testing bad selector paths ---------------------------------------------------
link <- 'https://inscriptions.packhum.org/text/1825'
page <- read_html(link)

#textpage > div.hdr2 > span.fullref > a
PHIno <- page %>% html_nodes('div.docref') %>% html_text()
PHIno <- gsub("\\n", "", PHIno)

print(PHIno)



SCRAPE <- function() {
  #for (i in c(1754:1764)) {
  for (i in c(1:10)) {
    # testing cleaning
    #link <- 'https://inscriptions.packhum.org/text/1825'
    link <- paste('https://inscriptions.packhum.org/text/', i, sep="")
    page <- read_html(link)
    
    #textpage > div.text > div.greek.text-nowrap.dblclk > table
    text <- page %>% html_nodes('div.greek.text-nowrap.dblclk') %>% html_text()
    text <- gsub("[\\d\\[\\]]", "", text, perl=T)
    text <- gsub("(\\-\\n{1,})", "", text, perl=T)
    text <- gsub("\\n{1,}", "", text, perl=T)
    text <- gsub("\\s{1,}", " ", text, perl=T)
    
    print(link)
    print(text)
  }
}

TEST <- function(PHIno = 10) {
  link <- paste('https://inscriptions.packhum.org/text/', PHIno, sep="")
  page <- read_html(link)
  
  #textpage > div.text > div.greek.text-nowrap.dblclk > table
  text <- page %>% html_nodes('div.greek.text-nowrap.dblclk') %>% html_text()
  text <- gsub("[\\d\\[\\]]", "", text, perl=T)
  text <- gsub("(\\-\\n{1,})", "", text, perl=T)
  text <- gsub("\\n{1,}", "", text, perl=T)
  text <- gsub("\\s{1,}", " ", text, perl=T)
  
  print(link)
  print(text)
  
}
