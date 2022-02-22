library(dplyr)
library(rvest)

### GRAB PAGE ------------------------------------------------------------------
link <- 'https://inscriptions.packhum.org/text/1825?&bookid=3&location=1701'
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
PHno <- page %>% html_nodes('div.docref') %>% html_text()
  PHno <- gsub("\\n", "", PHno)
  
### GRAB TEXT ------------------------------------------------------------------
#textpage > div.text > div.greek.text-nowrap.dblclk > table
text <- page %>% html_nodes('div.greek.text-nowrap.dblclk') %>% html_text()
  text <- gsub("\\n", "", text)
  text <- gsub("\\d", "", text)
  text <- gsub("-", "", text)
  #ℎ PLANCK CONSTANT Unicode: U+210E, UTF-8: E2 84 8E
  text <- gsub("ℎ", "h", text)