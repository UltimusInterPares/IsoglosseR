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

### SCRAPER --------------------------------------------------------------------

# testing bad selector paths ---------------------------------------------------
link <- 'https://inscriptions.packhum.org/text/0'
page <- read_html(link)

#textpage > div.hdr2 > span.fullref > a
PHIno <- page %>% html_nodes('div.docref') %>% html_text()
PHIno <- gsub("\\n", "", PHIno)

print(PHIno)

# test function + working example ----------------------------------------------

TEST <- function(PHIno = 20) {
  link <- paste('https://inscriptions.packhum.org/text/', PHIno, sep="")
  page <- read_html(link)
  
  ### Text
  #textpage > div.text > div.greek.text-nowrap.dblclk > table
  text <- page %>% html_nodes('div.greek.text-nowrap.dblclk') %>% html_text()
  text <- gsub("[\\d\\[\\]]", "", text, perl=T)
  text <- gsub("(\\-\\n{1,})", "", text, perl=T)
  text <- gsub("\\n{1,}", " ", text, perl=T)
  text <- gsub("\\s{1,}", " ", text, perl=T)
  # ̣COMBINING DOT BELOW Unicode: U+0323, UTF-8: CC A3
  text <- gsub("̣", "", text, perl=T)
  text <- gsub("#", "", text, perl=T)
  # ℎ PLANCK CONSTANT Unicode: U+210E, UTF-8: E2 84 8E
  text <- gsub("ℎ", "h", text, perl=T)
  
  ### IGBook
  #textpage > div.hdr2 > span.fullref > a
  IGbook <- page %>% html_nodes("span.fullref > a") %>% html_text()
  IGbook <- gsub("\\n", "", IGbook)
  
  ### IGNo
  IGno <- page %>% html_nodes("span.fullref > span") %>% html_text()
  IGno <- gsub("\\n", "", IGno)
  
  # Atm keeping for debugging
  # print(link)
  # print(text)
  # print(IGbook)
  # print(IGno)
  # print(PHIno)
  
  # Make entry to be returned
  entry <- c(IGbook, IGno, PHIno, text, link)
  return(as.list(entry))
  
  # Atm keeping for debugging
  # rm(link)
  # rm(text)
  # rm(IGbook)
  # rm(IGno)
  # rm(PHIno)
}

# df[row, column]
test_df <- tibble(Book = NA, No = NA, Ref = NA, Text = NA, Link = NA)
test_df[1,] <- TEST(1)
test_df[2,] <- TEST(2)
test_df[3,] <- TEST(3)

for (i in c(1:100)) {
  test_df[i,] <- TEST(i)
}

# testing bad selector paths ---------------------------------------------------
test_prime <- tibble(Book = NA, No = NA, Ref = NA, Text = NA, Link = NA)
test_prime[1,] <- TEST(0)
