test_link <- paste('https://inscriptions.packhum.org/text/', 100, sep="")
test_page <- read_html(test_link)

  test_ig_no <- test_page %>% html_nodes("span.fullref > span") %>% html_text()
  test_ig_no < gsub("\\n", "", test_ig_no)
  

  
test_range_a <- "Ath 510-500"
test_range_b <- "after 306 BC"
test_range_c <- "early 3rd c. BC "
test_range_d <- "before early 3rd c. BC "
test_range_e <- "after early 3rd c. BC "

TranslateCentury <- function(header) {
  
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
