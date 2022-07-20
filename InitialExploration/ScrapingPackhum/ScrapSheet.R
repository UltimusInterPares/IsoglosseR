test_link <- paste('https://inscriptions.packhum.org/text/', 100, sep="")
test_page <- read_html(test_link)

  test_ig_no <- test_page %>% html_nodes("span.fullref > span") %>% html_text()
  test_ig_no < gsub("\\n", "", test_ig_no)
  

  
test_range_a <- "Ath 510-500"
test_range_b <- "after 306 BC"
test_range_c <- "early 3rd c. BC "
test_range_d <- "before early 3rd c. BC "
test_range_dd <- "before mid 3rd c. BC"
test_range_e <- "after early 3rd c. BC "
test_range_f <- "late 3rd c. BC"

test_range_g <- "after 2nd c. BC "

TranslateCentury <- function(header) {
  if (grepl("(\\d+)(?=\\w{2} c.)", header, perl = T)) {
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
      header <- gsub("(\\d+)(?>\\w{2} c.)", cen, header, perl = T)
      return(header)
    }
  } else {
    return(header)
  }

}

ReadDateAfter <- function(header) {
  if (grepl("\\d+-\\d+", header, perl = T)) {
    date_after <- str_extract(header, "\\d+(?=-)")
  } else if (grepl("(?<=after )\\d+", header, perl = T)) {
    date_after <- str_extract(header, "(?<=after )\\d+")
  }  else if (grepl("(?<=— )(\\d+)(?= BC)", header, perl = T)) {
    date_after <- str_extract(header, "(?<=— )(\\d+)(?= BC)")
  } #else {date_after <- NA}
  return(date_after)
}

ReadDateBefore <- function(header) {
  if (grepl("\\d+-\\d+", header, perl = T)) {
    date_before <- str_extract(header, "(?<=-)\\d+")
  } else if (grepl("(?<=before )\\d+", header, perl = T)) {
    date_before <- str_extract(header, "(?<=before )\\d+")
  } else if (grepl("(?<=— )(\\d+)(?= BC)", header, perl = T)) {
    date_before <- str_extract(header, "(?<=— )(\\d+)(?= BC)")
  } #else {date_before <- NA}
  return(date_before)
}
