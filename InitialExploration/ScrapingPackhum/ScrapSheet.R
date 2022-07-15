test_link <- paste('https://inscriptions.packhum.org/text/', 100, sep="")
test_page <- read_html(test_link)

  test_ig_no <- test_page %>% html_nodes("span.fullref > span") %>% html_text()
  test_ig_no < gsub("\\n", "", test_ig_no)
