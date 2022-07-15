### THIS SHOULD MAKE A TIBBLE TELLING ME HOW LONG IT TAKES TO RUN SUCCESSIVE
### AND INCRIMENTALLY LARGER ITERATIONS OF THE SCRAPER

df <- tibble(time=NA)

# Outer Loop determines how many to get
for (i in c(1:200)) {
  # Inner loop grabs
  df[i,] <- system.time(for (j in c(1:i)) {
    ScrapeTest(phi_no=i)
  })[3]
}

lm(df$time ~ c(1:nrow(df)), data = df)
#c(1:nrow(df)) are x's
#0.3763 is the slope (m)
#0.6856 is the intercept

ggplot(data = df) +
  geom_point(aes(x=c(1:nrow(df)), y=time)) +
  geom_smooth(aes(x=c(1:nrow(df)), y=time), method=lm, se=F,  color='red') +
  theme_classic()

# Estimated time in seconds
est_time <- (0.3794*(2100))+0.5912 
est_time %/% 60
