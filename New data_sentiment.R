options(install.packages.check.source = "no") #do not install versions that require compiling

# List of required packages for the examples
requiredPackages = c("rtweet","magrittr" ,"tidytext", "data.table", "ggplot2", "graphics", "topicmodels", 
                     "quanteda", "quanteda.textstats", "quanteda.textplots", "stats", "grDevices", 
                     "utils", "methods", "utf8", "sentimentr", "igraph", "vosonSML")

#Looping through the list of required packages
for (i in seq_along(requiredPackages)) {
  if (length(find.package(requiredPackages[i],quiet=TRUE))==0) { # If package not found
    print(paste("Package",requiredPackages[i], "not found, installing..."))
    install.packages(requiredPackages[i]) #install it
  }
}
new.df <- read.csv("new text dataset.csv")
save(review.df, file = "new.df.RData")

library(data.table)
setDT(new.df)


#sentiment analysis

require(sentimentr)
new.df$text[1]

new.df$text[1] %>% get_sentences

new.df$text[1] %>% get_sentences %>% sentiment

new.df$text[1] %>% get_sentences %>% sentiment_by

sentiment_by_review = 
  new.df$text %>% get_sentences %>% sentiment_by()

new2.df <- cbind(new.df,sentiment_by_review)

write.csv(new2.df, "scrape_sentiment.csv")

