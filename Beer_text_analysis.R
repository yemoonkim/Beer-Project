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
review.df <- read.csv("total_7 for text analysis")
save(review.df, file = "review_df.RData")


library(data.table)
setDT(review.df) 

#가장 빈도가 잦은 값
review.df$review %>% 
  table %>%                      # make a frequency table 
  sort(decreasing = TRUE) %>%    # sort it by decreasing frequency
  head(15)                       # display the 15 most common

#Graphing
library(quanteda)
library(ggplot2)


#Tokenize 
tok_review = review.df$review %>% 
  gsub("#","", . ) %>% 
  corpus %>% 
  tokens(what="word",
         remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_symbols=TRUE,
         remove_separators=TRUE,
         remove_url=TRUE)
head(tok_review,n=10)

stopwords(language = "en")

tok_review = tokens_remove(tok_review,stopwords(language = "en"))
head(tok_review,n=2)

words.to.remove = c(stopwords("english"),"2020","2021","2019","2018","L","S","T","Oz","oz","OZ","ml","like")

dfmat_corp_review = review.df$review %>% corpus() %>% 
  tokens( what = "word",
          remove_punct = TRUE,
          remove_url=TRUE,
          remove_symbols = TRUE) %>% 
  tokens_remove(words.to.remove) %>% 
  tokens_wordstem(language = "en") %>% 
  dfm()

library(quanteda.textstats) # loads function textstat_frequency to name space
dfFreq = textstat_frequency(dfmat_corp_review) %>%
  as.data.table

ggplot(dfFreq[1:20,], aes(x=feature, y=frequency)) + 
  geom_col() +
  coord_flip() +
  theme_minimal()

dfFreq[1:20,]

ggplot(dfFreq[1:50,], aes(x=reorder(feature, -rank), y=frequency)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Stemmed word", y = "Count") 

head(tweets.df[grepl("2",text), list(text) ])

quanteda.textplots::textplot_wordcloud(dfmat_corp_review, min_count = 10, random_order = FALSE,
                                       rotation = .25,
                                       color = RColorBrewer::brewer.pal(8, "Dark2"))



dfFreq_long_top50 = dfFreq[rank <= 30] %>% 
  melt(id.vars = c("feature","group","rank"),
       measure.vars = c("frequency","docfreq")
  )
ggplot(dfFreq_long_top50, aes(x=reorder(feature,-rank), y=value, fill = variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete() + 
  labs(x = "", y = "Occurances", fill = "") +
  coord_flip() +
  theme_minimal()

require(quanteda.textstats)
TokensStemmed = tokens_remove(tok_review, words.to.remove)

dfm2 = dfm(tokens_ngrams(TokensStemmed,n=2))

dfFreq2 = textstat_frequency(dfm2)

ggplot(dfFreq2[1:30,], aes(x=reorder(feature, frequency), y=frequency)) + 
  geom_col() +
  coord_flip() +
  scale_x_discrete(name = "2 gram") +
  theme(text=element_text(size=12))

require(quanteda.textstats)
TokensStemmed = tokens_remove(tok_review, words.to.remove)

dfm3 = dfm(tokens_ngrams(TokensStemmed,n=3))

dfFreq3 = textstat_frequency(dfm3)

ggplot(dfFreq3[1:30,], aes(x=reorder(feature, frequency), y=frequency)) + 
  geom_col() +
  coord_flip() +
  scale_x_discrete(name = "3 gram") +
  theme(text=element_text(size=12))


#clustering
similar_wrds <- textstat_simil(dfmat_corp_review, 
                               margin="features",
                               method = "cosine")


similar_wrds[,"beer"] %>% sort(decreasing = TRUE) %>% head(8)

similar_wrds[,"prison"] %>% sort(decreasing = TRUE) %>% head(8)

similar_wrds[,"leader"] %>% sort(decreasing = TRUE) %>% head(8)

fstat = dfmat_corp_review %>%
  dfm_trim(min_termfreq = 0.998, termfreq_type = "quantile") %>%
  textstat_dist(margin="features")
plot(hclust(as.dist(fstat)))

fstat = as.dfm(dfm2) %>% 
  dfm_trim(min_termfreq = 0.9997, termfreq_type = "quantile") %>%
  textstat_dist(margin="features")
plot(hclust(as.dist(fstat)))

fstat = as.dfm(dfm3) %>% 
  dfm_trim(min_termfreq = 0.99985, termfreq_type = "quantile") %>%
  textstat_dist(margin="features")
plot(hclust(as.dist(fstat)))

word_dendogram = function(textdata, min_termfreq = 0.99) {
  textdata %>%            # take the data in textdata (first argument)
    corpus() %>%         # convert it to a corpus
    tokens(what = "word",                     # tokenize it by words
           remove_punct = TRUE,                      # remove punctuation and urls and symbols
           remove_url=TRUE,
           remove_symbols = TRUE) %>% 
    tokens_remove(words.to.remove) %>%       # remove stopwords and search terms as in the vector words.to.remove
    tokens_wordstem(language = "en") %>%     # stem it using the english word stemmer. This can be removed if necc.
    dfm() %>%                                # construct a doc freq. matrix
    dfm_trim(min_termfreq = min_termfreq, termfreq_type = "quantile") %>%
    textstat_dist(margin="features") %>% 
    as.dist() %>% 
    hclust() %>% 
    plot()
}

review.df[grepl("beer", text)] %>% word_dendogram(min_termfreq = 0.98)

review.df[grepl("jail", text)] %>% word_dendogram(min_termfreq = 0.98)



#sentiment analysis

require(sentimentr)
review.df$review[1]

review.df$review[1] %>% get_sentences

review.df$review[1] %>% get_sentences %>% sentiment

review.df$review[1] %>% get_sentences %>% sentiment_by

sentiment_by_tweet = 
  review.df$review %>% get_sentences %>% sentiment_by()

#inspect the first 6 rows of sentiment_by_tweet
head(sentiment_by_tweet)

# make sure that the columns of sentiment_by_tweet have not already been added to tweets.df 
review.df[,colnames(sentiment_by_tweet) :=NULL]

# then add them to tweets.df
review.df = cbind(review.df,sentiment_by_tweet)

#lower 10
review.df[,.(review, ave_sentiment)][order(-ave_sentiment)] %>% tail(10)
#top 10
review.df[,.(review, ave_sentiment)][order(-ave_sentiment)] %>% head(10)


review.df[grepl("alcohol",review),.(review, ave_sentiment)][order(-ave_sentiment)] %>% head(10)

review.df[grepl("beer",review),.(review, ave_sentiment)][order(-ave_sentiment)] %>% tail(10)

review.df[grepl("taste",review),.(review, ave_sentiment)][order(-ave_sentiment)] %>% head(10)

review.df[grepl("smell",review),.(review, ave_sentiment)][order(-ave_sentiment)] %>% tail(10)

review.df[grepl("head",review),.(review, ave_sentiment)][order(-ave_sentiment)] %>% tail(10)


hist(review.df$ave_sentiment)
mean(review.df$ave_sentiment)

#topic modeling

require(topicmodels)

dtm = convert(dfmat_corp_review, to = "topicmodels")
lda = LDA(dtm, k = 3, control=list(seed=20))
terms(lda, 20)
topics(lda)[1:3]

topicAssignment = 
  data.table(
    index = lda %>% 
      topics %>% 
      names %>% 
      gsub("text","", .) 
    %>% as.integer,
    topic = lda %>% topics
  )
topicAssignment %>% head(4)

review.df$Topic = NA # creates a new col ‘topic’, assign it to NA
review.df$Topic[topicAssignment$index] = topicAssignment$topic
review.df$Topic = review.df$Topic %>% as.factor

ggplot(review.df, aes(x=created_at, y=Topic, col=Topic)) +
  geom_jitter(aes(size = retweet_count)) +
  ggtitle(paste0("Each dot is a tweet matching 'Ann San Suu Kyi'")) +
  scale_y_discrete() +
  scale_x_datetime(name = "") + 
  scale_color_discrete(guide = "none") + 
  scale_size_continuous(name="Retweets")

tweets.df[,list(Total.Retweets = sum(retweet_count)),by=Topic] %>% 
  ggplot(aes(x = Topic, y = Total.Retweets)) + 
  geom_col()

tweets.df[!is.na(Topic),
          list(
            TotalTweets = .N, 
            TotalReactions=sum(retweet_count, na.rm = TRUE) + 
              sum(favorite_count, na.rm = TRUE)+
              sum(reply_count, na.rm = TRUE)+
              sum(quote_count, na.rm = TRUE),
            Reach = sum(followers_count)/10000
          ), 
          by = Topic] %>% 
  melt(id.vars = "Topic") %>% 
  ggplot(aes(x = Topic, y = value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_discrete(name= "", breaks=c("TotalTweets","TotalReactions","Reach"), labels = c("Tweets","Reactions","Reach in 10,000s")) + 
  scale_y_continuous(name = "Count")

dfm2 = dfm(tokens_ngrams(TokensStemmed,n=2))
dfm2 = convert(dfm2, to = "topicmodels")
lda2 = LDA(dfm2, k = 3, control=list(seed=123))
terms(lda2, 12)

topicAssignment2grams = 
  data.table(
    index = lda2 %>% 
      topics %>% 
      names %>% 
      gsub("text","", .) 
    %>% as.integer,
    topic = lda2 %>% topics
  )
tweets.df$Topic2gram = NA # creates a new col ‘topic’, assign it to NA
tweets.df$Topic2gram[topicAssignment2grams$index] = topicAssignment2grams$topic
tweets.df$Topic2gram = tweets.df$Topic2gram %>% as.factor

tweets.df[Topic2gram == 1][sample.int(.N,10), text]
tweets.df[Topic2gram == 2][sample.int(.N,10), text]
tweets.df[Topic2gram == 3][sample.int(.N,10), text]
tweets.df[is.na(Topic2gram)][sample.int(.N,10), text]

ggplot(tweets.df, aes(x=created_at, y=Topic2gram, col=Topic2gram)) +
  geom_jitter(aes(size = retweet_count)) +
  ggtitle(paste0("Each dot is a tweet matching '","Ann San Suu Kyi","'")) +
  scale_y_discrete() +
  scale_x_datetime(name = "") + 
  scale_color_discrete(guide = "none") + 
  scale_size_continuous(name="Retweets")

tweets.df[!is.na(Topic2gram),
          list(
            TotalTweets = .N, 
            TotalReactions=sum(retweet_count, na.rm = TRUE) + 
              sum(favorite_count, na.rm = TRUE)+
              sum(reply_count, na.rm = TRUE)+
              sum(quote_count, na.rm = TRUE),
            Reach = sum(followers_count)/10000
          ), 
          by = Topic2gram] %>% 
  melt(id.vars = "Topic2gram") %>% 
  ggplot(aes(x = Topic2gram, y = value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") + 
  scale_fill_discrete(name= "", breaks=c("TotalTweets","TotalReactions","Reach"), labels = c("Tweets","Reactions","Reach in 10,000s")) + 
  scale_y_continuous(name = "Count")

require(data.table)
require(magrittr)
require(sentimentr)
setDT(tweets.df)

tweets.df[grepl("four years|4|four",text),ave_sentiment] %>% mean()

tweets.df[grepl("two years|2|two",text),ave_sentiment] %>% mean()

tweets.df[grepl("least charges",text),ave_sentiment] %>% mean()

tweets.df[grepl("NLD",text),ave_sentiment] %>% mean()
tweets.df[grepl("Junta",text),ave_sentiment] %>% mean()

t.test(
  x =  tweets.df[grepl("NLD",text),ave_sentiment],  # tweets matching zendaya
  y = tweets.df[grepl("Junta",text),ave_sentiment] # tweets matching thimothée OR thimothee OR chalamet
)
Myanmar = tweets.df[country == "Myanmar"]
US = tweets.df[country == "United States"]

tweets.df[,list(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

tweets.df[,.(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,positive] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

