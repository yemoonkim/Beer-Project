sentiment <- read.csv(file = 'new data set with sentiment.csv')
head(sentiment)
library(dplyr)
df <- select(sentiment, c('user','text','overall', 'ave_sentiment'))
summary(df)
df$ave_sentiment <- as.numeric(df$ave_sentiment)
summary(df)
df$sentiment <- ifelse(df$ave_sentiment >= 0, "Positive", "Negative")
library(keras)
library(ggplot2)
library(purrr)
df %>% count(sentiment)
df$text[1]
training_id <- sample.int(nrow(df), size = nrow(df)*0.8)
training <- df[training_id,]
testing <- df[-training_id,]
df$text %>%
  strsplit(" ") %>%
  sapply(length) %>%
  summary()

#define text vectorization layer & convert to tensors

num_words <- 10000
max_length <- 50
text_vectorization <- layer_text_vectorization(
  max_tokens = num_words,
  output_sequence_length = max_length,
)
text_vectorization %>%
  adapt(df$text)
get_vocabulary(text_vectorization)
text_vectorization(matrix(df$text[1], ncol = 1))


#Build the model
input <- layer_input(shape = c(1), dtype = "string")
output <- input %>%
  text_vectorization() %>%
  layer_embedding(input_dim = num_words + 1, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 1, activation = "sigmoid")
models <- keras_model(input,output)

#configure the model to use and optimzer and loss function
models %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

#train the model

history <-  models %>% fit(
  training$text,
  as.numeric(training$sentiment == "Positive"),
  epochs = 20,
  batch_size = 512,
  validation_split = 0.2,
  verbose = 2
)

results <- models %>% 
  evaluate(testing$text, as.numeric(testing$sentiment== "Positive"), verbose = 0)
results

plot(history)

test_predictions <- models %>% predict(testing$text)
test_predictions[ 1:100, 1]

