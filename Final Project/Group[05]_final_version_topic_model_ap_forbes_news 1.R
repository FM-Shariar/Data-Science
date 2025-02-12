library(rvest)
library(dplyr)
library(tm)
library(topicmodels)
library(ggplot2)
library(tidytext)
library(SnowballC)
library(textstem)
library(reshape2)


urls_and_selectors <- list(
  list(url = "https://apnews.com/world-news", css = ".PagePromoContentIcons-text"),
  list(url = "https://www.forbes.com/news/", css = ".Ccg9Ib-7 , ._1-FLFW4R , .Bg1Io")
)

scrape_headlines <- function(url, css_selector) {
  webpage <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(webpage)) {
    message(paste("Failed to read URL:", url))
    return(character(0))
  }
  
  headlines <- webpage %>%
    html_elements(css = css_selector) %>%
    html_text() %>%
    unique()
  
  return(headlines)
}

all_headlines <- lapply(urls_and_selectors, function(entry) {
  scrape_headlines(entry$url, entry$css)
}) %>%
  unlist()

print(all_headlines)
headlines_df <- data.frame(Headlines = all_headlines, stringsAsFactors = FALSE)
write.csv(headlines_df, "ap_forbes_reuters_topic_model.csv", row.names = FALSE)
getwd()

headlines_from_csv <- read.csv("C:/Users/F. M SHARIAR/Desktop/9th Semester/Data Science/Final/Project/ap_forbes_reuters_topic_model.csv", stringsAsFactors = FALSE)$Headlines

corpus <- VCorpus(VectorSource(headlines_from_csv))

custom_stopwords <- c(stopwords("en"), "news", "new", "sky", "alert", "cinema", "blockbuster", "anywhere",
                      "around", "art", "brand", "bring", "code", "contact", "channel", "ago", "minute",
                      "january", "jan", "scroll", "story", "hour", "yesterday")

corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, custom_stopwords) %>%
  tm_map(content_transformer(lemmatize_strings)) %>%
  tm_map(stripWhitespace)
preprocessed_text <- sapply(corpus, as.character)
write.csv(data.frame(Preprocessed_Text = preprocessed_text), "preprocessed_text.csv", row.names = FALSE)
getwd()
dtm <- DocumentTermMatrix(corpus)
inspect(dtm)
row_totals <- apply(dtm, 1, sum)
dtm <- dtm[row_totals > 0, ]

if (nrow(dtm) == 0) {
  stop("The Document-Term Matrix is empty. Please check your preprocessing steps.")
}

num_topics <- 10

lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

lda_tidy <- tidy(lda_model, matrix = "beta")

lda_tidy <- lda_tidy %>%
  distinct(topic, term, .keep_all = TRUE)

beta_variance <- lda_tidy %>%
  group_by(topic) %>%
  summarize(variance = var(beta))

print(beta_variance)

variance_threshold <- 0.000001
high_variance_topics <- beta_variance %>%
  filter(variance > variance_threshold) %>%
  pull(topic)

top_terms <- lda_tidy %>%
  filter(topic %in% high_variance_topics) %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 12, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)
print(top_terms, n = 100)

if (nrow(top_terms) > 0) {
  ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = as.factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y", nrow = 2) +
    coord_flip() +
    labs(title = "Top Terms in High-Variance Topics",
         x = "Terms", y = "Beta Values") +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_text(size = 10))
} else {
  message("No terms to plot. Adjust variance threshold or check data.")
}

#DTM PLOTTING 
dtm_matrix <- as.matrix(dtm)
word_frequencies <- colSums(dtm_matrix)
top_words <- names(sort(word_frequencies, decreasing = TRUE)[1:30])
dtm_subset <- dtm_matrix[, top_words, drop = FALSE]
dtm_melted <- melt(dtm_subset)
colnames(dtm_melted) <- c("Document", "Term", "Frequency")
ggplot(dtm_melted, aes(x = Term, y = Document, fill = Frequency)) +
  geom_tile(color = "white") +  # Add white grid lines
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Document-Term Matrix Visualization (Top Words)",
       x = "Top Terms", y = "Document")

#Beta variance visualization
ggplot(beta_variance, aes(x = factor(topic), y = variance, fill = variance)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Beta Variance Across Topics", x = "Topics", y = "Variance")



