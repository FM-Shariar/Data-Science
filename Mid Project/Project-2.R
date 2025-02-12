install.packages("rvest")
install.packages("tm")
install.packages("SnowballC")
install.packages("textclean")
install.packages("hunspell")
install.packages("openxlsx")

library(openxlsx)
library(hunspell)
library(textclean)
library(SnowballC)
library(tm)
library(rvest)

url <- "https://en.wikipedia.org/wiki/Wikipedia:About"

webpage <- read_html(url)

raw_text <- webpage %>%
  html_nodes("p") %>%
  html_text()

raw_text_combined <- paste(raw_text, collapse = " ")

cleaned_text <- gsub("[^a-zA-Z\\s]", " ", raw_text_combined)

cleaned_text <- tolower(cleaned_text)

tokens <- unlist(strsplit(cleaned_text, "\\s+"))

tokens <- tokens[tokens != ""]

tokens_rep <- paste0('"', tokens, '"',sep="", collapse = ", ")

cat(tokens_rep)

tokens <- unlist(strsplit(cleaned_text, "\\s+"))

cleaned_tokens <- paste0('"', tokens, '"', collapse = ", ")
cat("After Text Cleaning: ", cleaned_tokens, "\n")

normalized_tokens <- gsub("u\\.s\\.|us", "united states", tokens)

normalized_tokens_rep <- paste0('"', normalized_tokens, '"', collapse = ", ")
cat("After Normalization: ", normalized_tokens_rep, "\n")

stopwords_list <- stopwords("en")
tokens_no_stopwords <- tokens[!tokens %in% stopwords_list]

stopwords_removed_rep <- paste0('"', tokens_no_stopwords, '"', collapse = ", ")
cat("After Stop Word Removal: ", stopwords_removed_rep, "\n")

stemmed_tokens <- wordStem(tokens_no_stopwords, language = "en")

stemmed_tokens_rep <- paste0('"', stemmed_tokens, '"', collapse = ", ")
cat("After Stemming: ", stemmed_tokens_rep, "\n")

expanded_text <- replace_contraction(cleaned_text)

expanded_tokens <- unlist(strsplit(expanded_text, "\\s+"))
expanded_tokens_rep <- paste0('"', expanded_tokens, '"', collapse = ", ")
cat("After Handling Contractions: ", expanded_tokens_rep, "\n")

text_no_emojis <- gsub("[\U0001F600-\U0001F64F\U0001F300-\U0001F5FF\U0001F680-\U0001F6FF\U0001F1E6-\U0001F1FF]", "", expanded_text, perl = TRUE)

text_no_emojis_tokens <- unlist(strsplit(text_no_emojis, "\\s+"))
text_no_emojis_rep <- paste0('"', text_no_emojis_tokens, '"', collapse = ", ")
cat("After Removing Emojis: ", text_no_emojis_rep, "\n")

misspelled <- hunspell_find(tokens)
corrected_tokens <- hunspell_suggest(tokens)

corrected_tokens_rep <- paste0('"', corrected_tokens, '"', collapse = ", ")
cat("After Spell Checking: ", corrected_tokens_rep, "\n")

final_text <- paste0('"', stemmed_tokens, '"', collapse = ", ")

writeLines(final_text, "preprocessed_text.txt")

getwd()

quoted_tokens <- paste0('"', stemmed_tokens, '"')
df_stemmed <- data.frame(quoted_tokens = quoted_tokens)

write.xlsx(df_stemmed, "C:/Users/F. M SHARIAR/Desktop/9th Semester/Data Science/Mid/Project/Project2.xlsx")
getwd()
