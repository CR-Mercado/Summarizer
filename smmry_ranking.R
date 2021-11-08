source("summarizer/global.R")

txt <- paste0(readLines("example_test_LI_post.txt"),collapse = " ")

txt <- clean_txt(txt)

sentences <- get_sentences(txt)

pure_words <- get_pure_words(txt)

words_ranking <- get_words_ranking(pure_words)

sentence_ranking <- get_sentence_ranking(sentences, words_ranking)

top_percent = .2 

paste0(sentence_ranking$sentence[sentence_ranking$score_percentile >= (1 - top_percent)], collapse = ". ")
