# Global .R


clean_txt <- function(txt){ 
  # ugly little fixes 
  txt <- gsub("\n"," ", txt) # treat new lines as space 
  txt <- gsub("e.g.", "e.g,", txt, fixed = TRUE)
  txt <- gsub("i.e.", "i.e,", txt, fixed = TRUE)
  return(txt)
}

get_sentences <- function(txt){ 
  sentences <- strsplit(txt, split = "\\? |\\. ")
  sentences <- lapply(sentences, trimws)
  return(sentences)
}

get_pure_words <- function(txt){ 
  
  pure_words <- lapply(strsplit(x = txt, split = " "), tolower)
  
  pure_words <- lapply(pure_words, FUN = function(x){ 
    gsub(pattern = "[[:punct:]]","",x)
  })
  
  pure_words <- unlist(pure_words)
  pure_words <- pure_words[ !(pure_words == "")]
  
  return(pure_words)
}

get_words_ranking <- function(pure_words){ 
  
  words_ranking <- as.data.frame(table(pure_words),
                                 stringsAsFactors = FALSE)
  
  words_ranking$score <- words_ranking$Freq/sum(words_ranking$Freq)
  
  return(words_ranking)
}

get_sentence_ranking <- function(sentences, words_ranking){ 
  
  sentence_ranking <- data.frame(sentence = unlist(sentences),
                                 stringsAsFactors = FALSE)
  
  sentence_ranking$score = rep(0, nrow(sentence_ranking))
  
  for(i in 1:nrow(sentence_ranking)){ 
    temp_sentence <- gsub("[[:punct:]]", "",
                          tolower(sentence_ranking$sentence[i]))
    
    found_word_index <- unlist(lapply(words_ranking$pure_words,
                                      FUN = function(word){ 
                                        grepl(word, temp_sentence)
                                      }))
    
    temp_score <- sum(words_ranking$score[found_word_index])
    
    sentence_ranking$score[i] <- temp_score
  }
  
  sentence_ranking$score_percentile <- ecdf(sentence_ranking$score)(sentence_ranking$score)
  
  return(sentence_ranking)
}