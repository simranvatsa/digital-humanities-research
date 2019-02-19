library(tidyverse)
library(rvest)
library(stringr)
library(glue)
library(tidytext)
library(readr)

## scraping the site
home <- read_html("http://voices.iit.edu/search_results?filter_by=name")
names <- home %>% 
  html_nodes(".result_list a") %>% 
  html_text()
names <- gsub("\\n", "", names)
names <- trimws(names)

english <- read_html("http://voices.iit.edu/search_results.php?filter_by=broadcast_lang&filter_value=English")
english_speakers <- english %>% 
  html_nodes(".result_list a") %>% 
  html_text()
english_speakers <- gsub("\\n", "", english_speakers)
english_speakers <- trimws(english_speakers)
wrongly_listed <- c("Charles Jean", "Jean Kahn", "Louis Kahn", "Marcelle Kahn", "Abraham Schrameck", "Yanusch Deutsch", "Fira Monk")

nonenglish <- setdiff(names, english_speakers)
nonenglish <- c(nonenglish, wrongly_listed)
english_speakers <- setdiff(english_speakers, wrongly_listed)
all_int <- c(nonenglish, english_speakers)
all_int <- str_c(all_int, collapse = "\n")
write_file(all_int, "interviewee_names.txt")

main <- html_session("http://voices.iit.edu/search_results?filter_by=name")
transcripts <- rep(NA, 118)

for(i in 1:110) {
  transcripts[i] <- main %>% 
    follow_link(nonenglish[i]) %>% 
    follow_link("Read English Translation") %>% 
    html_nodes("ul:nth-child(3)") %>% 
    html_text()
}

for(i in 111:118) {
  transcripts[i] <- main %>% 
    follow_link(english_speakers[i-110]) %>% 
    follow_link("Read Transcript") %>% 
    html_nodes("ul:nth-child(3)") %>% 
    html_text()
}


## processing/tidying text for combined .txt/.doc file + separate .txt files
write("Boder transcripts and translations\n", "Boder_transcripts.txt")
for(i in 1:118) {
  transcripts[i] <- gsub(" {2,}", " ", transcripts[i])
  transcripts[i] <- str_split(transcripts[i], "(\\n ){4}")
  transcripts[[i]] <- gsub("\\n", "", transcripts[[i]])
  transcripts[[i]] <- gsub(" {2,}", " ", transcripts[[i]])
  write(transcripts[[i]], "Boder_transcripts.txt", append = TRUE)
  write(transcripts[[i]], paste0("Boder_transcripts",i,".txt"))
}


transcripts_dataframe <- data.frame(speech = transcripts[[1]], 
                                    interviewee = rep(1, length(transcripts[[1]])), stringsAsFactors = FALSE)

for(i in 2:118){
  for(j in 1:length(transcripts[[i]])){
    transcripts_dataframe <- rbind(transcripts_dataframe, c(transcripts[[i]][j], i))
  } 
}

all_speakers <- c(nonenglish, english_speakers)
transcripts_dataframe$interviewee <- as.numeric(transcripts_dataframe$interviewee)
transcripts_dataframe$interviewee_name <- all_speakers[transcripts_dataframe$interviewee]

pos <- NA
speaker <- NA
words <- NA

for(i in 1:dim(transcripts_dataframe)[1]) {
  pos[i] <- regexpr(':', transcripts_dataframe$speech[i])
  speaker[i] <- substr(transcripts_dataframe$speech[i], 1, pos[i]-1)
  words[i] <- substr(transcripts_dataframe$speech[i], pos[i]+2, 10000)
}

transcripts_dataframe <- data.frame(transcripts_dataframe, speaker, words, stringsAsFactors = FALSE)
transcripts_dataframe <- transcripts_dataframe[-c(1,2)]
transcripts_dataframe$speaker <- trimws(transcripts_dataframe$speaker)

## manually removed all footnotes from the text 