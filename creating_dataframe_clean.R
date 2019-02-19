library(tidyverse)
library(tidytext)
library(ggalt)
library(plotly)

all_transcripts <- c()
file_names <- str_c("Boder_transcripts", 1:118, ".txt") #importing transcripts without footnotes
for(i in 1:118) {
  all_transcripts[i] <- read_file(file_names[i])
}

# using separate testimonies to create a dataframe of testimonies

all_transcripts <- str_c(all_transcripts, collapse = "\n###:###\n") 
# creating demarcations b/w transcripts
all_transcripts <- str_split(all_transcripts, "\n")[[1]] %>% data.frame(stringsAsFactors = FALSE)
colnames(all_transcripts) <- "words"
all_transcripts <- all_transcripts %>% 
  separate(words, c("speaker", "words"), sep = ":", extra = "merge", fill = "left")
all_transcripts$words <- trimws(all_transcripts$words)
all_transcripts$speaker <- trimws(all_transcripts$speaker)
all_transcripts <- rbind(all_transcripts, "###")
hashes <- which(all_transcripts$speaker == "###") 
hashes <- c(0, hashes)

# creating split testimonies for further analysis by speaker:
for(i in c(1:35, 37:118)) {
  to_write <- all_transcripts[(hashes[i]+1):hashes[i+1],] %>% mutate(id = i)
  boder <- to_write %>% filter(speaker == "David Boder") %>% select(words) 
  boder <- boder[,1] %>% str_c(collapse = "\n")
  other <- to_write %>% filter(speaker != "David Boder") %>% select(words)
  other <- other[,1] %>% str_c(collapse = "\n")
  write_file(boder, str_c("boder_only", i, ".txt"))
  write_file(other, str_c("interviewee_and_others", i, ".txt"))
}

# adding sentence number and IDs to dataset
sent_num <- c()
id <- c()
for(i in c(1:118)) {
  len <- hashes[i+1] - hashes[i]
  sent_num <- c(sent_num, 1:len)
  id <- c(id, rep(i, len))
}
all_transcripts <- all_transcripts %>% mutate(sent_num = sent_num, id = id)
all_transcripts <- all_transcripts[c(4,3,1,2)]
all_transcripts <- all_transcripts %>% filter(!(is.na(speaker)), speaker != "###", !(is.na(words)))

# write_csv(all_transcripts, "Boder_transcripts_clean.csv")
all_transcripts <- read_csv("Boder_transcripts_clean.csv") 
# checkpoint of sorts, so you don't have to load all 118 transcripts and process them



