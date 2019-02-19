# dumbbell plot data preparation
transcripts_tokenized <- all_transcripts %>% unnest_tokens(word, words, strip_punct = TRUE)
transcripts_tokenized$inter <- ifelse(transcripts_tokenized$speaker == "David Boder", 
                                      "interviewer", "interviewee")
word_count_diffs <- transcripts_tokenized %>% 
  group_by(id, inter) %>%
  summarise(n = length(word)) %>% 
  filter(n > 100)
inter_names <- str_split(read_file("interviewee_names.txt"), "\n")[[1]]
inter_names <- inter_names[-36]
for_plot <- word_count_diffs %>% 
  spread(inter, n)
for_plot <- for_plot %>% mutate(diff = interviewee - interviewer)
for_plot <- for_plot %>% mutate(ratio = interviewee/interviewer)
for_plot <- cbind(for_plot, interviewee_name = inter_names)

# write_csv(for_plot, "word_ratios.csv")
for_plot <- for_plot %>% arrange(desc(diff))

# dumbbell plot
for_plot %>% 
  ggplot() +
  geom_dumbbell(aes(y = interviewee_name, xend = interviewee, x = interviewer), 
                colour_x = "cyan4", colour_xend = "palevioletred3", size_x = 2, size_xend = 2) +
  geom_text(aes(x = interviewee, y = interviewee_name, label = interviewee), 
            nudge_x = ifelse(for_plot$interviewee > for_plot$interviewer, 2100, -2100), size = 3) +
  geom_text(aes(x = interviewer, y = interviewee_name, label = interviewer), 
            nudge_x = ifelse(for_plot$interviewee > for_plot$interviewer, -2100, 2100), size = 3) +
  theme_minimal() +
  scale_y_discrete(limits = rev(for_plot$interviewee_name)) +
  ggtitle("Difference between word counts of Boder and each interviewee", 
          subtitle = "Boder's word count is in blue; the interviewees' are in pink.") +
  theme(axis.title = element_blank(), plot.title = element_text(size = 13), 
        plot.subtitle = element_text(size = 10))




# word-to-ellipsis plot data preparation
for_plot <- for_plot %>% arrange(id)
all_transcripts$ellipses <- str_count(all_transcripts$words, "(\\.( |)){2,}")

boder_ell <- all_transcripts %>% 
  group_by(id, speaker) %>%
  filter(speaker == "David Boder") %>% 
  summarise(boder_ellipses = sum(ellipses))

ellipse_count <- all_transcripts %>% 
  group_by(id) %>%
  summarise(ellipse_sum = sum(ellipses)) %>%
  mutate(word_count = for_plot$interviewee, interviewee = inter_names, 
         boder_ellipses = boder_ell$boder_ellipses) 

ellipse_count <- ellipse_count %>% 
  mutate(ratio = word_count/(ellipse_sum - boder_ellipses))

# ellipsis histogram
ellipse_count %>%
  filter(ratio < 1000) %>% 
  ggplot(aes(ratio)) +
  geom_histogram(fill = "turquoise3", color = I("grey20"), bins = 100) +
  scale_x_continuous(breaks = seq(0, 1000, 100), minor_breaks = seq(0, 1000, 20)) +
  theme_minimal() +
  ggtitle("Distribution of word-to-ellipsis ratios in the Boder testimonies",
          subtitle = "Three testimonies with ratios > 1000 (more than 1000 words for every ellipsis) have been left out.") +
  theme(plot.title = element_text(size = 20)) 




# language changes - data prep for plotly chart
all_transcripts$lang_change <- str_count(all_transcripts$words, "\\[(I|i)n \\w+\\]")

extr <- str_extract_all(all_transcripts$words, "\\[(I|i)n [A-Z]\\w+\\]")
new_lang <- c()
for(i in 1:52235) {
  langs <- extr[[i]] 
  new_lang[i] <- ifelse(length(langs) == 0, NA, str_c(langs, collapse = ", "))
}
all_transcripts$`New language` <- new_lang
all_transcripts$`New language` <- str_remove_all(all_transcripts$`New language`, "\\[(I|i)n ")
all_transcripts$`New language` <- str_remove_all(all_transcripts$`New language`, "\\]")

two_or_more_lang <- str_detect(all_transcripts$`New language`, ",")
comma_pos <- str_locate_all(all_transcripts$`New language`, ",")

for(i in 1:52235) {
  if(!is.na(two_or_more_lang[i]) & two_or_more_lang[i]){
    all_transcripts$`New language`[i+1] <- str_sub(all_transcripts$`New language`[i], 
                                                   (comma_pos[[i]][1]+2), -1)
    all_transcripts$`New language`[i] <- str_sub(all_transcripts$`New language`[i],
                                                 1, (comma_pos[[i]][1]-1))
  }
}
all_transcripts$`New language`[22527] <- "German"
inter_names <- str_split(read_file("interviewee_names.txt"), "\n")[[1]][-36] %>% as.data.frame()
inter_names <- inter_names %>% mutate(id = c(1:35, 37:118)) 
colnames(inter_names) <- c("interviewee_name", "id")

all_transcripts <- left_join(all_transcripts, inter_names, by = "id")



# ggplotly chart
color_pal <- c("#e27499","#00a7ff","#ab0077","#aed6e0","#8d00a7",
               "#01788c","#9069ff","#3d2843","#d7c2ff","#666279")

all_transcripts %>%
  filter(!is.na(`New language`), `New language` != "Sacheshausen", 
         `New language` != "Lodz", `New language` != "Israel", `New language` != "",
         `New language` != "Russia") %>% 
  ggplot(aes(interviewee_name, sent_num, color = `New language`, text = `New language`)) +
  geom_point(size = 1) +
  scale_color_manual(values = color_pal) +
  coord_flip() +
  theme_minimal() +
  labs(color = "New language") +
  theme(axis.title = element_blank())

ggplotly(width = 1400, height = 2000, tooltip = "text")