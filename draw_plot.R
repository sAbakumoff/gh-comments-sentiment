library(ggplot2)
library(dplyr)
data<-read.csv("gh-comment-sentiment.csv", header=TRUE)
stats1<-data %>% select(language, score) %>% 
  mutate(tone=as.factor(ifelse(score>0, "positive", ifelse(score<0, "negative", "neutral")))) %>%
  select(language, tone) %>% 
  group_by(language, tone) %>% 
  summarize(count=n()) %>%
  ungroup() %>%  arrange(language, desc(tone)) %>%
  group_by(language) %>%
  mutate(tone_percent = 100*count/sum(count), label_pos=cumsum(tone_percent) - 0.5 * tone_percent)
temp<-stats1 %>% filter(tone=="positive") %>% arrange(-tone_percent) %>% select(language)
temp$pos<-seq.int(nrow(temp))
stats1<-merge(stats1, temp, by="language")
ggplot() + theme_bw() + geom_bar(aes(y=tone_percent, x=reorder(language, -pos), fill=tone), data=stats1, stat="identity") + 
  geom_text(data=stats1, aes(x = language, y = label_pos, ymax=label_pos, hjust = 0.5, label = paste0(round(tone_percent),"%")), size=4) + 
  labs(x="Language", y="Percentage of sentiment") +
  scale_fill_manual(values=c('#F45E5A', '#5086FF', '#17B12B'), guide = guide_legend(reverse = TRUE)) + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) + coord_flip()
