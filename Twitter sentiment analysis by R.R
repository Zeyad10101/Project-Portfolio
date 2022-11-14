library(rtweet)
tweets <- search_tweets(q = "#MUNTOT", 
                        n = 18000,
                        include_rts = FALSE,
                        `-filter` = "replies",
                        lang = "en")

###############################################################
### Sentiment analysis with tweets######
###############################################################

library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix_titles

tweets_token <- tweets %>%
  unnest_tokens(word, text)

nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")

#inner joining the India movies and the surprise sentiments
tweets_token %>%
  filter(country == "India") %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)

########################################################
##### Comparing different sentiment libraries on tweets ####
########################################################

india <- tweets_token %>%
  filter(country == "India")

afinn <- india %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  india%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  india %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts <- india %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

######################################################
####### TF-IDF framework in tweets #######
######################################################


library(dplyr)
library(stringr)
library(tidytext)
#let's look at the data
library(tidytuesdayR)

#we're grouping by the country this time
tweets_token <- tweets %>%
  unnest_tokens(word, text) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

total_words <- tweets_token %>%
  group_by(country) %>%
  summarize(total=sum(n))

tweets_words <- left_join(tweets_token, total_words)%>%
  filter(country %in% c("United States", "Mexico", "India"))

print(tweets_words)

library(ggplot2)
ggplot(tweets_words, aes(n/total, fill = country))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~country, ncol=2, scales="free_y")
#what do the tails represent? 
#answer: exremely common words! 
# we are really interested in the not so common words. 

######################################
########## ZIPF's law ################
######################################

freq_by_rank <- tweets_words %>%
  group_by(country) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=country))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################

country_words <- tweets_words %>%
  bind_tf_idf(word, country, n)

country_words # we get all the zeors because we are looking at stop words ... too common

country_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?

#############
# looking at the graphical apprach:
country_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()

#############################################
###### N-grams and tokenizing ###############
#############################################

library(dplyr)
library(tidytext)
library(tidyr)
library(tidytuesdayR)

tweets_bigrams <- tweets %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

tweets_bigrams #We want to see the bigrams (words that appear together, "pairs")

tweets_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- tweets_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

###########################################################
###### What if we are interested in the most common #######
################ 4 consecutive words - quadro-gram ########
###########################################################
quadrogram <- tweets %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram

###########################################################
###### We can also apply the tf_idf framework  ############
########### on our bigram and quadro-gram #################
###########################################################

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(country, bigram) %>%
  bind_tf_idf(bigram, country, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

##### lets do the same for a quadrogram

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(country, quadrogram) %>%
  bind_tf_idf(quadrogram, country, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf

######################################################
######## visualising negated words ###################
###### negated words in sentiment analysis ###########
######################################################

negation_tokens <- c("no", "never", "without", "not")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(afinn_data, by=c(word2="word")) %>%
  count(word1, word2, score, sort=TRUE) %>%
  ungroup()

negated_words

##############################################
#### we can visuals the negated words ####
negated_words_plot <- function(x){
  negated_words %>%
    filter(word1 == x) %>%
    mutate(contribution = n* score) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*score, fill = n*score >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment score* number of occurences")+
    coord_flip()
}#closing the negated_words_plot function

negated_words_plot(x="not")
negated_words_plot(x="no")
negated_words_plot(x="without")

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

library(tidytuesdayR)
library(dplyr)
library(tidytext)

#############################################
### creating a tidy format for United States movies
usa <- tweets %>%
  filter(country== "United States")

tidy_usa <- usa %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_usa)

### creating a tidy format for Brazil movies
brazil <- tweets %>%
  filter(country== "Brazil")

tidy_brazil <- brazil %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_brazil)

### creating a tidy format for India posts
india <- tweets %>%
  filter(country== "India")

tidy_india <- india %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_india)

#############################################
####We want to combine all the datasets and do frequencies 
#############################################
library(tidyr)
frequency <- bind_rows(mutate(tidy_usa, author="United States"),
                       mutate(tidy_brazil, author= "Brazil"),
                       mutate(tidy_india, author="India")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brazil`, `India`)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`United States`, 
                      color = abs(`United States`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "United States", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$author == "Brazil",],
         ~proportion + `United States`)

cor.test(data=frequency[frequency$author == "India",],
         ~proportion + `United States`)



  







