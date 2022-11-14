#installing and loading the mongolite library to download the Airbnb data
install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://ID:PASSWORD@cluster0.jabwe.mongodb.net/myFirstDatabase?retryWrites=true&w=majority'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

# understanding the data
airbnb_all %>%
  group_by(room_type) %>%
  summarise(counts_obs=n()) %>%
  arrange(desc(counts_obs))

airbnb_all %>%
  group_by(property_type) %>%
  summarise(counts_obs=n()) %>%
  arrange(desc(counts_obs))


#tidy playgame with stop-words removal
library(dplyr)
library(tidyr)
library(tidytext)

tidy_airbnb <- airbnb_all %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)
print(tidy_airbnb)

#Decided to compare room types

data("stop_words")
ent.home <- airbnb_all %>%
  filter(room_type== "Entire home/apt")
tidy_home <- ent.home %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

Shared_room <- airbnb_all %>%
  filter(room_type== "Shared room")
tidy_shared <- Shared_room %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

Private_room <- airbnb_all %>%
  filter(room_type== "Private room")
tidy_private<- Private_room %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

library(tidyr)
library(tidyverse)
frequency_type <- bind_rows(mutate(tidy_home, author="Entire home/apt"),
                            mutate(tidy_shared, author="Shared room"),
                            mutate(tidy_private, author="Private room")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Shared room`, `Private room`)

library(scales)
ggplot(frequency_type, aes(x=proportion, y=`Entire home/apt`,
                           color = abs(`Entire home/apt`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Entire home/apt", x=NULL)
#Entire home/apt proportion(as benchmark) against Shared room and Private room in th plot
#we find in the plot the tokens in common in the room type and the tokens specific for the room types

#doing the cor.test()
cor.test(data=frequency_type[frequency_type$author == "Shared room",],
         ~proportion + `Entire home/apt`)

cor.test(data=frequency_type[frequency_type$author == "Private room",],
         ~proportion + `Entire home/apt`)

#dtm airbnb
airbnb_dtm <- tidy_airbnb %>%
  count(room_type, word) %>%
  cast_dtm(room_type, word, n)
airbnb_dtm

airbnb_dtm <- tidy_airbnb %>%
  count(property_type, word) %>%
  cast_dtm(property_type, word, n)
airbnb_dtm
## Sparsity for room type is 54% and the sparsity for the property type is 94%
#### so for the classification we decided to continue with property type


library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytuesdayR)

#converty to tidy format
apartment <- airbnb_all %>%
  filter(property_type== "Apartment")
tidy_Apartment <- apartment %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_Apartment)


#sentiment with NRC
Apartment_sentiment <- tidy_airbnb %>%
  filter(property_type == "Apartment") %>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment %in% c("positive", "negative")) %>%
               mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

House_sentiment <- tidy_airbnb %>%
  filter(property_type == "House") %>%
  inner_join(get_sentiments("nrc") %>%
               filter(sentiment %in% c("positive", "negative")) %>%
               mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

#bing for positives and negative words used in description
bing_counts_apartment <- tidy_Apartment %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_apartment

bing_counts_apartment %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#tidy format for "House"
house <- airbnb_all %>%
  filter(property_type== "House")
tidy_House <- house %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_House)

bing_counts_house <- tidy_House %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_house

bing_counts_house %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

Condo <- airbnb_all %>%
  filter(property_type == "Condominium")
tidy_Condo <- Condo %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_Condo)


bing_counts_condo <- tidy_Condo %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_condo

bing_counts_condo %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#ZIPFs
airbnb_property <- tidy_airbnb %>%
  count(property_type, word, sort=TRUE) %>%
  anti_join(stop_words) %>%
  ungroup()
airbnb_tot_prop <- airbnb_property %>%
  group_by(property_type) %>%
  summarise(total=sum(n))

airbnb_words <- left_join(airbnb_property, airbnb_tot_prop)%>%
  filter(property_type %in% c("Condominium", "House", "Apartment"))
print(airbnb_words)

library(ggplot2)
ggplot(airbnb_words, aes(n/total, fill = property_type))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~property_type, ncol=2, scales="free_y")


freq_by_rank_bnb <- airbnb_words %>%
  group_by(property_type) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank_bnb

freq_by_rank_bnb %>%
  ggplot(aes(rank, `term frequency`, color=property_type))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

#TF_IDF
property_type_words <- airbnb_words %>%
  bind_tf_idf(word, property_type, n)

property_type_words

property_type_words %>%
  arrange(desc(tf_idf))

property_type_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(property_type) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=property_type))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~property_type, ncol=2, scales="free")+
  coord_flip()
#The uniqueness of the words is because they are specific to specific locations

#bigrams
airbnb_bigrams <- airbnb_all%>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
airbnb_bigrams

airbnb_bigrams %>%
  count(bigram, sort = TRUE)
library(tidyr)
bigrams_separated <- airbnb_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") #separates the words from bigram
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>% #filtering word1 without stop words
  filter(!word2 %in% stop_words$word)

bigram_counts_airbnb <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts_airbnb

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")

bigram_tf_idf <- bigram_united %>%
  count(property_type, bigram) %>%
  bind_tf_idf(bigram, property_type, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

library(igraph)
bigram_graph <- bigram_counts_airbnb %>%
  filter(n>50) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+ #direction of tokens
  geom_node_point()+ #points are tokens
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#checking correlations in apartments to see something interesting for villa owners
library(widyr)
library(tidyr)
library(dplyr)
library(ggraph)
library(igraph)
library(tidytuesdayR)

my_tidy_df <- airbnb_all %>%
  filter(property_type == "Apartment") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

my_tidy_df

word_cors <- my_tidy_df %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, amenities, sort=TRUE)

word_cors %>%
  filter(item1 == "bed")
word_cors %>%
  filter(item1 %in% c("tv", "sofa", "size")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()