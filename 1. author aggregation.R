
#Project - Delphi study on twitch and VA
#Author - John O. Osborne
#Date - 12/2021

#Library

library(readxl)
library(readr)
library(tidyr)
library(car)
library(tidverse)
library(dplyr)
library(janitor)
library(visdat)
library(ggplot2)
library(stringr)

# Load data
d = read_csv('pubmed_search.csv')


# Variables
names(d)
head(d,10)
summary(d)

# reshape from wide to long
long <- gather(
  d, position, author_name, lead_author:position_15, factor_key = TRUE
  )

# Data cleaning
long <- drop_na(long,author_name) %>% #removes NA
  mutate(author_name = gsub("\\.", "", author_name))  #removes punctuation
 
long <-  long %>%
  mutate(lead_or_other = (str_extract(position, "[^_]+"))) %>% #make new column for 'lead' or 'other' author description
  mutate(lead_or_other = str_replace(lead_or_other, "position", "other")) #change 'position' to 'other'

#count most common authors
count <- long %>% 
  group_by(author_name,lead_or_other) %>%
  count(author_name, sort = TRUE) %>%
      group_by(author_name) %>%
      add_count(author_name, wt = n, name = "Total")

write.csv(count, 'aggreagrated authors.csv')
write.csv(long, 'pubmed long format.csv')


#List of experts

a <- count %>% filter(lead_or_other == 'lead') %>%
  filter(n >= 3) 

b <- count %>% filter(lead_or_other == 'other') %>%
  filter(n>= 10)

c <- rbind(a, b)

experts <- c %>% distinct(author_name, .keep_all = T)

write.csv(experts, 'list of experts.csv')
 



