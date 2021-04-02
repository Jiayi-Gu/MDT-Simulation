queries <- read.csv('queries.csv')
queries_attr <- read.csv('queries.attr.csv')
recommendations <- read.csv('recommendations.csv')
users <- read.csv('user.profiles.csv')
invitations <- read.csv('invitations.csv')
relationships <- read.csv('relationships.csv')
teams <- read.csv('teams.csv')

library(tidyverse)
library(htmltools)
library(plotly)
library(forcats)
library(reldist)
library(vegan)

distinct_invitations <- invitations %>%
  distinct(pair, .keep_all=TRUE)

distinct_recommendations <- recommendations %>%
  distinct(pair, .keep_all=TRUE)

invitations[invitations$n==6,]
invitations[invitations$n==5,]
invitations[invitations$n==4,]
invitations[invitations$n==3,]
invitations[invitations$n==2,]

rec_invite <- distinct_recommendations %>%
  left_join(distinct_invitations, by = c("sender", "project_id", "pair")) %>%
  rename(rec_count=n.x,inv_count=n.y) %>%
  arrange(inv_count)
## above is exploration in v1 data to identify which recs led to invites, 
##since  this is specified in v2. we'll only look at v2 data and ignore the part above

# ver 2 invitation analysis
v2_reccomendations <- recommendations %>%
  filter(preferences_id>='v2')
nrow(v2_reccomendations)

# % good invitations
invite_or_not <- na.omit(recommendations$invited)
total_v2_rec <- length(invite_or_not)
Percent_invite <- sum(invite_or_not)/length(invite_or_not)

good_rec <- recommendations %>%
  filter(invited==1) %>%
  rename(team_id_sender=team_id) %>%
  left_join(final_teams, by=c('recommended'='user_id','project_id')) %>%
  rename(team_id_recipient=team_id) 

#check if they in same team
good_rec <- good_rec %>%
  rowwise() %>%
  mutate(final_match=(team_id_sender==team_id_recipient))
# if either person has team NA, they are also not final match to each other
good_rec$final_match[is.na(good_rec$final_match)] <- FALSE

percent_final_team <- sum(good_rec$final_match)/nrow(good_rec)
# 34% the recommended in final team

rank_count <- good_rec %>% 
  count(rank) %>%
  mutate(percent=100*n/nrow(good_rec))

rank_count %>%
  plot_ly(x=~rank, y=~n) %>%
  add_bars()

mean(good_rec$rank) #

# now check if they chose non first 3 choice, are those they have a strong relationships with?
non_first_rec <- good_rec %>%
  filter(rank>3) %>%
  left_join(relationships,by=c('pair','project_id','sender'='source','recommended'='target')) 
non_first_rec$type[is.na(non_first_rec$type)] <- 'No Relationship'
length(unique(non_first_rec$pair)) #805

non_first_rec <- non_first_rec %>%
  filter(type=='strong' | type=='weak'| type=='No Relationship')

non_first_rec %>%
  count(type)%>%
  plot_ly(x=~reorder(type,-n), y=~n) %>%
  add_bars()


first_rec <- good_rec %>%
  filter(rank<=3) %>%
  left_join(relationships,by=c('pair','project_id','sender'='source','recommended'='target')) 
first_rec$type[is.na(first_rec$type)] <- 'No Relationship'
length(unique(first_rec$pair)) #405

first_rec <- first_rec %>%
  filter(type=='strong' | type=='weak'| type=='No Relationship')

first_rec %>%
  count(type)%>%
  plot_ly(x=~reorder(type,-n), y=~n) %>%
  add_bars()

# top results and relationships
top_results <- recommendations %>%
  filter(rank<=3) %>%
  left_join(relationships,by=c('pair','project_id','sender'='source','recommended'='target'))
top_results$type[is.na(top_results$type)] <- 'No Relationship'
top_results <- top_results %>%
  filter(type=='strong' | type=='weak'| type=='No Relationship')

top_results %>%
  count(type)%>%
  plot_ly(x=~reorder(type,-n), y=~n) %>%
  add_bars()

bottom_results <- recommendations %>%
  filter(rank>3) %>%
  left_join(relationships,by=c('pair','project_id','sender'='source','recommended'='target'))
bottom_results$type[is.na(bottom_results$type)] <- 'No Relationship'
bottom_results <- bottom_results %>%
  filter(type=='strong' | type=='weak'| type=='No Relationship')
bottom_results %>%
  count(type)%>%
  plot_ly(x=~reorder(type,-n), y=~n) %>%
  add_bars()



