#setwd("~/Desktop/MDT")
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


pair1 <- vector()
for (i in 1:nrow(recommendations)){
  pair1[i] <- paste(recommendations$sender[i],recommendations$recommended[i])
}
recommendations$pair <- pair1

pair2 <- vector()
for (i in 1:nrow(invitations)){
  pair2[i] <- paste(invitations$sender[i],invitations$recipient[i])
}
invitations$pair <- pair2

pair3 <- vector()
for (i in 1:nrow(relationships)){
  pair3[i] <- paste(relationships$source[i],relationships$target[i])
}
relationships$pair <- pair3

invitations <- invitations %>%
  add_count(pair)

recommendations <- recommendations %>%
  add_count(pair) %>%
  left_join(queries, by = c('preferences_id'='id','sender'='user_id','project_id'='project_id')) %>%
  rename(rec_time=date)



# simulation begins:
PROJECT_ID=230  # takes data on this project

project_users <- users %>%
  filter(project_id==PROJECT_ID)
project_total_users <- nrow(project_users)
project_users_list <- project_users$user_id
#team number team size
project_teams <- unique(project_users$team_id)
project_team_number <- length(na.omit(project_teams)) 
# IMPT: to better consider class/team size
   # set team number to 22 for project 101
   # set team number to 8 for 131
   #or code will give error :)

a_team_structure <- project_users %>%
  count(team_id) 
project_team_size_upper <- max(a_team_structure$n)
  

project_rec <- recommendations %>%
  filter(project_id==PROJECT_ID)

project_inv <- invitations %>%
  filter(project_id==PROJECT_ID)

project_invited_rec <- project_inv %>% 
  left_join(project_rec, by=c('pair','project_id','sender'='sender','recipient'='recommended')) %>% 
  filter(invited==1) %>%
  mutate(invite_from_rec=1)

#for projects that can invite directly, remove direct invites (other than project 231 which contains no direct invites
project_inv <- project_inv %>% 
  left_join(select(project_invited_rec,pair,invite_from_rec),by=c('pair')) %>% 
  filter(invite_from_rec==1)

project_queries <- queries %>%
  filter(project_id==PROJECT_ID)

project_queries_attr <- queries_attr %>%
  filter(project_id==PROJECT_ID)

project_enjoywork <- relationships %>%
  filter(project_id==PROJECT_ID,type=='enjoy.work')

# identify in recommendations and invitations whicn entries are enjoy.work
project_rec_relation <- project_rec %>%
  left_join(project_enjoywork,by=c('pair','project_id','sender'='source','recommended'='target')) %>% 
  mutate(ENJOYWORK= type=='enjoy.work')
project_rec_relation$ENJOYWORK[is.na(project_rec_relation$ENJOYWORK)] <- FALSE

# only consider accepted invitations
project_inv_relation <- project_inv %>% 
  filter(status=='AC') %>% 
  left_join(project_enjoywork,by=c('pair','project_id','sender'='source','recipient'='target')) %>%
  mutate(ENJOYWORK=type=='enjoy.work')
project_inv_relation$ENJOYWORK[is.na(project_inv_relation$ENJOYWORK)] <- FALSE

# sort with time replied 
project_inv_relation <- project_inv_relation %>%
  arrange(replied_at)


## functions

## accept probability function 
invitation_eligible <- function(sender,sender_team_id,recipient){
  # checks if size of both teams sum up to greater than the size limit
    # if recipient has a team
  if(sum(teams_users==recipient)>0){
    recipient_team_id=which(teams_users==recipient, arr.ind=TRUE)[1]
    total=sum(teams_users[sender_team_id,]>0)+ sum(teams_users[recipient_team_id,]>0)
  } else{
    # if recipient does not have a team
    total=sum(teams_users[sender_team_id,]>0)+1
  }
  if(total>project_team_size_upper) {
    print('invite not eligible: size limit')
  } else {
      print('invite eligible')
    }
  return (total<=project_team_size_upper)
}

invitation_accept <- function(){
  x= runif(1)
 if(x<0.5) { # probability of accept
    print('invite accepted')
  } else {
      print('invite rejected')
    }
  return (x<0.5)
}

add_user <- function(team_id,user){
  # dont have to check size, already know elegible to merge from the eligibility function
  curr_size=sum(teams_users[team_id,]>0)
  teams_users[team_id,curr_size+1]<<-user
  print('user added')
}

#function to remove a team if the team is merging with another team
remove_team<- function(team_id){ 
 teams_users<<- rbind(teams_users[c(-team_id),],replicate(project_team_size_upper,0))
 rownames(teams_users)<<- 1:nrow(teams_users)
 print('team removed')
}

invite_to_team <- function(sender,sender_teamid,recipient){###
  #check if recipient has a team
  if(sum(teams_users==recipient)>0){
    print('two teams merging')
    recipient_team_id=which(teams_users==recipient, arr.ind=TRUE)[1]
    recipient_team_size=sum(teams_users[recipient_team_id,]>0)
    for (i in 1:recipient_team_size){# copy each to sender team
      add_user(sender_teamid,teams_users[recipient_team_id,i])
    }
    remove_team(recipient_team_id)
  } else{
    add_user(sender_teamid,recipient)
  }
}

#funtion that goes through the list of recommended users from the same query until it find eligible user to invite
invite_through_query <- function(sender,sender_team_id,queryID,nrow_next) {
  invite_status=0
  queryID_initial=queryID
  print('searching query initial')
  #check if next is enjoy.work and invite 
  while (queryID==project_rec_relation$preferences_id[nrow_next] ) {# check within same query
    if (project_rec_relation$ENJOYWORK[nrow_next]==FALSE) {# next is not enjoy.work 
      # set next person as the recipient
      new_recipient=project_rec_relation$recommended[nrow_next]
      # check if we can invite next person = eligibility and accpetance
      if (invitation_eligible(sender,sender_team_id,new_recipient) & invitation_accept()){
        invite_to_team(sender,sender_team_id,new_recipient)
        print('invited, exit initial query search')
        invite_status=1
        break
      } else {
        nrow_next=nrow_next+1
      }
    } else { # next is enjoy.work 
      nrow_next=nrow_next+1
    }
  } # end of while loop
  
  # exhausted all recommendations from this query, invite first result and go down the list till one accept
  if (invite_status==0){ # if still made no invite
    queryID=queryID_initial
    print('searching query secondary')
    nrow_next=(1:nrow(project_rec_relation))[project_rec_relation$preferences_id==queryID & project_rec_relation$rank==1]
    while (queryID==project_rec_relation$preferences_id[nrow_next] ){
      new_recipient=project_rec_relation$recommended[nrow_next]
      #invite this person
      if (invitation_eligible(sender,sender_team_id,new_recipient) & invitation_accept()){
        #add to team
        invite_to_team(sender,sender_team_id,new_recipient)
        print('invited, exit initial query search')
        break
      } else{
        nrow_next=nrow_next+1
      }
    }# end of second while
  }
  
  print('query search complete')
}

num_skills <- 9 # 9 measured project skills
rep_num <- 200
average_gini <- vector()
average_gender_simpson <- vector()
average_international_simpson <- vector()

for (rep in 1:rep_num){
  
  teams_users <- data.frame(matrix(0,project_team_number,project_team_size_upper))
  colnames(teams_users) <- 1:project_team_size_upper
  team_id=1
  teams_sizes <- vector()
  unassigned_users <- vector()
  
  for (i in 1:nrow(project_inv_relation)){
    print(i)
    sender=project_inv_relation$sender[i]
    #check if sender has a team
    if (sum(teams_users==sender)>0){ # if sender already has a team
      print('sender has a team')
      sender_team_id=which(teams_users==sender, arr.ind=TRUE)[1]
      sender_position=which(teams_users==sender, arr.ind=TRUE)[2]
      sender_team_size=sum(teams_users[sender_team_id,]>0)
      if(sender_team_size>=project_team_size_upper){
        print('sender team full')
        next # team full break for loop move to next invitation
      }
    }else{ 
      # place user at the position of the first available slot
      # if not all teams filled. user becomes the first in an empty team
      # if all team slots occupied, append user to smallest team
      for (k in 1:project_team_number){
        teams_sizes[k]=sum(teams_users[k,]>0)
      }
      min_team_size=min(teams_sizes)
      min_size_team_number=(1:project_team_number)[teams_sizes==min_team_size][1]
      sender_team_id=min_size_team_number
      teams_users[sender_team_id,min_team_size+1]=sender
      if (min_team_size==0){
        print('created new team')
        print(sender_team_id)
      } else{
        print('add to existing team')
        print(sender_team_id)
      }
      
      ### old ver does not work close your eyes
      # if(team_id<=9){
      #   print(team_id)
      #   teams_users[team_id,1]=sender
      #   sender_team_id=team_id
      #   if(team_id<=8) {
      #     team_id=which(teams_users[,1]==0,arr.ind=TRUE)[1]
      #      print('created new team')
      #   }else{
      #     team_id=100 # dummy variable indicate out of new teams
      #   }
      # } else{
      #   # out of new team slots # add to team with fewest members
      #   for (k in 1:project_team_number){
      #     teams_sizes[k]=sum(teams_users[k,]>0)
      #   }
      #   min_team_size=min(teams_sizes)
      #   min_size_team_number=(1:project_team_number)[teams_sizes==min_team_size][1]
      #   sender_team_id=min_size_team_number
      #   print('add to existing team')
      #   print(sender_team_id)
      #   teams_users[sender_team_id,min_team_size+1]=sender
      # }
      ## end of old ver
    }
    
    pair=project_inv_relation$pair[i]
    
    # get row number in rec table and the query preference ID
    nrow_curr=(1:nrow(project_rec_relation))[project_rec_relation$pair==pair & project_rec_relation$invited==1]
    queryID = project_rec_relation$preferences_id[nrow_curr]
    
    
    # if the invite was sent to enjoy.work person
    if (project_inv_relation$ENJOYWORK[i]==TRUE){
      print('enjoywork')
      # move to next
      nrow_next=nrow_curr+1 # the invite through query function start at the next person
      invite_through_query(sender,sender_team_id,queryID,nrow_next)
    } else{
      # if the invite was sent to non enjoy.work person
      print('non-enjoywork')
      new_recipient=project_inv_relation$recipient[i]
      # invite this person
      if (invitation_eligible(sender,sender_team_id,new_recipient) & invitation_accept()) {
        # add to team
        invite_to_team(sender,sender_team_id,new_recipient)
        print('invited from inv list')
        next
      } else{ 3 #if invite fails go to recommendation and invite down the query list
        nrow_next=nrow_curr+1
        print('invite from inv failed, invite from query')
        invite_through_query(sender,sender_team_id,queryID,nrow_next)
      }
    }# end of second if
  } # end for loop
  
  
  # check who is not assigned to teams
  count=1
  project_Allusers <- project_users$user_id
  unassigned_users <- vector()
  for (i in 1:length(project_Allusers)){
    user=project_Allusers[i]
    if (sum(teams_users==user)>1){print(user)}
    if (sum(teams_users==user)==0){
      unassigned_users[count]=user
      count=count+1
    }
  }
  
  # place unassigned user at empty teams
  # randomize this process by shuffling the unassigned vector
  unassigned_users <- sample(unassigned_users)
  for (j in 1:length(unassigned_users)) {
    for (k in 1:project_team_number){
      teams_sizes[k]=sum(teams_users[k,]>0)
    }
    user=unassigned_users[j]
    min_team_size=min(teams_sizes)
    min_size_team_number=(1:project_team_number)[teams_sizes==min_team_size][1]
    teams_users[min_size_team_number,min_team_size+1]=user
    
  }
  
  # get project skill array:
  # array index: [team number, team member, skill id]
  team_project_skills <- array(0,dim=c(project_team_number,project_team_size_upper,num_skills))
  for (l in 1:nrow(teams_users)){
    for (m in 1:ncol(teams_users)){
      if (teams_users[l,m]!=0){
        user_nrow=(1:nrow(project_users))[project_users$user_id==teams_users[l,m]]
        team_project_skills[l,m,]=as.integer(project_users[user_nrow,77:85])
      }
    }
  }
  
  team_project_skills[is.na(team_project_skills)]=0
  
  # all team member skills overall gini
  team_gini <- vector()
  for (k in 1:project_team_number){
    teams_sizes[k]=sum(teams_users[k,]>0)
    team_project_skills_vector=na.omit(c(team_project_skills[k,,]))
    team_gini[k]=gini(team_project_skills_vector)
  }
  average_gini[rep] <- mean(team_gini)
  
  # get gender data
  teams_gender <- matrix(0,project_team_number,2) # male number. female number
  gender_simpson <- vector()
  for (l in 1:nrow(teams_users)){
    for (m in 1:ncol(teams_users)){
      if (teams_users[l,m]!=0){
        user_nrow=(1:nrow(project_users))[project_users$user_id==teams_users[l,m]]
        if (project_users$gender[user_nrow]=="M"){
          teams_gender[l,1]=teams_gender[l,1]+1
        }
        if(project_users$gender[user_nrow]=="F"){
          teams_gender[l,2]=teams_gender[l,2]+1
        }
      }
    }
  }
  for (u in 1:project_team_number){
    gender_simpson[u] <- diversity(teams_gender[u,],index='simpson',MARGIN=1,base=exp(1))
  }
  average_gender_simpson[rep]=mean(gender_simpson)
  
  # get international? data
  teams_itn <- matrix(0,project_team_number,2) # internationsl,not international
  itn_simpson <- vector()
  for (l in 1:nrow(teams_users)){
    for (m in 1:ncol(teams_users)){
      if (teams_users[l,m]!=0){
        user_nrow=(1:nrow(project_users))[project_users$user_id==teams_users[l,m]]
        if (project_users$international[user_nrow]==1){
          teams_itn[l,1]=teams_itn[l,1]+1
        }
        if(project_users$international[user_nrow]==0){
          teams_itn[l,2]=teams_itn[l,2]+1
        }
      }
    }
  }
  for (u in 1:project_team_number){
    itn_simpson[u] <- diversity(teams_itn[u,],index='simpson',MARGIN=1,base=exp(1))
  }
  average_international_simpson[rep]=mean(itn_simpson)
}

# skill CI
skill_CI_mean <- mean(average_gini)
skill_CI_sd <- sd(average_gini)
skill_lowerCI <- skill_CI_mean-1.96*skill_CI_sd/sqrt(length(average_gini))
skill_upperCI <- skill_CI_mean+1.96*skill_CI_sd/sqrt(length(average_gini))
skill_CI <- cbind(skill_lowerCI,skill_upperCI)
skill_CI

#gender CI
gender_CI_mean <- mean(average_gender_simpson)
gender_CI_sd <- sd(average_gender_simpson)
gender_lowerCI <- gender_CI_mean-1.96*gender_CI_sd/sqrt(rep_num)
gender_upperCI <- gender_CI_mean+1.96*gender_CI_sd/sqrt(rep_num)
gender_CI <- cbind(gender_lowerCI,gender_upperCI)
gender_CI

#international CI
itn_CI_mean <- mean(average_international_simpson)
itn_CI_sd <- sd(average_international_simpson)
itn_lowerCI <- itn_CI_mean-1.96*itn_CI_sd/sqrt(rep_num)
itn_upperCI <- itn_CI_mean+1.96*itn_CI_sd/sqrt(rep_num)
itn_CI <- cbind(itn_lowerCI,itn_upperCI)
itn_CI

#actual
# get team skills array
teams_skills <- project_users %>% 
  select(team_id,user_id,starts_with("project.skill.")) 
team_id_actual <- unique(na.omit(teams_skills$team_id))
teams_actual_users <- data.frame(matrix(0,project_team_number,project_team_size_upper))
colnames(teams_actual_users) <- 1:project_team_size_upper
for (i in 1:length(team_id_actual)){
  #print(i)
  users <- project_users %>% filter(team_id==team_id_actual[i]) %>% select(user_id)
  for (j in 1:nrow(users)){
    teams_actual_users[i,j]=users[j,1]
  }
}

team_project_skills_actual <- array(0,dim=c(project_team_number,project_team_size_upper,num_skills))
for (l in 1:nrow(teams_actual_users)){
  for (m in 1:ncol(teams_actual_users)){
    if (teams_actual_users[l,m]!=0){
      user_nrow=(1:nrow(project_users))[project_users$user_id==teams_actual_users[l,m]]
      team_project_skills_actual[l,m,]=as.integer(project_users[user_nrow,77:85])
    }
  }
}

team_project_skills_actual[is.na(team_project_skills_actual)]=0
team_gini_actual <- vector()
for (k in 1:project_team_number){
  teams_sizes[k]=sum(teams_actual_users[k,]>0)
  team_project_skills_actual_vector=na.omit(c(team_project_skills_actual[k,,]))
  team_gini_actual[k]=gini(team_project_skills_actual_vector)
}
average_gini_actual <- mean(team_gini)
average_gini_actual

# get gender array actual
teams_gender_actual <- matrix(0,project_team_number,2) # male number. female number
gender_simpson_actual <- vector()
for (l in 1:nrow(teams_actual_users)){
  for (m in 1:ncol(teams_actual_users)){
    if (teams_actual_users[l,m]!=0){
      user_nrow=(1:nrow(project_users))[project_users$user_id==teams_actual_users[l,m]]
      if (project_users$gender[user_nrow]=="M"){
        teams_gender_actual[l,1]=teams_gender_actual[l,1]+1
      }
      if(project_users$gender[user_nrow]=="F"){
        teams_gender_actual[l,2]=teams_gender_actual[l,2]+1
      }
    }
  }
}
for (u in 1:project_team_number){
  gender_simpson_actual[u] <- diversity(teams_gender_actual[u,],index='simpson',MARGIN=1,base=exp(1))
}
average_gender_simpson_actual=mean(gender_simpson_actual)

# get international? actual
teams_itn_actual <- matrix(0,project_team_number,2) # internationsl,not international
itn_simpson_actual <- vector()
for (l in 1:nrow(teams_actual_users)){
  for (m in 1:ncol(teams_actual_users)){
    if (teams_actual_users[l,m]!=0){
      user_nrow=(1:nrow(project_users))[project_users$user_id==teams_actual_users[l,m]]
      if (project_users$international[user_nrow]==1){
        teams_itn_actual[l,1]=teams_itn_actual[l,1]+1
      }
      if(project_users$international[user_nrow]==0){
        teams_itn_actual[l,2]=teams_itn_actual[l,2]+1
      }
    }
  }
}
for (u in 1:project_team_number){
  itn_simpson_actual[u] <- diversity(teams_itn_actual[u,],index='simpson',MARGIN=1,base=exp(1))
}
average_international_simpson_actual=mean(itn_simpson_actual)


gender_CI
average_gender_simpson_actual
itn_CI
average_international_simpson_actual
skill_CI
average_gini_actual

