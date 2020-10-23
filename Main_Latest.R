## Main File 
#install.packages("recosystem")
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("recommenderlab")
#install.packages("devtools")
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("doSNOW")


## Installing all of the Libraries required : 
  library(tidyverse)
  library(data.table)
  library(dplyr)
  library(reshape2)
  library(recommenderlab)
  library(recosystem)
  library(devtools)
  library(lubridate)
  library(stringr)
  library(doSNOW)
  registerDoSNOW(makeCluster(3, type = "SOCK"))

  set.seed(123)
  
  # Turning off warning for COde Sumbission 
  options(warn=-1)
  
  #Demo Users FOR GUI 
  Demo_UsersDay0 = c("fe5e359d","abd51bd2","9bffe329","8f7b79fd","8100aef3","560d7304","4ffee38a","22408aad","108ae76d","035f412b")
  # is the same as Test_USERS_GB

  
# Reading in the Master files from TeamStreamz
  # Date File Location 
  #setwd("~/Documents/NUS_EBAC/Data") # data location of Mac
  setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls26092020/Data") # location in Lenovo
  load("Base_DS_after_Filereads.RData")
  rm(pulsescore_Master_GB,pulsescore_Master_All) #assessment , views, 
  
# Coming back into current WD 
  #setwd("~/wls26092020/wls26092020/")
  setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls23102020Final")
  
# Sourcing the Underlying Functions 
  source("Code/underlyingfunctions.R")
  
# Sourcing and running the 
  source("Code/users_questions_marks_assessment_viewed.R")
   #u_q_t_M # Users, questions , Trial result master
   #u_d_a_m # Users, decks , actions master 
 
# Running the Pulse Score and Pre Processor 
  #Recommendation Date 
  recDate = as.Date("2020-05-01")
  source("Code/PS_Preprocessor.R")
  # o/p df_final_ps_score_set

# Churning out Recommendations
  source("Code/Recommendations_Latest.R")
  # o/p : Recommended_Tags
  
# Churning out the question and streams to be recommended 
  source("Code/upskill_users_streams_questions_Result.R")
  getwd()
  # o/p : Stream_Reco_Day_50S
  write.csv(Stream_Reco_Day_50S,file="Day1_Upskill_Stream_Recommended.csv")
  saveRDS(Stream_Reco_Day_50S,file="Day1_Upskill_Stream_Recommended.RDS")
  # o/p : Qns_Reco_Day_50Q
  write.csv(Qns_Reco_Day_50Q,file="Day1_Upskill_Question_Recommended.csv")
  saveRDS(Qns_Reco_Day_50Q,file="Day1_Upskill_Question_Recommended.RDS")
  
  print("Questions Recommended : ")
  head(Qns_Reco_Day_50Q,5)
  print("Streams Recommended : ")
  head(Stream_Reco_Day_50S,5)

  print("Please proceed to to uncomment lines '67' & '68'if you want to run the Recommendation Evaluation Code for the Next 30 Days.")  
  
# Output Evalution
  #source("Code/Recommendation_Evaluation.r")
  #print("Please proceed to view the Scores Generated, Recommendation Evaluation is completed")
  
#Saving Workspace 
    setwd("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls23102020Final")
    save.image("workspace_final_23102020.RData")
    #load("workspace_final_23102020.RData")
  
  
  