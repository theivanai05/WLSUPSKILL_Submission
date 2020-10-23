#============================================================================================================   
#Appending the Answers back to the Test Data Set 
#Creating Data Frame of multiple variable/column using Vector  
#============================================================================================================     

#New Day 1 Questions answered Data Set  
# colnames(ASSES_PS_EVAL_GB_28)
# [1] "question_id"       "country"           "org_id"            "role_id"           "submission_utc_ts" "no_of_trials"      "points_earned"    
# [8] "masked_user_id"    "tag1"              "tag2"              "tag3"              "tag4"              "tag5"              "tag6"             
# [15] "tag7"   

#colnames(Qns_Reco_Day_50Q)
#[1] "masked_user_id"  "question_id"     "question_tags"   "country"         "rating"          "answer_datetime"
#[7] "answer_date"     "qns_ans" 

require(tidyverse)
modelAssesmenteval =  data.frame(user=character(),
                                 viewedContent=character(),
                                 recommended=character(),
                                 viewcount=numeric(),
                                 matchcount=numeric(),
                                 matchpercentage=numeric(),
                                 stringsAsFactors=FALSE)

modelTageval =  data.frame(user=character(),
                           viewedContent=character(),
                           recommended=character(),
                           viewcount=numeric(),
                           matchcount=numeric(),
                           matchpercentage=numeric(),
                           stringsAsFactors=FALSE)

zz=0
ASSESS_W_RECO_ANS = ASSES_PS_EVAL_GB_28

for (zz in 1:30) 
  {
# ==> Day1 END of Day Pulse Score Creation <=  
  recDate = recDate + 1
print(paste("running model for " , recDate))
# source("Code/PS_Preprocessor.R")
# o/p df_final_ps_score_set

## Calling the Pulse Score Code now.. 
FOR_PS = ASSESS_W_RECO_ANS
source("Code/PulseScoreCalculator.r")
# o/p df_final_ps_score_set

#Removing Variables from previous code 
rm(df,df_expanded_tag_set, df_ps_comp_set, df_question_diff_level, df_tag_fullset, df_tag_penalty_set,dt_cntry_full_data,dt_correct_ans, dt_dump,dt_final_user_ques_set)
rm(dt_tag_fullset,gb,kSet,penalty_map,tag_set,bands,c,cntDataSet,i,k,kValues,lvl,partitions,penalty_set,scaleValue,tag,x,y,z)


## Prediction and Generation of TAGS 

df_final_ps_score_set[df_final_ps_score_set ==' '] <- NA
df_final_ps_score_set = df_final_ps_score_set %>% na.omit()
temp_all <- as(df_final_ps_score_set,"realRatingMatrix")

View(df_final_ps_score_set)

# Training Pulse Score, removing the test users ==> This ensures that the dimensions are all the same 
# for the training and Prediction... 
test_all  = temp_all[Demo_UsersDay0,]

#Predicting Recommendations for the next day
pre_pop<- predict(rec_pop, test_all ,type = "ratings")        #popular
pre_rereco <- predict(rec_rereco, test_all ,type = "ratings") #rerecommender

# Day 0 combined recommendation : 
predit_Rereco_op = as(pre_rereco,"data.frame")
#write_csv(predit_Rereco_op, "predict_rereco.csv")
PulseScore_Bottom_10_TAGS = predit_Rereco_op %>% group_by(user) %>% arrange(rating)%>% slice(1:10)
predit_Pop_op = as(pre_pop,"data.frame")
Popular_Top_10 = predit_Pop_op %>% group_by(user) %>% arrange(desc(rating))%>% slice(1:10)

## Generating 10 taged Recommendations with Rerecommender  + Popular Recommendations. 

ten_tag_reco = PulseScore_Bottom_10_TAGS %>% group_by(user) %>% count()
t=1
x=0
Recommended_Tags = data.frame()
filler = data.frame()
for (t in 1:nrow(ten_tag_reco)) 
{ if (ten_tag_reco[t,]$n != 10)
{
  x=10-ten_tag_reco[t,]$n
  filler = Popular_Top_10 %>% filter(user == ten_tag_reco[t,]$user) %>% slice(1:x)
}
  Recommended_Tags = rbind(PulseScore_Bottom_10_TAGS,filler)
  t <- t + 1 
}

# Setting the field names to match with the rest of the code. 
names(Recommended_Tags)[names(Recommended_Tags) == "user"] <- "masked_user_id"
names(Recommended_Tags)[names(Recommended_Tags) == "item"] <- "question_tags"
Recommended_Tags = Recommended_Tags %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)


## Creating theNezt Day's Questions 
## Creating the Question, COuntry, Tag Master

      Assess = ASSESS_W_RECO_ANS
      
      df = melt(Assess, id.vars = c("question_id","country","org_id","role_id","submission_utc_ts","no_of_trials","points_earned","masked_user_id"))
      names(df)[10] = "question_tags"
      df = within(df, rm("variable"))
      
      C_Q_Tag_M = unique(df %>% select("country","question_id","question_tags"))
      C_Q_Tag_M  = C_Q_Tag_M  %>% na_if(" ") %>% na.omit() 
      
      ## Updating questions that were answered ; keeping on the the latest questions answered. 
      u_q_t_M = unique(Assess[,c("masked_user_id","question_id", "no_of_trials","points_earned","submission_utc_ts")])  #  "points_earned" 
      u_q_t_M[, eval("answer_datetime"):=ymd_hms(u_q_t_M$submission_utc_ts)]
      u_q_t_M[, eval("answer_date"):=as_date(u_q_t_M$answer_datetime)]
      u_q_t_M[,c("submission_utc_ts")] <- NULL
      require(data.table)
      u_q_t_M <- data.table(u_q_t_M, key=c("masked_user_id","question_id"))
      setorder(u_q_t_M, masked_user_id,question_id,-answer_datetime)
      u_q_t_M = u_q_t_M[, head(.SD, 1), by=c("masked_user_id","question_id")]
      
      ####1)  To Create the Matrix for Questions ANswered by Users D.T = u_q_t_M
      new_var <- "qns_ans"
      u_q_t_M[,(new_var):=dplyr::case_when(
        qns_ans = points_earned == 10 ~ 1,
        qns_ans = points_earned == 5 ~ 1,
        qns_ans = points_earned == 0 ~ 0)]
      
      ## After Recommendation, need to pull out the Questions now: 
      Questions_Recommended = merge(C_Q_Tag_M, Recommended_Tags, by= c("question_tags"),allow.cartesian=TRUE)
      
      ## Merging with u_q_t_m to find out which of the questions recommended have already been answered by users 
      Questions_Recommended$masked_user_id <- as.factor(Questions_Recommended$masked_user_id)
      Questions_Recommended$question_id <- as.factor(Questions_Recommended$question_id)
      u_q_t_M$question_id <- as.factor(u_q_t_M$question_id)
      Questions_Recommended_w_qns_ans = unique(merge(Questions_Recommended,u_q_t_M,  by= c("masked_user_id","question_id"),all.x = TRUE))
      Questions_Recommended_w_qns_ans[,c("no_of_trials","points_earned")] <- NULL
      Questions_Recommended_w_qns_ans = unique(Questions_Recommended_w_qns_ans)
      
      #filter(u_q_t_M, qns_ans == "1")
      #filter(pulsescore_Master_GB_O28_DO, score != "0")
      Questions_Recommended_w_qns_ans_GB_Users = filter(Questions_Recommended_w_qns_ans, country == "GB")
      setorder(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id,question_id,-qns_ans)
      
      
      #finding Qns Recommended for Users on Day 1  
      Qns_Reco_Day = filter(Questions_Recommended_w_qns_ans_GB_Users, masked_user_id %in% Demo_UsersDay0) %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)
      Qns_Reco_Day$qns_ans[Qns_Reco_Day$qns_ans == 1] <- 3 # questions already answered 
      Qns_Reco_Day$qns_ans[is.na(Qns_Reco_Day$qns_ans)] <- 2 # Questions not yet Seen by User 
      Qns_Reco_Day$qns_ans[Qns_Reco_Day$qns_ans == 0] <- 1 # Questions Received but not Attempted 
      Qns_Reco_Day =   Qns_Reco_Day %>% group_by(masked_user_id)%>%arrange(qns_ans) # Arranging the Questions so that RECO is in the order of 1 , 2, 3 
      
      # Remove Duplicate Questions for same user 
      Qns_Reco_Day = Qns_Reco_Day  %>% distinct(question_id, masked_user_id , .keep_all = TRUE)
      
      ## Finding out the top 50 Questions for each User 
      ## Picking up the first 50 lines for each user 
      # ==> https://stackoverflow.com/questions/14800161/select-the-top-n-values-by-group
      
      #To be used for Final Recommendation 
      #To be used Question and Tag recommendations and Evaluation ==> 50 questions per User <== 
      require(data.table)
      d <- data.table(Qns_Reco_Day, key="masked_user_id")
      Qns_Reco_Day_50Q = d[, head(.SD, 50), by=masked_user_id]

# Synthetatic Data create_activity_data_from_recomm_questions()
      SYNTHETIC_activity_data <- create_activity_data_from_recomm_questions(Qns_Reco_Day_50Q,recDate)
      
      names(SYNTHETIC_activity_data)[names(SYNTHETIC_activity_data) == "question_tags"] <- "tag1"
      colnames_df <- t(t(colnames(ASSES_PS_EVAL_GB_28)))
      setcolorder(SYNTHETIC_activity_data, colnames_df)
      SYNTHETIC_activity_data[,c("rating","qns_ans","answer_datetime","answer_date")] <- NULL
      
      #Adding observations using rbind() function  
      ASSESS_W_RECO_ANS<- rbind(ASSESS_W_RECO_ANS,SYNTHETIC_activity_data)

# model testing logic

#SYNTHETIC_activity_data Date Creation 
#te_assess_dt_7_GB_org28 Date Creation already exists 

# convert timestamp into date time format
SYNTHETIC_activity_data$eventDateTime = as_datetime(SYNTHETIC_activity_data$submission_utc_ts,format="%Y-%m-%d %H:%M:%S.000")
# create new column  event date
SYNTHETIC_activity_data$eventDate = as.Date(substr(SYNTHETIC_activity_data$eventDateTime,1,10))
# create new column  event time
SYNTHETIC_activity_data$eventTime = substr(SYNTHETIC_activity_data$eventDateTime,12,22)


testAssesmentUsers =  te_assess_dt_7_GB_org28 %>% filter(masked_user_id %in% Demo_UsersDay0) %>% filter(eventDate == recDate) %>% group_by(masked_user_id) %>% count()
testAssesmentDataset = te_assess_dt_7_GB_org28 %>% filter(eventDate == recDate)
recAssesmentDatatestset = SYNTHETIC_activity_data %>% filter(eventDate == recDate)
names(recAssesmentDatatestset)[names(recAssesmentDatatestset) == "tag1"] <- "question_tags"

#viewCount <- 0
#userViewedQuestions <- 0

  for (user in testAssesmentUsers$masked_user_id){
    viewCount = testAssesmentUsers [ testAssesmentUsers$masked_user_id == user,2]
    recAssesmentDatatestset[recAssesmentDatatestset$masked_user_id != user,1]
    #recAssesmentDatatestset = data_memory(recAssesmentData$masked_user_id,recAssesmentData$question_id,index1 = T)
    #recAssesmentData$predicted =  rAssesment$predict(recAssesmentDatatestset,out_memory())
    recQuestions =  unique(recAssesmentDatatestset %>% arrange(desc(recAssesmentDatatestset$masked_user_id == user )) %>% select("question_id"))
    userViewedQuestions = unique(testAssesmentDataset%>% filter(masked_user_id == user) %>% select("question_id"))
    recTags =  unique(recAssesmentDatatestset %>% arrange(desc(recAssesmentDatatestset$masked_user_id == user )) %>% select("question_tags") %>% na.omit())
    userViewedTags = unique(testAssesmentDataset%>% filter(masked_user_id == user) %>% stack(c("tag1","tag2","tag3","tag4","tag5","tag6","tag7")) %>% na.omit() %>% select("values") %>% rename("question_tags" = "values"))
    
    #print(paste("user data for : ",user))
    
    match_q =     userViewedQuestions %>% filter(question_id %in% recQuestions$question_id)  
    matchper_q = (nrow(match_q) /viewCount) * 100
    
    match_t =     userViewedTags %>% filter(question_tags%in% recTags$question_tags)  
    matchper_t = (nrow(match_t) /viewCount) * 100
    
    rec1 = data.frame(user = user,
                      viewedContent_q=paste0(userViewedQuestions $question_id, collapse=","),
                      recommended_q=paste0(recQuestions$question_id, collapse=","),
                      viewcount=viewCount$n,
                      matchcount_qns=nrow(match_q),
                      matchpercentage_qns= matchper_q$n,
                      recdate = as.character(recDate))
    
    rec2 = data.frame(user = user,
                      viewedContent_t=paste0(userViewedTags$question_tags, collapse=","),
                      recommended_t=paste0(recTags$question_tags, collapse=","),
                      viewcount=viewCount$n,
                      matchcount_tag=nrow(match_t),
                      matchpercentage_tag= matchper_t$n,
                      recdate = as.character(recDate))
    #print(userViewedQuestions)
    #print(rec1)
    
    modelAssesmenteval = rbind(modelAssesmenteval,rec1)
    modelTageval = rbind(modelTageval,rec2)
  }

zz <- zz+1
}

#View(modelAssesmenteval)
#View(modelTageval)
print(paste("Completed Processing of Model Evaluation  !!"))

#csvFileName <- paste("~/wls26092020/wls26092020/Data/pulsescore/pulse_score_",recDate,".csv")
csvFileName <- paste("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls26092020/Data/modelAssesmenteval_Ending_",recDate,".csv")
file_path <- csvFileName
#file_path
write.csv( modelAssesmenteval,file_path)

#csvFileName <- paste("~/wls26092020/wls26092020/Data/pulsescore/pulse_score_",recDate,".csv")
csvFileName <- paste("C:/Users/theiv/Documents/2019_ISS_MTech_EBAC/Capstone Project/FYP_TeamsStreamz/wls26092020/Data/modelTageval_Ending_",recDate,".csv")
file_path <- csvFileName
#file_path
write.csv( modelTageval,file_path)

print("Evaluation Tables Printed and Saved.")



