## Pulse Score has been evaluated using file "PulseScoreCalculator.R"
## NOW FOR model building 

require(recommenderlab)
require(devtools)


df_final_ps_score_set = df_final_ps_score_set %>% na.omit()
temp_all <- as(df_final_ps_score_set,"realRatingMatrix")

# Training Pulse Score, removing the test users ==> This ensures that the dimensions are all the same 
# for the training and Prediction... 
test_all  = temp_all[Demo_UsersDay0,]
train_all = temp_all[] # %notin% c(Test_USERS_GB) 


# - training the Data from latest Pulse Score 
rec_pop <- Recommender(train_all, method = "POPULAR")
rec_rereco <- Recommender(train_all, method = "RERECOMMEND")

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

#PulseScoreCalculator_New30092020.R, Function holds the Recosystem Code
#if Recosystem needs to be used for Recommendation instead. 

# Setting the field names to match with the rest of the code. 
names(Recommended_Tags)[names(Recommended_Tags) == "user"] <- "masked_user_id"
names(Recommended_Tags)[names(Recommended_Tags) == "item"] <- "question_tags"
Recommended_Tags = Recommended_Tags %>% group_by(masked_user_id) %>% arrange(masked_user_id,rating)

#Recommended_Tags_Day0 = Recommended_Tags