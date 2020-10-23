
## Assessments served to GB 
# Mutating Time fields for processing in assess_dt_7. 
assess_dt_7$country = as.factor(assess_dt_7$country)
assess_dt_7$question_id = as.factor(assess_dt_7$question_id)
assess_dt_7$masked_user_id = as.factor(assess_dt_7$masked_user_id)
# convert timestamp into date time format
assess_dt_7$eventDateTime = as_datetime(assess_dt_7$submission_utc_ts,format="%Y-%m-%d %H:%M:%S.000")
# create new column  event date
assess_dt_7$eventDate = as.Date(substr(assess_dt_7$eventDateTime,1,10))
# create new column  event time
assess_dt_7$eventTime = substr(assess_dt_7$eventDateTime,12,22)

assess_dt_7_GB = unique(assess_dt_7 %>% filter (assess_dt_7$country == "GB"))

## Assessments served to GB Org ID 28 alone 
assess_dt_7_GB_org28 = unique(assess_dt_7 %>% filter (assess_dt_7$country == "GB") %>% filter(assess_dt_7_GB$org_id == "28"))

#1) code : Test is May, Jun , Jul 2020 
te_assess_dt_7_GB_org28 = assess_dt_7_GB_org28 %>% filter(eventDate > "2020-05-01")
#> count(te_assess_dt_7_GB_org28)
#n 1: 376
gb_te_users = data.frame(unique(te_assess_dt_7_GB_org28$masked_user_id))

#2) code : Training : April 2019 - April 2020  == 
tr_assess_dt_7_GB_org28 = assess_dt_7_GB_org28 %>% filter(eventDate > "2019-03-31", eventDate < "2020-05-01") #comma same as &
#> count(tr_assess_dt_7_GB_org28)
#n 1: 83206
gb_tr_users = data.frame(unique(tr_assess_dt_7_GB_org28$masked_user_id))

## checking if Test users are in the Training users set 
names(gb_tr_users)[names(gb_tr_users) == "unique.tr_assess_dt_7_GB_org28.masked_user_id."] <- "masked_user_id"
names(gb_te_users)[names(gb_te_users) == "unique.te_assess_dt_7_GB_org28.masked_user_id."] <- "masked_user_id"

Test_USERS_GB = data.frame(gb_tr_users %>% filter (masked_user_id %in% gb_te_users$masked_user_id))

#removing the additional DSs
rm(assess_dt_7_GB,gb_te_users,gb_tr_users)

#=============================================================================================================================
# ====> GENERATING SCORE for the Training PERIOD < ============
#=============================================================================================================================

## Preparation of Data to generate Pulse Score for GB train users 
ASSES_PS_EVAL_GB_28 = tr_assess_dt_7_GB_org28[,-c("eventDate","eventTime","eventDateTime")]
  
## Calling the Pulse Score Code now.. 
FOR_PS = ASSES_PS_EVAL_GB_28
source("Code/PulseScoreCalculator.r")

#Removing Variables from previous code 
rm(df,df_expanded_tag_set, df_ps_comp_set, df_question_diff_level, df_tag_fullset, df_tag_penalty_set,dt_cntry_full_data,dt_correct_ans, dt_dump,dt_final_user_ques_set)
rm(dt_tag_fullset,gb,kSet,penalty_map,tag_set,bands,c,cntDataSet,i,k,kValues,lvl,partitions,penalty_set,scaleValue,tag,x,y,z)

# Finla set of PS score for the day. 
#df_final_ps_score_set
