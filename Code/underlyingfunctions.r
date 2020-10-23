#==========================================================================================================#
###Function Libraries : 
#==========================================================================================================#

# computes average, mean absolute error
# each row contains prediction, actual, prediction, actual etc, hence errors are just the diff between consecutive cells
avgMAE = function(preds) {
  plist = unlist(preds)
  errors = sapply(1:(length(plist)/2),function(i) abs(plist[i*2-1]-plist[i*2]))
  errors = errors[errors != Inf]
  mean(errors,na.rm=TRUE)
}

showCM = function(preds, like) {
  plist = unlist(preds)
  cnts = sapply(1:(length(plist)/2), function(i) {
    pred = plist[i*2-1] ; actual = plist[i*2]
    if (!is.na(pred) & !is.nan(actual)) {
      if (pred>=like) {if(actual>=like) c(1,0,0,0) else c(0,1,0,0)}
      else if(actual<like) c(0,0,1,0) else c(0,0,0,1) 
    } else c(0,0,0,0)
  })
  s = rowSums(cnts)   #returns cnts for: TP, FP, TN, FN
  
  cat(sprintf("TN=%5d FP=%5d\n",s[3],s[2]))
  cat(sprintf("FN=%5d TP=%5d  (total=%d)\n",s[4],s[1], sum(s)))
  cat(sprintf("accuracy  = %0.1f%%\n",(s[1]+s[3])*100/sum(s)))
  cat(sprintf("precision = %3.1f%%\n",s[1]*100/(s[1]+s[2])))
  cat(sprintf("recall    = %3.1f%%\n",s[1]*100/(s[1]+s[4])))
}

#Creating a Negative Filter
'%notin%' <- Negate('%in%')

##Function to create Data for Questions answered from Recommendations.
create_activity_data_from_recomm_questions = function(x,recDate) {
  print(paste0("Inside create activity data from recomm questions function "))
  df = data.table(x)
  recomm_size = nrow(df)
  print(paste0("Recomm questions size = ", recomm_size))
  print(paste0("Col names = ", colnames(df)))
  country = rep("GB",recomm_size)
  org_id = rep(28, recomm_size)
  role_id =  rep(1,recomm_size)
  date = paste(recDate," 00:00:00.000")
  datetime_val = date
  trials = rep(1,recomm_size)
  points = rep(10,recomm_size)
  
  
  
  df[,"org_id" := 28]
  df[,"role_id" := 1] 
  df[,"country"]<- country
  df[,"points_earned" := 10]
  df[,"no_of_trials" := 1]
  df[,"submission_utc_ts" := date]
  df[,"tag2" := " "]
  df[,"tag3" := " "]
  df[,"tag4" := " "]
  df[,"tag5" := " "]
  df[,"tag6" := " "]
  df[,"tag7" := " "]
  print(paste0("SYNTHETIC activity data set size = ",nrow(df)))
  print(paste0("Col names = ", colnames(df)))
  return(df)
}
