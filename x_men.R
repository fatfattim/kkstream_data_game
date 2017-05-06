##### Load Data from csv #####
library("rjson")

event_train <-
  read.csv('~/Desktop/data_game/events_train.csv', stringsAsFactors = F)
labels_train  <-
  read.csv('~/Desktop/data_game/labels_train.csv', stringsAsFactors = F)

con<-file('~/Desktop/data_game/titles.json',encoding="UTF-8")
video_meta  <- fromJSON(file = con)

output_sample  <-
  read.csv('~/Desktop/data_game/sample.csv', stringsAsFactors = F)

event_test <-
  read.csv('~/Desktop/data_game/events_test.csv', stringsAsFactors = F)

summary(event_train)

#### To check which column has missing data####
for (i in 1:ncol(event_train)) {
  row <- event_train[, i]
  # do stuff with row
  if (sum(is.na(row)) > 0) {
    print(colnames(event_train[i]))
  }
}

#Title_id ==0 is valid item, and there is no invalid data of titlename
nrow(subset(event_train, event_train$title_name == ''))

#To find how many user in train table (41539 ~ 103845, count of them is 62307)
nrow(unique(event_train[c("user_id")]))

#### To build prediction model from events_train.csv ####
length(unique(event_train$user_id))
user_id_array <- unique(event_train$user_id)
subset(event_train, event_train$user_id == user_id_array[1])

#There are 730 items of 2017 year, we should use recommendation system which select one item for user

# Step 1 : Convert json list into data frame, it make us understand data more easily

converterFun <- function(video_meta) {
  
  output <- data.frame()
  
  videoIds <- names(video_meta)
  i <- 1
  for (titleId in videoIds) {
    test <- video_meta[[titleId]]
    testNames <- names(test)
    
    for (columnName in testNames) {
      value <- test[[columnName]]
      print(columnName)
      if(columnName == 'total_episode_counts') {
        concatedStrArray <- paste(value)
        value <- paste(concatedStrArray, collapse = ',')
      } else if (is.list(value)) {
        value <- toString(names(value))
      } else {
        value <- toString(value)
      }
      output[i , columnName] <- value
    }
    i <- i + 1
  }
  return(output)
}

# title_id frequency
# 123     699
famousVideoFun <- function(df) {
  df <- as.data.frame(table(df$title_id))
  output <- (df[with(df, order(-Freq)), ])
  names(output)[1]<-"title_id"
  return (output)
}

compareResultFun <- function(compareData, resultData) {
  matched <- length(labels_train[(labels_compare$title_id == labels_train$title_id ), ]$title_id)
  result <- matched / length(labels_train$title_id)
  return(result)
}

df_video_meta <- converterFun(video_meta)
#Please browse it by google sheet, it have encoding problem in Microsoft Excel
#write.csv(output, file = "~/Desktop/data_game/meta_data.csv")
output_user_id <- sort(unique(event_test[c("user_id")])$user_id)

#### Solution 1 : 2017 Famous videos ####
# 1. Get famous videos from label_train 
# 2. Give the famous videos to test data
# 3. Check user had been watched famous video (How to define it ?)
# 0.10323

famous2017 <- famousVideoFun(labels_train)
#Bad performance: http://stackoverflow.com/questions/16723325/assignment-to-big-r-data-frame
setFamousVideoFun <- function (userIds, famous) {
  output <- data.frame()
  i <- 1
  for (id in userIds)  {
    output[i , 'user_id'] <- 0
    output[i , 'title_id'] <- famous$title_id[1]
    
    i <- i + 1
  }
  return (output)
}
filterWatchedVideoFun <- function(output, df_test, famous) {
  
  i <- 1
  shrink_output <- output

  while(length(shrink_output$title_id) > 0 & i < length(famous$title_id)) {
    
    titleId <- as.numeric(as.character(famous$title_id[i]))
    
    df_watched <- df_test[which(as.numeric(df_test$title_id) == titleId), ]
    shrink_output <- shrink_output[shrink_output$user_id %in% unique(df_watched$user_id), ]
    
    output$title_id[output$user_id %in% shrink_output$user_id] <- titleId

    print(titleId)
    print(length(shrink_output$title_id))
    i <- i + 1
  }

  return(output)
}
output <- output_sample

output <- filterWatchedVideoFun(output, event_test, famous2017)

unique(output$title_id)

library(stringr)
output$user_id <- str_pad(output$user_id, 8, pad = "0")
output$title_id <- str_pad(output$title_id, 8, pad = "0")
write.csv(output, file = "~/Desktop/data_game/upload.csv", row.names=FALSE,  quote = FALSE)

#### Solution 2 : To trace user not watch complete video ####
# 1. Get famous videos from label_train 
labels_compare <- labels_train
labels_compare$title_id <- 669
test <- compareResultFun(labels_compare, labels_train)

notmatched <- labels_train[(labels_compare$title_id != labels_train$title_id ), ]
notmatched$user_id[1]
test <- event_train[which(event_train$user_id == notmatched$user_id[1]), ]
data <- df_video_meta[which(as.numeric(df_video_meta$title_id) == notmatched$title_id[1]), ]

matched <- length(labels_train[(labels_compare$title_id == labels_train$title_id ), ]$title_id)

#### To handle test data from events_test.csv ####
summary(unique(event_test[c("user_id")]))
