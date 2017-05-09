##### Load Data from csv #####
library("rjson")
library(anytime)
library(stringr)

event_train <-
  read.csv('~/Desktop/data_game/events_train.csv', stringsAsFactors = F)
labels_train  <-
  read.csv('~/Desktop/data_game/labels_train.csv', stringsAsFactors = F)

con<-file('~/Desktop/data_game/titles.json',encoding="UTF-8")
video_meta  <- fromJSON(file = con)

output_sample  <-
  read.csv('~/Desktop/data_game/sample.csv', stringsAsFactors = F)

train_mild_user <-
  read.csv('~/Desktop/data_game/train_mild_user.csv', stringsAsFactors = F)

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
  matched <- length(resultData[(resultData$title_id == compareData$title_id ), ]$title_id)
  result <- matched / length(resultData$title_id)
  return(result)
}

getDataByUserIdFun <- function(df, userId) {
  return(df[which(df$user_id == userId), ])
}

findTitleIdByUserIdFun <- function(id, twoColumnDF) {
  index <- match(c(id), twoColumnDF[, 1])
  return(twoColumnDF[index, ]$title_id)
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

#### Solution 2 : To trace user not watch complete video ####
# 1. Get famous videos from label_train 
# 2. (count, rowIndex) : 4666 , 13569
labels_compare <- labels_train
labels_compare$title_id <- 669

matched <- labels_train[(labels_compare$title_id == labels_train$title_id ), ]
toGetDFByLabelDataFun <- function (label_data, train_data) {
  count <- 0
  result <- data.frame()
  for(i in 1:nrow(label_data)) {
    row <- label_data[i,]
    test <- getDataByUserIdFun(train_data, row$user_id)
    if(length(which(test$title_id == row$title_id)) >0) {
      test[["2017_title_id"]] <- row$title_id
      result <- rbind(result,test)
      cat(sprintf("(count, rowIndex) %s , %s\n", count, i))
      count <- count + 1
    }
  }
  return (result)
}

# To find user will watch previous video before 2016 in 2017
# To get sample data from not matmatched
notmatched <- labels_train[(labels_compare$title_id != labels_train$title_id ), ]
notmatched <- notmatched[sample(nrow(notmatched), 20), ]
dd <- toGetDFByLabelDataFun(notmatched, event_train)
toConcludeDataByOneUserFun <- function(user_data) {
  output <- data.frame()
  i <- 1
  for(titleId in unique(user_data$title_id)) {
    tempData <- user_data[user_data$title_id %in% titleId,]
    tempData <- tempData[(order(format(anytime(tempData$time), format="%Y%m%d%H"))),]
    
    output[i, 'title_id'] <- titleId
    output[i, 'title_name'] <- tempData$title_name[1]
    output[i, 'last_watch_time'] <- tempData[length(tempData$time), ]$time
    output[i, 'total_watch_time'] <- sum(tempData$watch_time)
    print(titleId)
    i <- i + 1
  }
  return(output)
}

getLastWatchedTitleIdByOneUserFun <- function(user_data) {
  temp <- user_data[(order(format(anytime(user_data$time), format="%Y%m%d%H"))),]
  titleId <- temp[length(temp$time), ]$title_id
  return(titleId)
}

getResultByLastWatchedTitleId <- function(userData , trainData, limit) {
  output <- data.frame()
  user_ids <- userData[, 1]
  i <- 1
  for(userId in user_ids) {
    
    userId <- as.numeric(userId)
    titleId <-getLastWatchedTitleIdByOneUserFun(trainData[trainData$user_id == userId, ])
    
    if ((i %% 100) == 0) {
      print(i)  
    }
    
    output[i, names(userData)[1]] <- userId
    output[i, names(userData)[2]] <- titleId
    
    i <- i + 1
    if(i > limit && limit > 0) {
      break;
    }
  }
  return(output)
}

#output <- toConcludeDataByOneUserFun(dd)
output0507 <- getResultByLastWatchedTitleId(output_sample, event_test, -1)
result <- compareResultFun(output, labels_train)

#### Solution 3 : Compare Solution 1 with 2 ####
# 1. Add sum of total function for video meta data
sumOfTotalEpisodeFun <- function(df) {
  df$total_episode_counts <- lapply(df$total_episode_counts, function(x) {
    sum(as.integer(strsplit(x, ",")[[1]]))
  })
  df$total_episode_counts <- unlist(df$total_episode_counts)
  return(df)
}

findMildUserFun <- function(df, mildCount) {
  output <- data.frame()
  usertable <- as.data.frame(table(df$user_id))
  colnames(usertable)[1] <- "user_id"
  print(length(1:nrow(usertable[usertable$Freq < mildCount, ])))
  output <- usertable[usertable$Freq < mildCount, ]
  return (output)
}

sortedTest <- event_train[with(event_train, order(user_id)), ]
mildUser <- findMildUserFun(sortedTest , 3)

concludeDataByMildUser <- function(mildUser, train_data, train_result) {
  output <- train_data[train_data$user_id %in% as.vector(mildUser$user_id),]
  output['result'] <- -1
  i<-1
  for(userId in mildUser$user_id) {
    output[output$user_id == userId, ]$result <- train_result[train_result$user_id == userId, ]$title_id
    i <- i + 1
    print(i)
  }
  return(output)
}

output <- concludeDataByMildUser(mildUser, event_train, labels_train)

getVideoMetaByTitleIdsFun <- function(videoMeta, titleIds) {
  return(videoMeta [as.integer(videoMeta$title_id)  %in%  c(titleIds), ])
}

#### To get beautiful meta data of video ####
beautiful_meta <- sumOfTotalEpisodeFun(df_video_meta)
beautiful_meta$title_id <- as.integer(beautiful_meta$title_id)
drops <- c("title_type","content_providers" , "copyright", "ordered_casts", "is_ending", "updated_at", "reverse_display_order", "content_agents" , "stills" , "tags" , "title_aliases", "wiki_zh" , "extra_title_id" , "characteristic" , "cover", "ost" , "producers", "wiki_orig", "has_series")
beautiful_meta <- beautiful_meta[ , !(names(beautiful_meta) %in% drops)]
beautiful_meta[beautiful_meta$country == "不使用", ]
names(beautiful_meta)

#### To seperate group ####
train_set1 <- subset(event_train, event_train$user_id <= 57116)
train_set1$time <- gsub("[^0-9]", "", train_set1$time)
train_set1$time <- as.numeric(train_set1$time)

label_set1 <- subset(labels_train, labels_train$user_id <= 57116)

train_set2 <- subset(event_train, event_train$user_id > 57116 & event_train$user_id <= 72692)
train_set3 <- subset(event_train, event_train$user_id > 72692 & event_train$user_id <= 88268)
train_set4 <- subset(event_train, event_train$user_id > 88268)
#check sum
nrow(train_set1) + nrow(train_set2) + nrow(train_set3) + nrow(train_set4) 
train_set1$time <- format(anytime(train_set1$time), format="%Y%m%d%H")

#This performance is better than getResultByLastWatchedTitleId function
mylist <- split(train_set1, train_set1$user_id)
haha1 <- do.call(rbind.data.frame, lapply(mylist, function(user_data) {
  output <- data.frame()
  temp <- user_data[(order(user_data$time)), ]
  titleId <- temp[length(temp$time), ]$title_id
  #Get last watched video
  output[1, 'user_id'] <- temp$user_id[1]
  output[1, 'title_id'] <- titleId
  print(titleId)
  output
}))

investigate <- label_set1[(haha1[haha1$user_id == label_set1$user_id, ]$title_id != label_set1$title_id), ]

result <- train_set1[train_set1$user_id %in% investigate$user_id[2], ]

#### To build recommendation data ####

# filter by train data
recommendationVideo2017 <- beautiful_meta[beautiful_meta$title_id %in% unique(labels_train$title_id), ]
# filter by na data
recommendationVideo2017 <- recommendationVideo2017[!is.na(recommendationVideo2017$country), ]
# merge to get Freq, we use it to be rating 
recommendationVideo2017 <- merge(recommendationVideo2017, famous2017, "title_id") 
names(recommendationVideo2017)

#### Train Model depends on group data####
global.env <- new.env()
global.env$video_meta <- beautiful_meta
global.env$recommend <- recommendationVideo2017

haha1 <- do.call(rbind.data.frame, lapply(mylist, function(user_data) {
  output <- data.frame()
  sortedTimeData <- user_data[(order(user_data$time)), ]
  titleId <- sortedTimeData[length(sortedTimeData$time), ]$title_id
  #default
  output[1, 'title_id'] <- titleId  
  output[1, 'user_id'] <- sortedTimeData$user_id[1]
  # Watched Only one video, and this video is episode
  video_meta <- get('video_meta', envir=global.env)
  recommend <- get('recommend', envir=global.env)

  if(length(unique(sortedTimeData$title_id)) == 1) {
    episode_count <- video_meta[video_meta$title_id == titleId, ]$total_episode_counts

    if(episode_count < 2) {
      cat(sprintf("(titleId, episode_count) %s %s\n", titleId, episode_count))
      #Look country, give highest one, and check user see it or not
      favoriteCountry <- video_meta[video_meta$title_id == titleId, ]$country
      recommendVideos <- recommend[grepl(favoriteCountry, recommendationVideo2017$country), ]
      #Remove watched video
      recommendVideos <- recommendVideos[recommendVideos$title_id != titleId, ]
      #Order
      recommendVideos <- recommendVideos[with(recommendVideos, order(-Freq)), ]
      output[1, 'title_id'] <- recommendVideos[1]
    }
  }
  output
}))

investigate <- label_set1[(haha2[haha2$user_id == label_set1$user_id, ]$title_id == label_set1$title_id), ]
investigate1 <- label_set1[(haha1[haha1$user_id == label_set1$user_id, ]$title_id == label_set1$title_id), ]

# Group data by table way
# as.data.frame(table(result$title_id))

haha <- df_video_meta[grepl("音樂", df_video_meta$themes), ]
#### To calculate system time ####
old <- Sys.time() # get start time
# method here
new <- Sys.time() - old # calculate difference
print(new) # print in nice format

system.time ( {
  event_haha <- event_test[with(event_test, order(user_id, time))]
} )

#### To handle test data from events_test.csv ####
summary(unique(event_test[c("user_id")]))

#### To generate Output data ####
output <- beautiful_meta
output$user_id <- str_pad(output$user_id, 8, pad = "0")
output$title_id <- str_pad(output$title_id, 8, pad = "0")
write.csv(output, file = "~/Desktop/data_game/beautiful_meta.csv", row.names=FALSE,  quote = FALSE)
