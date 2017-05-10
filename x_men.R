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

formatTimeFun <- function(df) {
  df$time <- gsub("[^0-9]", "", df$time)
  df$time <- as.numeric(df$time)
  return(df)
}

formated_event_train <- event_train
formated_event_test <- formatTimeFun(event_test)

train_set1 <- subset(formated_event_train, formated_event_train$user_id <= 57116)
label_set1 <- subset(labels_train, labels_train$user_id <= 57116)

train_set2 <- subset(formated_event_train, formated_event_train$user_id > 57116 & formated_event_train$user_id <= 72692)
train_set3 <- subset(formated_event_train, formated_event_train$user_id > 72692 & formated_event_train$user_id <= 88268)
train_set4 <- subset(formated_event_train, formated_event_train$user_id > 88268)

summary(output_sample)
test_set1 <- subset(formated_event_test, formated_event_test$user_id <= 20769)
test_set2 <- subset(formated_event_test, formated_event_test$user_id > 20769)

train_set_list <- list(train_set1, train_set2, train_set3, train_set4)
train_set_list <- list(test_set1, test_set2)
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

#### To build recommendation data ####

# filter by train data
getRecommendationVideoMetaFun <- function(videoMeta, result) {
  output <- videoMeta[videoMeta$title_id %in% unique(result$title_id), ]
  # filter by na data
  output <- output[!is.na(output$country), ]
  # To get famous data
  famous <- famousVideoFun(result)
  # merge to get Freq, we use it to be rating 
  return(merge(output, famous, "title_id"))
}

recommendationVideo2017 <- getRecommendationVideoMetaFun(beautiful_meta, labels_train)

#### Train Model depends on group data####
global.env <- new.env()
global.env$video_meta <- beautiful_meta
global.env$recommend <- recommendationVideo2017
global.env$mean_watched_time <- 1600
global.env$observer_user_id <- vector()
global.env$labels_result <- labels_train

mylist <- split(formated_event_test, formated_event_test$user_id)

#Parameter list of formated time data
  
getRecommendTitleIdFun <- function(favorite_country, recommend, watched_video, default_title) {
  recommendVideos <- recommend[grepl(favorite_country, recommend$country), ]
  #Remove watched video
  recommendVideos <- recommendVideos[!(recommendVideos$title_id %in% watched_video), ]
  #Order
  recommendVideos <- recommendVideos[with(recommendVideos, order(-Freq)), ]
  if(nrow(recommendVideos) > 0) {
    return(recommendVideos$title_id[1])
  }
  else {
    return (default_title)
  }
}

system.time( {
  for(i in 2:2)  {
    global.env$episode_count <- i
    output <- do.call(rbind.data.frame, lapply(train_set_list, function(train_set) {
      mylist <- split(train_set, train_set$user_id)
      do.call(rbind.data.frame, lapply(mylist, function(user_data) {
        output <- data.frame()
        
        sortedTimeData <- user_data[(order(user_data$time)), ]
        historyWatchedVideo <- sortedTimeData
        titleId <- sortedTimeData[length(sortedTimeData$time), ]$title_id
        userId <- sortedTimeData$user_id[1]
        #default
        output[1, 'title_id'] <- titleId  
        output[1, 'user_id'] <- userId
        # Watched Only one video, and this video is episode
        video_meta <- get('video_meta', envir=global.env)
        recommend <- get('recommend', envir=global.env)
        LIMIT_EPISODE <- get('episode_count', envir=global.env)

        videos2016 <- unique(sortedTimeData$title_id)
        
        if(length(videos2016) == 1) { # Legacy 
          # episode_count <- video_meta[video_meta$title_id == titleId, ]$total_episode_counts
          # 
          # if(episode_count < LIMIT_EPISODE) {
          #   #cat(sprintf("(titleId, userId) %s %s\n", titleId, userId))
          #   #Look country, give highest one, and check user see it or not
          #   favoriteCountry <- video_meta[video_meta$title_id == titleId, ]$country
          #   recommendVideos <- recommend[grepl(favoriteCountry, recommend$country), ]
          #   #Remove watched video
          #   recommendVideos <- recommendVideos[recommendVideos$title_id != titleId, ]
          #   #Order
          #   recommendVideos <- recommendVideos[with(recommendVideos, order(-Freq)), ]
          #   output[1, 'title_id'] <- recommendVideos$title_id[1]
          # }
        } else {
          video <- video_meta[video_meta$title_id == titleId, ]
          
          #Single Video
          if(video$total_episode_counts == 1) {
            
            repeat{
              sortedTimeData <- sortedTimeData[sortedTimeData$title_id != titleId, ]
              watchedHistoryCount <- nrow(sortedTimeData)

              if(watchedHistoryCount == 0) {
                
                titleId <- getRecommendTitleIdFun(video$country, recommendationVideo2017, unique(historyWatchedVideo$title_id) , titleId)
                cat(sprintf("(titleId, userId) %s %s\n", titleId, userId))
                output[1, 'title_id'] <- titleId
                break
              }
              
              titleId <- sortedTimeData[watchedHistoryCount, ]$title_id
              video <- video_meta[video_meta$title_id == titleId, ]
              if(!(video$total_episode_counts == 1)) {
                #cat(sprintf("(titleId, userId) %s %s\n", titleId, userId))
                output[1, 'title_id'] <- titleId
                break
              }
            }

          }
          
        }
        output
      }))
    }))
    
    cat(sprintf("(n, result) %s %s\n", i, compareResultFun(output, labels_train)))
  }
  
})

getGroupIdFun <- function(output, historyWatchedVideo) {
  result <- get('labels_result', envir=global.env)
  result_titleId <- result[result$user_id %in% output$user_id, ]$title_id
  if(result_titleId == output$title_id) {
    #cat(sprintf("Group 1 (user_id) %s \n", output$user_id))
    output[1, 'group'] <- 1
  } else {
    if(nrow(historyWatchedVideo[historyWatchedVideo$title_id %in% result_titleId, ]) > 1) {
      #cat(sprintf("Group 2 (user_id) %s \n", output$user_id))
      
      output[1, 'group'] <- 2  
    } else {
      #cat(sprintf("Group 3 (user_id) %s \n", output$user_id))
      output[1, 'group'] <- 3 
    }
  }
  return (output)
}
#To get group
system.time( {
    output <- do.call(rbind.data.frame, lapply(train_set_list, function(train_set) {
      mylist <- split(train_set, train_set$user_id)
      do.call(rbind.data.frame, lapply(mylist, function(user_data) {
        output <- data.frame()
        sortedTimeData <- user_data[(order(user_data$time)), ]
        historyWatchedVideo <- sortedTimeData
        titleId <- sortedTimeData[nrow(sortedTimeData), ]$title_id
        userId <- sortedTimeData$user_id[1]
        output[1, 'title_id'] <- titleId  
        output[1, 'user_id'] <- userId

        output <- getGroupIdFun(output, historyWatchedVideo)
        output
      }))
    }))
})

#to Train model
system.time( {
  for(i in 3:3)  {
    global.env$episode_count <- i
  output <- do.call(rbind.data.frame, lapply(train_set_list, function(train_set) {
    mylist <- split(train_set, train_set$user_id)
    do.call(rbind.data.frame, lapply(mylist, function(user_data) {
      output <- data.frame()
      sortedTimeData <- user_data[(order(user_data$time)), ]
      userId <- sortedTimeData$user_id[1]
      output[1, 'user_id'] <- userId
      result <- get('labels_result', envir=global.env)
      video_meta <- get('video_meta', envir=global.env)

      temp <- sortedTimeData
      repeat {
        
        if(nrow(temp) < 1) {
          break;
        }
        titleId <- temp[nrow(temp), ]$title_id

        output[1, 'title_id'] <- titleId
        titleIds_length <- length(unique(temp$title_id))

        if(titleIds_length > 1) {
          video <- video_meta[video_meta$title_id %in% titleId, ]
          time <- get('episode_count', envir=global.env) * 10 + 60
          n <- get('episode_count', envir=global.env)
          video_totall_duration <- video$total_episode_counts * time * 60
          
          watched_duration <- sum(temp[temp$title_id %in% titleId, ]$watch_time)
          
          #Best parameter
          n <- 6
          #User had been watched this video
          if(video$total_episode_counts < n) {
            cat(sprintf("(user_id, previous_title_id) %s %s\n", userId, titleId))
            #cat(sprintf("(video_totall_duration, watched_duration) %s %s\n", video_totall_duration, watched_duration))
            temp <- temp[temp$title_id != titleId, ]

          } else {
            break;
          }
        } else {
          break;
        }
        
      }

      #output <- getGroupIdFun(output, sortedTimeData)
      output
    }))
  }))
  cat(sprintf("(n, result) %s %s\n", i, compareResultFun(output, labels_train)))
  }
})

#### remain time solution ####
system.time( {
  for(i in 1:1)  {
    global.env$episode_count <- i
    output <- do.call(rbind.data.frame, lapply(train_set_list, function(train_set) {
      mylist <- split(train_set, train_set$user_id)
      do.call(rbind.data.frame, lapply(mylist, function(user_data) {
        output <- data.frame()
        video_meta <- get('video_meta', envir=global.env)
        userId <- user_data$user_id[1]
        output[1, 'user_id'] <- userId
        n <- 1 #get('episode_count', envir=global.env)
        titleId <- kingOf2017Fun(user_data, video_meta, n)
        output[1, 'title_id'] <- titleId
        #cat(sprintf("(userId, titleId) %s %s\n", userId, titleId))
        #output <- getGroupIdFun(output, user_data)
        output
      }))
    }))
    cat(sprintf("(n, result) %s %s\n", i, compareResultFun(output, labels_train)))
  }
})

#### compare data ####
table(original_group$group) / nrow(labels_train)
table(output$group) / nrow(labels_train)
table(output$group) - table(original_group$group)

temp <- output[output$title_id %in% labels_train$title_id, ]
sampleResult <- output[output$group == 2, ]
sampleUserIds <-sampleResult[
  sample(nrow(
    sampleResult), 10), ]

getMergedDFWithResultFun <- function(sampleUserIds , event_train, labels_train) {
  oneUserData <- event_train[event_train$user_id %in% sampleUserIds$user_id, ]
  oneUserData <- oneUserData[order(oneUserData$user_id, oneUserData$time), ]
  oneResultData <- labels_train[labels_train$user_id %in% sampleUserIds$user_id, ]
  
  colnames(oneResultData)[2] <- "result"
  oneMergedData <-merge(oneUserData, oneResultData, "user_id", all = TRUE)
  return(oneMergedData[order(oneMergedData$user_id, oneMergedData$time), ])
}

oneMergedData <- getMergedDFWithResultFun(sampleUserIds, event_train, labels_train)
titleIds <- kingOf2017Fun(oneMergedData[oneMergedData$user_id == 49334 , ], beautiful_meta, 1)

video <- beautiful_meta[beautiful_meta$title_id %in% 66, ]


sampleUserIds <-sampleResult[sample(nrow(sampleResult), 1000), ]
oneOutput <- verifyFun(sampleUserIds, event_train, labels_train)

oneUserData <- event_train[event_train$user_id %in% 47538, ]
titleId <- kingOf2017Fun(oneUserData, beautiful_meta, 1)
oneUserData <- event_train[event_train$user_id %in% 47538, ]

verifyFun <- function(sampleUserIds, event_train, labels_train) {
  output <- data.frame()
  oneUserData <- event_train[event_train$user_id %in% sampleUserIds$user_id, ]
  i <- 1
  for(userId in sampleUserIds$user_id) {
    titleId <- kingOf2017Fun(oneUserData[oneUserData$user_id %in% userId , ], beautiful_meta, 1)
    output[i , 'user_id'] <- userId
    output[i , 'title_id'] <- titleId
    i <- i + 1
  }
  
  result <- labels_train[labels_train$user_id %in% sampleUserIds$user_id, ]
  colnames(result)[2] <- "result"
  mergeData <-merge(output, result, "user_id", all = TRUE)
  print(nrow(mergeData[mergeData$title_id == mergeData$result, ]) / nrow(mergeData))
  #return(nrow(mergeData[mergeData$title_id == mergeData$result, ]) / nrow(mergeData))
  return(mergeData)
}
  
kingOf2017Fun <- function(user_data, video_meta, n) {
  titleIds <- unique(user_data$title_id)

  #default
  titleId <- titleIds

  output <- data.frame()
  #Only one 
  if(length(titleIds) == 1) {
    return(titleIds)
  }

  i <- 1
  for(titleId in titleIds) {
    video <- video_meta[video_meta$title_id %in% titleId, ]
    
    total_watched_time <- sum( user_data[user_data$title_id == titleId, ]$watch_time)
    output[i, 'title_id'] <- titleId
    output[i, 'total_watched_time'] <- total_watched_time
    one_video_time <- n * 10 + 40
    total_time <- video$total_episode_counts * one_video_time * 60
    output[i, 'total_time'] <- total_time
    output[i, 'total_episode_counts'] <- video$total_episode_counts
    ratio <- total_watched_time / total_time
    
    if(ratio > 0.6 ) { #watched
      output[i , 'weight'] <- 0
      
    } else if ( ratio < 0.01) {
      output[i , 'weight'] <- 0
      
    } else {
      output[i , 'weight'] <- total_watched_time
      
    }
    
    i <- i + 1
  }

  
  output <- output[order(-output$weight), ]

  return(output$title_id[1])
  #return(output)
}


# Group data by table way
# as.data.frame(table(result$title_id))

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
write.csv(output, file = "~/Desktop/data_game/upload.csv", row.names=FALSE,  quote = FALSE)
