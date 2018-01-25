library(leaflet)
library(dplyr)
library(ggplot2)
library(caret)
library(stats)
library(lubridate)
library(plotly)
library(geosphere)
library(pracma)
library(MASS)
library(Metrics)
library(VGAM)
library(rpart)
library(xgboost)
library(Matrix)
library(R.cache)
library(FNN)


load_data <- function()
{
    key <- list(c("allDS"))
    
    allDS<-readRDS("train_processed.bin")
    allDS <- allDS[1:10000,]
    
}
allDS <- load_data()

make_kmeans_cluster <- function(allDS)
{
    ret <- as.data.frame(
        as.matrix(cbind(kmeans(cbind(allDS$pickup_longitude, allDS$pickup_latitude), centers=10), 
                        kmeans(cbind(allDS$dropoff_longitude, allDS$dropoff_latitude), centers=10)), ncol=2)
    )
    names(ret) <- c("pickupKMS", "dropKMS")
    ret
}

predict_knn <- function(kms, testds)
{
    
    testds$trip_pickup_cluster <-
                get.knnx(kms$pickupKMS$centers, 
                        as.matrix(
                            cbind(testds$pickup_longitude, 
                                  testds$pickup_latitude), 
                                  ncol=2), k=10)$nn.index[1,1]
    testds$trip_dropoff_cluster <-
                get.knnx(kms$dropKMS$centers, 
                         as.matrix(
                             cbind(testds$dropoff_longitude, 
                                   testds$dropoff_latitude), 
                             ncol=2), k=10)$nn.index[1,1]
    testds
}

add_rushhour_ind <- function(tripTrain) 
{
    
    tripTrain <- tripTrain %>%
        mutate(trip_rush_hour = factor(ifelse(trip_hour %in% c(8:18), "R", "N"))) %>%
        mutate(trip_distance_cat = 
                   factor(case_when(
                       trip_distance < 1707.387 ~ 'DIST1',
                       trip_distance >=1707.387 & trip_distance < 3176.34  ~ 'DIST2',
                       trip_distance >= 3176.34 ~ 'DIST3'))) %>%
        mutate(trip_cluster_cat = 
                   paste(as.character(trip_pickup_cluster), 
                         "_",
                         as.character(trip_dropoff_cluster), sep=""))
    
    
    tripTrain
}

mutate_data <- function(all_DS)
{
    pickups  <- matrix(c(allDS[,6], allDS[,7]), ncol=2)
    dropoffs <- matrix(c(allDS[,8], allDS[,9]), ncol=2)
    allDS$trip_distance <- distGeo(pickups, dropoffs)
    
    allDS <- allDS %>% 
        filter(!is.na(pickup_datetime)) %>%
        filter(trip_duration < 1 *60*60) %>%
        mutate(pickup_datetime = as.POSIXct(strptime(pickup_datetime, format="%Y-%m-%d %H:%M:%S"))) %>%
        mutate(dropoff_datetime = as.POSIXct(strptime(dropoff_datetime, format="%Y-%m-%d %H:%M:%S"))) %>%
        filter(is.na(trip_duration)==FALSE)  %>%
        mutate(trip_hour = hour(pickup_datetime)) %>%
        mutate(trip_wkday_ind = 
                   ifelse(weekdays(pickup_datetime) %in% c("Saturday", "Sunday"), 
                          "F", "T")) %>% 
        mutate(trip_date = date(pickup_datetime)) %>%
        mutate(trip_duration_computed = as.numeric(difftime(dropoff_datetime, pickup_datetime, units="secs"))) %>%
        mutate(trip_month=month(trip_date)) %>%
        mutate(trip_wkday=weekdays(trip_date, abbreviate=TRUE))
    trainDS <- 
        allDS %>% 
        filter(!is.na(trip_duration)) %>%
        filter(!is.na(trip_duration_computed))
    trainDS
}
trainDS <- mutate_data(allDS)
kms <- make_kmeans_cluster(trainDS)
add_cluster_id <- function(ds)
{
    ds$trip_pickup_cluster <-kms$pickupKMS$cluster
    ds$trip_dropoff_cluster <- kms$dropKMS$cluster    
    ds
}

make_xgb_matrix<- function(d)
{
    print("Making matrix")
    print(d)
    sparse_matrix <- sparse.model.matrix(trip_duration~
                                             trip_distance+
                                             trip_hour+
                                             trip_pickup_cluster+
                                             trip_month,
                                             data = d)
    output_vector <- d$trip_duration
    ret <- xgb.DMatrix(data=sparse_matrix, label=output_vector)
    ret
}
make_xgb_model_prediction <- function(s, v)
{
    print("Boosting..")
    sparse_matrix_s <- make_xgb_matrix(s)
    output_vector <- s$trip_duration
    bst <- xgboost(data = sparse_matrix_s, label = output_vector, max_depth = 4,
                   eta = 1, nthread = 2, nrounds = 10,
                   eval.metric = "rmse", objective = "reg:linear")
    output_vector <- s$trip_duration
    foldsCV <- createFolds(output_vector, k=7, list=TRUE, returnTrain=FALSE)
    
    param <- list(colsample_bytree = 0.7
                  , booster = "gbtree"
                  , objective = "reg:linear"
                  , subsample = 0.7
                  , max_depth = 5
                  , eta = 0.037
                  , eval_metric = 'rmse'
                  , base_score = 0.012 #average
                  , seed = 4321)
    
    bst <- xgb.cv(data=sparse_matrix_s,
                  params=param, 
                  nrounds = 30,
                  folds=foldsCV,label=output_vector,
                  prediction=TRUE, nthread = 2,
                  early_stopping_rounds = 15,print_every_n = 5)
    
    nrounds <- bst$best_iteration
    
    
    print("training the xgb...")
    xgb <- xgb.train(params = param
                     , data = sparse_matrix_s
                     , nrounds = nrounds
                     , verbose = 1
                     , print_every_n = 5
                     #, feval = amm_mae
    )
    
    sparse_matrix_v <- make_xgb_matrix(v)
    
    print("Predict using the xgb boosting model...")
    predictedxgb <- predict(xgb, sparse_matrix_v)
    predictedxgb
}
make_linear_model <- function(s, v)
{
    print("Linear model ...")
    split_model <- lm(data=s,
                      trip_duration ~ 
                          trip_distance +
                          trip_rush_hour +
                          factor(trip_pickup_cluster) + 
                          1)
    print("Prediction using the linear model ...")
    predictv <- predict(split_model, newdata=v)
    predictv
}

final_model <- function(testDS)
{
    testList <- list()
    trainList <- list()
    predictionList <- list()
    final_prediction <- data.frame()
    
    trainDS <- add_cluster_id(trainDS)
    #print("trainDS names")
    #print(names(trainDS))
    trainDS <-add_rushhour_ind(trainDS)
    testDS <- add_rushhour_ind(testDS)
    #testDS <- bind_rows(testDS, trainDS[1,])
    #print("test DS in formal_model")
    #print(testDS)
    for (i in 1:3) 
    {
        #print("A New iteration ")
        trainList[[i]] <- subset(trainDS, 
                                 trip_distance_cat == paste('DIST', as.character(i), sep=""))
        testList[[i]] <- subset(testDS, 
                                trip_distance_cat == paste('DIST', as.character(i), sep=""))
        ## reset to 0 to avoid error
        if (dim(testList[[i]])[1] > 0)
            testList[[i]]$trip_duration <- 0
        else {
            # no need to continue if there is no subset
            next 
        }
            
        if (i == 1)
        {
            predictionList[[i]] <- 
                make_xgb_model_prediction(trainList[[i]], testList[[i]])
        }
        else
        {
            predictionList[[i]]  <-
                make_linear_model(trainList[[i]],testList[[i]])
        }
        # Construct a id <- prediction data frame
        currResult <- as.data.frame(
            as.matrix(cbind(as.character(testList[[i]]$id), predictionList[[i]]), ncol=2))
        if (nrow(currResult)>0 & nrow(final_prediction) > 0)
        {
           final_prediction <- as.data.frame(
                rbind(final_prediction, currResult))
        }
        else if (nrow(currResult)>0 && nrow(final_prediction) ==0)
            final_prediction <- currResult
    }
    
    names(final_prediction) <- c("id", "trip_duration")
    final_prediction$trip_duration <- as.numeric(as.character(final_prediction$trip_duration))
    print("final dimension")
    print(dim(final_prediction))
    final_prediction
}

modify_test_ds <- function(testds)
{
    pickups  <- matrix(c(testds$pickup_longitude, testds$pickup_latitude), ncol=2)
    dropoffs <- matrix(c(testds$dropoff_longitude, testds$dropoff_latitude), ncol=2)
    testds$trip_distance <- distGeo(pickups, dropoffs)

    testds <- testds %>% 
    filter(!is.na(pickup_datetime)) %>%
    mutate(pickup_datetime = as.POSIXct(strptime(pickup_datetime, format="%Y-%m-%d %H:%M:%S"))) %>%
    mutate(dropoff_datetime = as.POSIXct(strptime(dropoff_datetime, format="%Y-%m-%d %H:%M:%S"))) %>%
    mutate(trip_hour = hour(pickup_datetime)) %>%
    mutate(trip_wkday_ind = 
               ifelse(weekdays(pickup_datetime) %in% c("Saturday", "Sunday"), 
                      "F", "T")) %>% 
    mutate(trip_date = date(pickup_datetime)) %>%
    mutate(trip_month=month(trip_date)) %>%
    mutate(trip_wkday=weekdays(trip_date, abbreviate=TRUE))

    testds <- predict_knn(kms, testds)
    testds <- add_rushhour_ind(testds)
    testds
}
predictTripTime<-function(ds)
{
    predictTime <- final_model(ds)
}
getMinMaxLatLng <- function()
{
    minMaxDS <- as.data.frame(as.matrix(cbind(min(allDS$pickup_longitude),
                  max(allDS$pickup_longitude),
                  min(allDS$pickup_latitude),
                  max(allDS$pickup_latitude)), nrow=1,ncol=4))
    minMaxDS
}
