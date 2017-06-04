train$state[train$state == 33] <- 3


# full_sq,life_sq
train %>% filter(full_sq>1000)%>% summarise(n=n())
ggplot(train,aes(x=full_sq ,y=log(price_doc)))+geom_point()

# replacing outliers with NA for median imputation

train$full_sq[which(train$full_sq >1000)] <- NA

train$life_sq[which(train$life_sq >1000)] <- NA

train$build_year[13118]<-1970

train$build_year[which(train$build_year >2020)] <- NA
train$build_year[which(train$build_year <1600)] <- NA


test$build_year[which(test$build_year >2020)] <- NA
test$build_year[which(test$build_year <1600)] <- NA


# Drawing a scatter plot for life_sq and full_sq for outliers, we can see outliers
ggplot(train,aes(x=full_sq,y=life_sq))+geom_point()
train%>%select(full_sq,life_sq)%>%mutate(Diff=full_sq-life_sq)%>%filter(Diff<=-10)

#Analysis on similar linies can be done 
train$life_sq[which(train$life_sq ==802)] <- 80
train$life_sq[which(train$life_sq ==281&train$full_sq==44)] <- 28
train$life_sq[which(train$life_sq ==426&train$full_sq==73)] <- 46

train$life_sq[which(train$life_sq ==178&train$full_sq==30)] <- 18
train$life_sq[which(train$life_sq ==191&train$full_sq==37)] <- 19
train$life_sq[which(train$life_sq ==195&train$full_sq==31)] <- 19
train$life_sq[which(train$life_sq ==301&train$full_sq==47)] <- 30
train$life_sq[which(train$life_sq ==458&train$full_sq==77)] <- 48
train$life_sq[which(train$life_sq ==259&train$full_sq==45)] <- 29
train$life_sq[which(train$life_sq ==163&train$full_sq==32)] <- 16
train$life_sq[which(train$life_sq ==349&train$full_sq==52)] <- 49
train$life_sq[which(train$life_sq ==62&train$full_sq==33)] <- 32
train$life_sq[which(train$life_sq ==435&train$full_sq==75)] <- 45

train$full_sq[which(train$life_sq ==44&train$full_sq==9)] <- 49
train$full_sq[which(train$life_sq ==38&train$full_sq==18)] <- 48
train$full_sq[which(train$life_sq ==40&train$full_sq==5)] <- 45
train$full_sq[which(train$life_sq ==40&train$full_sq==1)] <- 49
train$full_sq[which(train$life_sq ==59&train$full_sq==46)] <- 66
train$full_sq[which(train$life_sq ==47&train$full_sq==1)] <- 51
train$full_sq[which(train$life_sq ==77&train$full_sq==0)] <- 80
train$full_sq[which(train$life_sq ==60&train$full_sq==1)] <- 61
train$full_sq[which(train$life_sq ==64&train$full_sq==1)] <- 71

train$full_sq[which(train$life_sq ==17 &train$full_sq==412)] <- 41
train$full_sq[which(train$life_sq ==44&train$full_sq==729)] <- 72
train$full_sq[which(train$life_sq ==38&train$full_sq==634)] <- 63
train$full_sq[which(train$life_sq ==27&train$full_sq==461)] <- 41


#same on test


test$life_sq[which(test$life_sq ==742 & test$full_sq==74.2)] <- 74
test$life_sq[which(test$life_sq ==361 & test$full_sq==36.1)] <- 36
test$life_sq[which(test$life_sq == 93.20 & test$full_sq==51.9)] <- 44
test$life_sq[which(test$life_sq ==237 & test$full_sq==40.2)] <- 23
test$life_sq[which(test$life_sq ==869 & test$full_sq==86.9)] <- 86
test$life_sq[which(test$life_sq ==283 & test$full_sq==56.3)] <- 28

test$price_doc <- 1

#combine the data set
d1 <- rbind(train, test)


# removing Id variables
idX<-str_subset(names(d1),pattern=fixed("ID"))

d1<-d1%>%select(-one_of(idX))


# For base model, not icluding sub_area and ecology variable 
# with dummy variables for factors, models performed poorly
# converting other factor variables to numeric 


d1$product_type=ifelse(d1$product_type=="Investment",0,1)
d1$nuclear_reactor_raion=ifelse(d1$nuclear_reactor_raion=="no",0,1)
d1$detention_facility_raion=ifelse(d1$detention_facility_raion=="no",0,1)
d1$culture_objects_top_25=ifelse(d1$culture_objects_top_25=="no",0,1)
d1$water_1line=ifelse(d1$water_1line=="no",0,1)
d1$big_road1_1line=ifelse(d1$big_road1_1line=="no",0,1)
d1$railroad_1line=ifelse(d1$railroad_1line=="no",0,1)
d1$thermal_power_plant_raion=ifelse(d1$thermal_power_plant_raion=="no",0,1)
d1$incineration_raion=ifelse(d1$incineration_raion=="no",0,1)
d1$oil_chemistry_raion=ifelse(d1$oil_chemistry_raion=="no",0,1)
d1$radiation_raion=ifelse(d1$radiation_raion=="no",0,1)
d1$railroad_terminal_raion=ifelse(d1$railroad_terminal_raion=="no",0,1)
d1$big_market_raion=ifelse(d1$big_market_raion=="no",0,1)



# Date features
d1$timestamp = as.Date(d1$timestamp)
d1<- d1 %>%mutate(date_yday=yday(timestamp)
                  ,date_month=month(timestamp)
                  ,date_year=year(timestamp)
                  ,date_week=week(timestamp)
                  ,date_mday=mday(timestamp)
                  ,date_wday=wday(timestamp)
)

# new features, not used divison creating for new features, because of NA
d1<- d1 %>%mutate(
  diff_floor = max_floor-floor
  ,diff_life_sq = full_sq-life_sq
  ,building_age = date_year - build_year
)

d2<-d1 %>% select(-c(price_doc,id,timestamp,sub_area,ecology))
#nzv<-nearZeroVar(d2,freqCut = 2,uniqueCut = 20)
#d2<- d2[,-nzv]
# no improvement with removing near zero vars using caret

# random forest model  

# median imputation using caret

dProc<-preProcess(d2, method = c("medianImpute","center","scale"))

d3<-predict(dProc,d2)

#separating train and test
myTrain<-d3[1:nrow(train),]
myTest<-d3[-(1:nrow(train)),]

# target var
myTrain$price_doc<-train$price_doc

# simple cross validation
myControl<-trainControl(method = "cv",
                        number = 5)

modelRf<-train(price_doc~.,data=myTrain,
               method="ranger",trControl=myControl)
modelRf

# model rmse 0.26

sample <- read.csv("sample_submission.csv")
rf.prediction <- predict(modelRf, myTest)

final.rf <- data.frame(id = sample$id, price_doc = rf.prediction)
write.csv(final.rf,row.names = F)

# 0.3279 on leader board
# Rest of the code is for Glmnet and comparing models

#modelGlmnet<-train(price_doc~.,data=myTrain,method="glmnet",trControl=myControl)
#modelGlmnet
#rmse 0.31
#glmnet.prediction <- predict(modelGlmnet, myTest)

#final.glmnet <- data.frame(id = sample$id, price_doc = glmnet.prediction)
#write.csv(final.glmnet, "basicGlmnet.csv",row.names = F)

#0.38 on leader board

# comparing models
#model_list<-list(glmnet=modelGlmnet,rf=modelRf)

#collect resamples from the Cv folds
#resamps<-resamples(model_list)
#resamps

#summarizing the results
#summary(resamps)
#xyplot(resamps)

list.files("../input")

# Any results you write to the current directory are saved as output.