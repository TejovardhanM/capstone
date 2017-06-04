source("config.R")
source("subarea.R")
df.housing<-read.csv("train.csv")
df.housing %>% filter(full_sq>1000)%>% summarise(n=n(), max = max(full_sq))
as.numeric(rownames(df.housing[df.housing$full_sq == 5326,]))
df.housing[3528, "full_sq"] = 54
ggplot(df.housing,aes(x=full_sq ,y=log(price_doc)))+geom_point()


df.housing %>% filter(life_sq>1000)%>% summarise(n=n(), max = max(life_sq))
summary(df.housing$life_sq)
df.housing[which(df.housing$life_sq == 7478),"life_sq"] = NA
#df.housing[3528, "life_sq"] = mean(df.housing$life_sq, na.rm = T)
ggplot(df.housing,aes(x=life_sq ,y=log(price_doc)))+geom_point()

df.housing$build_year[13118]<-1970

df.missingvalues<- map_dbl(df.housing, function(x){sum(is.na(x))})
df.missingvalues<- df.missingvalues[df.missingvalues>0]
df.missingvalues = as.data.frame(df.missingvalues)


#Analysis on similar linies can be done 
df.housing$life_sq[which(df.housing$life_sq ==802)] <- 80
df.housing$life_sq[which(df.housing$life_sq ==281&df.housing$full_sq==44)] <- 28
df.housing$life_sq[which(df.housing$life_sq ==426&df.housing$full_sq==73)] <- 46

df.housing$life_sq[which(df.housing$life_sq ==178&df.housing$full_sq==30)] <- 18
df.housing$life_sq[which(df.housing$life_sq ==191&df.housing$full_sq==37)] <- 19
df.housing$life_sq[which(df.housing$life_sq ==195&df.housing$full_sq==31)] <- 19
df.housing$life_sq[which(df.housing$life_sq ==301&df.housing$full_sq==47)] <- 30
df.housing$life_sq[which(df.housing$life_sq ==458&df.housing$full_sq==77)] <- 48
df.housing$life_sq[which(df.housing$life_sq ==259&df.housing$full_sq==45)] <- 29
df.housing$life_sq[which(df.housing$life_sq ==163&df.housing$full_sq==32)] <- 16
df.housing$life_sq[which(df.housing$life_sq ==349&df.housing$full_sq==52)] <- 49
df.housing$life_sq[which(df.housing$life_sq ==62&df.housing$full_sq==33)] <- 32
df.housing$life_sq[which(df.housing$life_sq ==435&df.housing$full_sq==75)] <- 45

df.housing$full_sq[which(df.housing$life_sq ==44&df.housing$full_sq==9)] <- 49
df.housing$full_sq[which(df.housing$life_sq ==38&df.housing$full_sq==18)] <- 48
df.housing$full_sq[which(df.housing$life_sq ==40&df.housing$full_sq==5)] <- 45
df.housing$full_sq[which(df.housing$life_sq ==40&df.housing$full_sq==1)] <- 49
df.housing$full_sq[which(df.housing$life_sq ==59&df.housing$full_sq==46)] <- 66
df.housing$full_sq[which(df.housing$life_sq ==47&df.housing$full_sq==1)] <- 51
df.housing$full_sq[which(df.housing$life_sq ==77&df.housing$full_sq==0)] <- 80
df.housing$full_sq[which(df.housing$life_sq ==60&df.housing$full_sq==1)] <- 61
df.housing$full_sq[which(df.housing$life_sq ==64&df.housing$full_sq==1)] <- 71

df.housing$full_sq[which(df.housing$life_sq ==17 &df.housing$full_sq==412)] <- 41
df.housing$full_sq[which(df.housing$life_sq ==44&df.housing$full_sq==729)] <- 72
df.housing$full_sq[which(df.housing$life_sq ==38&df.housing$full_sq==634)] <- 63
df.housing$full_sq[which(df.housing$life_sq ==27&df.housing$full_sq==461)] <- 41



fact = as.data.frame(sapply(df.housing, is.factor))


df.housing$product_type=as.numeric(ifelse(df.housing$product_type=="Investment",0,1))
df.housing$nuclear_reactor_raion=as.numeric(ifelse(df.housing$nuclear_reactor_raion=="no",0,1))
df.housing$detention_facility_raion=as.numeric(ifelse(df.housing$detention_facility_raion=="no",0,1))
df.housing$culture_objects_top_25=as.numeric(ifelse(df.housing$culture_objects_top_25=="no",0,1))
df.housing$water_1line=as.numeric(ifelse(df.housing$water_1line=="no",0,1))
df.housing$big_road1_1line=as.numeric(ifelse(df.housing$big_road1_1line=="no",0,1))
df.housing$railroad_1line=as.numeric(ifelse(df.housing$railroad_1line=="no",0,1))
df.housing$thermal_power_plant_raion=as.numeric(ifelse(df.housing$thermal_power_plant_raion=="no",0,1))
df.housing$incineration_raion=as.numeric(ifelse(df.housing$incineration_raion=="no",0,1))
df.housing$oil_chemistry_raion=as.numeric(ifelse(df.housing$oil_chemistry_raion=="no",0,1))
df.housing$radiation_raion=as.numeric(ifelse(df.housing$radiation_raion=="no",0,1))
df.housing$railroad_terminal_raion=as.numeric(ifelse(df.housing$railroad_terminal_raion=="no",0,1))
df.housing$big_market_raion=as.numeric(ifelse(df.housing$big_market_raion=="no",0,1))


df.housing <- within(df.housing, {
  ecology <- as.numeric(as.character(ecology))
})

unique(as.character(df.housing$ecology))

unfactor(df.housing$ecology)

for (i in 1:nrow(df.housing)){
  
  
  if( !is.na(df.housing[i, "ecology"] == "excellent") )df.housing[i, "ecology"] = as.numeric(1)
  if(!is.na(df.housing[i, "ecology"] == "good") )df.housing[i, "ecology"] = as.numeric(2)
  if(!is.na(df.housing[i, "ecology"] == "satisfactory") )df.housing[i, "ecology"] = as.numeric(3)
  if(!is.na(df.housing[i, "ecology"] == "poor") )df.housing[i, "ecology"] = as.numeric(4)
  if(!is.na(df.housing[i, "ecology"] == "no data")) df.housing[i, "ecology"] = as.numeric(5)
  }

ifelse(df.housing[i, "ecology"] == "excellent", df.housing[i, "ecology"] =1,
       ifelse(df.housing[i, "ecology"] == "good",df.housing[i, "ecology"] =2,
              ifelse(df.housing[i, "ecology"] == "poor",df.housing[i, "ecology"] =3,
                     ifelse(df.housing[i, "ecology"] == "satisfactory", df.housing[i, "ecology"] =4,df.housing[i, "ecology"] =5
                     ))))

#######################################################

train$build_year[which(train$build_year >2020)] <- NA
train$build_year[which(train$build_year <1600)] <- NA


test$build_year[which(test$build_year >2020)] <- NA
test$build_year[which(test$build_year <1600)] <- NA


# Drawing a scatter plot for life_sq and full_sq for outliers, we can see outliers
ggplot(train,aes(x=full_sq,y=life_sq))+geom_point()
train%>%select(full_sq,life_sq)%>%mutate(Diff=full_sq-life_sq)%>%filter(Diff<=-10)



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
#######################################################copied from Kaggle.



############
str(df.housing)
df.housing <- df.housing %>% 
  mutate(max_floor = as.numeric(max_floor), kitch_sq=as.numeric(kitch_sq), 
         num_room=as.numeric(num_room), build_year=as.numeric(build_year), 
         sub_area=as.factor(sub_area)) 

df.housing$timestamp = as.POSIXct(df.housing$timestamp, "GMT")

df.housing <- df.housing %>% 
  filter(build_year < 2020 | is.na(build_year))

df.housing <- df.housing %>% mutate(strange_full_sq = ifelse(full_sq <= 1, full_sq+1,0), full_sq = ifelse(full_sq > 800 | full_sq <= 1, NA, full_sq))

df.housing <- df.housing %>% mutate(strange_life_sq = ifelse(life_sq <= 1, life_sq+1,0), strange_life_sq= ifelse(is.na(strange_life_sq),0,strange_life_sq), life_sq = ifelse(life_sq > 400 | life_sq <= 1, NA, life_sq))

df.housing <- df.housing %>% mutate(kitch_sq = as.numeric(kitch_sq),strange_kitch_sq = ifelse(kitch_sq <= 1, kitch_sq+1,0),kitch_sq = ifelse(kitch_sq > 200 | kitch_sq <= 1, NA, kitch_sq))


df.housing <- df.housing %>% mutate(num_room = as.numeric(num_room))

df.housing <- df.housing %>% mutate(build_year = as.numeric(build_year), strange_build_year = ifelse(build_year <= 1, build_year+1,0), build_year = ifelse(build_year > 2018 | build_year < 1860, NA, build_year))

df.housing <- df.housing %>% mutate(floor = ifelse(floor > 45, NA, floor))

df.housing <- df.housing %>% mutate(max_floor = as.numeric(max_floor), strange_max_floor = ifelse(max_floor <= 1, max_floor+1,0), max_floor = ifelse(max_floor > 60 | max_floor <=1, NA, max_floor))

df.housing <- df.housing %>% mutate(state = as.numeric(state), state = ifelse(state > 4, NA, state))

df.housing <- df.housing %>% mutate(material = as.factor(material), material = ifelse(material == 3, NA, material))

df.housing <- df.housing %>% mutate(product_type = factor(product_type))

df.housing <- df.housing %>% mutate(sub_area = factor(sub_area))

# more cleaning
df.housing <- df.housing %>% filter(kitch_sq < full_sq | is.na(kitch_sq))
df.housing <- df.housing %>% filter(kitch_sq < life_sq | is.na(kitch_sq))

df.housing <- df.housing %>% mutate(num_room = ifelse(num_room==0,NA,num_room))

