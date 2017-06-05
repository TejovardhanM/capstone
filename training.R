source("config.R")
source("subarea.R")
df.housing<-read.csv("train.csv",stringsAsFactors = FALSE)
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


for (i in 1:nrow(df.housing)){
  
ecology =  ifelse(!is.na(df.housing[i, "ecology"]), df.housing[i, "ecology"], 0)
                   iassign = 0
                   
                   if (ecology == "excellent"){
                     iassign = as.numeric(1)
                   }
                   else if (ecology == "good"){
                     iassign = as.numeric(2)
                   }
                   else if (ecology == "satisfactory"){
                     iassign = as.numeric(3)
                   }
                   else if (ecology == "poor"){
                     iassign = as.numeric(4)
                   }
                   else if (ecology == "no data"){
                     iassign = as.numeric(5)
                   }
          df.housing[i, "ecology"] = iassign

}

df.housing$ecology = as.numeric(df.housing$ecology)

df.housing$build_year[which(df.housing$build_year >2020)] <- NA
df.housing$build_year[which(df.housing$build_year <1600)] <- NA



# Drawing a scatter plot for life_sq and full_sq for outliers, we can see outliers
ggplot(df.housing,aes(x=full_sq,y=life_sq))+geom_point()

df.housing =df.housing%>%
  mutate(Diff=full_sq-life_sq)


ggplot(df.housing,aes(x=Diff,y=log(price_doc)))+geom_point()



# Date features
df.housing$timestamp = as.Date(df.housing$timestamp)
df.housing<- df.housing %>%mutate(
                  #date_yday=yday(timestamp),
                  date_month=month(timestamp)
                  ,date_year=year(timestamp)
                  #,date_week=week(timestamp)
                  #,date_mday=mday(timestamp)
                  #,date_wday=wday(timestamp)
)


funQuant <- function(x){
quant = quantile(x, na.rm =T)
Q1 = quant[2]
Q3 = quant[4]
mean = quant[3]

IQR = (Q3 - Q1)

lboundary <-1.5 * (IQR) - Q1
rboundary <-1.5 * (IQR) + Q3
return(data.frame(lboundary, rboundary, mean, row.names = NULL))

}

df.quantile =data.frame()
for (i in colnames(df.housing[, -c(1:2)])){
  print(i)
  if(!class(train[, i]) =="character")
    df.quantile = rbind(df.quantile,cbind(columnname = i,funQuant(train[, i])))
}

rowindex  = 4
str(colindex)
myCols <- c("mpg","disp")
colNums <- match(myCols,names(mtcars))
mtcars %>% select(colNums)
rowindex =3
udpateOutlierswithNA <- function(df.housing, df.quantile){
  for( rowindex in 1: NROW(df.quantile)){
    quantilerecord = df.quantile[rowindex,]
    colname = as.character(quantilerecord$columnname);
   #colindex = match(colname, names(df.housing))
    upperbound = as.numeric(quantilerecord$rboundary);
   # qcol = paste("df.housing$", colname,">=", upperbound)
  #  qcol =gsub(" ", "", qcol, fixed = TRUE)
    df.housing[df.housing[colname] > upperbound, colname] =NA
  }
  
}
