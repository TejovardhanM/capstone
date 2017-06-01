source("config.R")
source("subarea.R")
df.housing<-read.csv("train.csv")
#9572/20899 = 45%

require(dplyr)
df.housing <-df.housing %>% select(-id)
df.housing$state[df.housing$state == 33] <- which.max(table(df.housing$state))
df.housing$build_year[df.housing$build_year == 20052009] <- 2007


df.housing=merge(df.housing, sub.districts, by.x = "sub_area", by.y = "sub_area")
df.missingpct = data.frame();
unique.districts <- unique(sub.districts$district)

for ( i in 1: length(unique.districts))
{
  
  miss_pct <- map_dbl( df.housing[df.housing$district == unique.districts[i],], function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
  
  miss_pct <- miss_pct[miss_pct > 0]
  
  df.missingpct = rbind(df.missingpct,data.frame(miss=miss_pct, var=names(miss_pct), district =unique.districts[i], row.names=NULL))
  
}

ggplot(df.missingpct, aes(x=reorder(var, -miss), y=miss))+
  geom_bar(stat= "identity", fill  = "orangered")+
  facet_grid(district~.)


ggplot(df.missingpct, aes(x=reorder(var, -miss), y=miss))+
  geom_boxplot()+
  facet_grid(district~.)+

    theme(axis.text.x=element_text(angle=90, hjust=1))


#df.housing[df.housing$district == unique.districts[1], 50:100] %>% correlate() %>% network_plot(min_cor=0.9)
chart.Correlation(df.housing[,50:100], histogram=TRUE, pch=19)



df.housing = df.housing[df.housing$build_year!=20052009, ]

# 1 rec --updated to 1961
df.housing[which(df.housing$build_year == 4965), "build_year"] = 1961

a = df.housing[df.housing$build_year %in% c(0,1), ]
a<-a[!is.na(a$price_doc), ]



ggplot(data = a, mapping = aes( sub_area))+geom_histogram(stat = "count")+coord_flip()

b<-df.housing[df.housing$sub_area =="Poselenie Vnukovskoe",]

ggplot(b, aes(timestamp,price_doc ))+
  geom_point(aes(color = factor(full_sq)))+
  facet_grid(.~build_year)

ggplot(b, aes(full_sq,price_doc))+geom_point(aes(color = factor(build_year)))+
facet_grid(material~max_floor)

scale_this <- function(x){
  if (is.numeric(x) || is.integer(x))
  {
    (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE) 
  }
  else {x}
}

df.housing <-data.frame(lapply(df.housing, scale_this))

## We need to select features to assign the assumpted mean value 
for ( col in names(df.housing)){
  xmean =  mean(df.housing[,col], na.rm = TRUE)
  df.housing[is.na(df.housing[col]), col] = xmean
}


housing.Matrix <- as.data.frame(model.matrix(~. + 0 ,data=df.housing[, -c(1,2)],
                                  contrasts.arg = lapply(df.housing[,c(12,13)], 
                                  contrasts, contrasts=FALSE)))
b<-cor(housing.Matrix)
if(!require(caret))install.packages("caret")

highlyCorrelated <- findCorrelation(b, cutoff=0.8)

test.df = df.housing[,-c(1,highlyCorrelated)]

model <-glm(price_doc~., data = test.df, family = "gaussian")

summary(model)
predict.glm(model, newdata = "test.csv", type = "response")

read.csv("test.csv")
####################

if(!require(ggplot2) )install.packages("ggplot2")

ggplot( df.housing, mapping = aes(price_doc, build_year))+geom_boxplot()

ggplot(data = df.housing,mapping = aes(sub_area, price_doc, fill = product_type))+
  geom_boxplot()+
  facet_grid(big_market_raion~.)

a<-matrix(lapply(df.housing, unique))



df.housing$price_doc<-scale_this(df.housing$price_doc)
str(df.housing)
col <- c("mosque_count_5000")
df.housing[,c(5:8,11)] <- data.frame(apply(df.housing[c(5:8,11)], 2, as.factor))

df.housing[,20]



boxplot(df.housing)

write.csv(df.housing,file = "zhousingdata.csv")
###########
new_DF <- df.housing[rowSums(is.na(df.housing)) > 0,]

a<-data.frame(sapply(lapply(new_DF, is.na), table))

nt <-sapply(new_DF, function(y) sum(length(which(is.na(y)))))

length(sort(nt[nt>0], decreasing = T))

a<-df.housing[!is.na(df.housing$build_count_brick),]


##########
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
if(!require(mlbench))install.packages("mlbench")
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(df.housing[,20:100])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
