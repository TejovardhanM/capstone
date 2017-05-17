source("config.R")

rm(list =ls())
df.housing<-read.csv("train.csv")

#9572/20899 = 45%

str(df.housing)
col <- c("mosque_count_5000")
df.housing[,c(5:8,11)] <- data.frame(apply(df.housing[c(5:8,11)], 2, as.factor))


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
  xmean =  median(df.housing[,col], na.rm = TRUE)
  df.housing[is.na(df.housing[col]), col] = xmean
  }

boxplot(df.housing)

write.csv(df.housing,file = "zhousingdata.csv")
###########
new_DF <- df.housing[rowSums(is.na(df.housing)) > 0,]

a<-data.frame(sapply(lapply(new_DF, is.na), table))

nt <-sapply(new_DF, function(y) sum(length(which(is.na(y)))))

length(sort(nt[nt>0], decreasing = T))

a<-df.housing[!is.na(df.housing$build_count_brick),]


