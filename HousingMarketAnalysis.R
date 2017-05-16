getwd()
rm(list =ls())
df.housing<-read.csv("train.csv")
str(df.housing)

table(is.na(df.housing$max_floor))

#9572/20899 = 45%

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

boxplot(df.housing)

write.csv(df.housing,file = "zhousingdata.csv")

