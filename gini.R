library(dplyr)

setwd("/Users/Avantika/Desktop/GiniIndexAssignment/data")

data <- read.csv("dataset.csv", header = T)cg
data

library(sqldf)

getGiniMin <- function(name,d)
{
  v <- vector(mode="logical",length=3)
  comb <- combn(d, 2, FUN = NULL, simplify = TRUE)
  comb
  min = 1000
  branch1 = ''
  branch2 = ''
  for(i in 1:ncol(comb)){
    print(comb[,i]) 
    first = comb[1,i]
    if(length(d)>2){
      second = comb[2,i]
    }
    else
    {
      second=''
      branch1 = comb[1,1]
      branch2 = comb[2,1]
    }
    str = paste("SELECT COUNT(*) FROM data WHERE ", name," = '",first,"' OR ",name," = '",second,"'",sep="")
    str
    ginilowmed <- sqldf(str)
    ginilowmed
    otherlevel <- d[d!=first & d!=second]
    otherlevel
    str = paste("SELECT COUNT(*) FROM data WHERE ",name," = '",otherlevel,"'",sep="")
    str
    giniother = sqldf(str)
    giniother
    str = paste("SELECT COUNT(*) FROM data WHERE buys = 'no' AND ", name," = '",first,"' OR buys='no' AND ",name," = '",second,"'",sep="")
    str
    calc1 <- sqldf(str)
    calc1
    str = paste("SELECT COUNT(*) FROM data WHERE buys = 'yes' AND ", name," = '",first,"' OR buys='yes' AND ", name," = '",second,"'",sep="")
    str
    calc2 <- sqldf(str)
    calc2
    str = paste("SELECT COUNT(*) FROM data WHERE buys = 'no' AND ", name," = '",otherlevel,"'",sep="")
    str
    calc3 <- sqldf(str)
    calc3
    str = paste("SELECT COUNT(*) FROM data WHERE buys = 'yes' AND ", name," = '",otherlevel,"'",sep="")
    str
    calc4 <- sqldf(str)
    calc4
    giniIncome = ginilowmed/totalclasses*(1-(calc1/ginilowmed)^2-(calc2/ginilowmed)^2)+giniother/totalclasses*(1-(calc3/giniother)^2-(calc4/giniother)^2)
    giniIncome
    print(giniIncome)
    if(giniIncome < min && length(d)>2)
    {
      min= giniIncome
      branch1 = first
      branch2 = second
    }
    if(length(d)==2)
    {
      min = giniIncome
    }
    # vector <- c(vector, giniIncome)
  }
  #print(giniIncome)
  datafr <- data.frame(cbind(Type=name, Min=min,branchA=branch1,branchB=branch2))
  return(datafr)
}
totalclassesyes <- sqldf("SELECT COUNT(*) FROM data WHERE buys = 'yes'")
totalclassesno <- sqldf("SELECT COUNT(*) FROM data WHERE buys = 'no'")
totalclasses <- sqldf("SELECT COUNT(*) FROM data")
gini <- 1-(totalclassesyes/totalclasses)^2-(totalclassesno/totalclasses)^2
gini
len <- length(data)-1
list <- vector("list", len)
for(j in 1:len) {
d<-unique(data[j])
t<- colnames(data[j])
for(i in d[t])
{
  v <- i
}
vec <- getGiniMin(t,v)
print(vec)
list[[j]]=vec
}
list
df <- do.call(rbind.data.frame, list)
df
h<-cbind(df,"Impurity Reduction"=gini-df$COUNT...) ## All values with gini indexes
h
h[which.min(h$COUNT...),] # Split at 




