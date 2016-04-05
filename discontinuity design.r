#Since a randoom experiment is impossible, use regression discontinuity design to mimic random assignment
#of first ranking and get unbiased effect estimation of ranking first on subsequent movie earnings.

library(plyr)
DB=read.csv("3DB.csv")
DB$releaseDate=as.Date(DB$releaseDate,"%m/%d/%Y")#convert date
DB$releaseDate=strftime(DB$releaseDate,format = "%d/%m/%Y-%V")#convert date to year+month+week number


#----regression----------
regressionDB=subset(DB,subsequentEarnings!=0) #drop the zero data
regressionDB$subsequentEarnings=log(regressionDB$subsequentEarnings)
reg=lm(subsequentEarnings~releaseRank,regressionDB)
summary(reg)

#------correlation------------
cor(DB[,c(3,4,6)],use = "complete.obs")#releaseWeekEarnings subsequentEarnings releaseRank

##------data reshape-----------------
firstWeekBoxOfficeDiff=c()
laterLogBoxOfficeDiff=c()
top1movie=c()
top2movie=c()
date=unique(DB$releaseDate)
length(date)# 1351 weeks
for (i in 1:length(date)) {
  temp=subset(DB,releaseDate==date[i])#choose all the movies released on week i
  if (nrow(temp>1)) {# have at least two movies in one week
    temp=temp[order(temp$releaseRank),]#ascending order on release rank
    temp=temp[1:2,]#top 2 leading movies
    firstWeekBoxOfficeDiff[i]=log(temp[1,3])-log(temp[2,3])
    laterLogBoxOfficeDiff[i]=log(temp[1,4])-log(temp[2,4])
    top1movie[i]=temp[1,1]# record the leading movie ID for later use in part3
    top2movie[i]=temp[2,1]
  }
  else{
    firstWeekBoxOfficeDiff[i]=NA
    laterLogBoxOfficeDiff[i]=NA
  }
}
DB2=data.frame(date,firstWeekBoxOfficeDiff,laterLogBoxOfficeDiff,top1movie,top2movie)
DB2=na.omit(DB2)#drop off NA
DB2=subset(DB2,laterLogBoxOfficeDiff!="-Inf"&laterLogBoxOfficeDiff!="Inf")#drop off infinite
save.image(file="Shixiao.cui.hw3.RData")

#---find a subset where first and second place are close------
summary(DB2)
DB3=subset(DB2,firstWeekBoxOfficeDiff<0.1)# find the movies that are close to each others.

#---find treatment and control group-----
strftime("2008-4-4",format = "%Y%m%V")# "20080414" find the week
DB[DB$releaseDate=="20080414",]
#---t.test----------
TestDB=subset(DB,X %in% DB3$top1movie|X %in% DB3$top2movie) #find the movies 
#in treatment group and control group
TestDB$logsubsequentEarnings=log(TestDB$subsequentEarnings)
t.test(data=TestDB,logsubsequentEarnings~releaseRank,paired = FALSE)
