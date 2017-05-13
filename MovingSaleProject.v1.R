setwd("C:/Users/Pi-Ching 2/Dropbox/Job/DataIncubatorChallenge")
library("Rfacebook")

token = "EAACEdEose0cBAB3FeMuZCDBHjTPkvaXEkNMLZC7iUcf3Mbe1Wldg4ZBdtkOVVLgo33WZC1r3Ir6mTGtS0uZCM35xpStWRcbnfLBmFMj9Dh0CoBDpy1hRmOqwZByMPjuMCSewTqNKbjTIUQhQnJwd0v90MAhZBf1pn7qCgznS10TrbBAOnQQyeSzTy4yw12FgBIZD"

me <- getUsers("me", token, private_info = TRUE)
summary(me)
me$id #851710138181514

x = getGroup("201945526573205",n=2000, token,since = '2016/01/01',feed = TRUE) #columbia group

write.csv(x,file = "FBdata.csv")

data = read.csv("FBdata.csv", header = T)
fix(data)
colnames(data)
postText = data$message
postText = levels(postText)[postText] #post text vector in chr type
postText = tolower(postText)

#start regular expression
postText = gsub("[\r\n]","",postText)
postText = gsub("[0-9]+","",postText)
postText = gsub("[[:punct:]]", " ", postText) #remove special characters
postText = gsub("\\s+"," ",postText) #remove extra white (trim)

stopList = read.csv("StopWordList.csv",header = F)
vstopList = levels(stopList$V1)[stopList$V1] #stop word List vector in chr type

textList = list()
for (i in 1:length(postText)){
  vtext = strsplit(postText[i]," ")[[1]]
  removeIndex = which(vtext %in% vstopList)
  if (length(removeIndex)!=0){
    vtext = vtext[-removeIndex]
  }
  textList[[i]] = vtext
}

#dress
clothList = c("dress","cloth","cloths","pant","pants","skirt","jacket","coat")
j=1
rowIndex = vector()
for (i in 1:length(textList)){
  exist = match(clothList,textList[[i]])
  
  if (sum(!is.na(exist))!=0){
    rowIndex[j] = i
    j = j+1
  }
}
dressMoney = data$money[rowIndex]
hist(dressMoney)
boxplot(dressMoney)
summary(dressMoney)
sd(dressMoney)

#AC
ACList = c("ac","a/c","air","conditioner")
j=1
rowIndex = vector()
for (i in 1:length(textList)){
  exist = match(ACList,textList[[i]])
  
  if (sum(!is.na(exist))!=0){
    rowIndex[j] = i
    j = j+1
  }
}
ACMoney = data$money[rowIndex]
hist(ACMoney)
boxplot(ACMoney)
summary(ACMoney)
sd(ACMoney)

#table
tableList = c("table","desk")
j=1
rowIndex = vector()
for (i in 1:length(textList)){
  exist = match(tableList,textList[[i]])
  
  if (sum(!is.na(exist))!=0){
    rowIndex[j] = i
    j = j+1
  }
}
tableMoney = data$money[rowIndex]
hist(tableMoney)
boxplot(tableMoney)
summary(tableMoney)
sd(tableMoney)
