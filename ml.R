getwd() #在哪里生成project就在哪儿生成目录

subject_name=c("john doe","jane doe","steve graves")
temperature=c(98.1,98.6,101.4)
flu=c(FALSE,FALSE,TRUE)

temperature[2]
temperature[2:3]
temperature[-3]
temperature[c(T,T,F)] #TRUE和FALSE可以简???

gender=factor(c("MALE","MALE","FEMALE"))
gender
blood=factor(c("o","a","ab"),levels = c("a","b","ab","o"))
blood

subject1=list(fullname=subject_name[1],
              temperature=temperature[1],
              flu=flu[1],
              gender=gender[1],
              blood=blood[1])
subject1
subject1[2]
subject1[5]
subject1$blood

pt=data.frame(subject_name,temperature,flu,gender,blood,stringsAsFactors = F)
pt
pt$subject_name
pt[c("subject_name","blood","gender")]
pt[2:4]
pt[c(1:3),c(1:2)]
pt[,c(1:3)]
pt[c(2:3),]
pt[-1,c(1:4)]

m1=matrix(c("a","b","c","d","e","f"),nrow=2)
m1
m2=matrix(c("a","b","c","d","e","f"),ncol=2)
m2

install.packages("RODBC")
library(RODBC)
db=odbcConnect("mydata",uid="root",pwd="liuyifei") #mydata在控制面板中设置,要run服务器才能连???
sqlTables(db)
query1="select * from sop.一手数???"
onehand=sqlQuery(channel = db,query = query1,stringsAsFactors=F)
onehand

usedcars=read.csv("usedcars.csv",stringsAsFactors = F)
usedcars
str(usedcars)
class(usedcars)
class(usedcars$year)
class(usedcars$model)
summary(usedcars$year)
summary(usedcars[c("price","mileage")])
summary(usedcars[3:4])
summary(usedcars[,3:4])

range(usedcars$price) #值域
diff(range(usedcars$price)) #极差
IQR(usedcars$price) #四分位???
min(usedcars$price)
max(usedcars$price)
mean(usedcars$price)
median(usedcars$price)
quantile(usedcars$price) #五数汇???
quantile(usedcars$price,probs = c(0.01,0.99))
quantile(usedcars$price,probs = c(0.25,0.75)) #指定分位???
quantile(usedcars$price,seq(0,1,0.25)) #指定区间

boxplot(usedcars$price,main="boxplot of used car prices",ylab="price($)")
hist(usedcars$price,main = "histogram of used car prices",xlab = "prices($)")

var(usedcars$price)
sd(usedcars$price)

table(usedcars$year)
tabulate(usedcars$year)
table(usedcars$model)
table(usedcars$color)
prop.table(table(usedcars$year))
prop.table(table(usedcars$color))*100
round(prop.table(table(usedcars$color))*100,digits = 2)
mode(usedcars)
class(usedcars)

plot(x=usedcars$mileage,y=usedcars$price,main = "scatterplot of price vs. mileage",
     xlab = "used car odometer",
     ylab = "used car price")

install.packages("gmodels")
library(gmodels)
usedcars$conservative=usedcars$color%in%c("Black","Gray","Silver","White")
table(usedcars$conservative)

CrossTable(x=usedcars$model,y=usedcars$conservative,chisq = T)


#神经网络
concrete=read.csv("concrete.csv")
concrete
head(concrete)
str(concrete)

normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concretenorm=as.data.frame(lapply(concrete,normalize))
head(concretenorm)
summary(concretenorm$cement)
concretetrain=concretenorm[1:773,]
concretetest=concretenorm[774:1030,]
concretetest

install.packages("neuralnet")
library(neuralnet)

concretemodel=neuralnet(strength~cement+slag+ash+water+superplastic
                        +coarseagg+fineagg+age,data = concretetrain)
plot(concretemodel)

modelresults=compute(concretemodel,concretetest[1:8]) #???8个位解释变量
modelresults
predictstrength=modelresults$net.result
cor(predictstrength,concretetest$strength)
cor.test(predictstrength,concretetest$strength)

concretemodel2=neuralnet(strength~cement+slag+ash+water+superplastic
                        +coarseagg+fineagg+age,data=concretetrain,hidden = 5)
plot(concretemodel2)
modelresults2=compute(concretemodel2,concretetest[1:8]) #???8个位解释变量
predictstrength2=modelresults2$net.result
cor(predictstrength2,concretetest$strength)

#支持向量???
letters=read.csv("letterdata.csv")
str(letters)
letterstrain=letters[1:16000,]
letterstest=letters[16001:20000,]

install.packages("kernlab")
library(kernlab)

letterclassifier=ksvm(letter~.,data=letterstrain,kernel="vanilladot")
letterclassifier
letterprediction=predict(letterclassifier,letterstest)
head(letterprediction)
table(letterprediction,letterstest$letter)
agreement=letterprediction==letterstest$letter
table(agreement)
prop.table(table(agreement))

letterclassifierrbf=ksvm(letter~.,data=letterstrain,kernel="rbfdot")
letterpredictionrbf=predict(letterclassifierrbf,letterstest)
agreement2=letterpredictionrbf==letterstest$letter
prop.table(table(agreement2))

#kNN
wbcd=read.csv("wisc_bc_data.csv",stringsAsFactors = F)
str(wbcd)
wbcd=wbcd[,-1]
str(wbcd)
wbcd$diagnosis=factor(wbcd$diagnosis,levels = c("B","M"),labels = c("Benign","Melignant")) #添加标签
class(wbcd$diagnosis)
table(wbcd$diagnosis)
round(prop.table(table(wbcd$diagnosis))*100,digits = 2)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}
wbcd_n=as.data.frame(lapply(wbcd[2:31],normalize)) #标准???
summary(wbcd_n[1:30])

wbcdtrain=wbcd_n[1:469,]
wbcdtest=wbcd_n[470:569,]

wbcdtrainlabel=wbcd[1:469,1]
wbcdtestlabel=wbcd[470:569,1]
wbcdtrainlabel

install.packages("class")
library(class)
wbcdtestpre=knn(train = wbcdtrain,test = wbcdtest,cl=wbcdtrainlabel,k=21)

library(gmodels)
CrossTable(wbcdtestlabel,wbcdtestpre,prop.chisq = F)

wbcd_z=as.data.frame(scale(wbcd[,-1]))
str(wbcd_z)
wbcdtrain2=wbcd_z[1:469,]
wbcdtest2=wbcd_z[470:569,]
wbcdtrain2label=wbcd[1:469,1]
wbcdtest2label=wbcd[470:569,1]
wbcdtest2pre=knn(train = wbcdtrain2,test = wbcdtest2,cl = wbcdtrain2label,k=21)
CrossTable(wbcdtest2label,wbcdtest2pre,prop.chisq = F)

#决策???
credit=read.csv("credit.csv")
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

order(c(1:10))
order(c(10,1,2,3,4,5,6,7,8,9))
set.seed(12345)
runif(1000)
order(runif(1000)) #记录每个数的排序
creditrand=credit[order(runif(1000)),]
creditrand[1:10,]

credittrain=creditrand[1:900,]
credittest=creditrand[901:1000,]
str(credittrain)
prop.table(table(credittrain$default))
prop.table(table(credittest$default))
class(credittrain$default)

credittrain$default=factor(credittrain$default,labels=c("no","yes"))
credittest$default=factor(credittest$default,labels=c("no","yes"))

credittrain$default
credittest$default

install.packages("C50")
library(C50)

creditmodel=C5.0(credittrain[,-21],credittrain$default)
creditmodel
summary(creditmodel)

creditpred=predict(creditmodel,credittest)
creditpred
library(gmodels)
CrossTable(credittest$default,creditpred,prop.chisq = F,prop.r = F,prop.c = F,dnn = c("actual","predict"))

creditmodelboost=C5.0(credittrain[,-21],credittrain$default,trials = 10) #boost算法
creditmodelboost
summary(creditmodelboost)
creditboostpre=predict(creditmodelboost,credittest)
creditboostpre
CrossTable(credittest$default,creditboostpre,prop.chisq = F,prop.r = F,prop.c = F,
           dnn = c("actual","predict"))

errorcost=matrix(c(0,1,4,0),nrow = 2)

creditmodelcost=C5.0(credittrain[,-21],credittrain$default,costs = errorcost)
creditcostpre=predict(creditmodelcost,credittest)
CrossTable(credittest$default,creditcostpre,prop.chisq = F,prop.r=F,prop.c=F,
           dnn = c("actual","predict"))

#文本分析
Sys.setlocale("LC_ALL", "English") #设置环境
smsraw=read.csv("sms_spam.csv",stringsAsFactors=F)
str(smsraw)

smsraw$type=factor(smsraw$type)
class(smsraw$type)
str(smsraw$type)
prop.table(table(smsraw$type))

library(tm)
smscorpus=Corpus(VectorSource(smsraw$text))
class(smscorpus)
print(smscorpus)
inspect(smscorpus)
smscorpus
corpusclean=tm_map(smscorpus,content_transformer(tolower)) #变小写，先运行有问题,但在删除空格后就没有问题
corpusclean=tm_map(corpusclean,content_transformer(removeNumbers)) #去数???
corpusclean=tm_map(corpusclean,content_transformer(removePunctuation)) #去标???
corpusclean=tm_map(corpusclean,content_transformer(stripWhitespace)) #去空???
corpusclean=tm_map(corpusclean,content_transformer(removeWords),stopwords()) #去停???

smsdtm=DocumentTermMatrix(corpusclean) #形成文档词汇矩阵（稀疏矩阵）
smsdtm=TermDocumentMatrix(corpusclean) #形成文档词汇矩阵（稀疏矩阵）和上面行列不???
smsdtm

smscorpustrain=corpusclean[1:4169]
smscorpustest=corpusclean[4170:5559]

library(wordcloud)
wordcloud(smscorpustrain,min.freq = 70,random.order = F) #生成词云，通用规则是开始时设置minfreq的值为语料库文档总数???10%，设置非随机则出现频率越高的单词越靠近中???

#关联规则
install.packages("arules") #通过菜单安装
library(arules)

groceries=read.transactions("groceries.csv",sep=",")  #生成稀疏矩???
groceries
summary(groceries)
inspect(groceries[1:10]) #查看项目内容
itemFrequency(groceries[,1:10]) #查看每个商品的支持度
itemFrequencyPlot(groceries,support=0.1) #控制支持???
itemFrequencyPlot(groceries,topN=20) #控制具体数量
image(groceries[1:10]) #可视化稀疏矩???

groceryrules=apriori(groceries,parameter = list(support=0.006,confidence=0.25,minlen=2))
groceryrules
summary(groceryrules)
inspect(groceryrules[1:5])

inspect(sort(groceryrules,by="lift") [1:10])#规则排序

berryrule=subset(groceryrules,items %in% "berries") #包含某个元素的规???
inspect(berryrule)
berryrule2=subset(groceryrules,items %in% c("berries","yogurt")) #???
inspect(berryrule2)
berryrule3=subset(groceryrules,items %ain% c("berries","yogurt")) #???
inspect(berryrule3)

write(groceryrules,file = "groceryrule.csv",sep=",",quote=T,row.names=F) #输出规则
groceryrules_df=as(groceryrules,"data.frame") #转换成数据框
str(groceryrules_df)

#聚类分析
library(stats)

teens=read.csv("snsdata.csv")
str(teens)
dim(teens)
nrow(teens)

table(teens$gender)
table(teens$gender,useNA = "ifany")
table(teens$gender,useNA = "no")
table(teens$gender,useNA = "always")
summary(teens$age)

teens$age=ifelse(teens$age>=13&teens$age<20,teens$age,NA)
summary(teens$age)
mean(teens$age,na.rm=T)
aggregate(data=teens,age~gradyear,mean,na.rm=T) #分组描述
aggregate(data=teens,age~gradyear,median,na.rm=T)

ave_age=ave(teens$age,teens$gradyear,FUN = function(x) mean(x,na.rm=T))
teens$age=ifelse(is.na(teens$age),ave_age,teens$age)
mean(teens$age,na.rm=T)
summary(teens$age)

interests=teens[,5:40]
str(interests)
interests_z=as.data.frame(lapply(interests,scale)) #同时处理多个数列

teenclusters=kmeans(interests_z,5)
str(teenclusters)
teenclusters$size
teenclusters$centers

teens$cluster=teenclusters$cluster
teens[1:5,c("cluster","gender","age","friends")]
aggregate(data=teens,age~cluster,mean,na.rm=T)
aggregate(data=teens,friends~cluster,mean,na.rm=T)

#回归???
library(rpart)
library(rpart.plot)
library(RWeka)
library(RWekajars)

wine<-read.csv("whitewines.csv")
summary(wine)
str(wine)

hist(wine$quality)
wine_train<-wine[1:3750,]
wine_test<-wine[3751:4898,]

m_rpart<-rpart(quality~.,wine_train)
m_rpart
summary(m_rpart)

rpart.plot(m_rpart,digits = 3,fallen.leaves = T,type = 3,extra = 101)

p.rpart<-predict(m_rpart,wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart,wine_test$quality)

mae<-function(actual,predicted) {
  mean(abs(actual-predicted))
} #平均绝对误差（Mean Absolute Error???

mae(p.rpart,wine_test$quality)
mae(wine_test$quality,p.rpart)

mean(wine_train$quality)
mean_abserror<-function(a,b){
  mean(abs(a-b))
}
mean_abserror(5.87,wine_test$quality)
mae(5.87,wine_test$quality)

m.m5p<-M5P(quality~.,wine_train)

#Naive Bayes
library(tm)
library(wordcloud)
library(e1071)
library(gmodels)

sms_raw<-read.csv("sms_spam.csv",stringsAsFactors = F,sep = ",")
str(sms_raw)
head(sms_raw)
sms_raw$type<-factor(sms_raw$type)
table(sms_raw$type)

sms_corpus<-Corpus(VectorSource(sms_raw$text))
sms_corpus
print(sms_corpus)
summary(sms_corpus)
inspect(sms_corpus)

corpus_clean<-tm_map(sms_corpus,tolower)
corpus_clean<-tm_map(corpus_clean,removeNumbers)
stopwords()
corpus_clean<-tm_map(corpus_clean,removeWords,stopwords())
corpus_clean<-tm_map(corpus_clean,stripWhitespace)

corpus_clean<-tm_map(corpus_clean, PlainTextDocument) #??Ҫ????????????ת????ϡ??????
sms_dtm<-DocumentTermMatrix(corpus_clean)
print(sms_dtm)

round(3/4*nrow(sms_raw))
sms_raw_train<-sms_raw[1:4180,]
sms_raw_test<-sms_raw[4180:5574,]
sms_dtm_train<-sms_dtm[1:4180,]
sms_dtm_test<-sms_dtm[4180:5574,]
sms_corpus_train<-corpus_clean[1:4180]
sms_corpus_test<-corpus_clean[4180:5574]

wordcloud(sms_corpus_train,min.freq = 40,random.order = F)
spam<-subset(sms_raw_train,type=="spam")
ham<-subset(sms_raw_train,type=="ham")
wordcloud(spam$text,max.words = 40,scale = c(3,0.5))
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))

findFreqTerms(sms_dtm_train,5)
sms_dict<-c(findFreqTerms(sms_dtm_train,5))
sms_train<-DocumentTermMatrix(sms_corpus_train,list(dictionary=sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test,list(dictionary=sms_dict))
str(sms_train)
str(sms_test) 

convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels = c("No","Yes"))
  return(x)
}

sms_train<-apply(sms_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_test,MARGIN = 2,convert_counts)

sms_classifier<-naiveBayes(sms_train,sms_raw_train$type)
sms_test_pred<-predict(sms_classifier,sms_test)

CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = F,prop.t = F,prop.r = F,dnn = c("predicted","actual"))
CrossTable(sms_raw_test$type,sms_test_pred,prop.chisq = F,prop.t = F,prop.r = F,dnn = c("actual","predicted"))

sms_classifier2<-naiveBayes(sms_train,sms_raw_train$type,laplace = 1)
sms_test_pred2<-predict(sms_classifier2,sms_test)
CrossTable(sms_raw_test$type,sms_test_pred2,prop.r = F,prop.t = F,prop.chisq = F,dnn = c("actual","predicted"))

#regression
library(psych) #pairs.panels

insurance<-read.csv("insurance.csv",stringsAsFactors = T)
str(insurance)
hist(insurance$charges)

cor(insurance[c("age","bmi","children","charges")])
pairs(insurance[c("age","bmi","children","charges")])
pairs.panels(insurance[c("age","bmi","children","charges")])

ins_model<-lm(charges~age+children+bmi+sex+smoker+region,data = insurance)
ins_model
summary(ins_model)

insurance$age2<-insurance$age^2
insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)

ins_model2<-lm(charges~age+age2+children+bmi+sex+bmi30*smoker+region,data = insurance) #??��????????Ե?????
summary(ins_model2)

ins_model3<-lm(charges~age+age2+children+bmi+sex+bmi30:smoker+region,data = insurance) #????��?????????Ե?????
summary(ins_model3)

#随机森林
library(randomForest)
library(caret)

str(credit)
set.seed(300)
credit$default<-factor(credit$default)
rf<-randomForest(default~.,data = credit)
rf

ctrl<-trainControl(method = "repeatedcv",number = 10,repeats = 10)
grid_rf<-expand.grid(.mtry=c(2,4,8,16))
ctrl
grid_rf

set.seed(300)
m_rf<-train(default~.,data = credit,method="rf",
            metric="Kappa",trControl=ctrl,tuneGrid=grid_rf)

grid_50<-expand.grid(.model="tree",
                     .trials=c(10,20,30,40),
                     .winnow="FALSE")
set.seed(300)
m_c50<-train(default~.,data = credit,method="C5.0",
             metric="Kappa",trControl=ctrl,
             tuneGrid=grid_50)

m_rf
m_c50

m_c50b<-train(default~.,data = credit,method="C5.0",
             trControl=ctrl,
             tuneGrid=grid_50)
m_c50b

#bagging
library(caret)
library(ipred)

set.seed(300)
mybag<-bagging(default~.,data = credit,nbagg=25)
mybag$y
mybag$X
mybag$mtrees[1]
mybag$OOB
mybag$comb
mybag$call

credict_pred<-predict(mybag,credit) #重代入预测
table(credict_pred,credit$default)

ctrl<-trainControl(method = "cv",number = 10)
train(default~.,data=credit,method="treebag",trControl=ctrl)

#神经网络
library(neuralnet)

concrete<-read.csv("concrete.csv")
str(concrete)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,normalize))
summary(concrete_norm)

concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

concrete_model<-neuralnet(strength~cement+slag+ash+water+superplastic+
                            coarseagg+fineagg+age,concrete_train)
plot(concrete_model)
model_results<-compute(concrete_model,concrete_test[1:8])
model_results
predicted_strength<-model_results$net.result
cor(predicted_strength,concrete_test$strength)

concrete_model2<-neuralnet(strength~cement+slag+ash+water+superplastic+
                            coarseagg+fineagg+age,concrete_train,hidden = 5)
plot(concrete_model2)
model_results2<-compute(concrete_model2,concrete_test[1:8])
predicted_strength2<-model_results2$net.result
cor(predicted_strength2,concrete_test$strength)
