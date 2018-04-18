rm(list=ls())  #清除所有暫存
graphics.off() #清除圖片的顯示
getwd()        #顯示這個r所在的路徑

#Package
{
  library(stats)
  library(plyr)
  library(dplyr)
  library(data.table)
  library(rpart)
  library(rpart.plot)
  require(arfima)
  require(TSA)
  require(astsa)
  library(MASS)
  library(base)
  library(robustbase)
  require(rugarch)
}

Power=read.csv("power.csv",header=F) #載入資料

#Calendar:日曆
{
  Calendar=as.data.frame(t(matrix(c(rep(NA,2),c(1:365),rep(NA,4)),7,53)))
  colnames(Calendar)=c("一","二","三","四","五","六","日")
}


#Calendar_Month:月曆
{
  Month=NA; M=c(1:12); D=c(31,28,31,30,31,30,31,31,30,31,30,31)
  for(i in 1:12){Month=c(Month,rep(M[i],D[i]))}
  Calendar_Month=as.data.frame(t(matrix(c(rep(NA,2),Month[-1],rep(NA,4)),7,53)))
  colnames(Calendar_Month)=c("一","二","三","四","五","六","日")
  rm(D,i,M)
}

#Power_Day_Split:將Power以天區分，共365天，一天有96筆資料
Power_Day_Split=matrix(as.matrix(Power),96,365)

#階層式分群
hc=hclust(dist(t(Power_Day_Split)), "complete")
hc_cluster=cutree(hc, k=5) #依照所要的群數k進行分類

{
  #階層式分群分五群，套入日曆格式
  #t(matrix(c(rep(NA,2),hc_cluster,rep(NA,4)),7,53))%>%View()
  
  #kmeans分群
  #KM=kmeans(t(Power_Day_Split),5)
  #table(KM$cluster)
  
  #kmean分2群，套入日曆格式(決定用來當作分類假日的依據)
  #t(matrix(c(rep(NA,2),kmeans(t(Power_Day_Split),2)$cluster,rep(NA,4)),7,53))%>%View()
}

#資料M1，HC5:階層式分五群。WK:星期，MON:月份，HOL:假日(用kmean分2群，2為假日)，SEA:季節
#HC5.1至HC5.5是將HC5轉換成五個值為0,1的dummy variable
{
  M1=data.table(HC5=factor(hc_cluster),
                WK=factor(rep(c(1:7),365%/%7+1)[-c(1:2)][1:365]),
                MON=factor(Month[-1]),
                HOL=factor(kmeans(t(Power_Day_Split),2)$cluster))%>%
    mutate(SEA=mapvalues(MON, c(1:12), c(4,4,1,1,1,2,2,2,3,3,3,4)))
  M1=M1%>%mutate(HC5.1=ifelse(M1$HC5==1,1,0),
                 HC5.2=ifelse(M1$HC5==2,1,0),
                 HC5.3=ifelse(M1$HC5==3,1,0),
                 HC5.4=ifelse(M1$HC5==4,1,0),
                 HC5.5=ifelse(M1$HC5==5,1,0))
}

#資料M2，是Power_Day_Split加上HC5與HC5.1至HC5.5
{
  M2=as.data.frame(t(Power_Day_Split))%>%
    mutate(HC5=M1$HC5)%>%
    mutate(HC5.1=ifelse(M1$HC5==1,1,0),
           HC5.2=ifelse(M1$HC5==2,1,0),
           HC5.3=ifelse(M1$HC5==3,1,0),
           HC5.4=ifelse(M1$HC5==4,1,0),
           HC5.5=ifelse(M1$HC5==5,1,0))
}


####################################################################################
#對所有的特徵資料做決策樹 先用來做後續的分析
{
  Tree_M1=M1%>%dplyr::select(HC5,WK,MON,HOL,SEA)
  M1_tree=rpart(HC5~., data=Tree_M1, method="class") #決策樹(分類)
  prp(M1_tree, fallen.leaves=TRUE,extra=1) #決策樹視覺化
  M1_tree_predict=predict(M1_tree, Tree_M1, type="class") #依照決策樹模型以分類方式進行預測
  M1_tree_table=table(Tree_M1$HC5, M1_tree_predict) #預測結果與實際分群的confusion table
  #sum(diag(M1_tree_table))/sum(M1_tree_table)=0.8273973 #精確度
  
  Tree_M1=Tree_M1%>%mutate(Tree.Predict=M1_tree_predict) #把預測結果加入M1
}


#預測為第1群的資料用mean處理
#rowMeans(Power_Day_Split[,which(M1$Predict==1)])

######
#以兩種方式看五類的分布情形
#第一種：
#把五群的平均值算出來，然後建立一個名為mean_class的data frame
#也可以採用中位數建成median_class，不過跟mean_class相差不大，所以後續還是利用平均
{
  mean_class=data.table(C1=rowMeans(Power_Day_Split[,which(Tree_M1$HC5==1)]),
                        C2=rowMeans(Power_Day_Split[,which(Tree_M1$HC5==2)]),
                        C3=rowMeans(Power_Day_Split[,which(Tree_M1$HC5==3)]),
                        C4=rowMeans(Power_Day_Split[,which(Tree_M1$HC5==4)]),
                        C5=rowMeans(Power_Day_Split[,which(Tree_M1$HC5==5)]))
  median_class=data.table(C1=rowMedians(Power_Day_Split[,which(Tree_M1$HC5==1)]),
                        C2=rowMedians(Power_Day_Split[,which(Tree_M1$HC5==2)]),
                        C3=rowMedians(Power_Day_Split[,which(Tree_M1$HC5==3)]),
                        C4=rowMedians(Power_Day_Split[,which(Tree_M1$HC5==4)]),
                        C5=rowMedians(Power_Day_Split[,which(Tree_M1$HC5==5)]))
}

###
#類別間猜測可能有常數關係 故將類似的分類彼此相除看看趨勢
{
  WeekdayRatio=mean_class%>%mutate(C32=C3/C2,C25=C2/C5,C35=C3/C5,C14=C1/C4)%>%dplyr::select(C32,C25,C35,C14)
  plot(c(1:96), WeekdayRatio$C32,
       type="l", xlim=c(1,96), ylim=c(min(WeekdayRatio), max(WeekdayRatio)),
       lty=1, col="red", lwd=2, xlab="time/15min", ylab="value")
  lines(c(1:96), WeekdayRatio$C25,
        type="l", xlim=c(1,96), ylim=c(min(WeekdayRatio), max(WeekdayRatio)),
        lty=1, col="blue", lwd=2, xlab="time/15min", ylab="value")
  lines(c(1:96), WeekdayRatio$C35,
        type="l", xlim=c(1,96), ylim=c(min(WeekdayRatio), max(WeekdayRatio)),
        lty=1, col="green", lwd=2, xlab="time/15min", ylab="value")
  lines(c(1:96), WeekdayRatio$C14,
        type="l", xlim=c(1,96), ylim=c(min(WeekdayRatio), max(WeekdayRatio)),
        lty=1, col="black", lwd=2, xlab="time/15min", ylab="value")
  legend(0,1.21, c("class 3/class 2","class 2/class 5","class 3/class 5","class 1/class 4"), fill=c("red","blue","green","black"))
}
###

#write.csv(mean_class, paste0("mean_class.csv"), row.names=F, quote=F) #輸出csv
#五種分類平均之後繪圖
{
  par(mfrow=c(1,1))
  COL=c("brown", "chocolate1", "darkgreen", "blue2", "black")
  plot(c(1:96), rowMeans(Power_Day_Split[,which(Tree_M1$Tree.Predict==1)]),
       type="l", xlim=c(1,96), ylim=c(min(mean_class), max(mean_class)),
       lty=1, col=COL[1], lwd=2, xlab="time/15min", ylab="value", main="Mean curve of 5 classes of power data")
  for(i in 2:5)
  {
    lines(c(1:96), rowMeans(Power_Day_Split[,which(Tree_M1$Tree.Predict==i)]),
          type="l" , lty=1, col=COL[i], lwd=2)
  }
  legend(75,1825, c("class 1","class 2","class 3","class 4","class 5"), fill=COL)
}

#第二種：
#將群內的所有圖繪出，再加入該群的平均曲線，這樣可以看到五群的分散情形
all_curve_and_mean=function(class)
{
  plot(c(1:96), type="n", xlab="time/15min", ylab="value", main=paste("Class", class),
       xlim=c(1,96), ylim=c(min(Power_Day_Split)-100, max(Power_Day_Split)+100))
  for(i in 1:length(which(Tree_M1$Tree.Predict==class)))
  {
    lines(c(1:96), Power_Day_Split[,which(Tree_M1$Tree.Predict==class)][,i],
          type="l" , lty=1, col="gray60", lwd=1)
  }
  lines(c(1:96), rowMeans(Power_Day_Split[,which(Tree_M1$Tree.Predict==class)]),
        type="l", lty=1, col="red", lwd=2)
  legend(68,2300, c(paste("all curve of class", class), paste("mean curve of class", class)), 
         fill=c("gray60","red"))
}
par(mfrow=c(1,1))
all_curve_and_mean(1)

#時間序列分析
#PowerAD:power資料中每一天扣除其預測類別的平均，在此先以HC5取代
PowerAD=array(NA,nrow(Power))
for(day in 1:365){
  class=Tree_M1$HC5[day]
  classmean=rowMeans(Power_Day_Split[,which(Tree_M1$HC5==class)])
  PowerAD[(day*96-95):(day*96)]=Power[(day*96-95):(day*96),]-classmean
}

par(mfrow=c(2,1))
tsplot(PowerAD,xlab="time/per 15 min",ylab="value",main="Time plot of PowerAD")
acf2(PowerAD,max.lag=80) #ACF&PACF
acf2(diff(PowerAD),max.lag=80) #差分後的ACF&PACF

#配適時間序列模型 利用arfima、arima、arma
{
  FIT1=arfima::arfima(PowerAD, order=c(1,0,1),cpus=2) #arfima(1,d,1)
  FIT2=arima(PowerAD,order=c(1,0,1)) #arma(1,1)
  FIT3=arima(diff(PowerAD),order=c(1,0,1)) #arima(1,1,1)
  #plot(tacvf(FIT1), maxlag=90, tacf=TRUE)
  
  #par(mfrow=c(3,1))
  #tsplot(resid(FIT2),ylab="resid_arma")
  #tsplot(resid(FIT3),ylab="resid_arima")
  #tsplot(resid(FIT1)[[1]],ylab="resid_arfima")
  
  #tsplot(resid(FIT2)[1:1000],ylab="resid_arma")
  #tsplot(resid(FIT3)[1:1000],ylab="resid_arima")
  #tsplot(resid(FIT1)[[1]][1:1000],ylab="resid_arfima")
  
  #acf2(resid(FIT2),max.lag=80,main="resid_arma")
  #acf2(resid(FIT3),max.lag=80,main="resid_arima")
  #acf2(resid(FIT1)[[1]],max.lag=80,main="resid_arfima")
}

#####################
#把一週的資料用來配適模型 並且用來預測未來一天#340 239 17 158 91
par(mfrow=c(1,1))
Dforecast=91; Dstart=Dforecast-7; Dend=Dforecast-1;
PowerAD_train=PowerAD[((96*(Dstart-1)+1)):(96*Dforecast)] #第351~358天都當作訓練資料

#滾動預測
spec=arfimaspec(mean.model=list(armaOrder=c(1,1),arfima=T))
fit1=arfimafit(spec,data=PowerAD_train,out.sample=96)
predict1=arfimaforecast(fit1, n.ahead=1, n.roll=95)
predict1_value=as.vector(predict1@forecast$seriesFor) #ArfimaPredict1day
plot(c(1:(96*7)),PowerAD[((96*(Dstart-1)+1)):(96*Dend)]
     ,xlim=c(630,770),ylim=c(-350,260),type="l",xlab="time/15min",
     ylab="Residual",main=paste("Predict residual of Day",Dforecast)) #前一週資料
lines(c((96*7):(96*8)),
      PowerAD[(96*Dend):(96*Dforecast)],type="l", col="blue",lwd=1)#真實
lines(c((96*7):(96*8)), #前一周fit後的預測結果
      c(PowerAD[96*Dend],predict1_value), type="l", col="red",lwd=1)
legend(640,-250,c("Real","Prediction"), fill=c("blue","red"))

write.csv(predict1_value, paste0("ArfimaPredict1day.csv"), row.names=F, quote=F)
#write.csv(PowerAD, paste0("PowerAD.csv"), row.names=F, quote=F)
#等價於多次一步預測
#第一次一步預測
fit2=arfimafit(spec,data=PowerAD[((96*350+1)):(96*357)])
predict2=arfimaforecast(fit2, n.ahead=1)
predict2_value=as.vector(predict2@forecast$seriesFor)
#下一時間點的真實值進來後第二次一步預測，以此類推
fit3=arfimafit(spec,data=c(PowerAD[((96*350+1)):(96*357+1)]))
predict3=arfimaforecast(fit3, n.ahead=1)
###################################################################################
#####決策樹
#特徵資料決策樹(5 fold)
{
  set.seed(10); Random=gtools::permute(1:365)
  Tree_M1=M1%>%dplyr::select(HC5,WK,MON,HOL,SEA)
  Tree_M1TrainAccuracyArray=Tree_M1TestAccuracyArray=rep(0,5)
  for(i in 0:4)
  {
    TestIndex=Random[c((1+73*i):(73*(i+1)))];   TrainIndex=Random[-c((1+73*i):(73*(i+1)))]
    Train_Tree_M1=Tree_M1[TrainIndex,];         Test_Tree_M1=Tree_M1[TestIndex,]
    
    tree=rpart(HC5~., data=Train_Tree_M1, method="class")
    Train_M1_tree_predict=predict(tree, Train_Tree_M1, type="class")
    Test_M1_tree_predict=predict(tree, Test_Tree_M1, type="class")
    Train_M1_tree_table=table(Train_Tree_M1$HC5, Train_M1_tree_predict)
    Test_M1_tree_table=table(Test_Tree_M1$HC5, Test_M1_tree_predict)
    Tree_M1TrainAccuracyArray[i+1]=sum(diag(Train_M1_tree_table))/sum(Train_M1_tree_table)
    Tree_M1TestAccuracyArray[i+1] =sum(diag(Test_M1_tree_table))/sum(Test_M1_tree_table)
  }
  Tree_M1TrainAccuracy=mean(Tree_M1TrainAccuracyArray) #0.8315068
  Tree_M1TestAccuracy =mean(Tree_M1TestAccuracyArray)  #0.7808219
}


#####LDA
#特徵資料LDA(5=fold)
{
  set.seed(20); Random=gtools::permute(1:365)
  LDA_M1=M1%>%dplyr::select(HC5,WK,MON,HOL,SEA)
  LDA_M1TrainAccuracyArray=LDA_M1TestAccuracyArray=rep(0,5)
  for(i in 0:4)
  {
    TestIndex=Random[c((1+73*i):(73*(i+1)))];   TrainIndex=Random[-c((1+73*i):(73*(i+1)))]
    Train_LDA_M1=LDA_M1[TrainIndex,];  Test_LDA_M1=LDA_M1[TestIndex,]
    Train_M1_LDA=lda(HC5~WK+MON+SEA, data=Train_LDA_M1)
    Train_M1_LDA_predict=predict(Train_M1_LDA,Train_LDA_M1)
    Test_M1_LDA_predict=predict(Train_M1_LDA,Test_LDA_M1)
    Train_M1_LDA_table=table(Train_LDA_M1$HC5, Train_M1_LDA_predict$class)
    Test_M1_LDA_table=table(Test_LDA_M1$HC5, Test_M1_LDA_predict$class)
    LDA_M1TrainAccuracyArray[i+1]=sum(diag(Train_M1_LDA_table))/sum(Train_M1_LDA_table)
    LDA_M1TestAccuracyArray[i+1] =sum(diag(Test_M1_LDA_table))/sum(Test_M1_LDA_table)
  }
  LDA_M1TrainAccuracy=mean(LDA_M1TrainAccuracyArray) #0.7986301
  LDA_M1TestAccuracy =mean(LDA_M1TestAccuracyArray)  #0.7616438
}


#每日資料LDA(5=fold)
{
  set.seed(30); Random=gtools::permute(1:365)
  LDA_M2=M2%>%dplyr::select(-c(HC5.1,HC5.2,HC5.3,HC5.4,HC5.5))
  LDA_M2TrainAccuracyArray=LDA_M2TestAccuracyArray=rep(0,5)
  for(i in 0:4)
  {
    TestIndex=Random[c((1+73*i):(73*(i+1)))];   TrainIndex=Random[-c((1+73*i):(73*(i+1)))]
    Train_LDA_M2=LDA_M2[TrainIndex,];  Test_LDA_M2=LDA_M2[TestIndex,]
    Train_M2_LDA=lda(HC5~., data=Train_LDA_M2)
    Train_M2_LDA_predict=predict(Train_M2_LDA,Train_LDA_M2)
    Test_M2_LDA_predict=predict(Train_M2_LDA,Test_LDA_M2)
    Train_M2_LDA_table=table(Train_LDA_M2$HC5, Train_M2_LDA_predict$class)
    Test_M2_LDA_table=table(Test_LDA_M2$HC5, Test_M2_LDA_predict$class)
    LDA_M2TrainAccuracyArray[i+1]=sum(diag(Train_M2_LDA_table))/sum(Train_M2_LDA_table)
    LDA_M2TestAccuracyArray[i+1] =sum(diag(Test_M2_LDA_table))/sum(Test_M2_LDA_table)
  }
  LDA_M2TrainAccuracy=mean(LDA_M2TrainAccuracyArray) #0.9856164
  LDA_M2TestAccuracy =mean(LDA_M2TestAccuracyArray)  #0.830137
}


#####GLM
#特徵資料GLM(5=fold)
{
  set.seed(100); Random=gtools::permute(1:365)
  GLM_M1=M1
  GLM_M1TrainAccuracyArray=GLM_M1TestAccuracyArray=rep(0,5)
  for(i in 0:4)
  {
    TestIndex=Random[c((1+73*i):(73*(i+1)))];   TrainIndex=Random[-c((1+73*i):(73*(i+1)))]
    Train_GLM_M1=GLM_M1[TrainIndex,];  Test_GLM_M1=GLM_M1[TestIndex,]
    
    glm_M1_01=glm(HC5.1~.,family=binomial,data=Train_GLM_M1%>%dplyr::select(-c(HC5,HC5.2,HC5.3,HC5.4,HC5.5)))
    glm_M1_02=glm(HC5.2~.,family=binomial,data=Train_GLM_M1%>%dplyr::select(-c(HC5,HC5.1,HC5.3,HC5.4,HC5.5)))
    glm_M1_03=glm(HC5.3~.,family=binomial,data=Train_GLM_M1%>%dplyr::select(-c(HC5,HC5.1,HC5.2,HC5.4,HC5.5)))
    glm_M1_04=glm(HC5.4~.,family=binomial,data=Train_GLM_M1%>%dplyr::select(-c(HC5,HC5.1,HC5.2,HC5.3,HC5.5)))
    glm_M1_05=glm(HC5.5~.,family=binomial,data=Train_GLM_M1%>%dplyr::select(-c(HC5,HC5.1,HC5.2,HC5.3,HC5.4)))
    
    prob_GLM_Train_M1=cbind(predict(glm_M1_01,Train_GLM_M1,type="response"),
                            predict(glm_M1_02,Train_GLM_M1,type="response"),
                            predict(glm_M1_03,Train_GLM_M1,type="response"),
                            predict(glm_M1_04,Train_GLM_M1,type="response"),
                            predict(glm_M1_05,Train_GLM_M1,type="response"))
    prob_GLM_Test_M1 =cbind(predict(glm_M1_01,Test_GLM_M1,type="response"),
                            predict(glm_M1_02,Test_GLM_M1,type="response"),
                            predict(glm_M1_03,Test_GLM_M1,type="response"),
                            predict(glm_M1_04,Test_GLM_M1,type="response"),
                            predict(glm_M1_05,Test_GLM_M1,type="response"))
    predict_GLM_Train_M1=predict_GLM_Test_M1=array()
    for(j in 1:nrow(prob_GLM_Train_M1)){predict_GLM_Train_M1[j]=which(prob_GLM_Train_M1[j,]==max(prob_GLM_Train_M1[j,]))}
    for(j in 1:nrow(prob_GLM_Test_M1)){predict_GLM_Test_M1[j]=which(prob_GLM_Test_M1[j,]==max(prob_GLM_Test_M1[j,]))}
    Train_M1_GLM_table=table(Train_GLM_M1$HC5, predict_GLM_Train_M1)
    Test_M1_GLM_table=table(Test_GLM_M1$HC5, predict_GLM_Test_M1)
    GLM_M1TrainAccuracyArray[i+1]=sum(diag(Train_M1_GLM_table))/sum(Train_M1_GLM_table)
    GLM_M1TestAccuracyArray[i+1] =sum(diag(Test_M1_GLM_table))/sum(Test_M1_GLM_table)
  }
  GLM_M1TrainAccuracy=mean(GLM_M1TrainAccuracyArray) #0.8390411
  GLM_M1TestAccuracy =mean(GLM_M1TestAccuracyArray)  #0.7753425
}


#每日資料GLM(5=fold)
{
  set.seed(200); Random=gtools::permute(1:365)
  GLM_M2=M2
  GLM_M2TrainAccuracyArray=GLM_M2TestAccuracyArray=rep(0,5)
  for(i in 0:4)
  {
    TestIndex=Random[c((1+73*i):(73*(i+1)))];   TrainIndex=Random[-c((1+73*i):(73*(i+1)))]
    Train_GLM_M2=GLM_M2[TrainIndex,];  Test_GLM_M2=GLM_M2[TestIndex,]
    
    glm_M2_01=glm(HC5.1~.,family=binomial,data=Train_GLM_M2%>%dplyr::select(-c(HC5,HC5.2,HC5.3,HC5.4,HC5.5)))
    glm_M2_02=glm(HC5.2~.,family=binomial,data=Train_GLM_M2%>%dplyr::select(-c(HC5,HC5.1,HC5.3,HC5.4,HC5.5)))
    glm_M2_03=glm(HC5.3~.,family=binomial,data=Train_GLM_M2%>%dplyr::select(-c(HC5,HC5.1,HC5.2,HC5.4,HC5.5)))
    glm_M2_04=glm(HC5.4~.,family=binomial,data=Train_GLM_M2%>%dplyr::select(-c(HC5,HC5.1,HC5.2,HC5.3,HC5.5)))
    glm_M2_05=glm(HC5.5~.,family=binomial,data=Train_GLM_M2%>%dplyr::select(-c(HC5,HC5.1,HC5.2,HC5.3,HC5.4)))
    
    prob_GLM_Train_M2=cbind(predict(glm_M2_01,Train_GLM_M2,type="response"),
                            predict(glm_M2_02,Train_GLM_M2,type="response"),
                            predict(glm_M2_03,Train_GLM_M2,type="response"),
                            predict(glm_M2_04,Train_GLM_M2,type="response"),
                            predict(glm_M2_05,Train_GLM_M2,type="response"))
    prob_GLM_Test_M2 =cbind(predict(glm_M2_01,Test_GLM_M2,type="response"),
                            predict(glm_M2_02,Test_GLM_M2,type="response"),
                            predict(glm_M2_03,Test_GLM_M2,type="response"),
                            predict(glm_M2_04,Test_GLM_M2,type="response"),
                            predict(glm_M2_05,Test_GLM_M2,type="response"))
    predict_GLM_Train_M2=predict_GLM_Test_M2=array()
    for(j in 1:nrow(prob_GLM_Train_M2)){predict_GLM_Train_M2[j]=which(prob_GLM_Train_M2[j,]==max(prob_GLM_Train_M2[j,]))}
    for(j in 1:nrow(prob_GLM_Test_M2)){predict_GLM_Test_M2[j]=which(prob_GLM_Test_M2[j,]==max(prob_GLM_Test_M2[j,]))}
    Train_M2_GLM_table=table(Train_GLM_M2$HC5, predict_GLM_Train_M2)
    Test_M2_GLM_table=table(Test_GLM_M2$HC5, predict_GLM_Test_M2)
    GLM_M2TrainAccuracyArray[i+1]=sum(diag(Train_M2_GLM_table))/sum(Train_M2_GLM_table)
    GLM_M2TestAccuracyArray[i+1] =sum(diag(Test_M2_GLM_table))/sum(Test_M2_GLM_table)
  }
  GLM_M2TrainAccuracy=mean(GLM_M2TrainAccuracyArray) #0.9746575
  GLM_M2TestAccuracy =mean(GLM_M2TestAccuracyArray)  #0.5342466
}


########
#每一類抽出1/3的天數出來做arfima的residual預測
class1=which(Tree_M1$HC5==1)[which(Tree_M1$HC5==1)>7]
class2=which(Tree_M1$HC5==2)[which(Tree_M1$HC5==2)>7]
class3=which(Tree_M1$HC5==3)[which(Tree_M1$HC5==3)>7&which(Tree_M1$HC5==3)!=22]
class4=which(Tree_M1$HC5==4)[which(Tree_M1$HC5==4)>7&which(Tree_M1$HC5==4)!=200&which(Tree_M1$HC5==4)!=201]
class5=which(Tree_M1$HC5==5)[which(Tree_M1$HC5==5)>7&which(Tree_M1$HC5==5)!=196&which(Tree_M1$HC5==5)!=365]

set.seed(99)
Sclass1=sort(sample(class1, size=floor(length(class1)/3)))
Sclass2=sort(sample(class2, size=floor(length(class2)/3)))
Sclass3=sort(sample(class3, size=floor(length(class3)/3)))
Sclass4=sort(sample(class4, size=floor(length(class4)/3)))
Sclass5=sort(sample(class5, size=floor(length(class5)/3)))

PRofClass1=data.frame(matrix(NA,96,length(Sclass1))); colnames(PRofClass1)=Sclass1
PRofClass2=data.frame(matrix(NA,96,length(Sclass2))); colnames(PRofClass2)=Sclass2
PRofClass3=data.frame(matrix(NA,96,length(Sclass3))); colnames(PRofClass3)=Sclass3
PRofClass4=data.frame(matrix(NA,96,length(Sclass4))); colnames(PRofClass4)=Sclass4
PRofClass5=data.frame(matrix(NA,96,length(Sclass5))); colnames(PRofClass5)=Sclass5

SC=PRofClass5
for(i in 1:ncol(SC))
{
  Dforecast=as.numeric(colnames(SC)[i]); Dstart=Dforecast-7; Dend=Dforecast-1;
  PowerAD_train=PowerAD[((96*(Dstart-1)+1)):(96*Dforecast)]
  spec=arfimaspec(mean.model=list(armaOrder=c(1,1),arfima=T))
  fit1=arfimafit(spec,data=PowerAD_train,out.sample=96)
  predict1=arfimaforecast(fit1, n.ahead=1, n.roll=95)
  predict1_value=as.vector(predict1@forecast$seriesFor) 
  SC[,i]=predict1_value
  print(as.numeric(colnames(SC)[i]))
}

write.csv(SC, paste0("ArfimaPredictclass5.csv"), row.names=F, quote=F)
#mean(predict1_value) #34.9351
