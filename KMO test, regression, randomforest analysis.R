library(xlsx)
library("psych", lib.loc="D:/R-3.5.3/library")
library(randomForest)
library(varSelRF)
library(pROC)
library(tidyverse)
d<-read.xlsx('socialrecode2.xlsx',1)
d1<-na.omit(d)
part2<-d1[,12:21]
part3<-d1[,22:61]
part4<-d1[,62:64]
part5<-d1[,65:75]
part6<-d1[,76:82]
part7<-d1[,87:91]
part8<-d1[,92:102]
part9<-d1[,103:104]
part10<-d1[,105:108]
part3.1_3.2<-part3[,1:8]
part3.3<-part3[-(1:8)][-(8:32)]
part3.4<-part3[-(1:15)][-(10:25)]
part3.5<-part3[-(1:24)][-(9:16)]
part3.6<-part3[,33:35]
part3.7<-as.data.frame(part3[,36])
part3.8<-as.data.frame(part3[,37:39])
part3.9<-as.data.frame(part3[,40])
#kmo检验
KMO(part2)
KMO(part3)#部分未通过，可处理
KMO(part3.3)
KMO(part3.4)
KMO(part3.5)
KMO(part3.6)
KMO(part3.8)
KMO(part4)
KMO(part5)
KMO(part6)
KMO(part7)
KMO(part8)
KMO(part9)#未通过
KMO(part10)#未通过

#民主价值观
fa.parallel(part5,fa='both',n.iter = 100,main="Scree plot with parallel analysis")
factor5<-principal(part5,nfactors = 2,rotate = "varimax",scores=TRUE)
factor5
head(factor5$scores)
reg5_3.4<-lm(factor5$scores[,2]~part3.4$q30+part3.4$q31+part3.4$q32+part3.4$q33+
               part3.4$q34+part3.4$q35+part3.4$q36+part3.4$q37+part3.4$q38)
summary(reg5_3.4)
#社会公平感
fa.parallel(part8,fa='both',n.iter = 100,main="Scree plot with parallel analysis")
factor8<-principal(part8,nfactors = 2,rotate = "varimax",scores=TRUE)
factor8
#政治参与
fa.parallel(part7,fa='both',n.iter = 100,main="平行分析碎石图")
title(xlab="主成分/因子个数")
factor7<-principal(part7,nfactors = 1,rotate = "varimax",scores=TRUE)
factor7
factor7oth1<-fa(part7,nfactors = 2,rotate = "varimax",fm='ml')
factor7oth1
#3.5
fa.parallel(part3.5,fa='both',n.iter = 100,main="Scree plot with parallel analysis")
factor3.5<-principal(part3.5,nfactors = 2,rotate = "varimax",scores=TRUE)
factor3.5
#其他因子
fa.parallel(part3.6,fa='both',n.iter = 100,main="Scree plot with parallel analysis")
factor3.6<-principal(part3.6,nfactors = 1,rotate = "varimax",scores=TRUE)

fa.parallel(part3.8,fa='both',n.iter = 100,main="Scree plot with parallel analysis")
factor3.8<-principal(part3.8,nfactors = 1,rotate = "varimax",scores=TRUE)

fa.parallel(part2,fa='both',n.iter = 100,main="Scree plot with parallel analysis")
factor2<-principal(part2,nfactors =3,rotate = "varimax",scores=TRUE)


fa.parallel(part6,fa='both',n.iter = 100,main="Scree plot with parallel analysis")
factor6<-principal(part6,nfactors = 1,rotate = "varimax",scores=TRUE)

fa.parallel(part6,fa='both',n.iter = 100,main="平行分析碎石图")
factor6new<-fa(part6,nfactors = 2,rotate = "varimax",scores=TRUE,fm="ml")
factor6new

#
reg6_3.4<-lm(factor6$scores~part3.4$q30+part3.4$q31+part3.4$q32+part3.4$q33+part3.4$q34+
               part3.4$q35+part3.4$q36+part3.4$q37+part3.4$q38+part)
summary(reg6_3.4)
reg6_8.1<-lm(factor6$scores[,1]~d1$q1+d1$q2+d1$q3+d1$q4_1+d1$q4_2+d1$q4_3+d1$q4_4 +
               d1$q5+d1$q6+d1$q7+d1$q8+d1$q9+d1$q10+factor8$scores[,1]+factor8$scores[,2]+
               factor3.5$scores[,1]+factor3.5$scores[,2])
summary(reg6_8.1)
reg3.4.3_3.5<-lm(d1$q32~d1$q1+d1$q2+d1$q3_1+d1$q3_2+d1$q3_3+d1$q3_4+d1$q4_1+d1$q4_2+d1$q4_3+d1$q4_4+
                   d1$q5+d1$q6+d1$q7+d1$q8+d1$q9+d1$q10+factor8$scores[,1]+factor8$scores[,2]+
                   factor3.5$scores[,1]+factor3.5$scores[,2]+d1$q55+d1$q56+d1$q57+factor6$scores[,1])
summary(reg3.4.3_3.5)
reg7<-lm(factor7$scores[,1]~d1$q1+d1$q2+d1$q3_1+d1$q3_2+d1$q3_3+d1$q3_4+d1$q4_1+d1$q4_2+d1$q4_3+
           d1$q4_4 +d1$q5+d1$q6+d1$q7+d1$q8+d1$q9+d1$q10+factor3.5$scores[,1]+factor3.5$scores[,2])
summary(reg7)
reg7oth1<-lm(d1$q80~d1$q1+d1$q2+d1$q3_1+d1$q3_2+d1$q3_3+d1$q3_4+d1$q4_1+d1$q4_2+d1$q4_3+d1$q4_4 +
               d1$q5+d1$q6+d1$q7+d1$q8+d1$q9+d1$q10+factor3.5$scores[,1]+factor3.5$scores[,2])
summary(reg7oth1)








#randomForest
library(randomForest)
library(varSelRF)
  #模型一
rfdata1<-cbind(part10,part9,part8,part7,part6,part5,part4,part3,part2,d1$q1,d1$q2,d1$q3_1,
               d1$q3_2,d1$q3_3,d1$q3_4,d1$q4_1,d1$q4_2,d1$q4_3,d1$q4_4,d1$q5,d1$q6,d1$q7,
               d1$q8,d1$q9,d1$q10,d1$q77_1,d1$q77_2,d1$q77_3,d1$q77_4,d1$q77_5,d1$q76_1,
               d1$q76_2,d1$q76_3,d1$q76_4,d1$q76_5)

set.seed(1234)
rfa1=randomForest(rfdata1,proximity=T,importance = T)
MDSplot(rfa1,d1$a1,palette = rep((3:4),2),cex=1.5,main="模型一")
round(importance(rfa1),2)
varImpPlot(rfa1)

  

  #模型二
rfdata2<-cbind(d1$q1,d1$q2,d1$q3_1,d1$q3_2,d1$q3_3,d1$q3_4,d1$q4_1,d1$q4_2,
               d1$q4_3,d1$q4_4,d1$q5,d1$q6,d1$q7,d1$q8,d1$q9,d1$q10,
               d1$q77_1,d1$q77_2,d1$q77_3,d1$q77_4,d1$q77_5,d1$q76_1,d1$q76_2,
               d1$q76_3,d1$q76_4,d1$q76_5,factor2$scores[,1],factor2$scores[,2],
               factor2$scores[,3],part3.1_3.2,part3.3,part3.4,factor3.5$scores[,1],
               factor3.5$scores[,2],factor3.6$scores[,1],
               part3.7,factor3.8$scores[,1],part3.9,part4,
               factor5$scores[,1],factor5$scores[,2],
               factor6$scores[,1],d1$q78,d1$q79,factor7$scores[,1],
               factor8$scores[,1],factor8$scores[,2],part9,part10)
set.seed(1234)
rfa2=randomForest(rfdata2,d1$a1, proximity=T,mtry=20)
MDSplot(rfa2,d1$a1,palette = rep((3:4),3),cex=1.5,main="模型二")
round(importance(rfa2),2)
varImpPlot(rfa2,n.var=22,cex=2)

rfdata3<-cbind(rfdata2,d1$a1,factor6new[["scores"]],factor7oth1[["scores"]])
myairquality=cbind(rfdata3[,-77],matrix(runif(96*nrow(rfdata3)),nrow(rfdata3),96))
# 交叉验证中添加随机数的训练集、分组、交叉验证的次数
result=rfcv(myairquality,rfdata3$`d1$a1`,cv.fold = 5)
# 绘制错误率曲线，观察错误率与使用Markers数量的变化
with(result,plot(n.var,error.cv,log="x",type="o",lwd=2))
result=replicate(3,rfcv(myairquality,rfdata3$`d1$a1`),simplify = FALSE)
error.cv=sapply(result,"[[","error.cv")
matplot(result[[1]]$n.var,cbind(rowMeans(error.cv),error.cv),type = "l",
        lwd = c(2,rep(1,ncol(error.cv))),col = 1,lty = 1,log="x",
        xlab = "模型中变量数量",ylab="错误率")
rfdataselvar<-as.data.frame( cbind(rfdata2$q36,rfdata2$q38,rfdata2$q30,rfdata2$q37,rfdata2$q35,
                                  rfdata2$`factor3.6$scores[, 1]`,rfdata2$q32,rfdata2$`factor2$scores[, 2]`,
                                  rfdata2$`factor8$scores[, 1]`,rfdata2$`factor5$scores[, 1]`,
                                  rfdata2$`factor3.5$scores[, 1]`,rfdata2$`factor3.5$scores[, 2]`,
                                  rfdata2$q34,rfdata2$`factor2$scores[, 1]`,rfdata2$`factor6$scores[, 1]`,
                                  rfdata2$`factor8$scores[, 2]`,rfdata2$`factor7$scores[, 1]`,
                                  rfdata2$q31,rfdata2$`factor2$scores[, 3]`,rfdata2$`factor5$scores[, 2]` ,d1$q100))
names(rfdataselvar)= c("愿给官员校领导网络提意见", "网络信访举报使用频率", "网络娱乐频率",
                       "网络参与对社会事务的投票频率", "发表时政评论的频率", 
                      "新闻媒体效用感知因子",
                      "访问境外媒体网络频率", "家庭亲属信任因子",
                      "社会分配感知因子", "威权因子"
                      , "网络参政认同因子", "网络使用能力因子",
                      "在社交媒体与他人交流时政观点的频率", "朋友熟人信任因子", 
                      "政治信任因子", "社会氛围感知因子",
                      "政治参与因子", "访问新闻网站的频率", 
                      "社会大多数人信任因子", "民主因子","政府官员会重视我们的态度和看法")
names(rfdataselvar)= c("q36", "q38", "q30", "q37", "q35", 
                      "factor3.6$scores[, 1]",
                      "q32", "factor2$scores[, 2]",
                      "factor8$scores[, 1]", "factor5$scores[, 1]"
                      , "factor3.5$scores[, 1]", "factor3.5$scores[, 2]",
                      "q34", "factor2$scores[, 1]", 
                      "factor6$scores[, 1]", "factor8$scores[, 2]",
                      "factor7$scores[, 1]", "q31", 
                      "factor2$scores[, 3]", "factor5$scores[, 2]","q100")

rfa4=randomForest(rfdataselvar,as.factor(d1$a1),proximity=T)
MDSplot(rfa4,d1$a1,palette = rep((3:4),3),cex=1.5)
varImpPlot(rfa4,main = "各指标重要性",pch=21,bg=9:10)
#模型评价
rfdata3<-cbind(rfdata2,d1$a1)
index <- sample(2,nrow(rfdata3),replace = TRUE,prob=c(0.8,0.2))
traindata <- rfdata3[index==1,]
testdata <- rfdata3[index==2,]
rfa3=randomForest(traindata[,-77] ,traindata[,77], proximity=T,mtry=20)
MDSplot(rfa3,traindata[,77],palette = rep((3:4),3))
library(pROC)
predrfa3<-as.data.frame(predict(rfa3,testdata[,-77]))
roc1<-roc(testdata[,77],predrfa3$`predict(rfa3, testdata[, -77])`,levels=c("1","2"))
plot(roc1,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), grid.col=c("green","red"), 
     max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)


#已得出指标重要性，现在继续对指标进行比较
      #拆分华大厦大数据集
rfdataxd<-rfdata2[556:680,]
rfdatahd<-rfdata2[1:555,]
    #比较平均水平
rfdatahdvar<-as.data.frame( cbind(rfdatahd$q36,rfdatahd$q38,rfdatahd$q30,rfdatahd$q37,rfdatahd$q35,
        rfdatahd$`factor3.6$scores[, 1]`,rfdatahd$q32,rfdatahd$`factor2$scores[, 2]`,
        rfdatahd$`factor8$scores[, 1]`,rfdatahd$`factor5$scores[, 1]`,
        rfdatahd$`factor3.5$scores[, 1]`,rfdatahd$`factor3.5$scores[, 2]`,
        rfdatahd$q34,rfdatahd$`factor2$scores[, 1]`,rfdatahd$`factor6$scores[, 1]`,
        rfdatahd$`factor8$scores[, 2]`,rfdatahd$`factor7$scores[, 1]`,
        rfdatahd$q31,rfdatahd$`factor2$scores[, 3]`,rfdatahd$`factor5$scores[, 2]`,rfdatahd$q100) )
library(tidyverse)
names(rfdatahdvar)= c("q36", "q38", "q30", "q37", "q35", 
                      "factor3.6$scores[, 1]",
                      "q32", "factor2$scores[, 2]",
                      "factor8$scores[, 1]", "factor5$scores[, 1]"
                      , "factor3.5$scores[, 1]", "factor3.5$scores[, 2]",
                      "q34", "factor2$scores[, 1]", 
                      "factor6$scores[, 1]", "factor8$scores[, 2]",
                      "factor7$scores[, 1]", "q31", 
                      "factor2$scores[, 3]", "factor5$scores[, 2]","q100")



rfdataxdvar<-as.data.frame( cbind(rfdataxd$q36,rfdataxd$q38,rfdataxd$q30,rfdataxd$q37,rfdataxd$q35,
                                  rfdataxd$`factor3.6$scores[, 1]`,rfdataxd$q32,rfdataxd$`factor2$scores[, 2]`,
                                  rfdataxd$`factor8$scores[, 1]`,rfdataxd$`factor5$scores[, 1]`,
                                  rfdataxd$`factor3.5$scores[, 1]`,rfdataxd$`factor3.5$scores[, 2]`,
                                  rfdataxd$q34,rfdataxd$`factor2$scores[, 1]`,rfdataxd$`factor6$scores[, 1]`,
                                  rfdataxd$`factor8$scores[, 2]`,rfdataxd$`factor7$scores[, 1]`,
                                  rfdataxd$q31,rfdataxd$`factor2$scores[, 3]`,rfdataxd$`factor5$scores[, 2]`,rfdataxd$q100) )
names(rfdataxdvar)= c("q36", "q38", "q30", "q37", "q35", 
                      "factor3.6$scores[, 1]",
                      "q32", "factor2$scores[, 2]",
                      "factor8$scores[, 1]", "factor5$scores[, 1]"
                      , "factor3.5$scores[, 1]", "factor3.5$scores[, 2]",
                      "q34", "factor2$scores[, 1]", 
                      "factor6$scores[, 1]", "factor8$scores[, 2]",
                      "factor7$scores[, 1]", "q31", 
                      "factor2$scores[, 3]", "factor5$scores[, 2]","q100")
summary(rfdatahdvar)
summary(rfdataxdvar)

rfdatasumvar<-rfdataselvar
#归一化（不可标准化）
rfdatasumvar_std1<-as.data.frame(0)
rfdatasumvar_std<-as.data.frame(0)
for (m in (1:21)) {
  for (i in (1:680)) {
    dmin<-min(rfdatasumvar[,m])
    dmax<-max(rfdatasumvar[,m])
    rfdatasumvar_std1<-append(rfdatasumvar_std1,
                              as.numeric(rfdatasumvar[i,m]- dmin/ (dmax-dmin)))
  }
  rfdatasumvar_std<-rfdatasumvar_std1
  rfdatasumvar_std<-cbind(rfdatasumvar_std,rfdatasumvar_std1)
  rfdatasumvar_std1<-as.data.frame(0)
 }

rfdatasumvar_std<-scale(rfdatasumvar)
rfdatahdvar_std<-rfdatasumvar_std[1:555,]
rfdataxdvar_std<-rfdatasumvar_std[556:680,]
summary(rfdatahdvar_std)
summary(rfdataxdvar_std)

write.csv(rfdatasumvar,"rfdatasumvar.csv")
write.csv(rfdata3,"rfdata3.csv")






