library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(Metrics)


rd_csv<-read.csv(file.choose())

head(rd_csv)

str(rd_csv)

rd_csv <- rd_csv %>% mutate(hd = ifelse(class > 0, 1, 0),sex=factor(sex,levels = c(0,1),labels = c("female","male")))

#hd<-rd_csv %>% ifelse(class > 0,1,0)
#sex<-factor(rd_csv$sex,levels = c(0,1),labels = c("female","male"))

hd_sex<-chisq.test(rd_csv$sex,rd_csv$hd)
hd_age<-t.test(rd_csv$age,rd_csv$hd)
hd_heartrate<-t.test(rd_csv$thalach,rd_csv$hd)

#hd_labelled<-ifelse(hd == 0,yes = "No disease",no="Disease")
hd_data_correct <- mutate(rd_csv, hd_labelled = ifelse(hd==0, "No disease", "Disease"))
ggplot(hd_data_correct,aes(x=hd_labelled,y=age))+geom_boxplot()

ggplot(hd_data_correct,aes(x=hd_labelled,fill=sex))+geom_bar(position = "fill")+ylab("sex %")                           

ggplot(hd_data_correct,aes(x=hd_labelled,y=thalach))+geom_boxplot()

model<-glm(hd~age+sex+thalach,data = rd_csv,family = "binomial")
summary(model)

  tidy_m<-tidy(model)
  tidy_m$OR<-exp(tidy_m$estimate)
  # calculate 95% CI and save as lower CI and upper CI
  tidy_m$lower_CI <- exp(tidy_m$estimate - 1.96 * tidy_m$std.error)
  tidy_m$upper_CI <- exp(tidy_m$estimate + 1.96 * tidy_m$std.error)

#predict
pred<-predict(model,type = "response",newdata = rd_csv)
rd_csv$pred_pre<-pred<-ifelse(pred>=0.5,1,0)

#calculate auc,accuracy,classification error
auc<-auc(rd_csv$hd,rd_csv$pred_pre)
acc<-accuracy(rd_csv$hd,rd_csv$pred_pre)
ce<-ce(rd_csv$hd,rd_csv$pred_pre)

#confusion matrix
confusion_matrix<-table(rd_csv$hd,rd_csv$pred_pre,dnn = c("True Status","Predicted Status"))
