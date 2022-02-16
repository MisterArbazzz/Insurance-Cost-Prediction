data <-read.csv(choose.files(caption = "Select CSV",multi = FALSE))
head(data)

#checking for missing values
colSums(is.na(data))

#removing duplicate entries
data[duplicated(data), ]
data <- data %>% distinct()

summary(data)

# modelling
insur<- lm(charges~.,data=data)
summary(insur)

# Converting Characters to boolean type Sex: Smoker:Yes=1,No=0, male=1,female=0 and 
data1 <- data %>% 
  mutate(smoking = if_else(smoker == "no",0,1)) %>% 
  mutate(sex = if_else(sex == "male",1,0))

data_trans=data1[,c('bmi','age', 'children','smoking','sex','charges')]

#pairs(~charges+bmi+age+children+sex+smoking,data=data_trans)

insur_lm <- lm(charges~bmi+age+children+smoking,data=data_trans)
summary(insur_lm)

predicted<-as.data.frame(insur_lm$fitted.values)
residual<-as.data.frame(insur_lm$residuals)
fitted<-cbind(data_trans,predicted,residual)


root_sq_error<-summary(insur_lm)$sigma 
root_sq_error/mean(data_trans$charges)
summary(insur_lm)$r.sq
