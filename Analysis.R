#packages
library(ggplot2)
library(GGally)
library(ggformula)
#Get the data
library(readr)
processed_data <- read_csv("processed data.csv")
head(processed_data)
str(processed_data)

#Slide 4
ggcorr(hjust = 0.75, processed_data[, 2:12], label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE)

fit <- aov( stroke ~ Residence_type+gender+age+hypertension+heart_disease+work_type+ever_married+avg_glucose_level+bmi+smoking_status, data = processed_data)     
summary(fit)

glm(stroke~ age + gender+heart_disease+avg_glucose_level+ever_married+Residence_type,
    family = "binomial",
    data = processed_data)

hist(processed_data$age,main="histogramm of age",xlab="age",col="darkseagreen",breaks =30)

#slide 5
allw<-subset(processed_data,gender==1)
allm<-subset(processed_data,gender==0)
piechart<-subset(processed_data,gender==0 | gender==1)
pie(table(piechart$gender),labels = naming,main = "Distribution gender",col=c("lightsteelblue","lightpink"))
naming<-c("men","women")

gf_dens(~age, data = processed_data,ylab = "Times",xlab = "All Ages", alpha = 0.85,title= "Age Density")
gf_dens(~age, data = allm,ylab = "Times",xlab = "Age men", alpha = 0.85,title="Age of all Men")
gf_dens(~age, data = allw,ylab = "Times",xlab = "Age women", alpha = 0.85,title="Age of all Women")
gf_jitter(stroke ~ age, data = processed_data,width=.1)
boxplot(data=processed_data,age~stroke)

#subset of stroke
allstroke1<-subset(processed_data,stroke==1)
mean(allstroke1$age)
median(allstroke1$age)
allstroke0<-subset(processed_data,stroke==0)
mean(allstroke0$age)
median(allstroke0$age)
#all gender
median(processed_data$age)
mean(processed_data$age)
boxplot(data=processed_data,age~stroke)
#men
boxplot(data=allm,age~stroke)
median(allm$age)
mean(allm$age)
#women
boxplot(data=allw,age~stroke)
median(allw$age)
mean(allw$age)

#slide 6
#heart disease
ggplot(data = processed_data) +
  geom_point(mapping = aes(x = stroke, y = age, color = heart_disease))
ggplot(data = processed_data) +
  geom_point(mapping = aes(x = heart_disease, y = age, color = stroke))+xlim(0, 1)
?geom_point
ggplot(data = processed_data) +
  geom_point(mapping = aes(x = heart_disease, y = age, color = stroke))+ylim(65, 85)

boxplot(processed_data$age~processed_data$heart_disease)#older people have more heart disease 

#avg_glucose_level
ggplot(data = processed_data,
       mapping = aes(y = avg_glucose_level,
                     x = age)) +
  geom_point() +
  geom_smooth()

ggplot(data = allm,
       mapping = aes(y = avg_glucose_level,
                     x = age)) +
  geom_point() +
  geom_smooth()

ggplot(data = allw,
       mapping = aes(y = avg_glucose_level,
                     x = age)) +
  geom_point() +
  geom_smooth()

mean(allw$avg_glucose_level)
median(allw$avg_glucose_level)
mean(allm$avg_glucose_level)
median(allm$avg_glucose_level)

# Bmi diagramm
library(ggplot2)
ggplot(data = processed_data,
       mapping = aes(y = bmi,
                     x = age)) +
  geom_point() +
  geom_smooth()

median(processed_data$bmi)
mean(processed_data$bmi)





#Other things we have tried 
# Boxplot
boxplot(data=processed_data,age~smoking_status)

boxplot(data=processed_data,age~work_type)
hist(processed_data$work_type,main="histogramm of working_type",xlab="working_type",col="darkseagreen",breaks =4)
sum(processed_data[,6])

#model that predict as high as possible, interpretate the data 
#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(fit_2)

#Reason of glucose level and age
#slide 2 reason for the high glucose level 
ggplot(data = processed_data,
       mapping = aes(y = avg_glucose_level,
                     x = age)) +
  geom_point() +
  geom_smooth()

glm(stroke~ age + gender+heart_disease+avg_glucose_level,
    family = "binomial",
    data = processed_data)

ggplot(data = processed_data,
       mapping = aes(y = heart_disease,
                     x = age)) +
geom_point()


#Reason for work_type and stroke
boxplot(data=processed_data,age~work_type)

##Logistic regression
f.full <- glm(stroke ~ gender + age + hypertension + heart_disease + ever_married +
                work_type + Residence_type + avg_glucose_level + smoking_status + bmi, family = "binomial",data = processed_data)
summary(f.full)

fit_2 <- lm(stroke ~ avg_glucose_level + heart_disease, data = processed_data)
summary(fit_2)

fit_3 <- lm(stroke ~ avg_glucose_level + heart_disease + ever_married + Residence_type + hypertension + work_type + bmi + smoking_status, data = processed_data)
summary(fit_3)

#Step by step variable selection
f.full <- glm(stroke ~ gender + age + hypertension + heart_disease + ever_married +
                work_type + Residence_type + avg_glucose_level + smoking_status + bmi, family = "binomial",data = processed_data)
f.empty <- glm(stroke ~ NULL, family = "binomial", data = processed_data)
add1(f.empty, scope = f.full)

f.1 <- update(f.empty, . ~ . + age)
add1(f.1, scope = f.full)

f.2 <- update(f.1, . ~ . + heart_disease)
add1(f.2, scope = f.full)

f.3 <- update(f.2, . ~ . + avg_glucose_level)
add1(f.3, scope = f.full)

f.4 <- update(f.3, . ~ . + hypertension)
add1(f.4, scope = f.full)

f.5 <- update(f.4, . ~ . + bmi)
add1(f.5, scope = f.full)
