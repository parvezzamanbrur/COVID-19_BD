library(ggplot2)
library(lattice)
library(caret)
library(dplyr)
library(lubridate)
library(corrplot)
library(tidyverse)

#Reading the dataset
data<- read.csv("C:/Users/User/Desktop/COV_EVIR.csv", header = T)

#Data Structure
data
dim(data)
head(data)
data$District <- as.factor(data$District)
summary(data)
str(data)

#Ploting the dataset
plot(data$T2M , data$CoVcase)
plot(data$District , data$CoVcase)
plot(data$CoVcase , data$RH2M)
plot(data$CoVcase, data$PRECTOTCORR)

featurePlot(x=data[,2:10],y=data$CoVcase, plot="scatter")
featurePlot(x=data[,2:10],y= data$CoVcase,plot="ellipse")
featurePlot(x=data[,2:10],y=data$CoVcase, plot="boxplot")
featurePlot(x=data[,2:10],y=data$CoVcase, plot="density")

plotdata <- data %>% select(-c(T2M, T2M_MAX, T2M_MIN,  QV2M,  RH2M, PRECTOTCORR, WS10M_MAX, WS50M)) %>%
  mutate(date = mdy(Dates))
plotdata <- plotdata[,-1]

plotdata
dim(plotdata)

plotdata %>% filter(District == "Dhaka")  %>%
  ggplot(aes(x = date , y = CoVcase)) + geom_line() +
  scale_x_date(date_breaks = "2 month", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "New Cases", title = "New Cofirmed COVID-19 cases")

plotdata %>% filter(District == "Barguna")  %>%
  ggplot(aes(x = date , y = CoVcase)) + geom_line() +
  scale_x_date(date_breaks = "2 month", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "New Cases", title = "New Cofirmed COVID-19 cases")

plotdata %>% filter(District %in% c("Sylhet","Rangpur"))  %>%
  ggplot(aes(x = date , y = CoVcase , color = District)) + geom_line() +
  scale_x_date(date_breaks = "2 month", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Date", y = "New Cases", title = "New Cofirmed COVID-19 cases")

plotdata %>% filter(District %in% c("Sylhet","Rangpur","Barguna","Barishal"))  %>%
  ggplot(aes(x = date , y = CoVcase , color = District)) + geom_line(show.legend = FALSE) +
  scale_x_date(date_breaks = "2 month", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ District , ncol = 1 , scales = "free_y") +
  labs(x = "Date", y = "New Cases", title = "New Cofirmed COVID-19 cases")

#Correlation Analysis
cor_data=cor(data[,2:10])
cor_data
corrplot(cor_data, type="lower",method="number")

cor_data=cor(data[,1:10])
cor_data
corrplot(cor_data, type="lower",method="number")

#Sorting the dataset according to Districts
Barguna <- subset(data, data$District == "Barguna")
dBarguna <- Barguna[,-1]
Barishal <- subset(data, data$District == "Barishal")
dBarishal <- Barishal[,-1]
Bhola <- subset(data, data$District == "Bhola")
dBhola <- Bhola[,-1]
Jhalokathi <- subset(data, data$District == "Jhalokathi")
dJhalokathi <- Jhalokathi[,-1]
Patuakhali <- subset(data, data$District == "Patuakhali")
dPatuakhali <- Patuakhali[,-1]
Pirojpur <- subset(data, data$District == "Pirojpur")
dPirojpur <- Pirojpur[,-1]
Rangamati <- subset(data, data$District == "Rangamati")
dRangamati <- Rangamati[,-1]
Noakhali <- subset(data, data$District == "Noakhali")
dNoakhali <- Noakhali[,-1]
Lakshmipur <- subset(data, data$District == "Lakshmipur")
dLakshmipur <- Lakshmipur[,-1]
Khagrachari <- subset(data, data$District == "Khagrachari")
dKhagrachari <- Khagrachari[,-1]
Feni <- subset(data, data$District == "Feni")
dFeni <- Feni[,-1]
Coxsbazar <- subset(data, data$District == "Coxsbazar")
dCoxsbazar <- Coxsbazar[,-1]
Comilla <- subset(data, data$District == "Comilla")
dComilla <- Comilla[,-1]
Chittagonj <- subset(data, data$District == "Chittagonj")
dChittagonj <- Chittagonj[,-1]
Chandpur <- subset(data, data$District == "Chandpur")
dChandpur <- Chandpur[,-1]
Brahmanbaria <- subset(data, data$District == "Brahmanbaria")
dBrahmanbaria <- Brahmanbaria[,-1]
Bandarban <- subset(data, data$District == "Bandarban")
dBandarban <- Bandarban[,-1]
Dhaka <- subset(data, data$District == "Dhaka")
dDhaka <- Dhaka[,-1]
Faridpur <- subset(data, data$District == "Faridpur")
dFaridpur <- Faridpur[,-1]
Gazipur <- subset(data, data$District == "Gazipur")
dGazipur <- Gazipur[,-1]
Gopalgonj <- subset(data, data$District == "Gopalgonj")
dGopalgonj <- Gopalgonj[,-1]
Kishoregonj <- subset(data, data$District == "Kishoregonj")
dKishoregonj <- Kishoregonj[,-1]
Manikgonj <- subset(data, data$District == "Manikgonj")
dManikgonj <- Manikgonj[,-1]
Madaripur <- subset(data, data$District == "Madaripur")
dMadaripur <- Madaripur[,-1]
Munsigonj <- subset(data, data$District == "Munsigonj")
dMunsigonj <- Munsigonj[,-1]
Narayangonj <- subset(data, data$District == "Narayangonj")
dNarayangonj <- Narayangonj[,-1]
Narsingdi <- subset(data, data$District == "Narsingdi")
dNarsingdi <- Narsingdi[,-1]
Rajbari <- subset(data, data$District == "Rajbari")
dRajbari <- Rajbari[,-1]
Sariatpur <- subset(data, data$District == "Sariatpur")
dSariatpur <- Sariatpur[,-1]
Tangail <- subset(data, data$District == "Tangail")
dTangail <- Tangail[,-1]
Bagerhat <- subset(data, data$District == "Bagerhat")
dBagerhat <- Bagerhat[,-1]
Chuadanga <- subset(data, data$District == "Chuadanga")
dChuadanga <- Chuadanga[,-1]
Jessore <- subset(data, data$District == "Jessore")
dJessore <- Jessore[,-1]
Jhenaidah <- subset(data, data$District == "Jhenaidah")
dJhenaidah <- Jhenaidah[,-1]
Khulna <- subset(data, data$District == "Khulna")
dKhulna <- Khulna[,-1]
Kushtia <- subset(data, data$District == "Kushtia")
dKushtia <- Kushtia[,-1]
Magura <- subset(data, data$District == "Magura")
dMagura <- Magura[,-1]
Meherpur <- subset(data, data$District == "Meherpur")
dMeherpur <- Meherpur[,-1]
Narail <- subset(data, data$District == "Narail")
dNarail <- Narail[,-1]
Satkhira <- subset(data, data$District == "Satkhira")
dSatkhira <- Satkhira[,-1]
Jamalpur <- subset(data, data$District == "Jamalpur")
dJamalpur <- Jamalpur[,-1]
Mymensingh <- subset(data, data$District == "Mymensingh")
dMymensingh <- Mymensingh[,-1]
Netrokona <- subset(data, data$District == "Netrokona")
dNetrokona <- Netrokona[,-1]
Sherpur <- subset(data, data$District == "Sherpur")
dSherpur <- Sherpur[,-1]
Bogura <- subset(data, data$District == "Bogura")
dBogura <- Bogura[,-1]
Chapainawabgonj <- subset(data, data$District == "Chapainawabgonj")
dChapainawabgonj <- Chapainawabgonj[,-1]
Jaipurhat <- subset(data, data$District == "Jaipurhat")
dJaipurhat <- Jaipurhat[,-1]
Naogaon <- subset(data, data$District == "Naogaon")
dNaogaon <- Naogaon[,-1]
Natore <- subset(data, data$District == "Natore")
dNatore <- Natore[,-1]
Pabna <- subset(data, data$District == "Pabna")
dPabna <- Pabna[,-1]
Rajshahi <- subset(data, data$District == "Rajshahi")
dRajshahi <- Rajshahi[,-1]
Sirajgonj <- subset(data, data$District == "Sirajgonj")
dSirajgonj <- Sirajgonj[,-1]
Dinajpur <- subset(data, data$District == "Dinajpur")
dDinajpur <- Dinajpur[,-1]
Gaibandha <- subset(data, data$District == "Gaibandha")
dGaibandha <- Gaibandha[,-1]
Kurigram <- subset(data, data$District == "Kurigram")
dKurigram <- Kurigram[,-1]
Lalmonirhat <- subset(data, data$District == "Lalmonirhat")
dLalmonirhat <- Lalmonirhat[,-1]
Nilphamari <- subset(data, data$District == "Nilphamari")
dNilphamari <- Nilphamari[,-1]
Panchagarh <- subset(data, data$District == "Panchagarh")
dPanchagarh <- Panchagarh[,-1]
Rangpur <- subset(data, data$District == "Rangpur")
dRangpur <- Rangpur[,-1]
Thakurgaon <- subset(data, data$District == "Thakurgaon")
dThakurgaon <- Thakurgaon[,-1]
Habigonj <- subset(data, data$District == "Habigonj")
dHabigonj <- Habigonj[,-1]
Maulvibazar <- subset(data, data$District == "Maulvibazar")
dMaulvibazar <- Maulvibazar[,-1]
Sunamgonj <- subset(data, data$District == "Sunamgonj")
dSunamgonj <- Sunamgonj[,-1]
Sylhet <- subset(data, data$District == "Sylhet")
dSylhet <- Sylhet[,-1]


#Spliting the dataset into Trian and Test
set.seed(1234)
ind<-sample(2,nrow(data),replace = T, prob = c(0.7,0.3))
training<-data[ind==1,]
test<-data[ind==2,]

# See available algorithms in caret
modelnames <- paste(names(getModelInfo()), collapse=',  ')
modelnames

# Model Training
data$District <- as.factor(data$District)

trcontrol<-trainControl(method = 'repeatedcv',number=10,repeats = 3)
metric <- "Rsquared"
algorithmList <- c('knn', 
                   'bridge', 
                   'xgbLinear', 
                   'glm', 
                   'lm',
                   'ridge',
                   'superpc')

# Summarizing the model predictive accuracy
summary(rBarguna <- resamples(mBarguna <- caretList(CoVcase ~ ., data= dBarguna, trControl=trcontrol, methodList=algorithmList)))
bwplot(rBarguna, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rBarishal <- resamples(mBarishal <- caretList(CoVcase ~ ., data= dBarishal, trControl=trcontrol, methodList=algorithmList)))
bwplot(rBarishal, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rBhola <- resamples(mBhola <- caretList(CoVcase ~ ., data= dBhola, trControl=trcontrol, methodList=algorithmList)))
bwplot(rBhola, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rJhalokathi <- resamples(mJhalokathi <- caretList(CoVcase ~ ., data= dJhalokathi, trControl=trcontrol, methodList=algorithmList)))
bwplot(rJhalokathi, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rPatuakhali <- resamples(mPatuakhali <- caretList(CoVcase ~ ., data= dPatuakhali, trControl=trcontrol, methodList=algorithmList)))
bwplot(rPatuakhali, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rPirojpur <- resamples(mPirojpur <- caretList(CoVcase ~ ., data= dPirojpur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rPirojpur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rBandarban <- resamples(mBandarban <- caretList(CoVcase ~ ., data= dBandarban, trControl=trcontrol, methodList=algorithmList)))
bwplot(rBandarban, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rBrahmanbaria <- resamples(mBrahmanbaria <- caretList(CoVcase ~ ., data= dBrahmanbaria, trControl=trcontrol, methodList=algorithmList)))
bwplot(rBrahmanbaria, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rChandpur <- resamples(mChandpur <- caretList(CoVcase ~ ., data= dChandpur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rChandpur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rChittagonj <- resamples(mChittagonj <- caretList(CoVcase ~ ., data= dChittagonj, trControl=trcontrol, methodList=algorithmList)))
bwplot(rChittagonj, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rComilla <- resamples(mComilla <- caretList(CoVcase ~ ., data= dComilla, trControl=trcontrol, methodList=algorithmList)))
bwplot(rComilla, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rCoxsbazar <- resamples(mCoxsbazar <- caretList(CoVcase ~ ., data= dCoxsbazar, trControl=trcontrol, methodList=algorithmList)))
bwplot(rCoxsbazar, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rFeni <- resamples(mFeni <- caretList(CoVcase ~ ., data= dFeni, trControl=trcontrol, methodList=algorithmList)))
bwplot(rFeni, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rKhagrachari  <- resamples(mKhagrachari  <- caretList(CoVcase ~ ., data= dKhagrachari , trControl=trcontrol, methodList=algorithmList)))
bwplot(rKhagrachari , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rLakshmipur <- resamples(mLakshmipur <- caretList(CoVcase ~ ., data= dLakshmipur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rLakshmipur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rNoakhali <- resamples(mNoakhali <- caretList(CoVcase ~ ., data= dNoakhali, trControl=trcontrol, methodList=algorithmList)))
bwplot(rNoakhali, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rRangamati <- resamples(mRangamati <- caretList(CoVcase ~ ., data= dRangamati, trControl=trcontrol, methodList=algorithmList)))
bwplot(rRangamati, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rDhaka <- resamples(mDhaka <- caretList(CoVcase ~ ., data= dDhaka, trControl=trcontrol, methodList=algorithmList)))
bwplot(rDhaka, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rFaridpur <- resamples(mFaridpur <- caretList(CoVcase ~ ., data= dFaridpur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rFaridpur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rGazipur <- resamples(mGazipur <- caretList(CoVcase ~ ., data= dGazipur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rGazipur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rGopalgonj <- resamples(mGopalgonj <- caretList(CoVcase ~ ., data= dGopalgonj, trControl=trcontrol, methodList=algorithmList)))
bwplot(rGopalgonj, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rKishoregonj  <- resamples(mKishoregonj  <- caretList(CoVcase ~ ., data= dKishoregonj , trControl=trcontrol, methodList=algorithmList)))
bwplot(rKishoregonj , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rMadaripur <- resamples(mMadaripur <- caretList(CoVcase ~ ., data= dMadaripur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rMadaripur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rManikgonj  <- resamples(mManikgonj  <- caretList(CoVcase ~ ., data= dManikgonj , trControl=trcontrol, methodList=algorithmList)))
bwplot(rManikgonj , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rMunsigonj  <- resamples(mMunsigonj  <- caretList(CoVcase ~ ., data= dMunsigonj , trControl=trcontrol, methodList=algorithmList)))
bwplot(rMunsigonj , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rNarayangonj  <- resamples(mNarayangonj  <- caretList(CoVcase ~ ., data= dNarayangonj , trControl=trcontrol, methodList=algorithmList)))
bwplot(rNarayangonj , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rNarsingdi <- resamples(mNarsingdi <- caretList(CoVcase ~ ., data= dNarsingdi, trControl=trcontrol, methodList=algorithmList)))
bwplot(rNarsingdi, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rRajbari <- resamples(mRajbari <- caretList(CoVcase ~ ., data= dRajbari, trControl=trcontrol, methodList=algorithmList)))
bwplot(rRajbari, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rSariatpur <- resamples(mSariatpur <- caretList(CoVcase ~ ., data= dSariatpur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rSariatpur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rTangail <- resamples(mTangail <- caretList(CoVcase ~ ., data= dTangail, trControl=trcontrol, methodList=algorithmList)))
bwplot(rTangail, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rBagerhat <- resamples(mBagerhat <- caretList(CoVcase ~ ., data= dBagerhat, trControl=trcontrol, methodList=algorithmList)))
bwplot(rBagerhat, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rChuadanga <- resamples(mChuadanga <- caretList(CoVcase ~ ., data= dChuadanga, trControl=trcontrol, methodList=algorithmList)))
bwplot(rChuadanga, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

str(dJessore)
summary(rJessore <- resamples(mJessore <- caretList(CoVcase ~ ., data= dJessore, trControl=trcontrol, methodList=algorithmList)))
bwplot(rJessore, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rJhenaidah <- resamples(mJhenaidah <- caretList(CoVcase ~ ., data= dJhenaidah, trControl=trcontrol, methodList=algorithmList)))
bwplot(rJhenaidah, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rKhulna  <- resamples(mKhulna  <- caretList(CoVcase ~ ., data= dKhulna , trControl=trcontrol, methodList=algorithmList)))
bwplot(rKhulna , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rKushtia <- resamples(mKushtia <- caretList(CoVcase ~ ., data= dKushtia, trControl=trcontrol, methodList=algorithmList)))
bwplot(rKushtia, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rMagura <- resamples(mMagura <- caretList(CoVcase ~ ., data= dMagura, trControl=trcontrol, methodList=algorithmList)))
bwplot(rMagura, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rMeherpur <- resamples(mMeherpur <- caretList(CoVcase ~ ., data= dMeherpur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rMeherpur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rNarail  <- resamples(mNarail  <- caretList(CoVcase ~ ., data= dNarail , trControl=trcontrol, methodList=algorithmList)))
bwplot(rNarail , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rSatkhira <- resamples(mSatkhira <- caretList(CoVcase ~ ., data= dSatkhira, trControl=trcontrol, methodList=algorithmList)))
bwplot(rSatkhira, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rJamalpur <- resamples(mJamalpur <- caretList(CoVcase ~ ., data= dJamalpur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rJamalpur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rMymensingh <- resamples(mMymensingh <- caretList(CoVcase ~ ., data= dMymensingh, trControl=trcontrol, methodList=algorithmList)))
bwplot(rMymensingh, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rNetrokona <- resamples(mNetrokona <- caretList(CoVcase ~ ., data= dNetrokona, trControl=trcontrol, methodList=algorithmList)))
bwplot(rNetrokona, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rSherpur <- resamples(mSherpur <- caretList(CoVcase ~ ., data= dSherpur, trControl=trcontrol, methodList=algorithmList)))
bwplot(rSherpur, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rBogura <- resamples(mBogura <- caretList(CoVcase ~ ., data= dBogura, trControl=trcontrol, methodList=algorithmList)))
bwplot(rBogura, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rChapainawabgonj <- resamples(mChapainawabgonj <- caretList(CoVcase ~ ., data= dChapainawabgonj, trControl=trcontrol, methodList=algorithmList)))
bwplot(rChapainawabgonj, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rJaipurhat <- resamples(mJaipurhat <- caretList(CoVcase ~ ., data= dJaipurhat, trControl=trcontrol, methodList=algorithmList)))
bwplot(rJaipurhat, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rNaogaon <- resamples(mNaogaon <- caretList(CoVcase ~ ., data= dNaogaon, trControl=trcontrol, methodList=algorithmList)))
bwplot(rNaogaon, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rNatore <- resamples(mNatore <- caretList(CoVcase ~ ., data= dNatore, trControl=trcontrol, methodList=algorithmList)))
bwplot(rNatore, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rPabna <- resamples(mPabna <- caretList(CoVcase ~ ., data= dPabna, trControl=trcontrol, methodList=algorithmList)))
bwplot(rPabna, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rRajshahi  <- resamples(mRajshahi  <- caretList(CoVcase ~ ., data= dRajshahi , trControl=trcontrol, methodList=algorithmList)))
bwplot(rRajshahi , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rSirajgonj <- resamples(mSirajgonj <- caretList(CoVcase ~ ., data= dSirajgonj, trControl=trcontrol, methodList=algorithmList)))
bwplot(rSirajgonj, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rDinajpur  <- resamples(mDinajpur  <- caretList(CoVcase ~ ., data= dDinajpur , trControl=trcontrol, methodList=algorithmList)))
bwplot(rDinajpur , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rGaibandha  <- resamples(mGaibandha  <- caretList(CoVcase ~ ., data= dGaibandha , trControl=trcontrol, methodList=algorithmList)))
bwplot(rGaibandha , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rKurigram <- resamples(mKurigram <- caretList(CoVcase ~ ., data= dKurigram, trControl=trcontrol, methodList=algorithmList)))
bwplot(rKurigram, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rLalmonirhat <- resamples(mLalmonirhat <- caretList(CoVcase ~ ., data= dLalmonirhat, trControl=trcontrol, methodList=algorithmList)))
bwplot(rLalmonirhat, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rNilphamari  <- resamples(mNilphamari  <- caretList(CoVcase ~ ., data= dNilphamari , trControl=trcontrol, methodList=algorithmList)))
bwplot(rNilphamari , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rPanchagarh <- resamples(mPanchagarh <- caretList(CoVcase ~ ., data= dPanchagarh, trControl=trcontrol, methodList=algorithmList)))
bwplot(rPanchagarh, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rRangpur  <- resamples(mRangpur  <- caretList(CoVcase ~ ., data= dRangpur , trControl=trcontrol, methodList=algorithmList)))
bwplot(rRangpur , scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rThakurgaon <- resamples(mThakurgaon <- caretList(CoVcase ~ ., data= dThakurgaon, trControl=trcontrol, methodList=algorithmList)))
bwplot(rThakurgaon, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rHabigonj <- resamples(mHabigonj <- caretList(CoVcase ~ ., data= dHabigonj, trControl=trcontrol, methodList=algorithmList)))
bwplot(rHabigonj, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rMaulvibazar <- resamples(mMaulvibazar <- caretList(CoVcase ~ ., data= dMaulvibazar, trControl=trcontrol, methodList=algorithmList)))
bwplot(rMaulvibazar, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rSunamgonj <- resamples(mSunamgonj <- caretList(CoVcase ~ ., data= dSunamgonj, trControl=trcontrol, methodList=algorithmList)))
bwplot(rSunamgonj, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))

summary(rSylhet <- resamples(mSylhet <- caretList(CoVcase ~ ., data= dSylhet, trControl=trcontrol, methodList=algorithmList)))
bwplot(rSylhet, scales=scales <- list(x=list(relation="free"), y=list(relation="free")))
