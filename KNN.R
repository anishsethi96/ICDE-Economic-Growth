library(class)

EduData<-read.csv(file.choose(), header = TRUE)
GDPData<-read.csv(file.choose(), header = TRUE)
GVAData<-read.csv(file.choose(), header = TRUE)

### JOINING THE DATA
GDPRegions <- GDPData[ which(GDPData$Country.or.Area == 'China' |
                               GDPData$Country.or.Area == 'Greece' |
                               GDPData$Country.or.Area == 'India' |
                               GDPData$Country.or.Area == 'Syrian Arab Republic' |
                               GDPData$Country.or.Area == 'United States'),]
GDPRegions <- GDPRegions[ which(GDPRegions$Year <= '2013' & GDPRegions$Year >= '1975'),]

GVADataRegions <- GVAData[ which(GVAData$ï..Country.or.Area == 'China' |
                                   GVAData$ï..Country.or.Area == 'Greece' |
                                   GVAData$ï..Country.or.Area == 'India' |
                                   GVAData$ï..Country.or.Area == 'Syrian Arab Republic' |
                                   GVAData$ï..Country.or.Area == 'United States'),]
GVADataRegions <- GVADataRegions[ which(GVADataRegions$Year <= '2013' & GVADataRegions$Year >= '1975'),]

EduData['GDP(in Billions of USD)']= GDPRegions['GDP..In.Billions.of.USD.']
EduData['GVA by Agro']= GVADataRegions['Agriculture..hunting..forestry.and.fishing....of.gross.value.added.']
EduData['GVA by Indus']= GVADataRegions['Industry....of.gross.value.added.']
EduData['GVA by Serv']= GVADataRegions['Services....of.gross.value.added.']
EduData <-na.omit(EduData)

### ANALYSIS OF GLOBAL DATA

cor(EduData$`GDP(in Billions of USD)`, EduData$Primary)
cor(EduData$`GDP(in Billions of USD)`, EduData$Secondary)
cor(EduData$`GDP(in Billions of USD)`, EduData$Tertiary)
cor(EduData$`GDP(in Billions of USD)`, EduData$`GVA by Agro`)
cor(EduData$`GDP(in Billions of USD)`, EduData$`GVA by Indus`)
cor(EduData$`GDP(in Billions of USD)`, EduData$`GVA by Serv`)

### ANALYSIS FOR INDIVIDUAL COUNTRIES
### INDIA

IndiaEduData <- EduData[ which(EduData$ï..Region=='India'), ]

cor(IndiaEduData$`GDP(in Billions of USD)`, IndiaEduData$Primary)
cor(IndiaEduData$`GDP(in Billions of USD)`, IndiaEduData$Secondary)
cor(IndiaEduData$`GDP(in Billions of USD)`, IndiaEduData$Tertiary)
cor(IndiaEduData$`GDP(in Billions of USD)`, IndiaEduData$`GVA by Agro`)
cor(IndiaEduData$`GDP(in Billions of USD)`, IndiaEduData$`GVA by Indus`)
cor(IndiaEduData$`GDP(in Billions of USD)`, IndiaEduData$`GVA by Serv`)

MRegLM<-lm(IndiaEduData$`GDP(in Billions of USD)` ~ IndiaEduData$`GVA by Serv` + IndiaEduData$Tertiary)
MRegLM
summary(MRegLM)
MRegCoeff<-coef(MRegLM)
MRegCoeff
IndiaEduData$Tertiary<-(58175135)
NEW<-data.frame(IndiaEduData$Primary,IndiaEduData$Secondary,IndiaEduData$Tertiary)
pENV<- predict(MRegLM,NEW,interval="prediction")
pENV
summary(pENV)

### USA

USAEduData <- EduData[ which(EduData$ï..Region=='United States of America'), ]

cor(USAEduData$`GDP(in Billions of USD)`, USAEduData$Primary)
cor(USAEduData$`GDP(in Billions of USD)`, USAEduData$Secondary)
cor(USAEduData$`GDP(in Billions of USD)`, USAEduData$Tertiary)
cor(USAEduData$`GDP(in Billions of USD)`, USAEduData$`GVA by Agro`)
cor(USAEduData$`GDP(in Billions of USD)`, USAEduData$`GVA by Indus`)
cor(USAEduData$`GDP(in Billions of USD)`, USAEduData$`GVA by Serv`)

MRegLM<-lm(USAEduData$`GDP(in Billions of USD)` ~USAEduData$`GVA by Serv` + USAEduData$Tertiary)
MRegLM
summary(MRegLM)
MRegCoeff<-coef(MRegLM)
MRegCoeff
USAEduData$Tertiary<-(19700221)
NEW<-data.frame(USAEduData$Primary,USAEduData$Secondary,USAEduData$Tertiary)
pENV<- predict(MRegLM,NEW,interval="prediction")
pENV
summary(pENV)

### Greece

GreeceEduData <- EduData[ which(EduData$ï..Region=='Greece'), ]

cor(GreeceEduData$`GDP(in Billions of USD)`, GreeceEduData$Primary)
cor(GreeceEduData$`GDP(in Billions of USD)`, GreeceEduData$Secondary)
cor(GreeceEduData$`GDP(in Billions of USD)`, GreeceEduData$Tertiary)
cor(GreeceEduData$`GDP(in Billions of USD)`, GreeceEduData$`GVA by Agro`)
cor(GreeceEduData$`GDP(in Billions of USD)`, GreeceEduData$`GVA by Indus`)
cor(GreeceEduData$`GDP(in Billions of USD)`, GreeceEduData$`GVA by Serv`)

MRegLM<-lm(GreeceEduData$`GDP(in Billions of USD)` ~ GreeceEduData$`GVA by Serv` + GreeceEduData$Tertiary)
MRegLM
summary(MRegLM)
MRegCoeff<-coef(MRegLM)
MRegCoeff
GreeceEduData$Tertiary<-(19700221)
NEW<-data.frame(GreeceEduData$Primary,GreeceEduData$Secondary,GreeceEduData$Tertiary)
pENV<- predict(MRegLM,NEW,interval="prediction")
pENV
summary(pENV)

### China

ChinaEduData <- EduData[ which(EduData$ï..Region=='China'), ]

cor(ChinaEduData$`GDP(in Billions of USD)`, ChinaEduData$Primary)
cor(ChinaEduData$`GDP(in Billions of USD)`, ChinaEduData$Secondary)
cor(ChinaEduData$`GDP(in Billions of USD)`, ChinaEduData$Tertiary)
cor(ChinaEduData$`GDP(in Billions of USD)`, ChinaEduData$`GVA by Agro`)
cor(ChinaEduData$`GDP(in Billions of USD)`, ChinaEduData$`GVA by Indus`)
cor(ChinaEduData$`GDP(in Billions of USD)`, ChinaEduData$`GVA by Serv`)

MRegLM<-lm(ChinaEduData$`GDP(in Billions of USD)` ~ChinaEduData$`GVA by Serv` + ChinaEduData$Tertiary)
MRegLM
summary(MRegLM)
MRegCoeff<-coef(MRegLM)
MRegCoeff
ChinaEduData$Tertiary<-(19700221)
NEW<-data.frame(ChinaEduData$Primary,ChinaEduData$Secondary,ChinaEduData$Tertiary)
pENV<- predict(MRegLM,NEW,interval="prediction")
pENV
summary(pENV)

### Syrian Arab Republic

SyriaEduData <- EduData[ which(EduData$ï..Region=='Syrian Arab Republic'), ]

cor(SyriaEduData$`GDP(in Billions of USD)`, SyriaEduData$Primary)
cor(SyriaEduData$`GDP(in Billions of USD)`, SyriaEduData$Secondary)
cor(SyriaEduData$`GDP(in Billions of USD)`, SyriaEduData$Tertiary)
cor(SyriaEduData$`GDP(in Billions of USD)`, SyriaEduData$`GVA by Agro`)
cor(SyriaEduData$`GDP(in Billions of USD)`, SyriaEduData$`GVA by Indus`)
cor(SyriaEduData$`GDP(in Billions of USD)`, SyriaEduData$`GVA by Serv`)

MRegLM<-lm(SyriaEduData$`GDP(in Billions of USD)` ~SyriaEduData$`GVA by Indus` + SyriaEduData$Tertiary)
MRegLM
summary(MRegLM)
MRegCoeff<-coef(MRegLM)
MRegCoeff
SyriaEduData$Tertiary<-(19700221)
NEW<-data.frame(SyriaEduData$Primary,SyriaEduData$Secondary,SyriaEduData$Tertiary)
pENV<- predict(MRegLM,NEW,interval="prediction")
pENV
summary(pENV)

### KNN

CountryGDP <- as.numeric(EduData$`GDP(in Billions of USD)`)
CountryGDP <- cut(CountryGDP, br=c(-1,60,2500,17000), labels = c("Bad", 'good', 'best'))
CountryGDP <- as.factor(CountryGDP)
summary(CountryGDP)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
cleanEdu = EduData[, -c(1:3)]

CGDPData <- as.data.frame(lapply(cleanEdu, normalize))
i <- sample(162, 22)
CGDPData_train <- CGDPData[i,]
CGDPData_test <- CGDPData[-i,]
cl <- CGDPData_train$GDP.in.Billions.of.USD.

Edu_KNN <- knn(train = CGDPData_train, test = CGDPData_test, cl, k = 7)
table(Edu_KNN)
Edu_KNN
confmat <- table(Edu_KNN, CGDPData_test$GDP.in.Billions.of.USD.)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(confmat)