library(class)
library(ggplot2)
library(hrbrthemes)
library(viridis)
temperatureColor <- "#69b3a2"
priceColor <- rgb(0.2, 0.6, 0.9, 1)

EtoPratio <- read.csv("Employment-to-population ratio.csv", header = TRUE)
GCEData <- read.csv("National accounts - Government consumption expenditure.csv", header = TRUE)

AgriPerc<-read.csv("Percent Employed in Agriculture.csv", header = TRUE)
IndusPERC<-read.csv("Percent Employed in Industry.csv", header = TRUE)
ServPERC<-read.csv("Percent Employed in Services.csv", header = TRUE)

PEnroll<-read.csv(file.choose(), header = TRUE)
SEnroll<-read.csv(file.choose(), header = TRUE)
TEnroll<-read.csv(file.choose(), header = TRUE)

PerCapGDP<-read.csv("Per capita GDP at current prices - US dollars.csv", header = TRUE)

TotRnD<-read.csv("Total number of R&D personnel in FTE.csv", header = TRUE)
LitAdult<-read.csv("Lit Rate Adult.csv", header = TRUE)
LitYoung<-read.csv("Literacy rate of 15-24 year-olds.csv", header = TRUE)

##############################################################

RegEtoPratio <- EtoPratio[ which(EtoPratio$Country.or.Area == 'Colombia'),]
RegGCE <- GCEData[ which(GCEData$Country.or.Area == 'Colombia'),]

RegAP <- AgriPerc[ which(AgriPerc$Country.or.Area == 'Colombia'),]
RegIP <- IndusPERC[ which(IndusPERC$Country.or.Area == 'Colombia'),]
RegSP <- ServPERC[ which(ServPERC$Country.or.Area == 'Colombia'),]

#RegTotRnD <- TotRnD[ which(TotRnD$Time.Period == '2014'),]
#RegLitAdult <- LitAdult[ which(LitAdult$Country.or.Area == 'Colombia'),]
#RegLitYoung <- LitYoung[ which(LitYoung$Country.or.Area == 'Colombia'),]
#RegPEnroll <- PEnroll[ which(PEnroll$ï..Country.or.Area == 'India' & PEnroll$Year > 1992),]
#RegSEnroll <- SEnroll[ which(SEnroll$ï..Country.or.Area == 'India' & SEnroll$Year > 1992),]
#RegTEnroll <- TEnroll[ which(TEnroll$ï..Country.or.Area == 'India' & TEnroll$Year > 1992),]

RegPEnroll <- PEnroll[ which(PEnroll$ï..Country.or.Area == 'India'),]
RegSEnroll <- SEnroll[ which(SEnroll$ï..Country.or.Area == 'India'),]
RegTEnroll <- TEnroll[ which(TEnroll$ï..Country.or.Area == 'India'),]

###############################################################

ggplot(RegPEnroll, aes(fill = RegPEnroll$Subgroup, y = RegPEnroll$Value, x = RegPEnroll$Year)) + 
  geom_bar(stat="identity", position = position_dodge(), color="black") +
  ggtitle("Primary School Enrollment in India") +
  xlab("Year") +
  ylab("Enrollment") +
  scale_fill_manual(values=c(temperatureColor, priceColor)) +
  labs(fill='Gender') 

ggplot(RegSEnroll, aes(fill = RegSEnroll$Subgroup, y = RegSEnroll$Value, x = RegSEnroll$Year)) + 
  geom_bar(stat="identity", position = position_dodge(), color="black") +
  ggtitle("Secondary School Enrollment in India") +
  xlab("Year") +
  ylab("Enrollment") +
  scale_fill_manual(values=c(temperatureColor, priceColor)) +
  labs(fill='Gender') 

ggplot(RegTEnroll, aes(fill = RegTEnroll$Subgroup, y = RegTEnroll$Value, x = RegTEnroll$Year)) + 
  geom_bar(stat="identity", position = position_dodge(), color="black") +
  ggtitle("Tertiary School Enrollment in India") +
  xlab("Year") +
  ylab("Enrollment") +
  scale_fill_manual(values=c(temperatureColor, priceColor)) +
  labs(fill='Gender') 

##########################

ggplot(EtoPratio, aes(x=Year, y=Value, fill=Subgroup)) + 
  geom_area(alpha=0.6 , size=1)

ggplot(RegGCE, aes(x=Year, y=Value)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  theme_ipsum() +
  ggtitle("Govt Consumption Colombia")

#####

ggplot(RegAP, aes(x=Year)) +
  
  geom_line( aes(y=Male), size=2, color=temperatureColor) + 
  geom_line( aes(y=Female), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Male %",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Female %")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("% Employed in Agriculture Colombia")

ggplot(RegIP, aes(x=Year)) +
  
  geom_line( aes(y=Male), size=2, color=temperatureColor) + 
  geom_line( aes(y=Female), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Male %",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Female %")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("% Employed in Industry Colombia")

ggplot(RegSP, aes(x=Year)) +
  
  geom_line( aes(y=Male), size=2, color=temperatureColor) + 
  geom_line( aes(y=Female), size=2, color=priceColor) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Male %",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Female %")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  ) +
  
  ggtitle("% Employed in Service Colombia")

########################

RegPerCapGDP <- PerCapGDP[ which(PerCapGDP$Country.or.Area == 'Colombia'),]

RegPerCapGDP %>%
  ggplot( aes(x=RegPerCapGDP$Year, y=RegPerCapGDP$Value)) +
  xlab("Year") +
  ylab("GDP Per Capita in US Dollars") +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  theme_ipsum() +
  ggtitle("GDP per capita of Colombia")

