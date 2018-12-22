install.packages("dplyr")
install.packages("usmap")
install.packages("dslabs")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("ggthemes")

library(ggplot2)
library(dplyr)
library(usmap)
library(dslabs)
library(RColorBrewer)
library(ggthemes)

dslabs::murders

murders$test <- murders$total/murders$population*10^6

data <- read.csv(file="C:/Users/dubac/Downloads/data_per_state_pop.csv",header=TRUE,sep=",")
names(data)[names(data)=="Staat"] <- "state"

q1 <- 20
q2 <- 30
q3 <- 40
q4 <- 80
q5 <- 120

data$rate <-  ifelse(data$Morde_pro_Million <= q1,paste("Risikostufe 1 - bis",q1,"Morde"),
              ifelse(data$Morde_pro_Million <= q2,paste("Risikostufe 2 - bis",q2,"Morde"),
              ifelse(data$Morde_pro_Million <= q3,paste("Risikostufe 3 - bis",q3,"Morde"),
              ifelse(data$Morde_pro_Million <= q4,paste("Risikostufe 4 - bis",q4,"Morde"),
              ifelse(data$Morde_pro_Million <= q5,paste("Risikostufe 5 - bis",q5,"Morde"),
                                               paste("Risikostufe 6 - über",q5,"Morde"))))))

usmap::plot_usmap(data = data, values = "rate" , lines="white", labels=TRUE) + 
  scale_fill_brewer(type = "div", palette = 8, direction = -1,
                    name = "Wahrscheinlichkeit ermordet zu werden") + 
  theme(legend.position = "right") + 
  labs(title = "Mordrate pro Million Einwohner", subtitle = "Zeitraum Anfang 2014 - Ende 2016 / USA") +
  theme(legend.position = "right")




data %>% 
  ggplot(aes(y=Morde_pro_Million,x=population/10^6,label = abb)) + 
  geom_label(aes(fill = region), colour = "white" ) +
  geom_smooth(method='lm',formula=y~x, color="red", size=1)+
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Bevölkerung pro Million (Skala log10)") +
  ylab("Anzahl Morde pro Million (Skala log10)") +
  ggtitle("Anzahl Morde pro Million zu Einwohner pro Million pro Staat") +
  scale_fill_manual(name="Region",values = c("#d8ae00", "#af0800","#2f6d00","#206994")) +
  theme_economist_white()


dataCSV <- read.csv(file="C:/Users/dubac/Google Drive/R/RStudio/GunUS.csv",header=TRUE,sep=",")
dataDC <- dataCSV %>% filter(state == 'District of Columbia')

usmap::plot_usmap(data = dataDC, values = "city_or_county", region == "iowa")

# Name city or county umschreiben
colnames(dataCSV)[6] <- "city"
# incident_id in id umbenennen
colnames(dataCSV)[4] <- "id"

data_per_day <- dataCSV %>% 
  select(date, id ,n_killed,n_injured) %>%
  group_by(date) %>%
  summarise("n_Gewalttaten" = length(id),
            "n_Morde" = sum(n_killed),
            "avg_Morde" = mean(n_killed),
            "n_Verletzte" = sum(n_injured),
            "avg_Verletzte" = mean(n_injured)
  )
head(data_per_day)
data_per_day$season <- as.factor(ifelse(data_per_day$monthInt %in% c("01","02","03"),"01 Winter",
                                        ifelse(data_per_day$monthInt %in% c("04","05","06"),"02 Frühling",
                                               ifelse(data_per_day$monthInt %in% c("07","08","09"),"03 Sommer","04 Herbst"))))
data_per_day %>% ggplot(aes(monthInt,n_Gewalttaten, color = season)) + stat_boxplot(geom ='errorbar') + geom_boxplot()


usmap::plot_usmap(data = data, values = "Morde_pro_Million" , lines="white", labels=TRUE) + 
  scale_fill_continuous(low= "thistle2", high="darkred",name = "Morde pro Million Einwohner", label = scales::comma) + 
  theme(legend.position = "right") + 
  labs(title = "Anzahl Morde pro Million Einwohner", subtitle = "Zeitraum 2014 - 2016 / PD") +
  theme(legend.position = "right")

usmap::plot_usmap(data = data, values = "rate" , lines="white", labels=TRUE) + 
  scale_fill_manual(name = "Wahrscheinlichkeit ermordet zu werden", values = c("green", "yellowgreen", "yellow","orange","red","darkred")) + 
  theme(legend.position = "right") + 
  labs(title = "Mordrate pro Million Einwohner", subtitle = "Zeitraum 2014 - 2016 / PD") +
  theme(legend.position = "right")

usmap::plot_usmap(data = data, values = "rate" , lines="black", labels=TRUE) + 
  scale_fill_brewer(type = "seq", palette = 9, direction = 1, aesthetics = "fill",
                    name = "Wahrscheinlichkeit ermordet zu werden") + 
  theme(legend.position = "right") + 
  labs(title = "Mordrate pro Million Einwohner", subtitle = "Zeitraum 2014 - 2016 / PD") +
  theme(legend.position = "right")















dataTot <- read.csv(file="C:/Users/dubac/Downloads/data_per_year.csv",header=TRUE,sep=",")
dataTot$Jahr <- as.factor(dataTot$Jahr)

table(dataTot)
