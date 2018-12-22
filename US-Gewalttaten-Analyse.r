
install.packages(c("dplyr","dslabs","ggplot2","corrgram","nortest","reshape2"))

library(dplyr)
library(dslabs)
library(ggpubr)
library(ggplot2)
library(corrgram)
library(nortest)
library(reshape2)
options(repr.plot.width=6, repr.plot.height=6)

# CSV einlesen
data <- read.csv(file="gunUS.csv",header=TRUE,sep=",")

# Auswahl der Spalten
data <- data %>% select(incident_id,date,state,city_or_county,n_killed,n_injured,latitude, longitude)
# Datum in Datum umwandeln
data$date <- as.Date(data$date)
# Year extrahieren und als Variable speichern
data$year <- as.factor(substr(data$date,1, 4))
# Lade Datensatz dsLabs
data("murders")
# Datensatz auf die Jahre 2014 - 2017 begrenzen
data <- data %>%  filter(year %in% c("2014","2015","2016"))
# Grunddatensatz ergänzen mit ABB und Region aus dem Datensatz dsLabs
data <- murders %>% select(state,abb,region) %>% merge(data, by="state")

# Name city or county umschreiben
 colnames(data)[6] <- "city"
# incident_id in id umbenennen
colnames(data)[4] <- "id"

# Datensatzlänge prüfen
length(data$id)
# Header der Datn anzeigen
head(data)

summary(data)

Tot_Gewalttaten <- length(data$id)
sprintf("Anzahl der Gewalttaten = %s", 
        Tot_Gewalttaten)
Tot_Anzahl_Tote <- sum(data$n_killed)
sprintf("Anzahl Tote bei allen Gewalttaten = %s", 
        Tot_Anzahl_Tote)
Tot_Anzahl_Verletzte <- sum(data$n_injured)
sprintf("Anzahl Verletzte bei allen Gewalttaten = %s", 
        Tot_Anzahl_Verletzte)

Tot_Sd_Tote <- sd(data$n_killed)
sprintf("Standardabweichung der Tötungen bei allen Gewalttaten = %s", 
        Tot_Sd_Tote)

Tot_Sd_Verletzte <- sd(data$n_injured)
sprintf("Standardabweichung der Verletzten bei allen Gewalttaten = %s", 
        Tot_Sd_Verletzte)

id_per_day <- length(data$id)/(365*length(unique(data$year)))
id_per_hour <- id_per_day / 24
id_per_minute <- 60 / id_per_hour

sprintf("%s Gewaltaten pro Tag im Schnitt", round(id_per_day,2))
sprintf("%s Gewaltaten pro Stunde im Schnitt", round(id_per_hour,2))
sprintf("Alle %s Minuten eine Gewalttat im Schnitt", round(id_per_minute,2))         

kills_per_day <- sum(data$n_killed)/(365*length(unique(data$year)))
kills_per_hour <- kills_per_day / 24
kills_per_minute <- 60 / kills_per_hour
                     
sprintf("%s Morde pro Tag im Schnitt", round(kills_per_day,2))
sprintf("%s Morde pro Stunde im Schnitt", round(kills_per_hour,2))
sprintf("Alle %s Minuten einen Mord im Schnitt", round(kills_per_minute,2))     

inj_per_day <- sum(data$n_injured)/(365*length(unique(data$year)))
inj_per_hour <- inj_per_day / 24
inj_per_minute <- 60 / inj_per_hour
                     
sprintf("%s Verletze pro Tag im Schnitt", round(inj_per_day,2))
sprintf("%s Verletze pro Stunde im Schnitt", round(inj_per_hour,2))
sprintf("Alle %s Minuten ein Verletzer im Schnitt", round(inj_per_minute,2))  

data_per_day <- data %>% 
  select(date, id ,n_killed,n_injured) %>%
  group_by(date) %>%
  summarise("n_Gewalttaten" = length(id),
            "n_Morde" = sum(n_killed),
            "avg_Morde" = mean(n_killed),
            "n_Verletzte" = sum(n_injured),
            "avg_Verletzte" = mean(n_injured)
)
head(data_per_day)

summary(data_per_day)

data_for_desityplot <- melt(data_per_day %>% select(n_Gewalttaten,n_Morde,n_Verletzte),id.vars=0)
ggplot(data_for_desityplot,aes(x=value, fill=variable)) + geom_density(alpha=0.5) + 
  scale_fill_discrete(name="Variable")

data_for_desityplot2 <- melt(data_per_day %>% select(avg_Morde,avg_Verletzte),id.vars=0)
ggplot(data_for_desityplot2,aes(x=value, fill=variable)) + geom_density(alpha=0.5) + 
  scale_fill_discrete(name="Variable")

ggplot(data_per_day,aes(x=n_Gewalttaten)) + geom_histogram(bins=30, color="magenta", fill="magenta", alpha=0.2)
ggplot(data_per_day,aes(x=n_Morde)) + geom_histogram(bins=30, color="magenta", fill="magenta", alpha=0.2)
ggplot(data_per_day,aes(x=n_Verletzte)) + geom_histogram(bins=30, color="magenta", fill="magenta", alpha=0.2)
ggplot(data_per_day,aes(x=avg_Morde)) + geom_histogram(bins=30, color="magenta", fill="magenta", alpha=0.2)
ggplot(data_per_day,aes(x=avg_Verletzte)) + geom_histogram(bins=30, color="magenta", fill="magenta", alpha=0.2)

qqnorm(data_per_day$n_Gewalttaten, main="Q-Q Anzahl Gewalttaten")
qqline(data_per_day$n_Gewalttaten)
qqnorm(data_per_day$n_Morde, main="Q-Q Anzahl Morde")
qqline(data_per_day$n_Morde)
qqnorm(data_per_day$n_Verletzte, main="Q-Q Anzahl Verletzte")
qqline(data_per_day$n_Verletzte)
qqnorm(data_per_day$avg_Morde, main="Q-Q Durchschnitt Morde")
qqline(data_per_day$avg_Morde)
qqnorm(data_per_day$avg_Verletzte, main="Q-Q Durchschnitt Verletzte")
qqline(data_per_day$avg_Verletzte)

cor(data_per_day %>% select(n_Gewalttaten,n_Morde,n_Verletzte,avg_Morde,avg_Verletzte))
corrgram(data_per_day %>% select(n_Gewalttaten,n_Morde,n_Verletzte,avg_Morde,avg_Verletzte))

lm_model <- lm(data_per_day$n_Morde ~ data_per_day$n_Gewalttaten+ data_per_day$avg_Verletzte)
summary(lm_model)
plot(lm_model)

data_per_day %>% ggplot(aes(n_Gewalttaten,n_Morde)) + geom_point(color="magenta",alpha = 0.2) + geom_smooth(method='lm',formula=y ~ x)
data_per_day %>% ggplot(aes(n_Gewalttaten,avg_Morde)) + geom_point(color="magenta",alpha = 0.2) + geom_smooth(method='lm',formula=y ~ x)
data_per_day %>% ggplot(aes(n_Gewalttaten,n_Verletzte)) + geom_point(color="magenta",alpha = 0.2) + geom_smooth(method='lm',formula=y ~ x)
data_per_day %>% ggplot(aes(n_Gewalttaten,avg_Verletzte)) + geom_point(color="magenta",alpha = 0.2) + geom_smooth(method='lm',formula=y ~ x)

data_per_day %>% ggplot(aes(date,n_Gewalttaten)) + geom_point(color="magenta",alpha = 0.2) + geom_smooth(method='lm',formula=y ~ x)
data_per_day %>% ggplot(aes(date,n_Morde)) + geom_point(color="magenta",alpha = 0.2) + geom_smooth(method='lm',formula=y ~ x)
data_per_day %>% ggplot(aes(date,n_Verletzte)) + geom_point(color="magenta",alpha = 0.2) + geom_smooth(method='lm',formula=y ~ x)

# Ergänze Monat zum Datenframe
data_per_day$month <- as.factor(format(data_per_day$date,"%B"))
data_per_day$monthInt <- format(data_per_day$date,"%m")
head(data_per_day)

data_per_month <- data_per_day %>%
    select(n_Gewalttaten,n_Morde,n_Verletzte,month,monthInt) %>%
    group_by("Monat" = month) %>%
    summarise("n_Gewalttaten" = sum(n_Gewalttaten),
            "n_Morde" = sum(n_Morde),
            "n_Verletzte" = sum(n_Verletzte),
             "Monat_Zahl" = max(monthInt)
    )
data_per_month

data_per_month %>% ggplot(aes(x=Monat_Zahl,y=n_Morde)) + 
    geom_col(color="magenta",fill = "magenta",alpha=0.3)

data_per_day$season <- as.factor(ifelse(data_per_day$monthInt %in% c("01","02","03"),"01 Winter",
                                        ifelse(data_per_day$monthInt %in% c("04","05","06"),"02 Frühling",
                                                ifelse(data_per_day$monthInt %in% c("07","08","09"),"03 Sommer","04 Herbst"))))
data_per_season <-  data_per_day %>%
    select(n_Gewalttaten,n_Morde,n_Verletzte,season) %>%
    group_by("Jahreszeit" = season) %>%
    summarise("n_Gewalttaten" = sum(n_Gewalttaten),
            "n_Morde" = sum(n_Morde),
            "n_Verletzte" = sum(n_Verletzte)
    )
data_per_season

data_per_day %>% ggplot(aes(monthInt,n_Gewalttaten, color = season)) + stat_boxplot(geom ='errorbar') + geom_boxplot()
data_per_day %>% ggplot(aes(season,n_Gewalttaten, color = season)) + stat_boxplot(geom ='errorbar') + geom_boxplot()

cor(data_per_season %>% select(n_Gewalttaten,n_Morde,n_Verletzte))

data_per_state <- data %>% 
  select(id,n_killed,n_injured,state) %>%
  group_by(state) %>%
  summarise("n_Gewalttaten" = length(id),
            "n_Morde" = sum(n_killed),
            "avg_Morde" = mean(n_killed),
            "n_Verletzte" = sum(n_injured),
            "avg_Verletzte" = mean(n_injured)
)
data_per_state_pop <- murders %>% select(state,abb,region,population) %>% merge(data_per_state, by="state")
data_per_state_pop <- data_per_state_pop %>% mutate("Gewalttaten_pro_Million" = n_Gewalttaten/population * 10^6)
data_per_state_pop <- data_per_state_pop %>% mutate("Morde_pro_Million" = n_Morde/population * 10^6)
data_per_state_pop <- data_per_state_pop %>% mutate("Verletzte_pro_Million" = n_Verletzte/population * 10^6)
head(data_per_state_pop)


summary(data_per_state_pop)

cor(data_per_state_pop %>% select(Gewalttaten_pro_Million,Verletzte_pro_Million,Morde_pro_Million,population))
corrgram(data_per_state_pop %>% select(Gewalttaten_pro_Million,Verletzte_pro_Million,Morde_pro_Million,population))

data_per_state_pop %>% 
    ggplot(aes(y=Gewalttaten_pro_Million,x=population/10^6,label = abb,color=region)) + 
    geom_label() +
    geom_smooth(method='lm',formula=y~x, color="red", alpha=0.5, size=0.1)+
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    xlab("Bevölkerung pro Million (Skala log10)") +
    ylab("Anzahl Gewalttaten pro Million (Skala log10)") +
    ggtitle("Anzahl Gewalttaten pro Million zu Einwohner pro Million pro Staat") +
    scale_color_discrete(name="Region")
data_per_state_pop %>% 
    ggplot(aes(y=Verletzte_pro_Million,x=population/10^6,label = abb,color=region)) + 
    geom_label() +
    geom_smooth(method='lm',formula=y~x, color="red", alpha=0.5, size=0.1)+
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    xlab("Bevölkerung pro Million (Skala log10)") +
    ylab("Anzahl Verletzte pro Million (Skala log10)") +
    ggtitle("Anzahl Verletzte pro Million zu Einwohner pro Million pro Staat") +
    scale_color_discrete(name="Region")
data_per_state_pop %>% 
    ggplot(aes(y=Morde_pro_Million,x=population/10^6,label = abb,color=region)) + 
    geom_label() +
    geom_smooth(method='lm',formula=y~x, color="red", alpha=0.5, size=0.1)+
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    xlab("Bevölkerung pro Million (Skala log10)") +
    ylab("Anzahl Morde pro Million (Skala log10)") +
    ggtitle("Anzahl Morde pro Million zu Einwohner pro Million pro Staat") +
    scale_color_discrete(name="Region")

data_per_state_pop %>% 
    ggplot(aes(x=Gewalttaten_pro_Million,y=Morde_pro_Million,label = abb,color=region)) + 
    geom_label() +
    geom_smooth(method='lm',formula=y~x, color="red", alpha=0.5, size=0.1)+
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    xlab("Anzahl Gewalttaten pro Million (Skala log10)") +
    ylab("Anzahl Morde pro Million (Skala log10)") +
    ggtitle("Anzahl Gewalttaten pro Million zu Morde pro Million pro Staat") +
    scale_color_discrete(name="Region")
data_per_state_pop %>% 
    ggplot(aes(x=Gewalttaten_pro_Million,y=Verletzte_pro_Million,label = abb,color=region)) + 
    geom_label() +
    geom_smooth(method='lm',formula=y~x, color="red", alpha=0.5, size=0.1)+
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    xlab("Anzahl Gewalttaten pro Million (Skala log10)") +
    ylab("Anzahl Verletzte pro Million (Skala log10)") +
    ggtitle("Anzahl Gewalttaten pro Million zu Verletzte pro Million pro Staat") +
    scale_color_discrete(name="Region")
data_per_state_pop %>% 
    ggplot(aes(x=Verletzte_pro_Million,y=Morde_pro_Million,label = abb,color=region)) + 
    geom_label() +
    geom_smooth(method='lm',formula=y~x, color="red", alpha=0.5, size=0.1)+
    scale_x_continuous(trans = "log10") +
    scale_y_continuous(trans = "log10") +
    xlab("Anzahl Verletzte pro Million (Skala log10)") +
    ylab("Anzahl Morde pro Million (Skala log10)") +
    ggtitle("Anzahl Verletzte pro Million zu Morde pro Million pro Staat") +
    scale_color_discrete(name="Region")

aov_model_gewalttaten <- aov(data_per_state_pop$Gewalttaten_pro_Million ~ data_per_state_pop$region)
summary(aov_model_gewalttaten)
TukeyHSD(aov_model_gewalttaten)

aov_model_Verletzte <- aov(data_per_state_pop$Verletzte_pro_Million ~ data_per_state_pop$region)
summary(aov_model_Verletzte)
TukeyHSD(aov_model_Verletzte)

aov_model_Morde <- aov(data_per_state_pop$Morde_pro_Million ~ data_per_state_pop$region)
summary(aov_model_Morde)
TukeyHSD(aov_model_Morde)

data_per_state_pop %>% ggplot(aes(region,Gewalttaten_pro_Million, color = region)) + 
stat_boxplot(geom ='errorbar') + geom_boxplot() + scale_y_continuous(trans = "log10")
data_per_state_pop %>% ggplot(aes(region,Verletzte_pro_Million, color = region)) + 
stat_boxplot(geom ='errorbar') + geom_boxplot() + scale_y_continuous(trans = "log10")
data_per_state_pop %>% ggplot(aes(region,Morde_pro_Million, color = region)) + 
stat_boxplot(geom ='errorbar') + geom_boxplot() + scale_y_continuous(trans = "log10")

shapiro.test(data_per_day$n_Gewalttaten)
shapiro.test(data_per_day$n_Morde)
shapiro.test(data_per_day$n_Verletzte)
shapiro.test(data_per_day$avg_Morde)
shapiro.test(data_per_day$avg_Verletzte)

wilcox.test(data_per_day$n_Gewalttaten,data_per_day$n_Verletzte)
wilcox.test(data_per_day$n_Gewalttaten,data_per_day$n_Morde)
wilcox.test(data_per_day$n_Verletzte,data_per_day$n_Morde)

ad.test(data_per_day$n_Gewalttaten)
ad.test(data_per_day$n_Morde)
ad.test(data_per_day$n_Verletzte)
ad.test(data_per_day$avg_Morde)
ad.test(data_per_day$avg_Verletzte)

summary(ecdf(data_per_day$n_Gewalttaten))
summary(ecdf(data_per_day$n_Morde))
summary(ecdf(data_per_day$n_Verletzte))
summary(ecdf(data_per_day$avg_Morde))
summary(ecdf(data_per_day$avg_Verletzte))

ggplot(data_per_day, aes(n_Gewalttaten)) + stat_ecdf(geom="point",color="magenta",alpha=0.5)
ggplot(data_per_day, aes(n_Verletzte)) + stat_ecdf(geom="point",color="magenta",alpha=0.5)
ggplot(data_per_day, aes(avg_Verletzte)) + stat_ecdf(geom="point",color="magenta",alpha=0.5)
ggplot(data_per_day, aes(n_Morde)) + stat_ecdf(geom="point",color="magenta",alpha=0.5)
ggplot(data_per_day, aes(avg_Morde)) + stat_ecdf(geom="point",color="magenta",alpha=0.5)
