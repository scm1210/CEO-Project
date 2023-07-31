#############################
###CEO SUPPLEMENTAL graphs###
#############################
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("rlang")) install.packages("rlang")
if (!require("zoo")) install.packages("zoo")
if (!require("readr")) install.packages("readr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotrix")) install.packages("plotrix")

library(tidyverse)
library(rlang)
library(ggpubr) 
library(readr)
library(zoo)
library(plotrix)

setwd("~/Desktop/CEO Project/Manuscript analyses") 

df <- read_csv("Big_CEO_copy.csv")#put code here to read in Big CEO data
df <- df %>% filter(WC<=5400)   %>% 
  filter(WC>=25)

df$month_year <- format(as.Date(df$Date), "%Y-%m") ###extracting month and year to build fiscal quarter graphs, need a new variable bc if not it'll give us issues

df2 <- df %>%#converting our dates to quarterly dates 
  group_by(month_year) %>% ###grouping by the Top100 tag and date 
  summarise_at(vars("Date","WC", "Clout","Tone","WPS","BigWords",'allnone', "emo_pos","emo_neg", #need Date here so we can do fancy ggplot things
  "emo_anx", "emo_anger","emo_sad",'focuspast','focuspresent','focusfuture'),  funs(mean, std.error),) #pulling the means and SEs for our variables of interest
View(df2)

df2 <- df2["2019-01"<= df2$month_year & df2$month_year <= "2021-03",] #covid dates 

######################
#####Affect Graphs####
######################

Emo_Tone <- ggplot(data=df2, aes(x=Date_mean, y=Tone_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=Tone_mean-Tone_std.error, ymax=Tone_mean+Tone_std.error), alpha=0.2) +
  ggtitle("Emotional Tone") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = 'Standardized score') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
Emo_Tone <- Emo_Tone + annotate(geom="text",x=as.Date("2020-07-01"),
                    y=65,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=65,label="Winter 2020 surge", size = 3.5)

emo_pos <- ggplot(data=df2, aes(x=Date_mean, y=emo_pos_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=emo_pos_mean-emo_pos_std.error, ymax=emo_pos_mean+emo_pos_std.error), alpha=0.2) +
  ggtitle("Positive Emotion") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) + 
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
emo_pos <- emo_pos+ annotate(geom="text",x=as.Date("2020-07-01"),
                             y=0.70,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=0.70,label="Winter 2020 surge", size = 3.5)

emo_neg <- ggplot(data=df2, aes(x=Date_mean, y=emo_neg_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=emo_neg_mean-emo_neg_std.error, ymax=emo_neg_mean+emo_neg_std.error), alpha=0.2) +
  ggtitle("Negative Emotion") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
  
emo_neg <- emo_neg+annotate(geom="text",x=as.Date("2020-07-01"),
                      y=0.10,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=0.10,label="Winter 2020 surge", size = 3.5)

emo_anx <- ggplot(data=df2, aes(x=Date_mean, y=emo_anx_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=emo_anx_mean-emo_anx_std.error, ymax=emo_anx_mean+emo_anx_std.error), alpha=0.2) +
  ggtitle("Anxiety Words") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
emo_anx <- emo_anx + annotate(geom="text",x=as.Date("2020-07-01"),
                            y=0.06,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=0.06,label="Winter 2020 surge", size = 3.5)

emo_anger <- ggplot(data=df2, aes(x=Date_mean, y=emo_anger_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=emo_anger_mean-emo_anger_std.error, ymax=emo_anger_mean+emo_anger_std.error), alpha=0.2) +
  ggtitle("Anger Words") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
emo_anger <- emo_anger + annotate(geom="text",x=as.Date("2020-07-01"),
                      y=0.030,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=0.030,label="Winter 2020 surge", size = 3.5)

emo_sad <- ggplot(data=df2, aes(x=Date_mean, y=emo_sad_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=emo_sad_mean-emo_sad_std.error, ymax=emo_sad_mean+emo_sad_std.error), alpha=0.2) +
  ggtitle("Sad words") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
emo_sad <- emo_sad + annotate(geom="text",x=as.Date("2020-07-01"),
                    y=0.012,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=0.012,label="Winter 2020 surge", size = 3.5)

#Tie the emotion graphs all together
emo_graphs <- ggpubr::ggarrange(Emo_Tone,emo_pos,emo_neg,emo_anx,emo_anger,emo_sad,ncol=2, nrow=3, common.legend = TRUE, legend = "bottom")
annotate_figure(emo_graphs,
                top = text_grob("Emotion Language Change", color = "black", face = "bold", size = 20),
                bottom = text_grob("Note. Vertical Line Represents the onset of the pandemic. Shading represents Standard Error and virus surges"
                                   , color = "Black",
                                   hjust = 1.375, x = 1, face = "italic", size = 16))
emo_graphs
#######################
#####Temporal Focus####
#######################

focuspast <- ggplot(data=df2, aes(x=Date_mean, y=focuspast_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=focuspast_mean-focuspast_std.error, ymax=focuspast_mean+focuspast_std.error), alpha=0.2) +
  ggtitle("Focus Past") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
focuspast <- focuspast + annotate(geom="text",x=as.Date("2020-07-01"),
                      y= 3.3 ,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=3.3,label="Winter 2020 surge", size = 3.5)

focuspresent <- ggplot(data=df2, aes(x=Date_mean, y=focuspresent_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=focuspresent_mean-focuspresent_std.error, ymax=focuspresent_mean+focuspresent_std.error), alpha=0.2) +
  ggtitle("Focus Present") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
focuspresent <- focuspresent + annotate(geom="text",x=as.Date("2020-07-01"),
                                     y= 6.3 ,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=6.3,label="Winter 2020 surge", size = 3.5)

focusfuture <- ggplot(data=df2, aes(x=Date_mean, y=focusfuture_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=focusfuture_mean-focusfuture_std.error, ymax=focusfuture_mean+focusfuture_std.error), alpha=0.2) +
  ggtitle("Focus Future") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
focusfuture <- focusfuture + annotate(geom="text",x=as.Date("2020-07-01"),
                                       y= 2.75 ,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=2.75,label="Winter 2020 surge", size = 3.5)

#Tie focus graphs together
focus_graphs <- ggpubr::ggarrange(focuspast,focuspresent,focusfuture,ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
annotate_figure(focus_graphs,
                top = text_grob("Temporal Focus Language Change",color = "black", face = "bold", size = 20),
                bottom = text_grob("Note. Vertical Line Represents the onset of the pandemic. Shading represents Standard Error"
                                   , color = "Black",
                                   hjust = 2, x = 1, face = "italic", size = 16))
################################
####Summary variable graphs ####
################################

Clout <- ggplot(data=df2, aes(x=Date_mean, y=Clout_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=Clout_mean-Clout_std.error, ymax=Clout_mean+Clout_std.error), alpha=0.2) +
  ggtitle("Clout") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = 'Standardized score') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
Clout <- Clout + annotate(geom="text",x=as.Date("2020-07-01"),
                                y= 87 ,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=87,label="Winter 2020 surge", size = 3.5)

WPS <- ggplot(data=df2, aes(x=Date_mean, y=WPS_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=WPS_mean-WPS_std.error, ymax=WPS_mean+WPS_std.error), alpha=0.2) +
  ggtitle("Words per sentence") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = 'Mean') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
WPS <- WPS + annotate(geom="text",x=as.Date("2020-07-01"),
                        y= 20.5 ,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=20.5,label="Winter 2020 surge", size = 3.5)

Big_words <- ggplot(data=df2, aes(x=Date_mean, y=BigWords_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=BigWords_mean-BigWords_std.error, ymax=BigWords_mean+BigWords_std.error), alpha=0.2) +
  ggtitle("Six-letter words") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
Big_words <- Big_words + annotate(geom="text",x=as.Date("2020-07-01"),
                            y= 19.5 ,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=19.5,label="Winter 2020 surge", size = 3.5)

allnone <- ggplot(data=df2, aes(x=Date_mean, y=allnone_mean, group=1)) +
  geom_line(colour = "dodgerblue3") +
  geom_ribbon(aes(ymin=allnone_mean-allnone_std.error, ymax=allnone_mean+allnone_std.error), alpha=0.2) +
  ggtitle("All or none") + 
  scale_x_date(date_breaks = "3 month", date_labels = "%Y-%m") +
  labs(x = "Month", y = '% of total Words') +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-01")), linetype=4) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.text=element_text(size = 14),
        axis.title=element_text(size = 20,face="bold"))+ 
  geom_rect(data = df2, #summer surge
            aes(xmin = as.Date("2020-06-15", "%Y-%m-%d"), 
                xmax = as.Date("2020-07-20",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009) +
  geom_rect(data = df2, #winter surge
            aes(xmin = as.Date("2020-11-15", "%Y-%m-%d"), 
                xmax = as.Date("2021-01-01",  "%Y-%m-%d"),
                ymin = -Inf, 
                ymax = Inf),
            fill = "gray", 
            alpha = 0.009)
allnone <- allnone + annotate(geom="text",x=as.Date("2020-07-01"),
                                y= 1.10 ,label="Summer 2020 surge", size = 3.5) + 
  annotate(geom="text",x=as.Date("2020-12-03"),
           y=1.10,label="Winter 2020 surge", size = 3.5)

sum_graphs <- ggpubr::ggarrange(Clout,WPS,Big_words,allnone,ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
annotate_figure(sum_graphs,
                top = text_grob("LIWC Summary Variables", color = "black", face = "bold", size = 20),
                bottom = text_grob("Note. Vertical Line Represents the onset of the pandemic. Shading represents Standard Error"
                                   , color = "Black",
                                   hjust = 2.15, x = 1, face = "italic", size = 16))

