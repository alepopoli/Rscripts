# Grafico riportante la media della mortalità giornaliera 2015-2019 
# e i morti giornalieri del 2020

# Alessandro Popoli

# link dataset istat:
# https://www.istat.it/it/files//2020/03/Dataset-decessi-comunali-giornalieri-e-tracciato-record_22ottobre2020.zip


# load libraries

library(tidyverse)


# Import data

datiISTAT <- read.csv("comuni_giornaliero_dati_fino_31agosto.csv", na="n.d.")


# Examine the data:

glimpse(datiISTAT)


# summarize the data: make date a date-readable information, 
# get rid of unnecessary information, sum total deaths per day, and make data 
# ready for plotting: average deaths per day for the period 2015-2019
# and calculate SD, split in two data.frames 
# (one for 2015-2019 and one for 2020), merge the data.frames


datiISTAT_2015_2019 <- datiISTAT %>% 
  mutate(DAY =  as.numeric(str_extract(GE, "\\d\\d$")) ,
         MONTH = as.numeric(str_remove(GE, "\\d\\d$"))) %>%
  mutate(data=as.Date((paste(MONTH, DAY,sep="-")), "%m-%d")) %>% 
  select(data, T_15, T_16, T_17, T_18, T_19) %>% 
  group_by(data) %>% 
  summarise("2015"=sum(T_15),
            "2016"=sum(T_16),
            "2017"=sum(T_17),
            "2018"=sum(T_18),
            "2019"=sum(T_19)) %>%
  group_by(data) %>% 
  pivot_longer(c("2015", "2016", "2017", "2018", "2019"),
               names_to= "anno",
               values_to="morti") %>% 
  summarise("morti per giorno (Italia)"=mean(morti),
            "SD morti" = sd(morti)) %>%
  mutate(Anni="media 2015-2019") %>% 
  filter(data != "2020-02-29")

datiISTAT_2020 <- datiISTAT %>% 
  mutate(DAY =  as.numeric(str_extract(GE, "\\d\\d$")) ,
         MONTH = as.numeric(str_remove(GE, "\\d\\d$"))) %>%
  mutate(data=as.Date((paste(MONTH, DAY,sep="-")), "%m-%d")) %>% 
  select(data, T_20) %>% 
  group_by(data) %>% 
  summarise("2020"=sum(T_20)) %>%
  group_by(data) %>% 
  pivot_longer("2020",
               names_to= "anno",
               values_to="morti") %>% 
  summarise("morti per giorno (Italia)"=mean(morti),
            "SD morti" = sd(morti)) %>%
  mutate(Anni="2020") %>%
  filter(data != "2020-02-29")

datiISTAT_forplot <- rbind(datiISTAT_2020, datiISTAT_2015_2019)

glimpse(datiISTAT_forplot)

# plot trend-lines with SD

datiISTAT_forplot$Anni <- factor(datiISTAT_forplot$Anni, 
                                 levels = c("media 2015-2019", "2020"))

ggplot(datiISTAT_forplot, 
       aes(x=data, y=`morti per giorno (Italia)`, fill=Anni, color=Anni))+
  geom_ribbon(
    aes(
      ymin=`morti per giorno (Italia)`-`SD morti`, 
      ymax=`morti per giorno (Italia)`+`SD morti`), 
    alpha=0.4)+
  scale_fill_manual(values = c("blue", "red"))+
  geom_line(size=2)+
  scale_x_date(date_labels = "%b", date_breaks = "1 months")+
  scale_color_manual(values = c("blue", "red"))+
  labs(x = "")


# calculate total excess deaths for March and April

datiISTAT %>% 
  mutate(DAY =  as.numeric(str_extract(GE, "\\d\\d$")) ,
         MONTH = as.numeric(str_remove(GE, "\\d\\d$"))) %>%
  mutate(data=as.Date((paste(MONTH, DAY,sep="-")), "%m-%d")) %>% 
  select(data, T_15, T_16, T_17, T_18, T_19, T_20) %>% 
  group_by(data) %>% 
  summarise("2015"=sum(T_15),
            "2016"=sum(T_16),
            "2017"=sum(T_17),
            "2018"=sum(T_18),
            "2019"=sum(T_19),
            anno_2020 =sum(T_20)) %>%
  group_by(data, anno_2020) %>% 
  pivot_longer(c("2015", "2016", "2017", "2018", "2019"),
               names_to= "anno",
               values_to="morti") %>% 
  summarise("media morti 2015-2019"=mean(morti),
            "SD morti 2015-2019" = sd(morti)) %>%
  filter(data != "2020-02-29") %>% 
  subset( data < as.Date("2020-05-01")) %>% 
  subset( data > as.Date("2020-02-28")) %>%
  mutate(death_excess= 
           anno_2020 - `media morti 2015-2019` - `SD morti 2015-2019`,
         group="1") %>% 
  group_by(group) %>% 
  summarise("morti in eccesso marzo e aprile"=sum(death_excess))