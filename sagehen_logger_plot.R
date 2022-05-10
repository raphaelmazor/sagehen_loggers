library(tidyverse)
library(dataRetrieval)
library(lubridate)

mygaugeid<-"10343500"
gauge_info<-readNWISsite(mygaugeid)
gauge_name<- gauge_infostation_nm
parameterCd <- "00060" #This is the parameter code for discharge
my_gage_data<-readNWISdv(mygaugeid,parameterCd, "1960-01-01","2022-05-10") %>%
  transmute(Date=ymd(Date),
            Variable="Discharge",
            Result=X_00060_00003,
            Year=year(Date),
            WaterYear=ifelse(month(Date)>10, Year+1, Year),
            Wet=Result>0)

deployment_df<-tibble(StartDate=c("11/3/2021") %>% mdy() %>% ymd(),
                      EndDate=c("5/6/2022") %>% mdy() %>% ymd(),
                      Date=c("5/6/2022") %>% mdy() %>% ymd(),
                      Resut=0)

sagehen_plot<-ggplot(data=my_gage_data, aes(x=Date, y=Result))+
  geom_path()+
  geom_area(aes(ymin=0, ymax=Result), fill="lightblue", alpha=.5)+
  geom_point(data=.%>% filter(!Wet), color="red", size=.5) +#Stream is truly perennial
  geom_vline(data=deployment_df %>% pivot_longer(cols=c(StartDate, EndDate)),
             aes(xintercept=value), linetype="dashed", color="red")


sagehen_plot + 
  coord_cartesian(xlim=c("6/1/2021", "5/10/2022") %>% mdy() %>% ymd())
