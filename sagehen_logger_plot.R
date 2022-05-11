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
###############
#Bring in logger data

library(readxl)

#Solinst
#THALWEG
sol_0<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/2141128_CA-0_2022_05_10_111248.csv",
                skip=11) %>%
  mutate(Brand="Solinst",
         sn="2141128",
         position=0)
#BASE OF BANK
sol_1<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/2141124_CA-1_2022_05_10_110141.csv",
                skip=11) %>%
  mutate(Brand="Solinst",
         sn="2141124",
         position=1)
#MIDBANK
sol_2<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/2141121_CA-2_2022_05_10_110904.csv",
                skip=11) %>%
  mutate(Brand="Solinst",
         sn="2141121",
         position=2)
#TOP OF BANK
sol_3<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/2141119_CA-3_2022_05_10_111555.csv",
                skip=11) %>%
  mutate(Brand="Solinst",
         sn="2141119",
         position=3)
#TREE/BAROLOGGER
sol_4<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/2141041_CA-4_2022_05_10_111902.csv",
                skip=10)  %>%
  mutate(Brand="Solinst",
         sn="2141041",
         position=4)

sol_df<-bind_rows(sol_0, sol_1, sol_2, sol_3, sol_4) %>%
  transmute(Brand=Brand, 
            position=position, 
            sn=sn,
            Date=mdy(Date),
            Time=Time,
            DateTime=make_datetime(year(Date), month(Date), day(Date), hour(Time), minute(Time)),
            Analyte="Pressure",
            Result=LEVEL,
            Unit="KPa",
            TempC=TEMPERATURE,
            ms=ms
  ) %>%
  #trim pre-deployment
  filter(DateTime>"2021-11-03 16:00" %>% ymd_hm()) %>%
  #trim post-retrieval
  filter(DateTime<"2022-05-06 9:40" %>% ymd_hm()) 

ggplot(data=sol_df, aes(x=DateTime, y=Result, group=position))+
  geom_path(aes(color=as.factor(position)))+
  scale_color_brewer(palette="Set1")


######
#HOBO
#THALWEG
hobo_0<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/21110501_2022_05_10.csv",
                 skip=1) %>%
  transmute(Brand="HOBO U20",
            sn="21110501",
            position=0,
            Pressure_KPA = `Abs Pres, kPa (LGR S/N: 21110501, SEN S/N: 21110501)`,
            TempC = `Temp, °C (LGR S/N: 21110501, SEN S/N: 21110501)`,
            DateTime=`Date Time, GMT-07:00`)

#BASE OF BANK
hobo_1<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/21110502_2022_05_10.csv",
                 skip=1) %>%
  transmute(Brand="HOBO U20",
            sn="21110502",
            position=1,
            Pressure_KPA = `Abs Pres, kPa (LGR S/N: 21110502, SEN S/N: 21110502)`,
            TempC = `Temp, °C (LGR S/N: 21110502, SEN S/N: 21110502)`,
            DateTime=`Date Time, GMT-07:00`)
#MIDBANK
hobo_2<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/21110500_2022_05_10.csv",
                 skip=1) %>%
  transmute(Brand="HOBO U20",
            sn="21110500",
            position=2,
            Pressure_KPA = `Abs Pres, kPa (LGR S/N: 21110500, SEN S/N: 21110500)`,
            TempC = `Temp, °C (LGR S/N: 21110500, SEN S/N: 21110500)`,
            DateTime=`Date Time, GMT-07:00`)
#TOP OF BANK
hobo_3<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/21110503_2022_05_10.csv",
                 skip=1) %>%
  transmute(Brand="HOBO U20",
            sn="21110503",
            position=3,
            Pressure_KPA = `Abs Pres, kPa (LGR S/N: 21110503, SEN S/N: 21110503)`,
            TempC = `Temp, °C (LGR S/N: 21110503, SEN S/N: 21110503)`,
            DateTime=`Date Time, GMT-07:00`)

hobo_df<-bind_rows(hobo_0, hobo_1, hobo_2, hobo_3) %>%
  transmute(Brand=Brand, 
            position=position, 
            sn=sn,
            DateTime=mdy_hms(DateTime) %>% ymd_hms(),
            Date=date(DateTime),
            # Time=lubridate::hms(DateTime),
            Analyte="Pressure",
            Result=Pressure_KPA,
            Unit="KPa",
            TempC=TempC) %>%
  #trim pre-deployment
  filter(DateTime>"2021-11-03 16:00" %>% ymd_hm()) %>%
  #trim post-retrieval
  filter(DateTime<"2022-05-06 9:40" %>% ymd_hm()) 


ggplot(data=hobo_df, aes(x=DateTime, y=Result, group=position))+
  geom_path(aes(color=as.factor(position)))+
  scale_color_brewer(palette="Set1")



######
#Alphamac
#THALWEG NO DATA
# alpha_0<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/21110501_2022_05_10.csv",
#                  skip=1) %>%
#   transmute(Brand="Alphamac",
#             sn="21110501",
#             position=0,
#             Pressure_KPA = `Abs Pres, kPa (LGR S/N: 21110501, SEN S/N: 21110501)`,
#             TempC = `Temp, °C (LGR S/N: 21110501, SEN S/N: 21110501)`,
#             DateTime=`Date Time, GMT-07:00`)

#BASE OF BANK
alpha_1<-  read_table("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/Nautical-Tidal-80021F081FFFD3A2_2022_05_10.txt",
                      skip=58) %>%
  transmute(Brand="Alphamac",
            sn="80021F081FFFD3A2",
            position=1,
            Pressure_KPA = Ambi.,
            TempC = Infr.,
            Date=Time,
            Time=paste(Red, `.`),
            DateTime=paste(Date, Time) %>% mdy_hms()
  ) %>%
  mutate(Date=mdy(Date))
#MIDBANK
alpha_2<- read_table("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/Nautical-Tidal-80021F081FFFD3AE_2022_05_10.txt",
                     skip=58) %>%
  transmute(Brand="Alphamac",
            sn="80021F081FFFD3AE",
            position=2,
            Pressure_KPA = Ambi.,
            TempC = Infr.,
            Date=Time,
            Time=paste(Red, `.`),
            DateTime=paste(Date, Time) %>% mdy_hms()
  ) %>%
  mutate(Date=mdy(Date))
#TOP OF BANK
alpha_3<-read_table("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/Nautical-Tidal-80021F081FFFD3CE_2022_05_10.txt",
                    skip=58) %>%
  transmute(Brand="Alphamac",
            sn="80021F081FFFD3CE",
            position=3,
            Pressure_KPA = Ambi.,
            TempC = Infr.,
            Date=Time,
            Time=paste(Red, `.`),
            DateTime=paste(Date, Time) %>% mdy_hms()
  ) %>%
  mutate(Date=mdy(Date))


alpha_df<-bind_rows(alpha_1, alpha_2, alpha_3) %>%
  transmute(Brand=Brand, 
            position=position, 
            sn=sn,
            DateTime=DateTime%>% ymd_hms(),
            Date=Date,
            # Time=lubridate::hms(DateTime),
            Analyte="Pressure",
            Result=Pressure_KPA,
            Unit="KPa",
            TempC=TempC) %>%
  #trim pre-deployment
  filter(DateTime>"2021-11-03 16:00" %>% ymd_hm()) %>%
  #trim post-retrieval
  filter(DateTime<"2022-05-06 9:40" %>% ymd_hm()) 

alpha_plot<-ggplot(data=alpha_df, aes(x=DateTime, y=Result, group=position))+
  geom_path(aes(color=as.factor(position)))+
  scale_color_brewer(palette="Set1")+
  facet_wrap(~position, scales="free_y", ncol=1)

ggsave(alpha_plot, filename="figures/alpha_plot.jpg", height=6, width=6)

####
######
#STIC
#THALWEG
stic_0<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/20906286_2022_05_10.csv",
                 skip=1) %>%
  transmute(Brand="STIC",
            sn="20906286",
            position=0,
            Intensity = `Intensity, Lux (LGR S/N: 20906286, SEN S/N: 20906286)`,
            TempC = `Temp, °C (LGR S/N: 20906286, SEN S/N: 20906286)`,
            DateTime=`Date Time, GMT-07:00`)

#BASE OF BANK
stic_1<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/20906281_2022_05_10.csv",
                 skip=1) %>%
  transmute(Brand="STIC",
            sn="20906281",
            position=1,
            Intensity = `Intensity, Lux (LGR S/N: 20906281, SEN S/N: 20906281)`,
            TempC = `Temp, °C (LGR S/N: 20906281, SEN S/N: 20906281)`,
            DateTime=`Date Time, GMT-07:00`)
#MIDBANK
stic_2<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/20906289_2022_05_10.csv",
                 skip=1) %>%
  transmute(Brand="STIC",
            sn="20906289",
            position=2,
            Intensity = `Intensity, Lux (LGR S/N: 20906289, SEN S/N: 20906289)`,
            TempC = `Temp, °C (LGR S/N: 20906289, SEN S/N: 20906289)`,
            DateTime=`Date Time, GMT-07:00`)
#TOP OF BANK
stic_3<-read_csv("C:/Users/Raphaelm/SCCWRP/Stream Flow Duration - Documents/Data loggers/Alaska field logger test/California Logger Data - Sagehen Creek/20906282_2022_05_10.csv",
                 skip=1) %>%
  transmute(Brand="STIC",
            sn="20906282",
            position=3,
            Intensity = `Intensity, Lux (LGR S/N: 20906282, SEN S/N: 20906282)`,
            TempC = `Temp, °C (LGR S/N: 20906282, SEN S/N: 20906282)`,
            DateTime=`Date Time, GMT-07:00`)

stic_df<-bind_rows(stic_0, stic_1, stic_2, stic_3) %>%
  transmute(Brand=Brand, 
            position=position, 
            sn=sn,
            DateTime=mdy_hms(DateTime) %>% ymd_hms(),
            Date=date(DateTime),
            # Time=lubridate::hms(DateTime),
            Analyte="Intensity",
            Result=Intensity,
            Unit="Lux",
            TempC=TempC) %>%
  #trim pre-deployment
  filter(DateTime>"2021-11-03 16:00" %>% ymd_hm()) %>%
  #trim post-retrieval
  filter(DateTime<"2022-05-06 9:40" %>% ymd_hm()) 


ggplot(data=stic_df, aes(x=DateTime, y=Result, group=position))+
  geom_path(aes(color=as.factor(position)))+
  scale_color_brewer(palette="Set1")

######
#TEMPERATURE PLOT

temp_df<-bind_rows(sol_df, alpha_df, hobo_df, stic_df) %>%
  select(-ms) %>%
  mutate(position.f=factor(position, levels=c(0,1,2,3,4),
                           labels=c("Thalweg","Base of bank","Mid-bank","Top of bank","Tree")))

tempC_plot<-
  ggplot(data=temp_df %>%
           filter(sn!="80021F081FFFD3AE"), aes(x=DateTime, y=TempC))+
  geom_path(aes(color=Brand))+
  facet_wrap(~position.f, ncol=1)+
  scale_color_brewer(palette="Set1")

ggsave(tempC_plot, filename="figures/tempC_plot.jpg", height=6, width=6)


pressure_plot<-
  ggplot(data=temp_df %>%
           filter(sn!="80021F081FFFD3AE") %>%
           filter(Brand!="STIC"),
         aes(x=DateTime, y=Result))+
  geom_path(aes(color=Brand))+
  facet_wrap(~position.f, ncol=1)+
  scale_color_brewer(palette="Set1")

ggsave(pressure_plot, filename="figures/pressure_plot.jpg", height=6, width=6)
