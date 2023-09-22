
rm(list = ls())

library(tidyverse)
library(openxlsx)
library(countrycode)

load('Data/cc_gwps.RData')


########### load up latest EDGAR version

edgar_co2 <- openxlsx::read.xlsx('Data/Not public/IEA-EDGAR_CO2_1970_2022.xlsx',sheet='IPCC 2006',startRow=10)
edgar_co2 <- gather(edgar_co2,year,value,Y_1970:Y_2022)
edgar_co2$year <- gsub("Y_","",edgar_co2$year)
#edgar_co2$Substance <- "CO2"

edgar_ch4 <- openxlsx::read.xlsx('Data/Not public/EDGAR_v8.0_CH4_1970_2022.xlsx',sheet='IPCC 2006',startRow=10)
edgar_ch4 <- gather(edgar_ch4,year,value,Y_1970:Y_2022)
edgar_ch4$year <- gsub("Y_","",edgar_ch4$year)
#edgar_ch4$Substance <- "CH4"

edgar_n2o <- openxlsx::read.xlsx('Data/Not public/EDGAR_v8.0_N2O_1970_2022.xlsx',sheet='IPCC 2006',startRow=10)
edgar_n2o <- gather(edgar_n2o,year,value,Y_1970:Y_2022)
edgar_n2o$year <- gsub("Y_","",edgar_n2o$year)
#edgar_n2o$Substance <- "N2O"

edgar_fgas <- openxlsx::read.xlsx('Data/Not public/EDGAR_v8.0_F-gases_1970_2022.xlsx',sheet='IPCC 2006',startRow=10)
edgar_fgas <- gather(edgar_fgas,year,value,Y_1970:Y_2022)
edgar_fgas$year <- gsub("Y_","",edgar_fgas$year)


########### merge into single dataframe and tidy

edgar_ghg <- rbind(edgar_co2,edgar_ch4)
edgar_ghg <- rbind(edgar_ghg,edgar_n2o)
edgar_ghg <- rbind(edgar_ghg,edgar_fgas)

edgar_ghg <- edgar_ghg %>% 
  select(country=Name,
         iso=Country_code_A3,
         code=ipcc_code_2006_for_standard_report,
         description=ipcc_code_2006_for_standard_report_name,
         fossil_bio,
         gas=Substance,
         year,value)


########### Remove CO2 short cycle biogenic
## as of this version no longer relevant

edgar_ghg <- edgar_ghg %>% 
  mutate(value=ifelse(fossil_bio=="bio" & gas=="CO2",NA,value)) %>%
  filter(!is.na(value))


########### checks and fixes !!!!!!

#### do any sector combinations have more than one entry?
edgar_ghg <- edgar_ghg %>% 
  group_by(iso,year,code,fossil_bio,gas) %>% 
  mutate(n=n())

check <- edgar_ghg %>% filter(n>1) ### Good - no 

#### are all sector codes and descriptions unique?
check <- unique(edgar_ghg[,c("code","fossil_bio","description")])
check <- check %>% 
  group_by(code,fossil_bio) %>% 
  mutate(count=sum(n()))


########### join ipcc sector mapping  ########### 
## sectors no longer match, seem to have a shortened file from JRC

# load('Data/cc_sectors.RData')
# 
# ipcc_sectors <- ipcc_sectors %>% 
#   add_row(code="20",IPCC.2006=NA,fossil_bio="fossil",IPCC_AR6_chapter_title="Industry",subsector_title="Chemicals")
# 
# edgar_ghg <- left_join(edgar_ghg,ipcc_sectors %>% select(code,fossil_bio,chapter_title=IPCC_AR6_chapter_title,subsector_title),by=c("code","fossil_bio"))
# 
# missing_codes <- edgar_ghg %>% 
#   filter(is.na(subsector_title)) %>% 
#   select(code,fossil_bio,description) %>% 
#   distinct()  #no missing codes left


########### join region categorisation from WGIII TSU ###########
## dont need for the analysis

# load('Data/cc_regions.RData')
# 
# edgar_ghg <- left_join(edgar_ghg,cc_regions %>% select(-name),by=c("iso"="ISO"))
# 
# missing_region <- edgar_ghg %>% 
#   filter(is.na(region_ar6_6) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev) | is.na(region_ar6_6_short) | is.na(region_ar6_10_short)) %>%
#   select(iso,country) %>% 
#   unique()
# 
# edgar_ghg$region_ar6_6[edgar_ghg$iso=="AIR"] <- "Intl. Aviation"
# edgar_ghg$region_ar6_10[edgar_ghg$iso=="AIR"] <- "Intl. Aviation"
# edgar_ghg$region_ar6_22[edgar_ghg$iso=="AIR"] <- "Intl. Aviation"
# edgar_ghg$region_ar6_dev[edgar_ghg$iso=="AIR"] <- "Intl. Aviation"
# 
# edgar_ghg$region_ar6_6[edgar_ghg$iso=="SEA"] <- "Intl. Shipping"
# edgar_ghg$region_ar6_10[edgar_ghg$iso=="SEA"] <- "Intl. Shipping"
# edgar_ghg$region_ar6_22[edgar_ghg$iso=="SEA"] <- "Intl. Shipping"
# edgar_ghg$region_ar6_dev[edgar_ghg$iso=="SEA"] <- "Intl. Shipping"
# 
# edgar_ghg$region_ar6_6_short[edgar_ghg$iso=="SEA"] <- "SEA"
# edgar_ghg$region_ar6_6_short[edgar_ghg$iso=="AIR"] <- "AIR"
# 
# edgar_ghg$region_ar6_10_short[edgar_ghg$iso=="SEA"] <- "SEA"
# edgar_ghg$region_ar6_10_short[edgar_ghg$iso=="AIR"] <- "AIR"


############## tidying up

edgar_ghg <- edgar_ghg %>% 
  mutate(year=as.numeric(year)) %>% 
  select(-n)


##############convert from gigagram to t

edgar_ghg <- edgar_ghg %>% 
  mutate(value=value*1000)


############## join gwps and calculate GHG emissions, using ar6 values

load('Data/cc_gwps.RData')
cc_gwps <- cc_gwps %>% 
  filter(gas!="CH4")

ch4_sectors <- read.xlsx('Data/sector_mapping.xlsx',sheet=3)
edgar_ghg <- left_join(edgar_ghg,ch4_sectors %>% select(-value) %>% rename(ch4_sectors=gas),by = join_by(code, description, fossil_bio))

edgar_ghg <- edgar_ghg %>% 
  mutate(gas=ifelse(gas=="CH4",ch4_sectors,gas))

edgar_ghg <- left_join(edgar_ghg,cc_gwps %>% select(gas,gwp100_ar6,gwp100_ar4),by="gas") %>% 
  select(-ch4_sectors)


# any gases now missing ?
missing_gases_ar6 <- anti_join(cc_gwps %>% select(gas,gwp100_ar6,gwp100_ar4),edgar_ghg,by="gas")


# do we have all the non-CH4 gases?

missing_gwps_ar6 <- edgar_ghg %>% filter(is.na(gwp100_ar6)) %>% select(code,gas) %>% distinct()
missing_gwps_ar4 <- edgar_ghg %>% filter(is.na(gwp100_ar4)) %>% select(code,gas) %>% distinct()


## apply all gwps
edgar_ghg_ar6 <- edgar_ghg %>% mutate(value_gwp=value*gwp100_ar6)
edgar_ghg_ar4 <- edgar_ghg %>% mutate(value_gwp=value*gwp100_ar4)

## merge all Fgases into a single variable

edgar_ghg_ar6 <- edgar_ghg_ar6 %>% 
  select(-value,-gwp100_ar6,-gwp100_ar4) %>% 
  mutate(gas=ifelse(grepl("CH4",gas),"CH4",gas))

edgar_ghg_ar4 <- edgar_ghg_ar4 %>% 
  select(-value,-gwp100_ar6,-gwp100_ar4) %>% 
  mutate(gas=ifelse(grepl("CH4",gas),"CH4",gas))

edgar_ghg_ar6 <- spread(edgar_ghg_ar6,gas,value_gwp)
edgar_ghg_ar4 <- spread(edgar_ghg_ar4,gas,value_gwp)

fgas_list_ar6 <- names(edgar_ghg_ar6[-(1:6)] %>% select(-CO2,-CH4,-N2O))
fgas_list_ar4 <- names(edgar_ghg_ar4[-(1:6)] %>% select(-CO2,-CH4,-N2O))

edgar_ghg_ar6 <- edgar_ghg_ar6 %>% 
  ungroup() %>% 
  mutate(Fgas=rowSums(.[fgas_list_ar6],na.rm=T)) %>% 
  mutate(Fgas=ifelse(Fgas==0,NA,Fgas))

edgar_ghg_ar4 <- edgar_ghg_ar4 %>% 
  ungroup() %>% 
  mutate(Fgas=rowSums(.[fgas_list_ar4],na.rm=T)) %>% 
  mutate(Fgas=ifelse(Fgas==0,NA,Fgas))

## remove underlying fgases

edgar_ghg_ar6 <- edgar_ghg_ar6 %>% select(-one_of(fgas_list_ar6))
edgar_ghg_ar4 <- edgar_ghg_ar4 %>% select(-one_of(fgas_list_ar4))


## calculate total GHG emissions

edgar_ghg_ar6 <- edgar_ghg_ar6 %>% 
  group_by(iso,year,code,fossil_bio) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(GHG = ifelse(is.na(CO2) & is.na(CH4) & is.na(N2O) & is.na(Fgas),NA,GHG))

edgar_ghg_ar4 <- edgar_ghg_ar4 %>% 
  group_by(iso,year,code,fossil_bio) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(GHG = ifelse(is.na(CO2) & is.na(CH4) & is.na(N2O) & is.na(Fgas),NA,GHG))

## value to end & relevel gases

edgar_ghg <- edgar_ghg %>% 
  relocate(value,.after=gwp100_ar4) %>% 
  mutate(gas=ifelse(grepl("CH4",gas),"CH4",gas))

edgar_ghg$gas <- as.factor(edgar_ghg$gas) 
edgar_ghg$gas <- fct_relevel(edgar_ghg$gas,c("CO2","CH4","N2O"))


## save both files

edgar_raw <- edgar_ghg
save(edgar_raw,file='Data/Not public/edgar_ft_v1_data_raw.RData')

edgar_ghg <- edgar_ghg_ar6
save(edgar_ghg,file='Data/Not public/edgar_ft_v1_data_ghg_gwp100_ar6.RData')
save(edgar_ghg_ar4,file='Data/Not public/edgar_ft_v1_data_ghg_gwp100_ar4.RData')
