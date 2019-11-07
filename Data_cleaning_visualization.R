rm(list = ls())
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library(ggthemes)
library(here)
library(forecast) #fourier terms
library(TSstudio) #manipulate ts objects
library(tsibble)  #for yearmonth and yearweek
library(splines)
# Import raw data ---------------------------------------------------------

COD1997_2016 <- fread(here("Data", "COD1997!2016!COD1997!2016_F1.csv"), 
                      drop = c("Serial_No", "RegistrationYear", "RegistrationMonth", 
                               "RegistrationDay", "MStatus", "Relationship", 
                               "NextKinSmoker", "EduCode", "OccupationGrp", "Smoker",
                               "PopGroup", "Citizenship", "BirthProv", "ResProv", 
                               "ResCountry", "Ascertainment","Part2_CauseA", 
                               "Part2_CauseB", "Part2_InjuryA", "Part2_InjuryB",
                               "ProcessingYear", "StillbornMass", "NaturalUnnatural",
                               "Underlying_Broad_Grp", "Underlying_Main_Grp"))
print(COD1997_2016)

Pop_estimates_SA <- read_excel(here("Data", "Midyear_Pop_Estimates.xlsx"), sheet = 1)
Pop_estimates_provincial <- read_excel(here("Data", "Midyear_Pop_Estimates.xlsx"), sheet = 2)
Pop_estimates_under5 <- read_excel(here("Data", "Midyear_Pop_Estimates.xlsx"), sheet = 3)

# source STATS SA. For 2012, we use 2011 Census results. All other years we have mid-year population estimates

# Correct miscoding of death type and stillborns --------------------------

# P95 fetal death of unspecified cause. Cannot be classified as individual death (deceased)

# Check miscoding

COD1997_2016[(CauseA == "P95" | CauseB == "P95" | CauseC == "P95" | CauseD == "P95") & DeathType == 1]
# 1557 records wrongly codded

# Correct miscoding

COD1997_2016[, DeathType := ifelse(CauseA == "P95" | CauseB == "P95" | CauseC == "P95" | CauseD == "P95", 2,DeathType)]

# Data cleaning -----------------------------------------------------------

COD1997_2016 <- COD1997_2016%>% 
  mutate(DeathType = factor(DeathType, levels = c(1,2), labels = c("Deceased", "Stillborn")),
         DOB = ymd(paste(BirthYear, BirthMonth, BirthDay, sep = "-")),
         DOD = ymd(paste(DeathYear, DeathMonth, DeathDay, sep = "-"), locale = "C"),
         WOD = yearweek(DOD),
         MOD = yearmonth(DOD),
         Sex = factor(Sex, levels = c(1,2,8,9), 
                      labels = c("Male", "Female", "Unknown", "Unspecified")),
         DeathProv = factor(DeathProv, levels = c(1:9, 9996, 9998),
                            labels = c("Western_Cape", "Eastern_Cape", "Northern_Cape",
                                       "Free_State", "KwaZulu-Natal", "North_West",
                                       "Gauteng", "Mpumalanga", "Limpopo", "Unknown",
                                       "Outside_SA")),
         Pregnancy = factor(Pregnancy, levels = c(1,2,3,8,9),
                            labels = c("Yes", "No", "Not_Applicable", "Unknown", "Unspecified")),
         DeathInst = factor(DeathInst, levels = c(1:6,8,9),
                            labels = c("Hospital", "Emergency_room_outpatient", "Dead_on_Arrival","Nursing_home", "Home", "Other", "Unknown", "Unspecified"))) %>%
  rename(Gender = Sex, DeathPlace = DeathInst) %>% 
  select(-c(BirthYear, BirthMonth, BirthDay)) 

#save(COD1997_2016, file = here("Data", "COD1997_2016.Rdata"))

# Age group variable ------------------------------------------------------
# Necessary if we are going to consider <5 only in RSV analyses

COD1997_2016_analysis <- COD1997_2016 %>% 
  mutate(Age_group = case_when(
    AgeYear < 5 ~ "<5",
    AgeYear >= 5 & AgeYear < 20 ~ "5-19",
    AgeYear >= 20 & AgeYear < 45 ~ "20-44",
    AgeYear >= 45 & AgeYear < 65 ~ "45-64",
    AgeYear >= 65 & AgeYear != 999 ~ "65+"
  ))

# split ICD10 code (letter and numeric) for cause of death, injury cause of death, other cause, other injury, underlying cause.

COD1997_2016_analysis <- COD1997_2016_analysis %>% 
  mutate(CauseA_L = str_sub(CauseA, start = 1, end = 1),
         CauseA_N = as.numeric(str_sub(CauseA, start = 2, end = 3)),
         CauseB_L = str_sub(CauseB, start = 1, end = 1),
         CauseB_N = as.numeric(str_sub(CauseB, start = 2, end = 3)),
         CauseC_L = str_sub(CauseC, start = 1, end = 1),
         CauseC_N = as.numeric(str_sub(CauseC, start = 2, end = 3)),
         CauseD_L = str_sub(CauseD, start = 1, end = 1),
         CauseD_N = as.numeric(str_sub(CauseD, start = 2, end = 3)),
         InjuryA_L = str_sub(InjuryA, start = 1, end = 1),
         InjuryA_N = as.numeric(str_sub(InjuryA, start = 2, end = 3)),
         InjuryB_L = str_sub(InjuryB, start = 1, end = 1),
         InjuryB_N = as.numeric(str_sub(InjuryB, start = 2, end = 3)),
         InjuryC_L = str_sub(InjuryC, start = 1, end = 1),
         InjuryC_N = as.numeric(str_sub(InjuryC, start = 2, end = 3)),
         InjuryD_L = str_sub(InjuryD, start = 1, end = 1),
         InjuryD_N = as.numeric(str_sub(InjuryD, start = 2, end = 3)),
         OtherInjury_L = str_sub(OtherInjury, start = 1, end = 1),
         OtherInjury_N = as.numeric(str_sub(OtherInjury, start = 2, end = 3)),
         OtherCause_L = str_sub(OtherCause, start = 1, end = 1),
         OtherCause_N = as.numeric(str_sub(OtherCause, start = 2, end = 3)),
         UnderlyingCause_L = str_sub(Underlyingcause, start = 1, end = 1),
         UnderlyingCause_N = as.numeric(str_sub(Underlyingcause, start = 2, end = 3)))
                               

# Generate all cause of death variable ------------------------------------
# exclude stillbirths and deaths outside SA

COD1997_2016_analysis <- COD1997_2016_analysis %>% 
  mutate(All = ifelse(DeathType == "Deceased" & DeathProv != "Outside_SA", 1, 0))
           
# Generate all respiratory causes of death variable -----------------------
# ICD10 code: J00-J99

COD1997_2016_analysis <- COD1997_2016_analysis %>% 
  mutate(AllRes = ifelse((CauseA_L == "J" | CauseB_L == "J" | CauseC_L == "J" | CauseD_L == "J" | OtherCause_L == "J" | UnderlyingCause_L == "J") & DeathType == "Deceased" & DeathProv != "Outside_SA", 1, 0)) 

# Categorise P22 (respiratory distress of newborn),P23 (congenital pneumonia), P24 (neonetal aspiration syndromes), P25 (interstitial emphysema and related conditions) and P28 (other respiratory conditions) as all respiratory because they are respiratory disorders specific to the parinetal period
 
COD1997_2016_analysis <- COD1997_2016_analysis %>% 
  mutate(AllRes = case_when(
    CauseA_L == "P" & (CauseA_N == 22|CauseA_N == 23|CauseA_N == 24|CauseA_N == 25|CauseA_N == 28) & DeathProv != "Outside_SA" ~ 1, 
    CauseB_L == "P" & (CauseB_N == 22|CauseB_N == 23|CauseB_N == 24|CauseB_N == 25|CauseB_N == 28) & DeathProv != "Outside_SA" ~ 1,
    CauseC_L == "P" & (CauseC_N == 22|CauseC_N == 23|CauseC_N == 24|CauseC_N == 25|CauseC_N == 28) & DeathProv != "Outside_SA" ~ 1,
    CauseD_L == "P" & (CauseD_N == 22|CauseD_N == 23|CauseD_N == 24|CauseD_N == 25|CauseD_N == 28) & DeathProv != "Outside_SA" ~ 1,
    OtherCause_L == "P" & (OtherCause_N == 22|OtherCause_N == 23|OtherCause_N == 24|OtherCause_N == 25|OtherCause_N == 28) & DeathProv != "Outside_SA" ~ 1,
    UnderlyingCause_L == "P" & (UnderlyingCause_N == 22|UnderlyingCause_N == 23|UnderlyingCause_N == 24|UnderlyingCause_N == 25|UnderlyingCause_N == 28) & DeathProv != "Outside_SA" ~ 1,
    TRUE ~ AllRes))

# Generate Pneumonia & Influenza cause of death variable ------------------
# ICD10 code: J10-J18
# Also include for congenital pneumonia P23

COD1997_2016_analysis <- COD1997_2016_analysis %>% 
  mutate(PI = case_when(
    CauseA_L == "J" & CauseA_N >= 10 & CauseA_N <=18 & DeathProv != "Outside_SA" ~ 1,
    CauseB_L == "J" & CauseB_N >= 10 & CauseB_N <=18 & DeathProv != "Outside_SA" ~ 1,
    CauseC_L == "J" & CauseC_N >= 10 & CauseC_N <=18 & DeathProv != "Outside_SA" ~ 1,
    CauseD_L == "J" & CauseD_N >= 10 & CauseD_N <=18 & DeathProv != "Outside_SA" ~ 1,
    OtherCause_L == "J" & OtherCause_N >= 10 & OtherCause_N <=18 & DeathProv != "Outside_SA" ~ 1,
    UnderlyingCause_L == "J" & UnderlyingCause_N >= 10 & UnderlyingCause_N <=18 & DeathProv != "Outside_SA" ~ 1,
    ((CauseA_L == "P" & CauseA_N == 23)|(CauseB_L == "P" & CauseB_N == 23)|(CauseC_L == "P" & CauseC_N == 23)|(CauseD_L == "P" & CauseD_N == 23)|(OtherCause_L == "P" & OtherCause_N == 23)|(UnderlyingCause_L == "P" & UnderlyingCause_N == 23)) & DeathProv != "Outside_SA" ~ 1,
    TRUE ~ 0
  ))

# Generate Pneumonia & other viral cause of death variable ----------------
# Viral pneumonia, not elsewhere classified J12

COD1997_2016_analysis <- COD1997_2016_analysis %>% 
  mutate(POther = case_when(
    CauseA_L == "J" & CauseA_N == 12 & DeathProv != "Outside_SA" ~ 1,
    CauseB_L == "J" & CauseB_N == 12 & DeathProv != "Outside_SA" ~ 1,
    CauseC_L == "J" & CauseC_N == 12 & DeathProv != "Outside_SA" ~ 1,
    CauseD_L == "J" & CauseD_N == 12 & DeathProv != "Outside_SA" ~ 1,
    OtherCause_L == "J" & OtherCause_N == 12 & DeathProv != "Outside_SA" ~ 1,
    UnderlyingCause_L == "J" & UnderlyingCause_N == 12 & DeathProv != "Outside_SA" ~ 1,
    TRUE ~ 0
  ))

# Generate Influenza specific cause of death variable ---------------------
# J10 and J11

COD1997_2016_analysis <- COD1997_2016_analysis %>% 
  mutate(Flu = case_when(
    CauseA_L == "J" & CauseA_N >= 10 & CauseA_N <=11 & DeathProv != "Outside_SA" ~ 1,
    CauseB_L == "J" & CauseB_N >= 10 & CauseB_N <=11 & DeathProv != "Outside_SA" ~ 1,
    CauseC_L == "J" & CauseC_N >= 10 & CauseC_N <=11 & DeathProv != "Outside_SA" ~ 1,
    CauseD_L == "J" & CauseD_N >= 10 & CauseD_N <=11 & DeathProv != "Outside_SA" ~ 1,
    OtherCause_L == "J" & OtherCause_N >= 10 & OtherCause_N <=11 & DeathProv != "Outside_SA" ~ 1,
    UnderlyingCause_L == "J" & UnderlyingCause_N >= 10 & UnderlyingCause_N <=11 & DeathProv != "Outside_SA" ~ 1,
    TRUE ~ 0
  ))


# Generate RSV cause of death variable for children <5 --------------------
# ICD10 codes: J20-J22 (acute lower respiratory infection: broncitis, bronchiolitis, unspecified)

COD1997_2016_analysis <- COD1997_2016_analysis %>% 
  mutate(RSV = case_when(
    CauseA_L == "J" & CauseA_N >= 20 & CauseA_N <=22 & Age_group == "<5" & DeathProv != "Outside_SA" ~ 1,
    CauseB_L == "J" & CauseB_N >= 20 & CauseB_N <=22 & Age_group == "<5" & DeathProv != "Outside_SA" ~ 1,
    CauseC_L == "J" & CauseC_N >= 20 & CauseC_N <=22 & Age_group == "<5" & DeathProv != "Outside_SA" ~ 1,
    CauseD_L == "J" & CauseD_N >= 20 & CauseD_N <=22 & Age_group == "<5" & DeathProv != "Outside_SA" ~ 1,
    OtherCause_L == "J" & OtherCause_N >= 20 & OtherCause_N <=22 & Age_group == "<5" & DeathProv != "Outside_SA" ~ 1,
    UnderlyingCause_L == "J" & UnderlyingCause_N >= 20 & UnderlyingCause_N <=22 & Age_group == "<5" & DeathProv != "Outside_SA" ~ 1,
    TRUE ~ 0
  ))


# save(COD1997_2016_analysis, file = here("Data","COD1997_2016_analysis.Rdata"))

# Aggregated datasets -----------------------------------------------------

# Weekly deaths 1997_2016 for the different causes without province

Weekly_COD1997_2016_analysis <- COD1997_2016_analysis %>%
  group_by(WOD) %>%
  summarise(All_deaths = sum(All), AllRes_deaths = sum(AllRes),
            PI_deaths = sum(PI), POther_deaths = sum(POther),
            Flu_deaths = sum(Flu), RSV_deaths = sum(RSV)) %>% 
  drop_na() %>% 
  mutate(Year = as.numeric(format(WOD, "%Y"))) %>%
  ungroup()

# save(Weekly_COD1997_2016_analysis, file = here("Data","Weekly_COD1997_2016_analysis.Rdata"))

# Weekly deaths 1997_2016 for the different causes and provinces

Weekly_COD1997_2016_analysis_by_Province <-COD1997_2016_analysis %>%
  group_by(WOD, DeathProv) %>%
  summarise(All_deaths = sum(All), AllRes_deaths = sum(AllRes),
            PI_deaths = sum(PI), POther_deaths = sum(POther),
            Flu_deaths = sum(Flu), RSV_deaths = sum(RSV)) %>% 
  filter(DeathProv != "Unknown" & DeathProv != "Outside_SA") %>% 
  drop_na() %>% 
  mutate(Year = as.numeric(format(WOD, "%Y"))) %>%
  ungroup()

# save(Weekly_COD1997_2016_analysis_by_Province, file = here("Data","Weekly_COD1997_2016_analysis_by_Province.Rdata"))

# Monthly deaths 1997_2016 for the different causes without province

Monthly_COD1997_2016_analysis <- COD1997_2016_analysis %>%
  group_by(MOD) %>%
  summarise(All_deaths = sum(All), AllRes_deaths = sum(AllRes),
            PI_deaths = sum(PI), POther_deaths = sum(POther),
            Flu_deaths = sum(Flu), RSV_deaths = sum(RSV)) %>% 
  drop_na() %>% 
  mutate(Year = as.numeric(format(MOD, "%Y"))) %>%
  ungroup()

# save(Monthly_COD1997_2016_analysis, file = here("Data","Monthly_COD1997_2016_analysis.Rdata"))

# Monthly deaths 1997_2016 for the different causes and provinces

Monthly_COD1997_2016_analysis_by_Province <- COD1997_2016_analysis %>%
  group_by(MOD, DeathProv) %>%
  summarise(All_deaths = sum(All), AllRes_deaths = sum(AllRes),
            PI_deaths = sum(PI), POther_deaths = sum(POther),
            Flu_deaths = sum(Flu), RSV_deaths = sum(RSV)) %>% 
  filter(DeathProv != "Unknown" & DeathProv != "Outside_SA") %>% 
  drop_na() %>% 
  mutate(Year = as.numeric(format(MOD, "%Y"))) %>% 
  ungroup()

# save(Monthly_COD1997_2016_analysis_by_Province, file = here("Data","Monthly_COD1997_2016_analysis_by_Province.Rdata"))

# # Yearly totals
# 
# Yearly_totals <- Monthly_COD1997_2016_analysis_by_Province %>% 
#   group_by(Year, DeathProv) %>% 
#   summarise(All_deaths_T = sum(All_deaths), AllRes_deaths_T = sum(AllRes_deaths),
#             PI_deaths_T = sum(PI_deaths), POther_deaths_T = sum(POther_deaths),
#             Flu_deaths_T = sum(Flu_deaths), RSV_deaths_T = sum(RSV_deaths))



# Mortality rates ---------------------------------------------------------
# Weekly
# Countrywide

Weekly_COD1997_2016_analysis_Rates <- left_join(Weekly_COD1997_2016_analysis, Pop_estimates_SA,by = "Year") %>% 
  mutate(All_deaths_rate = (All_deaths/PopEstimate)*100000, 
         AllRes_dealth_rate = (AllRes_deaths/PopEstimate)*100000, 
         PI_deaths_rate = (PI_deaths/PopEstimate)*100000, 
         POther_deaths_rate = (POther_deaths/PopEstimate)*100000, 
         Flu_deaths_rate = (Flu_deaths/PopEstimate)*100000,  
         RSV_deaths_rate = (RSV_deaths/PopEstimate)*100000) %>% 
  select(WOD, All_deaths_rate:RSV_deaths_rate)

# save(Weekly_COD1997_2016_analysis_Rates, file = here("Data","Weekly_COD1997_2016_analysis_Rates.Rdata"))

# Provincial

Weekly_COD1997_2016_analysis_by_Province_Rates <- left_join(Weekly_COD1997_2016_analysis_by_Province, Pop_estimates_provincial,
                                                             by = c("DeathProv" = "Province", "Year")) %>% left_join(., Pop_estimates_under5, by = c("DeathProv" = "Province", "Year")) %>% 
  mutate(All_deaths_rate = (All_deaths/PopEstimate)*100000, 
         AllRes_dealth_rate = (AllRes_deaths/PopEstimate)*100000, 
         PI_deaths_rate = (PI_deaths/PopEstimate)*100000, 
         POther_deaths_rate = (POther_deaths/PopEstimate)*100000, 
         Flu_deaths_rate = (Flu_deaths/PopEstimate)*100000,  
         RSV_deaths_rate = (RSV_deaths/PopEstimateU5)*100000) %>% 
  select(WOD, DeathProv, All_deaths_rate:RSV_deaths_rate)
#drop_na() %>% 

# save(Weekly_COD1997_2016_analysis_by_Province_Rates, file = here("Data","Weekly_COD1997_2016_analysis_by_Province_Rates.Rdata"))

# Monthly
# Countrywide

Monthly_COD1997_2016_analysis_Rates <- left_join(Monthly_COD1997_2016_analysis, Pop_estimates_SA,by = "Year") %>% 
  mutate(All_deaths_rate = (All_deaths/PopEstimate)*100000, 
         AllRes_dealth_rate = (AllRes_deaths/PopEstimate)*100000, 
         PI_deaths_rate = (PI_deaths/PopEstimate)*100000, 
         POther_deaths_rate = (POther_deaths/PopEstimate)*100000, 
         Flu_deaths_rate = (Flu_deaths/PopEstimate)*100000,  
         RSV_deaths_rate = (RSV_deaths/PopEstimate)*100000) %>% 
  select(MOD, All_deaths_rate:RSV_deaths_rate)

# save(Monthly_COD1997_2016_analysis_Rates, file = here("Data","Monthly_COD1997_2016_analysis_Rates.Rdata"))

# Provincial

Monthly_COD1997_2016_analysis_by_Province_Rates <- left_join(Monthly_COD1997_2016_analysis_by_Province, Pop_estimates_provincial,
                by = c("DeathProv" = "Province", "Year")) %>% left_join(., Pop_estimates_under5, by = c("DeathProv" = "Province", "Year")) %>% 
  mutate(All_deaths_rate = (All_deaths/PopEstimate)*100000, 
         AllRes_dealth_rate = (AllRes_deaths/PopEstimate)*100000, 
         PI_deaths_rate = (PI_deaths/PopEstimate)*100000, 
         POther_deaths_rate = (POther_deaths/PopEstimate)*100000, 
         Flu_deaths_rate = (Flu_deaths/PopEstimate)*100000,  
         RSV_deaths_rate = (RSV_deaths/PopEstimateU5)*100000) %>% 
  select(MOD, DeathProv, All_deaths_rate:RSV_deaths_rate)
  #drop_na() %>% 

# save(Monthly_COD1997_2016_analysis_by_Province_Rates, file = here("Data","Monthly_COD1997_2016_analysis_by_Province_Rates.Rdata"))


# Mortality plots ---------------------------------------------------------

# Without provinces

tidy_Weekly <- gather(data = Weekly_COD1997_2016_analysis_Rates,
                    key = "deaths",
                    value = "rates",
                    All_deaths_rate:RSV_deaths_rate,
                    na.rm = T)

ggplot(tidy_Weekly,aes(x = WOD, y = rates)) +
  geom_line() + 
  facet_wrap(~deaths, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Weekly death rates per 100 000")+
  ggtitle("Weekly death rates per 100 000 from 1997-2016")


tidy_Monthly <- gather(data = Monthly_COD1997_2016_analysis_Rates,
                     key = "deaths",
                     value = "rates",
                     All_deaths_rate:RSV_deaths_rate,
                     na.rm = T)

ggplot(tidy_Monthly,aes(x = MOD, y = rates)) +
  geom_line() + 
  facet_wrap(~deaths, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Monthly death rates per 100 000")+
  ggtitle("Monthly death rates per 100 000 from 1997-2016")

# With provinces
# (i) Weekly deaths

ggplot(Weekly_COD1997_2016_analysis_by_Province_Rates,aes(x = WOD, y = AllRes_dealth_rate)) +
  geom_line() + 
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Weekly All respiratory death rate per 100 000")+
  ggtitle("Weekly All respiratory death rate per 100 000 from 1997-2016")

ggplot(Weekly_COD1997_2016_analysis_by_Province_Rates,aes(x = WOD, y = PI_deaths_rate)) +
  geom_line() + 
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Weekly PI death rate per 100 000")+
  ggtitle("Weekly PI death rate per 100 000 from 1997-2016")

# ggsave("Weekly_PI_death_rates.pdf")

ggplot(Weekly_COD1997_2016_analysis_by_Province_Rates,aes(x = WOD, y = RSV_deaths_rate)) +
  geom_line() + 
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Weekly RSV death rate per 100 000")+
  ggtitle("Weekly RSV death rate per 100 000 for under 5 from 1997-2016")

# (ii) Monthly deaths

ggplot(Monthly_COD1997_2016_analysis_by_Province_Rates,aes(x = MOD, y = AllRes_dealth_rate)) +
  geom_line() + 
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Monthly all respiratory deaths per 100 000")+
  ggtitle("Monthly all respiratory deaths per 100 000 from 1997-2016")+
  theme_bw()

ggplot(Monthly_COD1997_2016_analysis_by_Province_Rates,aes(x = MOD, y = PI_deaths_rate)) +
  geom_line() + 
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  xlab("") +
  ylab("Monthly P&I mortality per 100 000 people")+
  ggtitle("Monthly P&I mortality per 100 000 people from 1997-2016")+
  theme_bw()

# ggsave("Monthly_PI_death_rates.pdf")

ggplot(Monthly_COD1997_2016_analysis_by_Province_Rates,aes(x = MOD, y = Flu_deaths_rate)) +
  geom_line() + 
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Monthly Flu mortality per 100,000 people")+
  ggtitle("Monthly Flu mortality per 100,000 people from 1997-2016")

ggplot(Monthly_COD1997_2016_analysis_by_Province_Rates,aes(x = MOD, y = RSV_deaths_rate)) +
  geom_line() + 
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Monthly RSV mortality per 100 000 children under five")+
  ggtitle("Monthly RSV mortality per 100 000 children under five from 1997-2016")


# Detreding data ----------------------------------------------------------

Monthly_PI_death_detrend <- Monthly_COD1997_2016_analysis_by_Province_Rates %>% 
  select(MOD, DeathProv, PI_deaths_rate) 

# a. differencing
Monthly_PI_death_diff_det <-Monthly_PI_death_detrend%>% 
  group_by(DeathProv) %>% 
  mutate(PI_deaths_diff = PI_deaths_rate - lag(PI_deaths_rate),
         PI_deaths_diff_Order2 = PI_deaths_diff - lag(PI_deaths_diff))

ggplot(Monthly_PI_death_diff_det) +
  geom_line(aes(x = MOD, y = PI_deaths_diff),col = "blue") +
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Monthly P&I death rate per 100 000")+
  ggtitle("Montlhy P&I mortality rate per 100 000 difference detrended")

# ggsave("Diff_Detrended.pdf")

ggplot(Monthly_PI_death_diff_det) +
  geom_line(aes(x = MOD, y = PI_deaths_diff_Order2),col = "blue") +
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("Monthly P&I death rate per 100 000")+
  ggtitle("Montlhy P&I mortality rate per 100 000 difference detrended")

# b. Model fitting
# Use stl() or decompose() function for trend decomposition
# stl() allows seasonality to vary unlike decompose

province_list <- names(table(Monthly_PI_death_detrend$DeathProv))
trend_list <- list()
detrended_list <- list()

for (i in 1:length(province_list)) {
  province = province_list[i]
  Prov_data <- filter(Monthly_PI_death_detrend, DeathProv == province)
  ts_data = ts(Prov_data$PI_deaths_rate, frequency = 12, 
               start = c(1997,1), end = c(2016,12))
  trend <- stl(ts_data, s.window = "periodic", t.window = 60)$time.series[,"trend"]
  trend_list[[province]] <- trend
  detrended_list[[province]] <- ts_data - trend
}

Montlhy_PI_Trend_dt <- lapply(trend_list, ts_reshape, type = "long") %>% 
  plyr::ldply(data.frame) %>% 
  unite("MOD",year, month, sep = "-", remove = T) %>% 
  transmute(DeathProv = .id, 
            MOD = yearmonth(MOD),
            Trend = value)

Monthly_PI_Detrended_dt <- lapply(detrended_list, ts_reshape, type = "long") %>% 
  plyr::ldply(data.frame) %>% 
  unite("MOD",year, month, sep = "-", remove = T) %>% 
  transmute(DeathProv = .id, 
            MOD = yearmonth(MOD),
            Detrended = value)

Monthly_PI_death_detrend_dt <- Monthly_PI_death_detrend %>% 
  left_join(Montlhy_PI_Trend_dt, by = c("DeathProv", "MOD")) %>% 
  left_join(Monthly_PI_Detrended_dt, by = c("DeathProv", "MOD")) %>% 
  mutate(MOD = yearmonth(MOD))


ggplot(Monthly_PI_death_detrend_dt) +
  geom_line(aes(x = MOD, y = Detrended), col = "blue")+
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("")+
  ggtitle("Montlhy P&I mortality rate per 100 000 model detrended")

# ggsave("Model_Detrended.pdf")

ggplot(Monthly_PI_death_detrend_dt) +
  geom_line(aes(x = MOD, y = PI_deaths_rate)) + 
  geom_line(aes(x = MOD, y = Trend), col = "red")+
  geom_line(aes(x = MOD, y = Detrended), col = "blue")+
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y")+
  theme_bw() +
  xlab("") +
  ylab("")+
  ggtitle("Montlhy P&I mortality rate per 100 000 model detrended")+
  labs(caption = "The black curve shows the original time series, the red curve shows the trend and the blue curve shows the detrended time series")+
  theme(plot.caption = element_text(size = 8, hjust = 0))

# ggsave("Detrended.pdf")

# Using spline model

# Add a sequence of time

# t = seq(from = 1997.000, by = 1/12, length.out = 240)

Monthly_PI_death_detrend_spline <- Monthly_PI_death_detrend %>% 
  mutate(t = zoo::as.yearmon(MOD),
         t = as.numeric(t))

province_list <- names(table(Monthly_PI_death_detrend$DeathProv))
mylist <- list()

for (i in 1:length(province_list)) {
  province = province_list[i]
  Prov_data <- filter(Monthly_PI_death_detrend_spline, DeathProv == province)
  fit = lm(PI_deaths_rate ~ ns(t, 4),data = Prov_data)
  trend <- predict(fit)
  detrended <- Prov_data$PI_deaths_rate - trend
  data <- mutate(Prov_data,
                 trend,
                 detrended)
  mylist[[province]] <- data
}

Monthly_PI_death_detrend_spline_df<- bind_rows(mylist)

ggplot(data = Monthly_PI_death_detrend_spline_df)+
  geom_line(aes(x = t, y = PI_deaths_rate), col = "black")+
  geom_line(aes(x = t, y = trend),col = "red", size = 1)+
  geom_line(aes(x = t, y = detrended), col = "blue")+
  facet_wrap(~DeathProv, ncol = 3, scales = "free_y") +
  theme_bw()+
  xlab("") +
  ylab("")+
  ggtitle("Montlhy P&I death rate per 100 000")+
  labs(caption = "The black curve shows the original time series, the red curve shows the trend and the blue curve shows the detrended time series")+
  theme(plot.caption = element_text(size = 8, hjust = 0))

ggsave("Spline_detrended.pdf")

# # Time series heatmap
# # P&I
# 
# ggplot(Monthly_PI_death_diff_det, aes(MOD, DeathProv, fill = PI_deaths_diff))+
#   geom_tile() +
#   scale_fill_gradient(name = "P&I mortality rate model detrended", low = "yellow", high = "red")+
#   labs(y = "Province", x = "Year", title = "Monthly P&I mortality per 100,000 people")

