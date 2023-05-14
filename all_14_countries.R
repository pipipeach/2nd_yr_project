
# Download Packages
#####################################################################################################################################################################
library(dplyr)
library(haven)
library(plyr)
library(foreign)
library(readxl)
library(ggplot2)

# Change working directory 
setwd("/Users/ruoyingtao/R_Files/Medical School Openings/IPUMS data")

# Graphing 
g1 <- ggplot(province_occ_pop, aes(x = age_at_opening, y = md_density)) +
        geom_point() + geom_vline(xintercept = 27)
g1


# Benin 
#####################################################################################################################################################################
# Download Data
benin <- read_dta("benin.dta")
benin_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "benin")

# Medical School opening in 1975 and 2001, MD occupation available in 1992, 2002, and 2013 data, 
# code 0061, 124, 129
table(benin$year)
summary(benin$age) # age available in all individuals
# Does not have the birth year variable, so I created it from scratch 

# For 1975 opening: 
benin_clean <- benin %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1975 - birthyr)

summary(benin_clean$birthyr)
table(benin_clean$year)
summary(benin_clean$age_at_opening)

province_pop <- benin_clean %>%
        group_by(year, migi_p_bj, age_at_opening) %>%
        dplyr::summarise(province_population = n())

# Department of birth migi_p_bj is also available 
province_occ_pop <- benin_clean %>%
        group_by(year,migi_p_bj,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 124 | occ == 129) %>%
        left_join(province_pop, by = c("year" = "year", "migi_p_bj" = "migi_p_bj","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(benin_province, by = "migi_p_bj") %>%
        filter(migi_p_bj != 999) %>%
        filter(md_density < 1)

# For 2001 opening: 
benin_clean <- benin %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2001 - birthyr)

summary(benin_clean$birthyr)
table(benin_clean$year)
summary(benin_clean$age_at_opening)

province_pop <- benin_clean %>%
        group_by(year, migi_p_bj, age_at_opening) %>%
        dplyr::summarise(province_population = n())

# Department of birth migi_p_bj is also available 
province_occ_pop <- benin_clean %>%
        group_by(year,migi_p_bj,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 124 | occ == 129) %>%
        left_join(province_pop, by = c("year" = "year", "migi_p_bj" = "migi_p_bj","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(benin_province, by = "migi_p_bj") %>%
        filter(migi_p_bj != 999) %>%
        filter(md_density < 1)

# Botswana 
#####################################################################################################################################################################
# Import data
botswana <- read_dta("botswana.dta")
botswana_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "botswana")

# Medical School opened in 1969. MD occupation available in 2001 & 2011 data, code 061,501, 1401,1404
# Does not have the birth year variable, so I created it from scratch 
summary(botswana$birthyr)
table(botswana$year)
summary(botswana$age)

botswana_clean <- botswana %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2009 - birthyr)

summary(botswana_clean$birthyr)
table(botswana_clean$year)
summary(botswana_clean$age_at_opening)

province_pop <- botswana_clean %>%
        group_by(year, bplbw, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- botswana_clean %>%
        group_by(year,bplbw,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 231) %>%
        left_join(province_pop, by = c("year" = "year","bplbw" = "bplbw","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(botswana_province, by = "bplbw") %>%
        filter(bplbw != 99) %>%
        filter(md_density < 1)


# Burkina Faso  
#####################################################################################################################################################################
# Download Data
burkinafaso <- read_dta("burkinafaso.dta")
burkinafaso_province <- read_xlsx("burkinafaso_province.xlsx")

# MD only available in 1985 data - occupation code 120 (MD), 121(Physician Specialist)

summary(burkinafaso$birthyr)
summary(burkinafaso$year)
summary(burkinafaso$age)


# Eliminate the people without documented age, and calculate birth year manually 
# Medical school opened in 1981, so we can calculate age at opening by subtracting birth year from 1981
# Only 34 MDs in the entire sample population
burkinafaso_clean <- burkinafaso %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1981 - birthyr)

# Age distribution of entire population 
hist(burkinafaso_clean$age)
hist(burkinafaso_clean$age_at_opening)

summary(burkinafaso_clean$birthyr)
summary(burkinafaso_clean$year)
summary(burkinafaso_clean$age)

province_pop <- burkinafaso_clean %>%
        group_by(year,bplbf,age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- burkinafaso_clean %>%
        group_by(year,bplbf,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 120) %>%
        left_join(province_pop, by = c("year" = "year", "bplbf" = "bplbf","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(burkinafaso_province, by = "bplbf") %>%
        filter(bplbf !=98)


# Egypt
#####################################################################################################################################################################

# Import data
egypt <- read_dta("egypt.dta")
egypt_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "egypt")

# Med Sch opened in 1827, 1942, 1948, 1960, 1962 (two med sch), 1964, 1965, 1970, 1977, 1981,
# 1984 (two med sch), 1989, 1992, 1995, 1996 (two med sch), 2000, 2007, 2013 (five med sch),
# 2016, 2018, 2019 (four med sch), 2020 & 2021 
# MD occupation available in 1986 & 2006 data, code 006 & 222
# Does not have the birth year variable, so I created it from scratch 
summary(egypt$birthyr)
table(egypt$year)
summary(egypt$age)

# for 1942 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1942 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1948 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1948 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1960 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1960 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1962 medical school opening (two medical schools opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1962 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1964 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1964 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1965 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1965 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1970 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1970 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1977 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1977 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1981 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1981 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1984 medical school opening (two medical schools opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1984 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1989 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1989 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1992 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1992 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1995 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1995 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 1996 medical school opening (two medical schools opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1996 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 2000 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2000 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 2007 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2007 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 2013 medical school opening (five medical schools opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2013 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 2016 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2016 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 2018 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2018 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 2019 medical school opening (four medical schools opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2019 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 2020 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2020 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# for 2021 medical school opening (one medical school opened)
egypt_clean <- egypt %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2021 - birthyr)

summary(egypt_clean$birthyr)
table(egypt_clean$year)
summary(egypt_clean$age_at_opening)

province_pop <- egypt_clean %>%
        group_by(year, bpleg3, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- egypt_clean %>%
        group_by(year,bpleg3,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bpleg3" = "bpleg3","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(egypt_province, by = "bpleg3") %>%
        filter(bpleg3 != 9998 | bpleg3 != 9999) %>%
        filter(md_density < 1)


# Guinea
#####################################################################################################################################################################
# Import Data
guinea <- read_dta("guinea.dta")
guinea_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "guinea")

# Medical School opening in 1967, MD occupation available in 1996 and 2014 data, 
# code 222, 103, 104, 108
summary(guinea$year)
summary(guinea$age) # age available in all individuals
# Does not have the birth year variable, so I created it from scratch 

# For 1967 opening: 
guinea_clean <- guinea %>%
        filter(age != 999 & year > 1983) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1967 - birthyr)

summary(guinea_clean$birthyr)
summary(guinea_clean$year)
summary(guinea_clean$age_at_opening)

province_pop <- guinea_clean %>%
        group_by(year, bplgn, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- guinea_clean %>%
        group_by(year,bplgn,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 222 | occ == 103 | occ == 104 | occ == 108) %>%
        left_join(province_pop, by = c("year" = "year", "bplgn" = "bplgn","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(guinea_province, by = "bplgn") %>%
        filter(age_at_opening>0 & bplgn != 99) %>%
        filter(md_density < 1)



# Kenya
#####################################################################################################################################################################

kenya <- read_dta("kenya.dta")
kenya_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "kenya")

# Medical School opened in 1967,1990, 2004 & 2007. MD occupation available in 1989 data, code 1211
# Does not have the birth year variable, so I created it from scratch 
summary(kenya$year)
summary(kenya$age)

# For 1967 opening: 
kenya_clean <- kenya %>%
        filter(age != 999 & year >= 1989) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1967 - birthyr)

summary(kenya_clean$birthyr)
summary(kenya_clean$year)
summary(kenya_clean$age)

province_pop <- kenya_clean %>%
        group_by(year, bplke, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- kenya_clean %>%
        group_by(year,bplke,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 1211) %>%
        left_join(province_pop, by = c("year" = "year", "bplke" = "bplke","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(kenya_province, by = "bplke") %>%
        filter (bplke != 900 & bplke != 999) %>%
        # One instance where the md_density = 1, and it messes up the distribution of dots on the scatterplot. Delete for now
        filter(md_density < 1)

# For 1990 opening: 
kenya_clean <- kenya %>%
        filter(age != 999 & year >= 1989) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1990 - birthyr)

summary(kenya_clean$birthyr)
summary(kenya_clean$year)
summary(kenya_clean$age)

province_pop <- kenya_clean %>%
        group_by(year, bplke, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- kenya_clean %>%
        group_by(year,bplke,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 1211) %>%
        left_join(province_pop, by = c("year" = "year", "bplke" = "bplke","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(kenya_province, by = "bplke") %>%
        filter (bplke != 900 & bplke != 999) %>%
        # One instance where the md_density = 1, and it messes up the distribution of dots on the scatterplot. Delete for now
        filter(md_density < 1)


# For 2004 opening: 
kenya_clean <- kenya %>%
        filter(age != 999 & year >= 1989) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2004 - birthyr)

summary(kenya_clean$birthyr)
summary(kenya_clean$year)
summary(kenya_clean$age)

province_pop <- kenya_clean %>%
        group_by(year, bplke, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- kenya_clean %>%
        group_by(year,bplke,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 1211) %>%
        left_join(province_pop, by = c("year" = "year", "bplke" = "bplke","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(kenya_province, by = "bplke") %>%
        filter (bplke != 900 & bplke != 999) %>%
        # One instance where the md_density = 1, and it messes up the distribution of dots on the scatterplot. Delete for now
        filter(md_density < 1)

# For 2007 opening: 
kenya_clean <- kenya %>%
        filter(age != 999 & year >= 1989) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2007 - birthyr)

summary(kenya_clean$birthyr)
summary(kenya_clean$year)
summary(kenya_clean$age)

province_pop <- kenya_clean %>%
        group_by(year, bplke, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- kenya_clean %>%
        group_by(year,bplke,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 1211) %>%
        left_join(province_pop, by = c("year" = "year", "bplke" = "bplke","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(kenya_province, by = "bplke") %>%
        filter (bplke != 900 & bplke != 999) %>%
        # One instance where the md_density = 1, and it messes up the distribution of dots on the scatterplot. Delete for now
        filter(md_density < 1)


# Mali
#####################################################################################################################################################################
# Import data
mali <- read_dta("mali.dta")
mali_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "mali")

# Medical School opened in 1969. MD occupation available in 1987, 1998, 2009 data, code 061, 501,1401, 1404
# Does not have the birth year variable, so I created it from scratch 
summary(mali$birthyr)
table(mali$year)
summary(mali$age)

mali_clean <- mali %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1969 - birthyr)

summary(mali_clean$birthyr)
table(mali_clean$year)
summary(mali_clean$age_at_opening)

province_pop <- mali_clean %>%
        group_by(year, bplml, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- mali_clean %>%
        group_by(year,bplml,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 501 | occ == 1401 | occ == 1404) %>%
        left_join(province_pop, by = c("year" = "year","bplml" = "bplml","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(mali_province, by = "bplml") %>%
        filter(bplml != 998) %>%
        filter(md_density < 1)



# Mozambique
#####################################################################################################################################################################

# Import data
mozambique <- read_dta("mozambique.dta")
mozambique_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "mozambique")

# Medical Schools opened in 1963, 2001,2007, and 2010. MD occupation available in 1997 & 2007 data, code 222
summary(mozambique$birthyr)
table(mozambique$year)
summary(mozambique$age)

# Does not have the birth year variable, so I created it from scratch 
mozambique_clean <- mozambique %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1963 - birthyr)

summary(mozambique_clean$birthyr)
table(mozambique_clean$year)
summary(mozambique_clean$age_at_opening)

#mozambique has a birthpalce with higher level (bplmz1) 
province_pop <- mozambique_clean %>%
        group_by(year, bplmz2, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- mozambique_clean %>%
        group_by(year,bplmz2,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bplmz2" = "bplmz2","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(mozambique_province, by = "bplmz2") %>%
        filter(bplmz2 != 9998 & bplmz2 != 9999) %>%
        filter(md_density < 1)



# another medical school opening date (2001)

# Does not have the birth year variable, so I created it from scratch 
mozambique_clean <- mozambique %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2001 - birthyr)

summary(mozambique_clean$birthyr)
table(mozambique_clean$year)
summary(mozambique_clean$age_at_opening)

#mozambique has a birthpalce with higher level (bplmz1) 
province_pop <- mozambique_clean %>%
        group_by(year, bplmz2, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- mozambique_clean %>%
        group_by(year,bplmz2,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bplmz2" = "bplmz2","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(mozambique_province, by = "bplmz2") %>%
        filter(bplmz2 != 9998 & bplmz2 != 9999) %>%
        filter(md_density < 1)


#another medical school opening date (2007)

# Does not have the birth year variable, so I created it from scratch 
mozambique_clean <- mozambique %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2007 - birthyr)

summary(mozambique_clean$birthyr)
table(mozambique_clean$year)
summary(mozambique_clean$age_at_opening)

#mozambique has a birthpalce with higher level (bplmz1) 
province_pop <- mozambique_clean %>%
        group_by(year, bplmz2, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- mozambique_clean %>%
        group_by(year,bplmz2,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bplmz2" = "bplmz2","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(mozambique_province, by = "bplmz2") %>%
        filter(bplmz2 != 9998 & bplmz2 != 9999) %>%
        filter(md_density < 1)



# another medical school opening date (2010)

# Does not have the birth year variable, so I created it from scratch 
mozambique_clean <- mozambique %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2010 - birthyr)

summary(mozambique_clean$birthyr)
table(mozambique_clean$year)
summary(mozambique_clean$age_at_opening)

# mozambique has a birthpalce with higher level (bplmz1) 
province_pop <- mozambique_clean %>%
        group_by(year, bplmz2, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- mozambique_clean %>%
        group_by(year,bplmz2,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 222) %>%
        left_join(province_pop, by = c("year" = "year","bplmz2" = "bplmz2","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(mozambique_province, by = "bplmz2") %>%
        filter(bplmz2 != 9998 & bplmz2 != 9999) %>%
        filter(md_density < 1)


# Rwanda
#####################################################################################################################################################################

# Import data
rwanda <- read_dta("rwanda.dta")
rwanda_province_pre2006 <- read_xlsx("rwanda_province.xlsx",sheet = "1991&2002")
rwanda_province_post2006 <- read_xlsx("rwanda_province.xlsx",sheet = "2012")

# Medical School opened in 1963, MD occupation available in 1991 and 2002 data, code 221 and 222
summary(rwanda$birthyr)
summary(rwanda$year)
summary(rwanda$age)

# For 1963 opening:  
rwanda_clean <- rwanda %>%
        filter(age != 999 & year < 2012) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1963 - birthyr)

summary(rwanda_clean$year)
summary(rwanda_clean$birthyr)
summary(rwanda_clean$age_at_opening)

province_pop <- rwanda_clean %>%
        group_by(year, bplrw1, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- rwanda_clean %>%
        group_by(year,bplrw1,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 221 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year", "bplrw1" = "bplrw1","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(rwanda_province_pre2006, by = "bplrw1") %>%
        filter(bplrw1 !=9999 & bplrw1 !=9888)


# Senegal 
#####################################################################################################################################################################

# Import data
senegal <- read_dta("senegal.dta")
senegal_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "senegal")

# Medical Schools opened in 1950, 2000, and 2008. MD occupation available in 2001 & 2011 data, code 222 and 221
summary(senegal$birthyr)
table(senegal$year)
summary(senegal$age)

# 1950 Opening 
senegal_clean <- senegal %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1950 - birthyr)

summary(senegal_clean$birthyr)
table(senegal_clean$year)
summary(senegal_clean$age_at_opening)

province_pop <- senegal_clean %>%
        group_by(year, bplsn, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- senegal_clean %>%
        group_by(year,bplsn,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 222 | occ == 221) %>%
        left_join(province_pop, by = c("year" = "year","bplsn" = "bplsn","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(senegal_province, by = "bplsn") %>%
        filter(bplsn != 999) %>%
        filter(md_density < 1)


# another med school opening date (2000)
senegal_clean <- senegal %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2000 - birthyr)

summary(senegal_clean$birthyr)
table(senegal_clean$year)
summary(senegal_clean$age_at_opening)

province_pop <- senegal_clean %>%
        group_by(year, bplsn, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- senegal_clean %>%
        group_by(year,bplsn,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 222 | occ == 221) %>%
        left_join(province_pop, by = c("year" = "year","bplsn" = "bplsn","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(senegal_province, by = "bplsn") %>%
        filter(bplsn != 999) %>%
        filter(md_density < 1)


# another medical school opening date (2008)
senegal_clean <- senegal %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2008 - birthyr)

summary(senegal_clean$birthyr)
table(senegal_clean$year)
summary(senegal_clean$age_at_opening)

province_pop <- senegal_clean %>%
        group_by(year, bplsn, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- senegal_clean %>%
        group_by(year,bplsn,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 222 | occ == 221) %>%
        left_join(province_pop, by = c("year" = "year","bplsn" = "bplsn","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(senegal_province, by = "bplsn") %>%
        filter(bplsn != 999) %>%
        filter(md_density < 1)


# Uganda
#####################################################################################################################################################################

# Import data
uganda <- read_dta("uganda.dta")
uganda_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "uganda")

# Med Sch opened in 1949, 1989, 1996, 2003, 2004, 2013 (two med sch), 2014 (two med sch), 2016, 2017, & 2018. 
#MD occupation available in 1991 & 2002 data, code 224,225
# Does not have the birth year variable, so I created it from scratch 
summary(uganda$birthyr)
table(uganda$year)
summary(uganda$age)

# for 1949 medical school opening (one medical school opened in 1949)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1949 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)


# for 1989 medical school opening (one medical school opened in 1989)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1989 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)


# for 1996 medical school opening (one medical school opened in 1996)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 1996 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)



# for 2003 medical school opening (one medical school opened in 2003)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2003 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)


# for 2004 medical school opening (one medical school opened in 2004)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2004 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)


# for 2013 medical school opening (two medical schools opened in 2013)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2013 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)


# for 2014 medical school opening (two medical schools opened in 2014)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2014 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)


# for 2016 medical school opening (one medical school opened in 2016)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2016 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)


# for 2017 medical school opening (one medical school opened in 2017)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2017 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)


# for 2018 medical school opening (one medical school opened in 2018)
uganda_clean <- uganda %>%
        filter(age != 999) %>%
        mutate(birthyr = year-age,
               age_at_opening = 2018 - birthyr)

summary(uganda_clean$birthyr)
table(uganda_clean$year)
summary(uganda_clean$age_at_opening)

province_pop <- uganda_clean %>%
        group_by(year, bplug, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- uganda_clean %>%
        group_by(year,bplug,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 224 | occ == 225) %>%
        left_join(province_pop, by = c("year" = "year","bplug" = "bplug","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(uganda_province, by = "bplug") %>%
        filter(bplug != 998 | bplug != 999) %>%
        filter(md_density < 1)


# Zambia
#####################################################################################################################################################################
zambia <- read_dta("zambia.dta")
zambia_province <- read_xlsx("birthplace_ipums.xlsx", sheet = "zambia")

# Medical School opening in 1966, MD occupation available in 1990, 2000, 2010 data , code 061 and 222
summary(zambia$year)
summary(zambia$age) # age available in all individuals
# Does not have the birth year variable, so I created it from scratch 

# For 1966 opening: 
zambia_clean <- zambia %>%
        mutate(birthyr = year-age,
               age_at_opening = 1966 - birthyr)

summary(zambia_clean$birthyr)
summary(zambia_clean$year)
summary(zambia_clean$age_at_opening)

province_pop <- zambia_clean %>%
        group_by(year, bplzm, age_at_opening) %>%
        dplyr::summarise(province_population = n())

province_occ_pop <- zambia_clean %>%
        group_by(year,bplzm,occ,age_at_opening) %>%
        dplyr::summarise(md_population = n()) %>%
        filter(occ == 61 | occ == 222) %>%
        left_join(province_pop, by = c("year" = "year", "bplzm" = "bplzm","age_at_opening" = "age_at_opening")) %>%
        mutate(md_density = md_population/province_population) %>%
        left_join(zambia_province, by = "bplzm") %>%
        filter(bplzm != 9998 & bplzm != 9999) %>%
        filter(md_density < 1)
