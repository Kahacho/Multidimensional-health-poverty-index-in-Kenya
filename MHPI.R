# Clearing the memory
rm(list = ls()); graphics.off()

setwd("C:\\Users\\admin\\Desktop\\Mwenda\\KIHBIS2015\\Data")

if(!require("haven")) install.packages("haven")
library(haven)
library(dplyr)

# Loading the data
# From questionnaire 1A
HH_Members_Information <- read_dta("HH_Members_Information.dta")
# From questionnaire Q1B
HH_Information <- read_dta("HH_Information.dta")

#View(head(HH_Members_Information, 20))
#View(head(HH_Information, 20))

# Viewing the column names
colnames(HH_Members_Information)
colnames(HH_Information) 

#Checking dimensions
dim(HH_Members_Information)
dim(HH_Information)

# Checking the cluster ID and household id
# a)  for household member information 
HH_Members_Information %>%
  select(clid) %>%
  distinct() %>%
  arrange(clid)

HH_Members_Information %>%
  select(clid) %>%
  n_distinct()

# b) for household information
HH_Information %>%
  select(clid) %>%
  distinct() %>%
  arrange(clid)

HH_Information %>%
  select(clid) %>%
  n_distinct()

# Checking the household ID 
# a)  for household member information 
HH_Members_Information %>%
  select(hhid) %>%
  distinct() %>%
  arrange(hhid)

HH_Members_Information %>%
  select(hhid) %>%
  n_distinct()

# b) for household information
HH_Information %>%
  select(hhid) %>%
  distinct() %>%
  arrange(hhid)

HH_Information %>%
  select(hhid) %>%
  n_distinct()

# Grouping the data frames by cluster id and household id
# HH_Members_Information
HH_Members_Information <- HH_Members_Information %>% 
  group_by(clid, hhid) %>% 
  arrange(clid, hhid)

# HH_Information
HH_Information <- HH_Information %>%
  group_by(clid, hhid) %>%
  arrange(clid, hhid)

# Checking the number of unique households per cluster
HH_Members_Information %>%
  select(clid, hhid) %>%
  n_distinct()

HH_Information %>%
  select(clid, hhid) %>%
  n_distinct()

#View(HH_Members_Information)
#View(HH_Information)

# Creating my dataframe of interest from the HH_Members_Information
colnames(HH_Members_Information)
df_1 <- select(HH_Members_Information, clid, hhid, b05_mm, b05_yy, e02, e05, e07,
               f03, f04, f05, f07, f08, f09, f12, f13, f18, f20)
summary(df_1)
df_1[is.na(df_1)] <- 0 #Replace all NA values with 0's

head(df_1)
dim(df_1)


glimpse(df_1)
head(df_1, 15)

# Changing the data type
df_1 <- df_1 %>%
  mutate_at(c(3:17), as.numeric)

glimpse(df_1)
summary(df_1)

# The transform function to get information about a household from household
# members information

transform_func <- function(df){
  # Creating a dataframe with clid and hhid columns
  clid_hhid_df <- select(HH_Members_Information, clid, hhid)
  
  # Creating a third column and assigning it the vector classifying an individual
  # as deprived or not deprived.
  clid_hhid_df['d'] <- df
  
  # grouping the dataframe into distinct clid and hhid and then counting the
  # number of groups (deprived and not deprived) in each unique clid and hhid.
  clid_hhid_df <- clid_hhid_df %>%
    group_by(clid, hhid) %>%
    distinct() %>%
    mutate(d_count = n())
  
  # Creating another vector that classifies the unique households where there is
  # a deprived case as 1 and 0 otherwise.
  uniq <- ifelse((clid_hhid_df$d_count == 2 | clid_hhid_df$d_count == 1) & clid_hhid_df$d ==
                   1, 1, 0)
  
  # Assigning the uniq vector to the dataframe
  clid_hhid_df['uniq'] <- uniq
  
  # Filtering households where there is a deprived case(takes care of households
  # where the is only individual in a house)
  deprived_case <- filter(clid_hhid_df, d == 1, uniq == 1)
  
  # Filter households where there is no deprived case (if the d_count == 2, means
  # the household has a deprived case, but it's taken care of by the deprived_case
  # filter. The d_count will come in handy to get rid of the duplicate cases.)
  no_deprive_case <- filter(clid_hhid_df, d == 0 & d_count == 1)
  
  # Merging the filtered dataframes
  clid_hhid_df <- rbind(deprived_case, no_deprive_case)
  
  # Rearranging the merged dataframe in ascending order
  clid_hhid_df <- clid_hhid_df %>%
    arrange(clid, hhid)
  
  # renaming the uniq column
  clid_hhid_df <- rename(clid_hhid_df, deprived = uniq)
  
  # creating a deprived dataframe with columns of interest
  deprived_df <- select(clid_hhid_df, clid, hhid, deprived)
  
  return(deprived_df)
  
}

#filter(a, clid == 1, hhid == 1)
#filter(a, clid == 278, hhid == 7) # household with more than one member
#filter(a, clid == 6, hhid == 2) #Household with only one member

# Dimension 1
# Indicator 1 - Consult a doctor during sickness
d1 <- as_tibble(ifelse(df_1$e02 == 2 | df_1$e02 == 0  | (df_1$e02 == 1 & (df_1$e05 == 1 | 
                                                                            df_1$e05 == 2 | df_1$e07 == 1)), 0, 1))
d1
sum(d1 == 1) #Deprived
sum(d1 == 0) #Not deprived
head(d1)
dim(d1)
dim(df_1)

I_d1 <- transform_func(d1) #Indicator d1
I_d1
sum(I_d1$deprived == 1) # deprived households
sum(I_d1$deprived == 0) # not deprived households

#I_d1[I_d1$clid == 2200 & I_d1$hhid == 3, ]
#select(filter(HH_Members_Information, clid == 2200, hhid == 3), e02, e05, e07)


# Indicator 2 - Assisted delivery
d2 <- as_tibble(ifelse(df_1$f04 == 0 | (df_1$f04 == 1 | df_1$f04 == 2 | df_1$f04 == 4), 0, 1))
d2
sum(d2 == 1)
sum(d2 == 0)
head(d2)
dim(d2)
which(d2 == 1)

I_d2 <- transform_func(d2) #Indicator d2
I_d2
sum(I_d2$deprived == 1)
sum(I_d2$deprived == 0)

#I_d2[I_d2$clid == 2200 & I_d2$hhid == 3, ]
#select(filter(HH_Members_Information, clid == 2200, hhid == 3), f04, f03)

# Indicator 3 - Institutional delivery
d3 <- as_tibble(ifelse(df_1$f03 == 0 | (df_1$f03 == 1 | df_1$f03 == 2 | df_1$f03 == 3 | df_1$f03 
                                        == 4), 0, 1))
sum(d3 == 1)
sum(d3 == 0)
head(d3)
dim(d3)

I_d3 <- transform_func(d3) #Indicator d3
I_d3
sum(I_d3$deprived == 1)
sum(I_d3$deprived == 0)

#I_d3[I_d3$clid == 888 & I_d3$hhid == 7, ]
#select(filter(HH_Members_Information, clid == 888, hhid == 7), f03, f04)

# Indicator 4 - Immunization
# a) Measles
d4 <- as_tibble(ifelse(df_1$f18 == 0 | df_1$f18 == 1 | ((df_1$f18 == 2 | 
                                                           df_1$f18 == 3) & df_1$f20 == 1), 0, 1))
sum(d4 == 1)
sum(d4 == 0)

I_d4 <- transform_func(d4) #Indicator d4
I_d4
sum(I_d4$deprived == 1)
sum(I_d4$deprived == 0)

I_d4[I_d4$clid == 99 & I_d4$hhid == 6, ]
select(filter(HH_Members_Information, clid == 99, hhid == 6), b05_mm, b05_yy, f13, f18, f20)

# b) Growth monitoring
df_1
d5 <- as_tibble(ifelse(df_1$f13 == 0 | df_1$f13 == 1, 0, 1))
sum(d5 == 1)
sum(d5 == 0)
head(d5)
dim(d5)

I_d5 <- transform_func(d5) #Indicator 5
sum(I_d5$deprived == 1)
sum(I_d5$deprived == 0)

select(filter(HH_Members_Information, clid == 1112, hhid == 4), b05_mm, b05_yy, f13, f18, f20)
I_d5[I_d5$clid == 1112 & I_d5$hhid == 2, ]


#########
# NEXT  #
#########
colnames(HH_Information)
df_2 <- select(HH_Information, clid, hhid, county, j01_dr, j02 ,j08_1, j08_2, j08_3, j10,
               j11, j13)
glimpse(df_2)
summary(df_2)

df_2[is.na(df_2)] <- 0 #Replace all NA values with 0's

summary(df_2)

glimpse(df_2)

# Changing data types
df_2 <- df_2 %>%
  mutate_at(c(4, 5, 9, 10, 11), as.numeric)

glimpse(df_2)
df_2

# Transform function - 2
transform_func_2 <- function(df){
  desired_df <- select(df_2, clid, hhid, county)
  desired_df['d'] <- df
  return(desired_df)
}

# Improved drinking water
# Indicator 6 - Improved drinking water without necessarily making it safe to drink
d6 <- ifelse((df_2$j01_dr == 1 | df_2$j01_dr == 2 | df_2$j01_dr == 3 | 
                df_2$j01_dr == 4 | df_2$j01_dr == 5 | df_2$j01_dr == 7 |
                df_2$j01_dr == 9 | df_2$j01_dr == 14), 0, 1)
sum(d6 == 1)
sum(d6 == 0)

colnames(df_2)
I_d6 <- transform_func_2(d6) #Indicator 6
I_d6
sum(I_d6$d == 1)
sum(I_d6$d == 0)


# Improved drinking water after making it safe through boiling or adding
# a) Indicator 10 - Boiling
#d10 <- ifelse((df_2$j01_dr == 6 | df_2$j01_dr == 8 | df_2$j01_dr == 10 |
#             df_2$j01_dr == 11 | df_2$j01_dr == 12 | df_2$j01_dr == 13 |
#             df_2$j01_dr == 96) & (df_2$j08_1 == "B" | df_2$j08_2 == "B" | 
#            df_2$j08_3 == "B"), 0, 1)
#sum(d10 == 1)
#sum(d10 == 0)

#I_d10 <- transform_func_2(d10)
#I_d10
#sum(I_d10$d == 1)
#sum(I_d10$d == 0)


# b) Indicator 11 - Bleach or chlorine
#d11 <- ifelse((df_2$j01_dr == 6 | df_2$j01_dr == 8 | df_2$j01_dr == 10 |
#                df_2$j01_dr == 11 | df_2$j01_dr == 12 | df_2$j01_dr == 13 |
#                df_2$j01_dr == 96) & (df_2$j08_1 == "C" | df_2$j08_2 == "C" | 
#                df_2$j08_3 == "C") , 0, 1)
#sum(d11 == 1)
#sum(d11 == 0)

#I_d11 <- transform_func_2(d11) # Indicator 11
#I_d11
#sum(I_d11$d == 1)
#sum(I_d11$d == 0)

# Indicator 12 - Time cost to get drinking water
d7 <- ifelse(df_2$j02 <= 30, 0, 1)
sum(d7 == 1)
sum(d7 == 0)

I_d7 <- transform_func_2(d7) #Indicator 12
I_d7
sum(I_d7$d == 1)
sum(I_d7$d == 0)

# Sanitation
# Adequate/ Improved human waste disposal 
# a) Indicator 13 - Improved/adequate sanitation

d8 <- ifelse((df_2$j10 == 11 | df_2$j10 == 12 | df_2$j10 == 13 | df_2$j10 == 21 |  
                df_2$j10 == 22 | df_2$j10 == 31), 0, 1)
sum(d8 == 1)
sum(d8 == 0)

I_d8 <- transform_func_2(d8)
I_d8
sum(I_d8$d == 1)

###########
## NEXT ##
##########

first_cutoff <- cbind(I_d6[,c("county", "clid", "hhid")], I_d1$deprived , I_d2$deprived ,
                      I_d3$deprived, I_d4$deprived, I_d5$deprived, I_d6$d, I_d7$d,
                      I_d8$d)

colnames(first_cutoff)[4:11] <- c("IND_1", "IND_2", "IND_3", "IND_4", "IND_5",
                                  "IND_6", "IND_7", "IND_8")

first_cutoff

weights <- matrix(c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1, 0.2), ncol = 1)
weights

class(first_cutoff[,4:11])

first_cutoff['count'] <- apply(as.matrix(first_cutoff[,4:11])%*%weights, 1, sum)
first_cutoff

unique(first_cutoff$county) #All the counties

#################
# DUAL CUT-OFF #
################
k = 0.25 # k = 35%

first_cutoff['dual_cutoff'] <- ifelse(first_cutoff$count >= k, 1, 0)
first_cutoff


##################
# COUNTRY LEVEL #
#################

# Headcount Ratio In the country
q <- sum(first_cutoff$dual_cutoff)
q

n <- nrow(first_cutoff)
n

H <- q/n
H

# Censored vector of deprivation count per individual
c_ik <- first_cutoff$dual_cutoff * first_cutoff$count 
c_ik

# Average deprivation share across the poor
d <- 8
A <- sum(c_ik)/(q * d)
A

# Adjusted Headcount Ratio
M_0 <- H * A
M_0

################
# COUNTY LEVEL #
################

county_df <- first_cutoff %>% 
  group_by(county) %>%
  arrange() %>%
  summarise(c_county = mean (count), q_county = sum(dual_cutoff), n_county = n()) %>%
  mutate(H_county = q_county / n_county, c_ik_county = q_county * c_county, 
         A_county = sum(c_ik_county)/(q_county * d), M0_county = H_county * A_county)

county_df

first_cutoff
q_county$
  
  n_county  <- first_cutoff %>%
  group_by(county) %>%
  summarise(n())
n_county

county_df

sum(county_df$`sum(dual_cutoff)`)
sum(n_county$n())