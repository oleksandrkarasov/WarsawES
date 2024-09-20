library("xlsx")
library("dplyr")
install.packages("igraph")
# Open data
getwd() # get working directory
setwd("C:\\Users\\karas\\Documents\\ES data")
who_df <- read.xlsx(file.choose(), 1)  # read first sheet
what_where_df <- read.xlsx(file.choose(), 1)  # read first sheet

what_df <- read.xlsx(file.choose(), 1)  # read first sheet

# Explore data
str(who_df)

# Gender
who_df <- who_df %>%
  mutate(GENDER = ifelse(WOMEN == 1, "WOMEN", 
                         ifelse(MEN == 1, "MEN", 
                                       "NA")))
# Age
who_df <- who_df %>%
  mutate(AGE = ifelse(age16_24 == 1, "16_24", 
                         ifelse(age25_34 == 1, "25_34", 
                                ifelse(age35_44 == 1, "35_44", 
                                       ifelse(age45_54 == 1, "45_54", 
                                              ifelse(age55_64 == 1, "55_64",
                                                     ifelse(age65_74 == 1, "65_74",
                                                            ifelse(age.74 == 1, ">74",
                                       "NA"))))))))

# Work situation
who_df <- who_df %>%
  mutate(WORK_SITUATION = ifelse(WORKING == 1, "WORKING", 
                      ifelse(IN_EDUCATION == 1, "IN_EDUCATION", 
                             ifelse(UNEMPLOYMENT == 1, "UNEMPLOYED", 
                                    ifelse(DISABLED == 1, "DISABLED", 
                                           ifelse(REITRED == 1, "RETIRED",
                                                  ifelse(HOUSE_WORK == 1, "HOUSE_WORK",
                                                                "NA")))))))
who_df$CHILDHOOD_PLACE
str(who_df)

# Childhood place
who_df <- who_df %>%
  mutate(CHILDHOOD_PLACE = ifelse(CITY == 1, "CITY", 
                      ifelse(COUNTRYSIDE == 1, "COUNTRYSIDE", 
                             ifelse(GARDEN == 1, "GARDEN", 
                                    ifelse(FOREST_NAT == 1, "FOREST", 
                                           ifelse(SEASIDE == 1, "SEASIDE",
                                                  ifelse(LAKE_RIVER_STREAM == 1, "FRESH_WATER",
                                                         ifelse(MOUNTAINS == 1, "MOUNTAINS",
                                                                "NA"))))))))

# Visitation frequency
who_df <- who_df %>%
  mutate(VISITING_FREQ = ifelse(EVERYDAY == 1, "EVERYDAY", 
                                  ifelse(FEW_TIMES_WEEK == 1, "FEW_TIMES_WEEK", 
                                         ifelse(ONCE_A_WEEK == 1, "ONCE_A_WEEK", 
                                                ifelse(FEW_TIMES_LAST_6_MONTHS == 1, "FEW_TIMES_LAST_6_MONTHS", 
                                                       ifelse(NEVER == 1, "NEVER",
                                                                            "NA"))))))
who_df$ACCESSIBILITY

# Accessibility
who_df <- who_df %>%
  mutate(ACCESSIBILITY = ifelse(PRIVATE_GARDEN == 1, "PRIVATE_GARDEN", 
                                ifelse(COMMUNITY_GARDEN == 1, "COMMUNITY_GARDEN", 
                                       ifelse(BALCONY_PATIO == 1, "BALCONY_PATIO", 
                                              ifelse(NON == 1, "NONE",
                                                            "NA")))))
# Select columns
who_df <- who_df %>% select(Session_ID,Language,PROFFESION_ECO,GENDER,
                            AGE,WORK_SITUATION,CHILDHOOD_PLACE,VISITING_FREQ,
                            ACCESSIBILITY,COVID,NATURE_RELATION)

# All columns to factor
cols <- c("Session_ID","Language","PROFFESION_ECO","GENDER",
          "AGE","WORK_SITUATION","CHILDHOOD_PLACE","VISITING_FREQ",
          "ACCESSIBILITY","COVID","NATURE_RELATION")
who_df <- who_df %>% mutate_at(cols, factor)
str(who_df)

###############################################################################
# Select columns for WHAT_WHERE
what_where_df <- what_where_df %>% select(Session_ID,FOOD_WATER,CLEAN_WATER,
                                CLEAN_AIR,NOISE_RED,TEMP_REG,EXTR_WEATHER,
                                FLOOD_REG,NAT_CONNECT,RECREATION,SOCIAL_BOND,
                                RELAX,SAFETY,KNOWLEDGE,SPIRI_IDENT_SYMBOL,
                                AESTHETIC,BIODIVE,ATTACHMENT,LON,LAT,fid_2,
                                TYPE_1)
# All columns to factor
cols <- c("Session_ID","FOOD_WATER","CLEAN_WATER",
          "CLEAN_AIR","NOISE_RED","TEMP_REG","EXTR_WEATHER",
          "FLOOD_REG","NAT_CONNECT","RECREATION","SOCIAL_BOND",
          "RELAX","SAFETY","KNOWLEDGE","SPIRI_IDENT_SYMBOL",
          "AESTHETIC","BIODIVE","ATTACHMENT","LON","LAT","nazwa",
          "TYPE_1")
what_where_df <- what_where_df %>% mutate_at(cols, factor)

# Session_ID to character
what_df$WATER_VALUE <- as.numeric(what_df$WATER_VALUE)
what_df$Session_ID <- as.character(what_df$Session_ID)
what_where_df$Session_ID <- as.character(what_where_df$Session_ID)
who_df$Session_ID <- as.character(who_df$Session_ID)

str(what_where_df)
str(who_df)

###############################################################################
# Join data
c1<-unique(as.character(what_where_df$Session_ID))
c<-unique(as.character(who_df$Session_ID))
setdiff(c, c1)

unique(who_df$Session_ID)

what_where_who_df <- inner_join(what_where_df, who_df, by = "Session_ID")
write.csv(what_where_who_df,"what_where_who_df1.csv", row.names = FALSE)


###############################################################################
# Average values per one Session_ID
what_df2<-what_df %>% 
  group_by(Session_ID)%>%
  mutate(WATER_VALUE=mean(WATER_VALUE, na.rm = TRUE),
         FOOD_WATER=mean(FOOD_WATER, na.rm = TRUE),
         CLEAN_WATER=mean(CLEAN_WATER, na.rm = TRUE),
         CLEAN_AIR=mean(CLEAN_AIR, na.rm = TRUE),
         NOISE_RED=mean(NOISE_RED, na.rm = TRUE),
         TEMP_REG=mean(TEMP_REG, na.rm = TRUE),
         EXTR_WEATHER=mean(EXTR_WEATHER, na.rm = TRUE),
         FLOOD_REG=mean(FLOOD_REG, na.rm = TRUE),
         NAT_CONNECT=mean(NAT_CONNECT, na.rm = TRUE),
         RECREATION=mean(RECREATION, na.rm = TRUE),
         SOCIAL_BOND=mean(SOCIAL_BOND, na.rm = TRUE),
         RELAX=mean(RELAX, na.rm = TRUE),
         SAFETY=mean(SAFETY, na.rm = TRUE),
         SPIRI_IDENT_SYMBOL=mean(SPIRI_IDENT_SYMBOL, na.rm = TRUE),
         KNOWLEDGE=mean(KNOWLEDGE, na.rm = TRUE),
         AESTHETIC=mean(AESTHETIC, na.rm = TRUE),
         BIODIVE=mean(BIODIVE, na.rm = TRUE),
         ATTACHMENT=mean(ATTACHMENT, na.rm = TRUE))%>%
  ungroup()

# Remove duplicates of mean values per user_id (Session_ID)
what_df3 <- what_df2[!duplicated(what_df2$Session_ID), ]

# Remove duplicates of user_ids (Session_ID)
who_df2 <- who_df[!duplicated(who_df$Session_ID), ]


# Select columns
what_df3 <- what_df3 %>% select(Session_ID,WATER_VALUE,FOOD_WATER,CLEAN_WATER,CLEAN_AIR,NOISE_RED,
                                TEMP_REG,EXTR_WEATHER,FLOOD_REG,NAT_CONNECT,RECREATION,
                                SOCIAL_BOND,RELAX,SAFETY,KNOWLEDGE,SPIRI_IDENT_SYMBOL,
                                AESTHETIC,BIODIVE,ATTACHMENT)

###############################################################################
# Median values per one Session_ID
what_df_med <-what_df %>% 
  group_by(Session_ID)%>%
  mutate(WATER_VALUE=median(WATER_VALUE, na.rm = TRUE),
         FOOD_WATER=median(FOOD_WATER, na.rm = TRUE),
         CLEAN_WATER=median(CLEAN_WATER, na.rm = TRUE),
         CLEAN_AIR=median(CLEAN_AIR, na.rm = TRUE),
         NOISE_RED=median(NOISE_RED, na.rm = TRUE),
         TEMP_REG=median(TEMP_REG, na.rm = TRUE),
         EXTR_WEATHER=median(EXTR_WEATHER, na.rm = TRUE),
         FLOOD_REG=median(FLOOD_REG, na.rm = TRUE),
         NAT_CONNECT=median(NAT_CONNECT, na.rm = TRUE),
         RECREATION=median(RECREATION, na.rm = TRUE),
         SOCIAL_BOND=median(SOCIAL_BOND, na.rm = TRUE),
         RELAX=median(RELAX, na.rm = TRUE),
         SAFETY=median(SAFETY, na.rm = TRUE),
         SPIRI_IDENT_SYMBOL=median(SPIRI_IDENT_SYMBOL, na.rm = TRUE),
         KNOWLEDGE=median(KNOWLEDGE, na.rm = TRUE),
         AESTHETIC=median(AESTHETIC, na.rm = TRUE),
         BIODIVE=median(BIODIVE, na.rm = TRUE),
         ATTACHMENT=median(ATTACHMENT, na.rm = TRUE))%>%
  ungroup()

what_df_med2 <- what_df_med[!duplicated(what_df_med$Session_ID), ]
what_df_med3 <- what_df_med2 %>% select(Session_ID,WATER_VALUE,FOOD_WATER,CLEAN_WATER,CLEAN_AIR,NOISE_RED,
                                TEMP_REG,EXTR_WEATHER,FLOOD_REG,NAT_CONNECT,RECREATION,
                                SOCIAL_BOND,RELAX,SAFETY,KNOWLEDGE,SPIRI_IDENT_SYMBOL,
                                AESTHETIC,BIODIVE,ATTACHMENT)




what_who_df_mean <- inner_join(what_df3, who_df2, by = "Session_ID")
what_who_df_med <- inner_join(what_df_med3, who_df2, by = "Session_ID")

write.csv(df,"Warsaw_median.csv", row.names = TRUE)
write.csv(what_df,"what_df.csv", row.names = TRUE)
write.csv(where_df,"where_df.csv", row.names = TRUE)
write.csv(who_df,"who_df.csv", row.names = TRUE)
write.csv(what_who_df_mean,"what_who_df_mean.csv", row.names = TRUE)
write.csv(what_who_df_med,"what_who_df_med.csv", row.names = TRUE)

###############################################################################
# Average values per one Session_ID (ses) and place (obj)
what_df_ses_obj_mean <-what_df %>% 
  group_by(SESSION_OBJECT)%>%
  mutate(WATER_VALUE=mean(WATER_VALUE, na.rm = TRUE),
         FOOD_WATER=mean(FOOD_WATER, na.rm = TRUE),
         CLEAN_WATER=mean(CLEAN_WATER, na.rm = TRUE),
         CLEAN_AIR=mean(CLEAN_AIR, na.rm = TRUE),
         NOISE_RED=mean(NOISE_RED, na.rm = TRUE),
         TEMP_REG=mean(TEMP_REG, na.rm = TRUE),
         EXTR_WEATHER=mean(EXTR_WEATHER, na.rm = TRUE),
         FLOOD_REG=mean(FLOOD_REG, na.rm = TRUE),
         NAT_CONNECT=mean(NAT_CONNECT, na.rm = TRUE),
         RECREATION=mean(RECREATION, na.rm = TRUE),
         SOCIAL_BOND=mean(SOCIAL_BOND, na.rm = TRUE),
         RELAX=mean(RELAX, na.rm = TRUE),
         SAFETY=mean(SAFETY, na.rm = TRUE),
         SPIRI_IDENT_SYMBOL=mean(SPIRI_IDENT_SYMBOL, na.rm = TRUE),
         KNOWLEDGE=mean(KNOWLEDGE, na.rm = TRUE),
         AESTHETIC=mean(AESTHETIC, na.rm = TRUE),
         BIODIVE=mean(BIODIVE, na.rm = TRUE),
         ATTACHMENT=mean(ATTACHMENT, na.rm = TRUE))%>%
  ungroup()
what_df_ses_obj_mean_nodup <- what_df_ses_obj_mean[!duplicated(what_df_ses_obj_mean$SESSION_OBJECT), ]


# Remove duplicates in user_place combinations
where_df2 <- where_df[!duplicated(where_df$SESSION_OBJECT), ]


# Select columns
what_df_ses_obj_mean_nodup2 <- what_df_ses_obj_mean_nodup %>% select(SESSION_OBJECT,WATER_VALUE,FOOD_WATER,CLEAN_WATER,CLEAN_AIR,NOISE_RED,
                                TEMP_REG,EXTR_WEATHER,FLOOD_REG,NAT_CONNECT,RECREATION,
                                SOCIAL_BOND,RELAX,SAFETY,KNOWLEDGE,SPIRI_IDENT_SYMBOL,
                                AESTHETIC,BIODIVE,ATTACHMENT)
what_where_df_mean <- inner_join(what_df_ses_obj_mean_nodup2, where_df2, by = "SESSION_OBJECT")

###############################################################################
# Median values per one Session_ID (ses) and place (obj)
what_df_ses_obj_med <-what_df %>% 
  group_by(SESSION_OBJECT)%>%
  mutate(WATER_VALUE=median(WATER_VALUE, na.rm = TRUE),
         FOOD_WATER=median(FOOD_WATER, na.rm = TRUE),
         CLEAN_WATER=median(CLEAN_WATER, na.rm = TRUE),
         CLEAN_AIR=median(CLEAN_AIR, na.rm = TRUE),
         NOISE_RED=median(NOISE_RED, na.rm = TRUE),
         TEMP_REG=median(TEMP_REG, na.rm = TRUE),
         EXTR_WEATHER=median(EXTR_WEATHER, na.rm = TRUE),
         FLOOD_REG=median(FLOOD_REG, na.rm = TRUE),
         NAT_CONNECT=median(NAT_CONNECT, na.rm = TRUE),
         RECREATION=median(RECREATION, na.rm = TRUE),
         SOCIAL_BOND=median(SOCIAL_BOND, na.rm = TRUE),
         RELAX=median(RELAX, na.rm = TRUE),
         SAFETY=median(SAFETY, na.rm = TRUE),
         SPIRI_IDENT_SYMBOL=median(SPIRI_IDENT_SYMBOL, na.rm = TRUE),
         KNOWLEDGE=median(KNOWLEDGE, na.rm = TRUE),
         AESTHETIC=median(AESTHETIC, na.rm = TRUE),
         BIODIVE=median(BIODIVE, na.rm = TRUE),
         ATTACHMENT=median(ATTACHMENT, na.rm = TRUE))%>%
  ungroup()
what_df_ses_obj_med_nodup <- what_df_ses_obj_med[!duplicated(what_df_ses_obj_med$SESSION_OBJECT), ]
# Select columns
what_df_ses_obj_med_nodup2 <- what_df_ses_obj_med_nodup %>% select(SESSION_OBJECT,WATER_VALUE,FOOD_WATER,CLEAN_WATER,CLEAN_AIR,NOISE_RED,
                                                                     TEMP_REG,EXTR_WEATHER,FLOOD_REG,NAT_CONNECT,RECREATION,
                                                                     SOCIAL_BOND,RELAX,SAFETY,KNOWLEDGE,SPIRI_IDENT_SYMBOL,
                                                                     AESTHETIC,BIODIVE,ATTACHMENT)
what_where_df_med <- inner_join(what_df_ses_obj_med_nodup2, where_df2, by = "SESSION_OBJECT")
write.csv(corr_matrix,"corr_matrix.csv", row.names = TRUE)
write.csv(what_where_df_mean,"what_where_df_mean.csv", row.names = TRUE)

