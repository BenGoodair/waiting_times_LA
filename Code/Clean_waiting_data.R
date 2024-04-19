if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo, stringr, patchwork)

####2010####

#file_vector <- list.files(path = "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015")
#file_vector %>% head()


csv_list <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/Raw_Data/2010/csvs/xl_list.csv"))[2]

df_incomplete <- data.frame()

for (i in c(1,3,4,15,18)) {
  waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/",csv_list$x[i],".csv", sep=""))
  if (i== 1) {
    df_incomplete <- waits
  } else {
    df_incomplete <- rbind(df_incomplete, waits)
  }
  paste(i)
}
waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/",csv_list$x[10],".csv", sep=""))
waits <- waits %>% dplyr::mutate(Total.within.18.weeks = (X.17.18+X.11.12+X.5.6+
                                                   X.16.17+X.10.11+X.0.1+
                                                   X.15.16+X.9.10+X.4.5+
                                                   X.14.15+X.8.9+X.3.4+
                                                   X.13.14+X.7.8+X.2.3+
                                                   X.12.13+X.6.7+X.1.2),
                                 X..within.18.weeks = Total.within.18.weeks/Total.number.of.incomplete.pathways)

df_incomplete <- rbind(df_incomplete, waits)





df_admitted <- data.frame()

for (i in c(5,8,9,11,14)) {
  waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/", csv_list$x[i],".csv", sep=""))
  if (nrow(df_admitted) == 0) {
    df_admitted <- waits
  } else {
    df_admitted <- rbind(df_admitted, waits)
  }
}

waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/",csv_list$x[17],".csv", sep=""))
waits <- waits %>% dplyr::mutate(Total..with.a.known.clock.start..within.18.weeks = (X.17.18+X.11.12+X.5.6+
                                                            X.16.17+X.10.11+X.0.1+
                                                            X.15.16+X.9.10+X.4.5+
                                                            X.14.15+X.8.9+X.3.4+
                                                            X.13.14+X.7.8+X.2.3+
                                                            X.12.13+X.6.7+X.1.2),
                                 X..within.18.weeks = Total..with.a.known.clock.start..within.18.weeks/Total.number.of.completed.pathways..with.a.known.clock.start.)

df_admitted <- rbind(df_admitted, waits)





df_non_admitted <- data.frame()

for (i in c(2,6,7,12,13)) {
  waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/", csv_list$x[i],".csv", sep=""))
  if (nrow(df_non_admitted) == 0) {
    df_non_admitted <- waits
  } else {
    df_non_admitted <- rbind(df_non_admitted, waits)
  }
}

waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2010/csvs/",csv_list$x[16],".csv", sep=""))
waits <- waits %>% dplyr::mutate(Total..with.a.known.clock.start..within.18.weeks = (X.17.18+X.11.12+X.5.6+
                                                                                       X.16.17+X.10.11+X.0.1+
                                                                                       X.15.16+X.9.10+X.4.5+
                                                                                       X.14.15+X.8.9+X.3.4+
                                                                                       X.13.14+X.7.8+X.2.3+
                                                                                       X.12.13+X.6.7+X.1.2),
                                 X..within.18.weeks = Total..with.a.known.clock.start..within.18.weeks/Total.number.of.completed.pathways..with.a.known.clock.start.)

df_non_admitted <- rbind(df_non_admitted, waits)

df_admitted <- df_admitted %>%dplyr::filter(Treatment.Function=="Total")%>%
  dplyr::select(PCT.Code,PCT.Name,
                Total.number.of.completed.pathways..with.a.known.clock.start.,
                Total..with.a.known.clock.start..within.18.weeks,X..within.18.weeks,
                X..within.18.weeks)%>%
  dplyr::group_by(PCT.Code,PCT.Name)%>%
  dplyr::summarise(   Total.number.of.completed.pathways..with.a.known.clock.start.= mean(Total.number.of.completed.pathways..with.a.known.clock.start.),
                   Total..with.a.known.clock.start..within.18.weeks = mean(Total..with.a.known.clock.start..within.18.weeks),
                   X..within.18.weeks = mean(X..within.18.weeks))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(type="admitted", year=2010)%>%
  dplyr::rename(Total_pathways = Total.number.of.completed.pathways..with.a.known.clock.start.,
                Total_pathways_within_18_weeks = Total..with.a.known.clock.start..within.18.weeks)

df_non_admitted <- df_non_admitted %>%dplyr::filter(Treatment.Function=="Total")%>%
  dplyr::select(PCT.Code,PCT.Name,
                Total.number.of.completed.pathways..with.a.known.clock.start.,
                Total..with.a.known.clock.start..within.18.weeks,X..within.18.weeks,
                X..within.18.weeks)%>%
  dplyr::group_by(PCT.Code,PCT.Name)%>%
  dplyr::summarise(Total.number.of.completed.pathways..with.a.known.clock.start.= mean(Total.number.of.completed.pathways..with.a.known.clock.start.),
                   Total..with.a.known.clock.start..within.18.weeks = mean(Total..with.a.known.clock.start..within.18.weeks),
                   X..within.18.weeks = mean(X..within.18.weeks))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(type="non_admitted",
                year=2010)%>%
  dplyr::rename(Total_pathways = Total.number.of.completed.pathways..with.a.known.clock.start.,
                Total_pathways_within_18_weeks = Total..with.a.known.clock.start..within.18.weeks)


df_incomplete <- df_incomplete %>%dplyr::filter(Treatment.Function=="Total")%>%
  dplyr::select(PCT.Code,PCT.Name,
                Total.number.of.incomplete.pathways,
                Total.within.18.weeks,
                X..within.18.weeks)%>%
  dplyr::group_by(PCT.Code,PCT.Name)%>%
  dplyr::summarise(Total.number.of.incomplete.pathways = mean(Total.number.of.incomplete.pathways),
                   Total.within.18.weeks= mean(Total.within.18.weeks),
                   X..within.18.weeks = mean(X..within.18.weeks))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(type="incomplete",
                year=2010)%>%
  dplyr::rename(Total_pathways = Total.number.of.incomplete.pathways,
                Total_pathways_within_18_weeks = Total.within.18.weeks)

df_2010 <- rbind(df_incomplete, df_admitted, df_non_admitted)


matrix_lookup <- read.csv("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/look_ups/PCT_CCG_matrix.csv")

matrix_lookup[matrix_lookup=='0%'] <- NA

matrix_lookup <-matrix_lookup %>% 
  pivot_longer(
    cols = !PCT_Code, 
    names_to = "CCG_Code", 
    values_to = "lookup"
  )

matrix_lookup <- matrix_lookup[complete.cases(matrix_lookup),]

names(matrix_lookup)[names(matrix_lookup)=="PCT_Code"] <- "PCT.Code"

yes <- matrix_lookup %>%
  dplyr::mutate(lookup = gsub('[[:punct:] ]+','',lookup))%>%
  dplyr::select(CCG_Code, lookup)%>%
  dplyr::group_by(CCG_Code)%>%
  dplyr::summarise(lookup=sum(as.numeric(lookup)))%>%
  dplyr::ungroup()%>%
  dplyr::rename(total_look = lookup)%>%
  dplyr::left_join(., matrix_lookup,by="CCG_Code")%>%
  dplyr::mutate(lookup = gsub('[[:punct:] ]+','',lookup),
                actual_look = as.numeric(lookup)/total_look)
  

yes$CCG_Code <-  sub('.', '', yes$CCG_Code)




er <- yes %>% dplyr::select(CCG_Code,actual_look) %>%
  dplyr::distinct()%>%
  dplyr::group_by(CCG_Code)%>%
  dplyr::summarise(actual_look=sum(as.numeric(actual_look)))%>%
  dplyr::mutate(keep = ifelse(actual_look==1, 1,0))

yes <- merge(yes, er[c("CCG_Code", "keep")],by="CCG_Code" )%>%
  dplyr::filter(keep==1)



df_2010 <- merge(df_2010, yes, by="PCT.Code", all=T)



df_2010$actual_look <- as.numeric(df_2010$actual_look)


df_2010 <- df_2010 %>%
  dplyr::mutate(Total_pathways = Total_pathways*(actual_look),
                Total_pathways_within_18_weeks = Total_pathways_within_18_weeks*(actual_look),
                X..within.18.weeks = X..within.18.weeks*(actual_look))%>%
  dplyr::select(-PCT.Code,-PCT.Name,-lookup, -actual_look,-total_look)%>%
  dplyr::group_by(type,year,CCG_Code)%>%
  dplyr::summarise(Total_pathways=sum(Total_pathways),
                   Total_pathways_within_18_weeks = sum(Total_pathways_within_18_weeks),
                   X..within.18.weeks = sum(X..within.18.weeks))%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(Total_pathways))


la_lookup <- read.csv("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/look_ups/LSOAtoCCGtoLAD19.csv")


la_lookup <- la_lookup%>%dplyr::select(CCG19CDH, LAD19CD, LAD19NM)%>%
  dplyr::rename(CCG_Code = CCG19CDH)%>%
  dplyr::distinct()

df_2010 <- merge(df_2010, la_lookup, by= "CCG_Code",all=T)




df_2010 <- df_2010 %>% dplyr::select(LAD19CD, LAD19NM, X..within.18.weeks, Total_pathways,Total_pathways_within_18_weeks)%>%
  dplyr::group_by(LAD19CD, LAD19NM)%>%
  dplyr::summarise(X..within.18.weeks = mean(X..within.18.weeks, na.rm=T),
                   Total_pathways_within_18_weeks = mean(Total_pathways_within_18_weeks, na.rm=T),
                   Total_pathways = mean(Total_pathways, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(election_year=2010)


#####2015#####



csv_list <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/Raw_Data/2015/csvs/xl_list.csv"))[2]

df_incomplete <- data.frame()

for (i in c(7:12)) {
  waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015/csvs/",csv_list$x[i],".csv", sep=""))[-c(2)]
  if (i== 7) {
    df_incomplete <- waits
  } else {
    df_incomplete <- rbind(df_incomplete, waits)
  }
  paste(i)
}



df_admitted <- data.frame()

for (i in c(1:6)) {
  waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015/csvs/", csv_list$x[i],".csv", sep=""))[-c(2)]
  if (nrow(df_admitted) == 0) {
    df_admitted <- waits
  } else {
    df_admitted <- rbind(df_admitted, waits)
  }
}





df_non_admitted <- data.frame()

for (i in c(13:18)) {
  waits <- read.csv(paste("C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015/csvs/", csv_list$x[i],".csv", sep=""))[-c(2)]
  if (nrow(df_non_admitted) == 0) {
    df_non_admitted <- waits
  } else {
    df_non_admitted <- rbind(df_non_admitted, waits)
  }
}

df_admitted <- df_admitted %>%dplyr::filter(Treatment.Function=="Total")%>%
  dplyr::select(CCG.Code,CCG.Name, Average..median..waiting.time..in.weeks.,
                Total.number.of.completed.pathways..with.a.known.clock.start.,
                Total..with.a.known.clock.start..within.18.weeks,X..within.18.weeks,
                X..within.18.weeks)%>%
  dplyr::group_by(CCG.Code,CCG.Name)%>%
  dplyr::summarise(   Total.number.of.completed.pathways..with.a.known.clock.start.= mean(as.numeric(Total.number.of.completed.pathways..with.a.known.clock.start.)),
                      Total..with.a.known.clock.start..within.18.weeks = mean(as.numeric(Total..with.a.known.clock.start..within.18.weeks)),
                      X..within.18.weeks = mean(as.numeric(X..within.18.weeks)),
                      Average..median..waiting.time..in.weeks. = mean(as.numeric(Average..median..waiting.time..in.weeks.)))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(type="admitted", year=2015)%>%
  dplyr::rename(Total_pathways = Total.number.of.completed.pathways..with.a.known.clock.start.,
                Total_pathways_within_18_weeks = Total..with.a.known.clock.start..within.18.weeks)

df_non_admitted <- df_non_admitted %>%dplyr::filter(Treatment.Function=="Total")%>%
  dplyr::select(CCG.Code,CCG.Name, Average..median..waiting.time..in.weeks.,
                Total.number.of.completed.pathways..with.a.known.clock.start.,
                Total..with.a.known.clock.start..within.18.weeks,X..within.18.weeks,
                X..within.18.weeks)%>%
  dplyr::group_by(CCG.Code,CCG.Name)%>%
  dplyr::summarise(   Total.number.of.completed.pathways..with.a.known.clock.start.= mean(as.numeric(Total.number.of.completed.pathways..with.a.known.clock.start.)),
                      Total..with.a.known.clock.start..within.18.weeks = mean(as.numeric(Total..with.a.known.clock.start..within.18.weeks)),
                      X..within.18.weeks = mean(as.numeric(X..within.18.weeks)),
                      Average..median..waiting.time..in.weeks. = mean(as.numeric(Average..median..waiting.time..in.weeks.)))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(type="non_admitted", year=2015)%>%
  dplyr::rename(Total_pathways = Total.number.of.completed.pathways..with.a.known.clock.start.,
                Total_pathways_within_18_weeks = Total..with.a.known.clock.start..within.18.weeks)

df_incomplete <- df_incomplete %>%dplyr::filter(Treatment.Function=="Total")%>%
  dplyr::select(CCG.Code,CCG.Name, Average..median..waiting.time..in.weeks.,
                Total.number.of.incomplete.pathways,
                X..within.18.weeks,
                Total.within.18.weeks)%>%
  dplyr::group_by(CCG.Code,CCG.Name)%>%
  dplyr::summarise(   Total.number.of.incomplete.pathways= mean(as.numeric(Total.number.of.incomplete.pathways)),
                      X..within.18.weeks = mean(as.numeric(X..within.18.weeks)),
                      Total.within.18.weeks = mean(as.numeric(Total.within.18.weeks)),
                      Average..median..waiting.time..in.weeks. = mean(as.numeric(Average..median..waiting.time..in.weeks.)))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(type="incomplete", year=2015)%>%
  dplyr::rename(Total_pathways = Total.number.of.incomplete.pathways,
                Total_pathways_within_18_weeks = Total.within.18.weeks)

df_2015 <- rbind(df_incomplete, df_admitted, df_non_admitted)





la_lookup <- read.csv("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/look_ups/LSOAtoCCGtoLAD19.csv")


la_lookup <- la_lookup%>%dplyr::select(CCG19CDH, LAD19CD, LAD19NM)%>%
  dplyr::rename(CCG.Code = CCG19CDH)%>%
  dplyr::distinct()

df_2015 <- merge(df_2015, la_lookup, by= "CCG.Code",all=T)




df_2015 <- df_2015 %>% dplyr::select(LAD19CD, LAD19NM, X..within.18.weeks, Total_pathways,Total_pathways_within_18_weeks,Average..median..waiting.time..in.weeks.)%>%
  dplyr::group_by(LAD19CD, LAD19NM)%>%
  dplyr::summarise(X..within.18.weeks = mean(X..within.18.weeks, na.rm=T),
                   Total_pathways_within_18_weeks = mean(Total_pathways_within_18_weeks, na.rm=T),
                   Total_pathways = mean(Total_pathways, na.rm=T),
                   Average..median..waiting.time..in.weeks. = mean(Average..median..waiting.time..in.weeks., na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(election_year=2015)



####2017####


df_2017 <- rbind(
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/Raw_Data/2017/18wksRTT%20-%20Monthly%20EXTRACTS%20-%20May17%20revised.csv"), skip=2),
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/Raw_Data/2017/18wksRTT%20-%20Monthly%20EXTRACTS%20-%20Apr17%20revised.csv"), skip=2),
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/Raw_Data/2017/18wksRTT%20-%20Monthly%20EXTRACTS%20-%20Mar17%20revised.csv"), skip=2),
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/Raw_Data/2017/18wksRTT%20-%20Monthly%20EXTRACTS%20-%20Feb17%20revised.csv"), skip=2),
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/Raw_Data/2017/18wksRTT%20-%20Monthly%20EXTRACTS%20-%20Jan17%20revised.csv"), skip=2),
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/waiting_times_LA/main/Data/Raw_Data/2017/18wksRTT%20-%20Monthly%20EXTRACTS%20-%20Jun17%20revised.csv"), skip=2),
  
  )%>%
  dplyr::select(`Commissioner Org Code`, `Commissioner Org Name`,`RTT Part Description`, 
                `Treatment Function Name`,  )%>%
  dplyr::filter(RTT Part Description=="Total")%>%
  
















benefits_sum_year <- benefits_sum_year %>%dplyr::select(CCG_Name, year,  deflated_per_person_benefits)

benefits_sum_year$CCG_Name <-  gsub('&','and',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  gsub('[[:punct:] ]+',' ',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  gsub('NHS ','',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  gsub('CCG','',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  str_trim(benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  toupper(benefits_sum_year$CCG_Name)

benefits_sum_year <- aggregate(.~CCG_Name+year,data=benefits_sum_year, mean)





