if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools, dplyr, tidyverse, tidyr, stringr,  curl, plm, readxl, zoo, stringr, patchwork)

#2010

file_vector <- list.files(path = "C:/Users/benjamin.goodair/OneDrive - Nexus365/Documents/GitHub/waiting_times_LA/Data/Raw_Data/2015")
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

matrix_lookup$CCG_Code <-  sub('.', '', matrix_lookup$CCG_Code)


df_2010 <- merge(df_2010, matrix_lookup, by="PCT.Code", all=T)


df_2010$lookup <- gsub('[[:punct:] ]+','',df_2010$lookup)

df_2010$lookup <- as.numeric(df_2010$lookup)

er <- df_2010 %>% dplyr::select(CCG_Code,lookup) %>%
  dplyr::distinct()%>%
  dplyr::group_by(CCG_Code)%>%
  dplyr::summarise(lookup=sum(as.numeric(lookup)))


df_2010 <- df_2010 %>%
  dplyr::mutate(Total_pathways = Total_pathways*(lookup/100),
                Total_pathways_within_18_weeks = Total_pathways_within_18_weeks*(lookup/100),
                X..within.18.weeks = X..within.18.weeks*(lookup/100))%>%
  dplyr::select(-PCT.Code,-PCT.Name,-lookup)%>%
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

df_2010 <- df2010 %>%

benefits_sum_year <- benefits_sum_year %>%dplyr::select(CCG_Name, year,  deflated_per_person_benefits)

benefits_sum_year$CCG_Name <-  gsub('&','and',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  gsub('[[:punct:] ]+',' ',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  gsub('NHS ','',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  gsub('CCG','',benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  str_trim(benefits_sum_year$CCG_Name)
benefits_sum_year$CCG_Name <-  toupper(benefits_sum_year$CCG_Name)

benefits_sum_year <- aggregate(.~CCG_Name+year,data=benefits_sum_year, mean)





