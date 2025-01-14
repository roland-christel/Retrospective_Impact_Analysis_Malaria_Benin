#################################### Completude ###############################


#Packages

if(!require("readxl")){install.packages("readxl")}
if(!require("stringr")){install.packages("stringr")}
if(!require("stringi")){install.packages("stringi")}

if(!require("chirps")){install.packages("chirps")}
if(!require("sf")){install.packages("sf")}
if(!require("rgeoboundaries")){install.packages("rgeoboundaries")}
if(!require("FNN")){install.packages("FNN")}
if(!require("tidyverse")){install.packages("tidyverse")}



#Data Importation 
data <- read_xlsx("processedData/COMPLETUDE_PNLP1_BENIN.xlsx", sheet = "data")
head(data, 10)


data <-  data %>% 
  filter(
    year != 2024
  ) %>% 
  mutate(
    HZs = case_when(
      HZs %in% c("Cotonou 1-Cotonou 4", "Cotonou 5", "Cotonou 2-Cotonou 3", "Cotonou 6") ~ "Cotonou",
      TRUE ~ HZs
    )
  )

#We could see that the maximum of actual report is 2 instead of 1.
summary(data)

#We suspect a reporting error and will change every 2 with 1.
data$actualReports <- as.numeric(gsub(2, 1, data$actualReports) )
summary(data)

# Completeness and Timeliness of data: Overall reporting rate
data$reportingrate <- data$actualReports/data$expectedReports*100  

data %>% 
  mutate(reportingrate  = case_when(is.na(reportingrate) ~ 0,
                                    TRUE ~ reportingrate)) %>% 
  mutate(cat_RR = ifelse(
    reportingrate < 25, '0-25', 
    ifelse(reportingrate >25 & reportingrate < 75, '25-75', '75-100'))) %>%  group_by(cat_RR) %>%
  summarise(cat_RR_N = n()) %>% 
  mutate(
    percent = cat_RR_N/sum(cat_RR_N), 
    label = paste0(cat_RR_N, " (", round(percent * 100, 1), "%)")
  ) %>% 
  ggplot(., aes(x = cat_RR, y = cat_RR_N, fill = cat_RR)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label = label), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("lightgreen", "darkgreen")) +
  labs(
    x = "Reporting Rate",
    y = "# of Health Facilities",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )


# reporting rate per HFs 
data %>% 
  mutate(reportingrate  = case_when(is.na(reportingrate) ~ 0,
                                    TRUE ~ reportingrate)) %>% 
  mutate(cat_RR = ifelse(
    reportingrate < 25, '0-25', 
    ifelse(reportingrate >25 & reportingrate < 75, '25-75', '75-100'))) %>%  
  group_by(HZs, cat_RR) %>%
  summarise(cat_RR_N = n(), .groups = "drop") %>% 
  mutate(
    percent = cat_RR_N/sum(cat_RR_N), 
    label = paste0(cat_RR_N, " (",round(percent * 100, 1), "%)")) %>% 
  ggplot(., aes(x = cat_RR, y = cat_RR_N, fill = cat_RR)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label = label), vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("lightgreen", "darkgreen")) +
  labs(
    x = "Reporting Rate",
    y = "# of Health Facilities",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) +
  facet_wrap(~HZs)

# Correcting naming issues, HFs from differents HZs having same names.
# I corrected adding the initials of the HZ at the end of their names
data <- data %>%
  mutate(HFs = ifelse(HFs == "CS Zounzonme" & HZs == "Bohicon-Za-kpota-Zogbodomey", 
                      "CS Zounzonme_BZZ", 
                      HFs),
         HFs = ifelse(HFs == "CS Partago" & HZs == "Bassila (ZS)", 
                      "CS Partago_Bassila", 
                      HFs),
         HFs = ifelse(HFs == "CS Deme" & HZs == "Bohicon-Za-kpota-Zogbodomey", 
                      "CS Deme_BZZ", 
                      HFs)
  )


# Eliminate facilities with >3 months of consecutive missing month data ???

data$monthYear <-  str_replace_all(data$monthYear,
                                   c("Janvier" = "Jan", 
                                     "Février" = "Feb", 
                                     "Mars" = "Mar", 
                                     "Avril" = "Apr", 
                                     "Mai" = "May", 
                                     "Juin" = "Jun",
                                     "Juillet" = "Jul", 
                                     "Août" = "Aug", 
                                     "Septembre" = "Sep", 
                                     "Octobre" = "Oct", 
                                     "Novembre" = "Nov", 
                                     "Décembre" = "Dec"))

data <- data |> 
  mutate_at(c(2:6), as.factor) |>
  mutate(monthYear = my(monthYear), 
         expectedXactual = expectedReports*actualReports,
         type = case_when(
           is.na(expectedXactual) ~ "Missing",
           expectedXactual == 1 ~ "Reported",
           expectedXactual == 0 ~ "Not Reported",
           TRUE ~ ""
         ))


## function for the plot 
createTimeline <- function(data) {
  # Ensure data has columns: district, date, status
  ggplot(data,
         aes(x = monthYear, y = HFs)) +
    # Create colored rectangles for each status
    geom_tile(aes(fill = type,height=0.5)) +
    # Set color scheme
    scale_fill_manual(
      values = c(
        "Reported" = "#00FF00",      # Green
        "Missing" = "#FF9999",       # Light red/pink
        "Not Reported" = "#FFD700"        # Yellow
      )
    ) +
    # Customize theme
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, size = 6),
      axis.text.y = element_text(size = 5, margin = unit(c(0, 0, 0.2, 0), "cm")),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 20),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.3, "cm")
    ) +
    # Set labels
    labs(
      x = "Time Period",
      y = "District",
      fill = "Status"
    ) +
    # Ensure districts are in the correct order
    scale_y_discrete(expand = expansion(add = .2))+
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                 expand = c(0, 0) )
  
}
d     <- unique(data$admin1)
plots <- list()

for(i in 1:length(d)){
  dat <- data |> 
    filter(admin1 == d[i])
  
  p <- createTimeline(dat)
  
  plots[[i]] <- p
  
  names(plots)[i] <- as.character(d[i])
  }

# show plots per admin 1 
plots$LITTORAL

#we are expecting 84 reports per facility (12*7)
  # let's show  the distribution of report percentages by health zone in Benin
  # Note : Most health zones have high reporting rates (>75%); Best Performances (high percentage with little variation)=N'Dali-Parakou;Dassa-Glazoué;Cotonou 6
  # More Variable Performances (wider boxes, indicating greater variability in reporting rates):Kouandé-Péhunco-Kérou, Banikoara (HZ) and Malanville-Karimama
  # Several health zones have isolated red points (•) at the bottom of the graph, indicating facilities with reporting rates significantly lower than their zone average

facility_reports <- data %>%
  group_by(HZs, HFs) %>%
  summarise(Reports = sum(as.numeric(expectedXactual), na.rm = TRUE)) %>% 
  mutate(
    percentage = Reports/84*100
  )

ggplot(facility_reports, aes(x = reorder(HZs, percentage), y = percentage)) +
  geom_boxplot(fill = "lightblue",outlier.color = "red", outlier.size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Report Percentages by Health Zone",
    x = "HZs",
    y = "Percentage (%)",
    caption = "Note: Isolated points, indicating facilities with reporting rates significantly lower than their zone average"
  )  +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    #panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))


facility_reports_year <- data %>%
                              group_by(HZs, HFs,year) %>%
                              summarise(Reports = sum(as.numeric(expectedXactual), na.rm = TRUE)) %>% 
                              mutate(
                                percentage = ifelse(year !=2024 ,Reports/12*100, Reports/9*100)
                              )

ggplot(facility_reports_year, aes(x = reorder(HZs, percentage), y = percentage)) +
  geom_boxplot(fill = "lightblue",outlier.color = "red", outlier.size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Report Percentages by Health Zone",
    x = "HZs",
    y = "Percentage (%)",
    caption = "Note: Isolated points, indicating facilities with reporting rates significantly lower than their zone average"
  )  +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    #panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))+ 
  facet_grid(~as.factor(year), scales = "free_y")


# percentage of facilities with reporting rates greater than or equal to 
# the corresponding threshold vs  threshold number of reports 
  # percentage of facilities meeting or exceeding
  # the reporting thresholds gradually decreases as the threshold number of reports increases
  # At least 85% of facilities submitted 90 or more reports. 
  # Reporting compliance decreases more significantly as the threshold nears the maximum value.

# Number of facilities that have submitted at least each unique value

unique_reports <- sort(unique(facility_reports$Reports))

result <- data.frame(
  Reports = unique_reports,
  Facilities = sapply(unique_reports, function(x) sum(facility_reports$Reports >= x))
)

result <- result %>% 
  mutate(
    percent = Facilities / length(unique(data$HFs)) * 100
  )

ggplot(result, aes(x = Reports, y = percent)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = c(33, 57, 90), linetype = "dashed") +
  annotate("text", x = 34.5, y = 97, label = "95%", hjust = 0) +
  annotate("text", x = 58.5, y = 93, label = "90%", hjust = 0) +
  annotate("text", x = 91.4, y = 86, label = "85%", hjust = 0) +
  scale_y_continuous(name = " ", limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_x_continuous(name = "Facility-Monthly Reports", breaks = seq(0, 100, 10)) +
  theme_minimal() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    text = element_text(size = 12)
  )


########################## CHIRPS Data (Precipitation) ################################

# Get Benin boundary
benin <- geoboundaries("Benin", adm_lvl = 2)

# Convert to points (for example, using centroids of admin regions)
benin_points <- st_centroid(benin) %>%
  st_coordinates() %>%
  as.data.frame() %>% 
  mutate(name = benin$shapeName )
colnames(benin_points) <- c("lon", "lat", "name"='districts')


coordsBenin <- matrix(c(benin_points$lon, benin_points$lat), ncol = 2)

# Get CHIRPS data for these points
benin_chirps_precipitation <- get_chirps(coordsBenin, 
                                dates = c("2017-01-01","2023-12-31"),
                                server = "ClimateSERV")

#save(benin_chirps_precipitation, file = 'benin_chirps_precipitation.RData')
#load("benin_chirps_precipitation.RData")
source('cds_data_function.R')

# Set your credentials
user <- "f70d75b3-62cf-4aef-8e7d-0418f0bc97fc"
key <- "832e3be5-c912-443a-ae25-5c4737f86ea6"

# Create empty list to store results
all_data <- list()

# Loop through years
for(year in 2017:2023) {
  cat("Downloading data for year:", year, "\n")

  climate_data <- get_cds(
    key = key,
    user = user,
    year = year,
    month = sprintf("%02d", 1:12),  # This gets all 12 months
    what = coordsBenin
  ) %>%
    mutate( `2m_temperature` = `2m_temperature` - 273.15)  # Convert temperature to Celsius

  all_data[[as.character(year)]] <- climate_data
}

# Combine all years into one dataframe
final_data <- bind_rows(all_data)

# Optional: Sort by date
benin_cds_precipi_temp <- final_data %>%
  arrange(date)
#save(benin_cds_precipi_temp, file = 'benin_cds_precipi_temp.RData')
#load("benin_cds_precipi_temp.RData")

# adding district names to climatic data 

data_preci_temp <- benin_cds_precipi_temp %>%
  mutate(date= as.Date(date)) %>% 
  left_join(benin_points, by = c('latitude' = 'lat', 'longitude' = 'lon')) 
  #left_join(benin_chirps_precipitation,  
            #by=c('date'='date', 'latitude'='lat')) 


# Imputation of missing temparature values 
coordinates <- data_preci_temp[!is.na(data_preci_temp$`2m_temperature`), c("longitude", "latitude")]
values <- data_preci_temp[!is.na(data_preci_temp$`2m_temperature`), "2m_temperature"]
missing_coords <- data_preci_temp[is.na(data_preci_temp$`2m_temperature`), c("longitude", "latitude")]

  ## Find nearest neighbors
nn <- get.knnx(coordinates, missing_coords, k = 3)
  ## Calculate weighted mean of nearest neighbors
values <- as.vector(values$`2m_temperature`)
imputed_values <- apply(nn$nn.index, 1, function(idx) mean(values[idx]))  
data_preci_temp$`2m_temperature`[is.na(data_preci_temp$`2m_temperature`)] <- imputed_values


# Imputation of missing temparature values 
coordinates <- data_preci_temp[!is.na(data_preci_temp$total_precipitation), c("longitude", "latitude")]
values <- data_preci_temp[!is.na(data_preci_temp$total_precipitation), "total_precipitation"]
missing_coords <- data_preci_temp[is.na(data_preci_temp$total_precipitation), c("longitude", "latitude")]

## Find nearest neighbors
nn <- get.knnx(coordinates, missing_coords, k = 3)
## Calculate weighted mean of nearest neighbors
values <- as.vector(values$total_precipitation)
imputed_values <- apply(nn$nn.index, 1, function(idx) mean(values[idx]))  
data_preci_temp$total_precipitation[is.na(data_preci_temp$total_precipitation)] <- imputed_values



data_preci_temp <- data_preci_temp %>% 
                                  rename(#precip_chirps  = chirps,
                                         temp_cds       = `2m_temperature`,
                                         precip_cds     = total_precipitation) %>% 
                                  select(longitude, latitude, districts, date, 
                                         #precip_chirps, 
                                         temp_cds,precip_cds)
Sys.setlocale("LC_TIME", "fr_FR.UTF-8") 
data_preci_temp <-  data_preci_temp %>% 
                                   mutate(
                                      year = year(date),     # Extraire l'année
                                      month = month(date),    # Extraire le mois
                                      month_start = as.Date(paste0(year(date), "-", sprintf("%02d", month(date)), "-01"))  # Premier jour du mois
                                    ) %>%
                                    group_by(month_start, districts) %>%
                                    summarise(
                                      #precip_chirps_avg = mean(precip_chirps, na.rm = TRUE),  # Moyenne des précipitations (CHIRPS)
                                      temp_cds_avg = mean(temp_cds, na.rm = TRUE),           # Moyenne de la température (CDS)
                                      precip_cds_sum = sum(precip_cds, na.rm = TRUE)         # Somme des précipitations (CDS)
                                    ) %>%
                                    ungroup() %>% 
                                    mutate(
                                      period = str_to_title(format(month_start, "%B %Y"))
                                    )


source('grouping_HZs_function.R')                  
data_preci_temp_HZs <- group_districts(data_preci_temp)



# data_preci_temp_HZs_monthly_data <- data_preci_temp_HZs %>%
#   mutate(
#     year = year(date),     # Extraire l'année
#     month = month(date),    # Extraire le mois
#     month_start = as.Date(paste0(year(date), "-", sprintf("%02d", month(date)), "-01"))  # Premier jour du mois
#   ) %>%
#   group_by(month_start, district_group) %>%
#   summarise(
#     #precip_chirps_avg = mean(precip_chirps, na.rm = TRUE),  # Moyenne des précipitations (CHIRPS)
#     temp_cds_avg = mean(temp_cds, na.rm = TRUE),           # Moyenne de la température (CDS)
#     precip_cds_sum = sum(precip_cds, na.rm = TRUE)         # Somme des précipitations (CDS)
#   ) %>%
#   ungroup() %>% 
#   mutate(
#     period = str_to_title(format(month_start, "%B %Y"))
#   )


dataCases <-  read_xlsx('processedData/malariaCases.xlsx')

dataCases$periodname_date <- as.Date(paste("01", dataCases$periodname), format = "%d %B %Y")

dataCases$year <- format(dataCases$periodname_date, "%Y")

dataCases <-  dataCases %>% 
  filter(
    year != 2024
  ) %>% 
  mutate(
    orgunitlevel3 = case_when(
      orgunitlevel3 %in% c("Cotonou 1-Cotonou 4", "Cotonou 5", "Cotonou 2-Cotonou 3", "Cotonou 6") ~ "Cotonou",
      TRUE ~ orgunitlevel3
    )
  )


dataCases <- dataCases %>% 
          group_by(orgunitlevel3,periodname) %>% 
          summarise(
            plus5= sum(plus5, na.rm = T),
            moins5=sum(moins5,na.rm = T)
          )

data <- data %>% 
  mutate(
    period = str_to_title(format(monthYear, "%B %Y"))
  )

data_with_reported_counts <- data %>%
  group_by(year, monthYear, HZs) %>%  # Grouper par année, mois et HZs
  summarise(
    meanReportingrate_HZs = mean(reportingrate, na.rm=TRUE),
    total_HFs = n(),    
    reported_HFs = sum(actualReports == 1, na.rm= TRUE)  # Compter les HFs avec actualReports == 1
  ) %>%
  ungroup()


dataFinal <- left_join(dataCases, data_preci_temp_HZs, 
                      by= c('orgunitlevel3'='district_group','periodname'='period')
                     ) %>% 
             left_join(data_with_reported_counts,
                      by= c('orgunitlevel3'='HZs', 'month_start'='monthYear')
                      ) %>% 
             rename(
               monthyear   = periodname,
               date       = month_start,
               HZs         = orgunitlevel3,
               casesOver5  = plus5,
               casesUnder5 = moins5
               
             ) %>% 
             relocate(date, monthyear, year)



# Adding population data
popData <- read.csv('processedData/popData.csv',fileEncoding = "latin1")

popData <- popData %>% 
        mutate(
          
          ZS = gsub("\\s+$", "", ZS),
          ZS = gsub('Porto-Novo-Sèmè-Kpodji-Aguégués', "Porto-Novo-Sèmè-kpodji-Aguégués", ZS)
        )


popData <-  popData %>% 
       group_by(ZS, Year) %>% 
       summarise(
         Total = sum(Total, na.rm = T),
         Totalu5 = sum(Totalu5, na.rm = T)
       )

dataFinal <- left_join(dataFinal, popData, 
                by= c('year'='Year', 'HZs'='ZS'))



dataFinal <- dataFinal %>%
  mutate(
    SMC = case_when(
      year == 2019 & HZs %in% c("Malanville-Karimama", "Tanguiéta-Cobly-Matéri") ~ 1,
      year == 2020 & HZs %in% c("Malanville-Karimama", "Tanguiéta-Cobly-Matéri", "Kandi-Gogounou-Ségbana", "Banikoara (ZS)") ~ 1,
      year == 2021 & HZs %in% c("Malanville-Karimama", "Tanguiéta-Cobly-Matéri", "Kandi-Gogounou-Ségbana", "Banikoara (ZS)", 
                                "Natitingou-Boukoumbé-Toucountouna", "Kouandé-Péhunco-Kérou") ~ 1,
      year == 2022 & HZs %in% c("Malanville-Karimama", "Tanguiéta-Cobly-Matéri", "Kandi-Gogounou-Ségbana", "Banikoara (ZS)", 
                                "Natitingou-Boukoumbé-Toucountouna", "Kouandé-Péhunco-Kérou") ~ 1,
      year == 2023 & HZs %in% c("Malanville-Karimama", "Tanguiéta-Cobly-Matéri", "Kandi-Gogounou-Ségbana", "Banikoara (ZS)", 
                                "Natitingou-Boukoumbé-Toucountouna", "Kouandé-Péhunco-Kérou") ~ 1,
      TRUE ~ 0
    ),
    ITN_Cov = case_when(
      year == 2017 & HZs == "Abomey-Calavi-So-ava" ~ 94.06,
      year == 2017 & HZs == "Adjohoun-Bonou-Dangbo" ~ 99.14,
      year == 2017 & HZs == "Akpro-missérété-Avrankou-Adjarra" ~ 95.78,
      year == 2017 & HZs == "Allada-Toffo-Zè" ~ 97.95,
      year == 2017 & HZs == "Aplahoué-Djakotomey-Dogbo" ~ 99.16,
      year == 2017 & HZs == "Banikoara (ZS)" ~ 107.98,
      year == 2017 & HZs == "Bassila (ZS)" ~ 99.62,
      year == 2017 & HZs == "Bembèrèkè-Sinendé" ~ 98.54,
      year == 2017 & HZs == "Bohicon-Za-kpota-Zogbodomey" ~ 96.74,
      year == 2017 & HZs == "Comè-Grand popo-Houèyogbé-Bopa" ~ 98.96,
      year == 2017 & HZs == "Cotonou" ~ 90.45,
      year == 2017 & HZs == "Covè-Zagnanado-Ouinhi" ~ 98.56,
      year == 2017 & HZs == "Dassa-Glazoué" ~ 98.28,
      year == 2017 & HZs == "Djidja-Abomey-Agbangnizoun" ~ 96.74,
      year == 2017 & HZs == "Djougou-Ouaké-Copargo" ~ 97.59,
      year == 2017 & HZs == "Kandi-Gogounou-Ségbana" ~ 99.43,
      year == 2017 & HZs == "Klouékamè-Toviklin-Lalo" ~ 99.57,
      year == 2017 & HZs == "Kouandé-Péhunco-Kérou" ~ 96.21,
      year == 2017 & HZs == "Lokossa-Athiémè" ~ 98.50,
      year == 2017 & HZs == "Malanville-Karimama" ~ 99.60,
      year == 2017 & HZs == "N'Dali-Parakou" ~ 96.74,
      year == 2017 & HZs == "Natitingou-Boukoumbé-Toucountouna" ~ 97.39,
      year == 2017 & HZs == "Nikki-Kalalé-Pèrèrè" ~ 98.53,
      year == 2017 & HZs == "Ouidah-Kpomassè-Tori-Bossito" ~ 94.96,
      year == 2017 & HZs == "Pobè-Kétou-Adja-ouèrè" ~ 98.48,
      year == 2017 & HZs == "Porto-Novo-Sèmè-kpodji-Aguégués" ~ 92.94,
      year == 2017 & HZs == "Sakété-Ifangni" ~ 96.84,
      year == 2017 & HZs == "Savalou-Bantè" ~ 97.90,
      year == 2017 & HZs == "Savè-Ouèssè" ~ 97.00,
      year == 2017 & HZs == "Tanguiéta-Cobly-Matéri" ~ 98.57,
      year == 2017 & HZs == "Tchaourou (ZS)" ~ 97.86,
      
      
      year == 2020 & HZs == "Abomey-Calavi-So-ava" ~ 218688/254808*100,
      year == 2020 & HZs == "Adjohoun-Bonou-Dangbo" ~ 66073/68666*100,
      year == 2020 & HZs == "Akpro-missérété-Avrankou-Adjarra" ~ 123161/131792*100,
      year == 2020 & HZs == "Allada-Toffo-Zè" ~ 110550/116870*100,
      year == 2020 & HZs == "Aplahoué-Djakotomey-Dogbo" ~ 132014/138847*100,
      year == 2020 & HZs == "Banikoara (ZS)" ~ 89702/93189*100,
      year == 2020 & HZs == "Bassila (ZS)" ~ 44793/47243*100,
      year == 2020 & HZs == "Bembèrèkè-Sinendé" ~ 74149/76933*100,
      year == 2020 & HZs == "Bohicon-Za-kpota-Zogbodomey" ~ 140641/149351*100,
      year == 2020 & HZs == "Comè-Grand popo-Houèyogbé-Bopa" ~ 104522/109387*100,
      year == 2020 & HZs == "Cotonou" ~ (31793+65167+56514+55033)/(62493+62612+73182+35489)*100,
      year == 2020 & HZs == "Covè-Zagnanado-Ouinhi" ~ 54039/56020*100,
      year == 2020 & HZs == "Dassa-Glazoué" ~ 76467/81359*100,
      year == 2020 & HZs == "Djidja-Abomey-Agbangnizoun" ~ 100809/107404*100,
      year == 2020 & HZs == "Djougou-Ouaké-Copargo" ~ 136852/148195*100,
      year == 2020 & HZs == "Kandi-Gogounou-Ségbana" ~ 135431/141617*100,
      year == 2020 & HZs == "Klouékamè-Toviklin-Lalo" ~ 98648/103457*100,
      year == 2020 & HZs == "Kouandé-Péhunco-Kérou" ~ 99181/103806*100,
      year == 2020 & HZs == "Lokossa-Athiémè" ~ 50872/53492*100,
      year == 2020 & HZs == "Malanville-Karimama" ~ 70569/73422*100,
      year == 2020 & HZs == "N'Dali-Parakou" ~ 125483/138212*100,
      year == 2020 & HZs == "Natitingou-Boukoumbé-Toucountouna" ~ 72910/77733*100,
      year == 2020 & HZs == "Nikki-Kalalé-Pèrèrè" ~ 142095/146994*100,
      year == 2020 & HZs == "Ouidah-Kpomassè-Tori-Bossito" ~ 83594/90031*100,
      year == 2020 & HZs == "Pobè-Kétou-Adja-ouèrè" ~ 129968/139885*100,
      year == 2020 & HZs == "Porto-Novo-Sèmè-kpodji-Aguégués" ~ 179126/194606*100,
      year == 2020 & HZs == "Sakété-Ifangni" ~ 67088/72375*100,
      year == 2020 & HZs == "Savalou-Bantè" ~ 81438/86734*100,
      year == 2020 & HZs == "Savè-Ouèssè" ~ 66476/70777*100,
      year == 2020 & HZs == "Tanguiéta-Cobly-Matéri" ~ 78878/81883*100,
      year == 2020 & HZs == "Tchaourou (ZS)" ~ 77535/82743*100,
      
      
      year == 2023 & HZs == "Abomey-Calavi-So-ava" ~ 89.51,
      year == 2023 & HZs == "Adjohoun-Bonou-Dangbo" ~ 91.27,
      year == 2023 & HZs == "Akpro-missérété-Avrankou-Adjarra" ~ 91.27,
      year == 2023 & HZs == "Allada-Toffo-Zè" ~ 89.51,
      year == 2023 & HZs == "Aplahoué-Djakotomey-Dogbo" ~ 95.21,
      year == 2023 & HZs == "Banikoara (ZS)" ~ 94.58,
      year == 2023 & HZs == "Bassila (ZS)" ~ 91.53,
      year == 2023 & HZs == "Bembèrèkè-Sinendé" ~ 93.91,
      year == 2023 & HZs == "Bohicon-Za-kpota-Zogbodomey" ~ 92.85,
      year == 2023 & HZs == "Comè-Grand popo-Houèyogbé-Bopa" ~ 94.35,
      year == 2023 & HZs == "Cotonou" ~ 85.54,
      year == 2023 & HZs == "Covè-Zagnanado-Ouinhi" ~ 92.85,
      year == 2023 & HZs == "Dassa-Glazoué" ~ 93.92,
      year == 2023 & HZs == "Djidja-Abomey-Agbangnizoun" ~ 92.85,
      year == 2023 & HZs == "Djougou-Ouaké-Copargo" ~ 91.53,
      year == 2023 & HZs == "Kandi-Gogounou-Ségbana" ~ 94.58,
      year == 2023 & HZs == "Klouékamè-Toviklin-Lalo" ~ 95.21,
      year == 2023 & HZs == "Kouandé-Péhunco-Kérou" ~ 95.09,
      year == 2023 & HZs == "Lokossa-Athiémè" ~ 94.35,
      year == 2023 & HZs == "Malanville-Karimama" ~ 94.58,
      year == 2023 & HZs == "N'Dali-Parakou" ~ 93.91,
      year == 2023 & HZs == "Natitingou-Boukoumbé-Toucountouna" ~ 94.58,
      year == 2023 & HZs == "Nikki-Kalalé-Pèrèrè" ~ 93.91,
      year == 2023 & HZs == "Ouidah-Kpomassè-Tori-Bossito" ~ 89.51,
      year == 2023 & HZs == "Pobè-Kétou-Adja-ouèrè" ~ 92.88,
      year == 2023 & HZs == "Porto-Novo-Sèmè-kpodji-Aguégués" ~ 91.27,
      year == 2023 & HZs == "Sakété-Ifangni" ~ 92.88,
      year == 2023 & HZs == "Savalou-Bantè" ~ 93.92,
      year == 2023 & HZs == "Savè-Ouèssè" ~ 93.92,
      year == 2023 & HZs == "Tanguiéta-Cobly-Matéri" ~ 94.58,
      year == 2023 & HZs == "Tchaourou (ZS)" ~ 93.91
      
    )
    
    
    
  )

# Visualisation of the case trends against number of facilities reporting & reporting rate  

uniqueHZs <-  unique(dataFinal$HZs)

plots <- list()

for( i in 1 : length(uniqueHZs)){
  HZ  =   uniqueHZs[i]
  data = dataFinal %>% 
    filter(HZs == HZ) %>% 
    group_by(date, HZs) %>% 
    summarise(casesOver5_sum = sum(casesOver5, na.rm = T), 
              casesUnder5_sum = sum(casesUnder5,  na.rm = T), 
    ) %>%  left_join(
      data_with_reported_counts,
      by = c("HZs"='HZs', 'date'='monthYear')
    )
  
  p <- ggplot(data, aes(x = as.Date(date))) +

    geom_line(aes(y = casesUnder5_sum + casesOver5_sum, color = "Malaria Cases"), size = 1) +
    geom_point(aes(y = casesUnder5_sum + casesOver5_sum, color = "Malaria Cases")) +
    
    # Ligne et points pour les HFs rapportés
    geom_line(aes(y = reported_HFs * (60000 / 42), color = "HFs Reported"), 
              size = 1) +
    geom_point(aes(y = reported_HFs * (60000 / 42), color = "HFs Reported")) +
    
    geom_line(aes(y = meanReportingrate_HZs * (60000 / 100), 
                  color = "Reporting Rate (%)"), size = 1) +
    geom_point(aes(y = meanReportingrate_HZs * (60000 / 100), 
                   color = "Reporting Rate (%)")) +

    scale_y_continuous(
      name = "Total malaria Cases",
      sec.axis = sec_axis(~ . * 100 / (60000), 
                          name = "HFs Reported & Mean Reporting Rate (%)")
    ) +
    
    geom_vline(xintercept = as.Date(c("2018-01-01", "2019-01-01","2020-01-01",
                                      "2021-01-01","2022-01-01","2023-01-01")), 
               linetype = "dashed") +
    
    # Ajout des légendes
    scale_color_manual(
      name = "Legend",
      values = c("Malaria Cases" = "blue4", 
                 "HFs Reported" = "forestgreen", 
                 "Reporting Rate (%)" = "firebrick")
    ) +
    
    # Thème et légendes
    theme_bw() +
    labs(
      title = HZ,
      x = NULL,
      y = "Total malaria Cases"
    ) +
    theme(
      axis.title.y = element_text(color = "black"),
      axis.title.y.right = element_text(color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = 'top',
      legend.title = element_blank()
    )
  
  plots[[i]] <- p 
  names(plots)[i] <- as.character(HZ)
  
}


plots$`Kandi-Gogounou-Ségbana`

for (i in 1:length(plots)){
  
  print(plots[i])
  
}

# Visualisation of the case trends against precipipation and temperature 

uniqueHZs <-  unique(dataFinal$HZs)

plots <- list()

for( i in 1 : length(uniqueHZs)){
  HZ  =   uniqueHZs[i]
  data = dataFinal %>% 
    filter(HZs == HZ) %>% 
    group_by(date, HZs) %>% 
    summarise(casesOver5_sum = sum(casesOver5, na.rm = T), 
              casesUnder5_sum = sum(casesUnder5,  na.rm = T), 
    ) %>%  left_join(
      data_with_reported_counts,
      by = c("HZs"='HZs', 'date'='monthYear')
    ) %>% 
    left_join(
      data_preci_temp_HZs,
      by=c("HZs"='district_group', 'date'="month_start")
    )
  
  # Deuxième graphique avec temp_cds_avg et precip_cds_sum
  p2 <- ggplot(data, aes(x = as.Date(date))) +
    # Ligne et points pour les cas de paludisme
    geom_line(aes(y = casesUnder5_sum + casesOver5_sum, color = "Malaria Cases"), size = 1) +
    geom_point(aes(y = casesUnder5_sum + casesOver5_sum, color = "Malaria Cases")) +
    
    # Ligne pour la température moyenne
    geom_line(aes(y = (data$temp_cds_avg * (60000 / max(data$temp_cds_avg, na.rm = TRUE))), 
                  color = "Average Temperature"), 
              size = 1) +
    
    
    # Ligne pour la somme des précipitations
    geom_line(aes(y = (data$precip_cds_sum * (60000 / max(data$precip_cds_sum, na.rm = TRUE))), 
                  color = "Precipitation Sum"), 
              size = 1, linetype = "dotted") +
    
    # Axe Y secondaire avec mise à l'échelle
    scale_y_continuous(
      name = "Total malaria Cases",
      sec.axis = sec_axis(
        ~ . * max(data$temp_cds_avg, na.rm = TRUE) / 60000, 
        name = "Temperature (°C) / Precipitation (mm)"
      )
    ) +
    
    # Ajout des lignes verticales pour marquer les années importantes
    geom_vline(xintercept = as.Date(c("2018-01-01", "2020-01-01", "2022-01-01")), 
               linetype = "dashed") +
    
    # Ajout des légendes
    scale_color_manual(
      values = c("Malaria Cases" = "black", 
                 "Average Temperature" = "red3", 
                 "Precipitation Sum" = "blue3"),
      name = "Legend"
    ) +
    
    # Thème et légendes
    theme_bw() +
    labs(
      title = HZ,
      x = NULL,
      y = "Total malaria Cases"
    ) +
    theme(
      axis.title.y = element_text(color = "black"),
      axis.title.y.right = element_text(color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "top",  # Place la légende en haut
      legend.title = element_blank() # Titre de la légende en gras
    )
  
  plots[[i]] <- p2 
  names(plots)[i] <- as.character(HZ)
  
}


for (i in 1:length(plots)){
  
  print(plots[i])
  
}

