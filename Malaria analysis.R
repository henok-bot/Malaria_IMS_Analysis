##############################################################################-
############## Malaria Weekly Data manipulationa and Analysis #################


#Necessary packages - load the Epi packages script

pacman::p_load(tidyverse, readxl, import, here, janitor, rio, magrittr, esquisse,
               stringdist, reshape2, scales, ggsci, RColorBrewer)

###1. Append Weekly incoming files #######
file_list <- list.files(path = "Data1", pattern = "*.xlsx", full.names = TRUE) %>% 
  .[!grepl("^~\\$", basename(.))] # remove temporary files while files are open

# Function to clean and standardize each file
clean_weekly_file <- function(file_path) {
  data <- read_excel(file_path)
  
  # Standardize column names and clean empty columns
  data <- data %>%
    rename_with(~ make.names(.)) %>%  # Ensure valid column names
    select(where(~ !all(is.na(.))))   # Remove fully empty columns
  
  return(data)
}

# Read, clean, and append all the weekly files, binding rows based on column names

malaria_date <- file_list %>% 
  map(clean_weekly_file) %>%  # Read and clean each file
  reduce(bind_rows) # Bind all rows together by matching column names

glimpse(malaria_date)
View(malaria_date)

tail(malaria_data)

###2. Basic data cleaning ################

# cleaning column names
malaria_data <-  malaria_date %>% janitor::clean_names() %>% 
  rename(total_case = total_malaria_confirmed_and_clinical,
         outp_case = t_malaria_out_p_cases,
         inp_case = t_malaria_in_p_cases,
         death = t_malaria_in_p_deaths,
         pf = pos_malaria_rdt_or_microscopy_pf_out_p_cases,
         pv = pos_malaria_rdt_or_microscopy_pv_out_p_cases,
         mixed = pos_malaria_rdt_or_microscopy_mixed_out_p_cases,
         test = tm_suspected_fever_examined,
         total_confirmed_s = total_confirmed,
         confirm_rate_s = percent_of_confirmed,
         pos_rate_s = test_positivity)


#clean woreda name based on standard dictionary

wor_cleaning_dic <- import("C:/Users/henok/OneDrive/Desktop/Database/All GIS names.xlsx")
cleaning_dic_vector <- setNames(wor_cleaning_dic$Gname, wor_cleaning_dic$Woreda)

malaria_data <- malaria_data %>% mutate (c_woreda = case_when(
  woreda %in% names(cleaning_dic_vector) ~ cleaning_dic_vector[woreda],
  TRUE ~ woreda)) %>% mutate(c_woreda = as.character(c_woreda))

View(malaria_data)
 View(cleaning_dic_vector)


#Assign cluster names to the woredas(with out vectorizing) (Needs revision)

#cluster_dic <- import("C:/Users/henok/OneDrive/Desktop/Database/Cluster population N.xlsx")

#malaria_data <- malaria_data %>% mutate(cluster =  case_when(
# woreda %in% cluster_dic$ADMIN3_EN ~ cluster_dic$Cluster_u[match(woreda,cluster_dic$ADMIN3_EN)],
# TRUE ~ NA_character_)) %>% mutate(mixed = as.numeric(mixed))

table(malaria_data$cluster,malaria_data$cluster_n)

# clean region names 

unique(malaria_data$region)

malaria_data <- malaria_data %>% 
  mutate(region = str_squish(str_to_title(region))) #Trim and convert to lowercase
unique(malaria_data$region)

# if checking for difference is still needed due to typos or slight variation , 
# string distance can be used using the jaro-winkler method
woreda_names <- malaria_data$woreda
woreda_names <- unique(woreda_names) 
sum(is.na(woreda_names))
unique(woreda_names)
woreda_names <- str_trim(woreda_names)

dist_matrix<- stringdistmatrix(woreda_names, method = "jw")
dimnames(dist_matrix) <- list(woreda_names)
str_dis_df <- as.data.frame(as.table(dist_matrix)) 
nrow(dist_matrix)
length(woreda_names)

#Substitute clear names for duplicate woreda names(will work on this)

str_to 
## Will be continued

### Very important  ### changing the woreda names to be similar for the cleaning dictionary 
#### based on the zone name


### 3. Basic Pivoting analysis ##################################

pivot_data <- malaria_data %>% group_by(region,year) %>% 
  summarise(Total_case = sum(total_case,na.rm = TRUE)) %>% 
  pivot_wider(names_from = year, values_from = Total_case)

head(pivot_data)
View(pivot_data)

?scale_fill_npg()

#### Playing around with analysis for cluster review meeting #

#Faceting 2024 trends 

malaria_data_24 <- malaria_data %>% filter(year == 2024)


plot_2 <-ggplot(malaria_data_24,aes(x= epi_week, y=total_case))+
  geom_bar(stat = "identity", fill = "#8B4513")+ 
  facet_wrap(~region, scales = "free_y", ncol = 4)+
  theme_minimal()+
  labs(title = "Regional Malaria case trend 2024", 
       subtitle = "Year of 2024 as of week 35",
       x = "Epi_week", y = "Cases")+
  theme(
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 12, face = "bold"),  # facet labels style
    strip.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.line.y.left = element_line(size = 1.0, colour = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # remove grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines for clarity
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +  # better x-axis scaling
  scale_y_continuous(labels = scales::comma)  # comma formatting for large y-axis values

ggsave("malaria_trend_plot_t.png", plot = plot_2, width = 12, height = 8, dpi = 300, bg = "white")


# Practicing labeling --- better not to do it on facets 

# using color from the sape color reference
plot_3 <- ggplot(malaria_data_24,aes(x= epi_week, y=total_case))+
  geom_bar(stat = "identity", fill = "bisque4")+ 
  facet_wrap(~region, scales = "free_y", ncol = 4)+
  theme_minimal()+
  labs(title = "Regional Malaria case trend 2024", 
       subtitle = "Year of 2024 as of week 35",
       x = "Epi_week", y = "Cases")+
  theme(
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 12, face = "bold"),  # facet labels style
    strip.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.line.y.left = element_line(size = 1.0, colour = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # remove grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines for clarity
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +  # better x-axis scaling
  scale_y_continuous(labels = scales::comma)  # comma formatting for large y-axis values
ggsave("malaria_trend_plot_3.png", plot = plot_3, width = 12, height = 8, dpi = 300, bg = "white")


### top 10 woreda trend of each week 

top_10_woredas <- malaria_data %>% 
  filter(year == 2024 & epi_week == max(epi_week[year == 2024], na.rm = TRUE)) %>% 
  group_by(g_name) %>% 
  summarise(case_latest_week = sum(total_case, na.rm = TRUE)) %>% 
  arrange(desc(case_latest_week)) %>% 
  slice_head(n = 10)

top_10_data_24 <- malaria_data_24 %>% 
  filter(g_name %in% top_10_woredas$g_name) %>% 
  select(g_name, epi_week, total_case) %>% group_by(g_name, epi_week) %>% 
  summarise(epiweek_case = sum(total_case, na.rm = TRUE)) %>% 
  ungroup()

# To order the data based on latest week trend 

order_data <- top_10_data_24 %>% filter(epi_week == 42) %>% 
  arrange(desc(epiweek_case)) %>% mutate(order = row_number()) 

Top_10_data_24_ord <- top_10_data_24 %>% left_join(order_data %>%  select(g_name, order),
                                                   by = "g_name")
#The trick is making it factor as below to show 
Top_10_data_24_ord <- Top_10_data_24_ord %>%
  mutate(g_name = factor(g_name, levels = order_data$g_name[order_data$order]))


  

plot_top_woreda  <- ggplot(Top_10_data_24_ord, aes(x = epi_week, y = epiweek_case)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  facet_wrap(~g_name, scales = "free_y", ncol = 4)+
  theme_minimal()+
  labs(title = "Malaria Trend of Highest case reporting 10 woredas(Week 42)" ,
       subtitle = "2024 as of week 42",
       x = "Epi week", y = "Cases") +
  theme(
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 12, face = "bold"),  # facet labels style
    strip.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.line.y.left = element_line(size = 1.0, colour = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # remove grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines for clarity
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +  # better x-axis scaling
  scale_y_continuous(labels = scales::comma)  # comma formatting for large y-axis values

ggsave("Output/Top woreda plot Week 42.png", plot = plot_top_woreda, width = 12, height = 8, dpi = 300, bg = "white")


### Analysis for Cluster review meeting 

## Faceted case trend with unique indicator for the cluster time 

colnames(malaria_data)
unique(malaria_data$cluster)

malaria_data <- malaria_data %>% mutate(cluster_o = as.factor(cluster_n))
unique(malaria_data$cluster_o) 


# preparing Cluster woredas only date 
cluster_data <- malaria_data %>% filter(cluster_n != "_")

cluster_data_g <- cluster_data %>% group_by(region, year, epi_week) %>% 
  summarise(total_test = sum(test, na.rm = TRUE),
            total_case = sum(total_case, na.rm = TRUE),
            pf = sum(pf, na.rm = TRUE),
            pv = sum(pv, na.rm = TRUE),
            mixed = sum(mixed, na.rm = TRUE),
            inp_case = sum(inp_case, na.rm = TRUE),
            death = sum(death, na.rm = TRUE),
            confirm_case = sum(pf, pv, mixed, na.rm = TRUE)
            ) %>% ungroup() 
cluster_data_g <- cluster_data_g %>% mutate( pos_rate = (confirm_case/total_test)*100,
                                             inp_rate = (inp_case/total_case) * 100,
                                             cfr = (death/total_case) * 100)

head(cluster_data_g)
tail(cluster_data_g)
unique(cluster_data_g$region)

# Computing cluster woredas contribution and looking at the trend

regional_total_case <- malaria_data %>% group_by(region, year, epi_week) %>% 
  summarise(total_region_case = sum(total_case, na.rm = TRUE)) %>% 
  ungroup() %>% filter(!region %in% c("Harari", "Dire Dawa", "Somali", "Addis Ababa"))
unique(malaria_data$region)

cluster_con_24 <- cluster_data_g %>% left_join(regional_total_case, by = c("region","year","epi_week")) %>% 
   filter(year == 2024)
cluster_con_24 <- cluster_con_24 %>% group_by(region,year,epi_week) %>% 
  summarise(clust_cont = (sum(total_case,na.rm = TRUE)/sum(total_region_case,na.rm = TRUE))*100) %>% ungroup()

# Faceted graph of total cases 
plot_1 <- ggplot(cluster_data_g,aes(x = epi_week, y = total_case, group = year, colour = as.factor(year))) +
  geom_line(size = 1)+
  geom_vline(xintercept = 26, linetype = "dashed", colour = "blue")+
  facet_wrap(~region, scales = "free_y", ncol = 4)+
  labs( 
    title = "Malaria case trend in Cluster woredas by reigon(2019-2024(wk40)",
    subtitle = "Cluster approach launched in Week 26, 2024(Dashed line)",
    x = "Epidemiological week",
    y = "Total case" ) +
    scale_color_brewer(palette = "Dark2") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 12, face = "bold"),  # facet labels style
    strip.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.line.y.left = element_line(size = 1.0, colour = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # remove grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines for clarity
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +  # better x-axis scaling
  scale_y_continuous(labels = scales::comma)  # comma formatting for large y-axis values

ggsave("output/Case trend of cluster woredas.png", plot = plot_1, width = 12, height = 8, dpi = 300, bg = "white")



# only 2024 trend for inpatient rate , pos rate and inpatient rate
cluster_data_24g <- cluster_data_g %>% filter(year == 2024)


# Faceted graph of Inpatient   rate 
plot_inp  <-  ggplot(cluster_data_24g,aes(x = epi_week, y = inp_rate)) +
  geom_line(size = 1)+
  geom_vline(xintercept = 26, linetype = "dashed", colour = "blue")+
  facet_wrap(~region, scales = "free_y", ncol = 4)+
  labs( 
    title = "Malaria inpatient rate trend in Cluster woredas by reigon in 2024(until wk40)",
    subtitle = "Cluster approach launched in Week 26, 2024(Dashed line)",
    x = "Epidemiological week",
    y = "Inpatient rate(%)" ) +
  scale_color_manual(values = c("2024" = "#1d3557" ))+
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 12, face = "bold"),  # facet labels style
    strip.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.line.y.left = element_line(size = 1.0, colour = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # remove grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines for clarity
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +  # better x-axis scaling
  scale_y_continuous(labels = scales::comma)  # comma formatting for large y-axis values



ggsave("output/Inpaatient trend cluster woredas.png", plot = plot_inp, width = 12, height = 8, dpi = 300, bg = "white")

# Faceted graph of Positivty   rate 
plot_pos  <- ggplot(cluster_data_24g,aes(x = epi_week, y = pos_rate, colour = as.factor(year))) +
  geom_line(size = 1)+
  geom_vline(xintercept = 26, linetype = "dashed", colour = "blue")+
  facet_wrap(~region, scales = "free_y", ncol = 4)+
  labs( 
    title = "Malaria positivity rate trend in Cluster woredas by reigon in 2024(until wk40)",
    subtitle = "Cluster approach launched in Week 26, 2024(Dashed line)",
    x = "Epidemiological week",
    y = "Positivity rate(%)" ) +
  scale_color_manual(values = c("2024" = "#e76f51" ))+
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 12, face = "bold"),  # facet labels style
    strip.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.line.y.left = element_line(size = 1.0, colour = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # remove grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines for clarity
    legend.position = "none" #remove the legend
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +  # better x-axis scaling
  scale_y_continuous(labels = scales::comma)  # comma formatting for large y-axis values

ggsave("output/Pos rate trend cluster woredas.png", plot = plot_pos, width = 12, height = 8, dpi = 300, bg = "white")

# Faceted plot with CFR
plot_cfr <- ggplot(cluster_data_24g,aes(x = epi_week, y = cfr , colour = as.factor(year))) +
  geom_line(size = 1)+
  geom_vline(xintercept = 26, linetype = "dashed", colour = "blue")+
  facet_wrap(~region, scales = "free_y", ncol = 4)+
  labs( 
    title = "Malaria Case fatality rate trend in Cluster woredas by reigon in 2024(until wk40)",
    subtitle = "Cluster approach launched in Week 26, 2024(Dashed line)",
    x = "Epidemiological week",
    y = "Case fatality rate(%)" ) +
  scale_color_manual(values = c("2024" = "#d62828" ))+
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 12, face = "bold"),  # facet labels style
    strip.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.line.y.left = element_line(size = 1.0, colour = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # remove grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines for clarity
    legend.position = "none" #remove the legend
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +  # better x-axis scaling
  scale_y_continuous(labels = scales::comma)  # comma formatting for large y-axis values

ggsave("output/CFR trend cluster woredas.png", plot = plot_cfr, width = 12, height = 8, dpi = 300, bg = "white")


## Cluster woreda contribution 
plot_cwc <- ggplot(cluster_con_24,aes(x = epi_week, y = clust_cont , colour = as.factor(year))) +
  geom_line(size = 1)+
  geom_vline(xintercept = 26, linetype = "dashed", colour = "blue")+
  facet_wrap(~region, scales = "free_y", ncol = 4)+
  labs( 
    title = "Cluster woreda contribution trend by reigon in 2024(until wk40)",
    subtitle = "Cluster approach launched in Week 26, 2024(Dashed line)",
    x = "Epidemiological week",
    y = "Cluster woreda contribution(%)" ) +
  scale_color_manual(values = c("2024" = "#7570b3" ))+
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 12, face = "bold"),  # facet labels style
    strip.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.line.y.left = element_line(size = 1.0, colour = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # remove grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines for clarity
    legend.position = "none" #remove the legend
  ) +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +  # better x-axis scaling
  scale_y_continuous(labels = scales::comma)  # comma formatting for large y-axis values

ggsave("output/Cluster woreda contribution.png", plot = plot_cwc, width = 12, height = 8, dpi = 300, bg = "white")

### Trial of Heat map 

plot_heat <- ggplot(cluster_data_24g, aes(x = epi_week, y = region, fill = pos_rate))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "red")+
  geom_vline(xintercept = 26, linetype = "dashed", color = "black")+
  labs(title = "Heat map of Positivity rate change by reigon in 2024GC",
       subtitle = "Cluster approach launched in week 26(Dashed line) ",
       x = "Epidemiological week",
       y = "Region",
       fill = "Positivity rate"
  )+
  theme_minimal()+
  theme(text = element_text(family = "Arial"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5))  # Rotate the x-axis labels for readability

ggsave("output/Heat map pso rate.png", plot = plot_heat , width = 12, height = 10, dpi = 300, bg = "white")
### Anlysis on HMIS helaht post contribuiton data 

hmis2016 <- import(here("Healt post contribution regional HMIS.xls"))
unique(hmis2016$Region)
hmisclus <- hmis2016 %>% filter(!Region %in% c("Addis Ababa", "Harari", "Dire Dawa", "Somali")) %>% 
  mutate(Month =factor(Month, levels = c("Mes","Tik","Hid",
                                         "Tah", "Tir", "Yek",
                                         "Meg","Mia", "Gin",
                                         "Sen" , "Ham", "Neh", "Mes 17")))


unique(hmis2016$Month)
unique(hmisclus$Region)

hmisclus <- hmisclus %>% 
  mutate(Region = factor(Region, levels = c("Southwest Ethiopia","Oromia",
                                            "South Ethiopia","Sidama", "Central Ethiopia",
                                            "Afar","Benishangul Gumuz", "Amhara", "Gambella",
                                            "Tigray")))

plot_3 <- ggplot(hmisclus,aes(x= Month, y=`test_hp%`))+
  geom_bar(stat = "identity", fill = "dodgerblue2")+ 
  geom_vline(xintercept = "Sen", linetype = "dashed", colour = "blue")+
  facet_wrap(~Region, scales = "free_y", ncol = 4)+
  theme_minimal()+
  labs(title = "Health post level Malaria service by reigon in 2016EC", 
       subtitle = "Cluster approach launched in Sene, 2016(Dashed line)",
       x = "Month", y = "Health post level test(%)")+
  theme(
    text = element_text(family = "Arial"),
    strip.text = element_text(size = 12, face = "bold"),  # facet labels style
    strip.background = element_rect(fill = "white", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.line.y.left = element_line(size = 1.0, colour = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),  # remove grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines for clarity
  ) 


  scale_x_continuous(breaks = pretty_breaks(n = 5)) +  # better x-axis scaling
  scale_y_continuous(labels = scales::comma)  # comma formatting for large y-axis values
ggsave("Output/Hptest contrib ord.png", plot = plot_3, width = 22, height = 11, dpi = 500, bg = "white")


