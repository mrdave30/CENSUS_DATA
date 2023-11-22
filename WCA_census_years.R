library(readxl)
library(tidyr)
library(dplyr)
library(fs)

# Read the "FAOSTA.xlsx" file and select relevant columns
faostat <- read_excel("raw_data/FAOSTAT.xls") %>% 
  select(Area, `WCA Round`, `Census Year`) %>%
  group_by(`WCA Round`, `Census Year`) %>%
  distinct(Area) %>% 
  filter(!is.na(`Census Year`))

# Read the data and select the necessary columns
table2.1 <- read_xlsx("raw_data/Census_years.xlsx") %>% 
  select(`Countries by region`, `2020`:`1930`) %>%
  # Gather the columns into key-value pairs
  gather(WCAROUND2.1, censusyear2.1, `2020`:`1930`, na.rm = TRUE) %>%
  # Filter the data based on the 'Countries by region' column matching 'faostat$Area'
  filter(`Countries by region` %in% faostat$Area)

# Get unique WCA rounds
unique_rounds <- unique(faostat$`WCA Round`)

# Loop through each WCA round
for (round in unique_rounds) {
  # Filter data for the current round
  faostat_round <- faostat %>% filter(`WCA Round` == round)
  table2.1_round <- table2.1 %>% filter(WCAROUND2.1 == round)
  
  # Merge the dataframes based on a common key
  merged_data <- left_join(table2.1_round, faostat_round, by = c("Countries by region" = "Area"))
  
  # Perform the comparison and create the "com" column
  merged_data <- merged_data %>% 
    mutate(com = ifelse(`Census Year` == censusyear2.1, "Equal", "Not equal"))%>%
    filter(!is.na(`WCA Round`))
  
  # Generate the filename for the current round
  filename <- paste0(round, ".xlsx")
  
  # Write the comparison file to your directory
  writexl::write_xlsx(merged_data, filename)
  
}


