library(tidyverse)

water_huc <- readRDS("./data/water_huc.rds")
turbidity <- filter(water_huc, result_type == "Turbidity")
tss <- filter(water_huc, result_type == "TSS")

################################ Turbidity EDA plots ################################################ 
# plot a histogram of turbidity values
turbidity %>%
  filter(0 < measurement & measurement < 100) %>%
  ggplot(aes(measurement)) +
  geom_histogram(binwidth = 0.5)

# plot mean turbidity by study 
turbidity %>%
  group_by(Study_ID) %>%
  summarise(m.turbidity = mean(measurement)) %>%
  ggplot(aes(x = reorder(Study_ID, m.turbidity), y = m.turbidity)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean turbidity by location
turbidity %>%
  group_by(Location_ID) %>%
  summarise(m.turbidity = mean(measurement)) %>%
  ggplot(aes(x = reorder(Location_ID, m.turbidity), y = m.turbidity)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean turbidity by Watershed_WRIA
turbidity %>%
  group_by(Watershed_WRIA) %>%
  summarise(m.turb = mean(measurement)) %>%
  ggplot(aes(x = reorder(Watershed_WRIA, m.turb), y = m.turb)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot log turbidity in each wria over time
ggplot(data = turbidity, mapping = aes(x = start_date, y = logMeasurement)) + 
  geom_point() + 
  geom_smooth() +
  # geom_hline(yintercept = 1.90309, color = "red", show.legend = TRUE) +
  facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time")

# funding project data is from 2007-2015 ONLY\
sub <- subset(TURB_TSS, ID == "4388")
TURB_TSS %>%
  filter(result_type == "Turbidity") %>%
  ggplot(mapping = aes(x = start_date, y = logMeasurement)) +
  geom_point(alpha = 1/4) +
  geom_point(data = sub, color = 'red', size = 5) +
  scale_color_manual('Status', values = mycolors) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time")

# funding project data is from 2007-2015 ONLY
turbidity %>%
  filter(format(start_date,"%Y") >= 2004) %>%
  ggplot(mapping = aes(x = start_date, y = logMeasurement)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time (2004-2016)")

# funding project data is from 2007-2015 ONLY
# only want longer length projects for meta-analysis
filtered.turb <- turbidity %>%
  mutate(year = format(start_date,"%Y")) %>%
  filter(year >= 2004) %>%
  group_by(Study_ID) %>%
  filter(n_distinct(year) >= 6)

filtered.turb %>%
  ggplot(mapping = aes(x = start_date, y = logMeasurement)) +
  geom_point(mapping = aes(color = Study_ID)) +
  geom_smooth() +
  # facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log Turbidity (NTU)") +
  ggtitle("Turbidity Trends Over Time (2004-2016)")

################################ TSS EDA plots ################################################ 

# plot a histogram of tss values
tss %>%
  filter(0 < measurement & measurement < 100) %>%
  ggplot(aes(measurement)) +
  geom_histogram(binwidth = 0.5)

# plot mean tss by study 
tss %>%
  group_by(Study_ID) %>%
  summarise(m.tss = mean(measurement)) %>%
  ggplot(aes(x = reorder(Study_ID, m.tss), y = m.tss)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean tss by location
tss %>%
  group_by(Location_ID) %>%
  summarise(m.tss = mean(measurement)) %>%
  ggplot(aes(x = reorder(Location_ID, m.tss), y = m.tss)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# plot mean tss by Watershed_WRIA
tss %>%
  group_by(Watershed_WRIA) %>%
  summarise(m.tss = mean(measurement)) %>%
  ggplot(aes(x = reorder(Watershed_WRIA, m.tss), y = m.tss)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Watershed WRIA") +
  ylab("Mean TSS (mg/L)")

# plot tss by date and Watershed_WRIA
# http://www.pugetsoundnearshore.org/supporting_documents/wria14_lfa.pdf
# The U.S. Fish and Wildlife Service recommends a maximum TSS level of 80
# mg/L to protect salmonid fish (Fish and Wildlife Service 1995). 
ggplot(data = tss, mapping = aes(x = start_date, y = logMeasurement)) + 
  geom_point() + 
  geom_smooth() +
  geom_hline(yintercept = 1.90309, color = "red", show.legend = TRUE) +
  facet_wrap(~ Watershed_WRIA)

# create good/bad tss categories
tss %>%
  mutate(safety = ifelse(logMeasurement >= 1.90309, "unsafe", "safe")) %>%
  ggplot(mapping = aes(x = start_date, y = logMeasurement)) + 
  geom_point(mapping = aes(color = safety)) + 
  scale_color_brewer(palette="Dark2") +
  geom_smooth() +
  geom_hline(yintercept = 1.90309, color = "black", show.legend = TRUE) +
  facet_wrap(~ Watershed_WRIA) +
  xlab("Year") +
  ylab("Log TSS (mg/L)")

# plot measurements by year in each study_id
ggplot(data = tss) + 
  geom_point(mapping = aes(x = start_date, y = logMeasurement)) + 
  facet_wrap(~ Study_ID) +
  xlab("Year") +
  ylab("Log TSS (mg/L)")

# plot measurements by year (ugly)
tss %>%
  mutate(yr = format(start_date,"%Y")) %>%
  ggplot() + 
  geom_point(mapping = aes(x = start_date, y = logMeasurement)) + 
  facet_wrap(~ yr) +
  xlab("Year") +
  ylab("Log TSS (mg/L)")

# funding project data is from 2007-2015 ONLY
tss %>%
  filter(format(start_date,"%Y") >= 2004) %>%
  ggplot() +
  geom_point(mapping = aes(x = start_date, y = logMeasurement)) +
  facet_wrap(~ Watershed_WRIA)

# plot turbidity vs tss over time
water_huc %>%
  ggplot(mapping = aes(x = start_date, y = logMeasurement)) +
  geom_point(mapping = aes(color = result_type)) +
  geom_smooth(mapping = aes(x = start_date, y = logMeasurement, linetype = result_type)) +
  facet_grid(result_type~ Watershed_WRIA) +
  xlab("Year") +
  ylab("") +
  ggtitle("Turbidity vs TSS") +
  scale_x_date(date_breaks = '10 years', date_labels = '%Y')
