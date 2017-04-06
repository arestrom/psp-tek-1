library(tidyverse)
library(stringr)

#################### EAGL DATA #################### 
eagl_df <- read.csv('./data/eagl.csv', header = TRUE) %>%
  select(Funding.Fiscal.Year, WRIA, Project.Title, 
         Funding.Provided, Latitude, Longitude) %>%
  rename(year = Funding.Fiscal.Year, name = Project.Title, WRIA_ID = WRIA,
         cost = Funding.Provided, lat = Latitude, lon = Longitude) %>%
  # remove missing coordinates
  filter(!lat %in% c('#N/A', '0'),
         !lon %in% c('#N/A', '0')) %>%
  # convert dollar strings ($50,000.00) to numeric values (50000) 
  mutate(cost_sm = str_sub(cost, 2, -4),
         cost = as.numeric(gsub(",", "", cost_sm)),
         lat = as.numeric(levels(lat))[lat],
         lon = as.numeric(levels(lon))[lon]) %>%
  select(-cost_sm)

# label wria number with wria name
eagl_df <- eagl_df %>% 
  mutate(WRIA_Name = ifelse(WRIA_ID == 15, "Kitsap",
                          ifelse(WRIA_ID == 14, "Kennedy-Goldsborough",
                                 ifelse(WRIA_ID == 16, "Skokomish-Dosewallips",
                                        ifelse(WRIA_ID == 17, "Quilcence-Snow",
                                               ifelse(WRIA_ID == 18, "Elwah-Dungeness",
                                                      NA))))))

#################### PRISM DATA #################### 
hc_df <- read.csv('./data/locs.csv', header = TRUE)
f_df <- read.csv('./data/fund.csv', header = TRUE)

mdf <- merge(x = hc_df, y = f_df, by = "ProjectNumber", all.x = TRUE) %>%
  select(ProjectNumber, ProjectYear, ProjectName.x, HUC, WRIA, 
         PrimaryProgramAmount, ProjectLatitude, ProjectLongitude) %>%
  rename(id = ProjectNumber, year = ProjectYear, name = ProjectName.x, 
         cost = PrimaryProgramAmount, lat = ProjectLatitude, lon = ProjectLongitude) %>%
  # create wria number and wria name columns from combo string
  separate(WRIA, into = c("WRIA_Name", "WRIA_ID"), sep = " \\(", convert = TRUE) %>%
  mutate(WRIA_ID = as.numeric(str_sub(WRIA_ID, 1, -2))) %>%
  filter(!is.na(lat), !is.na(cost))


#################### MERGE #################### 
all_projects <- bind_rows(mdf, eagl_df)

# write.csv(mdf, file = "merge.csv",row.names=FALSE,quotes=FALSE)

# mdf <- tbl_df(mdf)
# mdf
# eagl_df <- tbl_df(eagl_df)
# eagl_df
