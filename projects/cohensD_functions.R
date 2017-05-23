library(tidyverse)

cohensD_manual <- function(before, after) {
  lb <- length(before)
  la <- length(after)
  mb <- mean(before, na.rm = TRUE)
  ma <- mean(after, na.rm = TRUE)
  sdb <- sd(before, na.rm = TRUE)
  sda <- sd(after, na.rm = TRUE)
  step1 <- (la-1) * (sda^2)
  step2 <- (lb-1) * (sdb^2)
  var_pooled <- sqrt((step1+step2)/(la+lb-1))
  d <- (ma - mb) / var_pooled
  return(d)
}

# for water quality: group dataframe by result type and then by HUC
# categorize each measurement as before, during, or after the median investment year in that HUC
# add a 'noProject' category for HUCs without a project
# calculate the cohensD for each measurement in each HUC (add new column)
# NOTE: This incorporates a sign change to make change stat consistent with chum
# add new columns for the mean before and after median investment year in each HUC
# reorganize the dataframe
# then, categorize the effect size based on cohen's d standards
# determined by Leska Fore of PSP:
# Cohen's D value     Category
# ----------------    -----------------
# D ??? 0.8             Large improvement
# 0.2 ??? D < 0.8       Small improvement
# -0.2 < D < 0.2      No change
# -0.8 < D ??? -0.2     Small decline
# D ??? -0.8            Large decline

# for chum: group dataframe by chum site
# categorize each measurement as before, during, or after the median investment year in that HUC
# add a 'noProject' category for sites without a project
# calculate the cohensD for each site (add new column) 
# add new columns for the mean and sd in each site, as well as other variables for the roll-up
# group the dataframe by HUC
# calculate the mean cohensD for each HUC (add new column), as well as other variables
# reorganize the dataframe
# then, categorize the effect size based on cohen's d standards
# determined by Leska Fore of PSP - see above

apply_cohensD <- function(data, outcome, HUC) {
  if (outcome == 'water') { 
    data %>%
      group_by_('result_type', HUC) %>%
      mutate(TimePeriod = ifelse(year > medianyr, 'after',
                                 ifelse(year < medianyr, 'before',
                                        'during'))) %>%
      mutate(TimePeriod = ifelse(is.na(TimePeriod), 'noProject', TimePeriod)) %>%
      spread(TimePeriod, measurement) %>%
      mutate(cohensd_huc_mean = -1*cohensD_manual(before, after),
             huc_mean_after = mean(after, na.rm = TRUE),
             huc_mean_before = mean(before, na.rm = TRUE),
             noProject = if (exists('noProject')) noProject else NA) %>%
      gather(key = 'TimePeriod', value = 'measurement', `before`, `after`, `during`, `noProject`, na.rm = TRUE) %>%
      mutate(effectsize = ifelse(abs(cohensd_huc_mean) < 0.2, 'no change',
                                 ifelse(abs(cohensd_huc_mean) >= 0.8, 'large',
                                        'small')))
  } else if (outcome == 'chum') { 
    data %>%
      group_by(site) %>%
      mutate(TimePeriod = ifelse(year > medianyr, 'after',
                                 ifelse(year < medianyr, 'before',
                                        'during'))) %>%
      mutate(TimePeriod = ifelse(is.na(TimePeriod), 'noProject', TimePeriod)) %>%
      spread(TimePeriod, count) %>%
      mutate(cohensd = cohensD_manual(before, after),
             site_mean_after = mean(after, na.rm = TRUE),
             site_mean_before = mean(before, na.rm = TRUE),
             site_sd_before = sd(before, na.rm = TRUE),
             site_sd_after = sd(after, na.rm = TRUE),
             var_pooled = sqrt((((length(after)-1)*(site_sd_after^2))+((length(before)-1)*(site_sd_before^2)))/(length(after)+length(before)-1)),
             var_cohensd = (length(before) + length(after))/(length(before) * length(after)) + (cohensd^2)/(2*(length(before) + length(after))),
             sd_cohensd = var_cohensd^0.5,
             wsubi = 1/var_cohensd,
             wsubixd = cohensd*wsubi) %>%
      ungroup() %>%
      group_by_(HUC) %>%
      mutate(cohensd_huc_mean = (sum(wsubixd)/sum(wsubi)),
             huc_mean_after = mean(after, na.rm = TRUE),
             huc_mean_before = mean(before, na.rm = TRUE),
             sum_wsubixd = sum(wsubixd),
             sum_wsubi = sum(wsubi),
             cohensd_huc_var = 1/sum_wsubi,
             cohensd_huc_sd = cohensd_huc_var^0.5,
             plus_minus = cohensd_huc_sd*1.645,
             noProject = if (exists('noProject')) noProject else NA) %>%
      gather(key = 'TimePeriod', value = 'count', `before`, `after`, `during`, `noProject`, na.rm = TRUE) %>%
      mutate(effectsize = ifelse(abs(cohensd_huc_mean) < 0.2, 'no change',
                                 ifelse(abs(cohensd_huc_mean) >= 0.8, 'large',
                                        'small')),
             site_effectsize = ifelse(abs(cohensd) < 0.2, 'no change',
                                      ifelse(abs(cohensd) >= 0.8, 'large',
                                             'small')))
  }
}

# Adding status colors
# Note: With sign change on water Cohen's D, there's now no need for if/else
# Apply a diverging color scale (based on effect size and status)
# Standard colors are ColorBrewer 5-class RdYlGn; middle color replaced by lightgray (#d3d3d3)
# Reference: http://colorbrewer2.org/?type=diverging&scheme=RdYlGn&n=5
# colorblind colors are ColorBrewer 5-class PiYG
# Reference: http://colorbrewer2.org/?type=diverging&scheme=PiYG&n=5
add_status_colors <- function(data) {
  colorscale = c('#d7191c','#fdae61','#d3d3d3','#a6d96a','#1a9641')
  colorscale_colorblind = c('#d01c8b','#f1b6da','#f7f7f7','#b8e186','#4dac26')
  data %>% 
    mutate(status = ifelse(cohensd_huc_mean < -0.2, 'worse',
                           ifelse(cohensd_huc_mean > 0.2, 'improving',
                                  'no change')),
           coloreffect = ifelse(effectsize == 'large' & status == 'worse', colorscale[1],
                                ifelse(effectsize == 'small' & status == 'worse', colorscale[2],
                                       ifelse(effectsize == 'no change' & status == 'no change', colorscale[3],
                                              ifelse(effectsize == 'small' & status == 'improving', colorscale[4],
                                                     ifelse(effectsize == 'large' & status == 'improving', colorscale[5],
                                                            NA))))),
           colorblind = ifelse(effectsize == 'large' & status == 'worse', colorscale_colorblind[1],
                               ifelse(effectsize == 'small' & status == 'worse', colorscale_colorblind[2],
                                      ifelse(effectsize == 'no change' & status == 'no change', colorscale_colorblind[3],
                                             ifelse(effectsize == 'small' & status == 'improving', colorscale_colorblind[4],
                                                    ifelse(effectsize == 'large' & status == 'improving', colorscale_colorblind[5],
                                                           NA))))))
}