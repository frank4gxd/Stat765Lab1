#By GuoXiaodong 29/02/2024
#frist Assignment Lab01 for Stat765 by Lisa Chan UOA


# load library
library(tidyverse)

# set random seed
seed <- 480730230 # <Replace '480730230' with GuoXiaodong' student ID here.>
set.seed(seed)

# generate alarm id, dates and region vectors
alarms_id.vt <- paste0('alarms_', sample(LETTERS, size = 10, replace = FALSE))
alarms_dates.vt <- paste0('d.',seq(Sys.Date()-60, Sys.Date(), by = '1 day'))  # 60 days back.
alarms_region.vt <- c('AKL_North', 'AKL_Central', 'Waiheke','AKL_South', 'AKL_Others')

# generate random alarm frequency counts
alarms_count.mt <- matrix(round(runif(length(alarms_id.vt) * length(alarms_dates.vt))*seed),
                          nrow = length(alarms_id.vt), ncol=length(alarms_dates.vt))
colnames(alarms_count.mt) <- alarms_dates.vt

# set up data frames
alarms_count.df <- data.frame(alarm_id = alarms_id.vt, alarms_count.mt)
alarms_info.df <- data.frame(alarm_ID = alarms_id.vt, alarms_region = alarms_region.vt)

#Data source alarms_count.df
#data source alarms_info.df

#End of Generate DataSource


