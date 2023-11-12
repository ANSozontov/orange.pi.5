library(cronR)
cronR::cron_clear(ask = FALSE)
cmd <- cron_rscript("/home/orangepi/monitor/monitor.R")
cron_add(command = cmd, frequency = 'minutely', id = 'my_task', ask = FALSE)
