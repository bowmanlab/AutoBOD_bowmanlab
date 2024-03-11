
system('python3 clean_autobod.py')
name <- '20240208_test'
path <- '~/bowman_lab/sccoos/autobod/'
name <- paste0(path, name)

columns <- c('amplitude',
             'phase',
             'O2',
             'error_code',
             'position',
             'bottle_number',
             'sample_number',
             'date',
             'elapsed_time',
             'T_internal',
             'T_bottle',
             'steps',
             'light_level',
             'serial_number',
             'voltage')

data <- read.csv(paste0(name, '.clean.csv'), header = F, sep = ',', col.names = columns)
data <- data[data$error_code %in% c(1,5),]
data$elapsed_date_time <- paste(data$date, data$elapsed_time)
data$elapsed_date_time <- strptime(data$elapsed_date_time, format = '%d %H:%M:%S')
day1_times <- data$elapsed_date_time[which(data$date == 1)]
init_time <- day1_times[order(day1_times, decreasing = F)[1]]
data$elapsed_time_decimal <- as.numeric(data$elapsed_date_time - init_time)/60 # minutes is necessary for binning into time steps

## eliminate data where T is more than 1 SD from mean
data <- data[abs(data$T_internal - mean(data$T_internal)) < sd(data$T_internal),]

## Group data by 10 minute intervals
data$elapsed_rounded <- round(data$elapsed_time_decimal/10) * 10

r.rate <- as.data.frame(matrix(ncol = 3, nrow = length(unique(data$bottle_number))))
colnames(r.rate) <- c('Mean', 'Mode', 'Slope')
row.names(r.rate) <- unique(data$bottle_number)

#### calculate DO consumption ####

pdf(paste0(name, '.pdf'),
    width = 6,
    height = 6)

for(bottle in sort(unique(data$bottle_number), decreasing = F)){
  temp <- data[data$bottle_number == bottle,]

  temp.round <- as.data.frame(as.matrix(tapply(temp$O2, temp$elapsed_rounded, FUN = mean)))
  colnames(temp.round) <- c('O2')
  temp.round$min <- as.numeric(row.names(temp.round))

  plot(temp.round$min, temp.round$O2,
       ylab = 'O2',
       xlab = 'Minutes',
       main = paste('Bottle', bottle))

  temp.model <- lm(O2 ~ min, data = temp.round)
  abline(temp.model)

  ## use residuals to cull data, drop data with residuals > 25 % of max

  temp.max.residual <- max(temp.model$residuals)
  temp.round <- temp.round[(temp.model$residuals/temp.max.residual) < 0.25,]
  temp.model <- lm(O2 ~ min, data = temp.round)

  ## randomized comparisons
  #!!! orginal approach discards comparisons < 60 minutes

  temp.random <- vector(length = (dim(temp.round)[1] * (dim(temp.round)[1] - 1)) - 1, mode = 'numeric')

  k <- 1

  for(i in 1:dim(temp.round)[1]){
    for(j in 2:dim(temp.round)[1]){
      temp.O2 <- temp.round$O2[i] - temp.round$O2[j]
      temp.min <- temp.round$min[i] - temp.round$min[j]
      temp.rate <- temp.O2/temp.min
      temp.random[k] <- temp.rate
      k <- k + 1
      #print(paste(k, temp.rate))
    }
  }

  temp.random <- na.omit(abs(temp.random))
  temp.hist <- hist(temp.random, breaks = 1000)
  temp.mean <- mean(temp.random)
  temp.mode <- temp.hist$mids[which.max(temp.hist$counts)]

  ## values converted to X per hour

  r.rate[paste0(bottle), 'Mean'] <- temp.mean * -60
  r.rate[paste0(bottle), 'Mode'] <- temp.mode * -60
  r.rate[paste0(bottle), 'Slope'] <- temp.model$coefficients[2] * 60

  print(paste0(c(bottle, temp.mode)))

  plot(temp.round$min, temp.round$O2,
       ylab = 'O2',
       xlab = 'Minutes',
       main = paste('Bottle', bottle))

  abline(temp.model)
  abline(temp.model$coefficients[1], temp.mean * -1, col = 'red')
  abline(temp.model$coefficients[1], temp.mode * -1, col = 'blue')

  legend('topright',
         legend = c('Mean', 'Mode', 'Slope'),
         col = c('red', 'blue', 'black'),
         lty = 1)
}

dev.off()

r.rate$bottle <- row.names(r.rate)

write.csv(r.rate, file = paste0(name, '_rates.csv'), row.names = F, quote = F)
