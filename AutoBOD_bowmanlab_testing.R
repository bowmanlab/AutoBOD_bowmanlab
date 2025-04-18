
system('python3 clean_autobod.py')
name <- 'RottingExperiment_072521_MkII_150m_formatted.txt'
path <- './'
name <- paste0(path, name)

library(pracma)

calc_air_sat <- function(phase, IRBotT){
  
  cal0 <-  60.21 #B6
  cal100 <-  27.42 #B7
  airpres <-  981 #B8
  T0 <-  20 #E6
  T100 <-  20 #E7
  dF_k <-  -0.0847 #B12
  f1 <-  0.833 #B11
  dksv_k <-  0.000416 #B13
  m <-  34 #B14
  
  tan_psi0_t100 <-  tand(((cal0+dF_k*(T100-T0)))*pi/180) #D11
  tan_psi100_t100  <-  tand(cal100*pi/180) #D13
  
  A <-  tan_psi100_t100/tan_psi0_t100*1/m*100^2 #F11
  B <-  tan_psi100_t100/tan_psi0_t100*100+tan_psi100_t100/tan_psi0_t100*100/m-f1*100/m-100+f1*100 #F12
  C <-  tan_psi100_t100/tan_psi0_t100-1 #F13
  
  ksv_t100 <-  (-B+sqrt(B^2-4*A*C))/(2*A) #H11
  return(as.numeric(-((tand(phase*pi/180))/(tand((cal0+(dF_k*(IRBotT-T0)))*pi/180))*(ksv_t100+(dksv_k*(IRBotT-T100)))+(tand(phase*pi/180))/(tand((cal0+(dF_k*(IRBotT-T0)))*pi/180))*1/m*(ksv_t100+(dksv_k*(IRBotT-T100)))-f1*1/m*(ksv_t100+(dksv_k*(IRBotT-T100)))-(ksv_t100+(dksv_k*(IRBotT-T100)))+f1*(ksv_t100+(dksv_k*(IRBotT-T100))))+(sqrt(((((tand(phase*pi/180))/(tand((cal0+(dF_k*(IRBotT-T0)))*pi/180))*(ksv_t100+(dksv_k*(IRBotT-T100)))+(tand(phase*pi/180))/(tand((cal0+(dF_k*(IRBotT-T0)))*pi/180))*1/m*(ksv_t100+(dksv_k*(IRBotT-T100)))-f1*1/m*(ksv_t100+(dksv_k*(IRBotT-T100)))-(ksv_t100+(dksv_k*(IRBotT-T100)))+f1*(ksv_t100+(dksv_k*(IRBotT-T100))))^2))-4*((tand(phase*pi/180))/(tand((cal0+(dF_k*(IRBotT-T0)))*pi/180))*1/m*((ksv_t100+(dksv_k*(IRBotT-T100)))^2))*((tand(phase*pi/180))/(tand((cal0+(dF_k*(IRBotT-T0)))*pi/180))-1))))/(2*((tand(phase*pi/180))/(tand((cal0+(dF_k*(IRBotT-T0)))*pi/180))*1/m*((ksv_t100+(dksv_k*(IRBotT-T100)))^2))))} #oxygen = airsat*20.9/100

calc_o2_conc <- function(airsat, IRBotT){
  
  cal0 <-  60.21 #B6
  cal100 <-  27.42 #B7
  airpres <-  981 #B8
  T0 <-  20 #E6
  T100 <-  20 #E7
  dF_k <-  -0.0847 #B12
  f1 <-  0.833 #B11
  dksv_k <-  0.000416 #B13
  m <-  34 #B14
  
  tan_psi0_t100 <-  tand(((cal0+dF_k*(T100-T0)))*pi/180) #D11
  tan_psi100_t100  <-  tand(cal100*pi/180) #D13
  
  A <-  tan_psi100_t100/tan_psi0_t100*1/m*100^2 #F11
  B <-  tan_psi100_t100/tan_psi0_t100*100+tan_psi100_t100/tan_psi0_t100*100/m-f1*100/m-100+f1*100 #F12
  C <-  tan_psi100_t100/tan_psi0_t100-1 #F13
  
  ksv_t100 <-  (-B+sqrt(B^2-4*A*C))/(2*A) #H11
  
  return(((airpres-exp(52.57-6690.9/(273.15+IRBotT)-4.681*log(273.15+IRBotT)))/1013)*
           airsat/100.*0.2095*(48.998-1.335*IRBotT+0.02755*IRBotT^2-
                                 0.000322*IRBotT^3+0.000001598*IRBotT^4)*32/22.414)
}


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
             'light_level')

test_sat <- calc_air_sat(seq(2699, 3036)/100, rep(20, length(seq(2699, 3036))))
test_do <- calc_o2_conc(test_sat, rep(20, length(seq(2699, 3036))))
plot(test_do ~ seq(2699, 3036))
plot(test_do ~ test_sat)

calc_air_sat(2699, 20)

data <- read.csv(paste0(name), header = F, sep = '\t', col.names = columns)
data <- data[data$error_code %in% c(1,5),]
data$elapsed_date_time <- paste(data$date, data$elapsed_time)
data$elapsed_date_time <- strptime(data$elapsed_date_time, format = '%m/%d/%Y %H:%M:%S')
day1_times <- data$elapsed_date_time[which(data$date == "1/5/2021")]
init_time <- day1_times[order(day1_times, decreasing = F)[1]]
data$elapsed_time_decimal <- as.numeric(data$elapsed_date_time - init_time)/60 # minutes is necessary for binning into time steps

data$air_sat <- calc_air_sat(data$phase/100, data$T_bottle)
data$o2_calc <- calc_o2_conc(data$air_sat, data$T_bottle)
data$o2_calc <- data$o2_calc * 31.25 # Conversion factor in Dan's code, not sure what it's for

plot(data$o2_calc ~ data$phase)

# for(r in 1:dim(data)[1]){
#   temp_phase <- data[r, 'phase']
#   temp_T_bottle <- data[r, 'T_bottle']
#   temp_air_sat <- calc_air_sat(temp_phase, temp_T_internal)
#   temp_O2_calc <- calc_o2_conc(temp_air_sat, temp_T_internal)
#   
#   data[r, 'air_sat'] <- temp_air_sat
#   data[r, 'o2_calc'] <- temp_O2_calc
# }

## eliminate data where T is more than 1 SD from mean
data <- data[abs(data$T_internal - mean(data$T_internal)) < sd(data$T_internal),]

## Group data by 10 minute intervals
data$elapsed_rounded <- round(data$elapsed_time_decimal/10) * 10

r.rate <- as.data.frame(matrix(ncol = 4, nrow = length(unique(data$bottle_number))))
colnames(r.rate) <- c('Mean', 'Mode', 'Slope', 'Original')
row.names(r.rate) <- unique(data$bottle_number)

r.rate["7", 'Original'] <- -1.3044
r.rate["8", 'Original'] <- -1.1634
r.rate["9", 'Original'] <- -1.4347
r.rate["10", 'Original'] <- -1.3316
r.rate["11", 'Original'] <- -1.5774
r.rate["12", 'Original'] <- -1.8488

#### calculate DO consumption ####

pdf(paste0(name, '.pdf'),
    width = 6,
    height = 6)

param <- 'O2'
param <- 'o2_calc'

for(bottle in sort(unique(data$bottle_number), decreasing = F)){
  temp <- data[data$bottle_number == bottle,]

  temp.round <- as.data.frame(as.matrix(tapply(temp[,param], temp$elapsed_rounded, FUN = mean)))
  colnames(temp.round) <- c(param)
  temp.round$min <- as.numeric(row.names(temp.round))

  plot(temp.round$min, temp.round[,param],
       ylab = param,
       xlab = 'Minutes',
       main = paste('Bottle', bottle))

  temp.model <- lm(temp.round[,param] ~ temp.round$min)
  abline(temp.model)

  ## use residuals to cull data, drop data with residuals > 25 % of max

  temp.max.residual <- max(temp.model$residuals)
  temp.round <- temp.round[(temp.model$residuals/temp.max.residual) < 0.25,]
  temp.model <- lm(temp.round[,param] ~ temp.round$min)

  ## randomized comparisons
  #!!! orginal approach discards comparisons < 60 minutes

  temp.random <- vector(length = (dim(temp.round)[1] * (dim(temp.round)[1] - 1)) - 1, mode = 'numeric')

  k <- 1

  for(i in 1:dim(temp.round)[1]){
    for(j in 2:dim(temp.round)[1]){
      temp.O2 <- temp.round[i, param] - temp.round[j, param]
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

  plot(temp.round$min, temp.round[,param],
       ylab = param,
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

plot(r.rate$Mode ~ r.rate$Original)
