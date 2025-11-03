# EDM Scenario Exploration
# Reid Steele 06/09/2022

# working directory

# Libraries
library(rEDM)
library(xlsx)
library(mlegp)
library(tidyverse)
library(forecast)
library(nlme)
library(Metrics)
library(MuMIn)
library(fpp3)
library(knitr)
source('SE_funcs.R')


# Load
#load('SE.RData')

# Read in data from Mariano
cap_bm = read.csv('./All_data/cap_acoustic.csv')
colnames(cap_bm) = c('Year', 'acoustic', 'catch')

# Remove 1982 and earlier
cap_bm = cap_bm[cap_bm$Year >= 1982,]

# Make numeric
for(i in 1:ncol(cap_bm)){cap_bm[,i] = as.numeric(cap_bm[,i])}

# Filter to acoustic and catch
cap_acoustic = data.frame(cap_bm[,c('Year', 'acoustic', 'acoustic', 'catch')])

# rename columns
colnames(cap_acoustic) = c('Year', 'acoustic', 'acoustic_pc', 'catch')

# gather acoustic data range
range_acoustic = range(cap_acoustic[is.na(cap_acoustic$acoustic) == FALSE,]$Year)

# Filter years to only within range
cap_acoustic = cap_acoustic[(cap_acoustic$Year >= range_acoustic[1]) & (cap_acoustic$Year <= range_acoustic[2]),]

# set years before 1991 to NA for acoustic post-collapse column
cap_acoustic$acoustic_pc = ifelse(cap_acoustic$Year < 1991, NA, cap_acoustic$acoustic_pc)

# Read in environmental data from Fred Cyr
env = read.csv('./All_data/NL_climate_index_all_fields_natural_signs.csv')
ci = read.csv('./All_data/NL_climate_index.csv')
env = left_join(env, ci, by = "Year")

# Keep normal nlci
env$CI.annual = env$Climate.index

# Calculate cumulative NLCI
env[!is.na(env$Climate.index), 'Climate.index'] =
  cumsum(env[!is.na(env$Climate.index), 'Climate.index'])

# Standardize cumulative NLCI
env[!is.na(env$Climate.index), 'Climate.index'] = (env[!is.na(env$Climate.index), 'Climate.index'] -
                                                     mean(env[!is.na(env$Climate.index), 'Climate.index'])) /
  sd(env[!is.na(env$Climate.index), 'Climate.index'])

# # Calculate smoothed NLCI
# env[!is.na(env$Climate.index), 'Climate.index'] = 
#  as.numeric(ma(env[!is.na(env$Climate.index), 'Climate.index'], 3))

# # Calculate smoothed NLCI, backwards only
# env$Climate.index = as.numeric(stats::filter(env$Climate.index, rep(1,3), sides = 1)/3)



# # Adjust year for winter NAO and Sea Ice
# env$Wint..NAO = c(env$Wint..NAO[-1], NA)
# env$Sea.Ice = c(env$Sea.Ice[-1], NA)

# add summer_nao

# # Read in NAO data
# nao = read.table('./All_data/NAO.txt'); colnames(nao) = c('year', month.abb)
# 
# # Filter relevant years
# nao = nao[nao$year %in% env$Year,]
# 
# # take summer mean NAO
# env$summer_nao = apply(nao[,c(7:9)], 1, mean)

# # Calculate smoothed environmental variables
# for(i in 1:ncol(env)){
#   env[,i] = as.numeric(stats::filter(env[,i], rep(1,3), sides = 1)/3)
# }

# Read in tice
tice = read.csv('./All_data/ice-m1-2020.csv')

# Reduce to tice
tice = tice[,c('year', 'tice')]

# Standardize tice
tice$tice = (tice$tice - mean(tice$tice))/sd(tice$tice)

# Join tice
env = left_join(env, tice, by = c('Year' = 'year'))

# Read in Predator data
pred_bm = read.csv('./All_data/predator_biomass.csv') # unlogged

# Read in cod catch
cod_catch = read.csv('./All_data/cod_catch.csv'); colnames(cod_catch) = c('Year', 'Country', 'Division', 'Species', 'c_catch')

# Database cod catch
cod_catch = group_by(cod_catch, Year) %>%
  summarise(c_catch = sum(c_catch))

pred_bm = left_join(pred_bm, cod_catch, by = 'Year')

# # change stomach contents to numeric
# cap_bm$sc_cod = as.numeric(cap_bm$sc_cod)
# cap_bm$sc_turbot = as.numeric(cap_bm$sc_turbot)
# cap_bm = cap_bm[cap_bm$Year < 2021,]
# 
# # remove trawl 2J3K
# cap_bm = cap_bm[,colnames(cap_bm) != 'trawl_2J3K']

# # Log ecological data
# cap_acoustic[,-1] = log(cap_acoustic[,-1])

# Save unfilled
# cap_bm_u = cap_bm
cap_acoustic_u = cap_acoustic; colnames(cap_acoustic_u) = c('Year', 'acoustic_u', 'acoustic_pc_u', 'catch_u')

# Fill holes - mlegp

# Remove NAs
ca_noNA = cap_acoustic[is.na(cap_acoustic$acoustic_pc) == FALSE,]

# Run gaussian process
cafill = mlegp(ca_noNA$Year, ca_noNA$acoustic_pc)

# Index of holes to fill - after first year of data
fill = which(is.na(cap_acoustic$acoustic_pc))[which(is.na(cap_acoustic$acoustic_pc)) > min(which(is.na(cap_acoustic$acoustic_pc) == FALSE))]

# Fill holes with GP prediction
cap_acoustic$acoustic_pc[fill] = predict(cafill, newData = data.frame(cap_acoustic[fill, 'Year']))

# plotting data frame
cap_plot = cap_acoustic[is.na(cap_acoustic$acoustic_pc) == FALSE,]

# Plot new time series
plot(acoustic_pc ~ Year, data = cap_plot, type = 'b', ylab = 'Acoustic Index',
     col = ifelse(is.na(cap_acoustic_u[is.na(cap_acoustic$acoustic_pc) == FALSE,'acoustic_pc_u']), 'red', 'black'), pch = 16)

# Move filled holes from acoustic_pc to acoustic
cap_acoustic$acoustic = ifelse(is.na(cap_acoustic$acoustic_pc) == FALSE, cap_acoustic$acoustic_pc, cap_acoustic$acoustic)

# Remove NAs
ca_noNA = cap_acoustic[is.na(cap_acoustic$acoustic) == FALSE,]

# Run gaussian process
cafill = mlegp(ca_noNA$Year, ca_noNA$acoustic)

# Index of holes to fill - after first year of data
fill = which(is.na(cap_acoustic$acoustic))[which(is.na(cap_acoustic$acoustic)) > min(which(is.na(cap_acoustic$acoustic) == FALSE))]

# Fill holes with GP prediction
cap_acoustic$acoustic[fill] = predict(cafill, newData = data.frame(cap_acoustic[fill, 'Year']))

# plotting data frame
cap_plot = cap_acoustic[is.na(cap_acoustic$acoustic) == FALSE,]

# Plot new time series
plot(acoustic ~ Year, data = cap_plot, type = 'b', ylab = 'Acoustic Index',
     col = ifelse(is.na(cap_acoustic_u[is.na(cap_acoustic$acoustic) == FALSE,'acoustic_u']), 'red', 'black'), pch = 16)


# # Calculate smoothed acoustic index, backwards only
# cap_acoustic$acoustic = as.numeric(stats::filter(cap_acoustic$acoustic, rep(1,3), sides = 1)/3)
# cap_acoustic$acoustic_pc = as.numeric(stats::filter(cap_acoustic$acoustic_pc, rep(1,3), sides = 1)/3)

# # Standardize data
# for(i in 2:ncol(cap_acoustic)){
#   
#   # Standardize
#   cap_acoustic[,i] = (cap_acoustic[,i] - mean(na.omit(cap_acoustic[,i])))/sd(na.omit(cap_acoustic[,i]))
#   #cap_acoustic_log[,i] = (cap_acoustic_log[,i] - mean(na.omit(cap_acoustic_log[,i])))/sd(na.omit(cap_acoustic_log[,i]))
#   
#   if(i<4){
#     
#     # Plot
#     plot(cap_acoustic[,i] ~ Year, data = cap_acoustic, type = 'b', ylab = 'Standardized Acoustic Index',
#          col = ifelse(is.na(cap_acoustic_u[is.na(cap_acoustic$acoustic) == FALSE,'acoustic_u']), 'red', 'black'), pch = 16)
#     
#   }
#   
# }

###########################################################################################################################################


# All Data

# Generate data frame for environmental testing
cap_env = left_join(cap_acoustic, env, by = 'Year')

# merge capelin and predator data frames
cap_all_ay = left_join(cap_env, pred_bm, by = 'Year')

# No cod in 1982
cap_all_ay = cap_all_ay[cap_all_ay$Year > 1982,] # No cod in 1982

# Remove acoustic_pc
cap_all_ay = cap_all_ay[,colnames(cap_all_ay) != 'acoustic_pc']

# # Add effort
# source('effort_calculations.R')
# cap_all_ay = cap_all_ay %>% left_join(effort_cap_std) %>%
#   left_join(effort_cod_std)
# 
# # Add Nominal Catch
# cap_acoustic$cap_nominal = cap_acoustic$catch/cap_acoustic$acoustic
# cap_nominal = select(cap_acoustic, Year, cap_nominal)
# 
# cod_biomass_raw = read.csv('./All_data/predator_biomass_raw.csv')
# cod_biomass_raw = left_join(cod_biomass_raw, cod_catch)
# cod_biomass_raw$c_catch = cod_biomass_raw$c_catch/10000
# cod_biomass_raw$cod_nominal = cod_biomass_raw$c_catch/cod_biomass_raw$cod
# scale(cod_biomass_raw$cod_nominal)
# cod_nominal = select(cod_biomass_raw, Year, cod_nominal)
# 
# # Add to cap_all_ay
# cap_all_ay = cap_all_ay %>% left_join(cap_nominal) %>%
#   left_join(cod_nominal)

###########################################################################################################################################

# identify targets and columns
cap_cols = colnames(cap_all_ay)[2:3]
cap_tar = colnames(cap_all_ay)[-1:-3]

# # Post-Collapse
# cap_all_ay = cap_all_ay[cap_all_ay$Year >= 1991,]

# Gather embedding dimension - all years
cap_ay_ed = EmbedDimension(dataFrame = cap_all_ay, 
                           target = 'acoustic', columns = 'acoustic', 
                           lib = paste(1, nrow(cap_all_ay)), 
                           pred = paste(1, nrow(cap_all_ay)), maxE = 5)
cap_ay_E = cap_ay_ed[which.max(cap_ay_ed$rho), 'E']

# Test Nonlinearity and gather theta
cap_ay_nl = PredictNonlinear(dataFrame = cap_all_ay, 
                             target = 'acoustic', columns = 'acoustic', 
                             lib = paste(1, nrow(cap_all_ay)), E = cap_ay_E,
                             pred = paste(1, nrow(cap_all_ay)))
cap_ay_theta = cap_ay_nl[which.max(cap_ay_nl$rho), 'Theta']

# Embed data
cap_all_ay_embed = Embed(dataFrame = cap_all_ay, E=2, columns = gsub(',', '', toString(colnames(cap_all_ay)[c(-1)])))
cap_all_ay_embed = cbind(Year = cap_all_ay$Year, cap_all_ay_embed)
cap_all_ay_embed2 = Embed(dataFrame = cap_all_ay, E=3, columns = gsub(',', '', toString(colnames(cap_all_ay)[c(-1)])))
cap_all_ay_embed2 = cbind(Year = cap_all_ay$Year, cap_all_ay_embed2)

# Standardize data
for(i in 2:ncol(cap_all_ay_embed)){

  # Standardize
  cap_all_ay_embed[,i] = (cap_all_ay_embed[,i] - mean(na.omit(cap_all_ay_embed[,i])))/sd(na.omit(cap_all_ay_embed[,i]))
  #cap_all_ay_embed_log[,i] = (cap_all_ay_embed_log[,i] - mean(na.omit(cap_all_ay_embed_log[,i])))/sd(na.omit(cap_all_ay_embed_log[,i]))

}

# Standardize data
for(i in 2:ncol(cap_all_ay_embed2)){
  
  # Standardize
  cap_all_ay_embed2[,i] = (cap_all_ay_embed2[,i] - mean(na.omit(cap_all_ay_embed2[,i])))/sd(na.omit(cap_all_ay_embed2[,i]))
  
}

# Remove first row (NAs)
cap_all_ay_embed = cap_all_ay_embed[complete.cases(cap_all_ay_embed),]
cap_all_ay_embed2 = cap_all_ay_embed2[complete.cases(cap_all_ay_embed2),]

#############################################################################################################################################

# Run CCMs and CCM significance testing

# Run capelin CCM
ccm_cap = ccm_one(cap_all_ay, cap_ay_E, 'acoustic', colnames(cap_all_ay)[!(colnames(cap_all_ay) %in% c('acoustic', 'Year', 'c_catch', 'effort_cod', 'cod_nominal'))])

# Gather covariate columns
cov_cols = (2:ncol(cap_all_ay)); cov_cols = cov_cols[!(colnames(cap_all_ay)[cov_cols] %in% c('acoustic', 'c_catch', 'effort_cod', 'cod_nominal'))]

# Data frame to hold surrogate test results
rho_surr = matrix(nrow = 1000, ncol = length(cov_cols))
rho_surr = as.data.frame(rho_surr)

# Rename columns
colnames(rho_surr) = colnames(cap_all_ay)[cov_cols]

# Carrier for significance test results
ccm_sig_cap = rep(NA, length(cov_cols))
names(ccm_sig_cap) = colnames(cap_all_ay)[cov_cols]

# Carrier for cross-correlation values
ccm_ccf_cap = rep(NA, length(cov_cols))
names(ccm_ccf_cap) = colnames(cap_all_ay)[cov_cols]

# CCM Significance Testing
for(i in cov_cols){
  
  # Generate surrogate test data
  ccm_test_data = SurrogateData(cap_all_ay[,i], method = c('random_shuffle'), num_surr = 1000, alpha = 0)
  
  # Add year and acoustic
  ccm_test_data = cbind(cap_all_ay[,c('Year', 'acoustic')], ccm_test_data)
  
  # Run CCMs
  # Cross mapping
  for (j in 1:1000) {
    
    # Run surrogate cross map
    ccm_out = CCM(dataFrame = ccm_test_data, E = cap_ay_E, columns = "acoustic",
                  target = paste0(j), sample = 1,
                  libSizes = c((nrow(cap_all_ay) - cap_ay_E + 1), (nrow(cap_all_ay) - cap_ay_E + 1), 1))
    
    col = paste("acoustic", ":", paste0(j), sep = "")
    
    rho_surr[j, which(colnames(rho_surr) == colnames(cap_all_ay)[i])] = ccm_out[1, col]
    
  }
  
  # Enter significance result
  ccm_sig_cap[which(colnames(rho_surr) == colnames(cap_all_ay)[i])] = 
    1 - ecdf(rho_surr[,which(colnames(rho_surr) == 
                               colnames(cap_all_ay)[i])])(ccm_cap$`2on1_blue`[grep(names(ccm_sig_cap[which(colnames(rho_surr) == 
                                                                                                             colnames(cap_all_ay)[i])]), ccm_cap$test)])
  
  # Calculate cross-correlation function
  ccm_ccf_cap[which(colnames(rho_surr) == colnames(cap_all_ay)[i])] = max(abs(ccf(cap_all_ay$acoustic, cap_all_ay[,i], type = 'correlation', lag.max = 1, plot = FALSE)$acf))
  
}

# Check results
ccm_sig_cap


delta = c(0.5, 1)

# Run Deltas
# dcap_sst = se_delta(cap_all_ay_embed, cap_ay_E+1, cap_ay_theta, 'acoustic(t-0)', 'SST(t-2)', delta, tp = 2)
# dcap_nlci = se_delta(cap_all_ay_embed, cap_ay_E+1, cap_ay_theta, 'acoustic(t-0)', 'Climate.index(t-2)', delta, tp = 2)
# dcap_tice = se_delta(cap_all_ay_embed, cap_ay_E+1, cap_ay_theta, 'acoustic(t-0)', 'tice(t-2)', delta, tp = 2)
# dcap_cod = se_delta(cap_all_ay_embed, cap_ay_E+1, cap_ay_theta, 'acoustic(t-0)', 'cod(t-2)', delta, tp = 2)
# dcap_gh = se_delta(cap_all_ay_embed, cap_ay_E+1, cap_ay_theta, 'acoustic(t-0)', 'turbot(t-2)', delta, tp = 2)
# dcap_ct = se_delta(cap_all_ay_embed, cap_ay_E+1, cap_ay_theta, 'acoustic(t-0)', 'catch(t-2)',delta, tp = 2)

dcap_sst = se_delta(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'SST(t-1)', delta, tp = 1)
dcap_nlci = se_delta(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'Climate.index(t-1)', delta, tp = 1)
dcap_at = se_delta(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'Air.Temp(t-1)', delta, tp = 1)
dcap_tice = se_delta(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'tice(t-1)', delta, tp = 1)
dcap_cod = se_delta(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'cod(t-1)', delta, tp = 1)
dcap_gh = se_delta(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'turbot(t-1)', delta, tp = 1)
dcap_ct = se_delta(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'catch(t-1)',delta, tp = 1)
# dcap_ef = se_delta(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'effort_cap(t-1)',delta, tp = 1)
# dcap_nc = se_delta(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'cap_nominal(t-1)',delta, tp = 1)




# data, E, theta, target, vars, delta, tp = 1
# # plots by year
# par(mfrow = c(2,2), mar = c(4,4,1,1), oma = c(2,2,2,2))
# 
# # NLCI Time Series
# plot(Observations ~ Year, data = dcap_nlci1, col = 'black', type = 'l', lty = 2, lwd = 2, 
#      ylim = range(dcap_nlci1[,-1]), ylab = 'Capelin Index', main = 'NLCI, Lag 1')
# lines(Predictions ~ Year, data = dcap_nlci1, col = 'black', type = 'l', lty = 1, lwd = 2)
# lines(pred_up_0.5  ~ Year, data = dcap_nlci1, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_up_1  ~ Year, data = dcap_nlci1, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_0.5  ~ Year, data = dcap_nlci1, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_1  ~ Year, data = dcap_nlci1, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)
# # NLCI Time Series
# plot(Observations ~ Year, data = dcap_nlci, col = 'black', type = 'l', lty = 2, lwd = 2, 
#      ylim = range(dcap_nlci[,-1]), ylab = 'Capelin Index', main = 'NLCI, Lag 2')
# lines(Predictions ~ Year, data = dcap_nlci, col = 'black', type = 'l', lty = 1, lwd = 2)
# lines(pred_up_0.5  ~ Year, data = dcap_nlci, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_up_1  ~ Year, data = dcap_nlci, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_0.5  ~ Year, data = dcap_nlci, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_1  ~ Year, data = dcap_nlci, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)
# 
# # NLCI Scatter
# plot(dcap_nlci1$delta_0.5 ~ cap_all_ay_embed$`Climate.index(t-1)`, col = 'blue', 
#      pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
#      xlab = 'NLCI', ylab = 'dcap/dNLCI')
# points(dcap_nlci1$delta_0.75 ~ cap_all_ay_embed$`Climate.index(t-1)`, col = 'green', 
#        pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
# points(dcap_nlci1$delta_1 ~ cap_all_ay_embed$`Climate.index(t-1)`, col = 'red', 
#        pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
# #text(cap_all_ay_embed$`Climate.index(t-1)`, dcap_nlci1/delta, labels = cap_all_ay_embed$Year)
# abline(h = 0)
# # NLCI Scatter
# plot(dcap_nlci$delta_0.5 ~ cap_all_ay_embed$`Climate.index(t-1)`, col = 'blue', 
#      pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
#      xlab = 'NLCI', ylab = 'dcap/dNLCI')
# points(dcap_nlci$delta_0.75 ~ cap_all_ay_embed$`Climate.index(t-1)`, col = 'green', 
#        pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
# points(dcap_nlci$delta_1 ~ cap_all_ay_embed$`Climate.index(t-1)`, col = 'red', 
#        pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
# #text(cap_all_ay_embed$`Climate.index(t-1)`, dcap_nlci/delta, labels = cap_all_ay_embed$Year)
# abline(h = 0)


# Boxplots
par(mfrow = c(3,1), mar = c(0,4,0,1), oma = c(4,1,1,1), cex = 1.2)
boxplot(dcap_sst$delta_0.5, dcap_nlci$delta_0.5, dcap_tice$delta_0.5, dcap_cod$delta_0.5, dcap_gh$delta_0.5, dcap_ct$delta_0.5,
        names = NA, xaxt = 'n', border = 'blue', col = 'white', ylim = c(-2, 3))
abline(h = 0, lty = 2)
boxplot(dcap_sst$delta_0.5, dcap_nlci$delta_0.5, dcap_tice$delta_0.5, dcap_cod$delta_0.5, dcap_gh$delta_0.5, dcap_ct$delta_0.5,
        names = NA, xaxt = 'n', border = 'green', col = 'white', ylim = c(-2, 3), ylab = 'Delta')
abline(h = 0, lty = 2)
boxplot(dcap_sst$delta_0.5, dcap_nlci$delta_0.5, dcap_tice$delta_0.5, dcap_cod$delta_0.5, dcap_gh$delta_0.5, dcap_ct$delta_0.5,
        names = c('SST', 'NLCI', 'Ice Timing', 'Cod', 'Halibut', 'Catch'), border = 'red', col = 'white', ylim = c(-2, 3))
abline(h = 0, lty = 2)

# plots by year
par(mfrow = c(2,1), mar = c(4,4,1,1), oma = c(2,2,2,2))

# Gather useful elements
max_delta = max(delta)
min_delta = min(delta)

# SST Time Series
plot(Observations ~ Year, data = dcap_sst, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_sst[,-1]), ylab = 'Capelin Index', main = 'SST')
lines(Predictions ~ Year, data = dcap_sst, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_sst, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_sst, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_sst, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_sst, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# SST Scatterplot
plot(dcap_sst$delta_0.5 ~ cap_all_ay_embed$`SST(t-1)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
     xlab = 'SST', ylab = 'dcap/dSST')
points(dcap_sst$delta_0.75 ~ cap_all_ay_embed$`SST(t-1)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
points(dcap_sst$delta_1 ~ cap_all_ay_embed$`SST(t-1)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
#text(cap_all_ay_embed$`SST(t-1)`, dcap_sst/delta, labels = cap_all_ay_embed$Year)
abline(h = 0)


# NLCI Time Series
plot(Observations ~ Year, data = dcap_nlci, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_nlci[,-1]), ylab = 'Capelin Index', main = 'NLCI')
lines(Predictions ~ Year, data = dcap_nlci, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_nlci, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_nlci, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_nlci, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_nlci, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# NLCI Scatter
plot(dcap_nlci$delta_0.5 ~ cap_all_ay_embed$`Climate.index(t-1)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
     xlab = 'NLCI', ylab = 'dcap/dNLCI')
points(dcap_nlci$delta_0.75 ~ cap_all_ay_embed$`Climate.index(t-1)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
points(dcap_nlci$delta_1 ~ cap_all_ay_embed$`Climate.index(t-1)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
#text(cap_all_ay_embed$`Climate.index(t-1)`, dcap_nlci/delta, labels = cap_all_ay_embed$Year)
abline(h = 0)


# tice Time Series
plot(Observations ~ Year, data = dcap_tice, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_tice[,-1]), ylab = 'Capelin Index', main = 'tice')
lines(Predictions ~ Year, data = dcap_tice, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_tice, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_tice, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_tice, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_tice, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# tice Scatterplot
plot(dcap_tice$delta_0.5 ~ cap_all_ay_embed$`tice(t-1)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
     xlab = 'Ice Timing', ylab = 'dcap/dtice')
points(dcap_tice$delta_0.75 ~ cap_all_ay_embed$`tice(t-1)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
points(dcap_tice$delta_1 ~ cap_all_ay_embed$`tice(t-1)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
#text(cap_all_ay_embed$`tice(t-1)`, dcap_tice/delta, labels = cap_all_ay_embed$Year)
abline(h = 0)


# Cod Time Series
plot(Observations ~ Year, data = dcap_cod, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_cod[,-1]), ylab = 'Capelin Index', main = 'Cod')
lines(Predictions ~ Year, data = dcap_cod, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_cod, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_cod, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_cod, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_cod, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Cod Scatterplot
plot(dcap_cod$delta_0.5 ~ cap_all_ay_embed$`cod(t-1)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
     xlab = 'Cod', ylab = 'dcap/dcod')
points(dcap_cod$delta_0.75 ~ cap_all_ay_embed$`cod(t-1)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
points(dcap_cod$delta_1 ~ cap_all_ay_embed$`cod(t-1)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
#text(cap_all_ay_embed$`cod(t-1)`, dcap_cod/delta, labels = cap_all_ay_embed$Year)
abline(h = 0)


# Greenland Halibut Time Series
plot(Observations ~ Year, data = dcap_gh, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_gh[,-1]), ylab = 'Capelin Index', main = 'Greenland Halibut')
lines(Predictions ~ Year, data = dcap_gh, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_gh, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_gh, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_gh, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_gh, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Greenland Halibut Scatterplot
plot(dcap_gh$delta_0.5 ~ cap_all_ay_embed$`turbot(t-1)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
     xlab = 'Greenland Halibut', ylab = 'dcap/dGH')
points(dcap_gh$delta_0.75 ~ cap_all_ay_embed$`turbot(t-1)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
points(dcap_gh$delta_1 ~ cap_all_ay_embed$`turbot(t-1)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
#text(cap_all_ay_embed$`turbot(t-1)`, dcap_gh/delta, labels = cap_all_ay_embed$Year)
abline(h = 0)


# Catch Time Series
plot(Observations ~ Year, data = dcap_ct, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_ct[,-1]), ylab = 'Capelin Index', main = 'Catch')
lines(Predictions ~ Year, data = dcap_ct, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_ct, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_ct, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_ct, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_ct, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Catch Scatterplot
plot(dcap_ct$delta_0.5 ~ cap_all_ay_embed$`catch(t-1)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
     xlab = 'Catch', ylab = 'dcap/dcatch')
points(dcap_ct$delta_0.75 ~ cap_all_ay_embed$`catch(t-1)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
points(dcap_ct$delta_1 ~ cap_all_ay_embed$`catch(t-1)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
#text(cap_all_ay_embed$`catch(t-1)`, dcap_ct/delta, labels = cap_all_ay_embed$Year)
abline(h = 0)


# # effort Time Series
# plot(Observations ~ Year, data = dcap_ef, col = 'black', type = 'l', lty = 2, lwd = 2, 
#      ylim = range(dcap_ef[,-1]), ylab = 'Capelin Index', main = 'Effort')
# lines(Predictions ~ Year, data = dcap_ef, col = 'black', type = 'l', lty = 1, lwd = 2)
# lines(pred_up_0.5  ~ Year, data = dcap_ef, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_up_1  ~ Year, data = dcap_ef, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_0.5  ~ Year, data = dcap_ef, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_1  ~ Year, data = dcap_ef, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)
# 
# # effort Scatterplot
# plot(dcap_ef$delta_0.5 ~ cap_all_ay_embed$`effort_cap(t-1)`, col = 'blue', 
#      pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
#      xlab = 'effort', ylab = 'dcap/deffort')
# # points(dcap_ct$delta_0.75 ~ cap_all_ay_embed$`catch(t-1)`, col = 'green', 
# #        pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
# points(dcap_ef$delta_1 ~ cap_all_ay_embed$`effort_cap(t-1)`, col = 'red', 
#        pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
# #text(cap_all_ay_embed$`catch(t-1)`, dcap_ct/delta, labels = cap_all_ay_embed$Year)
# abline(h = 0)
# 
# 
# # nominal catch Time Series
# plot(Observations ~ Year, data = dcap_nc, col = 'black', type = 'l', lty = 2, lwd = 2, 
#      ylim = range(dcap_nc[,-1]), ylab = 'Capelin Index', main = 'Nominal Catch')
# lines(Predictions ~ Year, data = dcap_nc, col = 'black', type = 'l', lty = 1, lwd = 2)
# lines(pred_up_0.5  ~ Year, data = dcap_nc, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_up_1  ~ Year, data = dcap_nc, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_0.5  ~ Year, data = dcap_nc, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_1  ~ Year, data = dcap_nc, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)
# 
# # effort Scatterplot
# plot(dcap_nc$delta_0.5 ~ cap_all_ay_embed$`effort_cap(t-1)`, col = 'blue', 
#      pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1),
#      xlab = 'effort', ylab = 'dcap/dnominal')
# # points(dcap_ct$delta_0.75 ~ cap_all_ay_embed$`catch(t-1)`, col = 'green', 
# #        pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
# points(dcap_nc$delta_1 ~ cap_all_ay_embed$`effort_cap(t-1)`, col = 'red', 
#        pch = ifelse(cap_all_ay_embed$Year > 1990, 16, 1))
# #text(cap_all_ay_embed$`catch(t-1)`, dcap_ct/delta, labels = cap_all_ay_embed$Year)
# abline(h = 0)





# Tp = 2

dcap_sst2 = se_delta(cap_all_ay_embed2, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'SST(t-2)', delta, tp = 2)
dcap_nlci2 = se_delta(cap_all_ay_embed2, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'Climate.index(t-2)', delta, tp = 2)
dcap_tice2 = se_delta(cap_all_ay_embed2, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'tice(t-2)', delta, tp = 2)
dcap_cod2 = se_delta(cap_all_ay_embed2, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'cod(t-2)', delta, tp = 2)
dcap_gh2 = se_delta(cap_all_ay_embed2, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'turbot(t-2)', delta, tp = 2)
dcap_ct2 = se_delta(cap_all_ay_embed2, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'catch(t-2)',delta, tp = 2)

# Boxplots
par(mfrow = c(3,1), mar = c(0,4,0,1), oma = c(4,1,1,1), cex = 1.2)
boxplot(dcap_sst2$delta_0.5, dcap_nlci2$delta_0.5, dcap_tice2$delta_0.5, dcap_cod2$delta_0.5, dcap_gh2$delta_0.5, dcap_ct2$delta_0.5,
        names = NA, xaxt = 'n', border = 'blue', col = 'white', ylim = c(-2, 3))
abline(h = 0, lty = 2)
boxplot(dcap_sst2$delta_0.5, dcap_nlci2$delta_0.5, dcap_tice2$delta_0.5, dcap_cod2$delta_0.5, dcap_gh2$delta_0.5, dcap_ct2$delta_0.5,
        names = NA, xaxt = 'n', border = 'green', col = 'white', ylim = c(-2, 3), ylab = 'Delta')
abline(h = 0, lty = 2)
boxplot(dcap_sst2$delta_0.5, dcap_nlci2$delta_0.5, dcap_tice2$delta_0.5, dcap_cod2$delta_0.5, dcap_gh2$delta_0.5, dcap_ct2$delta_0.5,
        names = c('SST', 'NLCI', 'Ice Timing', 'Cod', 'Halibut', 'Catch'), border = 'red', col = 'white', ylim = c(-2, 3))
abline(h = 0, lty = 2)

# plots by year
par(mfrow = c(2,1), mar = c(4,4,1,1), oma = c(2,2,2,2))

# Gather useful elements
max_delta = max(delta)
min_delta = min(delta)

# SST Time Series
plot(Observations ~ Year, data = dcap_sst2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_sst2[,-1]), ylab = 'Capelin Index', main = 'SST')
lines(Predictions ~ Year, data = dcap_sst2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_sst2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_sst2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_sst2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_sst2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# SST Scatterplot
plot(dcap_sst2$delta_0.5 ~ cap_all_ay_embed2$`SST(t-2)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1),
     xlab = 'SST', ylab = 'dcap/dSST')
points(dcap_sst2$delta_0.75 ~ cap_all_ay_embed2$`SST(t-2)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
points(dcap_sst2$delta_1 ~ cap_all_ay_embed2$`SST(t-2)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
#text(cap_all_ay_embed2$`SST(t-2)`, dcap_sst2/delta, labels = cap_all_ay_embed2$Year)
abline(h = 0)


# NLCI Time Series
plot(Observations ~ Year, data = dcap_nlci2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_nlci2[,-1]), ylab = 'Capelin Index', main = 'NLCI')
lines(Predictions ~ Year, data = dcap_nlci2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_nlci2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_nlci2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_nlci2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_nlci2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# NLCI Scatter
plot(dcap_nlci2$delta_0.5 ~ cap_all_ay_embed2$`Climate.index(t-2)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1),
     xlab = 'NLCI', ylab = 'dcap/dNLCI')
points(dcap_nlci2$delta_0.75 ~ cap_all_ay_embed2$`Climate.index(t-2)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
points(dcap_nlci2$delta_1 ~ cap_all_ay_embed2$`Climate.index(t-2)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
#text(cap_all_ay_embed2$`Climate.index(t-2)`, dcap_nlci2/delta, labels = cap_all_ay_embed2$Year)
abline(h = 0)


# tice Time Series
plot(Observations ~ Year, data = dcap_tice2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_tice2[,-1]), ylab = 'Capelin Index', main = 'tice')
lines(Predictions ~ Year, data = dcap_tice2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_tice2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_tice2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_tice2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_tice2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# tice Scatterplot
plot(dcap_tice2$delta_0.5 ~ cap_all_ay_embed2$`tice(t-2)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1),
     xlab = 'Ice Timing', ylab = 'dcap/dtice')
points(dcap_tice2$delta_0.75 ~ cap_all_ay_embed2$`tice(t-2)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
points(dcap_tice2$delta_1 ~ cap_all_ay_embed2$`tice(t-2)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
#text(cap_all_ay_embed2$`tice(t-2)`, dcap_tice2/delta, labels = cap_all_ay_embed2$Year)
abline(h = 0)


# Cod Time Series
plot(Observations ~ Year, data = dcap_cod2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_cod2[,-1]), ylab = 'Capelin Index', main = 'Cod')
lines(Predictions ~ Year, data = dcap_cod2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_cod2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_cod2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_cod2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_cod2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Cod Scatterplot
plot(dcap_cod2$delta_0.5 ~ cap_all_ay_embed2$`cod(t-2)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1),
     xlab = 'Cod', ylab = 'dcap/dcod')
points(dcap_cod2$delta_0.75 ~ cap_all_ay_embed2$`cod(t-2)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
points(dcap_cod2$delta_1 ~ cap_all_ay_embed2$`cod(t-2)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
#text(cap_all_ay_embed2$`cod(t-2)`, dcap_cod2/delta, labels = cap_all_ay_embed2$Year)
abline(h = 0)


# Greenland Halibut Time Series
plot(Observations ~ Year, data = dcap_gh2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_gh2[,-1]), ylab = 'Capelin Index', main = 'Greenland Halibut')
lines(Predictions ~ Year, data = dcap_gh2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_gh2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_gh2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_gh2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_gh2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Greenland Halibut Scatterplot
plot(dcap_gh2$delta_0.5 ~ cap_all_ay_embed2$`turbot(t-2)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1),
     xlab = 'Greenland Halibut', ylab = 'dcap/dGH')
points(dcap_gh2$delta_0.75 ~ cap_all_ay_embed2$`turbot(t-2)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
points(dcap_gh2$delta_1 ~ cap_all_ay_embed2$`turbot(t-2)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
#text(cap_all_ay_embed2$`turbot(t-2)`, dcap_gh2/delta, labels = cap_all_ay_embed2$Year)
abline(h = 0)


# Catch Time Series
plot(Observations ~ Year, data = dcap_ct2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcap_ct2[,-1]), ylab = 'Capelin Index', main = 'Catch')
lines(Predictions ~ Year, data = dcap_ct2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcap_ct2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcap_ct2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcap_ct2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcap_ct2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Catch Scatterplot
plot(dcap_ct2$delta_0.5 ~ cap_all_ay_embed2$`catch(t-2)`, col = 'blue', 
     pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1),
     xlab = 'Catch', ylab = 'dcap/dcatch')
points(dcap_ct2$delta_0.75 ~ cap_all_ay_embed2$`catch(t-2)`, col = 'green', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
points(dcap_ct2$delta_1 ~ cap_all_ay_embed2$`catch(t-2)`, col = 'red', 
       pch = ifelse(cap_all_ay_embed2$Year > 1990, 16, 1))
#text(cap_all_ay_embed2$`catch(t-2)`, dcap_ct2/delta, labels = cap_all_ay_embed2$Year)
abline(h = 0)





####################################################################################################################################################################################

# Extend to Cod

# Gather embedding dimension - all years
cod_ay_ed = EmbedDimension(dataFrame = cap_all_ay, 
                           target = 'cod', columns = 'cod', 
                           lib = paste(1, nrow(cap_all_ay)), 
                           pred = paste(1, nrow(cap_all_ay)), maxE = 5)
cod_ay_E = cod_ay_ed[which.max(cod_ay_ed$rho), 'E']

# Test Nonlinearity and gather theta
cod_ay_nl = PredictNonlinear(dataFrame = cap_all_ay, 
                             target = 'cod', columns = 'cod', 
                             lib = paste(1, nrow(cap_all_ay)), E = cod_ay_E,
                             pred = paste(1, nrow(cap_all_ay)))
cod_ay_theta = cod_ay_nl[which.max(cod_ay_nl$rho), 'Theta']

# Run cod CCM
ccm_cod = ccm_one(cap_all_ay, cod_ay_E, 'cod', colnames(cap_all_ay)[!(colnames(cap_all_ay) %in% c('cod', 'Year', 'catch', 'effort_cap', 'cap_nominal'))])

# Gather covariate columns
cov_cols = (2:ncol(cap_all_ay)); cov_cols = cov_cols[!(colnames(cap_all_ay)[cov_cols] %in% c('cod', 'catch', 'effort_cap', 'cap_nominal'))]

# Data frame to hold surrogate test results
rho_surr = matrix(nrow = 1000, ncol = length(cov_cols))
rho_surr = as.data.frame(rho_surr)

# Rename columns
colnames(rho_surr) = colnames(cap_all_ay)[cov_cols]

# Carrier for significance test results
ccm_sig_cod = rep(NA, length(cov_cols))
names(ccm_sig_cod) = colnames(cap_all_ay)[cov_cols]

# Carrier for cross-correlation values
ccm_ccf_cod = rep(NA, length(cov_cols))
names(ccm_ccf_cod) = colnames(cap_all_ay)[cov_cols]

# CCM Significance Testing
for(i in cov_cols){
  
  # Generate surrogate test data
  ccm_test_data = SurrogateData(cap_all_ay[,i], method = c('ebisuzaki'), num_surr = 1000, alpha = 0)
  
  # Add year and acoustic
  ccm_test_data = cbind(cap_all_ay[,c('Year', 'cod')], ccm_test_data)
  
  # Run CCMs
  # Cross mapping
  for (j in 1:1000) {
    
    # Run surrogate cross map
    ccm_out = CCM(dataFrame = ccm_test_data, E = cap_ay_E, columns = "cod",
                  target = paste0(j), sample = 1,
                  libSizes = c((nrow(cap_all_ay) - cap_ay_E + 1), (nrow(cap_all_ay) - cap_ay_E + 1), 1))
    
    col = paste("cod", ":", paste0(j), sep = "")
    
    rho_surr[j, which(colnames(rho_surr) == colnames(cap_all_ay)[i])] = ccm_out[1, col]
    
  }
  
  # Enter significance result
  ccm_sig_cod[which(colnames(rho_surr) == colnames(cap_all_ay)[i])] = 
    1 - ecdf(rho_surr[,which(colnames(rho_surr) == 
                               colnames(cap_all_ay)[i])])(ccm_cod$`2on1_blue`[grep(names(ccm_sig_cod[which(colnames(rho_surr) == 
                                                                                                                colnames(cap_all_ay)[i])]), ccm_cod$test)])
  
  # Calculate cross-correlation function
  ccm_ccf_cod[which(colnames(rho_surr) == colnames(cap_all_ay)[i])] = max(abs(ccf(cap_all_ay$cod, cap_all_ay[,i], type = 'correlation', lag.max = 1, plot = FALSE)$acf))
  
}

# Check results
ccm_sig_cod

# Add necessary lags
cod_all_ay_embed = Embed(dataFrame = cap_all_ay, E=cod_ay_E+1, columns = gsub(',', '', toString(colnames(cap_all_ay)[-1])))
cod_all_ay_embed = cbind(Year = cap_all_ay$Year, cod_all_ay_embed)
cod_all_ay_embed2 = Embed(dataFrame = cap_all_ay, E=cod_ay_E+2, columns = gsub(',', '', toString(colnames(cap_all_ay)[-1])))
cod_all_ay_embed2 = cbind(Year = cap_all_ay$Year, cod_all_ay_embed2)

# Standardize data
for(i in 2:ncol(cod_all_ay_embed)){
  
  # Standardize
  cod_all_ay_embed[,i] = (cod_all_ay_embed[,i] - mean(na.omit(cod_all_ay_embed[,i])))/sd(na.omit(cod_all_ay_embed[,i]))
  
}

# Standardize data
for(i in 2:ncol(cod_all_ay_embed2)){
  
  # Standardize
  cod_all_ay_embed2[,i] = (cod_all_ay_embed2[,i] - mean(na.omit(cod_all_ay_embed2[,i])))/sd(na.omit(cod_all_ay_embed2[,i]))
  
}


# Remove NA rows
cod_all_ay_embed = cod_all_ay_embed[complete.cases(cod_all_ay_embed),]
cod_all_ay_embed2 = cod_all_ay_embed2[complete.cases(cod_all_ay_embed2),]


# Tp = 1

dcod_sst = se_delta(cod_all_ay_embed, cod_ay_E-1, cod_ay_theta, 'cod(t-0)', 
                    c('SST(t-1)'), delta, tp=1)
dcod_nlci = se_delta(cod_all_ay_embed, cod_ay_E-1, cod_ay_theta, 'cod(t-0)', 
                     c('Climate.index(t-1)'), delta, tp=1)
dcod_bt = se_delta(cod_all_ay_embed, cod_ay_E-1, cod_ay_theta, 'cod(t-0)', 
                   c('Bottom.T(t-1)'), delta, tp=1)
dcod_at = se_delta(cod_all_ay_embed, cod_ay_E-1, cod_ay_theta, 'cod(t-0)', 
                   c('Air.Temp(t-1)'), delta, tp=1)
dcod_cap = se_delta(cod_all_ay_embed, cod_ay_E-1, cod_ay_theta, 'cod(t-0)', 
                    c('acoustic(t-1)'), delta, tp=1)
dcod_gh = se_delta(cod_all_ay_embed, cod_ay_E-1, cod_ay_theta, 'cod(t-0)', 
                   c('turbot(t-1)'), delta, tp=1)
dcod_ct = se_delta(cod_all_ay_embed, cod_ay_E-1, cod_ay_theta, 'cod(t-0)',  
                   c('c_catch(t-1)'), delta, tp=1)
# dcod_ef = se_delta(cod_all_ay_embed, cod_ay_E-1, cod_ay_theta, 'cod(t-0)',  
#                    c('effort_cod(t-1)'), delta, tp=1)
# dcod_nc = se_delta(cod_all_ay_embed, cod_ay_E-1, cod_ay_theta, 'cod(t-0)',  
#                    c('cod_nominal(t-1)'), delta, tp=1)


# Boxplots
par(mfrow = c(3,1), mar = c(0,4,0,1), oma = c(4,1,1,1), cex = 1.2)
boxplot(dcod_sst$delta_0.5, dcod_nlci$delta_0.5, dcod_bt$delta_0.5, dcod_at$delta_0.5, dcod_cap$delta_0.5, dcod_gh$delta_0.5,
        names = NA, xaxt = 'n', border = 'blue', col = 'white', ylim = c(-0.2, 2))
abline(h = 0, lty = 2)
boxplot(dcod_sst$delta_0.75, dcod_nlci$delta_0.75, dcod_bt$delta_0.75, dcod_at$delta_0.75, dcod_cap$delta_0.75, dcod_gh$delta_0.75,
        names = NA, xaxt = 'n', border = 'green', col = 'white', ylim = c(-0.2, 2), ylab = 'Delta')
abline(h = 0, lty = 2)
boxplot(dcod_sst$delta_1, dcod_nlci$delta_1, dcod_bt$delta_1, dcod_at$delta_1, dcod_cap$delta_1, dcod_gh$delta_1,
        names = c('SST', 'NLCI', 'Bottom T', 'Air T', 'Capelin', 'Halibut'), border = 'red', col = 'white', ylim = c(-0.2, 2))
abline(h = 0, lty = 2)

# plots by year
par(mfrow = c(2,1), mar = c(4,4,1,1), oma = c(2,2,2,2))

# Gather useful elements
max_delta = max(delta)
min_delta = min(delta)

# SST Time series
plot(Observations ~ Year, data = dcod_sst, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_sst[,-1]), ylab = 'Cod Index', main = 'SST')
lines(Predictions ~ Year, data = dcod_sst, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_sst, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_sst, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_sst, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_sst, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# SST Scatterplot
plot(dcod_sst$delta_0.5 ~ cod_all_ay_embed$`SST(t-1)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'SST', ylab = 'dcod/dSST')
points(dcod_sst$delta_0.75 ~ cod_all_ay_embed$`SST(t-1)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_sst$delta_1 ~ cod_all_ay_embed$`SST(t-1)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`SST(t-1)`, dcod_sst/delta, labels = cod_all_ay_embed$Year)
abline(h = 0)


# Climate index Time Series
plot(Observations ~ Year, data = dcod_nlci, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_nlci[,-1]), ylab = 'Cod Index', main = 'NLCI')
lines(Predictions ~ Year, data = dcod_nlci, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_nlci, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_nlci, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_nlci, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_nlci, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Climate index scatterplot
plot(dcod_nlci$delta_0.5 ~ cod_all_ay_embed$`Climate.index(t-1)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'NLCI', ylab = 'dcod/dNLCI')
points(dcod_nlci$delta_0.75 ~ cod_all_ay_embed$`Climate.index(t-1)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_nlci$delta_1 ~ cod_all_ay_embed$`Climate.index(t-1)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`Climate.index(t-1)`, dcod_nlci$delta_0.5, labels = cod_all_ay_embed$Year)
abline(h = 0)


# Bottom Temperature Time Series
plot(Observations ~ Year, data = dcod_bt, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_bt[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_bt, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_bt, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_bt, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_bt, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_bt, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Bottom Temperature Scatterplot
plot(dcod_bt$delta_0.5 ~ cod_all_ay_embed$`Bottom.T(t-1)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'Bottom Temperature', ylab = 'dcod/dBT')
points(dcod_bt$delta_0.75 ~ cod_all_ay_embed$`Bottom.T(t-1)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_bt$delta_1 ~ cod_all_ay_embed$`Bottom.T(t-1)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`Bottom.T(t-1)`, dcod_bt/delta, labels = cod_all_ay_embed$Year)
abline(h = 0)


# Air Temperature Time Series
plot(Observations ~ Year, data = dcod_at, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_at[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_at, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_at, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_at, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_at, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_at, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Air Temperature Scatterplot
plot(dcod_at$delta_0.5 ~ cod_all_ay_embed$`Air.Temp(t-1)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'Air Temperature', ylab = 'dcod/dAT')
points(dcod_at$delta_0.75 ~ cod_all_ay_embed$`Air.Temp(t-1)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_at$delta_1 ~ cod_all_ay_embed$`Air.Temp(t-1)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`Air.Temp(t-1)`, dcod_at/delta, labels = cod_all_ay_embed$Year)
abline(h = 0)


# Capelin Time Series
plot(Observations ~ Year, data = dcod_cap, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_cap[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_cap, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_cap, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_cap, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_cap, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_cap, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Capelin Scatterplot
plot(dcod_cap$delta_0.5 ~ cod_all_ay_embed$`acoustic(t-1)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'Capelin', ylab = 'dcod/dcap')
points(dcod_cap$delta_0.75 ~ cod_all_ay_embed$`acoustic(t-1)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_cap$delta_1 ~ cod_all_ay_embed$`acoustic(t-1)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`acoustic(t-1)`, dcod_cap/delta, labels = cod_all_ay_embed$Year)
abline(h = 0)


# Greenland Halibut Time Series
plot(Observations ~ Year, data = dcod_gh, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_gh[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_gh, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_gh, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_gh, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_gh, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_gh, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Greenland Halibut Scatterplot
plot(dcod_gh$delta_0.5 ~ cod_all_ay_embed$`turbot(t-1)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'Greenland Halibut', ylab = 'dcod/dGH')
points(dcod_gh$delta_0.75 ~ cod_all_ay_embed$`turbot(t-1)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_gh$delta_1 ~ cod_all_ay_embed$`turbot(t-1)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`turbot(t-1)`, dcod_gh/delta, labels = cod_all_ay_embed$Year)
abline(h = 0)


# Catch Time series
plot(Observations ~ Year, data = dcod_ct, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_ct[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_ct, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_ct, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_ct, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_ct, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_ct, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Catch Scatterplot
plot(dcod_ct$delta_0.5 ~ cod_all_ay_embed$`c_catch(t-1)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'Catch', ylab = 'dcod/dcatch')
points(dcod_ct$delta_0.75 ~ cod_all_ay_embed$`c_catch(t-1)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_ct$delta_1 ~ cod_all_ay_embed$`c_catch(t-1)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`c_catch(t-1)`, dcod_ct/delta, labels = cod_all_ay_embed$Year)
abline(h = 0)


# effort Time series
plot(Observations ~ Year, data = dcod_ef, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_ef[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_ef, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_ef, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_ef, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_ef, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_ef, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# effort Scatterplot
plot(dcod_ef$delta_0.5 ~ cod_all_ay_embed$`effort_cod(t-1)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'Effort', ylab = 'dcod/dcatch')
# points(dcod_ef$delta_0.75 ~ cod_all_ay_embed$`effort_cod(t-1)`, col = 'green', 
#        pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_ef$delta_1 ~ cod_all_ay_embed$`c_catch(t-1)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`c_catch(t-1)`, dcod_ef/delta, labels = cod_all_ay_embed$Year)
abline(h = 0)


# # nominal catch Time series
# plot(Observations ~ Year, data = dcod_nc, col = 'black', type = 'l', lty = 2, lwd = 2, 
#      ylim = range(dcod_nc[,-1]), ylab = 'Cod Index')
# lines(Predictions ~ Year, data = dcod_nc, col = 'black', type = 'l', lty = 1, lwd = 2)
# lines(pred_up_0.5  ~ Year, data = dcod_nc, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_up_1  ~ Year, data = dcod_nc, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_0.5  ~ Year, data = dcod_nc, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
# lines(pred_down_1  ~ Year, data = dcod_nc, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)
# 
# # effort Scatterplot
# plot(dcod_nc$delta_0.5 ~ cod_all_ay_embed$`effort_cod(t-1)`, col = 'blue', 
#      pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
#      xlab = 'Effort', ylab = 'dcod/dnominal')
# # points(dcod_nc$delta_0.75 ~ cod_all_ay_embed$`effort_cod(t-1)`, col = 'green', 
# #        pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
# points(dcod_nc$delta_1 ~ cod_all_ay_embed$`c_catch(t-1)`, col = 'red', 
#        pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
# #text(cod_all_ay_embed$`c_catch(t-1)`, dcod_nc/delta, labels = cod_all_ay_embed$Year)
# abline(h = 0)






# Tp = 2

dcod_sst2 = se_delta(cod_all_ay_embed2, cod_ay_E, cod_ay_theta, 'cod(t-0)', 
                    c('SST(t-2)'), delta, tp=2)
dcod_nlci2 = se_delta(cod_all_ay_embed2, cod_ay_E, cod_ay_theta, 'cod(t-0)', 
                     c('Climate.index(t-2)'), delta, tp=2)
dcod_bt2 = se_delta(cod_all_ay_embed2, cod_ay_E, cod_ay_theta, 'cod(t-0)', 
                   c('Bottom.T(t-2)'), delta, tp=2)
dcod_at2 = se_delta(cod_all_ay_embed2, cod_ay_E, cod_ay_theta, 'cod(t-0)', 
                   c('Air.Temp(t-2)'), delta, tp=2)
dcod_cap2 = se_delta(cod_all_ay_embed2, cod_ay_E, cod_ay_theta, 'cod(t-0)', 
                    c('acoustic(t-2)'), delta, tp=2)
dcod_gh2 = se_delta(cod_all_ay_embed2, cod_ay_E, cod_ay_theta, 'cod(t-0)', 
                   c('turbot(t-2)'), delta, tp=2)
dcod_ct2 = se_delta(cod_all_ay_embed2, cod_ay_E, cod_ay_theta, 'cod(t-0)',  
                   c('c_catch(t-2)'), delta, tp=2)


# Boxplots
par(mfrow = c(3,1), mar = c(0,4,0,1), oma = c(4,1,1,1), cex = 1.2)
boxplot(dcod_sst2$delta_0.5, dcod_nlci2$delta_0.5, dcod_bt2$delta_0.5, dcod_at2$delta_0.5, dcod_cap2$delta_0.5, dcod_gh2$delta_0.5, 
        names = NA, xaxt = 'n', border = 'blue', col = 'white', ylim = c(-0.2, 2))
abline(h = 0, lty = 2)
boxplot(dcod_sst2$delta_0.75, dcod_nlci2$delta_0.75, dcod_bt2$delta_0.75, dcod_at2$delta_0.75, dcod_cap2$delta_0.75, dcod_gh2$delta_0.75,
        names = NA, xaxt = 'n', border = 'green', col = 'white', ylim = c(-0.2, 2), ylab = 'Delta')
abline(h = 0, lty = 2)
boxplot(dcod_sst2$delta_1, dcod_nlci2$delta_1, dcod_bt2$delta_1, dcod_at2$delta_1, dcod_cap2$delta_1, dcod_gh2$delta_1,
        names = c('SST', 'NLCI', 'Bottom T', 'Air T', 'Capelin', 'Halibut'), border = 'red', col = 'white', ylim = c(-0.2, 2))
abline(h = 0, lty = 2)

# plots by year
par(mfrow = c(2,1), mar = c(4,4,1,1), oma = c(2,2,2,2))

# Gather useful elements
max_delta = max(delta)
min_delta = min(delta)

# SST Time series
plot(Observations ~ Year, data = dcod_sst2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_sst2[,-1]), ylab = 'Cod Index', main = 'SST')
lines(Predictions ~ Year, data = dcod_sst2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_sst2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_sst2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_sst2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_sst2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# SST Scatterplot
plot(dcod_sst2$delta_0.5 ~ cod_all_ay_embed2$`SST(t-2)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1),
     xlab = 'SST', ylab = 'dcod/dSST')
points(dcod_sst2$delta_0.75 ~ cod_all_ay_embed2$`SST(t-2)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
points(dcod_sst2$delta_1 ~ cod_all_ay_embed2$`SST(t-2)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
#text(cod_all_ay_embed2$`SST(t-2)`, dcod_sst2/delta, labels = cod_all_ay_embed2$Year)
abline(h = 0)


# Climate index Time Series
plot(Observations ~ Year, data = dcod_nlci2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_nlci2[,-1]), ylab = 'Cod Index', main = 'NLCI')
lines(Predictions ~ Year, data = dcod_nlci2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_nlci2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_nlci2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_nlci2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_nlci2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Climate index scatterplot
plot(dcod_nlci2$delta_0.5 ~ cod_all_ay_embed2$`Climate.index(t-2)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1),
     xlab = 'NLCI', ylab = 'dcod/dNLCI')
points(dcod_nlci2$delta_0.75 ~ cod_all_ay_embed2$`Climate.index(t-2)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
points(dcod_nlci2$delta_1 ~ cod_all_ay_embed2$`Climate.index(t-2)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
#text(cod_all_ay_embed2$`Climate.index(t-2)`, dcod_nlci2$delta_0.5, labels = cod_all_ay_embed2$Year)
abline(h = 0)


# Bottom Temperature Time Series
plot(Observations ~ Year, data = dcod_bt2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_bt2[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_bt2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_bt2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_bt2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_bt2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_bt2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Bottom Temperature Scatterplot
plot(dcod_bt2$delta_0.5 ~ cod_all_ay_embed2$`Bottom.T(t-2)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1),
     xlab = 'Bottom Temperature', ylab = 'dcod/dBT')
points(dcod_bt2$delta_0.75 ~ cod_all_ay_embed2$`Bottom.T(t-2)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
points(dcod_bt2$delta_1 ~ cod_all_ay_embed2$`Bottom.T(t-2)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
#text(cod_all_ay_embed2$`Bottom.T(t-2)`, dcod_bt2/delta, labels = cod_all_ay_embed2$Year)
abline(h = 0)


# Air Temperature Time Series
plot(Observations ~ Year, data = dcod_at2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_at2[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_at2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_at2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_at2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_at2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_at2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Air Temperature Scatterplot
plot(dcod_at2$delta_0.5 ~ cod_all_ay_embed2$`Air.Temp(t-2)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1),
     xlab = 'Air Temperature', ylab = 'dcod/dAT')
points(dcod_at2$delta_0.75 ~ cod_all_ay_embed2$`Air.Temp(t-2)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
points(dcod_at2$delta_1 ~ cod_all_ay_embed2$`Air.Temp(t-2)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
#text(cod_all_ay_embed2$`Air.Temp(t-2)`, dcod_at2/delta, labels = cod_all_ay_embed2$Year)
abline(h = 0)


# Capelin Time Series
plot(Observations ~ Year, data = dcod_cap2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_cap2[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_cap2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_cap2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_cap2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_cap2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_cap2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Capelin Scatterplot
plot(dcod_cap2$delta_0.5 ~ cod_all_ay_embed2$`acoustic(t-2)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1),
     xlab = 'Capelin', ylab = 'dcod/dcap')
points(dcod_cap2$delta_0.75 ~ cod_all_ay_embed2$`acoustic(t-2)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
points(dcod_cap2$delta_1 ~ cod_all_ay_embed2$`acoustic(t-2)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
#text(cod_all_ay_embed2$`acoustic(t-2)`, dcod_cap2/delta, labels = cod_all_ay_embed2$Year)
abline(h = 0)


# Greenland Halibut Time Series
plot(Observations ~ Year, data = dcod_gh2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_gh2[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_gh2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_gh2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_gh2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_gh2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_gh2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Greenland Halibut Scatterplot
plot(dcod_gh2$delta_0.5 ~ cod_all_ay_embed2$`turbot(t-2)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1),
     xlab = 'Greenland Halibut', ylab = 'dcod/dGH')
points(dcod_gh2$delta_0.75 ~ cod_all_ay_embed2$`turbot(t-2)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
points(dcod_gh2$delta_1 ~ cod_all_ay_embed2$`turbot(t-2)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
#text(cod_all_ay_embed2$`turbot(t-2)`, dcod_gh2/delta, labels = cod_all_ay_embed2$Year)
abline(h = 0)


# Catch Time series
plot(Observations ~ Year, data = dcod_ct2, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_ct2[,-1]), ylab = 'Cod Index')
lines(Predictions ~ Year, data = dcod_ct2, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_ct2, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_ct2, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_ct2, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_ct2, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Catch Scatterplot
plot(dcod_ct2$delta_0.5 ~ cod_all_ay_embed2$`c_catch(t-2)`, col = 'blue', 
     pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1),
     xlab = 'Catch', ylab = 'dcod/dcatch')
points(dcod_ct2$delta_0.75 ~ cod_all_ay_embed2$`c_catch(t-2)`, col = 'green', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
points(dcod_ct2$delta_1 ~ cod_all_ay_embed2$`c_catch(t-2)`, col = 'red', 
       pch = ifelse(cod_all_ay_embed2$Year > 1991, 16, 1))
#text(cod_all_ay_embed2$`c_catch(t-2)`, dcod_ct2/delta, labels = cod_all_ay_embed2$Year)
abline(h = 0)


####################################################################################################################################################################################

# Capelin - Cod multivariate


# calculate delta
dcod_sst_c = se_delta_c(cod_all_ay_embed, dcap_sst, cod_ay_E, cod_ay_theta, 'cod(t-0)', 'acoustic(t-1)', 'SST(t-1)', delta)
dcod_nlci_c = se_delta_c(cod_all_ay_embed, dcap_nlci, cod_ay_E, cod_ay_theta, 'cod(t-0)', 'acoustic(t-1)', 'Climate.index(t-1)', delta)




# SST Time series
plot(Observations ~ Year, data = dcod_sst_c, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_sst_c[,-1]), ylab = 'Cod Index', main = 'SST')
lines(Predictions ~ Year, data = dcod_sst_c, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_sst_c, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_sst_c, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_sst_c, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_sst_c, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# SST Scatterplot
plot(dcod_sst_c$delta_0.5 ~ cod_all_ay_embed$`SST(t-1)`[cod_all_ay_embed$Year %in% dcod_sst_c$Year], col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'SST', ylab = 'dcod/dSST')
points(dcod_sst_c$delta_0.75 ~ cod_all_ay_embed$`SST(t-1)`[cod_all_ay_embed$Year %in% dcod_sst_c$Year], col = 'green', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_sst_c$delta_1 ~ cod_all_ay_embed$`SST(t-1)`[cod_all_ay_embed$Year %in% dcod_sst_c$Year], col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`SST(t-1)`, dcod_sst_c/delta, labels = cod_all_ay_embed$Year)
abline(h = 0)



# Climate index Time Series
plot(Observations ~ Year, data = dcod_nlci_c, col = 'black', type = 'l', lty = 2, lwd = 2, 
     ylim = range(dcod_nlci_c[,-1]), ylab = 'Cod Index', main = 'NLCI')
lines(Predictions ~ Year, data = dcod_nlci_c, col = 'black', type = 'l', lty = 1, lwd = 2)
lines(pred_up_0.5  ~ Year, data = dcod_nlci_c, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_up_1  ~ Year, data = dcod_nlci_c, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
lines(pred_down_0.5  ~ Year, data = dcod_nlci_c, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
lines(pred_down_1  ~ Year, data = dcod_nlci_c, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)

# Climate index scatterplot
plot(dcod_nlci_c$delta_0.5 ~ cod_all_ay_embed$`Climate.index(t-1)`[cod_all_ay_embed$Year %in% dcod_nlci_c$Year], col = 'blue', 
     pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1),
     xlab = 'NLCI', ylab = 'dcod/dNLCI')
points(dcod_nlci_c$delta_0.75 ~ cod_all_ay_embed$`Climate.index(t-1)`[cod_all_ay_embed$Year %in% dcod_nlci_c$Year], col = 'green', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
points(dcod_nlci_c$delta_1 ~ cod_all_ay_embed$`Climate.index(t-1)`[cod_all_ay_embed$Year %in% dcod_nlci_c$Year], col = 'red', 
       pch = ifelse(cod_all_ay_embed$Year > 1991, 16, 1))
#text(cod_all_ay_embed$`Climate.index(t-1)`, dcod_nlci_c$delta_0.5, labels = cod_all_ay_embed$Year)
abline(h = 0)


# Save
save.image(file = 'SE.RData')












