# EDM Scenario Exploration Plotting
# Reid Steele 03/01/2022

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

# Load in results
load('SE.RData')


# CCM Significance test table

# Add dummy NAs for capelin and cod against themselves
ccm_sig_cap$acoustic = NA
ccm_sig_cod$cod = NA

# Rename cod catch to catch
names(ccm_sig_cod)[which(names(ccm_sig_cod) == 'c_catch')] = 'catch'

# Reorder
ccm_sig_cod = ccm_sig_cod[names(ccm_sig_cap)]

# Rename to final names
names(ccm_sig_cap) = c('Catch', 'Winter NAO', 'Air Temperature', 'Sea Ice', 'Icebergs', 'SST', 'S27 Temperature', 'S27 Salinity', 'S27 CIL',
                       'CIL Area', 'Bottom Temperature', 'CNLCI', 'ANLCI', 'Ice Timing', 'Greenland Halibut', 'Atlantic Cod', 'Capelin')
names(ccm_sig_cod) = c('Catch', 'Winter NAO', 'Air Temperature', 'Sea Ice', 'Icebergs', 'SST', 'S27 Temperature', 'S27 Salinity', 'S27 CIL',
                       'CIL Area', 'Bottom Temperature', 'CNLCI', 'ANLCI', 'Ice Timing', 'Greenland Halibut', 'Atlantic Cod', 'Capelin')

# Reorder
ccm_sig_cap = ccm_sig_cap[c('Catch', 'Capelin', 'Atlantic Cod', 'Greenland Halibut', 'CNLCI', 'ANLCI', 'Winter NAO', 'CIL Area', 'Sea Ice', 'Icebergs', 'Ice Timing', 'Air Temperature', 
                            'SST', 'Bottom Temperature', 'S27 Temperature', 'S27 Salinity', 'S27 CIL')]
ccm_sig_cod = ccm_sig_cod[c('Catch', 'Capelin', 'Atlantic Cod', 'Greenland Halibut', 'CNLCI', 'ANLCI', 'Winter NAO', 'CIL Area', 'Sea Ice', 'Icebergs', 'Ice Timing', 'Air Temperature', 
                            'SST', 'Bottom Temperature', 'S27 Temperature', 'S27 Salinity', 'S27 CIL')]

# Generate table
ccm_sig = cbind(ccm_sig_cap, ccm_sig_cod)

# Rename columns
colnames(ccm_sig) = c('Capelin', 'Atlantic Cod')

# Export table
write.csv(ccm_sig, file = '../FiguresCh2/CCM_significance.csv')






# CV Rho table

# Driver names
drivers = c('Catch', 'Capelin', 'Atlantic Cod', 'Greenland Halibut', 'CNLCI', 'Ice Timing', 'SST')

# Capelin rho
cap_rho = c(fc_imp(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'catch(t-1)', lib = c(1,nrow(cap_all_ay_embed)), pred = c(1,nrow(cap_all_ay_embed)))$multivariate,
            NA,
            fc_imp(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'cod(t-1)', lib = c(1,nrow(cap_all_ay_embed)), pred = c(1,nrow(cap_all_ay_embed)))$multivariate,
            fc_imp(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'turbot(t-1)', lib = c(1,nrow(cap_all_ay_embed)), pred = c(1,nrow(cap_all_ay_embed)))$multivariate,
            fc_imp(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'Climate.index(t-1)', lib = c(1,nrow(cap_all_ay_embed)), pred = c(1,nrow(cap_all_ay_embed)))$multivariate,
            fc_imp(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'tice(t-1)', lib = c(1,nrow(cap_all_ay_embed)), pred = c(1,nrow(cap_all_ay_embed)))$multivariate,
            fc_imp(cap_all_ay_embed, cap_ay_E, cap_ay_theta, 'acoustic(t-0)', 'SST(t-1)', lib = c(1,nrow(cap_all_ay_embed)), pred = c(1,nrow(cap_all_ay_embed)))$multivariate)

# Cod rho
cod_rho = c(fc_imp(cod_all_ay_embed, 4, cod_ay_theta, 'cod(t-0)', 'c_catch(t-1)', lib = c(1,nrow(cod_all_ay_embed)), pred = c(1,nrow(cod_all_ay_embed)))$multivariate,
            fc_imp(cod_all_ay_embed, 4, cod_ay_theta, 'cod(t-0)', 'acoustic(t-1)', lib = c(1,nrow(cod_all_ay_embed)), pred = c(1,nrow(cod_all_ay_embed)))$multivariate,
            NA,
            fc_imp(cod_all_ay_embed, 4, cod_ay_theta, 'cod(t-0)', 'turbot(t-1)', lib = c(1,nrow(cod_all_ay_embed)), pred = c(1,nrow(cod_all_ay_embed)))$multivariate,
            fc_imp(cod_all_ay_embed, 4, cod_ay_theta, 'cod(t-0)', 'Climate.index(t-1)', lib = c(1,nrow(cod_all_ay_embed)), pred = c(1,nrow(cod_all_ay_embed)))$multivariate,
            NA,
            fc_imp(cod_all_ay_embed, 4, cod_ay_theta, 'cod(t-0)', 'SST(t-1)', lib = c(1,nrow(cod_all_ay_embed)), pred = c(1,nrow(cod_all_ay_embed)))$multivariate)

# Enter into data frame
cv_rho = data.frame(Driver = drivers, Capelin = cap_rho, Cod = cod_rho)

# Export
write.csv(cv_rho, file = '../FiguresCh2/CV_rho.csv')








# GGplot figures

# Boxplot

# Create combined data frame - capelin 0.5
cap_0.5 = data.frame(Year = dcap_sst$Year, Observed = dcap_sst$Observations, SST = dcap_sst$delta_0.5, CNLCI = dcap_nlci$delta_0.5, `Air_Temp` = dcap_at$delta_0.5, 
                     `Ice_Timing` = dcap_tice$delta_0.5, Cod = dcap_cod$delta_0.5, G._Halibut = dcap_gh$delta_0.5, Catch = dcap_ct$delta_0.5)

# Pivot to long format
cap_0.5_l = pivot_longer(cap_0.5, colnames(cap_0.5)[-1:-2]) %>% mutate(perturbation = '0.5 SD') %>% mutate(species = 'Capelin')

# Create combined data frame - capelin 1
cap_1 = data.frame(Year = dcap_sst$Year, Observed = dcap_sst$Observations, SST = dcap_sst$delta_1, CNLCI = dcap_nlci$delta_1, `Air_Temp` = dcap_at$delta_1, 
                   `Ice_Timing` = dcap_tice$delta_1, Cod = dcap_cod$delta_1, G._Halibut = dcap_gh$delta_1, Catch = dcap_ct$delta_1)

# Pivot to long format
cap_1_l = pivot_longer(cap_1, colnames(cap_1)[-1:-2]) %>% mutate(perturbation = '1 SD') %>% mutate(species = 'Capelin')

# Create combined data frame - cod 0.5
cod_0.5 = data.frame(Year = dcod_sst$Year, Observed = dcod_sst$Observations, SST = dcod_sst$delta_0.5, CNLCI = dcod_nlci$delta_0.5, Capelin = dcod_cap$delta_0.5, 
                     G._Halibut = dcod_gh$delta_0.5, Catch = dcod_ct$delta_0.5)

# Pivot to long format
cod_0.5_l = pivot_longer(cod_0.5, colnames(cod_0.5)[-1:-2]) %>% mutate(perturbation = '0.5 SD') %>% mutate(species = 'Cod')

# Create combined data frame - cod 1
cod_1 = data.frame(Year = dcod_sst$Year, Observed = dcod_sst$Observations, SST = dcod_sst$delta_1, CNLCI = dcod_nlci$delta_1, Capelin = dcod_cap$delta_1, 
                   G._Halibut = dcod_gh$delta_1, Catch = dcod_ct$delta_1)

# Pivot to long format
cod_1_l = pivot_longer(cod_1, colnames(cod_1)[-1:-2]) %>% mutate(perturbation = '1 SD') %>% mutate(species = 'Cod')






# Create combined data frame - cod 0.5
codcap_0.5 = data.frame(Year = dcod_sst_c$Year, Observed = dcod_sst_c$Observations, SST = dcod_sst_c$delta_0.5, CNLCI = dcod_nlci_c$delta_0.5)

# Pivot to long format
codcap_0.5_l = pivot_longer(codcap_0.5, colnames(codcap_0.5)[-1:-2]) %>% mutate(perturbation = '0.5 SD') %>% mutate(species = 'CodCap')

# Create combined data frame - cod 1
codcap_1 = data.frame(Year = dcod_sst_c$Year, Observed = dcod_sst_c$Observations, SST = dcod_sst_c$delta_1, CNLCI = dcod_nlci_c$delta_1)

# Pivot to long format
codcap_1_l = pivot_longer(codcap_1, colnames(codcap_1)[-1:-2]) %>% mutate(perturbation = '1 SD') %>% mutate(species = 'CodCap')




# Join all
box_data = rbind(cap_0.5_l, cap_1_l, cod_0.5_l, cod_1_l, codcap_0.5_l, codcap_1_l)

# Factorize perturbation
box_data$perturbation = as.factor(box_data$perturbation)

# Remove underscores
box_data$name = gsub('_', ' ', box_data$name)

# Reorder names
box_data$name = factor(box_data$name, levels = c('Catch', 'G. Halibut', 'Cod', 'Capelin', 'Air Temp', 'Ice Timing', 'CNLCI', 'SST'))

# PNG
png(file='../FiguresCh2/boxplots.png',width=8,height=6,units='in',res=300)

# Plot
ggplot(filter(box_data, species != 'CodCap'), aes(x = name, y = value, grourp = name, fill = perturbation)) + geom_hline(yintercept = 0) + 
  geom_boxplot() + coord_flip() + facet_wrap(~species, scale = 'free') + theme_classic() + 
  theme(panel.border = element_rect(linetype = "solid", fill = NA),  strip.background = element_blank(), strip.text = element_text(size = 15)) +
  #        text = element_text(size = 15), axis.text.y = element_text(size = 15)) +
  ylab('\u0394B/\u0394Cov') + xlab(NULL) + labs(fill = 'Perturbation')

# End
dev.off()


# Scatterplots

# Add marker column
box_data = box_data %>% mutate(marker = ifelse(Year >= 1991, 'Post-Collapse', 'Pre-Collapse'))

# Add climatological/ecological
box_data = box_data %>% mutate(type = ifelse(as.character(name) %in% c('Catch', 'G. Halibut', 'Cod', 'Capelin'), 'Ecological', 'Climatological'))

# Gather covariate data
cap_covs = cap_all_ay_embed[,c('Year', 'catch(t-1)', 'Air.Temp(t-1)', 'SST(t-1)', 'Climate.index(t-1)', 'tice(t-1)', 'turbot(t-1)', 'cod(t-1)')]
cod_covs = cod_all_ay_embed[,c('Year', 'c_catch(t-1)', 'SST(t-1)', 'Climate.index(t-1)', 'turbot(t-1)', 'acoustic(t-1)')]
codcap_covs = cod_all_ay_embed[,c('Year', 'SST(t-1)', 'Climate.index(t-1)')]

# Rename columns
colnames(cap_covs) = c('Year', 'Catch', 'Air Temp', 'SST', 'CNLCI', 'Ice Timing', 'G. Halibut', 'Cod')
colnames(cod_covs) = c('Year', 'Catch', 'SST', 'CNLCI', 'G. Halibut', 'Capelin')
colnames(codcap_covs) = c('Year', 'SST', 'CNLCI')

# Pivot long
cap_covs = pivot_longer(cap_covs, colnames(cap_covs)[-1], values_to = 'cov_value') %>% mutate(species = 'Capelin')
cod_covs = pivot_longer(cod_covs, colnames(cod_covs)[-1], values_to = 'cov_value') %>% mutate(species = 'Cod')
codcap_covs = pivot_longer(codcap_covs, colnames(codcap_covs)[-1], values_to = 'cov_value') %>% mutate(species = 'CodCap')

# Merge
data_covs = rbind(cap_covs, cod_covs, codcap_covs)

# Join covariate data
data_covs = left_join(box_data, data_covs)

# Add combined perturbation and post/pre-collapse
data_covs = data_covs %>% 
  mutate(mark_per = paste(marker, perturbation, sep = ' ')) %>%
  mutate(mark_spec = paste(species, marker, sep = ' '))

# Air temp to Air Temperature
data_covs$name = ifelse(data_covs$name == 'Air Temp', 'Air Temperature', data_covs$name)
# G. Halibut to Greenland Halibut
data_covs$name = ifelse(data_covs$name == 'G. Halibut', 'Greenland Halibut', data_covs$name)

# Filter climatological and Ecological
clim_scatter = filter(data_covs, (type == 'Climatological')) %>%
  filter(species != 'CodCap')
ecol_scatter = filter(data_covs, (type == 'Ecological')) %>%
  filter(species != 'CodCap')
multi_scatter = filter(data_covs, species == 'CodCap')

# Add to multi-scatter covariate names
multi_scatter$name = paste(multi_scatter$name, '+ Capelin Prediction') 


# PNG
png(file='../FiguresCh2/scatter_clim.png',width=8,height=6,units='in',res=300)

# # Plot climatological variables
# ggplot(clim_scatter, aes(x = cov_value, y = value, shape = mark_per, color = species)) +
#   geom_point() + facet_wrap(~name, scales = 'free') + scale_shape_manual(values = c(16, 15, 1, 0)) +
#   geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
#   ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
#   theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12))

# ggplot(clim_scatter, aes(x = cov_value, y = value, color = mark_spec, shape = perturbation)) + 
#   geom_point() + facet_wrap(~name, scales = 'free') + scale_color_manual(values = c('#FF0000', '#FFBEC8', '#00BAFF', '#A5EFFF')) +
#   geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
#   ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
#   theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12))

# ggplot(clim_scatter, aes(x = cov_value, y = value, color = mark_spec, shape = perturbation)) + 
#   geom_point() + facet_wrap(~name, scales = 'free') + 
#   scale_color_manual(values = colorspace::diverging_hcl(6, palette = "Green-Brown")[c(2,3,5,4)],
#                      name = 'Species, Time Period') +
#   geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
#   ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
#   theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12))

ggplot(clim_scatter, aes(x = cov_value, y = value, color = species, shape = perturbation, fill = mark_spec, alpha = marker)) +
  geom_point() + facet_wrap(~name, scales = 'free') +
  scale_fill_manual(values = c('#B98E45', 'white', '#00AB9C', 'white')) +
  scale_color_manual(values = c('#B98E45', '#00AB9C'), labels = c('Capelin', 'Cod')) +
  scale_alpha_manual(name = 'Time Period', values = c(1, 1), guide = guide_legend(override.aes = list(shape = 21, fill = c('black', 'white')))) +
  scale_shape_manual(values = c(21, 24), guide = guide_legend(override.aes = list(fill = 'black'))) +
  geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
  ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
  theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12)) + 
  guides(fill = 'none')

dev.off()

png(file='../FiguresCh2/scatter_ecol.png',width=8,height=6,units='in',res=300)

# # Plot ecological variables
# ggplot(ecol_scatter, aes(x = cov_value, y = value, shape = mark_per, color = species)) + 
#   geom_point() + facet_wrap(~name, scales = 'free') + scale_shape_manual(values = c(16, 15, 1, 0)) +
#   geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') + 
#   ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
#   theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12))

# ggplot(ecol_scatter, aes(x = cov_value, y = value, color = mark_spec, shape = perturbation)) + 
#   geom_point() + facet_wrap(~name, scales = 'free') + 
#   scale_color_manual(values = colorspace::diverging_hcl(6, palette = "Green-Brown")[c(2,3,5,4)],
#                      name = 'Species, Time Period') +
#   geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
#   ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
#   theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12))

ggplot(ecol_scatter, aes(x = cov_value, y = value, color = species, shape = perturbation, fill = mark_spec, alpha = marker)) +
  geom_point() + facet_wrap(~name, scales = 'free') +
  scale_fill_manual(values = c('#B98E45', 'white', '#00AB9C', 'white')) +
  scale_color_manual(values = c('#B98E45', '#00AB9C'), labels = c('Capelin', 'Cod')) +
  scale_alpha_manual(name = 'Time Period', values = c(1, 1), guide = guide_legend(override.aes = list(shape = 21, fill = c('black', 'white')))) +
  scale_shape_manual(values = c(21, 24), guide = guide_legend(override.aes = list(fill = 'black'))) +
  geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
  ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
  theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12)) + 
  guides(fill = 'none')

dev.off()

png(file='../FiguresCh2/scatter_multi.png',width=8,height=3.25,units='in',res=300)

# ggplot(multi_scatter, aes(x = cov_value, y = value, color = mark_spec, shape = perturbation)) + 
#   geom_point() + facet_wrap(~name, scales = 'free') + 
#   scale_color_manual(values = colorspace::diverging_hcl(6, palette = "Green-Brown")[c(2,3,5,4)],
#                      labels = c('Cod Post-Collapse', 'Cod Pre-Collapse'), name = 'Time Period') +
#   geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
#   ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
#   theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12))

ggplot(multi_scatter, aes(x = cov_value, y = value, color = species, shape = perturbation, fill = mark_spec, alpha = marker)) +
  geom_point() + facet_wrap(~name, scales = 'free') +
  scale_fill_manual(values = c('#00AB9C', 'white')) +
  scale_color_manual(values = c( '#00AB9C'), labels = c('Cod')) +
  scale_alpha_manual(name = 'Time Period', values = c(1, 1), guide = guide_legend(override.aes = list(shape = 21, fill = c('black', 'white')))) +
  scale_shape_manual(values = c(21, 24), guide = guide_legend(override.aes = list(fill = 'black'))) +
  geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
  ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
  theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12)) + 
  guides(fill = 'none')

dev.off()


# ggplot(multi_scatter, aes(x = cov_value, y = value, shape = perturbation, fill = mark_spec)) + 
#   geom_point(color = '#00AB9C') + facet_wrap(~name, scales = 'free') + 
#   scale_fill_manual(values = c('#00AB9C', 'white'),
#                     labels = c('Cod Post-Collapse', 'Cod Pre-Collapse'), name = 'Time Period', guide = guide_legend(override.aes = list(shape = 21))) +
#   scale_shape_manual(values = c(21, 24)) +
#   geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
#   ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
#   theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12))
# 
# 
# 
# 
# 
# 
# 
# ggplot(ecol_scatter, aes(x = cov_value, y = value, color = species, shape = perturbation, fill = mark_spec, alpha = marker)) +
#   geom_point() + facet_wrap(~name, scales = 'free') +
#   scale_fill_manual(values = c('#B98E45', 'white', '#00AB9C', 'white')) +
#   scale_color_manual(values = c('#B98E45', '#00AB9C'), labels = c('Capelin', 'Cod')) +
#   scale_alpha_manual(name = 'Time Period', values = c(1, 1), guide = guide_legend(override.aes = list(shape = 21, fill = c('black', 'white')))) +
#   scale_shape_manual(values = c(21, 24), guide = guide_legend(override.aes = list(fill = 'black'))) +
#   geom_hline(yintercept = 0) + theme_classic() + xlab('Normalized Covariate Anomaly') +
#   ylab('\u0394B/\u0394X') + labs(color = 'Species', shape = 'Perturbation') +
#   theme(panel.border = element_rect(linetype = "solid", fill = NA), strip.background = element_blank(), strip.text = element_text(size = 12)) + 
#   guides(fill = 'none')
# 
# 
# colorspace::diverging_hcl(2, palette = "Green-Brown")


# hcl(h = 10, c = 500, l = 100)
# hcl(h = 10, c = 150, l = 100)
# 
# hcl(h = 260, c = 400, l = 100)
# hcl(h = 260, c = 200, l = 100)



hcl(h = 120, c = 100, l = 50)
hcl(h = 40, c = 150, l = 50)

hcl(h = 280, c = 300, l = 75)
hcl(h = 280, c = 100, l = 100)


# Boxplots

# # Capelin
# png(file='../FiguresCh2/cap_deltas.png',width=8,height=6,units='in',res=300)

# Base Plot

# par(mfrow = c(2,1), mar = c(0,4,0,1), oma = c(4,1,1,1), cex = 1.2)
# boxplot(dcap_sst$delta_0.5, dcap_nlci$delta_0.5, dcap_at$delta_0.5, dcap_tice$delta_0.5, dcap_cod$delta_0.5, dcap_gh$delta_0.5, dcap_ct$delta_0.5,
#         names = NA, xaxt = 'n', border = 'blue', col = 'white', ylim = c(-5, 3))
# abline(h = 0, lty = 2)
# legend('topleft', bty = 'n', legend = '0.5 SD Perturbation', fill = 'blue')
# 
# boxplot(dcap_sst$delta_1, dcap_nlci$delta_1, dcap_at$delta_1, dcap_tice$delta_1, dcap_cod$delta_1, dcap_gh$delta_1, dcap_ct$delta_1,
#         names = c('SST', 'CNLCI', 'Air T', 'Ice Timing', 'Cod', 'Halibut', 'Catch'), border = 'red', col = 'white', ylim = c(-5, 3))
# abline(h = 0, lty = 2)
# legend('topleft', bty = 'n', legend = '1 SD Perturbation', fill = 'red')
# 
# mtext(text = bquote(Delta*.('B')/Delta*.('Cov')), outer = TRUE, side = 2, cex = 1.3, line = -2)
# 
# dev.off()

# # Cod
# png(file='../FiguresCh2/cod_deltas.png',width=8,height=6,units='in',res=300)
# 
# par(mfrow = c(2,1), mar = c(0,4,0,1), oma = c(4,1,1,1), cex = 1.2)
# boxplot(dcod_sst$delta_0.5, dcod_nlci$delta_0.5, dcod_cap$delta_0.5, dcod_gh$delta_0.5, dcod_ct$delta_0.5,
#         names = NA, xaxt = 'n', border = 'blue', col = 'white', ylim = c(-0.5, 5))
# abline(h = 0, lty = 2)
# legend('topleft', bty = 'n', legend = '0.5 SD Perturbation', fill = 'blue')
# 
# boxplot(dcod_sst$delta_1, dcod_nlci$delta_1, dcod_cap$delta_1, dcod_gh$delta_1, dcod_ct$delta_1,
#         names = c('SST', 'CNLCI', 'Capelin', 'Halibut', 'Catch'), border = 'red', col = 'white', ylim = c(-0.5, 5))
# abline(h = 0, lty = 2)
# legend('topleft', bty = 'n', legend = '1 SD Perturbation', fill = 'red')
# 
# mtext(text = bquote(Delta*.('B')/Delta*.('Cov')), outer = TRUE, side = 2, cex = 1.3, line = -2)
# 
# dev.off()





# Capelin time series and scatterplots

# Plot Times Series and Scatter - Capelin against SST
png(file='../FiguresCh2/cap_dSST.png',width=8,height=6,units='in',res=300)

plot_se(cap_all_ay_embed, dcap_sst, 'SST', 'Capelin', 'SST(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against SST
png(file='../FiguresCh2/cap_dSST_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cap_all_ay_embed, dcap_sst, 'SST', 'Capelin', 'SST(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against nlci
png(file='../FiguresCh2/cap_dnlci.png',width=8,height=6,units='in',res=300)

plot_se(cap_all_ay_embed, dcap_nlci, 'CNLCI', 'Capelin', 'Climate.index(t-1)', inset1 = -0.27)

dev.off()

# Plot Times Series and Scatter - Capelin against nlci
png(file='../FiguresCh2/cap_dnlci_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cap_all_ay_embed, dcap_nlci, 'CNLCI', 'Capelin', 'Climate.index(t-1)')

dev.off()

# Plot Times Series and Scatter - cap against at
png(file='../FiguresCh2/cap_dat.png',width=8,height=6,units='in',res=300)

plot_se(cap_all_ay_embed, dcap_at, 'Air Temperature', 'Capelin', 'Air.Temp(t-1)')

dev.off()


# Plot Times Series and Scatter - cap against at
png(file='../FiguresCh2/cap_dat_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cap_all_ay_embed, dcap_at, 'Air Temperature', 'Capelin', 'Air.Temp(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against tice
png(file='../FiguresCh2/cap_dtice.png',width=8,height=6,units='in',res=300)

plot_se(cap_all_ay_embed, dcap_tice, 'Ice Timing', 'Capelin', 'tice(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against tice
png(file='../FiguresCh2/cap_dtice_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cap_all_ay_embed, dcap_tice, 'Ice Timing', 'Capelin', 'tice(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against cod
png(file='../FiguresCh2/cap_dcod.png',width=8,height=6,units='in',res=300)

plot_se(cap_all_ay_embed, dcap_cod, 'Cod', 'Capelin', 'cod(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against cod
png(file='../FiguresCh2/cap_dcod_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cap_all_ay_embed, dcap_cod, 'Cod', 'Capelin', 'cod(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against gh
png(file='../FiguresCh2/cap_dgh.png',width=8,height=6,units='in',res=300)

plot_se(cap_all_ay_embed, dcap_gh, 'Greenland Halibut', 'Capelin', 'turbot(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against gh
png(file='../FiguresCh2/cap_dgh_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cap_all_ay_embed, dcap_gh, 'Greenland Halibut', 'Capelin', 'turbot(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against ct
png(file='../FiguresCh2/cap_dct.png',width=8,height=6,units='in',res=300)

plot_se(cap_all_ay_embed, dcap_ct, 'Catch', 'Capelin', 'catch(t-1)')

dev.off()

# Plot Times Series and Scatter - Capelin against ct
png(file='../FiguresCh2/cap_dct_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cap_all_ay_embed, dcap_ct, 'Catch', 'Capelin', 'catch(t-1)')

dev.off()





# Cod time series and scatterplots

# Plot Times Series and ScSSTter - Cod against SST
png(file='../FiguresCh2/cod_dSST.png',width=8,height=6,units='in',res=300)

plot_se(cod_all_ay_embed, dcod_sst, 'SST', 'Cod', 'SST(t-1)')

dev.off()


# Plot Times Series and ScSSTter - Cod against SST
png(file='../FiguresCh2/cod_dSST_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cod_all_ay_embed, dcod_sst, 'SST', 'Cod', 'SST(t-1)')

dev.off()


# Plot Times Series and Scatter - Cod against nlci
png(file='../FiguresCh2/cod_dnlci.png',width=8,height=6,units='in',res=300)

plot_se(cod_all_ay_embed, dcod_nlci, 'CNLCI', 'Cod', 'Climate.index(t-1)')

dev.off()


# Plot Times Series and Scatter - Cod against nlci
png(file='../FiguresCh2/cod_dnlci_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cod_all_ay_embed, dcod_nlci, 'CNLCI', 'Cod', 'Climate.index(t-1)')

dev.off()

# Plot Times Series and Scatter - Cod against cap
png(file='../FiguresCh2/cod_dcap.png',width=8,height=6,units='in',res=300)

plot_se(cod_all_ay_embed, dcod_cap, 'Capelin', 'Cod', 'acoustic(t-1)', inset1 = -0.27, inset2 = -0.25)

dev.off()


# Plot Times Series and Scatter - Cod against cap
png(file='../FiguresCh2/cod_dcap_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cod_all_ay_embed, dcod_cap, 'Capelin', 'Cod', 'acoustic(t-1)')

dev.off()


# Plot Times Series and Scatter - Cod against gh
png(file='../FiguresCh2/cod_dgh.png',width=8,height=6,units='in',res=300)

plot_se(cod_all_ay_embed, dcod_gh, 'Greenland Halibut', 'Cod', 'turbot(t-1)')

dev.off()


# Plot Times Series and Scatter - Cod against gh
png(file='../FiguresCh2/cod_dgh_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cod_all_ay_embed, dcod_gh, 'Greenland Halibut', 'Cod', 'turbot(t-1)')

dev.off()


# Plot Times Series and Scatter - Cod against ct
png(file='../FiguresCh2/cod_dct.png',width=8,height=6,units='in',res=300)

plot_se(cod_all_ay_embed, dcod_ct, 'Catch', 'Cod', 'c_catch(t-1)')

dev.off()


# Plot Times Series and Scatter - Cod against ct
png(file='../FiguresCh2/cod_dct_text.png',width=8,height=6,units='in',res=300)

plot_se_text(cod_all_ay_embed, dcod_ct, 'Catch', 'Cod', 'c_catch(t-1)')

dev.off()







# Figure 2 - Data
png(file='../FiguresCh2/rawdata.png',width=6,height=5,units='in',res=300)

# Gather data with NAs
cap_NA = cap_bm

# Filter to same years
cap_NA = cap_NA[cap_NA$Year %in% cap_plot$Year,]

# Point version
point = ifelse(is.na(cap_NA$acoustic), 21, 16)

# Plot post-collapse
plot(acoustic ~ Year, data = cap_plot, type = 'l', lwd = 3, ylab = 'Capelin Acoustic Index',
     col = 'black', pch = 16)

# # Add markers, pch indicates filled or observed
# points(acoustic_pc ~ Year, data = cap_all_ay, col = 'blue', bg = 'white',
#        pch = ifelse(is.na(cap_acoustic_u[is.na(cap_all_ay[-1,]$acoustic) == FALSE,'acoustic_u']), 21, 16))

# # Plot full dataset behind
# lines(acoustic ~ Year, data = cap_all_ay, type = 'l', lwd = 3,
#       col = 'red', pch = 16)

# Add markers, pch indicates filled or observed
points(acoustic ~ Year, data = cap_plot, col = 'black', bg = 'white',
       pch = point)

# # Add legend - color
# legend('top', bty = 'n', fill = c('red', 'blue'), legend = c('All Years', 'Post-Collapse'))

# Add legend - fill
legend('topright', bty = 'n', pch = c(16, 21), legend = c('Observed', 'Imputed'), inset = 0.2)

dev.off()























