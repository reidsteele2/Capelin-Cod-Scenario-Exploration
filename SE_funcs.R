# SE Functions
# Reid Steele, Nov 11, 2025

# Cross-map one function - for running unpaired CCMs
ccm_one = function(df, E, columns, targets){
  
  # Gather pairs to cross-map
  pairs = expand.grid(columns, targets)
  
  CCMs = NULL
  
  # loop through pairs
  for(i in 1:nrow(pairs)){
    
    # gather pair
    pair = pairs[i,]
    pair = as.character(unlist(pair))
    
    # generate data frame containing pair
    df_pair = df[,c(1, which(colnames(df) %in% pair))]
    
    # filter data frame to only data both variables have
    df_pair = df_pair[(is.na(df_pair[,2]) == FALSE) & (is.na(df_pair[,3]) == FALSE),]
    
    # Gather embedding dimension - pair 1
    pair1_ed = EmbedDimension(dataFrame = df_pair,
                              target = pair[1], columns = pair[1],
                              lib = paste(1, nrow(df_pair)),
                              pred = paste(1, nrow(df_pair)),
                              showPlot = FALSE)
    
    # # Pull optimal embedding dimension - pair 1
    # pair1_E = which.max(pair1_ed$rho)
    
    pair1_E = E[which(columns == pair[1])]
    
    # CCM - pair 1
    pair1_CCM = CCM(dataFrame = df_pair, E = pair1_E, target = pair[2], columns = pair[1], 
                    libSizes = c(max(c(pair1_E+1,3, ceiling(((nrow(df_pair) - pair1_E + 1))/10))), (nrow(df_pair) - pair1_E + 1), 1), 
                    sample = 100, showPlot = TRUE)
    
    # # Save CCMs
    # CCMs[[i]] = list(pair1_CCM, pair2_CCM)
    
    # Create data frame for CCMs
    pair1_row = c(paste(pair[1], 'x', pair[2]), pair1_E, max(pair1_ed$rho), pair1_CCM[nrow(pair1_CCM),])
    
    # concatenate CCM results
    CCMs = rbind(CCMs, pair1_row)
    
  } # end loop through pairs
  
  # set to data frame
  CCMs = as.data.frame(CCMs)
  
  # rename columns
  colnames(CCMs) = c('test', 'E',  'Univariate', 'LibSize', '2on1_blue', '1on2_red')
  
  # Check if multivariate improved rho
  CCMs$MV_improvement = as.numeric(CCMs$`2on1_blue`) > as.numeric(CCMs$Univariate)
  
  # return
  return(CCMs)
  
} # end ccm_pair function



# Scenario exploration delta calculation function
se_delta = function(data, E, theta, target, vars, delta, tp = 1){
  
  # Initialize data frame to hold result
  out = NULL
  
  # Loop through deltas
  for(i in 1:length(delta)){
    
    # create up perturbation
    up = rbind(data, data)
    up[(1:nrow(data)),vars] = 
      up[(1:nrow(data)),vars] + delta[i]
    
    # create down perturbation
    down = rbind(data, data)
    down[(1:nrow(data)),vars] = 
      down[(1:nrow(data)),vars] - delta[i]
    
    # Create library sizes
    pred = c(1, nrow(data))
    lib = pred + nrow(data)
    
    # Gather target lags
    def_cols = colnames(data)[grepl(gsub('*\\(t-0\\)', '', target), colnames(data))][1:(E+tp)]
    def_cols = def_cols[grepl('t-0', def_cols) == F]
    
    # Remove middle lags
    for(k in 1:tp){def_cols = def_cols[!grepl(tp-k, def_cols)]}
    
    # Gather default columns
    def_cols = gsub(',', '', toString(def_cols))
    
    # Gather covariate columns
    var_cols = gsub(',', '', toString(vars))
    
    # Gather E for SMap
    smap_E = E + length(vars)
    
    # Run up smap
    smap_up = SMap(dataFrame = up, 
                   target = target, columns = paste(def_cols, var_cols, sep = ' '),
                   lib = lib, 
                   pred = pred,
                   E = smap_E, theta = theta,
                   embedded = TRUE, Tp = 0, showPlot = FALSE)
    
    # run down smap
    smap_down = SMap(dataFrame = down, 
                     target = target, columns = paste(def_cols, var_cols, sep = ' '),
                     lib = lib, 
                     pred = pred,
                     E = smap_E, theta = theta,
                     embedded = TRUE, Tp = 0, showPlot = FALSE)
    
    # Calculate scenario exploration delta
    delta_se = smap_up$predictions$Predictions - smap_down$predictions$Predictions
    
    # Unstandardize and divide by delta to get a rate
    delta_se = (delta_se*sd(na.omit(data[,target])))/(delta[i]*2)
    
    # Gather predictions
    smap_up_pred = smap_up$predictions$Predictions
    smap_down_pred = smap_down$predictions$Predictions
    
    # create data frame to add
    add = data.frame(smap_up_pred, smap_down_pred, delta_se)
    
    # Name columns
    colnames(add) = c(paste0('pred_up_', delta[i]), paste0('pred_down_', delta[i]), paste0('delta_', delta[i]))
    
    # Store results
    if(i == 1){out = add} else {out = cbind(out, add)}
    
  }
  
  # Run basic s-map
  base = SMap(dataFrame = rbind(data,data), 
              target = target, columns = paste(def_cols, var_cols, sep = ' '),
              lib = lib, 
              pred = pred,
              E = smap_E, theta = theta,
              embedded = TRUE, Tp = 0, showPlot = FALSE)
  
  # Add on year
  out = cbind(base$predictions[,1:3], out)
  
  # Return output
  return(out)
  
}




# forecast improvement
fc_imp = function(data, E, theta, target, vars, tp = 0, lib, pred){
  
  # Initialize data frame to hold result
  out = NULL
  
  # # Create library sizes
  # lib = c(1, nrow(data))
  # # pred = lib
  # lib = c(9, nrow(data))
  
  # Gather E for SMap
  if(E == 1){E = E+1} else {E = E}
  smap_E = E
  
  # Gather target lags
  def_cols = colnames(data)[grepl(gsub('*\\(t-0\\)', '', target), colnames(data))][1:(E+tp)]
  def_cols = def_cols[grepl('t-0', def_cols) == F]
  
  # # Remove middle lags
  # for(k in 1:tp){def_cols = def_cols[!grepl(tp-k, def_cols)]}
  
  # Gather default columns
  def_cols = gsub(',', '', toString(def_cols))
  
  # Gather covariate columns
  var_cols = gsub(',', '', toString(vars))
  
  # Run univariate s-map
  base = SMap(dataFrame = rbind(data,data), 
              target = target, columns = paste(def_cols, sep = ' '),
              lib = lib, 
              pred = pred,
              E = smap_E-1, theta = theta,
              embedded = TRUE, Tp = 0, showPlot = FALSE)
  
  # Run multivariate s-map
  mv = SMap(dataFrame = rbind(data,data), 
            target = target, columns = paste(def_cols, var_cols, sep = ' '),
            lib = lib, 
            pred = pred,
            E = smap_E, theta = theta,
            embedded = TRUE, Tp = 0, showPlot = T)
  
  # Calculate stats
  uni_rho = cor(base$predictions$Observations, base$predictions$Predictions)
  mv_rho = cor(mv$predictions$Observations, mv$predictions$Predictions)
  delta_rho = mv_rho - uni_rho
  
  # Name columns
  out = data.frame(univariate = uni_rho, multivariate = mv_rho, delta = delta_rho)
  
  # Return output
  return(out)
  
}

# Plotting function
plot_se = function(data, ddata, variable, species, varcol, inset1 = -0.25, inset2 = -0.25){
  
  # Create abbreviation for long names
  if(grepl(' ', variable)){abb = paste0(substr(variable, 1, 1), substr(gsub('.* ', '', variable), 1, 1))} else {abb = variable}
  
  # plots by year
  par(mfrow = c(2,1), mar = c(3,3,0.2,7.5), oma = c(0,0,0,0))
  
  # Plot Time Series
  plot(Observations ~ Year, data = ddata, col = 'black', type = 'l', lty = 2, lwd = 2, 
       ylim = range(ddata[,-1]), ylab = paste(species, 'Index'), mgp=c(2,1,0))
  lines(Predictions ~ Year, data = ddata, col = 'black', type = 'l', lty = 1, lwd = 2)
  lines(pred_up_0.5  ~ Year, data = ddata, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
  lines(pred_up_1  ~ Year, data = ddata, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
  lines(pred_down_0.5  ~ Year, data = ddata, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
  lines(pred_down_1  ~ Year, data = ddata, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)
  l1 = legend('right', bty = 'n', legend = c('Observed', 'Predicted',
                                             paste(abb, '+', delta[2]),
                                             paste(abb, '+', delta[1]),
                                             paste(abb, '-', delta[1]),
                                             paste(abb, '-', delta[2])),
              lty = c(2,1,1,1,1,1), lwd = 2, col = c('black', 'black', 
                                                     rgb(1, 0, 0, 1), 
                                                     rgb(1, 0, 0, 0.3), 
                                                     rgb(0, 0, 1, 0.3), 
                                                     rgb(0, 0, 1, 1)),
              inset = c(inset1, 0), xpd = TRUE, xjust = 0)
  
  # Plot Scatterplot
  plot(ddata$delta_0.5 ~ data[,varcol], col = 'orange', 
       pch = ifelse(data$Year > 1991, 16, 1), mgp=c(2,1,0),
       xlab = variable, ylab = bquote(Delta*.('B')/Delta*.(abb)))
  points(ddata$delta_1 ~ data[,varcol], col = 'purple', 
         pch = ifelse(data$Year > 1991, 16, 1))
  #text(data$`at(t-1)`, ddata/delta, labels = data$Year)
  abline(h = 0) 
  legend('right', bty = 'n', 
         legend = sapply(c(bquote(Delta*.(abb)==.(delta[2]*2)), 
                           bquote(Delta*.(abb)==.(delta[1]*2)), 
                           'Pre-Collapse', 'Post-Collapse'), as.expression),
         pch = c(16, 16, 1, 16), col = c('orange', 'purple', 'black', 'black'),
         inset = c(inset2, 0), xpd = TRUE, xjust = 0)
  
  
}


# Multivariate scenario exploration delta calculation
se_delta_c = function(data, d1, E, theta, target, cvar, vars, delta, tp = 1){
  
  # Initialize output data frame
  out = NULL
  
  # Loop through delta
  for(i in 1:length(delta)){
    
    # gather column names
    up_col = paste0('pred_up_', delta[i])
    down_col = paste0('pred_down_', delta[i])
    
    # Gather target lags
    def_cols = colnames(data)[grepl(gsub('*\\(t-0\\)', '', target), colnames(data))][1:(E+tp)]
    
    # Remove middle lags
    for(k in 1:tp){def_cols = def_cols[!grepl(tp-k, def_cols)]}
    
    # Gather E for SMap
    smap_E = length(def_cols) + length(var_cols) + length(cvar_cols)
    
    # Gather default columns
    def_cols = gsub(',', '', toString(def_cols))
    
    # Gather covariate columns
    var_cols = gsub(',', '', toString(vars))
    
    # Gather carrythrough covariate columns
    cvar_cols = gsub(',', '', toString(cvar))
    
    # Initialize up perturbation
    up = d1[,c('Year', up_col)]
    
    # Rename column as cvar without the time index
    colnames(up)[2] = gsub("\\s*\\([^\\)]+\\)", '', cvar)
    
    # Embed and add year
    up_E = Embed(dataFrame = up, E = E+tp, columns = colnames(up)[2])
    up_E = cbind(Year = up$Year, up_E)
    
    # Initialize up perturbation data frame
    up = data
    
    # Match rows
    if(nrow(up) > nrow(up_E)){up = up[up$Year %in% up_E$Year,]} else {up_E = up_E[up_E$Year %in% up$Year,]}
    
    # Replace cvar data with predictions
    up[, colnames(up_E)[-1]] = up_E[,-1]
    
    # Remove NAs
    up = up[complete.cases(up),]
    
    # Perturb target
    up[,grep(gsub("\\s*\\([^\\)]+\\)", '', vars), colnames(up))] = 
      up[,grep(gsub("\\s*\\([^\\)]+\\)", '', vars), colnames(up))] + delta[i]
    
    # Create library sizes
    pred = c(1, nrow(up))
    lib = pred + nrow(up)
    
    # Add unperturbed data
    up = rbind(up, data[data$Year %in% up$Year,])
    
    # Run S-Map with NLCI
    smap_up = SMap(dataFrame = up, 
                   target = target, columns = paste(def_cols, var_cols, cvar_cols, sep = ' '),
                   lib = lib, 
                   pred = pred,
                   E = smap_E, theta = cod_ay_theta,
                   embedded = TRUE, Tp = 0, showPlot = FALSE)
    
    
    # Initialize down perturbation
    down = d1[,c('Year', down_col)]
    
    # Rename column as cvar without the time index
    colnames(down)[2] = gsub("\\s*\\([^\\)]+\\)", '', cvar)
    
    # Embed and add year
    down_E = Embed(dataFrame = down, E = E+tp, columns = colnames(down)[2])
    down_E = cbind(Year = down$Year, down_E)
    
    # Initialize down perturbation data frame
    down = data
    
    # Match rows
    if(nrow(down) > nrow(down_E)){down = down[down$Year %in% down_E$Year,]} else {down_E = down_E[down_E$Year %in% down$Year,]}
    
    # Replace cvar data with predictions
    down[, colnames(down_E)[-1]] = down_E[,-1]
    
    # Remove NAs
    down = down[complete.cases(down),]
    
    # Perturb target
    down[,grep(gsub("\\s*\\([^\\)]+\\)", '', vars), colnames(down))] = 
      down[,grep(gsub("\\s*\\([^\\)]+\\)", '', vars), colnames(down))] - delta[i]
    
    # Create library sizes
    pred = c(1, nrow(down))
    lib = pred + nrow(down)
    
    # Add unperturbed data
    down = rbind(down, data[data$Year %in% down$Year,])
    
    # Run S-Map with NLCI
    smap_down = SMap(dataFrame = down, 
                     target = target, columns = paste(def_cols, var_cols, cvar_cols, sep = ' '),
                     lib = lib, 
                     pred = pred,
                     E = smap_E, theta = cod_ay_theta,
                     embedded = TRUE, Tp = 0, showPlot = FALSE)
    
    # Calculate scenario exploration delta
    delta_se = smap_up$predictions$Predictions - smap_down$predictions$Predictions
    
    # Unstandardize and divide by delta to get a rate
    delta_se = (delta_se*sd(na.omit(data[,target])))/(delta[i]*2)
    
    # Gather predictions
    smap_up_pred = smap_up$predictions$Predictions
    smap_down_pred = smap_down$predictions$Predictions
    
    # create data frame to add
    add = data.frame(smap_up_pred, smap_down_pred, delta_se)
    
    # Name columns
    colnames(add) = c(paste0('pred_up_', delta[i]), paste0('pred_down_', delta[i]), paste0('delta_', delta[i]))
    
    # Store results
    if(i == 1){out = add} else {out = cbind(out, add)}
    
    
  } # End delta loop
  
  # Run basic s-map
  base = SMap(dataFrame = rbind(data[data$Year %in% down$Year,],data[data$Year %in% down$Year,]), 
              target = target, columns = paste(def_cols, var_cols, cvar_cols, sep = ' '),
              lib = lib, 
              pred = pred,
              E = smap_E, theta = theta,
              embedded = TRUE, Tp = 0, showPlot = FALSE)
  
  # Add on year
  out = cbind(base$predictions[,1:3], out)
  
  # Return output
  return(out)
  
} # End function


# Plotting function (text)
plot_se_text = function(data, ddata, variable, species, varcol){
  
  # plots by year
  par(mfrow = c(2,1), mar = c(3,3,0.1,7.5), oma = c(0,0,0,0))
  
  # Plot Time Series
  plot(Observations ~ Year, data = ddata, col = 'black', type = 'l', lty = 2, lwd = 2, 
       ylim = range(ddata[,-1]), ylab = paste(species, 'Index'), mgp=c(2,1,0))
  lines(Predictions ~ Year, data = ddata, col = 'black', type = 'l', lty = 1, lwd = 2)
  lines(pred_up_0.5  ~ Year, data = ddata, col = rgb(1, 0, 0, 0.3), type = 'l', lty = 1, lwd = 2)
  lines(pred_up_1  ~ Year, data = ddata, col = rgb(1, 0, 0, 1), type = 'l', lty = 1, lwd = 2)
  lines(pred_down_0.5  ~ Year, data = ddata, col = rgb(0, 0, 1, 0.3), type = 'l', lty = 1, lwd = 2)
  lines(pred_down_1  ~ Year, data = ddata, col = rgb(0, 0, 1, 1), type = 'l', lty = 1, lwd = 2)
  l1 = legend('right', bty = 'n', legend = c('Observed', 'Predicted',
                                             paste(variable, '+', delta[2]),
                                             paste(variable, '+', delta[1]),
                                             paste(variable, '-', delta[1]),
                                             paste(variable, '-', delta[2])),
              lty = c(2,1,1,1,1,1), lwd = 2, col = c('black', 'black', 
                                                     rgb(1, 0, 0, 1), 
                                                     rgb(1, 0, 0, 0.3), 
                                                     rgb(0, 0, 1, 0.3), 
                                                     rgb(0, 0, 1, 1)),
              inset = c(-0.25, 0), xpd = TRUE, xjust = 0)
  
  # Plot Scatterplot
  plot(ddata$delta_0.5 ~ data[,varcol], col = 'white', 
       pch = ifelse(data$Year > 1991, 16, 1), mgp=c(2,1,0),
       xlab = variable, ylab = paste('Effect of', variable, 'on', species))
  # points(ddata$delta_1 ~ data[,varcol], col = 'red', 
  #        pch = ifelse(data$Year > 1991, 16, 1))
  text(data[,varcol], ddata$delta_0.5, labels = substr(data$Year, 3, 4), col = 'blue')
  text(data[,varcol], ddata$delta_1, labels = substr(data$Year, 3, 4), col = 'red')
  abline(h = 0) 
  legend('right', bty = 'n', 
         legend = sapply(c(bquote(Delta*.(variable)==.(delta[2]*2)), 
                           bquote(Delta*.(variable)==.(delta[1]*2))), as.expression),
         pch = 16, col = c('red', 'blue'),
         inset = c(-0.2, 0), xpd = TRUE, xjust = 0)
  
  
}