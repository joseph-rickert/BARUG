# PCA - anomaly detection example
# Showing detection of a common anomaly among multiple time-series
# variables with common cyclic behavior, as isolated by top PCA components
# (c) John Mark Agosta, 14 Dec 2020

library(ggplot2)    # Graphics
library(scales)     # hue_pal - used for color palettes

### Constants #################################################
# Domain
pts <- 101
x <- seq(0,1, by= 1/(pts-1))   # A closed interval of 101 points 

# The anomaly step function
bump <- 31
size <- 0.2
anomaly <- c(rep(0, bump), rep(size, pts-bump))

# Common parameters of the raw cyclic signals
cnt <- 5         # How many time-series
period <- 3      # Number of cycles over the domain
noise <- 0.1     # Error term as fraction of amplitude

# Colors for normal case
color_codes <- hue_pal()(cnt)
# colors for the abnormal case
pca_colors <- c('red', 'darkorange', 'navy','#53c68c', '#3399ff')

### Simulated cyclic signals ###################################
# A Fourier component with random phase and Gaussian noise. 
random_phase_signal <- function(u, period, noise)
{
  pnts <- length(u)
  phase <- 2 * pi * runif(1)
  sin(period * 2*pi * u + phase ) + noise * rnorm(pnts)
}

# Create a dataframe of raw components
cyclic_df <- data.frame(x=x)
for (cpt in 1:cnt) {
  cn = paste0('c',cpt)
  cyclic_df[[cn]] <- random_phase_signal(x, period, noise)  # cleaner than using cbind. 
}

# Visualize them
p <- ggplot(data=cyclic_df) + theme_bw() +  ylab('Cyclic signals') 
for (cpt in 1:cnt) {
  y=paste0('c',cpt)
  p <- p + geom_line(aes_string(x='x', y=y), color = color_codes[cpt], size = 1.1)
  } 
show(p)


# Create PCA components ###

signals <- as.matrix(cyclic_df[2:(cnt+1)])  # Remove the index column
# Compute the PCA transformation. 
pca <- prcomp(signals)
# Multiply the raw signals by the pca transformation matrix to get the PCA components
pca_timeseries <- data.frame(signals %*% pca$rotation ) 
# Add back the index column for plotting
pca_timeseries <- cbind(x, pca_timeseries)

# View the PCA components - transformed timeseries
p <- ggplot(data=pca_timeseries) + theme_bw() + ylab("signal PCA components") 
for (cpt in 1:cnt) {
  y=paste0('PC',cpt)
  p <- p + geom_line(aes_string(x='x', y=y), color = pca_colors[cpt], size = 1.1)
} 
show(p)


# Repeat the same, with the anomaly added  #################################
# A Fourier component with random phase and Gaussian noise, this time
# also with the anomaly
random_phase_anomaly <- function(u, period, noise)
{
  pnts <- length(u)
  phase <- 2 * pi * runif(1)
  sin(period * 2*pi * u + phase ) + noise * rnorm(pnts) + anomaly
}

# Create a dataframe of cyclic components with common anomaly
cyclic_df <- data.frame(x=x)
for (cpt in 1:cnt) {
  cn = paste0('c',cpt)
  cyclic_df[[cn]] <- random_phase_anomaly(x, period, noise)  
}

# View the cyclic components (can you even see the anomaly?)
p <- ggplot(data=cyclic_df) + theme_bw() + ylab('Raw signals plus anomaly') 
for (cpt in 1:cnt) {
  y=paste0('c',cpt)
  p <- p + geom_line(aes_string(x='x', y=y), color = color_codes[cpt], size = 1.1)
} 
p <- p + geom_line(aes(x=x, y=anomaly), color = 'black') 
show(p)

#  As before, Create PCA components ###
signals <- as.matrix(cyclic_df[2:(cnt+1)])
pca <- prcomp(signals)
pca_timeseries <- data.frame(as.matrix(signals) %*% pca$rotation ) 
pca_timeseries <- cbind(x, pca_timeseries)

# Note, the 3rd component, emphasized, picks up the anomaly. 
p <- ggplot(data=pca_timeseries) + theme_bw() + ylab("PCA components") 
for (cpt in 1:cnt) {
  y=paste0('PC',cpt)
  if (cpt == 3)   # Emphasize the anomalous component
    p <- p + geom_line(aes_string(x='x', y=y), color = pca_colors[cpt], size=2.6)
  else
    p <- p + geom_line(aes_string(x='x', y=y), color = pca_colors[cpt], size = 1.1)
} 
p <- p + geom_line(aes(x=x, y=anomaly), color = 'black') 
show(p)

# EOF
