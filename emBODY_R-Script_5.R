#######################
### emBODY_R-Script ###
#######################
############################
### 5. Plotting examples ###
############################
library(data.table)
library(ggplot2)

colfunc <- colorRampPalette(c("black", "red", "yellow")) 
colfunc2 <- colorRampPalette(c("black", "dodgerblue4", "deepskyblue1")) # Define color spectrum for hexagon density plots

body <- fread("/path/") # Read preprocessed data of whole body from step 4 / 4a
p <- ggplot(body, aes(GRIDX, GRIDY))
p <- p + lims(x = c(1, 171), y = c(1, 522))
p <- p + geom_hex(binwidth=c(10.625,19.5),na.rm=T) # Derived from grid size
p <- p + scale_fill_gradientn("Frequency \n", limits=c(0,500), breaks=seq(0,500, by=100), colours=colfunc(8)) # Change limits and breaks depending on hexagon frequency maximum
p <- p + theme(panel.background = element_rect(fill = "black"))
p <- p + theme(panel.grid.major = element_line(colour = "black"))
p <- p + theme(panel.grid.minor = element_line(colour = "black"))
p <- p + labs(list(x = "X", y = "Y"))
p


body <- fread("/path/") # Read preprocessed data of whole body from step 4 / 4a
p <- ggplot(body, aes(GRIDX, GRIDY))
p <- p + lims(x = c(1, 171), y = c(1, 522))
p <- p + geom_hex(binwidth=c(10.625,19.5),na.rm=T) # Derived from grid size
p <- p + scale_fill_gradientn("Frequency \n", limits=c(0,500), breaks=seq(0,500, by=100), colours=colfunc2(8)) # Change limits and breaks depending on hexagon frequency maximum
p <- p + theme(panel.background = element_rect(fill = "black"))
p <- p + theme(panel.grid.major = element_line(colour = "black"))
p <- p + theme(panel.grid.minor = element_line(colour = "black"))
p <- p + labs(list(x = "X", y = "Y"))
p