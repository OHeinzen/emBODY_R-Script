#######################
### emBODY_R-Script ###
#######################
#########################################
### 1. emBODY data input & processing ###
#########################################
######################################
### 1.1 Instructions & preparation ###
######################################

# PACKAGES

setwd("/path/wd") # Set working directry

path <- "/path/demo_subjects" # Input directory to emBODY "subjects" folder
nr_stimuli <- 14 # Input number of stimuli
stimuli <- c("
             0 = 'Neutral';
             1 = 'Fear';
             2 = 'Anger';
             3 = 'Disgust';
             4 = 'Sadness';
             5 = 'Happiness';
             6 = 'Surprise';
             7 = 'Anxiety';
             8 = 'Love';
             9 = 'Depression';
             10 = 'Contempt';
             11 = 'Pride';
             12 = 'Shame';
             13 = 'Jealousy'
             ") # Input name and position of stimuli (IMPORTANT: first index is 0, not 1)

cnd <- c("SEX",
         "AGE", 
         "WEIGHT", 
         "HEIGHT", 
         "HAND", 
         "EDU", 
         "PSYLG",
         "PSYTR",
         "NEURO", 
         "NA"
) # Labels of demographic variables, which are measured during registration. After "NEURO" follows one NA. 
# The NA is intended but does not involve actual data. See the instructions for more details.



options(scipen=999) # Full-length display of decimal values

list.of.packages <- c("car", "corrgram", "data.table", "dplyr", 
                      "ggplot2", "grid", "plyr", "png", "psych", 
                      "reshape2", "stringr", "xml2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) # Check for packages

library(car)
library(data.table)
library(dplyr)
library(ggplot2)
library(grid) 
library(plyr) 
library(png)
library(reshape2)
library(stringr)
library(xml2) # Load packages


###############################
### 1.2 emBODY coordinates ###
###############################
d <- list.files(path) # Read emBODY-IDs from folder names
dc <- rep(d, each = nr_stimuli) # Repeat subject IDs for each combination of ID and stimulus

s <- c("0.csv", "1.csv", "2.csv", "3.csv", "4.csv", "5.csv", "6.csv", 
       "7.csv", "8.csv", "9.csv", "10.csv", "11.csv", "12.csv", "13.csv") # All possible stimuli file labels, necessary ones will be selected 
td <- "data.txt" # File name of demographic data
tp <- "presentation.txt" # File name of stimulus display order


f <- s[1:nr_stimuli] # Number and name of stimuli files

lc <- paste0(dc,"/", f)
ltd <- paste0(d, "/", td)
ltp <- paste0(d, "/", tp) # All combinations of ID and stimulus as path snippets 

nc <- paste0(path, "/", lc) 
ntd <- paste0(path, "/", ltd)
ntp <- paste0(path, "/", ltp) # Full data directories

yd <- list()
yp <- list()
x <- list() # Create empty lists


l <- rep((seq(1:nr_stimuli) - 1), times=length(d)) # Sequence of stimuli file names
l <- car::recode(l, stimuli) # Recode values to stimuli labels


for (i in 1:length(dc))
  x[[i]] <- read.csv(nc[i], sep=",", header = F) # Read all data files into list 

cnc <- c("TIME", "X", "Y") # Create variable labels
x <- lapply(x, setNames, nm = cnc) # Apply variable labels

# Cutoffs #
# The data is measured as follows: Upon stimuli presentation, all mouse movement will be recorded. On click, mouse movement will only be recorded
# when the left mouse button is pressed. The first click is indicated in the data by -1 | -1 | -1. This is the upper cutoff and will be stored by 
# row number in the "cutoffup" variable. When the "Continue" button is pressed, another indicator with -1 | -1 | -1 will be recorded. This lower
# cutoff will be stored by row number in the "cuttofflo" variable. The final data input skips to the first cutoff and stops reading at the
# second cutoff, so that only relevant data will be stored. 

cutoffup <- lapply(x, function(z) as.integer(row.names(z[z$TIME %in% -1,])[1])) # First, upper cutoff

for (i in 1:length(dc))
  x[[i]] <- read.csv(nc[i], sep=",", header = FALSE, skip = cutoffup[[i]]) # Read all files but skip to upper cutoff

x <- lapply(x, setNames, nm = cnc) # Apply variable labels
cutofflo <- lapply(x, function(z) as.integer(row.names(z[z$TIME %in% -1,])[1])) # Second, lower cutoff

for (i in 1:length(dc))
  x[[i]] <- read.csv(nc[i], sep=",", header = FALSE, skip = cutoffup[[i]], nrow = cutofflo[[i]]) # Read all files but only within bounds of cutoffs

x <- lapply(x, setNames, nm = cnc) # Apply variable labels

for (i in seq(x))
  assign(paste0("ID", "_", dc[i], "_",l[i]), x[[i]]) # Create individual data frames from list with "ID"_ID_Stimulus

dl <- mget(ls(.GlobalEnv, pattern = "ID")) # Again, collect all data frames in a list

df <- data.frame() # Create empty data frane
df <- do.call(rbind, dl) # Row bind the data frames to into single data frame
df$L <- rownames(df) # Create label variable
rownames(df) <- 1:nrow(df) # Change row names

df <- cbind(df, str_split_fixed(df$L, "\\.", 2))
df$`2` <- NULL
names(df)[names(df)=="1"] <- "MATCH"
df <- cbind(df, str_split_fixed(df$L, "_", 3))
df <- cbind(df, str_split_fixed(df$`3`, ".[[:digit:]]", 2)) # Create matching variable
df <- df[-c(4,6,8,10)] # Remove unwanted variables
names(df)[names(df)=="2"] <- "ID"
names(df)[names(df)=="1"] <- "STIM" # Renaming

df$TIME <- as.POSIXct((df$TIME)/1000, origin="1970-01-01") # Calculate real stime stamp
df$Y <- 601 - df$Y # Flip Y orientation
df$X <- round(df$X)
df$Y <- round(df$Y) # Rounding

rm(list=ls(pattern="ID")) # Remove unwanted data frames

dfr <- df # Create new, reduced data frame

dfr$DUPL <- paste0(dfr$MATCH, "_", dfr$X, "_", dfr$Y) # Create matching variable to remove duplicates before changing offset: "ID"_ID_STIM_X_Y
dfr <- dfr[!duplicated(dfr$DUPL),] # Remove duplicates

dfr$Y[dfr$Y < 70 | dfr$Y > 591] <- NA # Limit to grid
dfr$Y <- dfr$Y - 69 # Remove offset to Y = 1:171
dfr$X_L <- dfr$X # Split X and Y to left (activation) and right (deactiation)
dfr$X_L[dfr$X_L < 33 | dfr$X_L > 203] <- NA # Limit to grid
dfr$X_L <- dfr$X_L - 32 # Remove offset
dfr$Y_L <- dfr$Y # Same for Y
dfr$Y_L[is.na(dfr$X_L)] <- NA # Keep only Y values that match existing X value
dfr$X_R <- dfr$X
dfr$X_R[dfr$X_R < 696 | dfr$X_R > 866] <- NA
dfr$X_R <- dfr$X_R - 695
dfr$Y_R <- dfr$Y
dfr$Y_R[is.na(dfr$X_R)] <- NA # See above

dfr$MATCH_L <- paste0("ID", "_", dfr$ID, "_", dfr$STIM, "_", dfr$X_L, "_", dfr$Y_L) 
dfr$MATCH_R <- paste0("ID", "_", dfr$ID, "_", dfr$STIM, "_", dfr$X_R, "_", dfr$Y_R) # Create matching variables for left (activation) and right (deactiation)

grid <- (203-32) * (591-69)  # Grid for each combination of x and y
stimuli_sep <- unique(l) # Stimuli as simple characters

################################
### 1.3 emBODY demographics ###
################################
for (i in 1:length(d)) 
  yd[[i]] <- read.table(ntd[i], sep=",", header = FALSE) # Read sociodemographic data

### ! ATTENTION ! The displayed warning here is due to an erroneous comma within the data gathering process and can safely be ignored. ###

yd <- lapply(yd, setNames, nm = cnd) # Define new column names

embody <- data.frame() # Create new DF for all data to be gathered in
embody <- do.call(rbind, yd) # Add sociodemographic data
embody$ID <- d # Add IDs
embody <- embody[c((length(embody)), 1:(length(embody)-1))] 
embody <- embody[-c(length(embody))] # Sort DF

fwrite(embody,file="emBODY_1_OUTPUT.csv") # Write output file