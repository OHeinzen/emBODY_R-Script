#######################
### emBODY_R-Script ###
#######################
#########################################
### 4a. Whole body processing (DEACT) ###
#########################################
#######################################
### 4a.1 Instructions & preparation ###
#######################################
# Disclaimer: Given the straightforward structuring of the data, the calculation of resulting 
# scores can take up quite some time, especially with large sample sizes. This issue becomes 
# more prominent without the use of ROI restrictions. In development, whole body processing for 
# one stimulus at a sample size of about 260 participants took up to 36 hours. For several 
# operations, we recommend running several instances of R on a computer with at least 8GB RAM.
stim <- "Sadness"

################################
### 4a.2 Whole body matching ###
################################
disk <- data.frame(
  x =  rep(c(1:15), each = 15),
  y =  rep(c(1:15), times = 15)
) # Create data frame representing the 15x15 square around the pixel on click

disk <- disk[!(disk$x == 1 & disk$y < 6 | disk$x == 1 & disk$y > 10),]
disk <- disk[!(disk$x == 2 & disk$y < 4 | disk$x == 2 & disk$y > 12),]
disk <- disk[!(disk$x == 3 & disk$y < 3 | disk$x == 3 & disk$y > 13),]
disk <- disk[!(disk$x == 4 & disk$y < 2 | disk$x == 4 & disk$y > 14),]
disk <- disk[!(disk$x == 5 & disk$y < 2 | disk$x == 5 & disk$y > 14),]

disk <- disk[!(disk$x == 15 & disk$y < 6 | disk$x == 15 & disk$y > 10),]
disk <- disk[!(disk$x == 14 & disk$y < 4 | disk$x == 14 & disk$y > 12),]
disk <- disk[!(disk$x == 13 & disk$y < 3 | disk$x == 13 & disk$y > 13),]
disk <- disk[!(disk$x == 12 & disk$y < 2 | disk$x == 12 & disk$y > 14),]
disk <- disk[!(disk$x == 11 & disk$y < 2 | disk$x == 11 & disk$y > 14),] # Trim the square to a pixelated circle

disk <- disk - 8 # Offset the circle by 8, so that 1:15 becomes -7:7

dft <- data.frame(
  ID =  rep(d, each = grid),
  GRIDX =  rep(rep(c(1:171),each = 522), length(d)),
  GRIDY =  rep(rep(seq(1:522), times = 171), times = length(d)),
  ACT = 0,
  DEACT = 0
) # Create reference data frame

emo_DEACT <- dft
rm(dft)
emo_DEACT$STIM <- stim # Specify stimulus
emo_DEACT$MATCH <- paste0("ID", "_", emo_DEACT$ID, "_", emo_DEACT$STIM, "_", emo_DEACT$GRIDX, "_", emo_DEACT$GRIDY) # Create matching variable
emo_DEACT$ACT[emo_DEACT$MATCH %in% dfr$MATCH_L] <- 1 
emo_DEACT$DEACT[emo_DEACT$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

gc() # Memory garbage collection
sum(emo_DEACT$DEACT) # Check matching

emo_DEACT$STIM <- NULL
emo_DEACT$MATCH <- NULL # Remove unnecessary variables
emo_DEACT$label <- paste0(emo_DEACT$ID, "_",emo_DEACT$GRIDX, "_", emo_DEACT$GRIDY) # Create disk matching variable

gc() # Memory garbage collection

ID <- emo_DEACT$ID[emo_DEACT$DEACT==1]
x.values <- emo_DEACT$GRIDX[emo_DEACT$DEACT==1]
y.values <- emo_DEACT$GRIDY[emo_DEACT$DEACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_DEACT$DEACT[emo_DEACT$label %in%  disk$label] <- 1 # Matching
}

sum(emo_DEACT$DEACT) # Check matching

emo_DEACT <- emo_DEACT[!(emo_DEACT$DEACT != 1),]
fwrite(emo_DEACT, file=paste0(stim, "_DEACT.csv")) # Write output file





###################################
### 4a.3 Merging processed data ###
###################################
emo_DEACT <- fread(paste0(stim, "DEACT.csv"))
emo_DEACT.a <- aggregate(DEACT ~ ID, emo_DEACT, FUN=function(x)sum(x)/89262) # Aggregate activation and deactivation in whole body for stimulus and ID

embody <- merge(embody, emo_DEACT.a, by = "ID", all = T)
embody <- rename(embody, c("DEACT"=paste0(stim, "_DEACT")))

fwrite(embody,file="emBODY_4a_OUTPUT.csv") # Write output file