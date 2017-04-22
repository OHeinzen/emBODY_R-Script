#######################
### emBODY_R-Script ###
#######################
##################################
### 2. ROI-specific processing ###
##################################
######################################
### 2.1 Instructions & preparation ###
######################################
nrofrois <- 14 # Input number of ROIs you would like to define below

# roi_nr <- c(X1, X2, Y1, Y2, "Stimulus")
# "Stimulus" must be identical to the names given in "stimuli" object

roi_1 <- list(66, 106, 461, 511, "Happiness") # Formerly the head
roi_2 <- list(70, 102, 426, 466, "Disgust") # Formerly the throat
roi_3 <- list(86, 120, 336, 406, "Fear") # Formerly the heart
roi_4 <- list(0, 0, 0, 0, "Stimulus")
roi_5 <- list(0, 0, 0, 0, "Stimulus")
roi_6 <- list(0, 0, 0, 0, "Stimulus")
roi_7 <- list(0, 0, 0, 0, "Stimulus")
roi_8 <- list(0, 0, 0, 0, "Stimulus")
roi_9 <- list(0, 0, 0, 0, "Stimulus")
roi_10 <- list(0, 0, 0, 0, "Stimulus")
roi_11 <- list(0, 0, 0, 0, "Stimulus")
roi_12 <- list(0, 0, 0, 0, "Stimulus")
roi_13 <- list(0, 0, 0, 0, "Stimulus")
roi_14 <- list(0, 0, 0, 0, "Stimulus")

########################
### 2.2 ROI matching ###
########################

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


# ROI Nr. 1 w/ neutral comparison
emo_roi_1 <- as.data.frame(subset(dft, GRIDX > roi_1[1] & GRIDX < roi_1[2] & GRIDY > roi_1[3] &  GRIDY < roi_1[4])) # Limit reference DF to ROI
emo_roi_1$STIM <- roi_1[5] # Specify stimulus
emo_roi_1$MATCH <- paste0("ID", "_", emo_roi_1$ID, "_", emo_roi_1$STIM, "_", emo_roi_1$GRIDX, "_", emo_roi_1$GRIDY) # Create matching variable
emo_roi_1$ACT[emo_roi_1$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_1$DEACT[emo_roi_1$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_1$ACT) # Checking matching

emo_roi_1$label <- paste0(emo_roi_1$ID, "_", emo_roi_1$GRIDX, "_", emo_roi_1$GRIDY) # Create disk matching variable

ID <- emo_roi_1$ID[emo_roi_1$ACT==1]
x.values <- emo_roi_1$GRIDX[emo_roi_1$ACT==1]
y.values <- emo_roi_1$GRIDY[emo_roi_1$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_1$ACT[emo_roi_1$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_1$ACT) # Checking matching

sum(emo_roi_1$DEACT)

ID <- emo_roi_1$ID[emo_roi_1$DEACT==1]
x.values <- emo_roi_1$GRIDX[emo_roi_1$DEACT==1]
y.values <- emo_roi_1$GRIDY[emo_roi_1$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_1$DEACT[emo_roi_1$label %in%  disk$label] <- 1
}

sum(emo_roi_1$DEACT)

emo_roi_1.a <- aggregate(emo_roi_1, by = list(emo_roi_1$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_1 <- as.data.frame(subset(dft, GRIDX > roi_1[1] & GRIDX < roi_1[2] & GRIDY > roi_1[3] &  GRIDY < roi_1[4]))
ntr_roi_1$STIM <- "Neutral"
ntr_roi_1$MATCH <- paste0("ID", "_", ntr_roi_1$ID, "_", ntr_roi_1$STIM, "_", ntr_roi_1$GRIDX, "_", ntr_roi_1$GRIDY)
ntr_roi_1$ACT[ntr_roi_1$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_1$DEACT[ntr_roi_1$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_1$ACT)

ntr_roi_1$label <- paste0(ntr_roi_1$ID, "_",ntr_roi_1$GRIDX, "_", ntr_roi_1$GRIDY)

ID <- ntr_roi_1$ID[ntr_roi_1$ACT==1]
x.values <- ntr_roi_1$GRIDX[ntr_roi_1$ACT==1]
y.values <- ntr_roi_1$GRIDY[ntr_roi_1$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_1$ACT[ntr_roi_1$label %in%  disk$label] <- 1
}

sum(ntr_roi_1$ACT)

sum(ntr_roi_1$DEACT)

ID <- ntr_roi_1$ID[ntr_roi_1$DEACT==1]
x.values <- ntr_roi_1$GRIDX[ntr_roi_1$DEACT==1]
y.values <- ntr_roi_1$GRIDY[ntr_roi_1$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_1$DEACT[ntr_roi_1$label %in%  disk$label] <- 1
}

sum(ntr_roi_1$DEACT)

ntr_roi_1.a <- aggregate(ntr_roi_1, by = list(ntr_roi_1$ID), mean)





# ROI Nr. 2 w/ neutral comparison
if (nrofrois >= 2) {
emo_roi_2 <- as.data.frame(subset(dft, GRIDX > roi_2[1] & GRIDX < roi_2[2] & GRIDY > roi_2[3] &  GRIDY < roi_2[4])) # Limit reference DF to ROI
emo_roi_2$STIM <- roi_2[5] # Specify stimulus
emo_roi_2$MATCH <- paste0("ID", "_", emo_roi_2$ID, "_", emo_roi_2$STIM, "_", emo_roi_2$GRIDX, "_", emo_roi_2$GRIDY) # Create matching variable
emo_roi_2$ACT[emo_roi_2$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_2$DEACT[emo_roi_2$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_2$ACT) # Checking matching

emo_roi_2$label <- paste0(emo_roi_2$ID, "_", emo_roi_2$GRIDX, "_", emo_roi_2$GRIDY) # Create disk matching variable

ID <- emo_roi_2$ID[emo_roi_2$ACT==1]
x.values <- emo_roi_2$GRIDX[emo_roi_2$ACT==1]
y.values <- emo_roi_2$GRIDY[emo_roi_2$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_2$ACT[emo_roi_2$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_2$ACT) # Checking matching

sum(emo_roi_2$DEACT)

ID <- emo_roi_2$ID[emo_roi_2$DEACT==1]
x.values <- emo_roi_2$GRIDX[emo_roi_2$DEACT==1]
y.values <- emo_roi_2$GRIDY[emo_roi_2$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_2$DEACT[emo_roi_2$label %in%  disk$label] <- 1
}

sum(emo_roi_2$DEACT)

emo_roi_2.a <- aggregate(emo_roi_2, by = list(emo_roi_2$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_2 <- as.data.frame(subset(dft, GRIDX > roi_2[1] & GRIDX < roi_2[2] & GRIDY > roi_2[3] &  GRIDY < roi_2[4]))
ntr_roi_2$STIM <- "Neutral"
ntr_roi_2$MATCH <- paste0("ID", "_", ntr_roi_2$ID, "_", ntr_roi_2$STIM, "_", ntr_roi_2$GRIDX, "_", ntr_roi_2$GRIDY)
ntr_roi_2$ACT[ntr_roi_2$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_2$DEACT[ntr_roi_2$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_2$ACT)

ntr_roi_2$label <- paste0(ntr_roi_2$ID, "_",ntr_roi_2$GRIDX, "_", ntr_roi_2$GRIDY)

ID <- ntr_roi_2$ID[ntr_roi_2$ACT==1]
x.values <- ntr_roi_2$GRIDX[ntr_roi_2$ACT==1]
y.values <- ntr_roi_2$GRIDY[ntr_roi_2$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_2$ACT[ntr_roi_2$label %in%  disk$label] <- 1
}

sum(ntr_roi_2$ACT)

sum(ntr_roi_2$DEACT)

ID <- ntr_roi_2$ID[ntr_roi_2$DEACT==1]
x.values <- ntr_roi_2$GRIDX[ntr_roi_2$DEACT==1]
y.values <- ntr_roi_2$GRIDY[ntr_roi_2$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_2$DEACT[ntr_roi_2$label %in%  disk$label] <- 1
}

sum(ntr_roi_2$DEACT)

ntr_roi_2.a <- aggregate(ntr_roi_2, by = list(ntr_roi_2$ID), mean)
}





# ROI Nr. 3 w/ neutral comparison
if (nrofrois >= 3) {
emo_roi_3 <- as.data.frame(subset(dft, GRIDX > roi_3[1] & GRIDX < roi_3[2] & GRIDY > roi_3[3] &  GRIDY < roi_3[4])) # Limit reference DF to ROI
emo_roi_3$STIM <- roi_3[5] # Specify stimulus
emo_roi_3$MATCH <- paste0("ID", "_", emo_roi_3$ID, "_", emo_roi_3$STIM, "_", emo_roi_3$GRIDX, "_", emo_roi_3$GRIDY) # Create matching variable
emo_roi_3$ACT[emo_roi_3$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_3$DEACT[emo_roi_3$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_3$ACT) # Checking matching

emo_roi_3$label <- paste0(emo_roi_3$ID, "_", emo_roi_3$GRIDX, "_", emo_roi_3$GRIDY) # Create disk matching variable

ID <- emo_roi_3$ID[emo_roi_3$ACT==1]
x.values <- emo_roi_3$GRIDX[emo_roi_3$ACT==1]
y.values <- emo_roi_3$GRIDY[emo_roi_3$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_3$ACT[emo_roi_3$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_3$ACT) # Checking matching

sum(emo_roi_3$DEACT)

ID <- emo_roi_3$ID[emo_roi_3$DEACT==1]
x.values <- emo_roi_3$GRIDX[emo_roi_3$DEACT==1]
y.values <- emo_roi_3$GRIDY[emo_roi_3$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_3$DEACT[emo_roi_3$label %in%  disk$label] <- 1
}

sum(emo_roi_3$DEACT)

emo_roi_3.a <- aggregate(emo_roi_3, by = list(emo_roi_3$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_3 <- as.data.frame(subset(dft, GRIDX > roi_3[1] & GRIDX < roi_3[2] & GRIDY > roi_3[3] &  GRIDY < roi_3[4]))
ntr_roi_3$STIM <- "Neutral"
ntr_roi_3$MATCH <- paste0("ID", "_", ntr_roi_3$ID, "_", ntr_roi_3$STIM, "_", ntr_roi_3$GRIDX, "_", ntr_roi_3$GRIDY)
ntr_roi_3$ACT[ntr_roi_3$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_3$DEACT[ntr_roi_3$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_3$ACT)

ntr_roi_3$label <- paste0(ntr_roi_3$ID, "_",ntr_roi_3$GRIDX, "_", ntr_roi_3$GRIDY)

ID <- ntr_roi_3$ID[ntr_roi_3$ACT==1]
x.values <- ntr_roi_3$GRIDX[ntr_roi_3$ACT==1]
y.values <- ntr_roi_3$GRIDY[ntr_roi_3$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_3$ACT[ntr_roi_3$label %in%  disk$label] <- 1
}

sum(ntr_roi_3$ACT)

sum(ntr_roi_3$DEACT)

ID <- ntr_roi_3$ID[ntr_roi_3$DEACT==1]
x.values <- ntr_roi_3$GRIDX[ntr_roi_3$DEACT==1]
y.values <- ntr_roi_3$GRIDY[ntr_roi_3$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_3$DEACT[ntr_roi_3$label %in%  disk$label] <- 1
}

sum(ntr_roi_3$DEACT)

ntr_roi_3.a <- aggregate(ntr_roi_3, by = list(ntr_roi_3$ID), mean)
}





# ROI Nr. 4 w/ neutral comparison
if (nrofrois >= 4) {
emo_roi_4 <- as.data.frame(subset(dft, GRIDX > roi_4[1] & GRIDX < roi_4[2] & GRIDY > roi_4[3] &  GRIDY < roi_4[4])) # Limit reference DF to ROI
emo_roi_4$STIM <- roi_4[5] # Specify stimulus
emo_roi_4$MATCH <- paste0("ID", "_", emo_roi_4$ID, "_", emo_roi_4$STIM, "_", emo_roi_4$GRIDX, "_", emo_roi_4$GRIDY) # Create matching variable
emo_roi_4$ACT[emo_roi_4$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_4$DEACT[emo_roi_4$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_4$ACT) # Checking matching

emo_roi_4$label <- paste0(emo_roi_4$ID, "_", emo_roi_4$GRIDX, "_", emo_roi_4$GRIDY) # Create disk matching variable

ID <- emo_roi_4$ID[emo_roi_4$ACT==1]
x.values <- emo_roi_4$GRIDX[emo_roi_4$ACT==1]
y.values <- emo_roi_4$GRIDY[emo_roi_4$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_4$ACT[emo_roi_4$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_4$ACT) # Checking matching

sum(emo_roi_4$DEACT)

ID <- emo_roi_4$ID[emo_roi_4$DEACT==1]
x.values <- emo_roi_4$GRIDX[emo_roi_4$DEACT==1]
y.values <- emo_roi_4$GRIDY[emo_roi_4$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_4$DEACT[emo_roi_4$label %in%  disk$label] <- 1
}

sum(emo_roi_4$DEACT)

emo_roi_4.a <- aggregate(emo_roi_4, by = list(emo_roi_4$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_4 <- as.data.frame(subset(dft, GRIDX > roi_4[1] & GRIDX < roi_4[2] & GRIDY > roi_4[3] &  GRIDY < roi_4[4]))
ntr_roi_4$STIM <- "Neutral"
ntr_roi_4$MATCH <- paste0("ID", "_", ntr_roi_4$ID, "_", ntr_roi_4$STIM, "_", ntr_roi_4$GRIDX, "_", ntr_roi_4$GRIDY)
ntr_roi_4$ACT[ntr_roi_4$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_4$DEACT[ntr_roi_4$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_4$ACT)

ntr_roi_4$label <- paste0(ntr_roi_4$ID, "_",ntr_roi_4$GRIDX, "_", ntr_roi_4$GRIDY)

ID <- ntr_roi_4$ID[ntr_roi_4$ACT==1]
x.values <- ntr_roi_4$GRIDX[ntr_roi_4$ACT==1]
y.values <- ntr_roi_4$GRIDY[ntr_roi_4$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_4$ACT[ntr_roi_4$label %in%  disk$label] <- 1
}

sum(ntr_roi_4$ACT)

sum(ntr_roi_4$DEACT)

ID <- ntr_roi_4$ID[ntr_roi_4$DEACT==1]
x.values <- ntr_roi_4$GRIDX[ntr_roi_4$DEACT==1]
y.values <- ntr_roi_4$GRIDY[ntr_roi_4$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_4$DEACT[ntr_roi_4$label %in%  disk$label] <- 1
}

sum(ntr_roi_4$DEACT)

ntr_roi_4.a <- aggregate(ntr_roi_4, by = list(ntr_roi_4$ID), mean)
}





# ROI Nr. 5 w/ neutral comparison
if (nrofrois >= 5) {
emo_roi_5 <- as.data.frame(subset(dft, GRIDX > roi_5[1] & GRIDX < roi_5[2] & GRIDY > roi_5[3] &  GRIDY < roi_5[4])) # Limit reference DF to ROI
emo_roi_5$STIM <- roi_5[5] # Specify stimulus
emo_roi_5$MATCH <- paste0("ID", "_", emo_roi_5$ID, "_", emo_roi_5$STIM, "_", emo_roi_5$GRIDX, "_", emo_roi_5$GRIDY) # Create matching variable
emo_roi_5$ACT[emo_roi_5$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_5$DEACT[emo_roi_5$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_5$ACT) # Checking matching

emo_roi_5$label <- paste0(emo_roi_5$ID, "_", emo_roi_5$GRIDX, "_", emo_roi_5$GRIDY) # Create disk matching variable

ID <- emo_roi_5$ID[emo_roi_5$ACT==1]
x.values <- emo_roi_5$GRIDX[emo_roi_5$ACT==1]
y.values <- emo_roi_5$GRIDY[emo_roi_5$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_5$ACT[emo_roi_5$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_5$ACT) # Checking matching

sum(emo_roi_5$DEACT)

ID <- emo_roi_5$ID[emo_roi_5$DEACT==1]
x.values <- emo_roi_5$GRIDX[emo_roi_5$DEACT==1]
y.values <- emo_roi_5$GRIDY[emo_roi_5$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_5$DEACT[emo_roi_5$label %in%  disk$label] <- 1
}

sum(emo_roi_5$DEACT)

emo_roi_5.a <- aggregate(emo_roi_5, by = list(emo_roi_5$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_5 <- as.data.frame(subset(dft, GRIDX > roi_5[1] & GRIDX < roi_5[2] & GRIDY > roi_5[3] &  GRIDY < roi_5[4]))
ntr_roi_5$STIM <- "Neutral"
ntr_roi_5$MATCH <- paste0("ID", "_", ntr_roi_5$ID, "_", ntr_roi_5$STIM, "_", ntr_roi_5$GRIDX, "_", ntr_roi_5$GRIDY)
ntr_roi_5$ACT[ntr_roi_5$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_5$DEACT[ntr_roi_5$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_5$ACT)

ntr_roi_5$label <- paste0(ntr_roi_5$ID, "_",ntr_roi_5$GRIDX, "_", ntr_roi_5$GRIDY)

ID <- ntr_roi_5$ID[ntr_roi_5$ACT==1]
x.values <- ntr_roi_5$GRIDX[ntr_roi_5$ACT==1]
y.values <- ntr_roi_5$GRIDY[ntr_roi_5$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_5$ACT[ntr_roi_5$label %in%  disk$label] <- 1
}

sum(ntr_roi_5$ACT)

sum(ntr_roi_5$DEACT)

ID <- ntr_roi_5$ID[ntr_roi_5$DEACT==1]
x.values <- ntr_roi_5$GRIDX[ntr_roi_5$DEACT==1]
y.values <- ntr_roi_5$GRIDY[ntr_roi_5$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_5$DEACT[ntr_roi_5$label %in%  disk$label] <- 1
}

sum(ntr_roi_5$DEACT)

ntr_roi_5.a <- aggregate(ntr_roi_5, by = list(ntr_roi_5$ID), mean)
}




# ROI Nr. 6 w/ neutral comparison
if (nrofrois >= 6) {
emo_roi_6 <- as.data.frame(subset(dft, GRIDX > roi_6[1] & GRIDX < roi_6[2] & GRIDY > roi_6[3] &  GRIDY < roi_6[4])) # Limit reference DF to ROI
emo_roi_6$STIM <- roi_6[5] # Specify stimulus
emo_roi_6$MATCH <- paste0("ID", "_", emo_roi_6$ID, "_", emo_roi_6$STIM, "_", emo_roi_6$GRIDX, "_", emo_roi_6$GRIDY) # Create matching variable
emo_roi_6$ACT[emo_roi_6$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_6$DEACT[emo_roi_6$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_6$ACT) # Checking matching

emo_roi_6$label <- paste0(emo_roi_6$ID, "_", emo_roi_6$GRIDX, "_", emo_roi_6$GRIDY) # Create disk matching variable

ID <- emo_roi_6$ID[emo_roi_6$ACT==1]
x.values <- emo_roi_6$GRIDX[emo_roi_6$ACT==1]
y.values <- emo_roi_6$GRIDY[emo_roi_6$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_6$ACT[emo_roi_6$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_6$ACT) # Checking matching

sum(emo_roi_6$DEACT)

ID <- emo_roi_6$ID[emo_roi_6$DEACT==1]
x.values <- emo_roi_6$GRIDX[emo_roi_6$DEACT==1]
y.values <- emo_roi_6$GRIDY[emo_roi_6$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_6$DEACT[emo_roi_6$label %in%  disk$label] <- 1
}

sum(emo_roi_6$DEACT)

emo_roi_6.a <- aggregate(emo_roi_6, by = list(emo_roi_6$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_6 <- as.data.frame(subset(dft, GRIDX > roi_6[1] & GRIDX < roi_6[2] & GRIDY > roi_6[3] &  GRIDY < roi_6[4]))
ntr_roi_6$STIM <- "Neutral"
ntr_roi_6$MATCH <- paste0("ID", "_", ntr_roi_6$ID, "_", ntr_roi_6$STIM, "_", ntr_roi_6$GRIDX, "_", ntr_roi_6$GRIDY)
ntr_roi_6$ACT[ntr_roi_6$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_6$DEACT[ntr_roi_6$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_6$ACT)

ntr_roi_6$label <- paste0(ntr_roi_6$ID, "_",ntr_roi_6$GRIDX, "_", ntr_roi_6$GRIDY)

ID <- ntr_roi_6$ID[ntr_roi_6$ACT==1]
x.values <- ntr_roi_6$GRIDX[ntr_roi_6$ACT==1]
y.values <- ntr_roi_6$GRIDY[ntr_roi_6$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_6$ACT[ntr_roi_6$label %in%  disk$label] <- 1
}

sum(ntr_roi_6$ACT)

sum(ntr_roi_6$DEACT)

ID <- ntr_roi_6$ID[ntr_roi_6$DEACT==1]
x.values <- ntr_roi_6$GRIDX[ntr_roi_6$DEACT==1]
y.values <- ntr_roi_6$GRIDY[ntr_roi_6$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_6$DEACT[ntr_roi_6$label %in%  disk$label] <- 1
}

sum(ntr_roi_6$DEACT)

ntr_roi_6.a <- aggregate(ntr_roi_6, by = list(ntr_roi_6$ID), mean)
}





# ROI Nr. 7 w/ neutral comparison
if (nrofrois >= 7) {
emo_roi_7 <- as.data.frame(subset(dft, GRIDX > roi_7[1] & GRIDX < roi_7[2] & GRIDY > roi_7[3] &  GRIDY < roi_7[4])) # Limit reference DF to ROI
emo_roi_7$STIM <- roi_7[5] # Specify stimulus
emo_roi_7$MATCH <- paste0("ID", "_", emo_roi_7$ID, "_", emo_roi_7$STIM, "_", emo_roi_7$GRIDX, "_", emo_roi_7$GRIDY) # Create matching variable
emo_roi_7$ACT[emo_roi_7$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_7$DEACT[emo_roi_7$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_7$ACT) # Checking matching

emo_roi_7$label <- paste0(emo_roi_7$ID, "_", emo_roi_7$GRIDX, "_", emo_roi_7$GRIDY) # Create disk matching variable

ID <- emo_roi_7$ID[emo_roi_7$ACT==1]
x.values <- emo_roi_7$GRIDX[emo_roi_7$ACT==1]
y.values <- emo_roi_7$GRIDY[emo_roi_7$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_7$ACT[emo_roi_7$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_7$ACT) # Checking matching

sum(emo_roi_7$DEACT)

ID <- emo_roi_7$ID[emo_roi_7$DEACT==1]
x.values <- emo_roi_7$GRIDX[emo_roi_7$DEACT==1]
y.values <- emo_roi_7$GRIDY[emo_roi_7$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_7$DEACT[emo_roi_7$label %in%  disk$label] <- 1
}

sum(emo_roi_7$DEACT)

emo_roi_7.a <- aggregate(emo_roi_7, by = list(emo_roi_7$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_7 <- as.data.frame(subset(dft, GRIDX > roi_7[1] & GRIDX < roi_7[2] & GRIDY > roi_7[3] &  GRIDY < roi_7[4]))
ntr_roi_7$STIM <- "Neutral"
ntr_roi_7$MATCH <- paste0("ID", "_", ntr_roi_7$ID, "_", ntr_roi_7$STIM, "_", ntr_roi_7$GRIDX, "_", ntr_roi_7$GRIDY)
ntr_roi_7$ACT[ntr_roi_7$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_7$DEACT[ntr_roi_7$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_7$ACT)

ntr_roi_7$label <- paste0(ntr_roi_7$ID, "_",ntr_roi_7$GRIDX, "_", ntr_roi_7$GRIDY)

ID <- ntr_roi_7$ID[ntr_roi_7$ACT==1]
x.values <- ntr_roi_7$GRIDX[ntr_roi_7$ACT==1]
y.values <- ntr_roi_7$GRIDY[ntr_roi_7$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_7$ACT[ntr_roi_7$label %in%  disk$label] <- 1
}

sum(ntr_roi_7$ACT)

sum(ntr_roi_7$DEACT)

ID <- ntr_roi_7$ID[ntr_roi_7$DEACT==1]
x.values <- ntr_roi_7$GRIDX[ntr_roi_7$DEACT==1]
y.values <- ntr_roi_7$GRIDY[ntr_roi_7$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_7$DEACT[ntr_roi_7$label %in%  disk$label] <- 1
}

sum(ntr_roi_7$DEACT)

ntr_roi_7.a <- aggregate(ntr_roi_7, by = list(ntr_roi_7$ID), mean)
}





# ROI Nr. 8 w/ neutral comparison
if (nrofrois >= 8) {
emo_roi_8 <- as.data.frame(subset(dft, GRIDX > roi_8[1] & GRIDX < roi_8[2] & GRIDY > roi_8[3] &  GRIDY < roi_8[4])) # Limit reference DF to ROI
emo_roi_8$STIM <- roi_8[5] # Specify stimulus
emo_roi_8$MATCH <- paste0("ID", "_", emo_roi_8$ID, "_", emo_roi_8$STIM, "_", emo_roi_8$GRIDX, "_", emo_roi_8$GRIDY) # Create matching variable
emo_roi_8$ACT[emo_roi_8$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_8$DEACT[emo_roi_8$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_8$ACT) # Checking matching

emo_roi_8$label <- paste0(emo_roi_8$ID, "_", emo_roi_8$GRIDX, "_", emo_roi_8$GRIDY) # Create disk matching variable

ID <- emo_roi_8$ID[emo_roi_8$ACT==1]
x.values <- emo_roi_8$GRIDX[emo_roi_8$ACT==1]
y.values <- emo_roi_8$GRIDY[emo_roi_8$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_8$ACT[emo_roi_8$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_8$ACT) # Checking matching

sum(emo_roi_8$DEACT)

ID <- emo_roi_8$ID[emo_roi_8$DEACT==1]
x.values <- emo_roi_8$GRIDX[emo_roi_8$DEACT==1]
y.values <- emo_roi_8$GRIDY[emo_roi_8$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_8$DEACT[emo_roi_8$label %in%  disk$label] <- 1
}

sum(emo_roi_8$DEACT)

emo_roi_8.a <- aggregate(emo_roi_8, by = list(emo_roi_8$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_8 <- as.data.frame(subset(dft, GRIDX > roi_8[1] & GRIDX < roi_8[2] & GRIDY > roi_8[3] &  GRIDY < roi_8[4]))
ntr_roi_8$STIM <- "Neutral"
ntr_roi_8$MATCH <- paste0("ID", "_", ntr_roi_8$ID, "_", ntr_roi_8$STIM, "_", ntr_roi_8$GRIDX, "_", ntr_roi_8$GRIDY)
ntr_roi_8$ACT[ntr_roi_8$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_8$DEACT[ntr_roi_8$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_8$ACT)

ntr_roi_8$label <- paste0(ntr_roi_8$ID, "_",ntr_roi_8$GRIDX, "_", ntr_roi_8$GRIDY)

ID <- ntr_roi_8$ID[ntr_roi_8$ACT==1]
x.values <- ntr_roi_8$GRIDX[ntr_roi_8$ACT==1]
y.values <- ntr_roi_8$GRIDY[ntr_roi_8$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_8$ACT[ntr_roi_8$label %in%  disk$label] <- 1
}

sum(ntr_roi_8$ACT)

sum(ntr_roi_8$DEACT)

ID <- ntr_roi_8$ID[ntr_roi_8$DEACT==1]
x.values <- ntr_roi_8$GRIDX[ntr_roi_8$DEACT==1]
y.values <- ntr_roi_8$GRIDY[ntr_roi_8$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_8$DEACT[ntr_roi_8$label %in%  disk$label] <- 1
}

sum(ntr_roi_8$DEACT)

ntr_roi_8.a <- aggregate(ntr_roi_8, by = list(ntr_roi_8$ID), mean)
}





# ROI Nr. 9 w/ neutral comparison
if (nrofrois >= 9) {
emo_roi_9 <- as.data.frame(subset(dft, GRIDX > roi_9[1] & GRIDX < roi_9[2] & GRIDY > roi_9[3] &  GRIDY < roi_9[4])) # Limit reference DF to ROI
emo_roi_9$STIM <- roi_9[5] # Specify stimulus
emo_roi_9$MATCH <- paste0("ID", "_", emo_roi_9$ID, "_", emo_roi_9$STIM, "_", emo_roi_9$GRIDX, "_", emo_roi_9$GRIDY) # Create matching variable
emo_roi_9$ACT[emo_roi_9$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_9$DEACT[emo_roi_9$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_9$ACT) # Checking matching

emo_roi_9$label <- paste0(emo_roi_9$ID, "_", emo_roi_9$GRIDX, "_", emo_roi_9$GRIDY) # Create disk matching variable

ID <- emo_roi_9$ID[emo_roi_9$ACT==1]
x.values <- emo_roi_9$GRIDX[emo_roi_9$ACT==1]
y.values <- emo_roi_9$GRIDY[emo_roi_9$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_9$ACT[emo_roi_9$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_9$ACT) # Checking matching

sum(emo_roi_9$DEACT)

ID <- emo_roi_9$ID[emo_roi_9$DEACT==1]
x.values <- emo_roi_9$GRIDX[emo_roi_9$DEACT==1]
y.values <- emo_roi_9$GRIDY[emo_roi_9$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_9$DEACT[emo_roi_9$label %in%  disk$label] <- 1
}

sum(emo_roi_9$DEACT)

emo_roi_9.a <- aggregate(emo_roi_9, by = list(emo_roi_9$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_9 <- as.data.frame(subset(dft, GRIDX > roi_9[1] & GRIDX < roi_9[2] & GRIDY > roi_9[3] &  GRIDY < roi_9[4]))
ntr_roi_9$STIM <- "Neutral"
ntr_roi_9$MATCH <- paste0("ID", "_", ntr_roi_9$ID, "_", ntr_roi_9$STIM, "_", ntr_roi_9$GRIDX, "_", ntr_roi_9$GRIDY)
ntr_roi_9$ACT[ntr_roi_9$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_9$DEACT[ntr_roi_9$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_9$ACT)

ntr_roi_9$label <- paste0(ntr_roi_9$ID, "_",ntr_roi_9$GRIDX, "_", ntr_roi_9$GRIDY)

ID <- ntr_roi_9$ID[ntr_roi_9$ACT==1]
x.values <- ntr_roi_9$GRIDX[ntr_roi_9$ACT==1]
y.values <- ntr_roi_9$GRIDY[ntr_roi_9$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_9$ACT[ntr_roi_9$label %in%  disk$label] <- 1
}

sum(ntr_roi_9$ACT)

sum(ntr_roi_9$DEACT)

ID <- ntr_roi_9$ID[ntr_roi_9$DEACT==1]
x.values <- ntr_roi_9$GRIDX[ntr_roi_9$DEACT==1]
y.values <- ntr_roi_9$GRIDY[ntr_roi_9$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_9$DEACT[ntr_roi_9$label %in%  disk$label] <- 1
}

sum(ntr_roi_9$DEACT)

ntr_roi_9.a <- aggregate(ntr_roi_9, by = list(ntr_roi_9$ID), mean)
}





# ROI Nr. 10 w/ neutral comparison
if (nrofrois >= 10) {
emo_roi_10 <- as.data.frame(subset(dft, GRIDX > roi_10[1] & GRIDX < roi_10[2] & GRIDY > roi_10[3] &  GRIDY < roi_10[4])) # Limit reference DF to ROI
emo_roi_10$STIM <- roi_10[5] # Specify stimulus
emo_roi_10$MATCH <- paste0("ID", "_", emo_roi_10$ID, "_", emo_roi_10$STIM, "_", emo_roi_10$GRIDX, "_", emo_roi_10$GRIDY) # Create matching variable
emo_roi_10$ACT[emo_roi_10$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_10$DEACT[emo_roi_10$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_10$ACT) # Checking matching

emo_roi_10$label <- paste0(emo_roi_10$ID, "_", emo_roi_10$GRIDX, "_", emo_roi_10$GRIDY) # Create disk matching variable

ID <- emo_roi_10$ID[emo_roi_10$ACT==1]
x.values <- emo_roi_10$GRIDX[emo_roi_10$ACT==1]
y.values <- emo_roi_10$GRIDY[emo_roi_10$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_10$ACT[emo_roi_10$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_10$ACT) # Checking matching

sum(emo_roi_10$DEACT)

ID <- emo_roi_10$ID[emo_roi_10$DEACT==1]
x.values <- emo_roi_10$GRIDX[emo_roi_10$DEACT==1]
y.values <- emo_roi_10$GRIDY[emo_roi_10$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_10$DEACT[emo_roi_10$label %in%  disk$label] <- 1
}

sum(emo_roi_10$DEACT)

emo_roi_10.a <- aggregate(emo_roi_10, by = list(emo_roi_10$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_10 <- as.data.frame(subset(dft, GRIDX > roi_10[1] & GRIDX < roi_10[2] & GRIDY > roi_10[3] &  GRIDY < roi_10[4]))
ntr_roi_10$STIM <- "Neutral"
ntr_roi_10$MATCH <- paste0("ID", "_", ntr_roi_10$ID, "_", ntr_roi_10$STIM, "_", ntr_roi_10$GRIDX, "_", ntr_roi_10$GRIDY)
ntr_roi_10$ACT[ntr_roi_10$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_10$DEACT[ntr_roi_10$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_10$ACT)

ntr_roi_10$label <- paste0(ntr_roi_10$ID, "_",ntr_roi_10$GRIDX, "_", ntr_roi_10$GRIDY)

ID <- ntr_roi_10$ID[ntr_roi_10$ACT==1]
x.values <- ntr_roi_10$GRIDX[ntr_roi_10$ACT==1]
y.values <- ntr_roi_10$GRIDY[ntr_roi_10$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_10$ACT[ntr_roi_10$label %in%  disk$label] <- 1
}

sum(ntr_roi_10$ACT)

sum(ntr_roi_10$DEACT)

ID <- ntr_roi_10$ID[ntr_roi_10$DEACT==1]
x.values <- ntr_roi_10$GRIDX[ntr_roi_10$DEACT==1]
y.values <- ntr_roi_10$GRIDY[ntr_roi_10$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_10$DEACT[ntr_roi_10$label %in%  disk$label] <- 1
}

sum(ntr_roi_10$DEACT)

ntr_roi_10.a <- aggregate(ntr_roi_10, by = list(ntr_roi_10$ID), mean)
}




# ROI Nr. 11 w/ neutral comparison
if (nrofrois >= 11) {
emo_roi_11 <- as.data.frame(subset(dft, GRIDX > roi_11[1] & GRIDX < roi_11[2] & GRIDY > roi_11[3] &  GRIDY < roi_11[4])) # Limit reference DF to ROI
emo_roi_11$STIM <- roi_11[5] # Specify stimulus
emo_roi_11$MATCH <- paste0("ID", "_", emo_roi_11$ID, "_", emo_roi_11$STIM, "_", emo_roi_11$GRIDX, "_", emo_roi_11$GRIDY) # Create matching variable
emo_roi_11$ACT[emo_roi_11$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_11$DEACT[emo_roi_11$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_11$ACT) # Checking matching

emo_roi_11$label <- paste0(emo_roi_11$ID, "_", emo_roi_11$GRIDX, "_", emo_roi_11$GRIDY) # Create disk matching variable

ID <- emo_roi_11$ID[emo_roi_11$ACT==1]
x.values <- emo_roi_11$GRIDX[emo_roi_11$ACT==1]
y.values <- emo_roi_11$GRIDY[emo_roi_11$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_11$ACT[emo_roi_11$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_11$ACT) # Checking matching

sum(emo_roi_11$DEACT)

ID <- emo_roi_11$ID[emo_roi_11$DEACT==1]
x.values <- emo_roi_11$GRIDX[emo_roi_11$DEACT==1]
y.values <- emo_roi_11$GRIDY[emo_roi_11$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_11$DEACT[emo_roi_11$label %in%  disk$label] <- 1
}

sum(emo_roi_11$DEACT)

emo_roi_11.a <- aggregate(emo_roi_11, by = list(emo_roi_11$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_11 <- as.data.frame(subset(dft, GRIDX > roi_11[1] & GRIDX < roi_11[2] & GRIDY > roi_11[3] &  GRIDY < roi_11[4]))
ntr_roi_11$STIM <- "Neutral"
ntr_roi_11$MATCH <- paste0("ID", "_", ntr_roi_11$ID, "_", ntr_roi_11$STIM, "_", ntr_roi_11$GRIDX, "_", ntr_roi_11$GRIDY)
ntr_roi_11$ACT[ntr_roi_11$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_11$DEACT[ntr_roi_11$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_11$ACT)

ntr_roi_11$label <- paste0(ntr_roi_11$ID, "_",ntr_roi_11$GRIDX, "_", ntr_roi_11$GRIDY)

ID <- ntr_roi_11$ID[ntr_roi_11$ACT==1]
x.values <- ntr_roi_11$GRIDX[ntr_roi_11$ACT==1]
y.values <- ntr_roi_11$GRIDY[ntr_roi_11$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_11$ACT[ntr_roi_11$label %in%  disk$label] <- 1
}

sum(ntr_roi_11$ACT)

sum(ntr_roi_11$DEACT)

ID <- ntr_roi_11$ID[ntr_roi_11$DEACT==1]
x.values <- ntr_roi_11$GRIDX[ntr_roi_11$DEACT==1]
y.values <- ntr_roi_11$GRIDY[ntr_roi_11$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_11$DEACT[ntr_roi_11$label %in%  disk$label] <- 1
}

sum(ntr_roi_11$DEACT)

ntr_roi_11.a <- aggregate(ntr_roi_11, by = list(ntr_roi_11$ID), mean)
}





# ROI Nr. 12 w/ neutral comparison
if (nrofrois >= 12) {
emo_roi_12 <- as.data.frame(subset(dft, GRIDX > roi_12[1] & GRIDX < roi_12[2] & GRIDY > roi_12[3] &  GRIDY < roi_12[4])) # Limit reference DF to ROI
emo_roi_12$STIM <- roi_12[5] # Specify stimulus
emo_roi_12$MATCH <- paste0("ID", "_", emo_roi_12$ID, "_", emo_roi_12$STIM, "_", emo_roi_12$GRIDX, "_", emo_roi_12$GRIDY) # Create matching variable
emo_roi_12$ACT[emo_roi_12$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_12$DEACT[emo_roi_12$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_12$ACT) # Checking matching

emo_roi_12$label <- paste0(emo_roi_12$ID, "_", emo_roi_12$GRIDX, "_", emo_roi_12$GRIDY) # Create disk matching variable

ID <- emo_roi_12$ID[emo_roi_12$ACT==1]
x.values <- emo_roi_12$GRIDX[emo_roi_12$ACT==1]
y.values <- emo_roi_12$GRIDY[emo_roi_12$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_12$ACT[emo_roi_12$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_12$ACT) # Checking matching

sum(emo_roi_12$DEACT)

ID <- emo_roi_12$ID[emo_roi_12$DEACT==1]
x.values <- emo_roi_12$GRIDX[emo_roi_12$DEACT==1]
y.values <- emo_roi_12$GRIDY[emo_roi_12$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_12$DEACT[emo_roi_12$label %in%  disk$label] <- 1
}

sum(emo_roi_12$DEACT)

emo_roi_12.a <- aggregate(emo_roi_12, by = list(emo_roi_12$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_12 <- as.data.frame(subset(dft, GRIDX > roi_12[1] & GRIDX < roi_12[2] & GRIDY > roi_12[3] &  GRIDY < roi_12[4]))
ntr_roi_12$STIM <- "Neutral"
ntr_roi_12$MATCH <- paste0("ID", "_", ntr_roi_12$ID, "_", ntr_roi_12$STIM, "_", ntr_roi_12$GRIDX, "_", ntr_roi_12$GRIDY)
ntr_roi_12$ACT[ntr_roi_12$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_12$DEACT[ntr_roi_12$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_12$ACT)

ntr_roi_12$label <- paste0(ntr_roi_12$ID, "_",ntr_roi_12$GRIDX, "_", ntr_roi_12$GRIDY)

ID <- ntr_roi_12$ID[ntr_roi_12$ACT==1]
x.values <- ntr_roi_12$GRIDX[ntr_roi_12$ACT==1]
y.values <- ntr_roi_12$GRIDY[ntr_roi_12$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_12$ACT[ntr_roi_12$label %in%  disk$label] <- 1
}

sum(ntr_roi_12$ACT)

sum(ntr_roi_12$DEACT)

ID <- ntr_roi_12$ID[ntr_roi_12$DEACT==1]
x.values <- ntr_roi_12$GRIDX[ntr_roi_12$DEACT==1]
y.values <- ntr_roi_12$GRIDY[ntr_roi_12$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_12$DEACT[ntr_roi_12$label %in%  disk$label] <- 1
}

sum(ntr_roi_12$DEACT)

ntr_roi_12.a <- aggregate(ntr_roi_12, by = list(ntr_roi_12$ID), mean)
}





# ROI Nr. 13 w/ neutral comparison
if (nrofrois >= 13) {
emo_roi_13 <- as.data.frame(subset(dft, GRIDX > roi_13[1] & GRIDX < roi_13[2] & GRIDY > roi_13[3] &  GRIDY < roi_13[4])) # Limit reference DF to ROI
emo_roi_13$STIM <- roi_13[5] # Specify stimulus
emo_roi_13$MATCH <- paste0("ID", "_", emo_roi_13$ID, "_", emo_roi_13$STIM, "_", emo_roi_13$GRIDX, "_", emo_roi_13$GRIDY) # Create matching variable
emo_roi_13$ACT[emo_roi_13$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_13$DEACT[emo_roi_13$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_13$ACT) # Checking matching

emo_roi_13$label <- paste0(emo_roi_13$ID, "_", emo_roi_13$GRIDX, "_", emo_roi_13$GRIDY) # Create disk matching variable

ID <- emo_roi_13$ID[emo_roi_13$ACT==1]
x.values <- emo_roi_13$GRIDX[emo_roi_13$ACT==1]
y.values <- emo_roi_13$GRIDY[emo_roi_13$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_13$ACT[emo_roi_13$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_13$ACT) # Checking matching

sum(emo_roi_13$DEACT)

ID <- emo_roi_13$ID[emo_roi_13$DEACT==1]
x.values <- emo_roi_13$GRIDX[emo_roi_13$DEACT==1]
y.values <- emo_roi_13$GRIDY[emo_roi_13$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_13$DEACT[emo_roi_13$label %in%  disk$label] <- 1
}

sum(emo_roi_13$DEACT)

emo_roi_13.a <- aggregate(emo_roi_13, by = list(emo_roi_13$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_13 <- as.data.frame(subset(dft, GRIDX > roi_13[1] & GRIDX < roi_13[2] & GRIDY > roi_13[3] &  GRIDY < roi_13[4]))
ntr_roi_13$STIM <- "Neutral"
ntr_roi_13$MATCH <- paste0("ID", "_", ntr_roi_13$ID, "_", ntr_roi_13$STIM, "_", ntr_roi_13$GRIDX, "_", ntr_roi_13$GRIDY)
ntr_roi_13$ACT[ntr_roi_13$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_13$DEACT[ntr_roi_13$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_13$ACT)

ntr_roi_13$label <- paste0(ntr_roi_13$ID, "_",ntr_roi_13$GRIDX, "_", ntr_roi_13$GRIDY)

ID <- ntr_roi_13$ID[ntr_roi_13$ACT==1]
x.values <- ntr_roi_13$GRIDX[ntr_roi_13$ACT==1]
y.values <- ntr_roi_13$GRIDY[ntr_roi_13$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_13$ACT[ntr_roi_13$label %in%  disk$label] <- 1
}

sum(ntr_roi_13$ACT)

sum(ntr_roi_13$DEACT)

ID <- ntr_roi_13$ID[ntr_roi_13$DEACT==1]
x.values <- ntr_roi_13$GRIDX[ntr_roi_13$DEACT==1]
y.values <- ntr_roi_13$GRIDY[ntr_roi_13$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_13$DEACT[ntr_roi_13$label %in%  disk$label] <- 1
}

sum(ntr_roi_13$DEACT)

ntr_roi_13.a <- aggregate(ntr_roi_13, by = list(ntr_roi_13$ID), mean)
}





# ROI Nr. 14 w/ neutral comparison
if (nrofrois >= 14) {
emo_roi_14 <- as.data.frame(subset(dft, GRIDX > roi_14[1] & GRIDX < roi_14[2] & GRIDY > roi_14[3] &  GRIDY < roi_14[4])) # Limit reference DF to ROI
emo_roi_14$STIM <- roi_14[5] # Specify stimulus
emo_roi_14$MATCH <- paste0("ID", "_", emo_roi_14$ID, "_", emo_roi_14$STIM, "_", emo_roi_14$GRIDX, "_", emo_roi_14$GRIDY) # Create matching variable
emo_roi_14$ACT[emo_roi_14$MATCH %in% dfr$MATCH_L] <- 1 
emo_roi_14$DEACT[emo_roi_14$MATCH %in% dfr$MATCH_R] <- 1 # Matching for activation and deactivation

sum(emo_roi_14$ACT) # Checking matching

emo_roi_14$label <- paste0(emo_roi_14$ID, "_", emo_roi_14$GRIDX, "_", emo_roi_14$GRIDY) # Create disk matching variable

ID <- emo_roi_14$ID[emo_roi_14$ACT==1]
x.values <- emo_roi_14$GRIDX[emo_roi_14$ACT==1]
y.values <- emo_roi_14$GRIDY[emo_roi_14$ACT==1] # Extract identifiers


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i]) # Iteratively replace disk with current coordinates
  emo_roi_14$ACT[emo_roi_14$label %in%  disk$label] <- 1 # Matching
}

sum(emo_roi_14$ACT) # Checking matching

sum(emo_roi_14$DEACT)

ID <- emo_roi_14$ID[emo_roi_14$DEACT==1]
x.values <- emo_roi_14$GRIDX[emo_roi_14$DEACT==1]
y.values <- emo_roi_14$GRIDY[emo_roi_14$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_14$DEACT[emo_roi_14$label %in%  disk$label] <- 1
}

sum(emo_roi_14$DEACT)

emo_roi_14.a <- aggregate(emo_roi_14, by = list(emo_roi_14$ID), mean) # Aggregate activation and deactivation in ROI for stimulus and ID


ntr_roi_14 <- as.data.frame(subset(dft, GRIDX > roi_14[1] & GRIDX < roi_14[2] & GRIDY > roi_14[3] &  GRIDY < roi_14[4]))
ntr_roi_14$STIM <- "Neutral"
ntr_roi_14$MATCH <- paste0("ID", "_", ntr_roi_14$ID, "_", ntr_roi_14$STIM, "_", ntr_roi_14$GRIDX, "_", ntr_roi_14$GRIDY)
ntr_roi_14$ACT[ntr_roi_14$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_14$DEACT[ntr_roi_14$MATCH %in% dfr$MATCH_R] <- 1

sum(ntr_roi_14$ACT)

ntr_roi_14$label <- paste0(ntr_roi_14$ID, "_",ntr_roi_14$GRIDX, "_", ntr_roi_14$GRIDY)

ID <- ntr_roi_14$ID[ntr_roi_14$ACT==1]
x.values <- ntr_roi_14$GRIDX[ntr_roi_14$ACT==1]
y.values <- ntr_roi_14$GRIDY[ntr_roi_14$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_14$ACT[ntr_roi_14$label %in%  disk$label] <- 1
}

sum(ntr_roi_14$ACT)

sum(ntr_roi_14$DEACT)

ID <- ntr_roi_14$ID[ntr_roi_14$DEACT==1]
x.values <- ntr_roi_14$GRIDX[ntr_roi_14$DEACT==1]
y.values <- ntr_roi_14$GRIDY[ntr_roi_14$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_14$DEACT[ntr_roi_14$label %in%  disk$label] <- 1
}

sum(ntr_roi_14$DEACT)

ntr_roi_14.a <- aggregate(ntr_roi_14, by = list(ntr_roi_14$ID), mean)
}









##################################
### 2.3 Merging processed data ###
##################################
embody <- merge(embody, emo_roi_1.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_1[5], "_ROI_1_ACT"), "DEACT"=paste0(roi_1[5], "_ROI_1_DEACT")))
embody <- merge(embody, ntr_roi_1.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_1_ACT", "DEACT"="NEUTRAL_ROI_1_DEACT"))


if (nrofrois >= 2) {
embody <- merge(embody, emo_roi_2.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_2[5], "_ROI_2_ACT"), "DEACT"=paste0(roi_2[5], "_ROI_2_DEACT")))
embody <- merge(embody, ntr_roi_2.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_2_ACT", "DEACT"="NEUTRAL_ROI_2_DEACT"))
}


if (nrofrois >= 3) {
embody <- merge(embody, emo_roi_3.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_3[5], "_ROI_3_ACT"), "DEACT"=paste0(roi_3[5], "_ROI_3_DEACT")))
embody <- merge(embody, ntr_roi_3.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_3_ACT", "DEACT"="NEUTRAL_ROI_3_DEACT"))
}


if (nrofrois >= 4) {
embody <- merge(embody, emo_roi_4.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_4[5], "_ROI_4_ACT"), "DEACT"=paste0(roi_4[5], "_ROI_4_DEACT")))
embody <- merge(embody, ntr_roi_4.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_4_ACT", "DEACT"="NEUTRAL_ROI_4_DEACT"))
}


if (nrofrois >= 5) {
embody <- merge(embody, emo_roi_5.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_5[5], "_ROI_5_ACT"), "DEACT"=paste0(roi_5[5], "_ROI_5_DEACT")))
embody <- merge(embody, ntr_roi_5.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_5_ACT", "DEACT"="NEUTRAL_ROI_5_DEACT"))
}


if (nrofrois >= 6) {
embody <- merge(embody, emo_roi_6.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_6[5], "_ROI_6_ACT"), "DEACT"=paste0(roi_6[5], "_ROI_6_DEACT")))
embody <- merge(embody, ntr_roi_6.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_6_ACT", "DEACT"="NEUTRAL_ROI_6_DEACT"))
}


if (nrofrois >= 7) {
embody <- merge(embody, emo_roi_7.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_7[5], "_ROI_7_ACT"), "DEACT"=paste0(roi_7[5], "_ROI_7_DEACT")))
embody <- merge(embody, ntr_roi_7.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_7_ACT", "DEACT"="NEUTRAL_ROI_7_DEACT"))
}


if (nrofrois >= 8) {
embody <- merge(embody, emo_roi_8.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_8[5], "_ROI_8_ACT"), "DEACT"=paste0(roi_8[5], "_ROI_8_DEACT")))
embody <- merge(embody, ntr_roi_8.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_8_ACT", "DEACT"="NEUTRAL_ROI_8_DEACT"))
}


if (nrofrois >= 9) {
embody <- merge(embody, emo_roi_9.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_9[5], "_ROI_9_ACT"), "DEACT"=paste0(roi_9[5], "_ROI_9_DEACT")))
embody <- merge(embody, ntr_roi_9.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_9_ACT", "DEACT"="NEUTRAL_ROI_9_DEACT"))
}


if (nrofrois >= 10) {
embody <- merge(embody, emo_roi_10.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_10[5], "_ROI_10_ACT"), "DEACT"=paste0(roi_10[5], "_ROI_10_DEACT")))
embody <- merge(embody, ntr_roi_10.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_10_ACT", "DEACT"="NEUTRAL_ROI_10_DEACT"))
}


if (nrofrois >= 11) {
embody <- merge(embody, emo_roi_11.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_11[5], "_ROI_11_ACT"), "DEACT"=paste0(roi_11[5], "_ROI_11_DEACT")))
embody <- merge(embody, ntr_roi_11.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_11_ACT", "DEACT"="NEUTRAL_ROI_11_DEACT"))
}


if (nrofrois >= 12) {
embody <- merge(embody, emo_roi_12.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_12[5], "_ROI_12_ACT"), "DEACT"=paste0(roi_12[5], "_ROI_12_DEACT")))
embody <- merge(embody, ntr_roi_12.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_12_ACT", "DEACT"="NEUTRAL_ROI_12_DEACT"))
}


if (nrofrois >= 13) {
embody <- merge(embody, emo_roi_13.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_13[5], "_ROI_13_ACT"), "DEACT"=paste0(roi_13[5], "_ROI_13_DEACT")))
embody <- merge(embody, ntr_roi_13.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_13_ACT", "DEACT"="NEUTRAL_ROI_13_DEACT"))
}


if (nrofrois >= 14) {
embody <- merge(embody, emo_roi_14.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_14[5], "_ROI_14_ACT"), "DEACT"=paste0(roi_14[5], "_ROI_14_DEACT")))
embody <- merge(embody, ntr_roi_14.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"="NEUTRAL_ROI_14_ACT", "DEACT"="NEUTRAL_ROI_14_DEACT"))
}

fwrite(embody,file="emBODY_2_OUTPUT.csv") # Write output file