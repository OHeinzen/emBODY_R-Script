#######################
### emBODY_R-Script ###
#######################
###########################################
### 3. Extended ROI-specific processing ###
###########################################
######################################
### 3.1 Instructions & preparation ###
######################################

roi_A <- list(65, 107, 4, 244, "Sadness") # Formerly the legs
roi_Ba <- list(53, 66, 88, 244, "Sadness")
roi_Bb <- list(106, 119, 88, 244, "Sadness")
roi_Ca <- list(59, 66, 66, 89, "Sadness")
roi_Cb <- list(106, 113, 66, 89, "Sadness") 

# ROIs are defined by 1-pixel wide lines. Thus, for ROIs next to each other, these lines need to be accounted for.
# Here, for legs 2 & 3, right (a, 65->66) and left (b, 107->106) x-coordinates were moved 1 pixel towards the center.
# Further, in legs 3, the same was done for the upper y-coordinate (88->89).

roi_D_m <- list(79, 93, 36, 210, "Sadness")
roi_E_m <- list(80, 92, 4, 26, "Sadness") # Specify ROIs

name <- "FIRST" # Add unqiue name for each time you run this script

dft <- data.frame(
  ID =  rep(d, each = grid),
  GRIDX =  rep(rep(c(1:171),each = 522), length(d)),
  GRIDY =  rep(rep(seq(1:522), times = 171), times = length(d)),
  ACT = 0,
  DEACT = 0
) # Create reference data frame

#################################################
### 3.2 ROI matching (including substraction) ###
#################################################
emo_roi_A  <- as.data.frame(subset(dft, GRIDX > roi_A[1] & GRIDX < roi_A[2] & GRIDY > roi_A[3] &  GRIDY < roi_A[4]))
emo_roi_A$STIM <- roi_A[5]
emo_roi_A$MATCH <- paste0("ID", "_", emo_roi_A$ID, "_", emo_roi_A$STIM, "_", emo_roi_A$GRIDX, "_", emo_roi_A$GRIDY)
emo_roi_A$ACT[emo_roi_A$MATCH %in% dfr$MATCH_L] <- 1
emo_roi_A$DEACT[emo_roi_A$MATCH %in% dfr$MATCH_R] <- 1

emo_roi_Ba <- as.data.frame(subset(dft, GRIDX > roi_Ba[1] & GRIDX < roi_Ba[2] & GRIDY > roi_Ba[3] &  GRIDY < roi_Ba[4]))
emo_roi_Ba$STIM <- roi_Ba[5]
emo_roi_Ba$MATCH <- paste0("ID", "_", emo_roi_Ba$ID, "_", emo_roi_Ba$STIM, "_", emo_roi_Ba$GRIDX, "_", emo_roi_Ba$GRIDY)
emo_roi_Ba$ACT[emo_roi_Ba$MATCH %in% dfr$MATCH_L] <- 1
emo_roi_Ba$DEACT[emo_roi_Ba$MATCH %in% dfr$MATCH_R] <- 1

emo_roi_Bb <- as.data.frame(subset(dft, GRIDX > roi_Bb[1] & GRIDX < roi_Bb[2] & GRIDY > roi_Bb[3] &  GRIDY < roi_Bb[4]))
emo_roi_Bb$STIM <- roi_Bb[5]
emo_roi_Bb$MATCH <- paste0("ID", "_", emo_roi_Bb$ID, "_", emo_roi_Bb$STIM, "_", emo_roi_Bb$GRIDX, "_", emo_roi_Bb$GRIDY)
emo_roi_Bb$ACT[emo_roi_Bb$MATCH %in% dfr$MATCH_L] <- 1
emo_roi_Bb$DEACT[emo_roi_Bb$MATCH %in% dfr$MATCH_R] <- 1

emo_roi_Ca <- as.data.frame(subset(dft, GRIDX > roi_Ca[1] & GRIDX < roi_Ca[2] & GRIDY > roi_Ca[3] &  GRIDY < roi_Ca[4]))
emo_roi_Ca$STIM <- roi_Ca[5]
emo_roi_Ca$MATCH <- paste0("ID", "_", emo_roi_Ca$ID, "_", emo_roi_Ca$STIM, "_", emo_roi_Ca$GRIDX, "_", emo_roi_Ca$GRIDY)
emo_roi_Ca$ACT[emo_roi_Ca$MATCH %in% dfr$MATCH_L] <- 1
emo_roi_Ca$DEACT[emo_roi_Ca$MATCH %in% dfr$MATCH_R] <- 1

emo_roi_Cb <- as.data.frame(subset(dft, GRIDX > roi_Cb[1] & GRIDX < roi_Cb[2] & GRIDY > roi_Cb[3] &  GRIDY < roi_Cb[4]))
emo_roi_Cb$STIM <- roi_Cb[5]
emo_roi_Cb$MATCH <- paste0("ID", "_", emo_roi_Cb$ID, "_", emo_roi_Cb$STIM, "_", emo_roi_Cb$GRIDX, "_", emo_roi_Cb$GRIDY)
emo_roi_Cb$ACT[emo_roi_Cb$MATCH %in% dfr$MATCH_L] <- 1
emo_roi_Cb$DEACT[emo_roi_Cb$MATCH %in% dfr$MATCH_R] <- 1

emo_roi_m <- emo_roi_A
emo_roi_m <- rbind(emo_roi_m, 
                      emo_roi_Ba,
                      emo_roi_Bb,
                      emo_roi_Ca,
                      emo_roi_Cb)

emo_roi_D <- as.data.frame(subset(dft, GRIDX > roi_D_m[1] & GRIDX < roi_D_m[2] & GRIDY > roi_D_m[3] &  GRIDY < roi_D_m[4]))
emo_roi_D$STIM <- roi_D_m[5]
emo_roi_D$MATCH <- paste0("ID", "_", emo_roi_D$ID, "_", emo_roi_D$STIM, "_", emo_roi_D$GRIDX, "_", emo_roi_D$GRIDY)
emo_roi_D$ACT[emo_roi_D$MATCH %in% dfr$MATCH_L] <- 1
emo_roi_D$DEACT[emo_roi_D$MATCH %in% dfr$MATCH_R] <- 1

emo_roi_E <- as.data.frame(subset(dft, GRIDX > roi_E_m[1] & GRIDX < roi_E_m[2] & GRIDY > roi_E_m[3] &  GRIDY < roi_E_m[4]))
emo_roi_E$STIM <- roi_E_m[5]
emo_roi_E$MATCH <- paste0("ID", "_", emo_roi_E$ID, "_", emo_roi_E$STIM, "_", emo_roi_E$GRIDX, "_", emo_roi_E$GRIDY)
emo_roi_E$ACT[emo_roi_E$MATCH %in% dfr$MATCH_L] <- 1
emo_roi_E$DEACT[emo_roi_E$MATCH %in% dfr$MATCH_R] <- 1

emo_roi_m <- emo_roi_m[!(emo_roi_m$MATCH %in% emo_roi_D$MATCH),]
emo_roi_m <- emo_roi_m[!(emo_roi_m$MATCH %in% emo_roi_E$MATCH),]


sum(emo_roi_m$ACT)

emo_roi_m$label <- paste0(emo_roi_m$ID, "_",emo_roi_m$GRIDX, "_", emo_roi_m$GRIDY)

ID <- emo_roi_m$ID[emo_roi_m$ACT==1]
x.values <- emo_roi_m$GRIDX[emo_roi_m$ACT==1]
y.values <- emo_roi_m$GRIDY[emo_roi_m$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_m$ACT[emo_roi_m$label %in%  disk$label] <- 1
}

sum(emo_roi_m$ACT)

sum(emo_roi_m$DEACT)

ID <- emo_roi_m$ID[emo_roi_m$DEACT==1]
x.values <- emo_roi_m$GRIDX[emo_roi_m$DEACT==1]
y.values <- emo_roi_m$GRIDY[emo_roi_m$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  emo_roi_m$DEACT[emo_roi_m$label %in%  disk$label] <- 1
}

sum(emo_roi_m$DEACT)

emo_roi_m.a <- aggregate(emo_roi_m, by = list(emo_roi_m$ID), mean)


ntr_roi_A <- as.data.frame(subset(dft, GRIDX > roi_A[1] & GRIDX < roi_A[2] & GRIDY > roi_A[3] &  GRIDY < roi_A[4]))
ntr_roi_A$STIM <- "Neutral"
ntr_roi_A$MATCH <- paste0("ID", "_", ntr_roi_A$ID, "_", ntr_roi_A$STIM, "_", ntr_roi_A$GRIDX, "_", ntr_roi_A$GRIDY)
ntr_roi_A$ACT[ntr_roi_A$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_A$DEACT[ntr_roi_A$MATCH %in% dfr$MATCH_R] <- 1

ntr_roi_Ba <- as.data.frame(subset(dft, GRIDX > roi_Ba[1] & GRIDX < roi_Ba[2] & GRIDY > roi_Ba[3] &  GRIDY < roi_Ba[4]))
ntr_roi_Ba$STIM <- "Neutral"
ntr_roi_Ba$MATCH <- paste0("ID", "_", ntr_roi_Ba$ID, "_", ntr_roi_Ba$STIM, "_", ntr_roi_Ba$GRIDX, "_", ntr_roi_Ba$GRIDY)
ntr_roi_Ba$ACT[ntr_roi_Ba$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_Ba$DEACT[ntr_roi_Ba$MATCH %in% dfr$MATCH_R] <- 1

ntr_roi_Bb <- as.data.frame(subset(dft, GRIDX > roi_Bb[1] & GRIDX < roi_Bb[2] & GRIDY > roi_Bb[3] &  GRIDY < roi_Bb[4]))
ntr_roi_Bb$STIM <- "Neutral"
ntr_roi_Bb$MATCH <- paste0("ID", "_", ntr_roi_Bb$ID, "_", ntr_roi_Bb$STIM, "_", ntr_roi_Bb$GRIDX, "_", ntr_roi_Bb$GRIDY)
ntr_roi_Bb$ACT[ntr_roi_Bb$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_Bb$DEACT[ntr_roi_Bb$MATCH %in% dfr$MATCH_R] <- 1

ntr_roi_Ca <- as.data.frame(subset(dft, GRIDX > roi_Ca[1] & GRIDX < roi_Ca[2] & GRIDY > roi_Ca[3] &  GRIDY < roi_Ca[4]))
ntr_roi_Ca$STIM <- "Neutral"
ntr_roi_Ca$MATCH <- paste0("ID", "_", ntr_roi_Ca$ID, "_", ntr_roi_Ca$STIM, "_", ntr_roi_Ca$GRIDX, "_", ntr_roi_Ca$GRIDY)
ntr_roi_Ca$ACT[ntr_roi_Ca$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_Ca$DEACT[ntr_roi_Ca$MATCH %in% dfr$MATCH_R] <- 1

ntr_roi_Cb <- as.data.frame(subset(dft, GRIDX > roi_Cb[1] & GRIDX < roi_Cb[2] & GRIDY > roi_Cb[3] &  GRIDY < roi_Cb[4]))
ntr_roi_Cb$STIM <- "Neutral"
ntr_roi_Cb$MATCH <- paste0("ID", "_", ntr_roi_Cb$ID, "_", ntr_roi_Cb$STIM, "_", ntr_roi_Cb$GRIDX, "_", ntr_roi_Cb$GRIDY)
ntr_roi_Cb$ACT[ntr_roi_Cb$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_Cb$DEACT[ntr_roi_Cb$MATCH %in% dfr$MATCH_R] <- 1

ntr_roi_m <- ntr_roi_A
ntr_roi_m <- rbind(ntr_roi_m, 
                      ntr_roi_Ba,
                      ntr_roi_Bb,
                      ntr_roi_Ca,
                      ntr_roi_Cb)

ntr_roi_D <- as.data.frame(subset(dft, GRIDX > roi_D_m[1] & GRIDX < roi_D_m[2] & GRIDY > roi_D_m[3] &  GRIDY < roi_D_m[4]))
ntr_roi_D$STIM <- "Neutral"
ntr_roi_D$MATCH <- paste0("ID", "_", ntr_roi_D$ID, "_", ntr_roi_D$STIM, "_", ntr_roi_D$GRIDX, "_", ntr_roi_D$GRIDY)
ntr_roi_D$ACT[ntr_roi_D$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_D$DEACT[ntr_roi_D$MATCH %in% dfr$MATCH_R] <- 1

ntr_roi_E <- as.data.frame(subset(dft, GRIDX > roi_E_m[1] & GRIDX < roi_E_m[2] & GRIDY > roi_E_m[3] &  GRIDY < roi_E_m[4]))
ntr_roi_E$STIM <- "Neutral"
ntr_roi_E$MATCH <- paste0("ID", "_", ntr_roi_E$ID, "_", ntr_roi_E$STIM, "_", ntr_roi_E$GRIDX, "_", ntr_roi_E$GRIDY)
ntr_roi_E$ACT[ntr_roi_E$MATCH %in% dfr$MATCH_L] <- 1
ntr_roi_E$DEACT[ntr_roi_E$MATCH %in% dfr$MATCH_R] <- 1

ntr_roi_m <- ntr_roi_m[!(ntr_roi_m$MATCH %in% ntr_roi_D$MATCH),]
ntr_roi_m <- ntr_roi_m[!(ntr_roi_m$MATCH %in% ntr_roi_E$MATCH),]

ntr_roi_m$label <- paste0(ntr_roi_m$ID, "_",ntr_roi_m$GRIDX, "_", ntr_roi_m$GRIDY)

ID <- ntr_roi_m$ID[ntr_roi_m$ACT==1]
x.values <- ntr_roi_m$GRIDX[ntr_roi_m$ACT==1]
y.values <- ntr_roi_m$GRIDY[ntr_roi_m$ACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_m$ACT[ntr_roi_m$label %in%  disk$label] <- 1
}

sum(ntr_roi_m$ACT)
sum(ntr_roi_m$DEACT)

ID <- ntr_roi_m$ID[ntr_roi_m$DEACT==1]
x.values <- ntr_roi_m$GRIDX[ntr_roi_m$DEACT==1]
y.values <- ntr_roi_m$GRIDY[ntr_roi_m$DEACT==1]


for(i in 1:length(x.values))
{
  disk$label <- paste0(ID[i], "_", disk$x + x.values[i], "_", disk$y + y.values[i])
  ntr_roi_m$DEACT[ntr_roi_m$label %in%  disk$label] <- 1
}

sum(ntr_roi_m$DEACT)

ntr_roi_m.a <- aggregate(ntr_roi_m, by = list(ntr_roi_m$ID), mean)


##################################
### 2.3 Merging processed data ###
##################################
embody <- merge(embody, emo_roi_m.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0(roi_A[5], "_ROI_M_", name, "_ACT"), "DEACT"=paste0(roi_A[5], "_ROI_M_", name, "_DEACT")))
embody <- merge(embody, ntr_roi_m.a[c(1, 5, 6)], by.x = "ID", by.y="Group.1")
embody <- rename(embody, c("ACT"=paste0("NEUTRAL_ROI_M_", name, "_ACT"), "DEACT"=paste0("NEUTRAL_ROI_M_", name, "_DEACT")))

fwrite(embody,file="emBODY_3_OUTPUT.csv") # Write output file