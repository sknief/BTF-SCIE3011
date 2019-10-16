### code for PCA analysis and associated graphs ####


################# Start Commands ####################

attach(Legit_data)

#####################################################

#### Subsetting data ####

C14 <- subset(Legit_data, Legit_data$`Core No.` == "C14")
C5 <- subset(Legit_data, Legit_data$`Core No.` == "C5")
C15 <- subset(Legit_data, Legit_data$`Core No.` == "C15")
C6 <- subset(Legit_data, Legit_data$`Core No.` == "C6")
C8 <- subset(Legit_data, Legit_data$`Core No.` == "C8")
C12 <- subset(Legit_data, Legit_data$`Core No.` == "C12")
C18 <- subset(Legit_data, Legit_data$`Core No.` == "C18")


#### PCA code ####
PCALL <- prcomp(Legit_data[, (c(4:5,7:9))], center = TRUE, scale. = TRUE)
print(PCALL)
scores(PCALL)
pcamatrix <- matrix(data = PCALL$x, nrow = 27, ncol = 2)

#all - general thing that works
plot(pcamatrix, type = "n")
points(x = pcamatrix[,1], y = pcamatrix[,2])

#detailed and colour coding
plot(pcamatrix, type = "n", xlab = "PCA 1", ylab = "PCA 2")
points(x = pcamatrix[1:6,1], y = pcamatrix[1:6,2], col = "red", type = "b", pch = 2) #c14
points(x = pcamatrix[7:8,1], y = pcamatrix[7:8,2], col = "orange", type = "b", pch = 3) #c5
points(x = pcamatrix[9:13,1], y = pcamatrix[9:13,2], col = "brown", type = "b", pch = 2) #c15
points(x = pcamatrix[14:16,1], y = pcamatrix[14:16,2], col = "green", type = "b", pch = 3) #c6
points(x = pcamatrix[17:18,1], y = pcamatrix[17:18,2], col = "lightblue", type = "b", pch = 4) #c8
points(x = pcamatrix[19:22,1], y = pcamatrix[19:22,2], col = "blue", type = "b", pch = 3) #c12
points(x = pcamatrix[23:25,1], y = pcamatrix[23:25,2], col = "purple", type = "b", pch = 2) #c3
points(x = pcamatrix[26:27,1], y = pcamatrix[26:27,2], col = "black", type = "b", pch = 4) #c18
legend("topleft", legend=c("C14", "C5", "C15", "C6", "C8", "C12", "C3", "C18"), col=c("red", "orange", "brown", "green", "lightblue", "blue", "purple", "black"),  lty=1, cex=0.8)

#PCA 1 was pretty much purely age related, pointing right
#Diagonal is the foram non foram
#triangle = Lagoon Adjacent ( 2 )
#plus = reef edge ( 3 )
#cross = central apron ( 4 )

#loadings
loadings <-  matrix(data = PCALL$rotation, nrow = 5, ncol = 2)
print(loadings)
print(PCALL$rotation)

plot(pcamatrix, type = "n", xlab = "PCA 1", ylab = "PCA 2", xlim = c(-1, 1), ylim = c(-1, 1))
points(x = 0, y = 0)
arrows(x0 = 0, y0= 0, x1 = loadings[1,1], y1 = loadings[1,2], length = 0.15, angle = 30, code = 2, col = "red") #MSL
arrows(x0 = 0, y0= 0, x1 = loadings[2,1], y1 = loadings[2,2], length = 0.15, angle = 30, code = 2, col = "orange") #MSL
arrows(x0 = 0, y0= 0, x1 = loadings[3,1], y1 = loadings[3,2], length = 0.15, angle = 30, code = 2, col = "green") #MSL
arrows(x0 = 0, y0= 0, x1 = loadings[4,1], y1 = loadings[4,2], length = 0.15, angle = 30, code = 2, col = "blue") #MSL
arrows(x0 = 0, y0= 0, x1 = loadings[5,1], y1 = loadings[5,2], length = 0.15, angle = 30, code = 2, col = "purple") #MSL
legend("topleft", legend=c("MSL (depth M)", "Median Age (cal BP)", "Core Depth (mm)", "Foram percentage", "Non-foram percentage"), col=c("red", "orange", "green", "blue", "purple"),  lty=1, cex=0.7)
