birb <- read.csv("birds.csv")

summary(birb)

pairs(~density+size+distance+altitude+time, data = birb)

birb_1 <- subset(birb, select = -c(X, alien, protea, vegtype))
cormat <- cor(birb_1, use = "pairwise.complete.obs")


sd(birb_1$size)
sd(birb_1$distance)
sd(birb_1$altitude)
sd(birb_1$time)
sd(birb_1$density)

IQR(birb_1$size)
IQR(birb_1$distance)
IQR(birb_1$altitude)
IQR(birb_1$time)
IQR(birb_1$density)

hist(birb_1$size,main = "Size of Fynbos Patch", xlab = "Size")
hist(birb_1$distance, main = "Distance to the Nearest Patch", xlab = "Distance")
hist(birb_1$altitude, main = "Altitude", xlab = "Altitude")
hist(birb_1$time, main = "Time Since Last Fire", xlab = "Time")
hist(birb_1$density, main = "Population Density of Birds", xlab = "Density")

plot(density ~ size, data = birb, main = "Density vs Size", ylab = "Population Density of Birds [per Hectare]", xlab = "Size of Fynbos Patch [Hectares]", pch = 16)
DvS <-lm(density ~ size, data = birb)
abline(DvS, col = "purple", lwd = 3)
plot(density ~ distance, data = birb, main = "Density vs Distance", ylab = "Population Density of Birds [per Hectare]", xlab = "Distance to Nearest Patch [Km]", pch = 16)
DvD <-lm(density ~ distance, data = birb)
abline(DvD, col = "purple", lwd = 3)
plot(density ~ altitude, data = birb, main = "Density vs Altitude", ylab = "Population Density of Birds [per Hectare]", xlab = "Altitude [m]",pch = 16)
DvA <-lm(density ~ altitude, data = birb)
abline(DvA, col = "purple", lwd = 3)
plot(density ~ time, data = birb, main = "Density vs Time", ylab = "Population Density of Birds [per Hectare]", xlab = "Time Since Last Fire [Years]", pch = 16)
DvT <-lm(density ~ time, data = birb)
abline(DvT, col = "purple", lwd = 3)

boxplot(density ~ alien, data = birb, main = "Density and Alien Vegetation", ylab = "Population Density of Birds [per Hectare]", xlab = "Alien Veg")
boxplot(density ~ protea, data = birb, main = "Density and Protea", ylab = "Population Density of Birds [per Hectare]", xlab = "Protea")
boxplot(density ~ vegtype, data = birb, main = "Density and Vegetaion Type", ylab = "Population Density of Birds [per Hectare]", xlab = "Type")


H1 <- lm(density ~ protea + vegtype, data = birb)
summary(H1)

H2a <- lm(density ~ time + vegtype + alien, data = birb)
summary(H2a)
H2b <- lm(density ~ time + vegtype + alien + time:alien, data = birb)
summary(H2b)

H3 <- lm(density ~ distance + size, data = birb)
summary(H3)

H4a <- lm(density ~ alien + altitude, data = birb)
summary(H4a)
A <- birb$altitude^2
H4b <- lm(density ~ alien + altitude + A, data = birb)
summary(H4b) 


AIC_values <- c(AIC(H1), AIC(H2a), AIC(H2b), AIC(H3), AIC(H4a), AIC(H4b))
model_names <- c("H1", "H2a", "H2b", "H3", "H4a", "H4b")
aic_table <- data.frame(Model = model_names, AIC = AIC_values)
aic_table$AIC_weight <- exp((min(aic_table$AIC) - aic_table$AIC) / 2) / sum(exp((min(aic_table$AIC) - aic_table$AIC) / 2))
print(aic_table)

res_H2b <- residuals(H2b)
plot(predict(H2b), res_H2b, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot of H2b", pch = 16)
points(predict(H2b)[c(4, 44, 63)], res_H2b[c(4, 44, 63)], col = "red", pch = 16)
text(predict(H2b)[c(4, 44, 63)], res_H2b[c(4, 44, 63)], labels = c(4, 44, 63), pos = 4, col = "blue")
abline(h = 0, col = "yellow", lwd = 3)

res_H2a <- residuals(H2a)
plot(predict(H2a), res_H2a, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot of H2a", pch = 16)
points(predict(H2a)[c(4, 44, 63)], res_H2a[c(4, 44, 63)], col = "red", pch = 16)
text(predict(H2a)[c(4, 44, 63)], res_H2a[c(4, 44, 63)], labels = c(4, 44, 63), pos = 4, col = "blue")
abline(h = 0, col = "yellow", lwd = 3)

res_H4b <- residuals(H4b)
plot(predict(H4b), res_H4b, xlab = "Predicted Values", ylab = "Residuals", main = "Residual Plot of H4b", pch = 16)
points(predict(H4b)[c(4, 44, 63)], res_H4b[c(4, 44, 63)], col = "red", pch = 16)
text(predict(H4b)[c(4, 44, 63)], res_H4b[c(4, 44, 63)], labels = c(4, 44, 63), pos = 4, col = "blue")
abline(h = 0, col = "yellow", lwd = 3)