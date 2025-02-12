######################
#### Lake Manchar ####
######################

library(terra)
library(imageRy)
library(ggplot2)
library(viridis)
library(raster)
setwd("C:/Suz/Università/Data science/Monitoring Ecosysitems Changing and Functioning/Esame")

# https://www.nature.com/articles/s41598-024-76730-1 -> articolo su cui trarre spunto
# https://earthobservatory.nasa.gov/images/150306/lake-manchar-is-overflowing

#################################
######### Import data ###########
#################################

# Importing data: 18/09/2021
extent_crop <- ext(277785+50000, 508215-100000, 2757585+128000, 2992215-20000) 

# bande: blu, verde, rosso, infrarosso
b5_2021 <- rast("LC08_L2SP_152042_20210918_20210925_02_T1/LC08_L2SP_152042_20210918_20210925_02_T1_SR_B5.TIF") # infrarosso
ext(b5_2021)
b5_2021 <- crop(b5_2021, extent_crop)
b4_2021 <- rast("LC08_L2SP_152042_20210918_20210925_02_T1/LC08_L2SP_152042_20210918_20210925_02_T1_SR_B4.TIF") # Rosso
b4_2021 <- crop(b4_2021, extent_crop)
b3_2021 <- rast("LC08_L2SP_152042_20210918_20210925_02_T1/LC08_L2SP_152042_20210918_20210925_02_T1_SR_B3.TIF") # Verde
b3_2021 <- crop(b3_2021, extent_crop)
b2_2021 <- rast("LC08_L2SP_152042_20210918_20210925_02_T1/LC08_L2SP_152042_20210918_20210925_02_T1_SR_B2.TIF") # Blu
b2_2021 <- crop(b2_2021, extent_crop)

landsat_rgb_2021 <- c(b4_2021, b3_2021, b2_2021)

# Importing data: 13/09/2022
b5_2022 <- rast("LC09_L2SP_152042_20220913_20230329_02_T1/LC09_L2SP_152042_20220913_20230329_02_T1_SR_B5.TIF") # infrarosso
b5_2022 <- crop(b5_2022, extent_crop)
b4_2022 <- rast("LC09_L2SP_152042_20220913_20230329_02_T1/LC09_L2SP_152042_20220913_20230329_02_T1_SR_B4.TIF") # Rosso
b4_2022 <- crop(b4_2022, extent_crop)
b3_2022 <- rast("LC09_L2SP_152042_20220913_20230329_02_T1/LC09_L2SP_152042_20220913_20230329_02_T1_SR_B3.TIF") # Verde
b3_2022 <- crop(b3_2022, extent_crop)
b2_2022 <- rast("LC09_L2SP_152042_20220913_20230329_02_T1/LC09_L2SP_152042_20220913_20230329_02_T1_SR_B2.TIF") # Blu
b2_2022 <- crop(b2_2022, extent_crop)

landsat_rgb_2022 <- c(b4_2022, b3_2022, b2_2022)


# Importing data: 16/09/2023
b5_2023 <- rast("LC09_L2SP_152042_20230916_20230918_02_T1/LC09_L2SP_152042_20230916_20230918_02_T1_SR_B5.TIF") # infrarosso
b5_2023 <- crop(b5_2023, extent_crop)
b4_2023 <- rast("LC09_L2SP_152042_20230916_20230918_02_T1/LC09_L2SP_152042_20230916_20230918_02_T1_SR_B4.TIF") # Rosso
b4_2023 <- crop(b4_2023, extent_crop)
b3_2023 <- rast("LC09_L2SP_152042_20230916_20230918_02_T1/LC09_L2SP_152042_20230916_20230918_02_T1_SR_B3.TIF") # Verde
b3_2023 <- crop(b3_2023, extent_crop)
b2_2023 <- rast("LC09_L2SP_152042_20230916_20230918_02_T1/LC09_L2SP_152042_20230916_20230918_02_T1_SR_B2.TIF") # Blu
b2_2023 <- crop(b2_2023, extent_crop)

landsat_rgb_2023 <- c(b4_2023, b3_2023, b2_2023)

# Importing data: 18/09/2024
b5_2024 <- rast("LC09_L2SP_152042_20240918_20240923_02_T1/LC09_L2SP_152042_20240918_20240923_02_T1_SR_B5.TIF") # infrarosso
b5_2024 <- crop(b5_2024, extent_crop)
b4_2024 <- rast("LC09_L2SP_152042_20240918_20240923_02_T1/LC09_L2SP_152042_20240918_20240923_02_T1_SR_B4.TIF") # Rosso
b4_2024 <- crop(b4_2024, extent_crop)
b3_2024 <- rast("LC09_L2SP_152042_20240918_20240923_02_T1/LC09_L2SP_152042_20240918_20240923_02_T1_SR_B3.TIF") # Verde
b3_2024 <- crop(b3_2024, extent_crop)
b2_2024 <- rast("LC09_L2SP_152042_20240918_20240923_02_T1/LC09_L2SP_152042_20240918_20240923_02_T1_SR_B2.TIF") # Blu
b2_2024 <- crop(b2_2024, extent_crop)

landsat_rgb_2024 <- c(b4_2024, b3_2024, b2_2024)

#################################
######### Natural plot ##########
#################################

# RGB plot: natural color image
# stretch per migliorare i contrasti: normalizza i pixel nelle bande
par(mfrow=c(4,2))
plotRGB(landsat_rgb_2021, r=1, g=2, b=3, stretch="lin")
plotRGB(landsat_rgb_2021, r=1, g=2, b=3)

plotRGB(landsat_rgb_2022, r=1, g=2, b=3, stretch="lin") 
plotRGB(landsat_rgb_2022, r=1, g=2, b=3) 

plotRGB(landsat_rgb_2023, r=1, g=2, b=3, stretch="lin") 
plotRGB(landsat_rgb_2023, r=1, g=2, b=3) 

plotRGB(landsat_rgb_2024, r=1, g=2, b=3, stretch="lin") 
plotRGB(landsat_rgb_2024, r=1, g=2, b=3) 

#################################
####### False color plot ########
#################################
# red -> NIR = vegetazione
# green -> band green 
# blue -> band blue

false2021 <- c(b5_2021, b3_2021, b4_2021)
false2022 <- c(b5_2022, b3_2022, b4_2022)
false2023 <- c(b5_2023, b3_2023, b4_2023)
false2024 <- c(b5_2024, b3_2024, b4_2024)

par(mfrow=c(2,2))
plotRGB(false2021, r=1, g=2, b=3, stretch="lin")
plotRGB(false2022, r=1, g=2, b=3, stretch="lin") 
plotRGB(false2023, r=1, g=2, b=3, stretch="lin")
plotRGB(false2024, r=1, g=2, b=3, stretch="lin")

#################################
######### Infrared plot #########
#################################

# Analysing b5 plots - infrared
# We use b5 because it is thebest method to analyse water and to compute NDVI
water <- c(b5_2021, b5_2022, b5_2023, b5_2024)
cl_vir <- colorRampPalette(viridis(7))(255)
par(mfrow = c(2,2))
plot(water[[1]], col=cl_vir, main='2021')
plot(water[[2]], col=cl_vir, main= '2022')
plot(water[[3]], col=cl_vir, main= '2023')
plot(water[[4]], col=cl_vir, main= '2024')

# Correlation though years
pairs(water) # 2022 is less correlated with other years

#Plotting the difference between 2022 and 2023
par(mfrow = c(1,1))
diff= water[[2]] - water[[3]]
plot(diff, col=cl_vir)
title('Water difference (2022-2023)')

#Plotting an RGB with 2021 in the red channel, 2022 in the green channel, and 2023 in the blue channel
im.plotRGB(water, r=1, g=3, b=2)

#################################
####### Classification ##########
#################################

# Classification to detect water using b5 infrared and b2
par(mfrow=c(1,1))
set.seed(1234)
b5_class_2021 <- im.classify(b5_2021, 3)
# 1 - water, 2 - desert, 3 - vegetation

set.seed(1234)
b5_class_2022 <- im.classify(b5_2022, 3)
# 1 - water, 2 - desert, 3 - vegetation

set.seed(1234)
b5_class_2023 <- im.classify(b5_2023, 3)
# 1 - water, 2 - desert, 3 - vegetation

set.seed(1234)
b5_class_2024 <- im.classify(b5_2024, 3)
# 1 - water, 2 - desert, 3 - vegetation

f2021 <- freq(b5_class_2021)
tot2021 <- ncell(b5_class_2021)
p2021 = f2021 * 100 / tot2021
p2021
# water: 4,64%
# desert: 46,20%
# vegetation: 49,16%

f2022 <- freq(b5_class_2022)
tot2022 <- ncell(b5_class_2022)
p2022 = f2022 * 100 / tot2022
p2022
# water: 31,71%
# desert: 32,67%
# vegetation: 35,62%%

f2023 <- freq(b5_class_2023)
tot2023 <- ncell(b5_class_2023)
p2023 = f2023 * 100 / tot2023
p2023
# water: 6,93%
# desert: 43,51%
# vegetation: 49,56%

f2024 <- freq(b5_class_2024)
tot2024 <- ncell(b5_class_2024)
p2024 = f2024 * 100 / tot2024
p2024
# water: 9,35%
# desert: 49,27%
# vegetation: 41,40%

perc_water <- c(p2021[1,3],p2022[1,3],p2023[1,3],p2024[1,3])
year <- c("2021","2022","2023","2024")

results <- data.frame(year, perc_water) 
results

# Plot of the pixel percentage of water
ggplot(results, aes(x=year, y=perc_water)) +
  geom_bar(stat="identity",fill="skyblue") + 
  scale_x_discrete(limits = year) +
  xlab("Year") + ylab("Water percentage") + 
  ggtitle("Pixel percentage of water in september - Lake Manchar")

# NDVI
# DVI
par(mfrow=c(1,2))
dvi_2021 <- b5_2021 - b4_2021
#plot(dvi_2021, col=viridisc)
dvi_2022 <- b5_2022 - b4_2022
#plot(dvi_2022, col=viridisc)

# NDVI
# between -1 and 1 -> good vegetation
# 1: vegetali in buona salute
# 0: terreni aridi
# -1: non vegetali come acqua o neve
ndvi_2021 = dvi_2021 / (b5_2021 + b4_2021)
ndvi_2022 = dvi_2022 / (b5_2022 + b4_2022)
plot(ndvi_2021, col=cl_vir)
plot(ndvi_2022, col=cl_vir)
#plot(ndvi_2021)
#plot(ndvi_2022)


###########################
##### NASA image ##########
###########################
par(mfrow=c(1,1))
# Import image
sept_2022_NASA <- rast("manchar_oli_2022248_lrg.jpg")
june_2022_NASA <- rast("manchar_oli2_2022176_lrg.jpg")
plotRGB(june_2022_NASA, r = 1, g = 2, b = 3)
plotRGB(sept_2022_NASA, r = 1, g = 2, b = 3)
# Non ho la banda infrared

# Compute PCA and study its sd
# Perform PCA
pc <-  im.pca(sept_2022_NASA)
pc1 <- pc[[1]]
plot(pc1, col=cl_vir)
title('pc1 september')

# sd on pc1
pc1sd3 <- focal(pc1, matrix(1/9,3,3), fun=sd) # 3x3 moving window
plot(pc1sd3, col=cl_vir)

pc1sd7 <- focal(pc1, matrix(1/49, 7, 7), fun=sd) # 7x7 moving window -> smoother
plot(pc1sd7, col=cl_vir)

# sd on B3 a september
# Analizzo B3 perchè è la migliore per analizzare acqua un po' verde
B3 <- sept_2022_NASA[[2]]
sd3 <- focal(B3, matrix(1/9, 3, 3), fun=sd)
sd7 <- focal(B3, matrix(1/49, 7, 7), fun=sd)

# Standard deviation layers
sdstack <- c(sd3, sd7, pc1sd3, pc1sd7)
names(sdstack) <- c("sd3", "sd7", "pc1sd3", "pc1sd7")
plot(sdstack, col=cl_vir)
# in 7x7 moving window images, variance is higher and more visible
# the difference between principal component and NIR is not relevant
# indeed PC1 and NRI could be very correlated, due to the fact that
# the image is defined principally by vegetation and water

# Compute PCA for june
pc_june <-  im.pca(june_2022_NASA)
pc1_june <- pc_june[[1]]
plot(pc1_june, col=cl_vir)
title('pc1 june')

# PC difference
pca_diff <- pc[[1]] - pc_june[[1]]
plot(pca_diff, 
     main = "PCA difference june-september",
     col=cl_vir)

# PC difference variability
std_dev_pca_diff <- focal(pca_diff, w = matrix(1, 3, 3), fun = sd)
plot(std_dev_pca_diff, col=cl_vir)

# bands difference
diff_B2 <- sept_2022_NASA[[1]] - june_2022_NASA[[1]]
diff_B3 <- sept_2022_NASA[[2]] - june_2022_NASA[[2]]
diff_B4 <- sept_2022_NASA[[3]] - june_2022_NASA[[3]]
par(mfrow=c(1,3))
plot(diff_B2, 
     main = "Blue band B2 difference (water torbidity)",
     col=cl_vir)
plot(diff_B3, 
     main = "Green band B3 difference (vegetation)",
     col=cl_vir)
plot(diff_B4, 
     main = "Differenza Rosso difference(soil and sediments)",
     col=cl_vir)

# NDVI modified
ndvi_mod_june <- (june_2022_NASA[[2]]-june_2022_NASA[[3]])/(june_2022_NASA[[2]]+june_2022_NASA[[3]])
plot(ndvi_mod_june, col=cl_vir)
ndvi_mod_sept <- (sept_2022_NASA[[2]]-sept_2022_NASA[[3]])/(sept_2022_NASA[[2]]+sept_2022_NASA[[3]])
plot(ndvi_mod_sept,col=cl_vir)
diff_ndvi_mod <- ndvi_mod_sept - ndvi_mod_june
plot(diff_ndvi_mod, col=cl_vir)

# Temporal difference
par(mfrow=c(1,1))
diff <- june_2022_NASA[[1]] - sept_2022_NASA[[1]]
plot(diff, col=cl_vir) 
# the majour differences are in the part with extra water

names(june_2022_NASA) <- c("B2 - Blue band","B3 - Green band","B4 - Red band") 
density(june_2022_NASA)
names(sept_2022_NASA) <- c("B2 - Blue band","B3 - Green band","B4 - Red band") 
density(sept_2022_NASA)
