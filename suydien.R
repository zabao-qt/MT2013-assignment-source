install.packages (" tidyverse ")
install.packages (" nycflights13 ")
install.packages (" lubridate ")
install.packages (" readr ")
install.packages (" ggpubr ")
install.packages (" sqldf ")
install.packages (" dplyr ")
install.packages (" car")
install.packages (" ggplot2 ")
library ( ggplot2 )
library ( ggpubr )
library ( tidyverse )
library ( lubridate )
library ( readr )
library ( sqldf )
library ( dplyr )
GPU = read.csv("C:/Users/Admin/Documents/HCMUT/Probability/BTL_2024/All_GPUs.csv", header=TRUE, na.strings=c("", "\n-", "\n", "\nUnknown Release Date"))
GPU_new = GPU[, c("Memory", "Resolution_WxH", "Manufacturer", "Core_Speed", "Memory_Bus", "Memory_Speed", "Memory_Type", "Process", "Pixel_Rate", "Texture_Rate", "Release_Date")]
helper <- function(x) {
  if(is.na(x)) {
    return(NA)
  } else {
    return(as.double(strsplit(x, " ")[[1]][[1]]))
  }
}

GPU_new$Resolution_WxH[is.na(GPU_new$Resolution_WxH)] = "4096x2160"
GPU_new$Resolution_WxH <- factor(ifelse(GPU_new$Resolution_WxH == "4096x2160", 1, ifelse(GPU_new$Resolution_WxH == "2560x1600", 2, 3)))

GPU_new$Memory <- sapply(GPU_new$Memory, helper)
GPU_new$Memory[is.na(GPU_new$Memory)] = median(GPU_new$Memory, na.rm = T)

GPU_new$Memory_Bus <- sapply(GPU_new$Memory_Bus, helper)
GPU_new <- GPU_new[complete.cases(GPU_new$Memory_Bus), ]

GPU_new$Memory_Speed <- sapply(GPU_new$Memory_Speed, helper)
GPU_new$Memory_Speed[is.na(GPU_new$Memory_Speed)] <- median(GPU_new$Memory_Speed, na.rm = T)

mode_memory_type <- names(which.max(table(GPU_new$Memory_Type)))
GPU_new$Memory_Type[is.na(GPU_new$Memory_Type)] <- mode_memory_type
GPU_new$Memory_Type <- factor(gsub("[^A-Za-z]+.*", " ", GPU_new$Memory_Type))

GPU_new$Manufacturer = factor(GPU_new$Manufacturer)

GPU_new$Process <- as.double(gsub("[^0-9]", "", GPU_new$Process))
GPU_new$Process[is.na(GPU_new$Process)] = median(GPU_new$Process, na.rm = T)

GPU_new$Pixel_Rate <- sapply(GPU_new$Pixel_Rate, helper)
GPU_new$Texture_Rate <- sapply(GPU_new$Texture_Rate, helper)
GPU_new$Pixel_Rate[is.na(GPU_new$Pixel_Rate)] = median(GPU_new$Pixel_Rate, na.rm = T)
GPU_new$Texture_Rate[is.na(GPU_new$Texture_Rate)] = median(GPU_new$Texture_Rate, na.rm = T)

GPU_new$Release_Date <- ymd(GPU_new$Release_Date)
GPU_new$Release_Date[is.na(GPU_new$Release_Date)] <- as.Date("1970-01-01")

GPU_new$Core_Speed <- sapply(GPU_new$Core_Speed, helper)
GPU_new$Core_Speed[is.na(GPU_new$Core_Speed)] <- median(GPU_new$Core_Speed, na.rm = T)


GPU_new <- GPU_new %>% dplyr::distinct()

GPU_new_log <- GPU_new
numerical <- c("Memory", "Core_Speed", "Memory_Bus", "Memory_Speed", "Process", "Pixel_Rate", "Texture_Rate")
GPU_new_log[, numerical] <- log(GPU_new_log[, numerical])

summary(GPU_new)
summary(GPU_new_log)
apply(is.na(GPU_new), 2, sum)


install.packages("nortest")
library(nortest)

for(manufacturer in unique(GPU_new$Manufacturer)) {
  
  manufacturer_data_pixel_rate <- GPU_new$Pixel_Rate[GPU_new$Manufacturer == manufacturer]
  manufacturer_data_memory_speed <- GPU_new$Memory_Speed[GPU_new$Manufacturer == manufacturer]
  
  qqnorm(manufacturer_data_pixel_rate, main = paste("QQ Plot - Pixel_Rate for", manufacturer))
  qqline(manufacturer_data_pixel_rate, col = "red")
  
  # Kiểm tra phân phối chuẩn bằng Anderson-Darling cho Pixel_Rate
  ad_test_pixel_rate <- ad.test(manufacturer_data_pixel_rate)
  print(paste("Anderson-Darling Test for Pixel_Rate -", manufacturer))
  print(ad_test_pixel_rate)
  
  qqnorm(manufacturer_data_memory_speed, main = paste("QQ Plot - Memory_Speed for", manufacturer))
  qqline(manufacturer_data_memory_speed, col = "red")
  
  # Kiểm tra phân phối chuẩn bằng Anderson-Darling cho Memory Speed
  ad_test_memory_speed <- ad.test(manufacturer_data_memory_speed)
  print(paste("Anderson-Darling Test for Memory_Speed -", manufacturer))
  print(ad_test_memory_speed)
}

# Kiểm tra phân phối chuẩn cho Pixel_Rate & Mem Speed
ad.test(GPU_new$Pixel_Rate)
qqnorm(GPU_new$Pixel_Rate, main = "QQ Plot - Pixel Rate")
qqline(GPU_new$Pixel_Rate, col = "red")
ad.test(GPU_new$Memory_Speed)
qqnorm(GPU_new$Memory, main = "QQ Plot - Memory_Speed")
qqline(GPU_new$Memory, col = "red")

# package 'car' để sử dụng Levene's test
library(car)

# Kiểm tra đồng nhất phương sai
leveneTest(Pixel_Rate ~ Manufacturer, data=GPU_new)
leveneTest(Memory_Speed ~ Manufacturer, data=GPU_new)

# Thực hiện One-Way ANOVA giữa Pixel_Rate và Manufacturer
anova_pixel <- aov(Pixel_Rate ~ Manufacturer, data = GPU_new)
summary(anova_pixel)
TukeyHSD(anova_pixel)
# Thực hiện One-Way ANOVA giữa Memory_Speed và Manufacturer
anova_memory_speed <- aov(Memory_Speed ~ Manufacturer, data = GPU_new)
summary(anova_memory_speed)
TukeyHSD(anova_memory_speed)

# Mô hình 1
M1 <- lm(Pixel_Rate ~ Memory_Speed + Memory_Bus + Resolution_WxH + Manufacturer + Memory_Type + Process + Texture_Rate, 
         data = GPU_new)
summary(M1)
# Mô hình 2: Loại bỏ các biến (p-value > 0.05)
M2 <- lm(Pixel_Rate ~ Process + Resolution_WxH + Texture_Rate, 
         data = GPU_new)
summary(M2)
# So sánh hai mô hình M1 và M2
anova(M1, M2)

par(mfrow = c(2, 2)) 

# Plot 1: Residuals vs Fitted
#plot(M1, which = 1)
# Plot 2: Normal Q-Q
#plot(M1, which = 2)
# Plot 3: Scale-Location
#plot(M1, which = 3)
# Plot 4: Residuals vs Leverage
#plot(M1, which = 5)
plot(M1)
