library(lubridate)
library(dplyr)
GPU = read.csv("path",header=TRUE,na.strings=c("","\n- ","\n","\nUnknown Release Date "))

# Lấy các cột cần thiết
GPU_new = GPU[, c("Memory", "Resolution_WxH", "Manufacturer", "Core_Speed", "Memory_Bus", "Memory_Speed", "Memory_Type", "Process", "Pixel_Rate", "Texture_Rate", "Release_Date")]

# Hàm trợ giúp để chuyển đổi chuỗi sang số
helper <- function(x) {
  if (is.na(x)) {
    return(NA)
  } else {
    return(as.double(strsplit(x, " ")[[1]][[1]]))
  }
}

# Chuyển đổi và xử lý các cột dữ liệu
GPU_new$Core_Speed <- sapply(GPU_new$Core_Speed, helper)
GPU_new$Core_Speed[is.na(GPU_new$Core_Speed)] = median(GPU_new$Core_Speed, na.rm=T)

# Xử lý cột Resolution_WxH và nhóm các giá trị
GPU_new$Resolution_WxH[is.na(GPU_new$Resolution_WxH)] = "4096x2160"
GPU_new$Resolution_WxH <- factor(ifelse(GPU_new$Resolution_WxH == "4096x2160", 1,
                                        ifelse(GPU_new$Resolution_WxH == "2560x1600", 2, 3)))

# Chuyển đổi và xử lý cột Memory
GPU_new$Memory <- sapply(GPU_new$Memory, helper)
GPU_new$Memory[is.na(GPU_new$Memory)] = median(GPU_new$Memory, na.rm=T)

# Chuyển đổi và xử lý cột Memory_Bus
GPU_new$Memory_Bus <- sapply(GPU_new$Memory_Bus, helper)
GPU_new <- GPU_new[complete.cases(GPU_new$Memory_Bus),]

# Các cột Memory_Speed và Memory_Type
GPU_new$Memory_Speed <- sapply(GPU_new$Memory_Speed, helper)
GPU_new$Memory_Type = factor(gsub("[^A-Za-z]+.*", "", GPU_new$Memory_Type))

#manufacturer 
GPU_new$Manufacturer = factor(GPU_new$Manufacturer)

# Xử lý cột Process
GPU_new$Process <- as.double(gsub("[^0-9]", "", GPU_new$Process))
GPU_new$Process[is.na(GPU_new$Process)] = median(GPU_new$Process, na.rm=T)

# Các cột Pixel_Rate và Texture_Rate
GPU_new$Pixel_Rate <- sapply(GPU_new$Pixel_Rate, helper)
GPU_new$Texture_Rate <- sapply(GPU_new$Texture_Rate, helper)
GPU_new$Pixel_Rate[is.na(GPU_new$Pixel_Rate)] = median(GPU_new$Pixel_Rate, na.rm=T)
GPU_new$Texture_Rate[is.na(GPU_new$Texture_Rate)] = median(GPU_new$Texture_Rate, na.rm=T)

#Xử lí release_date
GPU_new$Release_Date <- dmy(GPU_new$Release_Date)


# Loại bỏ các giá trị trùng lặp
GPU_new <- GPU_new %>% dplyr::distinct()

# Chuyển đổi sang dạng logarit và kiểm tra giá trị âm
GPU_new_log <- GPU_new
numerical <- c("Memory", "Core_Speed", "Memory_Bus", "Memory_Speed", "Process", "Pixel_Rate", "Texture_Rate")
GPU_new_log[, numerical] <- log(GPU_new_log[, numerical])

# Tính toán và hiển thị tóm tắt dữ liệu
summary(GPU_new)
summary(GPU_new_log)
apply(is.na(GPU_new), 2, sum)
par(mfrow = c(1, 1))
# Vẽ các biểu đồ histogram và boxplot
for (i in 1:length(numerical)) {
  hist(GPU_new[[numerical[i]]], xlab = names(GPU_new)[which(names(GPU_new) == numerical[i])],
       main = paste("Biểu đồ cột ", names(GPU_new)[which(names(GPU_new) == numerical[i])]),
       col = "blue")
}
# Cùng cách vẽ biểu đồ với dữ liệu logarit
for (i in 1:length(numerical)) {
  hist(GPU_new_log[[numerical[i]]], xlab = names(GPU_new_log)[which(names(GPU_new_log) == numerical[i])],
       main = paste("Biểu đồ cột của log ", names(GPU_new_log)[which(names(GPU_new_log) == numerical[i])]),
       col = "red")
}

# Vẽ các biểu đồ barplot và boxplot
par(mfrow = c(1, 3))
barplot(table(GPU_new$Manufacturer), xlab = "Manufacturer", ylab = "Count", main = "By Manufacturer", col = c("red", "green", "blue", "yellow"))
barplot(table(GPU_new$Memory_Type), xlab = "Memory_Type", ylab = "Count", main = "By Memory Type", col = c("red", "green", "blue", "yellow"))
barplot(table(GPU_new$Resolution_WxH), xlab = "Resolution WxH", ylab = "Frequency", main = "By Max Resolution", col = c("red", "green", "blue"))

# Các biểu đồ hộp
par(mfrow = c(1, 2))
boxplot(Memory~Manufacturer, data = GPU_new, main = "Memory by Manufacturer", col = "blue")
boxplot(Memory~Manufacturer, data = GPU_new_log, main = "Log Memory by Manufacturer", col = "red")

# Vẽ các biểu đồ scatter và regression lines
par(mfrow = c(1, 2))
plot(GPU_new[,"Release_Date" ], GPU_new[, "Core_Speed"], ylab = "Core Speed", xlab = "Release_Date", main = "Release_Date and Core Speed", col = "blue")
fit_new <- lm(Release_Date ~ Core_Speed, data = GPU_new)
abline(fit_new, col = "red")
##
plot(GPU_new_log[,"Release_Date" ], GPU_new_log[, "Core_Speed"], ylab = "Log Core Speed", xlab = "Release_Date", main = "Release_Date and Log Core Speed", col = "red")
fit_new_log <- lm(Release_Date ~ log(Core_Speed), data = GPU_new_log)
abline(fit_new_log, col = "blue")

# Tiếp tục với các cặp thuộc tính khác
plot(GPU_new[, "Release_Date" ], GPU_new[, "Memory_Bus"], ylab = "Memory Bus", xlab = "Release_Date", main = "Release_Date and Memory Bus", col = "blue")
fit_new <- lm(Release_Date ~ Memory_Bus, data = GPU_new)
abline(fit_new, col = "red")

plot(GPU_new_log[, "Release_Date"], GPU_new_log[, "Memory_Bus"], ylab = "Log Memory Bus", xlab = "Log Memory", main = "Release_Date and Log Memory Bus", col = "red")
fit_new_log <- lm(Release_Date ~ log(Memory_Bus), data = GPU_new_log)
abline(fit_new_log, col = "blue")

plot(GPU_new[, "Release_Date"], GPU_new[, "Memory_Speed"], ylab = "Memory Speed", xlab = "Release_Date", main = "Release_Date and Memory Speed", col = "blue")
fit_new <- lm(Release_Date ~ Memory_Speed, data = GPU_new)
abline(fit_new, col = "red")

plot(GPU_new_log[, "Release_Date"], GPU_new_log[, "Memory_Speed"], ylab = "Log Memory Speed", xlab = "Release_Date", main = "Release_Date and Log Memory Speed", col = "red")
fit_new_log <- lm(Release_Date ~ log(Memory_Speed), data = GPU_new_log)
abline(fit_new_log, col = "blue")

# Vẽ biểu đồ pairs
pairs(GPU_new_log, main = "Biểu đồ pairs")

# B-Thống kê suy diễn
# Bài toán 1 mẫu
# Kiểm tra có phải phân phối chuẩn hay không
shapiro.test(GPU_new$Memory)
t.test(GPU_new$Memory, alternative = "less", mu = 3072, conf.level = 0.95)


# Bài toán 2 mẫu
Intel<-GPU_new$Memory[GPU_new$Manufacturer == "Intel"]
shapiro.test(Intel)

AMD<- GPU_new$Memory[GPU_new$Manufacturer == "AMD"]
shapiro.test(AMD)

t.test(Intel, AMD, alternative = "greater", mu= 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)


# Phân tích phương sai anova 1 yếu tố
shapiro.test(GPU_new_log$Memory[GPU_new_log$Manufacturer == "Intel"])
shapiro.test(GPU_new_log$Memory[GPU_new_log$Manufacturer == "AMD"])
shapiro.test(GPU_new_log$Memory[GPU_new_log$Manufacturer == "Nvidia"])
shapiro.test(GPU_new_log$Memory[GPU_new_log$Manufacturer == "ATI"])

ANOVA_1 <- aov(Memory ~ Manufacturer, data= GPU_new_log)
summary(ANOVA_1)

TukeyHSD(ANOVA_1)


# Phân tích phương sai anova 2 yếu tố
shapiro.test(GPU_new_log$Memory[GPU_new_log$Resolution_WxH == 3])
shapiro.test(GPU_new_log$Memory[GPU_new_log$Resolution_WxH == 2])
shapiro.test(GPU_new_log$Memory[GPU_new_log$Resolution_WxH == 1])

ANOVA_2 <- aov(Memory ~ Manufacturer + Resolution_WxH, data= GPU_new_log)
summary(ANOVA_2)

TukeyHSD(ANOVA_2)


# Hồi quy tuyến tính đơn
lm<-lm(Texture_Rate ~ Memory_Speed, data = GPU_new)
summary(lm)

par(mfrow=c(2, 2))
plot(lm)

predict(lm, newdata = data.frame(Memory_Speed = 1750))
predict(lm, newdata = data.frame(Memory_Speed = 800))
