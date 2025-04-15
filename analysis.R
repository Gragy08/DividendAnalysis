install.packages(c("readxl", "psych", "corrplot", "ggplot2", "GGally", "dplyr")) # chỉ cần chạy 1 lần

library(readxl)
library(psych)
library(corrplot)
library(ggplot2)
library(GGally)
library(dplyr)

# Đọc dữ liệu từ file Excel
data <- read_excel("raw_data-1.xlsx")

# Chọn các biến cần thiết
vars <- data %>% select(eps2018, pe2018, quickratio2018, currentratio2018, lev2018, divpayratio2018, divyield2018)

# Thống kê mô tả
desc_stats <- describe(vars)
print(desc_stats)

# Hệ số tương quan
cor_matrix <- cor(vars, use = "complete.obs")
corrplot(cor_matrix, method = "circle")

# Biểu đồ: EPS và dividend payout
ggplot(data, aes(x = eps, y = divpayratio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# Ma trận scatterplot
ggpairs(vars)
