# Cài các package cần thiết (chỉ cần chạy 1 lần)
install.packages(c("readxl", "psych", "corrplot", "ggplot2", "GGally", "dplyr", "tidyr"))

# Load các thư viện
library(readxl)
library(psych)
library(corrplot)
library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)

# Đọc dữ liệu từ file Excel
data <- read_excel("raw_data-1.xlsx")

# Chuyển đổi dữ liệu từ dạng wide sang long
data_long <- data %>%
  pivot_longer(
    cols = matches("(yearpaydiv|eps|pe|quickratio|currentratio|lev|divpayratio|divyield)\\d+"),
    names_to = c(".value", "year"),
    names_pattern = "([a-zA-Z]+)(\\d+)"
  ) %>%
  mutate(year = as.integer(year)) %>%
  drop_na()  # Hoặc có thể thay bằng: drop_na(eps, divpayratio) nếu muốn giữ lại các NA khác

# Kiểm tra dữ liệu đã xử lý
glimpse(data_long)

# Thống kê mô tả
desc_stats <- describe(data_long %>%
                         select(yearpaydiv, eps, pe, quickratio, currentratio, lev, divpayratio, divyield))
print(desc_stats)

# Hệ số tương quan
cor_matrix <- cor(data_long %>%
                    select(eps, pe, quickratio, currentratio, lev, divpayratio, divyield),
                  use = "complete.obs")
corrplot(cor_matrix, method = "circle")

# Biểu đồ: EPS và Dividend Payout Ratio
ggplot(data_long, aes(x = eps, y = divpayratio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# Ma trận scatterplot
ggpairs(data_long %>% select(eps, pe, quickratio, currentratio, lev, divpayratio, divyield))
