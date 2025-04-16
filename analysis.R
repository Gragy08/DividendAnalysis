# Cài các package cần thiết (chỉ cần chạy 1 lần)
install.packages(c("readxl", "psych", "corrplot", "ggplot2", "GGally", "dplyr", "tidyr", "broom"))

# Load các thư viện
library(readxl)
library(psych)
library(corrplot)
library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)
library(broom)

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
  drop_na()

# Kiểm tra dữ liệu đã xử lý
glimpse(data_long)

# ===============================
# 1. Thống kê mô tả
# ===============================
desc_stats <- describe(data_long %>%
                         select(yearpaydiv, eps, pe, quickratio, currentratio, lev, divpayratio, divyield))
print(desc_stats)

# ===============================
# 2. Hệ số tương quan + vẽ biểu đồ
# ===============================
cor_matrix <- cor(data_long %>%
                    select(eps, pe, quickratio, currentratio, lev, divpayratio, divyield),
                  use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "circle")

# ===============================
# 3. Biểu đồ: EPS và Dividend Payout
# ===============================
ggplot(data_long, aes(x = eps, y = divpayratio)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# ===============================
# 4. Ma trận scatterplot
# ===============================
ggpairs(data_long %>% select(eps, pe, quickratio, currentratio, lev, divpayratio, divyield))

# ===============================
# 5. Mô hình Logit: theo hình ảnh bạn gửi
# ===============================
# Giả định bạn đã có biến nhị phân TANG_GIA trong data
# Nếu chưa có thì bạn phải tạo biến này trước (ví dụ: dựa trên tỷ lệ tăng giá cổ phiếu)

# Đổi tên biến để phù hợp với phương trình logit (nếu cần)
data_logit <- data_long %>%
  rename(
    ROA = roa,
    ROE = roe,
    PE = pe,
    DE_RATIO = lev,
    DIVYIELD = divyield,
    QUICK_RATIO = quickratio
  )

# Fit mô hình logit
model_logit <- glm(TANG_GIA ~ ROA + ROE + PE + DE_RATIO + DIVYIELD + QUICK_RATIO,
                   data = data_logit,
                   family = binomial)

# Tóm tắt kết quả mô hình
summary(model_logit)

# Biến kết quả ra bảng đẹp (tùy chọn)
tidy(model_logit)
