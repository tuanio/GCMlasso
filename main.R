library(dplyr)
library(GCMlasso)

data_path = "C:/Users/nvatu/Desktop/R data processing/Surveytest01042022.csv"

data <- read.csv(data_path, encoding="UTF-8", na.strings = 0)

target = 'Suggestion2016'

# thống kê sơ bộ
# cột cuối (cột target) bị nan tới 92%

data_length = dim(data)[1]

# loại bỏ những cột bị nan trên 93%
na_remove_percent <- 0.95
na_count <- sapply(data, function(y) sum(length(which(is.na(y))))) / data_length
na_count <- data.frame(na_count)
# lọc
subset <- na_count %>% filter(.data[['na_count']] < na_remove_percent)

selected_cols <- row.names(subset)

print(cat("Những cột dữ liệu này sẽ được giữ lại (không bị nan quá ", na_remove_percent, "%):"))
print(selected_cols)

filtered_data <- data %>% select(all_of(selected_cols))

filtered_data <- filtered_data %>% filter(!is.na(.data[[target]]))

size <- length(filtered_data)

size

# loại bỏ cột đầu tiên, do nó là cột thứ tự

var_ord <- 2:(size - 1)
var_group <- size

print(var_ord)
print(var_group)

# xây dựng mô hình
GCMlasso_obj <- GCMlasso(data=filtered_data, var_ord=var_ord, var_group=var_group, nsamp=20000,
                         odens=1, nwarm=500, seed=1, s=1e-2, t=1e-4, verb=TRUE)

print(unique(filtered_data[target]))

# ví dụ
group1 = 2
group2 = 3

# so sánh 2 nhóm (cụm) với nhau
compare_group(GCMlasso_obj, grp1=group1, grp2=group2, var=var_ord, credible_level=0.95)

# vẽ đồ thị
plot_graph(GCMlasso_obj, var=var_ord, edge_perc=0.65)

# tính hệ số hồi quy cho biến có thứ tự là size - 1 (trường hợp này là 31 - 1 = 30)
reg_coef(GCMlasso_obj, var_pred=2:size - 2, var_response=size - 1)

# dự đoán xác suất
predict_val <- predict(GCMlasso_obj, var_response=size - 1, var_group=var_group)
predict_val
