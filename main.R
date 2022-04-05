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

# train model
train_data = Framingham
nsamp = 500 # 20000
nwarm = 100 # 500

size <- length(train_data)

print(size)

var_ord <- 1:(size - 1)
var_group <- size

print(var_ord)
print(var_group)

# xây dựng mô hình
GCMlasso_obj <- GCMlasso(data=train_data, var_ord=var_ord, var_group=var_group, nsamp=nsamp,
                         odens=1, nwarm=nwarm, seed=1, s=1e-2, t=1e-4, verb=TRUE)

# xuất ra các lớp duy nhất của biến muốn dự đoán
print(unique(train_data[size]))

# xuất ra kết quả của GCMlasso_obj
print(GCMlasso_obj)

# so sánh 2 nhóm (cụm) với nhau
group1 = 1:2
group2 = 3:4

compare_group(GCMlasso_obj, grp1=group1, grp2=group2, var=var_ord, credible_level=0.95)

# vẽ đồ thị
plot_graph(GCMlasso_obj, var=var_ord, edge_perc=0.65)

# tính hệ số hồi quy cho biến có thứ tự là size - 1 (trường hợp này là 31 - 1 = 30)
reg_coef(GCMlasso_obj, var_pred=1:14, var_response=15)

# dự đoán xác suất
predict_val <- predict(GCMlasso_obj, var_response=15, var_group=var_group)
predict_val
