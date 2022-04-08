library(plyr)
library(dplyr)
library(GCMlasso)

data_path = "C:\\Users\\nvatu\\Desktop\\R data processing\\Final08042022.csv"

data <- read.csv(data_path, encoding="UTF-8", na.strings = "#NULL!")

target = 'Year'

data <- data[, -which(names(data) %in% c("Land_ownership_type1",
                                         "Land_ownership_type2",
                                         "Land_ownership_type3",
                                         "Land_ownership_type4"))]


# thống kê sơ bộ
# cột cuối (cột target) bị nan tới 92%

data_length = dim(data)[1]

# loại bỏ những cột bị nan trên 93%
na_remove_percent <- 0.50
na_count <- sapply(data, function(y) sum(length(which(is.na(y))))) / data_length
na_count <- data.frame(na_count)
na_count

# lọc
subset <- na_count %>% filter(.data[['na_count']] < na_remove_percent)

selected_cols <- row.names(subset)

print(cat("Những cột dữ liệu này sẽ được giữ lại (không bị nan quá ", na_remove_percent, "%):"))
print(selected_cols)

filtered_data <- data %>% select(all_of(selected_cols))

filtered_data <- filtered_data %>% filter(!is.na(.data[[target]]))

# tạo thông tin để train model
train_data = filtered_data
nsamp = 1000 # 20000
nwarm = 500 # 500

size <- length(train_data)

print(size)

var_ord <- 1:(size - 1)
var_group <- size

print(var_ord)
print(var_group)

# train model
GCMlasso_object <- GCMlasso(data=train_data, var_ord=var_ord, var_group=var_group, nsamp=nsamp,
                         odens=1, nwarm=nwarm, seed=1, s=1e-2, t=1e-4, verb=TRUE)

n_unique <- length(unique(train_data[size])[[target]])

n_unique
print(cat("Có tổng cộng", n_unique, "lớp"))

# xuất ra các lớp duy nhất của biến muốn dự đoán

# giá trị gốc
list_value_unique <- unique(train_data[size])[[target]]
# giá trị thay thế (từ 1 đến (số lượng giá trị trong nhóm))
list_value_map <- 1:n_unique



train_data[[target]] <- mapvalues(train_data[[target]],
                                  from=list_value_unique,
                                  to=list_value_map)

print(unique(train_data[size]))

# xuất ra kết quả của GCMlasso_obj
# print(GCMlasso_object)

var_ord

GCMlasso_object$b.st

# so sánh 2 nhóm (cụm) với nhau
# group1 = c(1, 2)
# group2 = c(3, 4)
gr1 = 1:2
gr2 = 3:4

print(gr1)
print(gr2)

compare_group(GCMlasso_object, grp1=gr1, grp2=gr2, var=var_ord, credible_level=0.95)

# vẽ đồ thị
plot_graph(GCMlasso_object, var=var_ord, edge_perc=0.95)

# tính hệ số hồi quy cho biến có thứ tự là size - 1 (trường hợp này là 31 - 1 = 30)
reg_coef(GCMlasso_object, var_pred=1:(size - 2), var_response=size - 2)

# dự đoán xác suất
predict_val <- predict(GCMlasso_object, var_response=size - 2, var_group=var_group)
predict_val

m <- mean(predict_val)

rinv <- rinvgauss(n=1000, mean=m)

hist(rinv)
