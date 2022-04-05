library(dplyr)
library(GCMlasso)

data_path = "C:/Users/nvatu/Desktop/R data processing/Surveytest01042022.csv"

data <- read.csv(data_path, encoding="UTF-8", na.strings = 0)

# thống kê sơ bộ
# cột cuối (cột target) bị nan tới 92%

data_length = dim(data)[1]

# loại bỏ những cột bị nan trên 95%
na_remove_percent <- 0.95
na_count <- sapply(data, function(y) sum(length(which(is.na(y))))) / data_length
na_count <- data.frame(na_count)
# lọc
subset <- na_count %>% filter(.data[['na_count']] < na_remove_percent)

selected_cols <- row.names(subset)

print("Những cột dữ liệu này sẽ được giữ lại (không bị nan quá 50%):")
print(selected_cols)

filtered_data <- data %>% select(selected_cols)

size <- length(selected_cols)

# loại bỏ cột đầu tiên, do nó là cột thứ tự
var_ord <- 2:(size - 1)
var_group <- size

#
GCMlasso_obj <- GCMlasso(data=filtered_data,
                         var_ord=var_ord,
                         var_group=var_group,
                         nsamp=500,
                         odens=1,
                         nwarm=100,
                         seed=1,
                         s=1e-2,
                         t=1e-4,
                         verb=TRUE)

