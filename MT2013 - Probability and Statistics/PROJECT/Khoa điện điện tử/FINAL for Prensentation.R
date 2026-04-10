dirty_data <- read.csv("C:/Users/Anh Kiet/OneDrive - hcmut.edu.vn/Desktop/dirty_data.csv")

# Filter necessary datadata

new_data <- dirty_data[,c(4,6,7,10,11,12,13,14,16)]
head(new_data)


# Check data
apply(is.na(new_data),2,which)
unique(new_data$nearest_warehouse)

# Adjust datadata
new_data$nearest_warehouse[new_data$nearest_warehouse == 'thompson'] <- 'Thompson'
new_data$nearest_warehouse[new_data$nearest_warehouse == 'nickolson'] <- 'Nickolson'
unique(new_data$nearest_warehouse)
unique(new_data$season)
new_data$season[new_data$season == 'spring'] <- 'Spring'
new_data$season[new_data$season == 'summer'] <- 'Summer'
new_data$season[new_data$season == 'autumn'] <- 'Autumn'
new_data$season[new_data$season == 'winter'] <- 'Winter'
unique(new_data$season)

# Digitize data
new_data$is_expedited_delivery[new_data$is_expedited_delivery == 'True'] <- 1
new_data$is_expedited_delivery[new_data$is_expedited_delivery == 'False'] <- 0
new_data$is_happy_customer[new_data$is_happy_customer == 'True'] <- 1
new_data$is_happy_customer[new_data$is_happy_customer == 'False'] <- 0 

# Transform variable value
new_data$is_expedited_delivery<-as.numeric(new_data$is_expedited_delivery)
new_data$is_happy_customer<-as.numeric(new_data$is_happy_customer)

missing_data <- read.csv("C:/Users/Anh Kiet/OneDrive - hcmut.edu.vn/Desktop/archive (1)/missing_data.csv")
# Tính số lượng N/A của từng biến.
apply(is.na(missing_data),2,sum)
# Xuất ra vị trí các dòng có N/A của từng biến.
apply(is.na(missing_data),2,which)
# Tính tỉ lệ % N/A từng biến.
apply(is.na(missing_data),2, mean)
# Tóm tắt thống kê về dữ liệu các biến trong missing_data.
summary(missing_data)
# Tóm tắt thống kê về biến customer_long
summary(missing_data$customer_long)
A <- missing_data[c(9)]
# Truy cập thư viện dplyr
library(dplyr)
# Sử dụng hàm case_when, nếu dữ liệu là là N/A thì thay thế bằng mean.
A %>% mutate(customer_long = case_when(is.na(customer_long) ~ ceiling(mean(customer_long, na.rm = TRUE)), TRUE ~ customer_long)) ->A
# Chuyển dữ liệu từ A vào cột 9 (customer_long) của missing_data.
missing_data[c(9)] <- A
# Kiểm tra các dòng 97, 144, 156, 166, 232, 253, 364, 371, 377, 453 của biến customer_long đã được thay thế các missing values chưa.
missing_data[c(97, 144, 156, 166, 232, 253, 364, 371, 377, 453), c(9)]
# Tóm tắt thống kê về biến customer_lat
summary(missing_data$customer_lat)
B <- missing_data[c(8)]
# Truy cập thư viện dplyr
library(dplyr)
# Sử dụng hàm case_when, nếu dữ liệu là là N/A thì thay thế bằng mean.
B %>% mutate(customer_lat = case_when(is.na(customer_lat) ~ ceiling(mean(customer_lat, na.rm = TRUE)), TRUE ~ customer_lat)) ->B
# Chuyển dữ liệu từ B vào cột 8 (customer_lat) của missing_data.
missing_data[c(8)] <- B
# Kiểm tra các dòng 26,  76, 134, 190, 193, 204, 225, 325, 399, 461 của biến customer_lat đã được thay thế các missing values chưa.
missing_data[c(26,  76, 134, 190, 193, 204, 225, 325, 399, 461), c(8)]
# Tóm tắt thống kê về biến order_total.
summary(missing_data$order_total)
C <- missing_data[c(11)]
C %>% mutate(order_total = case_when(is.na(order_total) ~ median(order_total, na.rm = TRUE), TRUE ~ order_total)) -> C
# Chuyển dữ liệu từ C vào cột 11 (order_total) của missing_data.
missing_data[c(11)] <- C
# Kiểm tra các dòng 27,  74, 132, 203, 211, 249, 260, 346, 431, 481 của biến order_total đã được thay thế các missing values chưa
missing_data[c(27,  74, 132, 203, 211, 249, 260, 346, 431, 481), c(11)]
# Tóm tắt thống kê về biến order_price.
summary(missing_data$order_price)
D <- missing_data[c(6)]
D %>% mutate(order_price = case_when(is.na(order_price) ~ median(order_price, na.rm = TRUE), TRUE ~ order_price)) -> D
missing_data[c(6)] <- D
# Kiểm tra các dòng 20,  41,  85, 146, 171, 258, 274, 344, 426, 432 của biến order_price đã được thay thế các missing values chưa.
missing_data[c(20,  41,  85, 146, 171, 258, 274, 344, 426, 432), c(6)]
# Tạo ma trận E từ cột 3( biến date) và cột 12( biến season) gồm 10 dòng chứa missing values từ missing_data.
E <- missing_data[c(3,77,111,133,136,174,210,311,336,443),c(3,12)]
E
# Lưu dữ liệu các mốc thời gian.
time1 <- as.Date("2019-03-01")
time2 <- as.Date("2019-05-31")
time3 <- as.Date("2019-06-01")
time4 <- as.Date("2019-08-31")
time5 <- as.Date("2019-09-01")
time6 <- as.Date("2019-11-30")
time7 <- as.Date("2019-01-01")
time8 <- as.Date("2019-02-28")
time9 <- as.Date("2019-12-01")
time10 <- as.Date("2019-12-31")

# Sử dụng vòng lặp và lệnh if để gán các mùa phù hợp cho dữ liệu dựa trên mốc thời gian.
for (i in 1:nrow(E)) {
  if (time1 <= E$date[i] && E$date[i] <= time2) {
    E$season[i] <- "Autumn"
  }
}
for (i in 1:nrow(E)) {
  if (time3 <= E$date[i] && E$date[i] <= time4) {
    E$season[i] <- "Winter"
  }
}
for (i in 1:nrow(E)) {
  if (time5 <= E$date[i] && E$date[i] <= time6) {
    E$season[i] <- "Spring"
  }
}
for (i in 1:nrow(E)) {
  if (time7 <= E$date[i] && E$date[i] <= time8 || time9 <= E$date[i] && E$date[i] <= time10) {
    E$season[i] <- "Summer"
  }
}
E
# Chuyển dữ liệu từ ma trận E vào cột 12( biến season) trong missing_data.
missing_data[c(3,77,111,133,136,174,210,311,336,443),c(12)] <- E[c(2)]
# Truy cập vào vị trí các missing values trong cột 12( biến season) để kiểm tra lại các missing values.
missing_data[c(3,77,111,133,136,174,210,311,336,443),c(12)]
# Tạo ma trận F từ cột 4 ( biến nearest_warehouse), cột 8 ( biến customer_long) và cột 9 ( biến customer_lat) gồm 10 missing values từ missing_data.
F <- missing_data[c(8, 17, 106, 220, 237, 248, 252, 293, 296, 417),c(4,8,9)]
F
# Tạo ma trận F từ cột 4 ( biến nearest_warehouse), cột 8 ( biến customer_long) và cột 9 ( biến customer_lat) gồm 10 missing values từ missing_data.
F <- missing_data[c(8, 17, 106, 220, 237, 248, 252, 293, 296, 417),c(4,8,9)]
F

# Sử dụng vòng lặp và lệnh if để gán các nearest_warehouse phù hợp cho dữ liệu dựa trên các mốc tọa độ.

for (i in 1:nrow(F)) {
  if ((144.9212 <= F$customer_long[i] && F$customer_long[i] < 144.9554) 
      ||( -37.79872 <= F$customer_lat[i] && F$customer_lat[i] < -37.79504 )) {
    F$nearest_warehouse <- "Thompson"
  }
}

for (i in 1:nrow(F)) {
  if ((144.9554 <= F$customer_long[i] && F$customer_long[i] < 144.9737 
       || 144.9636 <= F$customer_long[i] && F$customer_long[i]< 144.9850)
      || (-37.82822 <= F$customer_lat[i] && F$customer_lat[i] < -37.82634)) {
    F$nearest_warehouse[i] <- "Nickolson"
  }
}


for (i in 1:nrow(F)) {
  if ((144.9850 <= F$customer_long[i] && F$customer_long[i]< 145.0170)
      || (-37.82634 <= F$customer_lat[i] && F$customer_lat[i] < -37.82574
          || -37.79504<= F$customer_lat[i] && F$customer_lat[i] < -37.79361)) {
    F$nearest_warehouse[i] <- "Bakers"
  }
}
F
# Chuyển dữ liệu từ ma trận F vào cột 4 ( biến nearest_warehouse)
missing_data[c(8, 17, 106, 220, 237, 248, 252, 293, 296, 417),c(4)] <- F[c(1)]
# Truy cập vào cột 4 ( biến nearest_warehouse) để kiểm tra lại các missing values.
missing_data[c(8, 17, 106, 220, 237, 248, 252, 293, 296, 417),c(4)]

library(readr)
warehouses <- read.csv("C:/Users/Anh Kiet/OneDrive - hcmut.edu.vn/Desktop/archive (1)/warehouses.csv")
#Chuyển dữ liệu từ cột 4( biến nearest_warehouse), cột 8( biến customer_long), cột 9( biến customer_lat) và cột 14( biến distance_to_nearest_warehouse) gồm 10 dòng chứa missing values vào ma trận G.
G <- missing_data[c(40, 185, 192, 264, 271, 282, 327, 348, 386, 393),c(4,8,9,14)]
G
# Chuyển toạ độ về đơn vị radian để thực hiện tính toán theo công thức Haversine
for (i in 1: nrow(G)){
  G$customer_lat[i] <- G$customer_lat[i] * (pi/180)
  G$customer_long[i] <- G$customer_long[i] * (pi/180)
}
for ( i in 1: nrow(warehouses)){
  warehouses$lat[i] <- warehouses$lat[i] * (pi/180)
  warehouses$lon[i] <- warehouses$lon[i] * (pi/180)
}
G
for (i in 1:nrow(G)){
  for (j in 1: nrow(warehouses)){
    if (G$nearest_warehouse[i] == warehouses$names[j]){
      a <- sin(abs(G$customer_lat[i] - warehouses$lat[j])/2)^2 +   cos(G$customer_lat[i])*cos(warehouses$lat[j])*sin(abs(G$customer_long[i] - warehouses$lon[j])/2)^2
      c <- 2 * atan2(sqrt(a), sqrt(1 - a)) 
      G$distance_to_nearest_warehouse[i] <- (round(6371 * c, digits = 4))
    }
  }
}
G
#Chuyển dữ liệu từ ma trận G vào cột 14( biến distance_to_nearest_warehouse) của missing_data.
missing_data[c(40, 185, 192, 264, 271, 282, 327, 348, 386, 393),c(14)] <- G[c(4)]
missing_data[c(40, 185, 192, 264, 271, 282, 327, 348, 386, 393),c(14)]
H <- missing_data[c(21, 23, 93, 130, 142, 180, 251, 304, 306, 489),c(15,16)]
H
for (i in 1:nrow(H)) {
  if  (i == 2 || i == 4){
    H$is_happy_customer[i] <- FALSE
  }
  else {
    H$is_happy_customer[i] <- TRUE
  }
}

H[c(2)]
missing_data[c(21, 23, 93, 130, 142, 180, 251, 304, 306, 489),c(16)] <- H[c(2)]
missing_data[c(21, 23, 93, 130, 142, 180, 251, 304, 306, 489),c(16)]

# Digitize data
missing_data$is_expedited_delivery[new_data$is_expedited_delivery == 'True'] <- 1
missing_data$is_expedited_delivery[new_data$is_expedited_delivery == 'False'] <- 0
missing_data$is_happy_customer[new_data$is_happy_customer == 'True'] <- 1
missing_data$is_happy_customer[new_data$is_happy_customer == 'False'] <- 0 

# Transform variable value
missing_data$is_expedited_delivery<-as.numeric(new_data$is_expedited_delivery)
missing_data$is_happy_customer<-as.numeric(new_data$is_happy_customer)


new_missing_data <- missing_data[,c("nearest_warehouse","is_expedited_delivery","order_price","delivery_charges", "coupon_discount","order_total","season","distance_to_nearest_warehouse","is_happy_customer")]
head(new_missing_data)

library(dplyr)
final_data <- bind_rows(new_data,new_missing_data)

final_data <- bind_cols(final_data[,c("is_expedited_delivery","nearest_warehouse","season","is_happy_customer")],final_data[,c("order_price","delivery_charges", "coupon_discount","order_total","distance_to_nearest_warehouse","is_happy_customer")]<-log(final_data[,c("order_price","delivery_charges", "coupon_discount","order_total","distance_to_nearest_warehouse")]+1))


#Thống kê các biến liên tục: chi phí đặt hàng, chi phí vận chuyển, giảm giá, chi phí tổng cộng, khoảng cách đến kho gần nhất
des_function <- function(x) {c(mean(x),median(x),sd(x),var(x),min(x),max(x))}
des_table <-apply(final_data[,c("order_price","delivery_charges","coupon_discount","order_total","distance_to_nearest_warehouse")],2,des_function)
rownames(des_table)=c("Kỳ vọng","Trung vị","Độ lệch chuẩn","Phương sai","Giá trị thấp nhất","Giá trị cao nhất")
des_table
#Thống kê bảng lượng khách hàng của 3 kho
table(final_data$nearest_warehouse)
#Thống kê bảng lượng khách hàng theo mùa
table(final_data$season)
#Thống kê bảng dịch vụ giao hàng nhanh
table(final_data$is_expedited_delivery)
#Thống kê bảng sự hài lòng của khách hàng
table(final_data$is_happy_customer) 
#Biểu đồ cột biểu thị lượng khách gần kho nhất của 3 kho
barplot(table(final_data$nearest_warehouse),main="Biểu đồ cột biểu thị lượng khách gần kho nhất của 3 kho",ylab="số khách hàng",ylim=c(0,500),col=c("red","green","blue"))
#Biểu đồ tròn biểu thị lượng khách hàng gần kho nhất của 3 kho
pie(table(final_data$nearest_warehouse),main="Biểu đồ tròn biểu thị lượng khách hàng gần kho nhất của 3 kho")
#Biểu đồ cột biểu thị sự hài lòng của khách hàng",ylab="số khách hàng",col="pink"
barplot(table(final_data$is_happy_customer),main="Biểu đồ cột biểu thị sự hài lòng của khách hàng",ylab="số khách hàng",ylim=c(0,1000),col="pink")
#Biểu đồ tròn biểu thị sự hài lòng của khách hàng
pie(table(final_data$is_happy_customer),main="Biểu đồ tròn biểu thị sự hài lòng của khách hàng",col=c("red","lightblue"))
#Lượng khách mua hàng vào các mùa
barplot(table(final_data$season),main="Lượng khách mua hàng vào các mùa",ylab="số khách hàng",col="Bisque",ylim=c(0,500))
#Biểu đồ tần suất biểu thị vị trí của khách hàng đến kho gần nhất
hist(final_data$distance_to_nearest_warehouse,main="Biểu đồ tần suất biểu thị vị trí của khách hàng đến kho gần nhất", ylab = "số khách hàng", xlab = "khoảng cách")
#Biểu đồ tần suất biểu thị chi phí đặt hàng của khách hàng 
hist(final_data$order_total,main="Biểu đồ tần suất biểu thị tổng chi phí đặt hàng của khách hàng",ylab="số khách hàng",xlab="Tổng chi phí đặt hàng",ylim=c(0,350),col="Coral")
#Biểu đồ hộp biểu thị chi phí đặt hàng của 3 kho
boxplot(order_total~nearest_warehouse,main = "Biểu đồ hộp biểu thị chi phí đặt hàng của 3 kho", xlab = "Kho",ylab = "Tổng chi phí đặt hàng",final_data)
#Dùng hàm r.m_out để tìm ngoại lai và chuyển các giá trị ngoại lai thành NA của biến order_total
rm.out <- function(x, na.rm = TRUE, ...){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm) 
  y <- x
  y[x < (qnt[1] - H)] <- NA 
  y[x > (qnt[2] + H)] <- NA 
  y
}
Bakers_data = subset(final_data,final_data$nearest_warehouse =="Bakers")
Bakers_data$order_total = rm.out(Bakers_data$order_total)

Nickolson_data = subset(final_data,final_data$nearest_warehouse =="Nickolson")
Nickolson_data$order_total = rm.out(Nickolson_data$order_total)

Thompson_data = subset(final_data,final_data$nearest_warehouse =="Thompson")
Thompson_data$order_total = rm.out(Thompson_data$order_total)
final_data_2 <- rbind(Bakers_data,Nickolson_data,Thompson_data)
#Kiểm tra tổng các giá trị NA và phần trăm NA trong biến final_data_2
apply(is.na(final_data_2),2,sum)
apply(is.na(final_data_2),2,mean) 
#Loại bỏ các giá trị NA
final_data_2<-na.omit(final_data_2)
#Vẽ lại đồ thị 
boxplot(order_total~nearest_warehouse, col = c('pink','grey','green'), main = "Biểu đồ hộp biểu thị chi phí đặt hàng ở 3 kho", xlab ="Kho", ylab = "Tổng chi phí đặt hàng",final_data_2)
#Biểu đồ biểu thị chi phí vận chuyển của 3 kho
boxplot(delivery_charges~nearest_warehouse,final_data, main = "Biểu đồ biểu thị chi phí vận chuyển của 3 kho")
#Dùng hàm r.m_out để tìm ngoại lai và chuyển các giá trị ngoại lai thành NA của biến delivery_charges 
rm.out <- function(x, na.rm = TRUE, ...){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm) 
  y <- x
  y[x < (qnt[1] - H)] <- NA 
  y[x > (qnt[2] + H)] <- NA 
  y
}
Bakers_data_1 = subset(final_data,final_data$nearest_warehouse =="Bakers")
Bakers_data_1$delivery_charges = rm.out(Bakers_data_1$delivery_charges)

Nickolson_data_1 = subset(final_data,final_data$nearest_warehouse =="Nickolson")
Nickolson_data_1$delivery_charges = rm.out(Nickolson_data_1$delivery_charges)

Thompson_data_1 = subset(final_data,final_data$nearest_warehouse =="Thompson")
Thompson_data_1$delivery_charges = rm.out(Thompson_data_1$delivery_charges)
final_data_3 <- rbind(Bakers_data_1,Nickolson_data_1,Thompson_data_1)  
#Kiểm tra tổng các giá trị NA và phần trăm NA trong biến final_data_3
apply(is.na(final_data_3),2,sum)
apply(is.na(final_data_3),2,mean)
#Loại bỏ các giá trị NA
final_data_3<-na.omit(final_data_3)
#Vẽ lại đồ thị
boxplot(delivery_charges~nearest_warehouse,main ="Biểu đồ hộp biểu thị chi phí vận chuyển của 3 kho", xlab ="Kho", ylab ="Chi phí vận chuyển", col = "LightCoral",final_data_3)
#Biểu đồ hộp biểu thị chi phí đặt hàng trong 4 mùa
boxplot(order_total~season,final_data, main="Biểu đồ hộp biểu thị chi phí đặt hàng trong 4 mùa", xlab ="Mùa", ylab="Chi phí đặt hàng")
Spring_data = subset(final_data,final_data$season =="Spring")
Spring_data$order_total = rm.out(Spring_data$order_total)

Summer_data = subset(final_data,final_data$season =="Summer")
Summer_data$order_total = rm.out(Summer_data$order_total)

Autumn_data = subset(final_data,final_data$season =="Autumn")
Autumn_data$order_total = rm.out(Autumn_data$order_total)

Winter_data = subset(final_data,final_data$season =="Winter")
Winter_data$order_total = rm.out(Winter_data$order_total)
final_data_1 <- rbind(Spring_data,Summer_data,Autumn_data,Winter_data)
#Kiểm tra tổng các giá trị NA và phần trăm NA trong biến final_data_1
apply(is.na(final_data_1),2,sum)  
apply(is.na(final_data_1),2,mean)
#Loại bỏ các giá trị NA
final_data_1<-na.omit(final_data_1)
#Vẽ lại đồ thị
boxplot(order_total~season, main="Biểu đồ hộp biểu thị chi phí đặt hàng trong 4 mùa", xlab ="Mùa", ylab ="Chi phí đặt hàng", col = "LavenderBlush",final_data_1)

final_data_2 <- rbind(Bakers_data,Nickolson_data,Thompson_data) 

Bakers_data = subset(final_data,final_data$nearest_warehouse =="Bakers")
Bakers_data$order_total = rm.out(Bakers_data$order_total)
Bakers_data_2<-subset(final_data_2,final_data_2$nearest_warehouse =="Bakers")
qqnorm(Bakers_data_2$order_total,main ="Biểu đồ Q_Q biểu thị phân phối chuẩn của chi phí đặt hàng của kho Bakers")
qqline(Bakers_data_2$order_total)


shapiro.test(Bakers_data_2$order_total)


Nickolson_data = subset(final_data,final_data$nearest_warehouse =="Nickolson")
Nickolson_data$order_total = rm.out(Nickolson_data$order_total)
Nickolson_data_2<-subset(final_data_2,final_data_2$nearest_warehouse =="Nickolson")
qqnorm(Nickolson_data_2$order_total,main ="Biểu đồ Q_Q biểu thị phân phối chuẩn của chi phí đặt hàng của kho Nickolson")
qqline(Nickolson_data_2$order_total)


shapiro.test(Nickolson_data_2$order_total)


Thompson_data = subset(final_data,final_data$nearest_warehouse =="Thompson")
Thompson_data$order_total = rm.out(Thompson_data$order_total)
Thompson_data_2<-subset(final_data_2,final_data_2$nearest_warehouse =="Thompson")
qqnorm(Thompson_data_2$order_total,main ="Biểu đồ Q_Q biểu thị phân phối chuẩn của chi phí đặt hàng của kho Thompson")
qqline(Thompson_data_2$order_total)


shapiro.test(Thompson_data_2$order_total)


library(carData)
leveneTest(order_total~as.factor(nearest_warehouse),data=final_data_2)


anova_model_1 <- aov(order_total~nearest_warehouse,data=final_data_2)
summary(anova_model_1)

a

Spring_data_2<-subset(final_data_2,final_data_2$season =="Spring")
qqnorm(Spring_data_2$order_total, main = "Biểu đồ Q_Q biểu thị phân phối chuẩn của chi phí đặt hàng mùa xuân")
qqline(Spring_data_2$order_total)


shapiro.test(Spring_data_2$order_total)


Summer_data_2<-subset(final_data_2,final_data_2$season =="Summer")
qqnorm(Summer_data_2$order_total, main = "Biểu đồ Q_Q biểu thị phân phối chuẩn của chi phí đặt hàng mùa hè")
qqline(Summer_data_2$order_total)


shapiro.test(Summer_data_2$order_total)


Autumn_data_2<-subset(final_data_2,final_data_2$season =="Autumn")
qqnorm(Autumn_data_2$order_total, main = "Biểu đồ Q_Q biểu thị phân phối chuẩn của chi phí đặt hàng mùa thu")
qqline(Autumn_data_2$order_total)


shapiro.test(Autumn_data_2$order_total)


Winter_data_2<-subset(final_data_2,final_data_2$season =="Winter")
qqnorm(Winter_data_2$order_total, main = "Biểu đồ Q_Q biểu thị phân phối chuẩn của chi phí đặt hàng mùa đông")
qqline(Winter_data_2$order_total)


shapiro.test(Winter_data_2$order_total)


library(car)
leveneTest(order_total~as.factor(season),data=final_data_2)


anova_model_2 <- aov(order_total~season,data=final_data_2)
summary(anova_model_2)

#Kiểm định Logistic

library(caTools)
set.seed(500)
split = sample.split(final_data$is_happy_customer, SplitRatio = 0.7)

mauxaydung = subset(final_data,split==TRUE)
maukiemdinh = subset(final_data,split==FALSE)

mohinh = glm(is_happy_customer~.,data=mauxaydung,family = binomial)
summary(mohinh)

dubaoxaydung = predict(mohinh,type="response", newdata = mauxaydung)


dubaokiemdinh = predict(mohinh,type="response", newdata = maukiemdinh)

table(maukiemdinh$is_happy_customer, dubaokiemdinh > 0.5)
 #Độ chính xác của mô hình
(13+203)/nrow(maukiemdinh)
#Độ chính xác của mô hình cơ sở
(13+72)/nrow(maukiemdinh)

library(ROCR)
phuongtrinhdubao = prediction(dubaokiemdinh, maukiemdinh$is_happy_customer)

hamthuchien = performance(phuongtrinhdubao,"tpr","fpr")

plot(hamthuchien)

plot(hamthuchien, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1) , text.adj=c(-0.2,1.7))

as.numeric(performance(phuongtrinhdubao,"auc")@y.values)


library(ResourceSelection)
hoslem.test(mohinh$fitted.values, mohinh$y,g=519)

