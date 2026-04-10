#3. Tien Xu Ly So Lieu

#3.1 Doc du lieu
dirty_data <- read.csv("D:/Minh/R&Rstu/dirty_data.csv")
head(dirty_data,10)

#3.2 Lam sach du lieu

#Chon loc bien
new_data <- dirty_data[,c("nearest_warehouse","order_price","delivery_charges",
             "coupon_discount","order_total","season","is_expedited_delivery",
             "distance_to_nearest_warehouse","is_happy_customer")]
head(new_data,10)

#Kiem tra du lieu khuyet
library(questionr)
freq.na(new_data)

#Kiem tra gia tri cua cac bien phan loai
unique(new_data$nearest_warehouse)
unique(new_data$season)
unique(new_data$is_expedited_delivery)
unique(new_data$is_happy_customer)

#Sua loi viet thuong
new_data$nearest_warehouse[new_data$nearest_warehouse == "nickolson"] <- "Nickolson"
new_data$nearest_warehouse[new_data$nearest_warehouse == "thompson"] <- "Thompson"

new_data$season[new_data$season == "spring"] <- "Spring"
new_data$season[new_data$season == "summer"] <- "Summer"
new_data$season[new_data$season == "autumn"] <- "Autumn"
new_data$season[new_data$season == "winter"] <- "Winter"

unique(new_data$nearest_warehouse)
unique(new_data$season)

#4. Thong Ke Mo Ta

#Tom tat cac thong ke mo ta cho nhung bien lien tuc
continous_data <- new_data[,c("order_price","delivery_charges",
                          "coupon_discount","order_total",
                          "distance_to_nearest_warehouse")]

library(psych)
describe(continous_data,fast=TRUE)

#Thong ke so luong cho cac bien phan loai
categorical_data <- new_data[,c("nearest_warehouse","season","is_expedited_delivery",
                             "is_happy_customer")]

categorical_data$nearest_warehouse <- as.factor(categorical_data$nearest_warehouse)
categorical_data$season <- as.factor(categorical_data$season)
categorical_data$is_expedited_delivery <- as.factor(categorical_data$is_expedited_delivery)
categorical_data$is_happy_customer <- as.factor(categorical_data$is_happy_customer)

summary(categorical_data)

#Histogram

library(ggplot2)
ggplot(continous_data, aes(x=order_total)) + geom_histogram(bins = 15,fill="lightblue")+
  labs(title="Order Total Histogram Plot",x="Order Total (USD)", y = "Count") 

ggplot(continous_data, aes(x=order_price)) + geom_histogram(bins = 15,fill="lightblue")+
  labs(title="Order Price Histogram Plot",x="Order Price (USD)", y = "Count")

ggplot(continous_data, aes(x=distance_to_nearest_warehouse)) + geom_histogram(bins = 15,fill="lightblue")+
  labs(title="Distance To Nearest Warehouse Histogram Plot",x="Distance (km)", y = "Count")

ggplot(continous_data, aes(x=coupon_discount)) + geom_histogram(bins = 15,fill="lightblue")+
  labs(title="Coupon Discount Histogram Plot",x="Coupon Discount (%)", y = "Count")

ggplot(continous_data, aes(x=delivery_charges)) + geom_histogram(bins = 15,fill="lightblue")+
  labs(title="Delivery Charges Histogram Plot",x="Delivery Charges (USD)", y = "Count")

#Không thấy rõ được phân phối, đa số lệch phải, vì có những điểm ngoại lai

### Xu ly ngoai lai (Q1-1.5IQR, Q3+1.5IQR)

new_data2 <- new_data

rm.out <- function (x, na.rm = TRUE , ...) {
   qnt <- quantile (x, probs =c(.25 , .75) , na.rm = na.rm , ...)
   H <- 1.5 * IQR(x, na.rm = na.rm)
   y <- x
   y[x < ( qnt [1] - H)] <- NA
   y[x > ( qnt [2] + H)] <- NA
   y
}

new_data2$order_total = rm.out(new_data2$order_total)
new_data2$order_price = rm.out(new_data2$order_price)
new_data2$distance_to_nearest_warehouse = rm.out(new_data2$distance_to_nearest_warehouse)

freq.na(new_data2)
new_data2 <- na.omit(new_data2)

ggplot(new_data2, aes(x=order_total)) + geom_histogram(bins = 15,fill="lightblue")+
  labs(title="Order Total Histogram Plot",x="Order Total (USD)", y = "Count") 

ggplot(new_data2, aes(x=order_price)) + geom_histogram(bins = 15,fill="lightblue")+
  labs(title="Order Price Histogram Plot",x="Order Price (USD)", y = "Count")

ggplot(new_data2, aes(x=distance_to_nearest_warehouse)) + geom_histogram(bins = 15,fill="lightblue")+
  labs(title="Distance To Nearest Warehouse Histogram Plot",x="Distance (km)", y = "Count")

#Do thi phan tan

ggplot(new_data, aes(x=order_price, y=order_total)) + geom_point(shape=23,color="darkblue")+
  labs(title="Order Total & Order Price Scatter Plot") 

ggplot(new_data2, aes(x=order_price, y=order_total)) + geom_point(shape=23,color="darkblue")+
  labs(title="Order Total & Order Price Scatter Plot") 

ggplot(new_data, aes(x=delivery_charges, y=order_total)) + geom_point(shape=23,color="darkblue")+
  labs(title="Order Total & Delivery Charges Scatter Plot") 

ggplot(new_data2, aes(x=delivery_charges, y=order_total)) + geom_point(shape=23,color="darkblue")+
  labs(title="Order Total & Delivery Charges Scatter Plot") 

ggplot(new_data, aes(x=coupon_discount, y=order_total)) + geom_point(shape=23,color="darkblue")+
  labs(title="Order Total & Coupon Discount Scatter Plot") 

ggplot(new_data2, aes(x=coupon_discount, y=order_total)) + geom_point(shape=23,color="darkblue")+
  labs(title="Order Total & Coupon Discount Scatter Plot") 

ggplot(new_data, aes(x=distance_to_nearest_warehouse, y=order_total)) + geom_point(shape=23,color="darkblue")+
  labs(title="Order Total & Distance To Nearest Warehouse Scatter Plot") 

ggplot(new_data2, aes(x=distance_to_nearest_warehouse, y=order_total)) + geom_point(shape=23,color="darkblue")+
  labs(title="Order Total & Distance To Nearest Warehouse Scatter Plot") 

#Do thi Boxplot

ggplot(new_data, aes(x=nearest_warehouse, y=order_total, fill=nearest_warehouse)) + geom_boxplot() + 
  labs(title="Plot Of Order Total Per Nearest Warehouse") 

ggplot(new_data2, aes(x=nearest_warehouse, y=order_total, fill=nearest_warehouse)) + geom_boxplot() + 
  labs(title="Plot Of Order Total Per Nearest Warehouse") 

#Kho hàng Thompson có tổng chi phí đặt hàng cao hơn so với kho hàng Bakers và Nickolson, tuy nhiên không cao hơn nhiều

ggplot(new_data, aes(x=season, y=order_total, fill=season)) + geom_boxplot() + 
  labs(title="Plot Of Order Total Per Season") 

ggplot(new_data2, aes(x=season, y=order_total,fill=season)) + geom_boxplot() + 
  labs(title="Plot Of Order Total Per Season") 

ggplot(new_data, aes(x=is_expedited_delivery, y=order_total, fill=is_expedited_delivery)) + geom_boxplot() + 
  labs(title="Plot Of Order Total Per Delivery") 

ggplot(new_data2, aes(x=is_expedited_delivery, y=order_total,fill=is_expedited_delivery)) + geom_boxplot() + 
  labs(title="Plot Of Order Total Per Delivery") 

ggplot(new_data, aes(x=is_happy_customer, y=order_total, fill=is_happy_customer)) + geom_boxplot() + 
  labs(title="Plot Of Order Total Per Customer") 

ggplot(new_data2, aes(x=is_happy_customer, y=order_total, fill=is_happy_customer)) + geom_boxplot() + 
  labs(title="Plot Of Order Total Per Customer") 

#5. Thong Ke Suy Dien

#5.1 Anova mot nhan to
#Bai toan: So sanh tong chi phi dat hang o bon mua voi muc y nghia 5%
#Loc du lieu

Spring_data <- subset(new_data2,season=="Spring")
Summer_data <- subset(new_data2,season=="Summer")
Autumn_data <- subset(new_data2,season=="Autumn")
Winter_data <- subset(new_data2,season=="Winter")

#Kiem tra phan phoi chuan

par(mfrow=c(2,2))
qqnorm(Spring_data$order_total,main = "Spring")
qqline(Spring_data$order_total)
qqnorm(Summer_data$order_total, main = "Summer")
qqline(Summer_data$order_total)
qqnorm(Autumn_data$order_total, main = "Autumn")
qqline(Autumn_data$order_total)
qqnorm(Winter_data$order_total, main = "Winter")
qqline(Winter_data$order_total)

shapiro.test(Spring_data$order_total)
shapiro.test(Summer_data$order_total)
shapiro.test(Autumn_data$order_total)
shapiro.test(Winter_data$order_total)

#Kiem dinh gia dinh ve tinh dong nhat cua phuong sai (levenetest)

library (car)
leveneTest(order_total~as.factor(season),data=new_data2)
#p-value > 5% chưa đủ bằng chứng bác bỏ H0

#Thuc hien mo hinh anova
anova_model <- aov(order_total~season,data=new_data2)
summary(anova_model)
##p-value > 5% chưa đủ bằng chứng bác bỏ H0

#Thuc hien kiem dinh kruskal-Wallis
kruskal.test(order_total~season,data=new_data2)

#5.2 Hoi quy da bien

#Chia du lieu

set.seed(99)
train.rows <- sample(rownames(new_data2), dim(new_data2)[1]* 0.8)
train_data <- new_data2[train.rows,]
test.rows <- setdiff(rownames(new_data2), train.rows)
test_data <- new_data[ test.rows,]

#Tim phuong trinh uoc luong
#Y = B0 + B1X1 + B2X2 +...+ epsilon

lm_model <- lm(order_total ~ nearest_warehouse + order_price + delivery_charges +
   coupon_discount + season + is_expedited_delivery +
   distance_to_nearest_warehouse + is_happy_customer, data = train_data)
summary

##Kiem dinh cac he so hoi quy

#H0: B1=0
#H1: B1=!0

#p-value < muc y nghia 5%, bac bo H, B1=!0, X1 co anh huong den Y
#p-value >= muc y nghia 5%, chua bac bo H, B1=0, X1 khong anh huong den Y
#RR = (-vc, -t_alpha/2,n-k-1) U (t_alpha/2,n-k-1, +vc) ; n-k-1 = 455-11-1
qt(p=.05/2,df=443, lower.tail=FALSE)

##Phan biet R^2 và R^2 hieu chinh => y nghia

##Kiem dinh duong hoi quy
#H0: B0=B1=B2=....=0 : đường hồi quy không có ý nghĩa. đường hồi quy không thích hợp (R^2=0)
#H1: Tồn tại Bi=!0 : đường hồi quy có ý nghĩa, đường hồi quy thích hợp
#p-value < 2.2e-16 => Đường hồi quy có ý nghĩa, có ít nhất1 biến độc lập có ý nghĩa trong việc dự báo Y

#RR = (F_alpha,k,n-k-1; +vc) = (F_0.05,11,443; +vc)
#qf(p=.05,df1=11,df2=443, lower.tail=FALSE)

lm_model2 <- lm(order_total ~ order_price + coupon_discount, data = train_data)
summary(lm_model2)

par(mfrow=c(2,2))
plot(lm_model2)

#Du bao
test_data$predicted_value <- predict(lm_model2, test_data)
plot(test_data$order_total,test_data$predicted_value)
r_squared <- 1 - (sum((test_data$order_total - test_data$predicted_value)^2)/
                    sum((test_data$order_total - mean(test_data$predicted_value))^2))
print(paste("R-squared", r_squared))
#Nhan xet:....
                        
                        
                        
                        
                        