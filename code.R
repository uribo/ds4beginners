# xの範囲を設定
x <- seq(-10, 10, by = 0.1)

# 平均0で標準偏差がそれぞれ1、0.2、5の正規分布の確率密度関数を計算
y1 <- dnorm(x, mean = 0, sd = 1)
y2 <- dnorm(x, mean = 0, sd = 0.2)
y3 <- dnorm(x, mean = 0, sd = 5)

# グラフを描画
plot(x, y1, type = "l", 
     lwd = 3,
     col = "#57467b",
     ylim = c(0, max(y1, y2, y3)), 
     xlab = "x", 
     ylab = "Density", 
     main = "正規分布")
lines(x, y2, lwd = 3, col = "#7cb4b8")
lines(x, y3, lwd = 3, col = "#70f8ba")

# 凡例を追加
legend("topright", 
       legend = c("標準偏差 = 1", 
                  "標準偏差 = 0.2", 
                  "標準偏差 = 5"), 
       col = c("#57467b", "#7cb4b8", "#70f8ba"), 
       lty = 1,
       lwd = 3)


x <- seq(-6, 6, length.out = 100)
y <- dnorm(x, mean = 0, sd = 1)
plot(x, y, type = "l", lwd = 3,
     ylim = c(0, 2),
     main = "Normal Distribution (mean = 0, sd = 1)",
     xlab = "x", ylab = "Density")

# 平均0で標準偏差が0.2の正規分布の確率密度関数を描画
y2 <- dnorm(x, mean = 0, sd = 0.2)
lines(x, y2, col = "red", lwd = 3)

# 平均0で標準偏差が5の正規分布の確率密度関数を描画
y3 <- dnorm(x, mean = 0, sd = 5)
lines(x, y3, col = "blue", lwd = 3)

library(palmerpenguins)
data("penguins")


dplyr::count(penguins, species)
x <- 
  subset(penguins, species == "Adelie", select = body_mass_g)[[1]]
x <- x[!is.na(x)]

p <- 
  hist(x, breaks = 10) # 体重の値を10個の階級に分ける
p # グラフの表示
p$breaks # 階級の幅は200

hist(x, breaks = 15) # 体重の値を15個の階級に分ける
hist(x, breaks = 5) # 体重の値を5個の階級に分ける

boxplot(x)
quantile(x)

x
mean_x <- 
  mean(x, na.rm = TRUE)
t.test(x)
conf_int_x <- t.test(x)$conf.int
print(paste("Confidence interval:", conf_int_x[1], "to", conf_int_x[2]))


