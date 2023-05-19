
# 3 -----------------------------------------------------------------------
data(penguins, package = "palmerpenguins")






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



dplyr::count(penguins, species)
x <- 
  subset(penguins, species == "Adelie", select = body_mass_g)[[1]]
x <- x[!is.na(x)]

p <- 
  hist(x, breaks = 10, col = "#57467b") # 体重の値を10個の階級に分ける
p # グラフの表示
p$breaks # 階級の幅は200

hist(x, breaks = 15) # 体重の値を15個の階級に分ける
hist(x, breaks = 5) # 体重の値を5個の階級に分ける

boxplot(x)
quantile(x)


# 4 -----------------------------------------------------------------------
set.seed(123)
population <- seq.int(100)
sample <- sample(population, 10)
sample

x <- seq.int(6)
y <- dunif(x, 1, 6)
plot(x, y, 
     type = "h", 
     lwd = 3, 
     col = "#57467b", 
     ylim = c(0, 0.2), 
     xlab = "サイコロの出る目", 
     ylab = "確率",
     main = "サイコロの出る目の離散一様分布")

# xの範囲を設定
x <- seq(-5, 5, by = 0.1)
y1 <- dnorm(x, mean = 0, sd = 1)
# グラフを描画
plot(x, y1, type = "l", 
     lwd = 3,
     col = "#57467b",
     ylim = c(0, 0.5), 
     xlab = "x", 
     ylab = "Density", 
     main = "正規分布")

# 平均0で分散がそれぞれ0.2、5の正規分布の確率密度関数を計算
y2 <- dnorm(x, mean = 0, sd = 0.2)
y3 <- dnorm(x, mean = 0, sd = 5)
plot(x, y1, type = "l", 
     lwd = 3,
     col = "#57467b",
     ylim = c(0, max(c(y1, y2, y3))), 
     xlab = "x", 
     ylab = "Density", 
     main = "正規分布")
lines(x, y2, lwd = 3, col = "#7cb4b8")
lines(x, y3, lwd = 3, col = "#70f8ba")
# 凡例を追加
legend("topright", 
       legend = c("分散 = 1", 
                  "分散 = 0.2", 
                  "分散 = 5"), 
       col = c("#57467b", "#7cb4b8", "#70f8ba"), 
       lty = 1,
       lwd = 3)


x
mean_x <- 
  mean(x, na.rm = TRUE)
t.test(x)
conf_int_x <- t.test(x)$conf.int
print(paste("Confidence interval:", conf_int_x[1], "to", conf_int_x[2]))


