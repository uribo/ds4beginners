
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
y1 <- dnorm(x, mean = 0, sd = sqrt(1))
# グラフを描画
plot(x, y1, type = "l", 
     lwd = 3,
     col = "#57467b",
     ylim = c(0, 0.5), 
     xlab = "x", 
     ylab = "Density", 
     main = "正規分布")

# 平均0で分散がそれぞれ0.2、5の正規分布の確率密度関数を計算
y2 <- dnorm(x, mean = 0, sd = sqrt(0.2))
y3 <- dnorm(x, mean = 0, sd = sqrt(5))
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


# 5 -----------------------------------------------------------------------
# 気温のデータを作成
temperature <- seq(18, 40, 4)
# アイスクリームの売り上げのデータを作成
icecream_sales <- c(28, 45, 72, 82, 96, 100)
# 散布図の作成
plot(temperature, icecream_sales)

# 単回帰モデルの構築
model <- lm(icecream_sales ~ temperature)

# 回帰直線の追加
abline(model)

predict(model, data.frame(temperature = 30))

humidity <- c(0.65, 0.8, 0.75, 0.85, 0.9, 0.8)
wind_speed <- c(2, 3, 4, 6, 3, 1)
model <- lm(icecream_sales ~ temperature + humidity + wind_speed)

coefficients(model)


# 6 -----------------------------------------------------------------------
library(jmastats)
d <- 
  jmastats::jma_collect(item = "daily", block_no = 47895, year = 2022, month = 5) |> 
  dplyr::select(pressure, humidity, temperature, weather_time) |> 
  tidyr::unnest(cols = c(pressure, humidity, temperature, weather_time)) |> 
  dplyr::select(1, 3, 5, 8, 9) |> 
  dplyr::rename_with(~ c("pressure", "humidity", "temperature", 
                         "weatherdaytime", "weathernighttime")) |> 
  dplyr::mutate(weather = dplyr::if_else(stringr::str_detect(weatherdaytime, "雨") | stringr::str_detect(weathernighttime, "雨"),
                                         "雨",
                                         "雨以外") |> 
                  as.factor()) |> 
  dplyr::select(!c(weatherdaytime, weathernighttime))
  

# ロジスティック回帰モデルの作成
model <- glm(weather ~ temperature + humidity + pressure, data=d, family=binomial)
summary(model)

# 新しい天気のデータ
new_weather <- data.frame(temperature = 16.3, humidity = 30, pressure = 1012)

# 晴れる確率の予測
predicted_prob <- predict(model, newdata = new_weather, type = "response")
predicted_prob

1 - predict(model, 
        newdata = data.frame(temperature = 15.1, humidity = 68, pressure = 1020), 
        type = "response")
