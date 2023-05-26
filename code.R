
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
# library(jmastats)
# stations |> View()
# d <-
#   jmastats::jma_collect(item = "daily", block_no = 47895, year = 2022, month = 5) |>
#   dplyr::bind_rows(
#     jmastats::jma_collect(item = "daily", block_no = 47891, year = 2022, month = 5),
#     jmastats::jma_collect(item = "daily", block_no = 47887, year = 2022, month = 5),
#     jmastats::jma_collect(item = "daily", block_no = 47893, year = 2022, month = 5)
#   ) |> 
#   dplyr::select(pressure, humidity, temperature, weather_time) |>
#   tidyr::unnest(cols = c(pressure, humidity, temperature, weather_time)) |>
#   dplyr::select(1, 3, 5, 8, 9) |>
#   dplyr::rename_with(~ c("pressure", "humidity", "temperature",
#                          "weatherdaytime", "weathernighttime")) |>
#   dplyr::mutate(weather = dplyr::if_else(stringr::str_detect(weatherdaytime, "雨") | stringr::str_detect(weathernighttime, "雨"),
#                                          "雨",
#                                          "雨以外") |>
#                   as.factor()) |>
#   dplyr::select(!c(weatherdaytime, weathernighttime))

#d |> readr::write_csv("weather.csv")

df_weather <-
  readr::read_csv("weather.csv", col_types = readr::cols(weather = readr::col_character(), 
                                                   pressure = readr::col_double(), 
                                                   humidity = readr::col_double(), 
                                                   temperature = readr::col_double())) |> 
  dplyr::mutate(weather = as.factor(weather) |> forcats::fct_rev())


# 数値としての扱われ方を確認... 1が雨、2が雨以外
# df_weather$weather |> as.numeric() |> table()

# ロジスティック回帰モデルの作成
model <- 
  glm(weather ~ temperature + humidity + pressure, data = df_weather, family = binomial)
summary(model)
# 1に近ければ「雨」
# 0から1までの値を出力するようにtype = "response"を指定
head(predict(model, type = "response"))
df_weather$weather[1:5]
contrasts(df_weather$weather)

# 散布図を描画し、データの傾向を確認
library(ggplot2)
p <- ggplot(df_weather) +
  aes(humidity, as.numeric(weather)-1) +
  geom_point() +
  ylab("weather")
p + stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "#57467b")

# 新しいデータをもとに「雨」の確率を求める
# 新しい天気のデータ
new_weather <- data.frame(temperature = 14.1, humidity = 88, pressure = 1001)
predicted_prob <- predict(model, newdata = new_weather, type = "response")
predicted_prob

# 1- にすることで「雨以外」の確率を求める
1 - predict(model, 
        newdata = data.frame(temperature = 22.6, humidity = 28, pressure = 1023), 
        type = "response")

