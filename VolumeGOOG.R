# Установка и загрузка необходимых пакетов
if (!require("quantmod")) install.packages("quantmod")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("lmtest")) install.packages("lmtest")

library(quantmod)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(lmtest)

# Загрузка данных по акциям GOOG
getSymbols("GOOG", from = "2023-01-01", to = Sys.Date(), periodicity = "daily")

# Создание датафрейма с классификацией объема
data <- data.frame(
  Date = index(GOOG),
  Close = as.numeric(Cl(GOOG)),
  Open = as.numeric(Op(GOOG)),
  High = as.numeric(Hi(GOOG)),
  Low = as.numeric(Lo(GOOG)),
  Volume = as.numeric(Vo(GOOG))
) %>%
  filter(Volume > 0) %>%
  mutate(
    Price_Change = Close - Open,
    Price_Change_Pct = (Close - Open) / Open * 100,
    Day_Type = ifelse(Close > Open, "Buying", "Selling"),
    Day_Strength = abs(Price_Change_Pct),
    log_Volume = log(Volume),
    log_Close = log(Close)
  ) %>%
  na.omit()

# Анализ распределения по типам дней
cat("=== РАСПРЕДЕЛЕНИЕ ДНЕЙ ПО ТИПАМ ===\n")
table(data$Day_Type)
prop.table(table(data$Day_Type))

# 1. График: Объем vs Изменение цены
p1 <- ggplot(data, aes(x = Volume, y = Price_Change_Pct, color = Day_Type)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = c("Buying" = "green", "Selling" = "red")) +
  labs(title = "Влияние объема на изменение цены",
       x = "Объем торгов (log)", 
       y = "Изменение цены (%)",
       color = "Тип дня") +
  theme_minimal()

# 2. График: Средний объем по типам дней
volume_summary <- data %>%
  group_by(Day_Type) %>%
  summarise(
    Mean_Volume = mean(Volume),
    Median_Volume = median(Volume),
    SD_Volume = sd(Volume),
    Count = n()
  )

p2 <- ggplot(volume_summary, aes(x = Day_Type, y = Mean_Volume, fill = Day_Type)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("Buying" = "green", "Selling" = "red")) +
  labs(title = "Средний объем по типам торговых дней",
       x = "Тип дня", y = "Средний объем") +
  theme_minimal()

# 3. Регрессионный анализ с взаимодействием
model_interaction <- lm(Price_Change_Pct ~ log_Volume * Day_Type, data = data)

cat("=== РЕГРЕССИОННЫЙ АНАЛИЗ С ВЗАИМОДЕЙСТВИЕМ ===\n")
summary(model_interaction)

# 4. Раздельные регрессии для покупок и продаж
model_buying <- lm(Price_Change_Pct ~ log_Volume, 
                  data = data %>% filter(Day_Type == "Buying"))
model_selling <- lm(Price_Change_Pct ~ log_Volume, 
                   data = data %>% filter(Day_Type == "Selling"))

cat("\n=== МОДЕЛЬ ДЛЯ ДНЕЙ ПОКУПОК ===\n")
summary(model_buying)

cat("\n=== МОДЕЛЬ ДЛЯ ДНЕЙ ПРОДАЖ ===\n")
summary(model_selling)

# 5. График раздельных регрессий
p3 <- ggplot(data, aes(x = log_Volume, y = Price_Change_Pct, color = Day_Type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("Buying" = "green", "Selling" = "red")) +
  labs(title = "Раздельные регрессии для покупок и продаж",
       x = "Логарифм объема", 
       y = "Изменение цены (%)",
       color = "Тип дня") +
  theme_minimal()

# 6. Анализ экстремальных объемов
high_volume_threshold <- quantile(data$Volume, 0.9)
data_high_volume <- data %>% filter(Volume > high_volume_threshold)

cat("\n=== АНАЛИЗ ЭКСТРЕМАЛЬНЫХ ОБЪЕМОВ (верхние 10%) ===\n")
table(data_high_volume$Day_Type)
prop.table(table(data_high_volume$Day_Type))

p4 <- ggplot(data_high_volume, aes(x = Day_Type, fill = Day_Type)) +
  geom_bar(alpha = 0.7) +
  scale_fill_manual(values = c("Buying" = "green", "Selling" = "red")) +
  labs(title = "Распределение экстремальных объемов по типам дней",
       x = "Тип дня", y = "Количество дней") +
  theme_minimal()

# 7. Анализ силы движения
p5 <- ggplot(data, aes(x = Volume, y = Day_Strength, color = Day_Type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = c("Buying" = "green", "Selling" = "red")) +
  labs(title = "Влияние объема на силу движения цены",
       x = "Объем (log)", 
       y = "Абсолютное изменение цены (%)",
       color = "Тип дня") +
  theme_minimal()

# Вывод всех графиков
grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

# Статистические тесты
cat("\n=== СТАТИСТИЧЕСКИЕ ТЕСТЫ ===\n")

# Тест на разницу средних объемов
t_test_result <- t.test(Volume ~ Day_Type, data = data)
cat("T-test разницы объемов между днями покупок и продаж:\n")
print(t_test_result)

# Тест на разницу в силе движения
strength_test <- t.test(Day_Strength ~ Day_Type, data = data)
cat("\nT-test разницы силы движения между днями покупок и продаж:\n")
print(strength_test)

# Дополнительный анализ: лаговые эффекты
if (nrow(data) > 1) {
  data <- data %>%
    arrange(Date) %>%
    mutate(
      Volume_Lag1 = lag(Volume, 1),
      Price_Change_Lag1 = lag(Price_Change_Pct, 1)
    )
  
  cat("\n=== АНАЛИЗ ЛАГОВЫХ ЭФФЕКТОВ ===\n")
  cor_lag <- cor(data$Volume_Lag1, data$Price_Change_Pct, use = "complete.obs")
  cat("Корреляция между объемом вчера и изменением цены сегодня:", round(cor_lag, 3), "\n")
}

# Вывод итоговых результатов
cat("\n=== ИТОГОВЫЕ ВЫВОДЫ ===\n")
cat("1. Дни покупок (Close > Open):", sum(data$Day_Type == "Buying"), "дней\n")
cat("2. Дни продаж (Close < Open):", sum(data$Day_Type == "Selling"), "дней\n")
cat("3. Средний объем в дни покупок:", round(mean(data$Volume[data$Day_Type == "Buying"]), 0), "\n")
cat("4. Средний объем в дни продаж:", round(mean(data$Volume[data$Day_Type == "Selling"]), 0), "\n")
cat("5. Сила связи объема и изменения цены (R²):", round(summary(model_interaction)$r.squared, 3), "\n")
