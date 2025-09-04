# Загрузка только необходимых пакетов
library(quantmod)
library(ggplot2)

# 1. Загрузка данных
getSymbols("GOOG", from = "2023-01-01", to = Sys.Date())

# 2. Создание dataframe
data <- data.frame(
  date = index(GOOG),
  price = as.numeric(Cl(GOOG)),
  volume = as.numeric(Vo(GOOG))
)

# 3. Расчет изменений
data$price_change <- c(NA, diff(data$price) / data$price[-nrow(data)] * 100)
data$volume_change <- c(NA, diff(data$volume) / data$volume[-nrow(data)] * 100)
data <- na.omit(data)

# 4. Визуальный анализ
plot(data$date, data$price_change, type = "l", col = "blue",
     main = "Динамика цены и объема", xlab = "Дата", ylab = "Изменение цены (%)")
lines(data$date, data$volume_change/10, col = "red")
legend("topright", legend = c("Цена", "Объем/10"), col = c("blue", "red"), lty = 1)

# 5. Scatter plot
plot(data$volume_change, data$price_change, pch = 16, col = "darkblue",
     main = "Зависимость цены от объема",
     xlab = "Изменение объема (%)", ylab = "Изменение цены (%)")
abline(lm(price_change ~ volume_change, data = data), col = "red")

# 6. Статистический анализ
correlation <- cor(data$volume_change, data$price_change)
cat("Коэффициент корреляции:", round(correlation, 4), "\n")

cor_test <- cor.test(data$volume_change, data$price_change)
cat("P-значение:", round(cor_test$p.value, 4), "\n")

# 7. Регрессионный анализ
model <- lm(price_change ~ volume_change, data = data)
cat("\nРезультаты регрессии:\n")
print(summary(model))

# 8. Анализ по направлениям
volume_up <- data$price_change[data$volume_change > 0]
volume_down <- data$price_change[data$volume_change <= 0]

cat("\nСреднее изменение цены при росте объема:", round(mean(volume_up), 4), "%\n")
cat("Среднее изменение цены при падении объема:", round(mean(volume_down), 4), "%\n")

t_test <- t.test(volume_up, volume_down)
cat("P-значение t-теста:", round(t_test$p.value, 4), "\n")

# 9. Интерпретация
cat("\n=== ВЫВОДЫ ===\n")
if (cor_test$p.value < 0.05) {
  if (correlation > 0) {
    cat("✅ Статистически значимая положительная связь\n")
    cat("📈 Рост объема торгов приводит к росту цены\n")
  } else {
    cat("✅ Статистически значимая отрицательная связь\n")
    cat("📉 Рост объема торгов приводит к падению цены\n")
  }
} else {
  cat("❌ Нет статистически значимой связи\n")
  cat("➡️ Рост объема не влияет на цену статистически значимо\n")
}

cat("Сила связи:", 
    ifelse(abs(correlation) > 0.5, "сильная",
           ifelse(abs(correlation) > 0.3, "умеренная", "слабая")), "\n")

# Минималистичный вариант без внешних зависимостей

price <- as.numeric(Cl(GOOG))
volume <- as.numeric(Vo(GOOG))

price_change <- c(NA, diff(price) / price[-length(price)] * 100)
volume_change <- c(NA, diff(volume) / volume[-length(volume)] * 100)

# Удаляем NA
valid_data <- !is.na(price_change) & !is.na(volume_change)
price_change <- price_change[valid_data]
volume_change <- volume_change[valid_data]

# Статистический анализ
correlation <- cor(volume_change, price_change)
p_value <- cor.test(volume_change, price_change)$p.value

# Вывод результатов
cat("РЕЗУЛЬТАТЫ АНАЛИЗА GOOG:\n")
cat("=======================\n")
cat("Корреляция:", round(correlation, 4), "\n")
cat("P-значение:", round(p_value, 4), "\n")
cat("Наблюдений:", length(price_change), "\n\n")

if (p_value < 0.05) {
  if (correlation > 0) {
    cat("ВЫВОД: Рост объема покупок приводит к статистически значимому росту цены\n")
  } else {
    cat("ВЫВОД: Рост объема покупок приводит к статистически значимому падению цены\n")
  }
} else {
  cat("ВЫВОД: Нет статистически значимой связи между объемом и ценой\n")
}
