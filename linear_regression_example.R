# Получаем значения показателей для каждого квартала
Q1_2014 <- data.frame(
        period = "${Q1_2014_end}",
		prib   = Q1_2014_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q1_2014_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q1_2014_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q1_2014_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q1_2014_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q2_2014 <- data.frame(
        period = "${Q2_2014_end}",
		prib   = Q2_2014_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q2_2014_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q2_2014_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q2_2014_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q2_2014_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q3_2014 <- data.frame(
        period = "${Q3_2014_end}",
		prib   = Q3_2014_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q3_2014_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q3_2014_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q3_2014_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q3_2014_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q4_2014 <- data.frame(
        period = "${Q4_2014_end}",
		prib   = Q4_2014_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q4_2014_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q4_2014_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q4_2014_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q4_2014_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q1_2015 <- data.frame(
        period = "${Q1_2015_end}",
		prib   = Q1_2015_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q1_2015_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q1_2015_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q1_2015_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q1_2015_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q2_2015 <- data.frame(
        period = "${Q2_2015_end}",
		prib   = Q2_2015_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q2_2015_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q2_2015_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q2_2015_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q2_2015_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q3_2015 <- data.frame(
        period = "${Q3_2015_end}",
		prib   = Q3_2015_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q3_2015_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q3_2015_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q3_2015_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q3_2015_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q4_2015 <- data.frame(
        period = "${Q4_2015_end}",
		prib   = Q4_2015_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q4_2015_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q4_2015_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q4_2015_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q4_2015_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q1_2016 <- data.frame(
        period = "${Q1_2016_end}",
		prib   = Q1_2016_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q1_2016_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q1_2016_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q1_2016_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q1_2016_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q2_2016 <- data.frame(
        period = "${Q2_2016_end}",
		prib   = Q2_2016_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q2_2016_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q2_2016_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q2_2016_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q2_2016_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q3_2016 <- data.frame(
        period = "${Q3_2016_end}",
		prib   = Q3_2016_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q3_2016_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q3_2016_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q3_2016_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q3_2016_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q4_2016 <- data.frame(
        period = "${Q4_2016_end}",
		prib   = Q4_2016_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q4_2016_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q4_2016_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q4_2016_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q4_2016_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q1_2017 <- data.frame(
        period = "${Q1_2017_end}",
		prib   = Q1_2017_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q1_2017_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q1_2017_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q1_2017_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q1_2017_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q2_2017 <- data.frame(
        period = "${Q2_2017_end}",
		prib   = Q2_2017_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q2_2017_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q2_2017_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q2_2017_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q2_2017_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q3_2017 <- data.frame(
        period = "${Q3_2017_end}",
		prib   = Q3_2017_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q3_2017_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q3_2017_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q3_2017_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q3_2017_РасчетыСПоставщиками[1, c("ДолгBalance")])
)
Q4_2017 <- data.frame(
        period = "${Q4_2017_end}",
		prib   = Q4_2017_ВыручкаИСебестоимость[1, c("СуммаВыручкиTurnover")] -
                 Q4_2017_ВыручкаИСебестоимость[1, c("СтоимостьTurnover")] -
                 Q4_2017_ПрочиеРасходы[1, c("СуммаTurnover")],
		deb    = Q4_2017_РасчетыСКлиентами[1, c("ДолгBalance")],
	 	kred   = abs(Q4_2017_РасчетыСПоставщиками[1, c("ДолгBalance")])
)

# Объединяем все наблюдения в одну таблицу
df <- rbind(Q1_2014, Q2_2014, Q3_2014, Q4_2014, Q1_2015, Q2_2015, Q3_2015, Q4_2015,
	 		Q1_2016, Q2_2016, Q3_2016, Q4_2016, Q1_2017, Q2_2017, Q3_2017, Q4_2017)

# Приводим данные к формату xts
library(xts)
data_xts <- xts(df[,-1], order.by=as.POSIXct(df[,1]))

# Строим график
library(dygraphs)
graph <- dygraph(data_xts, width=750, height=350) %>%
    dySeries("prib", label = "Прибыль") %>%
    dySeries("deb", label = "Деб_зад") %>%
    dySeries("kred", label = "Кред_зад") %>%
    dyOptions(drawPoints=TRUE, pointSize=2, drawAxesAtZero=TRUE, labelsKMB=TRUE) %>%
    dyRangeSelector()

# Сохраняем виджет с графиком 
library(htmlwidgets)
saveWidget(graph, file="/mnt/host/prib.html", selfcontained = TRUE)

## Пробуем спрогнозировать прибыль от продаж с помощью статистических методов

# Предварительно оцениваем линейную модель
model <- lm(data=df, prib~deb+kred)
paste(capture.output(print(summary(model))), collapse='\n')

# Проверяем, что набор данных не содержит "выбросов", т.е. наблюдений, которые плохо предсказываются моделью.
# Для этого строим график Квантиль-Квантиль
library(car)
qq <- qqPlot(model, labels = row.names(df), simulate = TRUE, main = "Q-Q plot")
print(qq)
dev.off()

# Удаляем "выброс"
df1 <- df[-c(12),]

# Снова оцениваем модель
model1 <- lm(data=df1, prib~deb+kred)
paste(capture.output(print(summary(model1))), collapse='\n')

# Проверяем выполнение всех предпосылок линейной регрессии
library(gvlma)
gvmodel <- gvlma(model1)
paste(capture.output(print(summary(gvmodel))), collapse='\n')

# Создаем таблицу с плановыми данными дебиторской и кредиторской задолженностей
# и приводим её к формату xts
q1_2018_plan <- data.frame(period="2018-03-31T23:59:59", deb=${deb_plan1}, kred=${kred_plan1})
q2_2018_plan <- data.frame(period="2018-06-30T23:59:59", deb=${deb_plan2}, kred=${kred_plan2})
q3_2018_plan <- data.frame(period="2018-09-30T23:59:59", deb=${deb_plan3}, kred=${kred_plan3})
plan_df <- rbind(q1_2018_plan, q2_2018_plan, q3_2018_plan)
plan_xts <- xts(plan_df[,-1], order.by=as.POSIXct(plan_df[,1]))

# Предсказываем прибыль от продаж
predicted <- predict(model1, newdata=plan_xts, interval="prediction")

## Выводим на график плановые значения дебиторской и кредиторской задолженностей 
## и предсказанные значения величины прибыли от продаж

# Объединяем все наборы данных
combined <- cbind(data_xts, plan_xts, predicted)

# Строим новый график
graph1 <- dygraph(combined, width=750, height=350) %>%
    dySeries("prib", label = "Прибыль", color="red") %>%
    dySeries("deb", label = "Деб_зад", color = "blue") %>%
    dySeries("kred", label = "Кред_зад", color = "green") %>%
    dySeries("deb.1", label = "Деб_зад_план", color = "blue", strokePattern="dashed") %>%
    dySeries("kred.1", label = "Кред_зад_план", color = "green", strokePattern="dashed") %>%
    dySeries(c("lwr", "fit", "upr"), label = "Прибыль_прогноз", color = "red", strokePattern="dashed") %>%
    dyEvent("2018-03-31", "Прогноз", labelLoc = "bottom", color="red", strokePattern="dotted") %>%
    dyLegend(width = 300) %>%
    dyOptions(drawPoints=TRUE, pointSize=2, drawAxesAtZero=TRUE, labelsKMB=TRUE) %>%
    dyRangeSelector()
    
# Создаем каталог для хранения файлов в точке монтирования /mnt/webapp-files,
# соответствующей каталогу файлов веб-приложения.
requestDir <- '/mnt/webapp-files/${resourceName}/${requestId}'
dir.create(requestDir, recursive=TRUE)

# Сохраняем виджет с новым графиком в каталоге для публикации
graphPath <- paste(requestDir, 'prib_predicted.html', sep='/')
saveWidget(graph1, file=graphPath, selfcontained = TRUE)

df
 
