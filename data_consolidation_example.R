 # Обрабатываем данные информационной базы ООО "УноФарма".

# Создаём таблицу данных, выбирая нужные нам поля набора данных "AccountingRegister_Хозрасчетный_Turnover1" 
# (соответствующего виртуальной таблице оборотов регистра бухгалтерии "Хозрасчетный").

ib1 <- AccountingRegister_Хозрасчетный_Turnover1[c('ExtDimension1____Presentation',
	 'ExtDimension1_Type', 'ExtDimension2____Presentation', 'ExtDimension2_Type', 'СуммаTurnoverDr')]

# Статья движения д/с может быть записана в разные реквизиты - Субконто1 или Субконто2.
# Разбиваем данные на два набора, соответствующих первому и второму случаю.
# Затем, объединяя эти наборы, получаем таблицу данных с отдельным столбцом статей движения д/с.

ib1_1 <- subset(ib1, ib1$ExtDimension1_Type == 'StandardODATA.Catalog_СтатьиДвиженияДенежныхСредств',
	 select=c(ExtDimension1____Presentation, СуммаTurnoverDr))

ib1_2 <- subset(ib1, ib1$ExtDimension2_Type == 'StandardODATA.Catalog_СтатьиДвиженияДенежныхСредств',
	 select=c(ExtDimension2____Presentation, СуммаTurnoverDr))

fact1 <- merge(ib1_1, ib1_2, by.x=c('ExtDimension1____Presentation', 'СуммаTurnoverDr'),
	 by.y=c('ExtDimension2____Presentation', 'СуммаTurnoverDr'), all = TRUE)

# Выполняем те же операции для информационной базы ООО "ПортоФарма".

ib2 <- AccountingRegister_Хозрасчетный_Turnover2[c('ExtDimension1____Presentation',
	 'ExtDimension1_Type', 'ExtDimension2____Presentation', 'ExtDimension2_Type', 'СуммаTurnoverDr')]

ib2_1 <- subset(ib2, ib2$ExtDimension1_Type == 'StandardODATA.Catalog_СтатьиДвиженияДенежныхСредств',
	 select=c(ExtDimension1____Presentation, СуммаTurnoverDr))

ib2_2 <- subset(ib2, ib2$ExtDimension2_Type == 'StandardODATA.Catalog_СтатьиДвиженияДенежныхСредств',
	 select=c(ExtDimension2____Presentation, СуммаTurnoverDr))

fact2 <- merge(ib2_1, ib2_2, by.x=c('ExtDimension1____Presentation', 'СуммаTurnoverDr'),
	 by.y=c('ExtDimension2____Presentation', 'СуммаTurnoverDr'), all = TRUE)

# Объединяем данные двух информационных баз и суммируем значения поступлений по статьям.

fact <- merge(fact1, fact2, , all=TRUE)

if (nrow(fact) > 0) fact <- aggregate(fact[, 2], by=list(fact[, 1]), sum)

# Переименовываем столбцы.

names(fact)[1] <- 'СтатьяДДС'
names(fact)[2] <- 'Факт'

# Считываем фактические данные поступлений д/с из файла Excel.
# Файл расположен в каталоге, примонтированном к Docker-контейнеру сервиса Rserve в точке /mnt/host.

library(xlsx)
plan <- read.xlsx('/mnt/host/План_1кв.xlsx', 1)

# Объединяем таблицы плановых и фактических данных.

pf <- merge(plan, fact, by=1, all=TRUE)

# Рассчитываем абсолютные и относительные отклонения фактических значений от плановых
# и помещаем результаты в новые колонки.

pf$ОтклонениеАбс <-  pf$Факт - pf$План
pf$ОтклонениеОтн <-  round((pf$Факт - pf$План) / pf$Факт, digits = 2)

# Подготавливаем данные для построения диаграммы.

# Приводим таблицу данных к нужному формату 
# (функция melt() преобразует данные в такой формат, что переменные "План" и "Факт"
# располагаются в собственных строках вместе с переменными "СтатьяДДС", их идентифицирующими.

library(reshape2)
names(pf)[1] <- 'СтатьяДДС'
pf.m <- melt(pf[,1:3], id.var='СтатьяДДС')

# Для экономии места на диаграмме разбиваем названия статей по словам.

pf.m$СтатьяДДС <- gsub("\\s","\n", pf.m$СтатьяДДС)

# Стоим столбчатую диаграмму. Указываем вид диаграммы - с группировкой, 
# горизонтальное расположение и размер шрифта.

library(ggplot2)
chart <- ggplot(pf.m, aes(x = СтатьяДДС, y = value, fill = variable)) + geom_bar(stat='identity', position = 'dodge') +
	coord_flip() + theme(text = element_text(size=13))

# Устанавливаем шрифт, используемый в диаграмме.
# Шрифт должен быть установлен в системе (в Docker-контейнере сервиса Rserve)

CairoFonts(regular='Free Helvetian:style=Regular')

# Выводим диаграмму на экран.

print(chart)
dev.off()

# Создаем каталог для хранения файлов в точке монтирования /mnt/webapp-files,
# соответствующей каталогу файлов веб-приложения.

requestDir <- '/mnt/webapp-files/${resourceName}/${requestId}'
dir.create(requestDir, recursive=TRUE)

# Записываем диаграмму в файл.

chartFile <- paste(requestDir, 'chart.png', sep='/')
CairoPNG(chartFile, width =520, height=360)
print(chart)
dev.off()

# Записываем таблицу данных в файл Excel.

xlsxFile <- paste(requestDir, 'ПланФакт.xlsx', sep='/')
write.xlsx(pf, xlsxFile, row.names = FALSE)

# Выводим таблицу данных.
pf
