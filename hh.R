# Анализ рынка труда 1С

library('httr')
library('jsonlite')
library('ggplot2')

token <- 'CHANGE_ME'

experience <- c("noExperience", "between1And3", "between3And6", "moreThan6")

search_1c <- '1c%20NOT%20Bitrix';
search_java <- 'java';
search_javascript <- 'javascript';
search_php <- 'php';
search_csharp <- 'c#';
search_python <- 'python';
search_cplusplus <- 'c++';
search_go <- 'go';

getdata <- function(url) {
    out <- tryCatch({
      resp <- GET(url, add_headers(Authorization = sprintf("Bearer %s", token)))
      fromJSON(content(resp, as="text") )
    },
    error=function(cond) {
      message(cond)
      return(NULL)
    }
  )    
  return(out)
}

vacancies <- function(area, search) {
  result <- NULL
  
  for (exp in experience) {
    i <- 0
    pages <- -1
    
    while (i != pages) {
      url <- sprintf("https://api.hh.ru/vacancies?specialization=1&area=%d&experience=%s&page=%d&per_page=100&search_field=name&text='%s'",
                     area, exp, i, search)
      
      df <- getdata(url)
      if (is.null(df)) {
        break
      }
      
      pages <- df$pages
      if(pages == 0 || length(df$items) == 0 || class(df$items$salary) == "logical") {
        break
      }
      
      items <- df$items[,c('id','area','employer','name','salary')]
      items <- cbind(items[,c('id','name')], 
                       area_name=items$area$name, employer_name=items$employer$name, salary_currency=items$salary$currency, 
                       salary_gross=items$salary$gross, salary_from=items$salary$from, salary_to=items$salary$to, Опыт=exp)
        
     if (is.null(result)) {
        result <- items 
     } else {
        result <- rbind(result, items)
     }

      i <- i+1
    }
  }
  
  return(result)
}

moscow_1c <- vacancies(1, search_1c) 
spetersburg_1c <- vacancies(2, search_1c)

moscow_1c$lang <- "1C"
spetersburg_1c$lang <- "1C"

moscow_java <- vacancies(1, search_java)
spetersburg_java <- vacancies(2, search_java)

moscow_java$lang <- "Java"
spetersburg_java$lang <- "Java"

moscow_javascript <- vacancies(1, search_javascript)
spetersburg_javascript <- vacancies(2, search_javascript)

moscow_javascript$lang <- "JavaScript"
spetersburg_javascript$lang <- "JavaScript"

moscow_php <- vacancies(1, search_php)
spetersburg_php <- vacancies(2, search_php)

moscow_php$lang <- "PHP"
spetersburg_php$lang <- "PHP"

moscow_csharp <- vacancies(1, search_csharp)
spetersburg_csharp <- vacancies(2, search_csharp)

moscow_csharp$lang <- "C#"
spetersburg_csharp$lang <- "C#"

moscow_python <- vacancies(1, search_python)
spetersburg_python <- vacancies(2, search_python)

moscow_python$lang <- "Python"
spetersburg_python$lang <- "Python"

moscow_cplusplus <- vacancies(1, search_cplusplus)
spetersburg_cplusplus <- vacancies(2, search_cplusplus)

moscow_cplusplus$lang <- "C++"
spetersburg_cplusplus$lang <- "C++"

moscow_go <- vacancies(1, search_go)
spetersburg_go <- vacancies(2, search_go)

moscow_go$lang <- "Go"
spetersburg_go$lang <- "Go"

combined <- rbind(moscow_1c, spetersburg_1c, moscow_java, spetersburg_java, moscow_javascript, spetersburg_javascript, moscow_php, 
                  spetersburg_php, moscow_csharp, spetersburg_csharp, moscow_python, spetersburg_python, moscow_cplusplus,
                  spetersburg_cplusplus, moscow_go, spetersburg_go)

combined$salary <- with(combined, ifelse(is.na(combined$salary_to), combined$salary_from, combined$salary_to))

# Удалим явные выбросы и оклады в иностранной валюте (их немного)
combined <- subset(combined, salary <= 350000 & salary_currency == "RUR")

# Учитываем НДФЛ
combined$salary <- with(combined, ifelse(combined$salary_gross, combined$salary*0.87, combined$salary))

# Отключаем Научную нотацию чисел на диаграммах
options(scipen=999)

qplot(area_name, salary, data=subset(combined, lang=="1C"), geom=c("jitter", "boxplot"), alpha=I(0.8), colour=Опыт, 
      main="Диаграмма предложений на рынке труда 1С (Октябрь 2018)", xlab="", ylab="Заработная плата ('net')", varwidth = TRUE)    

qplot(lang, salary, data=combined, geom="boxplot", alpha=I(1), main="Сравнение предложений на рынке труда для разработчиков (Октябрь 2018)", 
      xlab="", ylab="Заработная плата ('net')", varwidth = TRUE, colour=area_name)
      
################## Анализ текста вакансий ##################

# Получаем полные описания вакансий (в поиске выдаются сокращенные)
vacancies_id <- rbind(moscow_1c, spetersburg_1c)$id

descriptions <- data.frame(id=character(), desc=character())
i <- 0
for (vid in vacancies_id) {
  url <- sprintf("https://api.hh.ru/vacancies/%s", vid)
  
  df <- getdata(url)
  if (is.null(df)) {
    next
  }
  
  descriptions <- rbind(descriptions, data.frame(id=vid, desc=gsub("<.*?>", "", df$description), stringsAsFactors=FALSE))
}

with(descriptions, {
  
  has_kd <- grepl("(1C:|1С:|\\s)КД\\W|Конвертация", desc, ignore.case = TRUE)
  has_erp <- grepl("(1С:|\\s)ERP\\W|Управление предприятием", desc, ignore.case = TRUE)
  has_uh <- grepl("(1C:|1С:|\\s)УХ\\W|Управление холдингом", desc, ignore.case = TRUE)

  has_bgu <- grepl("(1C:|1С:|\\s)БГУ\\W|Бухгалтерия государственного учреждения", desc, ignore.case = TRUE)
  has_bp <- grepl("(1C:|1С:|\\s)БП\\W|Бухгалтерия предприятия|Бухгалтерия", desc, ignore.case = TRUE)
  has_dok <- grepl("Документооборот", desc, ignore.case = TRUE)
  
  has_zikgu <- grepl("(1C:|1С:|\\s)ЗИКГУ\\W|Зарплата и кадры государственного учреждения", desc, ignore.case = TRUE)
  has_zup <- grepl("(1C:|1С:|\\s)ЗУП\\W|Зарплата и Управление Персоналом|(1C|1С):?\\s?Зарплата", desc, ignore.case = TRUE)
  has_ka <- grepl("(1C:|1С:|\\s)КА\\W|Комплексная автоматизация", desc, ignore.case = TRUE)
  
  has_unf <- grepl("(1C:|1С:|\\s)УНФ\\W|Управление нашей фирмой", desc, ignore.case = TRUE)
  has_rozn <- grepl("Розница", desc, ignore.case = TRUE)
  has_upp <- grepl("(1C:|1С:|\\s)УПП\\W|Управление производственным предприятием", desc, ignore.case = TRUE)
  has_ut <- grepl("(1C:|1С:|\\s)УТ\\W|Управление торговлей|Торговля", desc, ignore.case = TRUE)
  
  conf_count <<- data.frame(conf=character(), count=integer())
  
  conf_count <<- rbind(conf_count, data.frame(conf="1С:Конвертация данных", count=nrow(descriptions[has_kd==TRUE,])))
  conf_count <<- rbind(conf_count, data.frame(conf="1C:ERP Управление предприятием", count=nrow(descriptions[has_erp==TRUE,])))
  conf_count <<- rbind(conf_count, data.frame(conf="1С:Управление холдингом", count=nrow(descriptions[has_uh==TRUE,])))
  
  conf_count <<- rbind(conf_count, data.frame(conf="Бухгалтерия государственного учреждения", count=nrow(descriptions[has_bgu==TRUE,])))
  conf_count <<- rbind(conf_count, data.frame(conf="Бухгалтерия предприятия", count=nrow(descriptions[has_bp==TRUE,])))
  conf_count <<- rbind(conf_count, data.frame(conf="Документооборот", count=nrow(descriptions[has_dok==TRUE,])))
  
  conf_count <<- rbind(conf_count, data.frame(conf="Зарплата и кадры государственного учреждения", count=nrow(descriptions[has_zikgu==TRUE,])))
  conf_count <<- rbind(conf_count, data.frame(conf="Зарплата и Управление Персоналом", count=nrow(descriptions[has_zup==TRUE,])))
  conf_count <<- rbind(conf_count, data.frame(conf="Комплексная автоматизация", count=nrow(descriptions[has_ka==TRUE,])))
  
  conf_count <<- rbind(conf_count, data.frame(conf="Управление нашей фирмой", count=nrow(descriptions[has_unf==TRUE,])))
  conf_count <<- rbind(conf_count, data.frame(conf="Розница", count=nrow(descriptions[has_rozn==TRUE,])))
  conf_count <<- rbind(conf_count, data.frame(conf="Управление производственным предприятием", count=nrow(descriptions[has_upp==TRUE,])))
  conf_count <<- rbind(conf_count, data.frame(conf="Управление торговлей", count=nrow(descriptions[has_ut==TRUE,])))
  
})

ggplot(conf_count, aes(x = reorder(conf, count), y = count)) + geom_bar(stat='identity', position = 'dodge', fill = '#ffdd03', width=0.8) +
  coord_flip() + ylab('Количество вакансий') + xlab('') + ggtitle('Конфигурации фирмы "1С" по упоминаниям в вакансиях (Октябрь 2018)')



