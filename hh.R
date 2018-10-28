# Анализ рынка труда по данным HeadHunter

library('httr')
library('jsonlite')
library('ggplot2')
library('R.cache')

experience <- c("noExperience", "between1And3", "between3And6", "moreThan6")

search_1c <- '1c+NOT+Bitrix';
search_java <- 'java';
search_javascript <- 'javascript';
search_php <- 'php';
search_csharp <- 'c#';
search_python <- 'python';
search_cplusplus <- 'c++';
search_go <- 'go';

getdata <- function(url) {
    out <- tryCatch({
      key <- as.list(url)
      resp <- loadCache(key)
      if (is.null(resp)) {
        Sys.sleep(0.5)
        resp_txt <- GET(url)
        resp <- fromJSON(content(resp_txt, as="text"), flatten=TRUE)
        saveCache(resp, key)
      }
      resp
    },
    error=function(cond) {
      print(cond)
      return(NULL)
    },
    warning=function(cond) {
      print(cond)
      return(NULL)
    })
    return(out)
}

vacancies <- function(area, search) {
  result <- NULL
  
  for (exp in experience) {
    page <- 0
    pages <- -1
    
    while (page != pages) {
      url <- sprintf("https://api.hh.ru/vacancies?specialization=1&area=%d&experience=%s&page=%d&per_page=100&search_field=name&text=%s",
                     area, exp, page, search)
      
      df <- getdata(url)
      if (is.null(df)) {
        stop("Сетевая ошибка")
      }
      
      pages <- df$pages
      if(pages == 0 || length(df$items) == 0 || class(df$items$salary) == "logical") {
        break
      }
      
     items <- df$items[,c('id','area.name','employer.name','name','salary.currency','salary.gross','salary.from','salary.to')]
     items$experience <- factor(exp)
        
     if (is.null(result)) {
        result <- items 
     } else {
        result <- rbind(result, items)
     }

      page <- page+1
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

combined$salary <- with(combined, ifelse(is.na(combined$salary.to), combined$salary.from, combined$salary.to))

# Удалим явные выбросы и оклады в иностранной валюте (их немного)
combined <- subset(combined, salary <= 350000 & salary.currency == "RUR")

# Учитываем НДФЛ
combined$salary <- with(combined, ifelse(combined$salary.gross, combined$salary*0.87, combined$salary))

# Отключаем Научную нотацию чисел на диаграммах
options(scipen=999)

p <- qplot(area.name, salary, data=subset(combined, lang=="1C"), geom=c("jitter", "boxplot"), alpha=I(0.8), colour=experience, 
      main="Диаграмма предложений на рынке труда 1С", xlab="", ylab='Заработная плата ("на руки")', varwidth = TRUE)
p

#p + geom_hline(aes(yintercept=160000)) + geom_text(aes(0,160000,label = 'Тест', vjust = -0.5, hjust=-0.5), inherit.aes=FALSE)

qplot(lang, salary, data=combined, geom="boxplot", alpha=I(1), main="Сравнение предложений на рынке труда для разработчиков", 
      xlab="", ylab="Заработная плата ('net')", varwidth = TRUE, colour=area.name)
      