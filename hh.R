# Анализ рынка труда по данным HeadHunter
# @author Kirill Mikhaylov

library(httr)
library(jsonlite)
library(ggplot2)
library(R.cache)
library(Cairo)

Sys.setenv(TZ = "Europe/Moscow")
resourceHostDir <- '/mnt/host/${resourceName}'
dir.create(resourceHostDir)
logFile <- paste(resourceHostDir, paste('log_', Sys.Date(), '.txt', sep=''), sep='/')

# R.cache не позволяет явно задать время жизни кэша. 
# Обходим это, создавая новый каталог для кэширования каждый день. Таким образом, время жизни кэша будет равняться суткам.
cacheDir <- paste('/tmp/Rcache/${resourceName}', Sys.Date(), sep='/')
dir.create(cacheDir, recursive=TRUE)
setCacheRootPath(path=cacheDir)

log <- function(message) {
    write(paste(Sys.time(), message, sep=" "), file=logFile, append=TRUE)
}

getData <- function(url) {
    out <- tryCatch({
      key <- as.list(url)
      resp <- loadCache(key, onError=c("quiet"))
      if (is.null(resp)) {
        log(url)
        Sys.sleep(0.5)
        resp_txt <- GET(url, user_agent("DataReducer/dev (admin@datareducer.ru)"))
        resp <- fromJSON(content(resp_txt, as="text"), flatten=TRUE)
        saveCache(resp, key)
      } else {
        log(paste(url, "(Получено из кэша)", sep=" "))
      }
      resp
    },
    error=function(cond) {
        log(cond)
        stop(cond)
    },
    warning=function(cond) {
        log(cond)
        stop(cond)
    })
    return(out)
}

getVacancies <- function(exp, search) {
    result <- NULL

    p <- 0
    pages <- -1
    while (p != pages) {
        url <- "https://api.hh.ru/vacancies";
        
        params_list <- list()
        
        if (!is.na(specialization_list)) {
            names(specialization_list) <- rep(c("specialization"), length(specialization_list))
            params_list <- append(params_list, specialization_list)
        }
        
        if (!is.na(area_list)) {
            names(area_list) <- rep(c("area"), length(area_list))
            params_list <- append(params_list, area_list)
        }
        
        if (!is.na(industry_list)) {
            names(industry_list) <- rep(c("industry"), length(industry_list))
            params_list <- append(params_list, industry_list)
        }
        
        if (!is.na(schedule_list)) {
            names(schedule_list) <- rep(c("schedule"), length(schedule_list))
            params_list <- append(params_list, schedule_list)
        }
        
        # Параметры, имеющие несколько значений для одних и тех же имён, надо установливать первыми
        url <- modify_url(url, query = params_list)
        
        url <- modify_url(url, query = list(page=p, per_page=100))
        url <- modify_url(url, query = list(only_with_salary="true", no_magic="false", search_field="name"))
        url <- modify_url(url, query = list(text=search))
        
        if (!is.null(exp)) {
            url <- modify_url(url, query = list(experience=exp))
        }
      
        df <- getData(url)
      
        pages <- df$pages
        if(pages == 0 || length(df$items) == 0 || class(df$items$salary) == "logical") {
            break
        }
      
        items <- df$items[,c('id','alternate_url','area.name','employer.name','name','salary.currency','salary.gross','salary.from','salary.to')]
        
        items$search <- factor(search)
        
        if (!is.null(exp)) {
            items$experience <- factor(exp)
        }
        
        if (is.null(result)) {
            result <- items 
        } else {
            result <- rbind(result, items)
        }
        p <- p+1
    }
  
  return(result)
}

log("========== Начало выполнения ==========")

# Проверка и обработка параметров

# Строка поиска
search_str <- trimws('${search}')
if (search_str == '' || search_str == 'NA') {
    stop("Не установлено значение строки поиска")
}
search_list <- as.list(strsplit(search_str, ",")[[1]]) 

# График работы
schedule_str <- '${schedule}'
if (schedule_str == 'NA') {
    schedule_list <- NA
} else {
    if (!grepl("^(fullDay|shift|flexible|remote|flyInFlyOut)(,(fullDay|shift|flexible|remote|flyInFlyOut))*$", schedule_str)) {
        stop(paste("Недопустимое значение параметра графика работы:", schedule_str, sep=" "))        
    } else {
        schedule_list <- as.list(strsplit(schedule_str, ",")[[1]]) 
    }
}

# Регион
area_str <- '${area}'
if (area_str == 'NA') {
    area_list <- NA
} else {
    if (!grepl("^\\d+(,\\d+)*$", area_str)) {
        stop(paste("Недопустимое значение параметра региона:", area_str, sep=" "));
    } else {
        area_list <- as.list(strsplit(area_str, ",")[[1]])    
    }
}

# Профобласть или специализация
specialization_str <- '${specialization}'
if (specialization_str == 'NA') {
    specialization_list <- NA
} else {
    if (!grepl("^\\d+(\\.\\d+)?(,\\d+(\\.\\d+)?)*$", specialization_str)) {
        stop(paste("Недопустимое значение параметра специализации:", specialization_str, sep=" "));
    } else {
        specialization_list <- as.list(strsplit(specialization_str, ",")[[1]])    
    }   
}

# Индустрия компании
industry_str <- '${industry}'
if (industry_str == 'NA') {
    industry_list <- NA
} else {
    if (!grepl("^\\d+(\\.\\d+)?(,\\d+(\\.\\d+)?)*$", industry_str)) {
        stop(paste("Недопустимое значение параметра индустрии компании:", industry_str, sep=" "));
    } else {
        industry_list <- as.list(strsplit(industry_str, ",")[[1]])    
    }   
}

# Группировка по опыту
expcolour <- as.logical('${expcolour}')
if (is.na(expcolour)) {
    expcolour <- FALSE
}

# Вывод точек вакансий (диаграмма рассеяния)
jitter <- as.logical('${jitter}')
if (is.na(jitter)) {
    jitter <- FALSE
}

# Параметры границ
limitmin_str <- '${limitmin}'
if (limitmin_str == "NA") {
    limitmin <- NA
} else {
    limitmin <- as.numeric(limitmin_str)
    if (is.na(limitmin)) {
        stop(paste("Недопустимое значение параметра нижней границы:", limitmin_str, sep=" "))
    }
}
limitmax_str <- '${limitmax}'
if (limitmax_str == "NA") {
    limitmax <- NA
} else {
    limitmax <- as.numeric(limitmax_str)
    if (is.na(limitmax)) {
        stop(paste("Недопустимое значение параметра верхней границы:", limitmax_str, sep=" "))
    }
}

# Заголовок диаграммы
maintitle <- trimws('${title}')

# Размеры диаграммы
plotwidth_str <- '${plotwidth}'
plotwidth <- as.numeric(plotwidth_str)
if (is.na(plotwidth)) {
    stop(paste("Недопустимое значение параметра ширины диаграммы:", plotwidth_str, sep=" "))
}
plotheight_str <- '${plotheight}'
plotheight <- as.numeric(plotheight_str)
if (is.na(plotheight)) {
    stop(paste("Недопустимое значение параметра высоты диаграммы:", plotheight_str, sep=" "))
}

# Ширина ящиков зависит от количества вакансий в соответствующей группе
boxwidth <- as.logical('${boxwidth}')
if (is.na(jitter)) {
    boxwidth <- FALSE
}

vacancies <- NULL

{ # XXX Цикл, не вложенный в фигурные скобки, приводит к ошибке RServe
for (search in search_list) {
    if (expcolour) {
        experience <- c("noExperience", "between1And3", "between3And6", "moreThan6")    
        for (exp in experience) {
            result <- getVacancies(exp, search) 
            if (is.null(vacancies)) {
                vacancies <- result
            } else {
                vacancies <- rbind(result, vacancies)
            }
        } 
    } else {
        result <- getVacancies(NULL, search) 
        if (is.null(vacancies)) {
            vacancies <- result
        } else {
            vacancies <- rbind(result, vacancies)
        }
    }
}
} # XXX

if (length(vacancies) == 0) {
    stop("Отсутствуют элементы для отображения")
}

vacancies$salary <- with(vacancies, ifelse(is.na(vacancies$salary.to), vacancies$salary.from, vacancies$salary.to))

# Удаляем оклады в иностранной валюте
vacancies <- subset(vacancies, salary.currency == "RUR")

# Учитываем НДФЛ
vacancies$salary <- with(vacancies, ifelse(vacancies$salary.gross, vacancies$salary*0.87, vacancies$salary))

# Отключаем Научную нотацию чисел на диаграммах
options(scipen=999)

CairoFonts(regular='DejaVu Serif:style=Regular')

if (!is.na(area_list)) {
    if (expcolour) {
        aes <- aes(x=area.name, y=salary, colour=experience)
    } else {
        aes <- aes(x=area.name, y=salary)
    }
} else {
    if (expcolour) {
        aes <- aes(x="Все регионы", y=salary, colour=experience)
    } else {
        aes <- aes(x="Все регионы", y=salary)
    }
}

plot <- ggplot(vacancies, mapping=aes)

if (jitter) {
    plot <- plot + geom_jitter() 
    # Отключаем изображение выбросов, чтобы они не дублировали точки вакансий
    plot <- plot + geom_boxplot(varwidth = boxwidth, outlier.colour = NA, alpha = 0.8)
} else {
    plot <- plot + geom_boxplot(varwidth = boxwidth, outlier.colour = I("red"), alpha = 0.8)
}

plot <- plot + facet_grid(. ~ search) + labs(title=maintitle, x="", y="Заработная плата (нетто)", color="Опыт работы")
            
if (!is.na(limitmin) || !is.na(limitmax)) {
    plot <- plot + ylim(limitmin, limitmax)
}

print(plot)
dev.off()

requestDir <- '/mnt/webapp-files/${resourceName}/${requestId}'
dir.create(requestDir, recursive=TRUE)

chartFile <- paste(requestDir, 'chart.png', sep='/')
CairoPNG(chartFile, width=plotwidth, height=plotheight)
print(plot)
dev.off()

vacancies
