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

# R.cache не позволяет явно задать время жизни кэша. Обходим это, создавая новый каталог для кэширования каждый день.
# Таким образом, время жизни кэша будет равняться суткам.
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

getVacancies <- function(exp, search, area.id) {
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
        
        if (!is.na(industry_list)) {
            names(industry_list) <- rep(c("industry"), length(industry_list))
            params_list <- append(params_list, industry_list)
        }
        
        if (!is.na(schedule_list)) {
            names(schedule_list) <- rep(c("schedule"), length(schedule_list))
            params_list <- append(params_list, schedule_list)
        }
        
        if (!is.na(employment_list)) {
            names(employment_list) <- rep(c("employment"), length(employment_list))
            params_list <- append(params_list, employment_list)
        }
        
        # Параметры, имеющие несколько значений для одних и тех же имён, надо установливать первыми
        #TODO Может ли params_list быть пустым?
        url <- modify_url(url, query = params_list)
        
        url <- modify_url(url, query = list(page=p, per_page=100))
        url <- modify_url(url, query = list(only_with_salary="true", no_magic="true", search_field="name"))
        url <- modify_url(url, query = list(text=search))
        url <- modify_url(url, query = list(area=area.id))
        
        if (!is.null(exp)) {
            url <- modify_url(url, query = list(experience=exp))
        }
      
        df <- getData(url)
      
        pages <- df$pages
        if(pages == 0 || length(df$items) == 0 || class(df$items$salary) == "logical") {
            break
        }
      
        items <- df$items[,c('id','alternate_url','area.name','employer.name','name','salary.currency',
            'salary.gross','salary.from','salary.to')]
        
        items$search <- factor(search)
        
        if (!is.null(exp)) {
            expval <- switch(exp, "noExperience"="Нет опыта", "between1And3"="От 1 года до 3 лет",
                "between3And6"="От 3 до 6 лет", "moreThan6"="Более 6 лет")
            items$experience <- factor(expval)
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

log("Начало выполнения")

# Проверка и обработка параметров

# Строка поиска
search_str <- trimws('${search}')
if (search_str == 'NA') {
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

# Тип занятости
employment_str <- '${employment}'
if (employment_str == 'NA') {
    employment_list <- NA
} else {
    if (!grepl("^(full|part|project|volunteer|probation)(,(full|part|project|volunteer|probation))*$", employment_str)) {
        stop(paste("Недопустимое значение параметра типа занятости:", employment_str, sep=" "))        
    } else {
        employment_list <- as.list(strsplit(employment_str, ",")[[1]]) 
    }
}

# Идентификаторы регионов
areaids_str <- '${areaids}'
if (areaids_str == 'NA') {
    stop("Не установлено значение параметра идентификаторов регионов")
} else {
    if (!grepl("^\\d+(,\\d+)*$", areaids_str)) {
        stop(paste("Недопустимое значение параметра индентификаторов регионов:", areaids_str, sep=" "));
    } else {
        areaids_list <- as.list(strsplit(areaids_str, ",")[[1]])    
    }
}

# Названия регионов
areanames_str <- '${areanames}'
if (areanames_str == 'NA') {
    stop("Не установлено значение параметра названий регионов")
} else {
    if (!grepl("^[ .А-Яа-я0-9()-]+(,[ .А-Яа-я0-9()-]+)*$", areanames_str)) {
        stop(paste("Недопустимое значение параметра названий регионов:", areanames_str, sep=" "));
    } else {
        areanames_list <- as.list(strsplit(areanames_str, ",")[[1]])    
    }
}

if (length(areaids_list) != length(areanames_list)) {
    stop("Количество идентификаторов не соответствует количеству названий регионов")
}

areas_list <- setNames(areaids_list, areanames_list)

# Объединить все регионы
groupareas <- as.logical('${groupareas}')
if (is.na(groupareas)) {
    groupareas <- FALSE
}

# Профобласть или специализация
specialization_str <- '${specialization}'
if (specialization_str == 'NA') {
    specialization_list <- NA
} else {
    if (!grepl("^\\d+(\\.\\d+)?(,\\d+(\\.\\d+)?)*$", specialization_str)) {
        # TODO Логирование ошибки ?
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
    limitmin <- FALSE
} else {
    limitmin <- as.numeric(limitmin_str)
    if (is.na(limitmin)) {
        stop(paste("Недопустимое значение параметра нижней границы:", limitmin_str, sep=" "))
    }
}
limitmax_str <- '${limitmax}'
if (limitmax_str == "NA") {
    limitmax <- FALSE
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

# Должна ли ширина ящиков зависеть от количества вакансий в соответствующей группе
boxwidth <- as.logical('${boxwidth}')
if (is.na(jitter)) {
    boxwidth <- FALSE
}

# Горизонтальные линии (отметки)
hlines_str <- '${hlines}'
if (hlines_str == 'NA') {
    hlines <- integer()
} else {
    if (!grepl("^\\d+(,\\d+)*$", hlines_str)) {
        stop(paste("Недопустимое значение параметра отметок:", hlines_str, sep=" "));
    } else {
        hlines <- as.integer(strsplit(hlines_str, ",")[[1]])    
    }
}

# Шаг делений вертикальной оси
breaksby_str <- '${breaksby}'
breaksby <- as.numeric(breaksby_str)
if (is.na(breaksby)) {
    stop(paste("Недопустимое значение параметра шага делений:", breaksby_str, sep=" "))
}

vacancies <- NULL

{ # XXX
for (name in names(areas_list)) {
    for (search in search_list) {
        if (expcolour) {
            experience <- c("noExperience", "between1And3", "between3And6", "moreThan6")    
            for (exp in experience) {
                result <- getVacancies(exp, search, areas_list[[name]]) 
                if (!is.null(result)) {
                    result$area.search_name <- factor(name)
                    if (is.null(vacancies)) {
                        vacancies <- result
                    } else {
                        vacancies <- rbind(vacancies, result)
                    }
                }
            }
        } else {
            result <- getVacancies(NULL, search, areas_list[[name]]) 
            if (!is.null(result)) {
                result$area.search_name <- factor(name)
                if (is.null(vacancies)) {
                    vacancies <- result
                } else {
                    vacancies <- rbind(vacancies, result)
                }
            }
        }
    }
}
} # XXX

if (length(vacancies) == 0) {
    # TODO Логировать количество вакансий
    stop("Отсутствуют элементы для отображения")
}

vacancies$salary <- with(vacancies, ifelse(is.na(vacancies$salary.to), vacancies$salary.from, vacancies$salary.to))

# Удаляем оклады в иностранной валюте
vacancies <- subset(vacancies, salary.currency == "RUR")

# Учитываем НДФЛ
vacancies$salary <- with(vacancies, ifelse(vacancies$salary.gross, vacancies$salary*0.87, vacancies$salary))

# Отключаем Научную нотацию чисел на диаграммах
options(scipen=999)

CairoFonts(regular='DejaVu Serif:style=Regular', bold='DejaVu Serif:style=Bold')

if (!groupareas) {
    if (expcolour) {
        aes <- aes(x=area.search_name, y=salary, colour=experience)
    } else {
        aes <- aes(x=area.search_name, y=salary)
    }
} else {
    if (expcolour) {
        aes <- aes(x=areanames_str, y=salary, colour=experience)
    } else {
        aes <- aes(x=areanames_str, y=salary)
    }
}

plot <- ggplot(vacancies, mapping=aes)

if (jitter) {
    plot <- plot + geom_jitter() 
    # Отключаем изображение выбросов, чтобы они не дублировали точки вакансий
    plot <- plot + geom_boxplot(varwidth = boxwidth, outlier.colour = NA, alpha = 0.8)
} else {
    plot <- plot + geom_boxplot(varwidth = boxwidth, outlier.colour = I("gray"), alpha = 0.8)
}

plot <- plot + facet_grid(. ~ search) + labs(title=maintitle, x="", y="Заработная плата (нетто)", color="Опыт работы") +
    theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), legend.text=element_text(size = 10),
        legend.title=element_text(size = 10, face="bold"), strip.text.x=element_text(size = 10, face="bold"))
    
if (limitmin || limitmax) {
    plot <- plot + coord_cartesian(ylim = c(limitmin, limitmax))    
}

# Выводим отметки и задаём шаг делений вертикальной оси
plot <- plot + geom_hline(yintercept=hlines, color="gray", linetype=5) + 
            scale_y_continuous(breaks=sort(c(seq(0,max(vacancies$salary, na.rm = TRUE),by=breaksby), hlines)))

# TODO Закомментировать
print(plot)
dev.off()

requestDir <- '/mnt/webapp-files/${resourceName}/${requestId}'
dir.create(requestDir, recursive=TRUE)

chartFile <- paste(requestDir, 'chart.png', sep='/')
CairoPNG(chartFile, width=plotwidth, height=plotheight)
print(plot)
dev.off()

vacancies

