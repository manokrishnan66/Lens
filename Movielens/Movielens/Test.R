library(tidyverse)
library(dslabs)
library(broom)
library(caret)
install.packages("htmlwidgets")
install.packages("textdata")
getwd()
dir <-"/R_Projects/Thrive_mano/Projects" 
setwd(dir)
# ---Importing a datafile from dslabs

p <- system.file("extdata", package="dslabs")
list.files(p)

# -- getting fullpath
filename <- "murders.csv"
fullpath <- file.path(p,filename)
fullpath

#-- copying to working directory
file.copy(fullpath,getwd())
file.exists(filename)

#---read files
library(tidyverse)
library(readxl)
read_line("murders.csv",n_max=3)
'??readlines'
dat <- read.csv(fullpath,stringsAsFactors = FALSE)
class(dat$state)

#-----Download from Internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

#---test
library(tidyverse)
install.packages("dplyr")
library(dplyr)
urlt <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
datat <- read_csv(urlt,col_names = FALSE)
str(datat)

url <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_annmean_mlo.txt"
co2_mauna_loa <- read_table(url,col_names = TRUE,skip = 56)
head(co2_mauna_loa)
nrow(co2_mauna_loa %>% select(year, mean, unc))

#-reshaping test
d <- read.csv("E:/R_Projects/Thrive_mano/Data/time.csv",stringsAsFactors = FALSE)
tidy_data <- d %>%
  gather(year, time, `X2015`:`X2017`)
tidy_data %>% spread(year, time)
tidy_data %>% spread(time, year, `X2015`:`X2017`)
dis <- read.csv("E:/R_Projects/Thrive_mano/Data/disease.csv",stringsAsFactors = FALSE)
dat_tidy <- dis %>%
  gather(key = disease, value = count, HepatitisA:Rubella)

dis_m <- read.csv("E:/R_Projects/Thrive_mano/Data/time_merge.csv",stringsAsFactors = FALSE)
tidy_data_m <- dis_m %>%
  gather(key = 'key', value = 'value', -age_group) %>%
  separate(col = key, into = c('year', 'variable_name'), sep = '_') %>% 
  spread(key = variable_name, value = value)


data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany") & !is.na(fertility)) %>%
  select(country, year, fertility)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)
dis_height <- read.csv("E:/R_Projects/Thrive_mano/Data/height.csv",stringsAsFactors = FALSE)
tidy_data <- dis_height %>%
separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge")%>%
spread(key = variable_name, value = value)

co2

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- co2_wide %>%
  gather(month,co2,-year)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants) %>%
  spread(gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp2 <- unite(tmp, column_name, c(key, gender))

tmp2 %>%  spread(column_name, value)


tab1 <- data.frame(state = c('Alabama','Alaska','Arizona','Delaware','District of Columbia'), population = c(4779736,710231,6392017,897934,601723))

tab2 <- data.frame(state = c('Alabama','Alaska','Arizona','California','Colorado','Connecticut'), electoral_votes = c(9,3,11,55,9,7))

dim(tab1)

dim(tab2)
dat <- left_join(tab1, tab2, by = 'state')
dim(dat)
  dat <- semi_join(tab1, tab2, by = 'state')
  dat


  dat <- bind_cols(tab1,tab2)
  
  df1 <- data.frame(x = c('a','b'), y = c('a','a'))
  df2 <- data.frame(x = c('a','a'), y = c('a','b'))
  df1
  df2
  
  final <- intersect(df1, df2)
  final
  
  final <- union(df1, df2)
  final
  
  final <- setdiff(df1, df2)
  final
  
  final <- setequal(df1, df2)
  final
  
  install.packages("Lahman")
  library(Lahman)
  
  top <- Batting %>% 
    filter(yearID == 2016) %>%
    arrange(desc(HR)) %>%    # arrange by descending HR count
    slice(1:10)    # take entries 1-10
  top %>% as_tibble()
  
  Master %>% as_tibble()
  
  top_names <- top %>% left_join(Master) %>%
    select(playerID)
  
  Salaries
  
  top_salary <- Salaries %>% filter(yearID == 2016) %>%
    right_join(top_names)%>%
    select(nameFirst, nameLast, teamID, HR, salary)
  
  award_16 <- AwardsPlayers %>% filter(yearID == 2016) %>% select(playerID)
  top_names %>% left_join(award_16)
  
  intersect(top_names,award_16)
  setdiff(award_16,top_names)
  
  
  #--------Webscraping--------------
  library(rvest)
  url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
  h <- read_html(url)
  class(h)
  h
  tab <- h %>% html_nodes("table")
  html_text(tab[[20]])
  html_table(tab[[20]])
  html_table(tab[[length(tab)-1]])
  tab_1 <-html_table(tab[[10]])
  tab_1 <- tab_1[-1,]
  tab_1 <- tab_1[,-1]
  names(tab_1) <- c("Team", "Payroll", "Average")
  tab_2 <-html_table(tab[[19]])
  tab_2 <- tab_2[-1,]
  names(tab_2) <- c("Team", "Payroll", "Average")
  tab_1 %>% full_join(tab_2,by = 'Team')
  
  
  library(rvest)
  library(tidyverse)
  url <-"https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
  href <- read_html(url)
  ref <- href %>% html_nodes("table")
  html_table(ref[[6]],fill=TRUE)
  length(ref)
  
  head(dat)
  
  dot <- data.frame(month = c("January"), Sales = c("$128,568"), Profit = c("$16,234"))
  
  class(dot$Sales)
  dot %>% mutate_at(2:3, parse_number)
  dot %>% mutate_at(2:3, as.numeric)
  dot %>% mutate_at(2:3, funs(str_replace_all(., c("\\$|,"), ""))) %>% 
    mutate_at(2:3, as.numeric)
  
  
  yes <- c("5", "6", "5'10", "5 feet", "4'11")
  no <- c("", ".", "Five", "six")
  s <- c(yes, no)
  pattern <- "\\d"
  # [56] means 5 or 6
  str_view(s, "[56]")
  # [4-7] means 4, 5, 6 or 7
  yes <- as.character(4:7)
  no <- as.character(1:3)
  s <- c(yes, no)
  str_detect(s, "[4-7]")
  # ^ means start of string, $ means end of string
  pattern <- "^\\d$"
  yes <- c("1", "5", "9")
  no <- c("12", "123", " 1", "a4", "b")
  s <- c(yes, no)
  str_view(s, pattern)
  
  pattern <- "^\\d{1,2}$"
  yes <- c("1", "5", "9", "12")
  no <- c("123", "a4", "b")
  str_view(c(yes, no), pattern)
  
  pattern <- "^[4-7]'\\d{1,2}\"$"
  yes <- c("5'7\"", "6'2\"",  "5'12\"")
  no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
  str_detect(yes, pattern)
  str_detect(no, pattern)
  
  
  pattern <- ","
  yes <- c("180 cm", "70 inches")
  no <- c("180", "70''")
  s <- c(yes, no)
  str_detect(s, "cm") | str_detect(s, "inches")
  str_detect(s, "cm|inches")
  # highlight the first occurrence of a pattern
  str_view(s, pattern)
  # highlight all instances of a pattern
  str_view_all(s, pattern)
  
  
  pattern_without_groups <- "^[4-7],\\d*$"
  pattern_with_groups <-  "^([4-7]),(\\d*)$"
  yes <- c("5,9", "5,11", "6,", "6,1")
  no <- c("5'9", ",", "2,8", "6.1.1")
  s <- c(yes, no)
  
  str_detect(s, pattern_without_groups)
  str_detect(s, pattern_with_groups)
  
  str_match(s, pattern_with_groups)
  str_extract(s, pattern_with_groups)
  
  
  not_inches <- function(x, smallest = 50, tallest = 84) {
    inches <- suppressWarnings(as.numeric(x))
    ind <- is.na(inches) | inches < smallest | inches > tallest 
    ind
  }
  
  not_inches <- function(x, smallest = 50, tallest = 84) {
    inches <- suppressWarnings(as.numeric(x))
    ind <- is.na(inches) | inches < smallest | inches > tallest 
    ind
  }
  
  s <- c("70","5 ft","4'11","",".","Six feet")
  pattern <- "\\d\\d|ft"
  str_view_all(s, pattern)
  
  animals <- c("cat", "puppy", "Moose", "MONKEY")
  pattern <- "[a-z]{4,5}"
  str_detect(animals, pattern)
  
  animals <- c("moose", "monkey", "meerkat", "mountain lion")
  pattern <- 'mo*'
  pattern <- 'mo?'
  pattern <- 'mo+'
  pattern <- 'moo*'
  str_detect(animals, pattern)
  
  schools <- c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts","University Georgia","U California","California State University")
  
  schools %>% 
    str_replace("Univ\\.?|U\\.?", "University ") %>% 
    str_replace("^University of |^University ", "University of ")
  
  schools %>% 
    str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
    str_replace("^University of |^University ", "University of ")
  
  problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
  pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
  pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
  str_replace(problems, pattern_with_groups, "\\1'\\2")
  
  yes <- c("5 feet 7inches", '5 7')
  no <- c("5ft 9 inches", "5 ft 9 inches")
  s <- c(yes, no)
  
  converted <- s %>% 
    str_replace("feet|foot|ft", "'") %>% 
    str_replace("inches|in|''|\"", "") %>% 
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

  
  pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
  str_detect(converted, pattern)
  data(gapminder)
  mute <- gapminder %>% filter(region=="Caribbean") %>%
    mutate(country = recode(country, 
                            'Antigua and Barbuda'="Barbuda",
                            'Dominican Republic' = "DR",
                            'St. Vincent and the Grenadines' = "St. Vincent",
                            'Trinidad and Tobago' = "Trinidad")) %>%
    ggplot(aes(year, life_expectancy, color = country)) +
    geom_line()
  
    schedule <- data.frame(day = c('Monday', 'Tuesday'), staff = c('Mandy, Chris and Laura','Steve, Ruth and Frank'))
  
  str_split(schedule$staff, ", | and ")
  str_split(schedule$staff, ",\\s|\\sand\\s")
  
  
  tidy <- schedule %>% 
    mutate(staff = str_split(staff, ", | and ")) %>% 
    unnest()
  
  tidy <- schedule %>% 
    mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
    unnest()
  
  library(rvest)
  library(tidyverse)
  library(stringr)
  library(lubridate)
  url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
  tab <- read_html(url) %>% html_nodes("table")
  polls <- tab[[7]] %>% html_table(fill = TRUE)
  str(polls)
  polls <- polls[-1,]
  head(polls)
  polls[[1]]
  names(polls) <- c("dates", "remain", "leave", "undecided",  "samplesize", "pollster","notes", "poll_type", "lead")
  str_detect(polls$undecided, 'NA')
  polls %>% filter (str_detect(polls$remain, "%"))
  parse_number(polls$remain)/100
  
  temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]{3,5}")
  end_date <- sapply(temp, function(x) x[length(x)])
  
  dates <- c("09-01-02", "01-12-07", "02-03-04")
  #ymd(dates)
  #mdy(dates)
  dmy(dates)
  
  library(dslabs)
  data(brexit_polls)
  head(brexit_polls)
  str(brexit_polls$startdate)
  brexit_polls %>% filter(month(brexit_polls$startdate) ==4)
  
  brexit_polls %>% round_date(brexit_polls$enddate,unit = "week",week_start = getOption("lubridate.week.start",7))
  
  nrow(brexit_polls %>% mutate( rounded =round_date(ymd(brexit_polls$enddate), "week")) %>% filter (rounded < ymd("2016-06-13")))
  
  sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")
  
  brexit_polls %>% mutate(weekday = weekdays(brexit_polls$enddate, abbreviate = FALSE)) %>% filter(weekday =="Thursday")
  
  data(movielens)
  
  table(year(as_datetime(movielens$timestamp)))
  
  table(hour(as_datetime(movielens$timestamp)))  
  
  install.packages("gutenbergr")
  install.packages("tidytext")
  library(tidyverse)
  library(gutenbergr)
  library(tidytext)
  options(digits = 3)
  
  head(gutenberg_metadata)
  str(gutenberg_metadata)
  gutenberg_metadata %>% filter (str_detect(gutenberg_metadata$title, 'Pride and Prejudice')) %>% select (gutenberg_id,title)
  gutenberg_works(title == 'Pride and Prejudice') %>% select (gutenberg_id,title)
  
  
    words <- gutenberg_download(1342) %>%
    unnest_tokens(word, text)
    
    check <- words %>% anti_join(stop_words)
     words %>% stop_words(1342)
  
     words %>%
       count(word) %>%
       top_n(1, n) %>%
       pull(n)
     
  word_counts <-data.frame(words %>%
    anti_join(stop_words, by = "word") %>%
    count(word, sort = TRUE))
  
  afinn <- get_sentiments("afinn")
  
  afinn_sentiments <- word_counts%>%
    inner_join(afinn)
  
  nrow(afinn_sentiments %>% filter(value ==4))
  
  
  #---------------
  book <- gutenberg_download(1342)
  words <- book %>%
    unnest_tokens(word, text)
  
  words <- words %>% anti_join(stop_words)
  
  words <- words %>%
    filter(!str_detect(word, "\\d"))
  
  afinn <- get_sentiments("afinn")
  afinn_sentiments <- words %>% inner_join(afinn, by = "word")
  nrow(afinn_sentiments)
  nrow(afinn_sentiments %>% filter(str_detect(afinn_sentiments$value, "\\-")))
  nrow(afinn_sentiments %>% filter(value < 0))
  mean(afinn_sentiments$value > 0)
  
  
  
  #-------Final assessment in Wrangling------
  install.packages("pdftools")
  library(tidyverse)
  library(pdftools)
  options(digits = 3)
  fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
  system("cmd.exe", input = paste("start", fn))
  
  #1me11
  #865404030660581
  #865404030660599
  
  library(pdftools)
  pdf_file <- "https://github.com/ropensci/tabulizer/raw/master/inst/examples/data.pdf"
  txt <- pdf_text(fn)
  str(x)
  cat(txt[9])
  x <- str_split(txt[9],'\n')
  length(x)
  
  s <- x[[1]]
  class(s)
  
  s <- str_trim(s, "both")
  
 
  header_index <- str_which(s, "2015")
  
  header_index <- str_which(s, "2015")[1]
  header <- s[header_index]
  str_split(header,'\\s{2,}',simplify=TRUE)
  
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  month
  
  tail_index  <- str_which(s, "Total")
  tail_index
  
  n <- str_count(s,"\\d+")
  str_which(n, 1)
  
  
  s <- s[-(header_index-1)]
  
  n <- str_count(s,"\\d+")
  
  s[1]
  s[7]
  
  s <- s[-5]
  s <- s[-7]
  
  str(s)
  
  out <- c(1:header_index, which(n==1), tail_index:length(s))
  s <- s[-out]
  length(s)
  
  s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
  colnames(s) <- c('day', header)
  class(s)
  
  tab <- s %>% 
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate_all(as.numeric)
  mean(tab$"2015")
  
  tab <- tab %>% gather(year, deaths, -day) %>%
    mutate(deaths = as.numeric(deaths))
  tab
  
  tab %>% filter(!(year %in% (2018)) ) %>% ggplot(aes(day, deaths, color=year)) + 
          geom_line() + ylim(0, 110)
  
  
  tab %>% filter(year < 2018) %>% 
    ggplot(aes(day, deaths, color = year)) +
    geom_line() +
    geom_vline(xintercept = 20) +
    geom_point() + ylim(0, 110)
  
  
  #------Linear Regression
  
  library(Lahman)
  library(tidyverse)
  library(dslabs)
  Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
    ggplot(aes(AB_per_game, R_per_game)) + 
    geom_point(alpha = 0.5)
  
  Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(X_per_game = X3B / G, Derror_per_game = X2B / G) %>%
    ggplot(aes(X_per_game, Derror_per_game)) + 
    geom_point(alpha = 0.5)
  
  library(tidyverse)
  install.packages('HistData')
  library(HistData)
  data("GaltonFamilies")
  data(galton_heights)
  
  set.seed(1983)
  galton_heights <- GaltonFamilies %>%
    filter(gender == "male") %>%
    group_by(family) %>%
    sample_n(1) %>%
    ungroup() %>%
    select(father, childHeight) %>%
    rename(son = childHeight)
  
  # means and standard deviations
  galton_heights %>%
    summarize(mean(father), sd(father), mean(son), sd(son))
  # scatterplot of father and son heights
  galton_heights %>%
    ggplot(aes(father, son)) +
    geom_point(alpha = 0.5)
  
  B <- 1000
  N <- 25
  R <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>%
      summarize(r = cor(father, son)) %>%
      pull(r)
  })
  qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
  
  # expected value and standard error
  mean(R)
  sd(R)
  
  B <- 1000
  N <- 25
  R <- replicate(B, {
    sample_n(Teams %>% filter(yearID %in% 1961:2001), N, replace = TRUE) %>%
      summarize(r = cor(AB / G, R / G)) %>%
      pull(r)
  })
  qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
  
  # expected value and standard error
  mean(R)
  sd(R)
  
  
  Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
  cor(Teams_small$X2B/Teams_small$G, Teams_small$X3B/Teams_small$G)
  
  set.seed(1989) #if you are using R 3.5 or earlier
  set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
  library(HistData)
  data("GaltonFamilies")
  
  female_heights <- GaltonFamilies%>%    
    filter(gender == "female") %>%    
    group_by(family) %>%    
    sample_n(1) %>%    
    ungroup() %>%    
    select(mother, childHeight) %>%    
    rename(daughter = childHeight)
  
  mu_x <- mean(female_heights$mother)
  mu_y <- mean(female_heights$daughter)
  s_x <- sd(female_heights$mother)
  s_y <- sd(female_heights$daughter)
  r <- cor(female_heights$mother, female_heights$daughter)
  m_1 <-  r * s_y / s_x
  b_1 <- mu_y - m_1*mu_x
  (m_1*60)+b_1
  
  
  galton_heights %>%
    mutate(z_father = round((father - mean(father)) / sd(father))) %>%
    filter(z_father %in% -2:2) %>%
    ggplot() +  
    stat_qq(aes(sample = son)) +
    facet_wrap( ~ z_father)
  
  B <- 1000
  N <- 50
  lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>%
      mutate(father = father - mean(father)) %>%
      lm(son ~ father, data = .) %>% .$coef 
  })
  
  cor(lse[1,], lse[2,]) 
  
  
  #----Linear Regression
  
  set.seed(1983)
  galton_heights <- GaltonFamilies %>%
    filter(gender == "male") %>%
    group_by(family) %>%
    sample_n(1) %>%
    ungroup() %>%
    select(father, childHeight) %>%
    rename(son = childHeight)
  rss <- function(beta0, beta1, data){
    resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
    return(sum(resid^2))
  }
  
  # plot RSS as a function of beta1 when beta0=25
  beta1 = seq(0, 1, len=nrow(galton_heights))
  results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 25))
  results %>% ggplot(aes(beta1, rss)) + geom_line() + 
    geom_line(aes(beta1, rss))
  
  beta1 = seq(0, 1, len=nrow(galton_heights))
  results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 36))
  results %>% ggplot(aes(beta1, rss)) + geom_line() + 
    geom_line(aes(beta1, rss), col=2)
  
  
  Teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(BB_per_game = BB / G, R_per_game = R / G, HR_per_game = HR / G)
    
  lm (R_per_game~BB_per_game+HR_per_game,data=Teams)%>% .$coef
  
  B <- 1000
  N <- 100
  lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>% 
      lm(son ~ father, data = .) %>% .$coef 
  })
  
  lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
  
  set.seed(1989) #if you are using R 3.5 or earlier
  set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
  library(HistData)
  data("GaltonFamilies")
  options(digits = 3)    # report 3 significant digits
  female_heights <- GaltonFamilies %>%     
    filter(gender == "female") %>%     
    group_by(family) %>%     
    sample_n(1) %>%     
    ungroup() %>%     
    select(mother, childHeight) %>%     
    rename(daughter = childHeight) 
  
  female_heights %>% lm(mother~ daughter, data=.)
  
  rss <- function(beta0, beta1, data){
    resid <- female_heights$mother - (beta0+beta1*female_heights$daughter)
    return(sum(resid^2))
  }
  
  # plot RSS as a function of beta1 when beta0=25
  beta1 = seq(0, 1, len=nrow(female_heights))
  results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 25))
  results %>% ggplot(aes(beta1, rss)) + geom_line() + 
    geom_line(aes(beta1, rss))
  #------------------
  model <- lm(mother ~ daughter, data = female_heights)
  predictions <- predict(model, interval = c("confidence"), level = 0.95)
  data <- as.tibble(predictions) %>% bind_cols(daughter = female_heights)
  
  ggplot(data, aes(x = father, y = fit)) +
    geom_line(color = "blue", size = 1) + 
    geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
    geom_point(data = galton_heights, aes(x = father, y = son))
  
  
  
  library(Lahman)
  bat_02 <- Batting %>% filter(yearID == 2002) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    select(playerID, singles, bb)
  
  bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
    mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
    filter(pa >= 100) %>%
    group_by(playerID) %>% 
    summarise(mean_singles = mean(singles), mean_bb = mean(bb))
  
  
  bat_99_01 %>%
    filter(mean_bb > 0.2) %>%
    distinct(playerID, mean_singles, mean_bb, .keep_all = FALSE)
  
  innerjoin <- bat_02 %>% inner_join(bat_99_01)
  innerjoin%>% summarise(cor(bb,mean_bb))
  innerjoin %>% ggplot(aes(singles,mean_singles)) + geom_point()
  innerjoin %>% ggplot(aes(bb,mean_bb)) + geom_point()
  
  get_slope <- function(x, y) cor(x, y) / (sd(x) * sd(y))
  
  bb_slope <- innerjoin %>% 
    summarize(slope = get_slope(mean_singles,singles))
  
  
  innerjoin %>% lm(singles~ mean_singles, data=.)
  innerjoin %>% lm(bb~ mean_bb, data=.)
  
  dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR = HR/G,
           R = R/G) %>%
    select(lgID, HR, BB, R) 
  
  dat %>% 
    group_by(lgID) %>% 
    do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
    filter(term == "HR")
  
  
  library(tidyverse)
  library(HistData)
  data("GaltonFamilies")
  set.seed(1) # if you are using R 3.5 or earlier
  set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
  galton <- GaltonFamilies %>%
    group_by(family, gender) %>%
    sample_n(1) %>%
    ungroup() %>% 
    gather(parent, parentHeight, father:mother) %>%
    mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
    unite(pair, c("parent", "child"))
  
  galton %>%
    group_by(pair) %>%
    summarize(cor = cor(parentHeight, childHeight)) %>%
    filter(cor == min(cor))
  
  galton %>%  group_by(pair) %>%
    do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
    filter(term == "parentHeight") %>%
    mutate(diff= conf.high - conf.low) %>%
    select(pair, estimate, conf.low, conf.high, diff)
  
  
  galton %>%
    group_by(pair) %>%
    do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
    filter(term == "parentHeight" & p.value < .05)
  
  
  
  
  
  
  pa_per_game <- Batting %>% 
    filter(yearID == 2002) %>% 
    group_by(teamID) %>%
    summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
    .$pa_per_game %>% 
    mean
  
  lm(R ~ 2 + 4 + 1 + 0 + 1)
  lm(R ~ 1 + 6 + 2 + 1)
  
  fit <- Teams %>% filter(yearID==1971) %>% 
    mutate(BB=BB/G,singles=(H-X2B-X3B-HR)/G,doubles=X2B/G,triples=X3B/G,HR=HR/G,R=R/G) %>% 
    do(tidy(lm(R~BB+HR, data=.)))
  
  Teams %>%
    filter(yearID == 1971) %>%
    lm(R ~ BB + HR, data = .) %>%
    tidy() %>%
    filter(term == "BB") %>%
    pull(estimate)
  
  res <- Teams %>%
    filter(yearID %in% 1961:2018) %>%
    group_by(yearID) %>%
    do(tidy(lm(R ~ BB + HR, data = .))) %>%
    ungroup() 
  res %>%
    filter(term == "BB") %>%
    ggplot(aes(yearID, estimate)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  fit<- Teams %>% +
    + filter(yearID %in% 1961:2018) %>% +
    + group_by(yearID) %>% +
    + do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE))
  
  fit2 <- fit %>% filter(term == "BB") 
  fit3 <- lm(estimate ~ yearID, data= fit2)
  
  Teams %>%
    filter(yearID %in% 1961:2018) %>%
    group_by(yearID) %>%
    do(tidy(lm(R ~ BB + HR, data = .))) %>%
    ungroup() %>%
    filter(term=='BB') %>%
    lm(estimate ~ yearID, data=.) %>%
    tidy()
  
  Teams %>%
    filter(yearID == 1971) %>%
    lm(R ~ BB + HR, data = .) %>%
    tidy() %>%
    filter(term == "BB")
  
  #--------------------------------
  library(tidyverse)
  library(broom)
  library(Lahman)
  Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G)
  
  runs <- Teams_small %>% mutate(rpg = R/G, hrpg = HR/G) %>% lm(avg_attendance ~ hrpg, data = .)
  
  tidy(runs)
  
  wins <- Teams_small %>% lm(avg_attendance ~ W, data = .)
  tidy(wins)
  
  Year <- Teams_small %>% lm(avg_attendance ~ yearID, data = .)
  tidy(Year)
  
  T <- Teams_small %>% mutate(rpg = R/G, hrpg = HR/G) 
  
  T%>% cor(W, rpg, use = "everything")
  
  cor(R/G, W)
  lm(W, R/G, data = Teams_small)
  
  cor(Teams_small$W, Teams_small$HR/Teams_small$G)
  cor(16149,15840)
  
  Strata_teams %>%  
    group_by(Strata) %>% cor(HR/G, avg_attendance)
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))
  
  Strata_teams <- Teams_small %>% mutate(Strata = round(W/10,0))%>%
    group_by(Strata)%>%
    filter(Strata %in% 5:10) 
  Strata_teams%>% filter(Strata == 8 )
  nrow(Strata_teams)
  
  
  dbstrat <- Strata_teams%>%
    mutate(HRG = HR/G,  RG = R/G)%>%
    do(tidy(lm(avg_attendance ~ RG, data = .),conf.int = TRUE))%>%
    filter(term == 'RG')
  
  
  Teams_small <- Teams %>% filter(yearID %in% 1961:2001) %>% 
    mutate(AT = attendance/G,HRG=HR/G,RG=R/G) %>%
    select(AT,HRG,W,RG,yearID) %>% 
    do(tidy(lm(AT~W+HRG+RG+yearID,data=.))) 
  Teams_small
  
  Team_model <- Teams %>% filter(yearID %in% 1961:2001) %>% 
    mutate(AT = attendance/G,HRG=HR/G,RG=R/G)
  
  model <- lm(AT~W+HRG+RG+yearID, data = Team_model)
  solution_02 <- data.frame(W = 80,HRG = 1.2,RG = 5, yearID = 2002)
  pred <- predict(model,solution_02)
  pred <- predict(model,Teams_small)
  predictions <- predict(model, interval = c("confidence"), level = 0.95)
  data <- as.tibble(predictions) %>% bind_cols(daughter = female_heights)
  data %>% summarise(avg=mean(fit))
  
  cor(pred, pred_original)
  x1=80
  x2=1.2
  x3=5
  x4=2002
  model$coefficients[1]+(x1*model$coefficients[2])+(x2*model$coefficients[2])+(x3*model$coefficients[3])+(x4*model$coefficients[4])
  
  Teams %>% 
    mutate(AT = attendance/G,HRG=HR/G,RG=R/G) %>%
    select(AT,HRG,W,RG,yearID) %>% 
    do(tidy(lm(AT~W+HRG+RG+yearID,data=.))) 
  
  Teams_small2002 <- Teams %>% filter(yearID %in% 2002) %>% 
  mutate(AT = attendance/G,HRG=HR/G,RG=R/G) 
  pred_original <- predict(model, Teams_small2002) 
  cor(pred_original,Teams_small2002$AT)
  
  library(dslabs)
  data("research_funding_rates")
  Gath <- research_funding_rates %>% mutate(notawarded_men = applications_men - awards_men, notawarded_women = applications_women - awards_women) %>%
    select(awards_men, awards_women,notawarded_men, notawarded_women)%>% 
    #spread(awards_men, awards_women,men_not_awarded,Women_not_awarded)
  gather (key, value)
  
  Sep <- Gath %>% separate(col = key, into = c('Awards', 'Gender'), sep = '_')
  
  final <- Sep %>% group_by(Gender,Awards)%>% summarise(V = sum(value)) %>% spread(Gender, V)
  
  chisq_test <- final %>% select(-Awards) %>% chisq.test() 
  
  #---Site solution
  two_by_two <- research_funding_rates %>% 
    select(-discipline) %>% 
    summarize_all(funs(sum)) %>%
    summarize(yes_men = awards_men, 
              no_men = applications_men - awards_men, 
              yes_women = awards_women, 
              no_women = applications_women - awards_women) %>%
    
    dat <- research_funding_rates %>% 
    mutate(discipline = reorder(discipline, success_rates_total)) %>%
    rename(success_total = success_rates_total,
           success_men = success_rates_men,
           success_women = success_rates_women) %>%
    gather(key, value, -discipline) %>%
    separate(key, c("type", "gender")) %>%
    spread(type, value) %>%
    filter(gender != "total")
  dat %>% ggplot(aes(discipline, success, fill = discipline)) + geom_bar(stat = "Identity") + facet_grid(.~gender)
  dat %>% pull(max(success))
  dat%>% group_by(discipline) %>% summarise(S = sum(success)) %>% arrange(desc(S))
    
    separate(key, c("awarded", "gender")) %>%
    spread(gender, value)
  two_by_two
  
  #------Machine Learning-----------------
  
  library(dslabs)
  
  s <- read_mnist()
  
  str(s)
  ncol(s$train$images)
  ncol()
  mnist
  
  ncol(mnist)
  
  
  library(dslabs)
  library(dplyr)
  library(lubridate)
  library(tidyverse)
  install.packages('caret')
  library(caret)
  data("reported_heights")
  
  dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
    filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
    mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
    select(sex, type)
  
  y <- factor(dat$sex, c("Female", "Male"))
  x <- dat$type
  
  inc <- nrow(dat %>% filter(type =='inclass'))
  inc_f <-nrow(dat %>% filter(type =='inclass' & sex == 'Female')) 
  inc_f/inc
  
  onc <- nrow(dat %>% filter(sex == 'Female'))
  
  
  
  onc_f <-nrow(dat %>% filter(type =='online' & sex == 'Female')) 
  onc_f/onc
  x
  
  dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))
  
  # maximize F-score
  cutoff <- c('online','inclass')
  F_1 <- map_dbl(cutoff, function(x1){
    y_hat <- ifelse(x=='online', "Male", "Female") %>% 
      factor(levels = levels(dat$sex))
    mean(y_hat == dat$sex)
  })
  max(F_1)
  
  y %>% filter(y == 'Male')
  
  
  y_hat <- ifelse(x == "online", "Male", "Female") %>% 
    factor(levels = levels(y))
  mean(y_hat==y)
  
  table(predicted = y_hat, actual = dat$sex)
  table(y_hat, y)
  
  sensitivity(y_hat,dat$sex)
  sensitivity(data = y_hat, reference = y)
  specificity(data = y_hat, reference = y)
  
  
  
  test_set %>% 
    mutate(y_hat = y_hat) %>%
    group_by(sex) %>% 
    summarize(accuracy = mean(y_hat == sex))
  prev <- mean(y == "Male")
  
  confusionMatrix(data = y_hat, reference = test_set$sex)
  
  
  
  library(caret)
  data(iris)
  iris <- iris[-which(iris$Species=='setosa'),]
  y <- iris$Species
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  set.seed(2)    # if using R 3.6 or later, use set.seed(2, sample.kind="Rounding")
  # line of code
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  test <- iris[test_index,]
  train <- iris[-test_index,]
  
  
  c <- seq(min(train$Sepal.Width),max(train$Sepal.Width),0.1)
  accuracy <- map_dbl(c,function(x){ 
    y_hat <- ifelse(train$Sepal.Width > x,"virginica","versicolor") %>%
      factor(levels = levels(train$Species))
  mean(y_hat==train$Species,reference = factor(train$Species))
  })
  
  max(accuracy)
  
  #sepal.width - 0.64
  
  c_sl <- seq(min(train$Sepal.Length),max(train$Sepal.Length),0.1)
  accuracy_sl <- map_dbl(c_sl,function(x){ 
    y_hat_sl <- ifelse(train$Sepal.Length > x,"virginica","versicolor") %>%
      factor(levels = levels(train$Species))
    mean(y_hat_sl==train$Species,reference = factor(train$Species))
  })
  
  max(accuracy_sl)
  
  #sepal.length - 0.5
  
  c_pl <- seq(min(train$Petal.Length),max(train$Petal.Length),0.1)
  accuracy_pl <- map_dbl(c_pl,function(x){ 
    y_hat_pl <- ifelse(train$Petal.Length > x,"virginica","versicolor") %>%
      factor(levels = levels(train$Species))
    mean(y_hat_pl==train$Species,reference = factor(train$Species))
  })
  
  max(accuracy_pl)
  
  #Petal.Length - 0.56
  
  c_pw <- seq(min(train$Petal.Width),max(train$Petal.Width),0.1)
  accuracy_pw <- map_dbl(c_pw,function(x){ 
    y_hat_pw <- ifelse(train$Petal.Width > x,"virginica","versicolor") %>%
      factor(levels = levels(train$Species))
    mean(y_hat_pw==train$Species,reference = factor(train$Species))
  })
  
  max(accuracy_pw)
  
  #Petal.width - 0.56 
  
  foo <- function(x){
    rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
    sapply(rangedValues,function(i){
      y_hat <- ifelse(x>i,'virginica','versicolor')
      mean(y_hat==train$Species)
    })
  }
  predictions <- apply(train[,-5],2,foo)
  sapply(predictions,max)	
  
  predictions <- foo(train[,3])
  rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
  cutoffs <-rangedValues[which(predictions==max(predictions))]
  
  y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
  mean(y_hat==test$Species)
  
  
  best_cutoff <- c_pw[which.max(accuracy_pw)]
  best_cutoff
  y_hat_pw <- ifelse(test$Petal.Width > best_cutoff,"virginica","versicolor") %>%
    factor(levels = levels(test$Species))
  #y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
   # factor(levels = levels(test_set$sex))
  y_hat_pw <- factor(y_hat_pw)
  mean(y_hat_pw==test$Species)
  
  
  plot(iris,pch=21,bg=iris$Species)
  
  best_cutoff_pw <- c_pw[which.max(accuracy_pw)]
  best_cutoff_pl <- c_pl[which.max(accuracy_pl)]
  y_hat_pwpl <- ifelse(test$Petal.Width > best_cutoff_pw | test$Petal.Length > best_cutoff_pl ,"virginica","versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat_pwpl==test$Species)
  
  #---COnditional Probablity
  
  p_testpos_disease <- 0.85
  p_testneg_healthy <- 0.90
  p_testpos_healthy <- 0.10
  p_disease <- 0.02
  p_disease_testpos <- p_testpos_disease * p_disease / (p_testpos_healthy)
  p_disease * p_testneg_healthy /p_testpos_disease
  
  set.seed(1)
  disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
  test <- rep(NA, 1e6)
  test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
  test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
  
  mean(1- (test[disease==1]))
  mean(disease[test==0])
  mean(disease[test==1]==1)/mean(disease==1)
  
  library(dslabs)
  data("heights")
  heights %>% 
    mutate(height = round(height)) %>%
    group_by(height) %>%
    summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)
  
  
  ps <- seq(0, 1, 0.1)
  heights %>% 
    mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
    summarize(p = mean(sex == "Male"), height = mean(height)) %>%
    qplot(height, p, data =.)
  
  
  Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
  dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  ps <- seq(0, 1, 0.1)
  dat %>% 
    mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
    group_by(g) %>%
    summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
  
  #---Regression
  
  set.seed(1) # set.seed(1, sample.kind="Rounding") if using R 3.6 or later
  n <- 100
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
 
  set.seed(1)
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  #sqrt(mean((y_hat-test_set$y)^2))
    })
  mean(rmse)
  sd(rmse)
    set.seed(1)    # if R 3.6 or later, set.seed(1, sample.kind="Rounding")
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  
  mean(rmse)
  sd(rmse)
  
  
  set.seed(1) # set.seed(1, sample.kind="Rounding") if using R 3.6 or later

  
  set.seed(1)

    #n <- 100
  n <- c(100, 500, 1000, 5000, 10000)
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  sapply(n, function(i){

    dat <- MASS::mvrnorm(n=n, c(69, 69), Sigma) %>%
      data.frame() %>% setNames(c("x", "y"))
    rmse <- replicate(100, {
        test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
    #sqrt(mean((y_hat-test_set$y)^2))
  })
  mean(rmse)
  sd(rmse)
  })
  
  set.seed(1)
  set.seed(1, sample.kind="Rounding")
  res1 <- function(n){
    Sigma1 <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
    dat1 <- MASS::mvrnorm(n, c(69, 69), Sigma1) %>% 
      data.frame() %>% setNames(c("x", "y"))
    rmse1 <- replicate(100, {
      test_index1 <- createDataPartition(dat1$y, times = 1, p = 0.5, list = FALSE)
      train_set1 <- dat1 %>% slice(-test_index1)
      test_set1 <- dat1 %>% slice(test_index1)
      fit1 <- lm(y ~ x, data = train_set1)
      y_hat1 <- predict(fit1, newdata = test_set1)
      sqrt(mean((y_hat1-test_set1$y)^2))
      #sqrt(mean((y_hat-test_set$y)^2))
    }) 
    result <- c(round(n, digits=3),round(mean(rmse1), digits=3),round(sd(rmse1), digits=3))
  }
  
  
  n <- c(100, 500, 1000, 5000, 10000)
  res_n1 <- sapply(n, res1)
  
  res_n1
  
  round(123.456, digits=2)
  
  # Question 4
  
  set.seed(1)
  n <- 100
  Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  set.seed(1, sample.kind="Rounding")
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
    #sqrt(mean((y_hat-test_set$y)^2))
  })
  mean(rmse)
  sd(rmse)
  
  
  # Question 5
  
  set.seed(1)
  Sigma2 <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
  dat2 <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma2) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  
  set.seed(1, sample.kind="Rounding")
  rmse2 <- replicate(100, {
    test_index2 <- createDataPartition(dat2$y, times = 1, p = 0.5, list = FALSE)
    train_set2 <- dat2 %>% slice(-test_index2)
    test_set2 <- dat2 %>% slice(test_index2)
    fit2 <- lm(y ~ x_2, data = train_set2)
    y_hat2 <- predict(fit2, newdata = test_set2)
    sqrt(mean((y_hat2-test_set2$y)^2))
    #sqrt(mean((y_hat-test_set$y)^2))
  })
  mean(rmse2)
  sd(rmse2)
  
  set.seed(1)
  Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
  dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))
  
  set.seed(1)
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y ~ x_2, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
  
  
  #y ~ x_1 + x_2
  #   mean(rmse2)
  # 0.3450927 -----0.6597865
  #   sd(rmse2)
  # 0.02475961
  
  
  #y ~ x_1
  #   mean(rmse2)
  # 0.6369011 ------0.6592608
  #   sd(rmse2)
  # 0.04133126
  
  
  #y ~ x_2
  #   mean(rmse2)
  # 0.6515611  ----------0.6400
  #   sd(rmse2)
  # 0.03701591
  
  
  #Logistic Regression
  
  set.seed(2) #if you are using R 3.5 or earlier
  set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
  make_data <- function(n = 1000, p = 0.5, 
                        mu_0 = 0, mu_1 = 2, 
                        sigma_0 = 1,  sigma_1 = 1){
    
    y <- rbinom(n, 1, p)
    f_0 <- rnorm(n, mu_0, sigma_0)
    f_1 <- rnorm(n, mu_1, sigma_1)
    x <- ifelse(y == 1, f_1, f_0)
    
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    
    list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
         test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
  }
  dat <- make_data()
  
  dat$train %>% ggplot(aes(x, color = y)) + geom_density()
  
  install.packages("e1071")
  
  set.seed(1, sample.kind="Rounding")
  make_data <- function(n = 1000, p = 0.5, 
                        mu_0 = 0, mu_1 = seq(0, 3, len=25), 
                        sigma_0 = 1,  sigma_1 = 1){
    
    y <- rbinom(n, 1, p)
    f_0 <- rnorm(n, mu_0, sigma_0)
    f_1 <- rnorm(n, mu_1, sigma_1)
    x <- ifelse(y == 1, f_1, f_0)
    
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    
    list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
         test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
  }
  dat <- make_data()
  #dat$train
  
  # fit logistic regression model
  glm_fit <- dat$train %>% 
    glm(y ~ x, data=., family = "binomial")
  p_hat_logit <- predict(glm_fit, newdata = dat$test, type = "response")
  y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor
  confusionMatrix(y_hat_logit, dat$test$y)$overall[["Accuracy"]] %>%
    ggplot(aes(Accuracy,mu_1, color = )) + geom_point()
  
  set.seed(1) #if you are using R 3.5 or earlier
  set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
  delta <- seq(0, 3, len = 25)
  res <- sapply(delta, function(d){
    dat <- make_data(mu_1 = d)
    fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
    y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
    mean(y_hat_glm == dat$test$y)
  })
  qplot(delta, res)
  
  #Smoothing--------------------
  
  library(tidyverse)
  library(lubridate)
  library(purrr)
  library(pdftools)
  
  fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
  dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
    s <- str_trim(s)
    header_index <- str_which(s, "2015")[1]
    tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
    month <- tmp[1]
    header <- tmp[-1]
    tail_index  <- str_which(s, "Total")
    n <- str_count(s, "\\d+")
    out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
    s[-out] %>%
      str_remove_all("[^\\d\\s]") %>%
      str_trim() %>%
      str_split_fixed("\\s+", n = 6) %>%
      .[,1:5] %>%
      as_data_frame() %>% 
      setNames(c("day", header)) %>%
      mutate(month = month,
             day = as.numeric(day)) %>%
      gather(year, deaths, -c(day, month)) %>%
      mutate(deaths = as.numeric(deaths))
  }) %>%
    mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                          "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
    mutate(date = make_date(year, month, day)) %>%
    filter(date <= "2018-05-01")
  
  span <- 60 / as.numeric(diff(range(dat$date)))
  fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
  dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
    ggplot() +
    geom_point(aes(date, deaths)) +
    geom_line(aes(date, smooth), lwd = 2, col = 2)
  
  
  library(dslabs)
  library(tidyverse)
  library(broom)
  data("mnist_27")
  mn <- mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
  
  qplot(x_2, y, data = mnist_27$train)
  mnist_27$train <- mnist_27$train %>% mutate(y = ifelse(y == 7, 1, 0))
  fit <- mnist_27$train %>% loess(y  ~ x_2, data = ., degree = 1)
  mnist_27$train %>% mutate(smooth = predict(fit, x_2)) %>%
    ggplot() +
    geom_point(aes(x_2, y)) +
    geom_line(aes(x_2, smooth), lwd = 2, col = 2)
  
  mnist_27$train %>% 
    #mutate(y = ifelse(y=="7", 1, 0)) %>%
    ggplot(aes(x_2, y)) + 
    geom_smooth(method = "loess")

  #Matrices
  
  x <- matrix(rnorm(100*10), 100, 10)
  dim(x)
  nrow(x)
  ncol(x)
  
  y <- 1:10
  y <- matrix(y,5,2)
  sweep(y,2,)
  
  sweep(y, 1, 1:nrow(y),"+")
  
  1:nrow(y)
  
  y + seq(nrow(y))
  
  1:ncol(y)
  1:col(y)
  sweep(y, 2, 1:ncol(y), FUN = "+")
  
  y
  rowMeans(y)
  
  sapply(y,mean)
  
  library(tidyverse)
  library(dslabs)
  if(!exists("mnist")) mnist <- read_mnist()
  
  class(mnist$train$images)
  
  mnist$train$images > 50
  
  view(mnist$train$images >50 & (mnist$train$images <250))
  
  mnist <- read_mnist()
  y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
  qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
  
  #-----------------Distance
  
  library(dslabs)
  data("tissue_gene_expression")
  dim(tissue_gene_expression$x)
  nrow(tissue_gene_expression$x)
  table(tissue_gene_expression$y)
  
  
  d <- dist(tissue_gene_expression$x)
  
  sqrt(crossprod(tissue_gene_expression$x[1,] - tissue_gene_expression$x[2,]))
  
  
  if(!exists("mnist")) mnist <- read_mnist()
  set.seed(1995) # if using R 3.5 or earlier
  set.seed(1995) # if using R 3.6 or later
  ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
  
  #the predictors are in x and the labels in y
  x <- mnist$train$images[ind,]
  y <- mnist$train$labels[ind]
  y[1:3]
  x_1 <- x[1,]
  x_2 <- x[2,]
  x_3 <- x[3,]
  
  #distance between two numbers
  sqrt(sum((x_1 - x_2)^2))
  sqrt(sum((x_1 - x_3)^2))
  sqrt(sum((x_2 - x_3)^2))
  
  #compute distance using matrix algebra
  sqrt(crossprod(x_1 - x_2))
  sqrt(crossprod(x_1 - x_3))
  sqrt(crossprod(x_2 - x_3))
  
  #compute distance between each row
  d <- dist(x)
  class(d)
  as.matrix(d)[1:3,1:3]
  
  #visualize these distances
  image(as.matrix(d))
  
  #order the distance by labels
  image(as.matrix(d)[order(y), order(y)])
  
  #compute distance between predictors
  d <- dist(t(x))
  dim(as.matrix(d))
  d_492 <- as.matrix(d)[492,]
  image(1:28, 1:28, matrix(d_492, 28, 28))
  
  sqrt(crossprod(tissue_gene_expression$x[1,] - tissue_gene_expression$x[2,]))
  image(as.matrix(d)[c(73,74), c(73,74)])
  
  image(as.matrix(d)[c(1,2,39,40,73,74), c(1,2,39,40,73,74)])
  
  library(dslabs)
  data("heights")
  y <- heights$sex
  
  set.seed(1) #if you are using R 3.5 or earlier
  set.seed(1, sample.kind = "Rounding") #if you are using R 3.6 or later
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- heights %>% slice(-test_index)
  test_set <- heights %>% slice(test_index)
  
  glm_fit <- train_set %>% 
    mutate(y = as.numeric(sex == "Female")) %>%
    glm(y ~ height, data=., family = "binomial")
  p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
  y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
  confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]
  
  knn_fit <- knn3(y ~ height, data = mnist_27$train)
  x <- as.matrix(mnist_27$train[,2:3])
  y <- mnist_27$train$y
  knn_fit <- knn3(x, y)
  knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)
  y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
  confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]
  
  ks <- seq(1, 101, 3)
  library(purrr)
  accuracy <- map_df(ks, function(k){
    fit <-train_set %>% 
          knn3(sex ~ height, data=., k = k)
    y_hat <- predict(fit,train_set , type = "class")
    y_hat
    cm_train <- confusionMatrix(data = y_hat, reference = train_set$sex)
    train_error <- cm_train$overall["Accuracy"]
    y_hat <- predict(fit, test_set, type = "class")
    cm_test <- confusionMatrix(data = y_hat, reference = test_set$sex)
    test_error <- cm_test$overall["Accuracy"]
    F_1 <- F_meas(data = y_hat, reference = test_set$sex)
    
    tibble(F_1,train = train_error, test = test_error)
  })
  

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)
max(accuracy$F_1)

#---------Following question------------

library(dslabs)
data("tissue_gene_expression")

 


set.seed(1) #if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
train_set_x <- tissue_gene_expression$x[-test_index,]
train_set_y <- tissue_gene_expression$y[-test_index]
test_set_x <- tissue_gene_expression$x[test_index,]
test_set_y <- tissue_gene_expression$y[test_index]

trainset_gene <- list('x' = train_set_x, 'y' = train_set_y)
testset_gene <- list('x' = test_set_x, 'y' = test_set_y)

ks <- c(1,3, 5, 7, 9, 11)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(trainset_gene$y ~ trainset_gene$x, data=trainset_gene, k = k)
  y_hat <- predict(fit,testset_gene , type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = testset_gene$y)
  test_error <- cm_test$overall["Accuracy"]
  tibble(test = test_error)
})

ks[which.max(accuracy$test)]
max(accuracy$test)


length(y_hat)
length(trainset_gene$y)
length(testset_gene$y)
length(tissue_gene_expression$y)


#------following question ends---------------
  if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
  BiocManager::install("genefilter")
  library(genefilter)
  tt <- colttests(x,y)
  tt <- colttests(x_subset, y)
pvals <- tt$p.value

colttests(x, fac, tstatOnly = FALSE)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

pvals <- tt$CORRECT.ANSWER

set.seed(1995,sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

mnist_27$train$y[indexes$Resample01]

sum(indexes[[1]] == 3)
sum(indexes[[2]] == 3)
sum(indexes[[3]] == 3)
sum(indexes[[4]] == 3)
sum(indexes[[5]] == 3)
sum(indexes[[6]] == 3)
sum(indexes[[7]] == 3)
sum(indexes[[8]] == 3)
sum(indexes[[9]] == 3)
sum(indexes[[10]] == 3)

x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)


y <- rnorm(100, 0, 1)


set.seed(1)
B <- 10^4
N <- 100
y_starfull <- replicate(B, {
  y <- rnorm(100, 0, 1)
  #set.seed(1, sample.kind="Rounding")
  #y_star <- sample(y, N, replace = TRUE)
  quantile(y, 0.75)
})
mean(y_starfull)
sd(y_starfull)


set.seed(1995,sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

mnist_27$train$y[indexes$Resample01]

set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
indexes <- createResample(y, 10000)
quantile(y, 0.75)


x=sapply(indexes, function(ind){
  quantile(y[ind], 0.75)
})
mean(x)
sd(x)

library(dslabs)
library(caret)
data("tissue_gene_expression")


set.seed(1993)
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda")

train_lda <- train(x, y, method = "lda", preProcess = "center")
train_qda <- train(x, y, method = "qda")

str(train_lda$finalModel$means)

t(train_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


t(train_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

d <- apply(train_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

data("tissue_gene_expression")
set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda", preProcess = "center")

#CART

# Load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# visualize the splits 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# change parameters
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))


polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ .,method = "rpart",tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)


library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1) #set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)
plot(fit)
text(fit)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(y_hat, x), col=2)

stop


train_rf <- randomForest(y ~ ., data=mnist_27$train)

library(randomForest)
fit <- randomForest(y ~ x, data = dat) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  plot(fit)
  text(fit)
  
  
  library(dslabs)
  library(caret)
  data("tissue_gene_expression")
  set.seed(1991)
  y <- tissue_gene_expression$y
  x <- tissue_gene_expression$x
  #x <- x[, sample(ncol(x), 10)]
  
  train_rpart <- train(x, y,method = "rpart",control = data.frame(cp = seq(0, 0.1, 0.01)),data = tissue_gene_expression)
  
  train_rpart <- rpart(y ~ ., data = tissue_gene_expression), control = rpart.control(cp = seq(0, 0.1, 0.01)))

fit <- rpart(y ~ x, data = tissue_gene_expression)
train_rpart <- train(y ~ x,method = "rpart",tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), data = tissue_gene_expression)

library(caret)
set.seed(1991)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x


#fit <- with(tissue_gene_expression, 
            #train(x, y, method = "rpart",
                 #tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

#ggplot(fit) 

fitc <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  control = rpart.control(minsplit = 0),
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

fittree <- with(tissue_gene_expression, 
             rpart(y ~ x,tissue_gene_expression))

plot(fitc$finalModel)
text(fitc$finalModel)

ggplot(fitc) 
confusionMatrix(fitc)


set.seed(1991)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

fitrf <- with(tissue_gene_expression, 
             train(x,y, method = "rf",
                   tuneGrid = data.frame(mtry = seq(50, 200, 25)), 
                   nodesize = 1))


ggplot(fit)

imp <- varImp(fitc)
imp$importance %>% mutate(name = row.names(.), rank = rank(.$Overall)) %>% filter(name == "CFHR4")

tree_terms <- as.character(unique(fitc$finalModel$frame$var[!(fitc$finalModel$frame$var == "<leaf>")]))
tree_terms

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)

# Titanic problem starts-------------------------------

library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

str(titanic_clean)

set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)

str(test_set)
nrow(train_set %>% filter(Survived==1)) / nrow(train_set) 

#set.seed(3)
set.seed(3, sample.kind = "Rounding")
# guess with equal probability of survival
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1))

test_set %>%
  summarize( (sum(Sex == 'female' & Survived == 1) + sum(Sex == 'male' & Survived == 0)) / n())

train_set %>%
  group_by(Sex,Pclass) %>%
  summarize(Survived = mean(Survived == 1))

test_set %>%
  #group_by(Sex,Pclass) %>%
  summarize( (sum(Pclass == 1 & Sex == 'female' & Survived == 1) + sum(Pclass == 1 & Sex == 'male' & Survived == 0) 
              + sum(Pclass == 2 & Sex == 'female' & Survived == 1) + sum(Pclass == 2 & Sex == 'male' & Survived == 0)
              + sum(Pclass == 3 & Sex == 'female' & Survived == 0) + sum(Pclass == 3 & Sex == 'male' & Survived == 0))/ n())
              

sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy

y_hat_logit_sex <-  sex_model%>% factor
confusionMatrix(y_hat_logit_sex, test_set$Survived)

# Sensitivity : 0.873         
# Specificity : 0.739         
# Pos Pred Value : 0.842         
# Neg Pred Value : 0.785         
# Prevalence : 0.615         
# Detection Rate : 0.536         
# Detection Prevalence : 0.637         
# Balanced Accuracy : 0.806         
# 
# 'Positive' Class : 0             

F_meas(y_hat_logit_sex, test_set$Survived)

# 0.857

class_model <- ifelse(test_set$Pclass == 1, 1, 0)    # predict survival only if first class
mean(class_model == test_set$Survived)    # calculate accuracy


y_hat_logit_class <-  class_model%>% factor
confusionMatrix(y_hat_logit_class, test_set$Survived)

# Sensitivity : 0.855        
# Specificity : 0.464        
# Pos Pred Value : 0.718        
# Neg Pred Value : 0.667        
# Prevalence : 0.615        
# Detection Rate : 0.525        
# Detection Prevalence : 0.732        
# Balanced Accuracy : 0.659        
# 
# 'Positive' Class : 0 
F_meas(y_hat_logit_class, test_set$Survived)
#0.78
sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)

y_hat_logit_SC <-  sex_class_model%>% factor
confusionMatrix(y_hat_logit_SC, test_set$Survived)
#   
# Sensitivity : 0.991         
# Specificity : 0.551         
# Pos Pred Value : 0.779         
# Neg Pred Value : 0.974         
# Prevalence : 0.615         
# Detection Rate : 0.609         
# Detection Prevalence : 0.782         
# Balanced Accuracy : 0.771         
# 
# 'Positive' Class : 0

F_meas(y_hat_logit_SC, test_set$Survived)
#0.872

set.seed(1)
train_qda <- train(Survived ~ Fare, data = train_set, method = 'qda')
Survived_hat_qda <- predict(train_qda, test_set)
mean(test_set$Survived == Survived_hat_qda)

set.seed(1)
glm_fit <- glm(Survived ~ Age, data=train_set, family = "binomial")
p_hat_logit <- predict(glm_fit, test_set)
y_hat_logit <- ifelse(p_hat_logit >= 0,1,0) 
confusionMatrix(y_hat_logit, test_set$Survived)

fit_logreg_a <- glm(Survived ~ Age, data = train_set, family = 'binomial')
survived_hat_a <- ifelse(predict(fit_logreg_a, test_set) >= 0, 1, 0)
mean(survived_hat_a == test_set$Survived)


fit_logreg_a <- glm(Survived ~ . , data = train_set, family = 'binomial')
survived_hat_a <- ifelse(predict(fit_logreg_a, test_set) >= 0, 1, 0)
mean(survived_hat_a == test_set$Survived)

set.seed(6, sample.kind = "Rounding")
k <- seq(3,51,2)
fit_knn9a <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
fit_knn9a$bestTune

  #ggplot(fit_knn9a)
  
  survived_hat <- predict(fit_knn9a, test_set) %>% factor(levels = levels(test_set$Survived))
  cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
  cm_test$overall["Accuracy"]
  

  set.seed(8, sample.kind = "Rounding")
  k <- seq(3,51,2)
  fit_knn10a <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k), trControl = trainControl(method = "cv", number=10, p=0.9))
  fit_knn10a$bestTune
  
  #ggplot(fit_knn9a)
  
  survived_hat10 <- predict(fit_knn10a, test_set) %>% factor(levels = levels(test_set$Survived))
  cm_test10 <- confusionMatrix(data = survived_hat10, reference = test_set$Survived)
  cm_test10$overall["Accuracy"]
  
  
  set.seed(10, sample.kind = "Rounding")
  train_rpart <- train(Survived ~ .,
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                       data = train_set)
  
  train_rpart$bestTune
  
  survived <- predict(train_rpart,test_set)
  confusionMatrix(data = survived, reference = test_set$Survived)
  
  
  plot(train_rpart$finalModel)
  text(train_rpart$finalModel)
  
  train_rpart$finalModel
  plot(train_rpart$finalModel, margin=0.1)
  text(train_rpart$finalModel, cex = 0.75)
  
  ggplot(train_rpart$finalModel) 
  confusionMatrix(fitc)
  
  train_set %>% filter(Sex =='male' & Survived == 1) %>% summarize(Age)
  
  
  #------------------
  
  #set.seed(10)
  set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
  train_rpart <- train(Survived ~ ., 
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                       data = train_set)
  train_rpart$bestTune
  rpart_preds <- predict(train_rpart, test_set)
  mean(rpart_preds == test_set$Survived)
  train_rpart$finalModel # inspect final model
  
  # make plot of decision tree
  plot(train_rpart$finalModel, margin = 0.1)
  text(train_rpart$finalModel)
  
  # install.packages("rpart.plot")
  # library(rpart)
  
    
  rpart_train <- rpart(Survived ~ ., data = train_set, cp = .016)
  
  install.packages("sos")
  library(plyr)
  findFn("rbind.fill")
  
  str(test_set)
  
  rpart.plot(rpart_train)
  oos_test <- test_set[0,]
  oos_test <- rbind.fill(oos_test, data.frame(Sex="male",  Age=28))
  oos_test <- rbind.fill(oos_test, data.frame(Sex="female",  Pclass=2))
  oos_test <- rbind.fill(oos_test, data.frame(Sex="female",  Pclass=3, Fare=8))
  oos_test <- rbind.fill(oos_test, data.frame(Sex="male",  Age=5, SibSp = 4))
  oos_test <- rbind.fill(oos_test, data.frame(Sex="female",  Pclass=3, Fare=25))
  oos_test <- rbind.fill(oos_test, data.frame(Sex="female",  Pclass=1, Age=17, SibSp = 2))
  oos_test <- rbind.fill(oos_test, data.frame(Sex="male",  Pclass=1, Age=17, SibSp = 2))
  predict(rpart_train, oos_test, type="class")
  
  fit_rf <- train(Survived ~ ., method = "rf",data = train_set,
        tuneGrid = data.frame(mtry = seq(1,7,1)), 
        ntree=100)
  survived <- predict(fit_rf,test_set)
  confusionMatrix(data = survived, reference = test_set$Survived)
  
  varImp(fit_rf)
  
  
  # -----Ensemble-------------------------
  models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
  
  library(caret)
  library(dslabs)
  set.seed(1) # use `set.seed(1, sample.kind = "Rounding")` in R 3.6 or later
  data("mnist_27")
  
  fits <- lapply(models, function(model){ 
    print(model)
    train(y ~ ., method = model, data = mnist_27$train)
  }) 
  
  names(fits) <- models
  
  models_y_hat <- sapply(fits, function(fit_model){
    predict(fit_model, mnist_27$test)
  })
  dim(models_y_hat)
  
  model_accuracies <- colMeans(models_y_hat == mnist_27$test$y)
  mean(model_accuracies)
  
  votes <- rowMeans(models_y_hat == "7")
  y_hat <- ifelse(votes > 0.5, "7", "2")
  mean(y_hat == mnist_27$test$y)
  
  acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
  mean(acc_hat)
  
  min(fits$results$Accuracy)
  fits$results$Accuracy
  
  df<-as.data.frame(table(models_y_hat[11,]), stringsAsFactors =TRUE)
  df$Var1
  
  y_hat_maj <- sapply(seq(1,nrow(models_y_hat)), function(index_line){
    df <- as.data.frame(table(models_y_hat[index_line,]))
    df[which.max(df$Freq),]$Var1
    df
  })
  mean(y_hat_maj == mnist_27$test$y)
  
  ind <- acc_hat >= 0.8
  votes <- rowMeans(models_y_hat[,ind] == "7")
  y_hat <- ifelse(votes>=0.5, 7, 2)
  mean(y_hat == mnist_27$test$y)
  
  #-----------DImension reduction--------
  data("tissue_gene_expression")
  dim(tissue_gene_expression$x)
  
  ggplot(aes(tissue_gene_expression$x[,1],tissue_gene_expression$x[,2], color=tissue_gene_expression$y)) + geom_point()
  
  pc <- prcomp(tissue_gene_expression$x)
  
  data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
             tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, pc_2, color = tissue)) +
    geom_point()
  
  bias <- rowMeans(tissue_gene_expression$x)
  data.frame(pc_1 = pc$x[,1], bias, 
             tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, bias, color = tissue)) +
    geom_point()
  
  cor(pc$x[,1], bias)
  
  x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
  pc <- prcomp(x)
  data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
             tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, pc_2, color = tissue)) +
    geom_point()
  
  for(i in 1:10){
    boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
  }
  
  summary(pc)
  pca_var <- sapply(pc2$sdev[1:10], function(pc_i){
    pc_i^2 / (t(pc2$sdev) %*% pc2$sdev)  
  })
  plot(pca_var) 
  
  plot(summary(pc)$importance[3,])
  
  library(dslabs)
  data("movielens")
  
  movielens %>% group_by(movieId) %>% 
         summarize(n = n_distinct(userId),year= as.character(first(year))) %>%
  
  qplot(year, n, data = ., geom = "boxplot") +
    coord_trans(y = "sqrt") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  movielens %>% filter(year >= 1993 & year <= 2018 ) %>% group_by(movieId) %>% 
    summarize(avg_rating = mean(rating), n = n(), title=title[1], years=2018 - first(year)) %>%
    mutate(n_year = n/years) %>%
    top_n(25, n_year) %>%
    arrange(desc(n_year))
  
    qplot(year, n, data = ., geom = "boxplot") +
    coord_trans(y = "sqrt") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
    
    movielens %>% filter(year >= 1993 & year <= 2018 ) %>% group_by(movieId) %>% 
      summarize(avg_rating = mean(rating), n = n(), title=title[1], years=2018 - first(year)) %>%
      mutate(n_year = n/years) %>%
      #top_n(25, n_year) %>%
      arrange(desc(n_year)) %>%
    ggplot(aes(n_year,avg_rating)) +
      geom_point() + geom_smooth()
    
    movielens %>%
      mutate(date = round_date(date, unit = "week")) %>%
      group_by(date) %>% 
      summarize(avg_rating = mean(rating)) %>%
      ggplot(aes(date,avg_rating)) +
      geom_point() + geom_smooth()
    
    
    movielens %>% mutate(date = round_date(date, unit = "week")) %>%
      group_by(date) %>%
      summarize(rating = mean(rating)) %>%
      ggplot(aes(date, rating)) +
      geom_point() +
      geom_smooth()
    
    movielens %>% group_by(genres) %>% 
      summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
      filter(n >=1000) %>%
      mutate(genres = reorder(genres, avg)) %>%
      ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
      geom_point() +
      geom_errorbar() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
    
    set.seed(1986, sample.kind="Rounding")
    n <- round(2^rnorm(1000, 8, 1))
    set.seed(1, sample.kind="Rounding")
    mu <- round(80 + 2*rt(1000, 5))
    range(mu)
    schools <- data.frame(id = paste("PS",1:1000),
                          size = n,
                          quality = mu,
                          rank = rank(-mu))
    schools %>% 
      top_n(10, quality) %>% 
      arrange(desc(quality))
    set.seed(1, sample.kind="Rounding")
    mu <- round(80 + 2*rt(1000, 5))
    scores <- sapply(1:nrow(schools), function(i){
      scores <- rnorm(schools$size[i], schools$quality[i], 30)
      scores
    })
    schools <- schools %>% mutate(score = sapply(scores, mean))
    head(schools)
    
    
    schools_top10 <- schools %>%
      top_n(10, score) %>% 
      arrange(desc(score))
    schools_top10
    
    schools %>% summarise(median(size))
    schools_top10 %>% summarise(median(size))
    
    schools %>% ggplot(aes(size,score)) + geom_point()+
    geom_point(data = filter(schools, rank<=10), col = 2) 
    
    overall <- mean(sapply(scores, mean))
    alpha <- 25
    schools5 <- schools %>%
      mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
      arrange(desc(score_dev))
    #    mutate(quality_new = score_dev-80)
    schools5 %>%
      top_n(10, score_dev)
    
    alpha_best <- 135
    schools7 <- schools %>%
      mutate(score_dev = overall + (score - overall) * size / (size + alpha_best)) %>%
      arrange(desc(score_dev)) %>%
      top_n(10, score_dev)
    schools7
    
    
    #---------------- Capstone
    
    ################################
    # Create edx set, validation set
    ################################
    
    # Note: this process could take a couple of minutes
    
    if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
    if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
    if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
    
    # MovieLens 10M dataset:
    # https://grouplens.org/datasets/movielens/10m/
    # http://files.grouplens.org/datasets/movielens/ml-10m.zip
    
    dl <- tempfile()
    download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
    
    ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                     col.names = c("userId", "movieId", "rating", "timestamp"))
    
    movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
    colnames(movies) <- c("movieId", "title", "genres")
    movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                               title = as.character(title),
                                               genres = as.character(genres))
    
    movielens <- left_join(ratings, movies, by = "movieId")
    
    # Validation set will be 10% of MovieLens data
    set.seed(1, sample.kind="Rounding")
    # if using R 3.5 or earlier, use `set.seed(1)` instead
    test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
    edx <- movielens[-test_index,]
    temp <- movielens[test_index,]
    
    # Make sure userId and movieId in validation set are also in edx set
    validation <- temp %>% 
      semi_join(edx, by = "movieId") %>%
      semi_join(edx, by = "userId")
    
    # Add rows removed from validation set back into edx set
    removed <- anti_join(temp, validation)
    edx <- rbind(edx, removed)
    
    rm(dl, ratings, movies, test_index, temp, movielens, removed)
    
    
    nrow(edx %>% filter(rating == 3))
    
    edx %>% filter()
    
    n_distinct(edx$userId)
    
    edx %>% group_by(genres) %>% filter(genres == 'Drama') %>% summarise(n = n())
    
    edx %>% filter(genres == 'Drama')
    
    drama <- edx %>% filter(str_detect(genres,"Drama"))
    comedy <- edx %>% filter(str_detect(genres,"Comedy"))
    thriller <- edx %>% filter(str_detect(genres,"Thriller"))
    romance <- edx %>% filter(str_detect(genres,"Romance"))
    nrow(drama)
    nrow(comedy)
    nrow(thriller)
    nrow(romance)
    
    edx %>% group_by(title) %>% summarise(number = n()) %>%
      arrange(desc(number))
    
    
    edx %>% group_by(rating) %>% summarise(number = n()) %>%
      arrange(desc(number))
    
    
    movielens$year <- as.numeric(substr(as.character(movielens$title),nchar(as.character(movielens$title))-4,nchar(as.character(movielens$title))-1))
    
    plot(table(movielens$year),
         col = "#2E9FDF")
    
    tinytex::install_tinytex()
    