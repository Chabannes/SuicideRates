install.packages("tidyverse")
install.packages("ggalt")
install.packages("countrycode")
install.packages("rworldmap")
install.packages("gridExtra")
install.packages("broom")


library(tidyverse) 
library(ggalt)
library(countrycode) 
library(rworldmap) 
library(gridExtra) 
library(broom) 

options(digits=3)


# IMPORTING THE DATA

data <- read_csv("/home/francois/Documents/RProjects/SuicideRates/master.csv") 
data<- as.data.frame()

data <- data %>% 
  select(-c(`HDI for year`, `suicides/100k pop`)) %>%
  rename(gdp_for_year = `gdp_for_year ($)`, 
         gdp_per_capita = `gdp_per_capita ($)`, 
         country_year = `country-year`) %>%
  as.data.frame()

# dropping the last year (uncomplete)
data <- data %>%
  filter(year != 2016) %>% 
  select(-country_year)

# dropping countris with less than 3 years of data recording
minimum_years <- data %>%
  group_by(country) %>%
  summarize(rows = n(), 
            years = rows / 12) %>%
  arrange(years)
data <- data %>%
  filter(!(country %in% head(minimum_years$country, 7)))


# ORDERING DATAFRAME

data$age <- gsub(" years", "", data$age)

# getting continent data:
data$continent <- countrycode(sourcevar = data[, "country"],
                              origin = "country.name",
                              destination = "continent")

# making country, sex and continent nominal factors
nominal_data <- c('country', 'sex', 'continent')
data[nominal_data] <- lapply(data[nominal_data], function(x){factor(x)})

# making age and generation ordinals
data$age <- factor(data$age, 
                   ordered = T, 
                   levels = c("5-14", "15-24","25-34","35-54","55-74","75+"))

data$generation <- factor(data$generation, 
                          ordered = T, 
                          levels = c("G.I. Generation","Silent","Boomers", "Generation X", "Millenials", "Generation Z"))

data <- as_tibble(data)



# PLOT WORLD AVERAGE EVOLUTION OVER TIME

data %>%
  group_by(year) %>%
  summarize(population = sum(population), 
            suicides = sum(suicide_number), 
            suicide_percentage = (suicides/ population)*100) %>%
  ggplot(aes(x = year, y = suicide_percentage)) + 
  geom_line(col = "blue", size = 1) + 
  geom_point(col = "blue", size = 2) + 
  labs(title = "World Suicide Rate Evolution (percentage)",
       subtitle = "Time period : 1985 - 2015.",
       x = "Year", 
       y = "Suicide rate (%)") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) 


# SUICIDE RATE BY CONTINENTS

continent <- data %>%
  group_by(continent) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100) %>%
  arrange(suicide_percentage)

continent$continent <- factor(continent$continent, ordered = T, levels = continent$continent)

continent_box <- ggplot(continent, aes(x = continent, y = suicide_percentage, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(title = "World Suicide Rate Evolution (percentage) by Continent",
       x = "Continent", 
       y = "Suicide rate (%)", 
       fill = "Continent") +
  theme(legend.position = "none", title = element_text(size = 10)) + 
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F)

continent_time <- data %>%
  group_by(year, continent) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100)

continent_time$continent <- factor(continent_time$continent, ordered = T, levels = continent$continent)
continent_time_plot <- ggplot(continent_time, aes(x = year, y = suicide_percentage, col = factor(continent))) + 
  facet_grid(continent ~ ., scales = "free") + 
  geom_point() + 
  geom_line() +
  labs(title = "Suicide rate evolution over time per continent", 
       x = "Year", 
       y = "Suicide rate (%)", 
       color = "Continent") + 
  theme(legend.position = "none") + scale_x_continuous(breaks = seq(1985, 2015, 5))
dev.new(width=40, height=40)
grid.arrange(continent_time_plot, continent_box, ncol=2)


# BY COUNTRY

dev.new(width=40, height=40)
country <- data %>%
  group_by(country, continent) %>%
  summarize(n = n(), 
            suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100) %>%
  arrange(desc(suicide_percentage))

country$country <- factor(country$country, 
                          ordered = T, 
                          levels = rev(country$country))

ggplot(country, aes(x = country, y = suicide_percentage, fill = continent)) + 
  geom_bar(stat = "identity") + 
  labs(title = "World Suicide Rate (percentage) by Country",
       x = "Country", 
       y = "Suicide rate (%)", 
       fill = "Continent") +
  coord_flip() +
  theme(legend.position = "bottom")


# BY AGE

# box plot
age_box <- data %>%
  group_by(age) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100) %>%
  ggplot(aes(x = age, y = suicide_percentage, fill = age)) + 
  geom_bar(stat = "identity") + 
  geom_bar(stat='identity') +
  coord_flip()
  labs(title = "World Suicide Rate (percentage) by age",
       x = "Age", 
       y = "Suicide rate (%)") +
  theme(legend.position = "none") + 
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

# time evolution
age_time <- data %>%
  group_by(year, age) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100) %>%
  ggplot(aes(x = year, y = suicide_percentage, col = age)) + 
  facet_grid(age ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "World Suicide Rate (percentage) by age", 
       x = "Year", 
       y = "Suicide rate (%)", 
       color = "Age") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F)
dev.new(width=40, height=40)
grid.arrange(age_time, age_box, ncol=2)


# SUICIDE RATE BY GENDER

# global box plot
sex_box <- data %>%
  group_by(sex) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100) %>%
  ggplot(aes(x = sex, y = suicide_percentage, fill = sex)) + 
  geom_bar(stat = "identity") + 
  labs(title = "World Suicide Rate (percentage) by gender",
       x = "Gender", 
       y = "Suicide rate (%)") +
  theme(legend.position = "none") 

# time evolution
sex_time <- data %>%
  group_by(year, sex) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100) %>%
  ggplot(aes(x = year, y = suicide_percentage, col = factor(sex))) + 
  facet_grid(sex ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "World Suicide Rate (percentage) by gender", 
       x = "Year", 
       y = "Suicide rate (%)", 
       color = "Gender") + 
  theme(legend.position = "none") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5))
dev.new(width=40, height=40)

grid.arrange(sex_time,sex_box,  ncol = 2)


# COUNTRIES COMPARISON

data_filtered <- data %>%
  filter(country %in% c("France", 
                        "Norway",
                        "United States", 
                        "Russian Federation", 
                        "Brazil")) 

country_time <- data_filtered %>%
  group_by(country, year) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100) %>%
  ggplot(aes(x = year, y = suicide_percentage, col = country)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(se = F, span = 0.2) + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F) + 
  labs(title = "Countries comparison", 
       x = "Year", 
       y = "Suicide rate (%)", 
       col = "Country")

gender_box <- data_filtered %>%
  group_by(country, sex) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100) %>%
  ggplot(aes(x = country, y = suicide_percentage, fill = sex)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_y_continuous(labels = scales::percent) + 
  coord_flip() +
  scale_fill_manual(values=c("#FF4500", "#1E90FF"))

  labs(title = "Suicide proportion by gender", 
       x = "Country", 
       y = "", 
       fill = "Sex")

dev.new(width=40, height=40)
grid.arrange(country_time, gender_box, ncol=2)
  


data_filtered %>%
  group_by(country, age) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100) %>%
  ggplot(aes(x = country, y = suicide_percentage, fill = age)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Age ", 
       x = "Country", 
       y = "Suicide rate (%)", 
       fill = "Age")


# CORRELATION GDP AND SUICIDE RATE


country_mean_gdp <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_percentage = (sum(as.numeric(suicide_number)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))

ggplot(country_mean_gdp, aes(x = gdp_per_capita, y = suicide_percentage, col = continent)) + 
  geom_point() + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP (per capita) and Suicides per 100k", 
       subtitle = "Plot containing every country",
       x = "GDP (per capita)", 
       y = "Suicides per 100k", 
       col = "Continent") 


model_lin <- lm(suicide_percentage ~ gdp_per_capita, data = country_mean_gdp)
rSquared <- summary(model_lin)$r.squared
pVal_lin <- anova(model_lin)$'Pr(>F)'[1]

model_non_lin <- lm(suicide_percentage ~ gdp_per_capita + I(gdp_per_capita^2) + I(gdp_per_capita^3), data =  country_mean_gdp)
rSquared <- summary(model_non_lin)$r.squared
pVal_non_lin <- anova(model_non_lin)$'Pr(>F)'[1]


dev.new(width=40, height=40)
ggplot( country_mean_gdp, aes(x = gdp_per_capita, y = suicide_percentage, col = continent)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 1)) + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP and suicide rate", 
       subtitle = paste("Linear model : p-value = ",format(round(pVal_lin, 2), nsmall = 3)),
       x = "GDP (per capita)", 
       y = "Suicide rate (%)", 
       col = "Continent") + 
  theme(legend.position = "none")


dev.new(width=40, height=40)
ggplot( country_mean_gdp, aes(x = gdp_per_capita, y = suicide_percentage, col = continent)) + 
  geom_point() + 
  geom_smooth(method = "lm", aes(group = 1), formula = y ~ poly(x,3)) + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP and suicide rate", 
       subtitle = paste("Polynomial order 2 model : p-value = ", format(round(pVal_non_lin, 2), nsmall = 3)),
       x = "GDP", 
       y = "Suicide rate (%)", 
       col = "Continent") + 
  theme(legend.position = "none")
