library(tidyverse)
library(tidycovid19)
library(zoo)
theCountry='Mexico'
# Compare Mexico against the world
merged <- download_merged_data(cached = TRUE, silent = TRUE)

## New cases
### The world
w1 = merged %>%
  select(date,confirmed) %>%
  group_by(date) %>%
  summarise(confirmed=sum(confirmed, na.rm = TRUE)) %>%
  mutate(
    new_cases = confirmed - lag(confirmed),
    ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right"),
    region="World"
  ) %>%
  filter(!is.na(new_cases), !is.na(ave_new_cases))

### Mexico
m1 = merged %>%
  filter(iso3c == "MEX") %>%
  select(date,confirmed) %>%
  mutate(
    new_cases = confirmed - lag(confirmed),
    ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right"),
    region="Mexico"
  ) %>%
  filter(!is.na(new_cases), !is.na(ave_new_cases))

a1 = merge(x = w1, y = m1, by = 'date', all.x = TRUE)
a1 = a1 %>% select(date, new_cases.x,new_cases.y,ave_new_cases.x,ave_new_cases.y)
#a1 = mutate_all(a1, ~if_else(is.na(.), 0, .))
a1$new_cases.x = a1$new_cases.x/(max(a1$new_cases.x, na.rm = T) - min(a1$new_cases.x, na.rm = T))
a1$new_cases.y = a1$new_cases.y/(max(a1$new_cases.y, na.rm = T) - min(a1$new_cases.y, na.rm = T))
a1$ave_new_cases.x = a1$ave_new_cases.x/(max(a1$ave_new_cases.x, na.rm = T) - min(a1$ave_new_cases.x, na.rm = T))
a1$ave_new_cases.y = a1$ave_new_cases.y/(max(a1$ave_new_cases.y, na.rm = T) - min(a1$ave_new_cases.y, na.rm = T))
thisColours = c("Mundo"="#f04546","Mexico"="blue")
ggplot(data = a1, aes(x = date)) +
  #geom_bar(aes(y = new_cases.x, fill = "Mundo"), stat = "identity") +
  geom_line(aes(y = ave_new_cases.x, colour="Mundo")) +
  #geom_bar(aes(y = new_cases.y, fill="Mexico"), stat = "identity") +
  geom_line(aes(y = ave_new_cases.y,colour ="Mexico")) +
  ggtitle("New cases") +
  scale_colour_manual(name="Region", values = thisColours) +
  #scale_fill_manual(name="Region",  values=alpha(thisColours,0.5)) + 
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(caption = "En esta sobre nuevos casos solo importa la forma,
       \nestá normalizada.
       \nSon los nuevos casos del mundo contra los de México para ver
       \nqué tan en sincronía estamos con el mundo.")

## Daily deaths
### The world
w1 = merged %>%
  select(date,deaths) %>%
  group_by(date) %>%
  summarise(confirmed=sum(deaths, na.rm = TRUE)) %>%
  mutate(
    new_cases = confirmed - lag(confirmed),
    ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right"),
    region="World"
  ) %>%
  filter(!is.na(new_cases), !is.na(ave_new_cases))

### Mexico
m1 = merged %>%
  filter(iso3c == "MEX") %>%
  select(date,deaths) %>%
  mutate(
    new_cases = deaths - lag(deaths),
    ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right"),
    region="Mexico"
  ) %>%
  filter(!is.na(new_cases), !is.na(ave_new_cases))

a1 = merge(x = w1, y = m1, by = 'date', all.x = TRUE)
a1 = a1 %>% select(date, new_cases.x,new_cases.y,ave_new_cases.x,ave_new_cases.y)
#a1 = mutate_all(a1, ~if_else(is.na(.), 0, .))
a1$new_cases.x = a1$new_cases.x/(max(a1$new_cases.x, na.rm = T) - min(a1$new_cases.x, na.rm = T))
a1$new_cases.y = a1$new_cases.y/(max(a1$new_cases.y, na.rm = T) - min(a1$new_cases.y, na.rm = T))
a1$ave_new_cases.x = a1$ave_new_cases.x/(max(a1$ave_new_cases.x, na.rm = T) - min(a1$ave_new_cases.x, na.rm = T))
a1$ave_new_cases.y = a1$ave_new_cases.y/(max(a1$ave_new_cases.y, na.rm = T) - min(a1$ave_new_cases.y, na.rm = T))
thisColours = c("Mundo"="#f04546","Mexico"="blue")
ggplot(data = a1, aes(x = date)) +
  #geom_bar(aes(y = new_cases.x, fill = "Mundo"), stat = "identity") +
  geom_line(aes(y = ave_new_cases.x, colour="Mundo")) +
  #geom_bar(aes(y = new_cases.y, fill="Mexico"), stat = "identity") +
  geom_line(aes(y = ave_new_cases.y,colour ="Mexico")) +
  ggtitle(paste("Muertes en el mundo y en", theCountry)) +
  scale_colour_manual(name="Region", values = thisColours) +
  #scale_fill_manual(name="Region",  values=alpha(thisColours,0.5)) + 
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(caption = "Son números normalizados para ver el parecido (o no)\n
       de las gráficas.")

## Daily recovers
### The world
w1 = merged %>%
  select(date,recovered) %>%
  group_by(date) %>%
  summarise(confirmed=sum(recovered, na.rm = TRUE)) %>%
  mutate(
    new_cases = confirmed - lag(confirmed),
    ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right"),
    region="World"
  ) %>%
  filter(!is.na(new_cases), !is.na(ave_new_cases))

### Mexico
m1 = merged %>%
  filter(iso3c == "MEX") %>%
  select(date,recovered) %>%
  mutate(
    new_cases = recovered - lag(recovered),
    ave_new_cases = rollmean(new_cases, 7, na.pad=TRUE, align="right"),
    region="Mexico"
  ) %>%
  filter(!is.na(new_cases), !is.na(ave_new_cases))

a1 = merge(x = w1, y = m1, by = 'date', all.x = TRUE)
a1 = a1 %>% select(date, new_cases.x,new_cases.y,ave_new_cases.x,ave_new_cases.y)
#a1 = mutate_all(a1, ~if_else(is.na(.), 0, .))
a1$new_cases.x = a1$new_cases.x/(max(a1$new_cases.x, na.rm = T) - min(a1$new_cases.x, na.rm = T))
a1$new_cases.y = a1$new_cases.y/(max(a1$new_cases.y, na.rm = T) - min(a1$new_cases.y, na.rm = T))
a1$ave_new_cases.x = a1$ave_new_cases.x/(max(a1$ave_new_cases.x, na.rm = T) - min(a1$ave_new_cases.x, na.rm = T))
a1$ave_new_cases.y = a1$ave_new_cases.y/(max(a1$ave_new_cases.y, na.rm = T) - min(a1$ave_new_cases.y, na.rm = T))
thisColours = c("Mundo"="#f04546","Mexico"="blue")
ggplot(data = a1, aes(x = date)) +
  #geom_bar(aes(y = new_cases.x, fill = "Mundo"), stat = "identity") +
  geom_line(aes(y = ave_new_cases.x, colour="Mundo")) +
  #geom_bar(aes(y = new_cases.y, fill="Mexico"), stat = "identity") +
  geom_line(aes(y = ave_new_cases.y,colour ="Mexico")) +
  ggtitle(paste("Muertes en el mundo y en", theCountry)) +
  scale_colour_manual(name="Region", values = thisColours) +
  #scale_fill_manual(name="Region",  values=alpha(thisColours,0.5)) + 
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(caption = "Son números normalizados para ver el parecido (o no)\n
       de las gráficas.")

#' Place of Mexico by population or pop_density by
#' confirmed, ecdc_cases
#' deaths, ecdc_deaths
#' recovered
#' total_tests
#' positive_rate
#' hosp_patients
#' icu_patients
#' total_vaccinations
#' soc_dist
#' mov_rest
#' pub_health
#' gov_soc_econ
#' lockdown
#' 
#' How to use this:
#' "apple_mtr_driving"      "apple_mtr_walking"     
#' "apple_mtr_transit"      "gcmr_retail_recreation" "gcmr_grocery_pharmacy" 
#' "gcmr_parks"             "gcmr_transit_stations"  "gcmr_workplaces"       
#' "gcmr_residential"       "gtrends_score"          "gtrends_country_score" 
#' 
#' For each variable v1 get
#' i1 = v1 by date by country * 100000 / population
#' Show top 3, bottom 3, Mexico, 3 above Mexico, 3 below Mexico
#' and some distance indicator

m2mex = merged[merged$iso3c=="MEX",]
n = nrow(m2mex)
theNames = names(m2mex)
x3 = sapply(1:ncol(m2mex), function(c1){
  ifelse(test = n==sum(is.na(m2mex[,theNames[c1]]))
         , yes = print(paste(theNames[c1],"is empty"))
         ,  no = print(paste(theNames[c1],"has", sum(!is.na(m2mex[,theNames[c1]])),"not empty rows from", n)))
})

## Only for max date
m2 = merged %>%
  select(date, iso3c, country, population,
         confirmed, ecdc_cases,
         deaths, ecdc_deaths,
         recovered,
         total_tests,
         positive_rate,
         #hosp_patients,
         #icu_patients,
         total_vaccinations
  ) %>%
  filter(date == max(date)) %>%
  mutate(
    confirmedX100K = confirmed * 100000 / population,
    ecdc_casesX100K = ecdc_cases * 100000 / population,
    deathsX100K = deaths * 100000 / population,
    ecdc_deathsX100K = ecdc_deaths * 100000 / population,
    recoveredX100K = recovered * 100000 / population,
    total_testsX100K = total_tests * 100000 / population,
    positive_rateX100K = positive_rate * 100000 / population,
    #hosp_patientsX100K = hosp_patients * 100000 / population,
    #icu_patientsX100K = icu_patients * 100000 / population,
    total_vaccinationsX100K = total_vaccinations * 100000 / population
  ) %>%
  select(iso3c,country,confirmedX100K, 
         ecdc_casesX100K, deathsX100K, 
         ecdc_deathsX100K, recoveredX100K, 
         total_testsX100K, positive_rateX100K, 
         total_vaccinationsX100K)

## Mexican place for each metric
x3 = sapply(names(m2), function(thisMetric){
  c1 = m2[,c("iso3c","country",thisMetric)]
  c1 = c1[!is.na(c1[,thisMetric]),]
  if(nrow(c1) > 0 && nrow(c1[c1$iso3c=="MEX",thisMetric])==1) {
    c1 = c1[order(c1[,thisMetric]),]
    c1$place = seq(1:nrow(c1))
    mex.place = which(c1$iso3c=="MEX")
    print(paste("Mexico place:", mex.place,"for",thisMetric,"from",nrow(c1)))
    c1
  }
})
x3[sapply(x3, is.null)] <- NULL

## testing plots
tv=x3[["total_vaccinationsX100K"]]
tv=x3[["deathsX100K"]]
highlight=which(tv$iso3c=="MEX")
#tv[,"iso3c"] <- as.factor(tv[,"iso3c"])
bar.colors = c(rep(x="blue",highlight-1),"red",rep(x="blue",nrow(tv)-highlight))
ggplot(data = tv) +
  geom_bar(aes(x = reorder(country, place), y = tv[[3]], fill = as.factor(place)), 
           stat = "identity") +
  theme(axis.text.x = element_text(angle = 45),
        legend.position = "none") +
  scale_fill_manual(values = bar.colors)

