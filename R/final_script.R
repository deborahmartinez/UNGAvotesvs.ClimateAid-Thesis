# Libraries --------------------------------------------------------------
library(dplyr)
library(unvotes)
library(lubridate)
library(tidyverse)
library(haven)
library(stargazer)


# data load ---------------------------------------------------------------

bd <- read_csv("data/model_data.csv")
bd17 <- bd %>%  filter(year==2017)

# Descriptive statistics --------------------------------------------------

# Figure 1
aid17 <- bd17  %>% 
  select(d_name, mitigation, adaptation, both, climate_aid, ) %>% 
  gather(type, aid, mitigation:climate_aid) %>% 
  group_by(d_name, type) %>%  summarise(tot_aid =sum(aid, na.rm = T)) %>% 
  ungroup()

aid17  %>% inner_join(top_cont) %>% 
  filter(type %in% c("adaptation", "mitigation")) %>% 
  mutate(type= case_when(type == "adaptation"~"Adaptation aid",
                         type == "mitigation"~"Mitigation aid")) %>% 
  ggplot(aes(x = fct_reorder(d_name, -tot_aid), y = tot_aid,group = type, fill = type))+
  geom_col(alpha= .9, width = .7)+
  scale_fill_manual(values = c("Mitigation aid" = "#F26419",
                               "Adaptation aid"= "#33658A"))+
  # geom_line(data=aid17  %>% inner_join(top_cont) %>% 
  #          filter(type == "climate_aid"))+
  geom_point(data=aid17  %>% inner_join(top_cont) %>% 
               filter(type == "climate_aid")) +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  labs(x = NULL, fill = NULL, y= "Millions USD", 
       title = "Amount of adaptation and mitigation aid",
       subtitle = "Top 10 countries that allocate the most aid in 2017")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = .8),
        text = element_text(family = "Times New Roman", size = 15))

# Fiigure 2
bd %>%  group_by(d_name, year) %>%
  summarise(mean_similarity =sum(climate_aid, na.rm= T)) %>% 
  ungroup() %>% 
  filter(d_name %in% c("Japan", "Germany", "France", "United States",
                       "United Kingdom"
                       # "Sweden", "Norway", "Netherlands",
                       # "Australia", "Korea"
  )) %>% 
  ggplot(aes(x = year, y= mean_similarity, color= d_name))+
  geom_point()+
  scale_color_manual(values = c("Japan" = "#3E5641",
                                "Germany" = "#687736",
                                "France" = "#91972A", 
                                "United States" = "#B6C454",
                                "United Kingdom" = "#D8D174"))+
  geom_line(linewidth =.9)+
  labs(x = NULL, color = NULL, y= "Millions USD", 
       title = "Amount of Climate aid per year",
       subtitle = "Top 5 donors")+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = .8),
        text = element_text(family = "Times New Roman", size = 15))

# Figure 3
bd %>%  group_by(d_name, year) %>%
  summarise(mean_similarity =mean(voting_similarity, na.rm= T)) %>% 
  ungroup() %>% 
  filter(d_name %in% c("Japan", "Germany", "France", "United States",
                       "United Kingdom"
                       # "Sweden", "Norway", "Netherlands",
                       # "Australia", "Korea"
  )) %>% 
  mutate(d_name= factor(d_name, c("Japan", "Germany", "France",
                                  "United Kingdom", "United States"))) %>% 
  ggplot(aes(x = year, y= mean_similarity, color= d_name))+
  scale_color_manual(values = c("Japan" = "#062637",
                                "Germany" = "#0D4C6E",
                                "France" = "#3D87AE", 
                                "United States" = "#A5D6F1",
                                "United Kingdom" = "#6DC1EE"))+
  labs(x = NULL, color = NULL, y= "Voting similarity", 
       title = "Average voting similarity at the \nUNGA per year",
       subtitle = "Top 5 donors of climate aid")+
  geom_line(linewidth =.9)+
  geom_point()+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        # legend.position = "none",
        # plot.margin = unit(c(1,3,1,1), "lines"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = .8),
        text = element_text(family = "Times New Roman", size = 15))


# Figure
bd %>% mutate(year = as.character(year)) %>% 
  ggplot( aes(x=voting_similarity, color = year, fill= year, group = year)) + 
  geom_density(linewidth =.7, alpha= .2)+
  labs(title = "UNGA voting alignment distribution", 
       x="Voting similariry", y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        # plot.margin = unit(c(1,3,1,1), "lines"),
        text = element_text(family = "Times New Roman", size = 15))

# Figure
bd17  %>% 
  select(d_name,r_name, mitigation, adaptation, both, climate_aid, voting_similarity) %>% 
  gather(type, aid, mitigation:climate_aid) %>%
  mutate(aid = aid %>%  log,
         type= case_when(type == "adaptation"~"Adaptation aid",
                         type == "both"~"Adaptation &\n mitigation overlap",
                         type == "climate_aid"~"Climate aid",
                         type == "mitigation"~"Mitigation aid")) %>% 
  filter(aid>0) %>% 
  ggplot(aes(x = voting_similarity , y = aid, color= type ))+
  geom_point(alpha= .4)+
  facet_wrap(~type)+
  labs(title = "Different types of aid vs. UNGA voting alignment distribution", subtitle = "Year 2017",
       x="Voting similariry", y = "Aid (log)")+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        # plot.margin = unit(c(1,3,1,1), "lines"),
        text = element_text(family = "Times New Roman", size = 15))

# regression models and tables -------------------------------------------

# H1 test ----------------------------------------------------------------------

model1 <- lm(both ~voting_similarity + gdp_recipient + 
               imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
               ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
               LDC+ODA+emissions_total+emissions_percapita,
             data = bd%>% filter(year>2009))

model2 <- lm(climate_aid ~voting_similarity + gdp_recipient + gdp_donor+
               imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
               ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
               LDC+ODA+emissions_total+emissions_percapita,
             data = bd)


model3 <- lm(mitigation ~voting_similarity + gdp_recipient + gdp_donor+
               imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
               ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
               LDC+ODA+emissions_total+emissions_percapita,
             data = bd %>% filter(year>2009))

model4 <- lm(adaptation ~voting_similarity + gdp_recipient + gdp_donor+
               imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
               ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
               LDC+ODA+emissions_total+emissions_percapita,
             data = bd%>% filter(year>2009))

# h1 table
stargazer( model2, model1, model4,model3, title = "Regression results", out = "Table_total.html", 
           dep.var.labels = c("Total Climate Aid", "Aadaptation and Mitigation Aid", "Aadaptation Aid", "Mitigation Aid"  ))


# Model without controls
model11 <- lm(both ~voting_similarity ,
              data = bd%>% filter(year>2009))

model21 <- lm(climate_aid ~voting_similarity ,
              data = bd)


model31 <- lm(mitigation ~voting_similarity ,
              data = bd%>% filter(year>2009))

model41 <- lm(adaptation ~voting_similarity,
              data = bd%>% filter(year>2009))


stargazer( model21, model11, model41,model31,
           title = "Regression results without control variables", 
           out = "no_controls.html", 
           dep.var.labels= "Climate aid",
           column.labels = c("Total",
                             "Overlap",
                             "Aadaptation", "Mitigation"  ))
# H2 test -----------------------------------------------------------------

jap <- lm(climate_aid ~voting_similarity + gdp_recipient + gdp_donor+
            imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
            ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
            LDC+ODA+emissions_total+emissions_percapita,
          data = bd %>%  filter(d_name == "Japan"))

jap1 <- lm(adaptation ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(d_name == "Japan",year>2009))


jap2 <- lm(mitigation ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(d_name == "Japan",year>2009))
jap3 <- lm(both ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(d_name == "Japan",year>2009))

ger <- lm(climate_aid ~voting_similarity + gdp_recipient + gdp_donor+
            imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
            ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
            LDC+ODA+emissions_total+emissions_percapita,
          data = bd %>%  filter(d_name == "Germany"))
ger1 <- lm(adaptation ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(d_name == "Germany",year>2009))
ger2 <- lm(mitigation ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(d_name == "Germany",year>2009))
ger3 <- lm(both ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(d_name == "Germany",year>2009))

fra <- lm(climate_aid ~voting_similarity + gdp_recipient + gdp_donor+
            imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
            ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
            LDC+ODA+emissions_total+emissions_percapita,
          data = bd %>%  filter(d_name == "France"))

fra1 <- lm(adaptation ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(d_name == "France",year>2009))
fra2 <- lm(mitigation ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(d_name == "France",year>2009))
fra3 <- lm(both ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(d_name == "France",year>2009))

us1 <- lm(climate_aid ~voting_similarity + gdp_recipient + gdp_donor+
            imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
            ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
            LDC+ODA+emissions_total+emissions_percapita,
          data = bd %>%  filter(d_name == "United States"))


us2 <-lm(adaptation ~voting_similarity + gdp_recipient + gdp_donor+
           imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
           ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
           LDC+ODA+emissions_total+emissions_percapita,
         data = bd %>%  filter(d_name == "United States",year>2009)) 
us3<- lm(mitigation ~voting_similarity + gdp_recipient + gdp_donor+
           imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
           ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
           LDC+ODA+emissions_total+emissions_percapita,
         data = bd %>%  filter(d_name == "United States",year>2009)) 

us4<- lm(both ~voting_similarity + gdp_recipient + gdp_donor+
           imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
           ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
           LDC+ODA+emissions_total+emissions_percapita,
         data = bd %>%  filter(d_name == "United States",year>2009)) 

stargazer( us1, us2, us3, us4, model2, title = "Regression results for the U.S ", 
           out = "us.html",column.labels = c("Total", "Adaptation",
                                             "Mitigation", "Overlap", "Original model for comparisson" ),
           dep.var.labels = c("Climate Aid"  ))

stargazer( jap, jap1, jap2,jap3, title = "Regression results for Japan ", 
           out = "japan.html",column.labels = c("Total", "Adaptation",
                                                "Mitigation", "Overlap" ),
           dep.var.labels = c("Climate Aid"  ))

stargazer( ger, ger1, ger2,ger3, title = "Regression results for Germany ", 
           out = "germany.html",column.labels = c("Total", "Adaptation",
                                                  "Mitigation", "Overlap" ),
           dep.var.labels = c("Climate Aid"  ))

stargazer( fra, fra1, fra2,fra3, title = "Regression results for France ", 
           out = "francia.html",column.labels = c("Total", "Adaptation",
                                                  "Mitigation", "Overlap" ),
           dep.var.labels = c("Climate Aid"  ))



# H3 test -----------------------------------------------------------------


eu <- lm(climate_aid ~voting_similarity + gdp_recipient + gdp_donor+
           imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
           ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
           LDC+ODA+emissions_total+emissions_percapita,
         data = bd %>%  filter(european_donor==1))
noeu <- lm(climate_aid ~voting_similarity + gdp_recipient + gdp_donor+
             imports+exports+contig+comlang_off+colony+dist + popRecipient+VulnerabilityIndex+
             ReadinessIndex+corruption+ruleoflaw+regulatoryquality+politicalstability+SIDS+
             LDC+ODA+emissions_total+emissions_percapita,
           data = bd %>%  filter(european_donor!=1))


stargazer( eu,noeu, model2, title = "Regression results for H3 ", 
           out = "h3.html",column.labels = c("European donors",
                                             "Non european donors",
                                             "Original model"  ),
           dep.var.labels = c("Climate Aid" ))


