


# Data reading ------------------------------------------------------------
# # votes <- un_votes %>%
# #   inner_join(un_roll_calls, by = "rcid") %>% 
# #   mutate(year = year(date))
# wb <- read_dta("data/dataverse_files_aid/climate_aid_trade_data/data/raw/raw_trade/baci9517.dta")
# # this line takes super long to run
# # aid <- read_csv("~/Downloads/aid_all.csv")
# pwtn <- read_dta("data/dataverse_files_aid/climate_aid_trade_data/data/raw/raw_controls/pwt91.dta")
# # aid <- read_dta("data/dataverse_files_aid/climate_aid_trade_data/data/data_final/base_ijt_clean.dta")

aid <- read_dta("data/dataverse_files_aid/climate_aid_trade_data/data/data_final/base_ijt_ext_clean.dta")


oda <- read_csv("data/DAC-List-of-ODA-Recipients-for-reporting-2024-25-flows.csv")
pais <- read_csv("data/country_names_classification.csv")

countries <- pais %>%  full_join(oda) %>%  select(country_name, iso_name, numeric_country, 
                                                  type, country_type) %>% 
  mutate(country_type=case_when(is.na(country_type)~type, T~country_type)) %>% 
  filter(!is.na(country_type)) %>%  select(-type)

recipients <- countries %>% filter(country_type== "ODA-Recipients")
donors <- countries %>% filter(country_type== "DAC")

bd <-aid %>%  filter(isorecipient %in% recipients$iso_name) 

bd <- bd %>% select(year, isodonor, d_name, isorecipient, r_name, mitigation =miti,
                    adaptation= adapt, both, climate_aid = tot_aid, miti_deflat,
                    adapt_deflat, both_deflat, climate_aid_deflat= tot_aid_deflat,
                    voting_similarity = agree, imports=x_ijt_m_deflat,
                    exports= x_ijt_x_deflat, contig, comlang_off, colony, dist, popDonor,
                    popRecipient, VulnerabilityIndex, ReadinessIndex,GainIndex, 
                    gdp_donor = gdpwb_do, gdp_recipient = gdpwb_recipient,
                    popRecipient_millions =popwb_recipient, corruption, ruleoflaw, 
                    regulatoryquality, governmenteffectiveness, politicalstability, 
                    SIDS, LDC,  ODA, ODA_deflat, emissions_total =total_ghg_owid_do , 
                    emissions_percapita = ghg_cap_owid_do, european_donor =eu_do, 
                    i,j,t,ij, it, jt)

rm(aid, countries, donors, oda, pais, recipients)

y17 <- bd %>%  filter(year == 2017)


y17 %>%filter(climate_aid>0) %>% 
  mutate(both = climate_aid %>%  log()) %>% 
  filter(both>-10) %>%
  ggplot(aes(y = voting_similarity %>% log(), x= both  ))+
  geom_point()+
  geom_smooth(method = "lm")

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


lm1 <-summary(model1) 
lm2<-summary(model2)
summary(model3)
summary(model4)

stargazer( model2, model1, model4,model3, title = "Regression results", out = "Table_total.html", 
           dep.var.labels = c("Total Climate Aid", "Aadaptation and Mitigation Aid", "Aadaptation Aid", "Mitigation Aid"  ))


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



# H2 us----------------------------------------------------------------------

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

# sandbox -----------------------------------------------------------------
library(highcharter)
bd17 <- bd %>%  filter(year==2017)

aid17 <- bd17  %>% 
  select(d_name, mitigation, adaptation, both, climate_aid, ) %>% 
  gather(type, aid, mitigation:climate_aid) %>% 
  group_by(d_name, type) %>%  summarise(tot_aid =sum(aid, na.rm = T)) %>% 
  ungroup()


bd17  %>% 
  select(d_name, mitigation, adaptation, both) %>% 
  gather(type, aid, mitigation:both) %>% 
  group_by(d_name, type) %>%  
  summarise(tot_aid =sum(aid, na.rm = T)) %>% 
  group_by(d_name) 

bd17 %>%  select(d_name, mitigation:climate_aid) %>% 
  mutate(suma =mitigation+adaptation-both) %>% 
  group_by(d_name) %>%  summarise_all(sum)


aid17 %>% filter(type == "climate_aid") %>%  arrange(desc(tot_aid)) %>% 
  top_n(n = 5) %>%  
  mutate(aalgo = paste0(d_name, " (", tot_aid %>%  round() %>%  
                          scales::comma(), "), ")) %>% 
  summarise(paste0(aalgo,collapse = "")) %>% 
  pull()


top_cont<- aid17 %>% filter(type == "climate_aid") %>%  arrange(desc(tot_aid)) %>% 
  top_n(n =10) %>%  select(d_name)

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

# similitud promedi o
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
  # geom_text(data =bd %>%  group_by(d_name, year) %>%
  #             summarise(mean_similarity =mean(voting_similarity, na.rm= T)) %>% 
  #             ungroup() %>% 
  #             filter(d_name %in% c("Japan", "Germany", "France", "United States",
  #                                  "United Kingdom"
  #                                  # "Sweden", "Norway", "Netherlands",
  #                                  # "Australia", "Korea"
  #             )) %>% 
  #             filter(year == "2017"), 
  #           aes(label = d_name,
  #               x = year + 1, 
  #               # y = 2017, 
  #               color = "black"), check_overlap = T)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        # legend.position = "none",
        # plot.margin = unit(c(1,3,1,1), "lines"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = .8),
        text = element_text(family = "Times New Roman", size = 15))



bd %>%  filter(voting_similarity>.9, year==2017, climate_aid>0) %>%  
  select(d_name, r_name, climate_aid, voting_similarity, year ) %>% 
  hchart(hcaes(from = d_name, to= r_name, weight = climate_aid), type = "sankey")

y17 %>%  arrange(desc(climate_aid)) %>%  
  select( d_name, r_name, climate_aid, voting_similarity, popRecipient) %>%
  head(20) %>% 
  ggplot(aes(x = climate_aid, y = voting_similarity,
             color= d_name))+
  geom_point(alpha =.7)+
  ggrepel::geom_text_repel(aes(label = r_name), size = 2.7)+
  scale_color_manual(values = c("France" = "Blue", 
                                "Germany" = "Black",
                                "Japan" = "red"))+
  labs(title = "UNGA Voting similarity vs. Climate aid", subtitle = "Top 3 donors",
       x = "Climate aid (Millions USD)", color = "Donor country", y= "Voting similary")+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        # legend.position = "none",
        # plot.margin = unit(c(1,3,1,1), "lines"),
        text = element_text(family = "Times New Roman", size = 15))
# viridis::scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A")

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

bd17 %>%
  filter(climate_aid>0) %>% 
  ggplot( aes(x=climate_aid)) + 
  geom_density( linewidth =.9)+
  labs(title = "UNGA voting alignment distribution", subtitle = "Year 2017",
       x="Voting similariry", y = NULL)+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        # legend.position = "none",
        # plot.margin = unit(c(1,3,1,1), "lines"),
        text = element_text(family = "Times New Roman", size = 15))

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


