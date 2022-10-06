library(ConvergenceClubs)
library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(plm)
library(readr)




##Reading ghonnerea file and widening it
gonorrhealong<-read_xlsx('C:/Users/deanh/Downloads/AtlasPlusTableDatagon.xlsx')
gonorrheadata <- gonorrhealong %>% 
  as.data.frame() %>%
  rename(Y=names(.)[3]) %>%
  pivot_wider(names_from=Year, values_from=Y) %>%
  as.data.frame() %>%
  dplyr::select(State, rev(everything()))

##club convergence
findClubs(gonorrheadata, 
          dataCols = 2:ncol(gonorrheadata), 
          refCol = ncol(gonorrheadata), 
          unit_names = 1, 
          HACmethod = "AQSB",
          time_trim = 0.1)

clubsmerged2 <- mergeClubs(findClubs(gonorrheadata, 
                                     dataCols = 2:ncol(gonorrheadata),
                                     refCol = ncol(gonorrheadata), 
                                     unit_names = 1, 
                                     HACmethod = "AQSB", 
                                     time_trim = 0.1),
                           mergeDivergent = FALSE,
                           mergeMethod = "PS")

summary(clubsmerged2)

ggplot(gonorrhealong, aes(Year, CasesPerCapita, color=State)) +
  geom_line() +
  ggtitle("Gonnerhea Cases")+
  ylab("Cases Per 100,000") +
  theme(plot.title = element_text(hjust = 0.5))

##faceting by club and graphing. 
gonorrhealong2 <- gonorrhealong %>%
  mutate(Club = case_when(State %in% clubsmerged2$club1$unit_names ~'Club 1',
                          State %in% clubsmerged2$club2$unit_names ~'Club 2',
                          State %in% clubsmerged2$club3$unit_names ~'Club 3',
                          State %in% clubsmerged2$divergent$unit_names~'Divergent'))

ggplot(gonorrhealong2, aes(Year, CasesPerCapita, color=State)) +
  geom_line() +
  ylab("Cases Per 100,000") +
  ggtitle("Gonorrhea Clubs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(vars(Club)) 

#############CLUB AVG 

gonorrhealong3 <- gonorrhealong2 %>%
  group_by(Year, Club) %>%
  summarise(Avg=mean(CasesPerCapita))

ggplot(gonorrhealong3, aes(Year, Avg, color=Club)) +
  geom_line() +
  ylab("Cases Per 100,000") +
  ggtitle("Gonorrhea Clubs: Average Trends") +
  theme(plot.title = element_text(hjust = 0.5))

##Reading chlamydia file and widening it
Chlamydialong<-read_xlsx('C:/Users/deanh/Downloads/AtlasPlusTableDataChlamydia.xlsx')
Chlamydiadata <- Chlamydialong %>% 
  as.data.frame() %>%
  rename(Y=names(.)[3]) %>%
  pivot_wider(names_from=Year, values_from=Y) %>%
  as.data.frame() %>%
  dplyr::select(State, rev(everything()))

##club convergence
findClubs(Chlamydiadata, 
          dataCols = 2:ncol(Chlamydiadata), 
          refCol = ncol(Chlamydiadata), 
          unit_names = 1, 
          HACmethod = "AQSB",
          time_trim = 0.1)

clubsmerged1 <- mergeClubs(findClubs(Chlamydiadata, 
                             dataCols = 2:ncol(Chlamydiadata),
                             refCol = ncol(Chlamydiadata), 
                             unit_names = 1, 
                             HACmethod = "AQSB", 
                             time_trim = 0.1),
                   mergeDivergent = FALSE,
                   mergeMethod = "PS")

summary(clubsmerged1)

ggplot(Chlamydialong, aes(Year, CasesPerCapita, color=State)) +
  ggtitle("Chlamydia Cases") +
  ylab("Cases Per 100,000") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line()

##faceting by club and graphing. 
Chlamydialong2 <- Chlamydialong %>%
       mutate(Club = case_when(State %in% clubsmerged1$club1$unit_names ~'Club 1',
                                State %in% clubsmerged1$club2$unit_names ~'Club 2',
                                State %in% clubsmerged1$club3$unit_names ~'Club 3',
                                State %in% clubsmerged1$divergent$unit_names~'Divergent'))

ggplot(Chlamydialong2, aes(Year, CasesPerCapita, color=State)) +
  geom_line() +
  ylab("Cases Per 100,000") +
  ggtitle("Chlamydia Clubs") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(vars(Club)) 

Chlamydialong3<- Chlamydialong2 %>%
  group_by(Year, Club) %>%
  summarise(Avg=mean(CasesPerCapita))

#############CLUB AVG 

ggplot(Chlamydialong3, aes(Year, Avg, color=Club)) +
  geom_line() +
  ylab("Cases Per 100,000") +
  ggtitle("Chlamydia Clubs: Average Trends") +
  theme(plot.title = element_text(hjust = 0.5))

##############Reading syphilis file and widening it
syphlong<-read_xlsx('C:/Users/deanh/Downloads/AtlasPlusTableDatasyph.xlsx')
syphdata <- syphlong %>% 
  as.data.frame() %>%
  rename(Y=names(.)[3]) %>%
  pivot_wider(names_from=Year, values_from=Y) %>%
  as.data.frame() %>%
  dplyr::select(State, rev(everything()))

##########club convergence
findClubs(syphdata, 
          dataCols = 2:ncol(syphdata), 
          refCol = ncol(syphdata), 
          unit_names = 1, 
          HACmethod = "AQSB",
          time_trim = 0.07)

clubsmerged3 <- mergeClubs(findClubs(syphdata, 
                                     dataCols = 2:ncol(syphdata),
                                     refCol = ncol(syphdata), 
                                     unit_names = 1, 
                                     HACmethod = "AQSB", 
                                     time_trim = 0.07),
                           mergeDivergent = FALSE,
                           mergeMethod = "PS")

summary(clubsmerged3)

########Syph casses

ggplot(syphlong, aes(Year, CasesPerCapita, color=State)) +
  ggtitle("Syphilis Cases") +
  ylab("Cases Per 100,000") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_line()

##########faceting by club and graphing. 
syphlong2 <- syphlong %>%
  mutate(Club = case_when(State %in% clubsmerged3$club1$unit_names ~'Club 1',
                          State %in% clubsmerged3$club2$unit_names ~'Club 2',
                          State %in% clubsmerged3$club3$unit_names ~'Club 3',
                          State %in% clubsmerged3$divergent$unit_names~'Divergent'))

ggplot(syphlong2, aes(Year, CasesPerCapita, color=State)) +
  geom_line() +
  ylab("Cases Per 100,000") +
  ggtitle("Syphilis Clubs") +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(vars(Club)) 

#############CLUB AVG SYPH

syphlong3<- syphlong2 %>%
  group_by(Year, Club) %>%
  summarise(Avg=mean(CasesPerCapita))

ggplot(syphlong3, aes(Year, Avg, color=Club)) +
  geom_line() +
  ylab("Cases Per 100,000") +
  ggtitle("Syphilis Clubs: Average Trends") +
  theme(plot.title = element_text(hjust = 0.5))



############# Social Determinants of Health

fetable <- read_xlsx("C:/Users/deanh/Downloads/fetable.xlsx")

fetablec <- fetable %>%
  mutate(Club = case_when(State %in% clubsmerged1$club1$unit_names ~'Club 1',
                          State %in% clubsmerged1$club2$unit_names ~'Club 2',
                          State %in% clubsmerged1$club3$unit_names ~'Club 3',
                          State %in% clubsmerged1$divergent$unit_names~'Divergent')) %>%
  group_by(Year, Club) %>%
  summarise(AerageUninsured=mean(Uninsured), avgedu=mean(`Population 25 years and older w/o HS diploma`))

ggplot(fetablec, aes(Year, AerageUninsured, color=Club)) +
  geom_line() +
  ylab("Uninsured Percentage") +
  ggtitle("Chlamydia Clubs: Average Uninsurance Rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_binned()

ggplot(fetablec, aes(Year, avgedu, color=Club)) +
  geom_line() +
  ylab("Pop. W/O High School Diploma Percentage") +
  ggtitle("Chlamydia Clubs: Average Lack of Education Rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_binned()


fetableg <- fetable %>%
  mutate(Club = case_when(State %in% clubsmerged2$club1$unit_names ~'Club 1',
                          State %in% clubsmerged2$club2$unit_names ~'Club 2',
                          State %in% clubsmerged2$club3$unit_names ~'Club 3',
                          State %in% clubsmerged2$divergent$unit_names~'Divergent')) %>%
  group_by(Year, Club) %>%
  summarise(AerageUninsured=mean(Uninsured), avgedu=mean(`Population 25 years and older w/o HS diploma`))

ggplot(fetableg, aes(Year, AerageUninsured, color=Club)) +
  geom_line() +
  ylab("Uninsured Percentage") +
  ggtitle("Gonorrhea Glubs: Average Uninsurance Rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_binned()

ggplot(fetableg, aes(Year, avgedu, color=Club)) +
  geom_line() +
  ylab("Pop. W/O High School Diploma Percentage") +
  ggtitle("Gonorrhea Clubs: Average Lack of Education Rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_binned()

fetables <- fetable %>%
mutate(Club = case_when(State %in% clubsmerged3$club1$unit_names ~'Club 1',
                        State %in% clubsmerged3$club2$unit_names ~'Club 2',
                        State %in% clubsmerged3$club3$unit_names ~'Club 3',
                        State %in% clubsmerged3$divergent$unit_names~'Divergent')) %>%
  group_by(Year, Club) %>%
  summarise(AerageUninsured=mean(Uninsured), avgedu=mean(`Population 25 years and older w/o HS diploma`))

ggplot(fetables, aes(Year, AerageUninsured, color=Club)) +
  geom_line() +
  ylab("Uninsured Percentage") +
  ggtitle("Syphilis Clubs: Average Uninsurance Rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_binned()

ggplot(fetables, aes(Year, avgedu, color=Club)) +
  geom_line() +
  ylab("Pop. W/O High School Diploma Percentage") +
  ggtitle("Syphilis Clubs: Average Lack of Education Rates") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_binned()


attach(fetable)

fe1 <- plm(Chlamydia ~ `Vacant housing`+Uninsured +
      `Population 25 years and older w/o HS diploma`+`Households living below the federal poverty level`+
      `Population Density`, data = fetable, index = c("State", "Year"), model = "within")


fe2 <- plm(Gonorrhea ~ `Vacant housing`+Uninsured +
             `Population 25 years and older w/o HS diploma`+`Households living below the federal poverty level`+
             `Population Density`, data = fetable, index = c("State", "Year"), model = "within")


fe3 <- plm(`Primary and Secondary Syphilis` ~ `Vacant housing`+Uninsured +
             `Population 25 years and older w/o HS diploma`+`Households living below the federal poverty level`+
             `Population Density`, data = fetable, index = c("State", "Year"), model = "within")


stargazer(fe1, fe2, fe3)

################

fundingpercap <- read_csv("C:/Users/deanh/Downloads/fundingpercap.csv")

chlam2019 <- Chlamydialong2 %>%
  filter(Year==2019) %>%
  mutate(FundingPerCapita=round(fundingpercap$`Funding Per.Cap.`,2)) %>%
  filter(State!="District of Columbia")

ggplot(chlam2019, aes(FundingPerCapita, CasesPerCapita, color=Club)) +
  geom_point() + xlab("Funding Per Capita") + ylab("Cases Per Capita") + ggtitle("Funding vs Chlamydia Cases")+
  theme(plot.title = element_text(hjust = 0.5))

gon2019 <- gonorrhealong2 %>%
  filter(Year==2019) %>%
  mutate(FundingPerCapita=round(fundingpercap$`Funding Per.Cap.`,2)) %>%
  filter(State!="District of Columbia")

ggplot(gon2019, aes(FundingPerCapita, CasesPerCapita, color=Club)) +
  geom_point() + xlab("Funding Per Capita") + ylab("Cases Per Capita") + ggtitle("Funding vs Gonorrhea Cases")+
  theme(plot.title = element_text(hjust = 0.5))

syph2019 <- syphlong2 %>%
  filter(Year==2019) %>%
  mutate(FundingPerCapita=round(fundingpercap$`Funding Per.Cap.`,2))%>%
  filter(State!="District of Columbia")

ggplot(syph2019, aes(FundingPerCapita, CasesPerCapita, color=Club)) +
  geom_point() + xlab("Funding Per Capita") + ylab("Cases Per Capita") + ggtitle("Funding vs Syphilis Cases")+
  theme(plot.title = element_text(hjust = 0.5))


