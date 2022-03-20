## Bocas plankton community - Univariate GLMs
## Date created: 01.07.22
## Date updated: 01.07.22
## Run in R 4.1.1

library(MASS)

# load data
data = read.csv("data/combined_counts.csv")
data$Date = as.Date(data$Date,"%m/%d/%y")
data$Site = factor(data$Site,
                   levels = c("STRI Point",
                              "Cristobal",
                              "Pastores"))
data$Depth_ctgry = factor(data$Depth_ctgry)

## PTEROPODS
######
glm_pter_1 = glm.nb(pteropods ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=50)) #increase to 50 so model converges
summary(glm_pter_1) # AIC = 1682.9
1 - (glm_pter_1$deviance/glm_pter_1$null.deviance) # = 0.266

glm_pter_2 = glm.nb(pteropods ~ Depth_ctgry + 
                        DO + 
                        temp_C + 
                        salinity +
                        chlorophyll +
                        DOM + 
                        DO:temp_C + 
                        DO:salinity + 
                        #DO:chlorophyll + 
                        DO:DOM + 
                        temp_C:salinity + 
                        temp_C:chlorophyll + 
                        temp_C:DOM + 
                        salinity:chlorophyll + 
                        salinity:DOM + 
                        chlorophyll:DOM,
                      data = data,
                      link = log,
                      control=glm.control(maxit=50)) #increase to 50 so model converges
summary(glm_pter_2) # AIC = 1681.1
1 - (glm_pter_2$deviance/glm_pter_2$null.deviance) # = 0.266

glm_pter_3 = glm.nb(pteropods ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      #salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=50)) #increase to 50 so model converges
summary(glm_pter_3) # AIC = 1679.4
1 - (glm_pter_3$deviance/glm_pter_3$null.deviance) # = 0.265

glm_pter_4 = glm.nb(pteropods ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      #temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      #salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=50)) #increase to 50 so model converges
summary(glm_pter_4) # AIC = 1678.2
1 - (glm_pter_4$deviance/glm_pter_4$null.deviance) # = 0.262

glm_pter_5 = glm.nb(pteropods ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      #temp_C:salinity + 
                      temp_C:chlorophyll + 
                      #temp_C:DOM + 
                      salinity:chlorophyll + 
                      #salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=50)) #increase to 50 so model converges
summary(glm_pter_5) # AIC = 1677.7
1 - (glm_pter_5$deviance/glm_pter_5$null.deviance) # = 0.228

plot(residuals(glm_pter_5)); abline(h=0)
######

# CHAETOGNATHS
######
glm_chaet_1 = glm.nb(chaetognaths ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_chaet_1) # AIC = 2533.6
1 - (glm_chaet_1$deviance/glm_chaet_1$null.deviance) # = 0.244

glm_chaet_2 = glm.nb(chaetognaths ~ #Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       DO:salinity + 
                       DO:chlorophyll + 
                       DO:DOM + 
                       temp_C:salinity + 
                       temp_C:chlorophyll + 
                       temp_C:DOM + 
                       salinity:chlorophyll + 
                       salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_chaet_2) # AIC = 2531.6
1 - (glm_chaet_2$deviance/glm_chaet_2$null.deviance) # = 0.244

glm_chaet_3 = glm.nb(chaetognaths ~ #Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       DO:salinity + 
                       #DO:chlorophyll + 
                       DO:DOM + 
                       temp_C:salinity + 
                       temp_C:chlorophyll + 
                       temp_C:DOM + 
                       salinity:chlorophyll + 
                       salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_chaet_3) # AIC = 2529.6
1 - (glm_chaet_3$deviance/glm_chaet_3$null.deviance) # = 0.244

glm_chaet_4 = glm.nb(chaetognaths ~ #Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       DO:salinity + 
                       #DO:chlorophyll + 
                       DO:DOM + 
                       #temp_C:salinity + 
                       temp_C:chlorophyll + 
                       temp_C:DOM + 
                       salinity:chlorophyll + 
                       salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_chaet_4) # AIC = 2527.7
1 - (glm_chaet_4$deviance/glm_chaet_4$null.deviance) # = 0.244

glm_chaet_5 = glm.nb(chaetognaths ~ #Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       DO:salinity + 
                       #DO:chlorophyll + 
                       DO:DOM + 
                       #temp_C:salinity + 
                       temp_C:chlorophyll + 
                       temp_C:DOM + 
                       salinity:chlorophyll + 
                       #salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_chaet_5) # AIC = 2525.8
1 - (glm_chaet_5$deviance/glm_chaet_5$null.deviance) # = 0.243

glm_chaet_6 = glm.nb(chaetognaths ~ #Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       #DO:salinity + 
                       #DO:chlorophyll + 
                       DO:DOM + 
                       #temp_C:salinity + 
                       temp_C:chlorophyll + 
                       temp_C:DOM + 
                       salinity:chlorophyll + 
                       #salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_chaet_6) # AIC = 2523.8
1 - (glm_chaet_6$deviance/glm_chaet_6$null.deviance) # = 0.243

glm_chaet_7 = glm.nb(chaetognaths ~ #Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       #DO:salinity + 
                       #DO:chlorophyll + 
                       DO:DOM + 
                       #temp_C:salinity + 
                       #temp_C:chlorophyll + 
                       temp_C:DOM + 
                       salinity:chlorophyll + 
                       #salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_chaet_7) # AIC = 2522.7
1 - (glm_chaet_7$deviance/glm_chaet_7$null.deviance) # = 0.241

glm_chaet_8 = glm.nb(chaetognaths ~ #Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       #DO:salinity + 
                       #DO:chlorophyll + 
                       DO:DOM + 
                       #temp_C:salinity + 
                       #temp_C:chlorophyll + 
                       #temp_C:DOM + 
                       salinity:chlorophyll + 
                       #salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_chaet_8) # AIC = 2522.7
1 - (glm_chaet_8$deviance/glm_chaet_8$null.deviance) # = 0.237

plot(residuals(glm_chaet_8)); abline(h=0)
######

# LARVACEANS
######
glm_larv_1 = glm.nb(larvaceans ~ Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       DO:salinity + 
                       DO:chlorophyll + 
                       DO:DOM + 
                       temp_C:salinity + 
                       temp_C:chlorophyll + 
                       temp_C:DOM + 
                       salinity:chlorophyll + 
                       salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_larv_1) # AIC = 2744.1
1 - (glm_larv_1$deviance/glm_larv_1$null.deviance) # = 0.415

glm_larv_2 = glm.nb(larvaceans ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_larv_2) # AIC = 2742.1
1 - (glm_larv_2$deviance/glm_larv_2$null.deviance) # = 0.415

glm_larv_3 = glm.nb(larvaceans ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_larv_3) # AIC = 2740.2
1 - (glm_larv_3$deviance/glm_larv_3$null.deviance) # = 0.415

glm_larv_4 = glm.nb(larvaceans ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_larv_4) # AIC = 2738.3
1 - (glm_larv_4$deviance/glm_larv_4$null.deviance) # = 0.415

glm_larv_5 = glm.nb(larvaceans ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      #temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_larv_5) # AIC = 2738.8
1 - (glm_larv_5$deviance/glm_larv_5$null.deviance) # = 0.410

glm_larv_6 = glm.nb(larvaceans ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      #temp_C:chlorophyll + 
                      temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_larv_6) # AIC = 2739.1
1 - (glm_larv_6$deviance/glm_larv_6$null.deviance) # = 0.406

glm_larv_7 = glm.nb(larvaceans ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      #temp_C:chlorophyll + 
                      temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase to 100 so model converges
summary(glm_larv_7) # AIC = 2739.1
1 - (glm_larv_7$deviance/glm_larv_7$null.deviance) # = 0.406

plot(residuals(glm_larv_7))
######

# COPEPODS
######
data_nona = subset.data.frame(data, is.na(data$copepods)==F)
glm_cop_1 = glm(copepods ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) #increase so model converges
summary(glm_cop_1) # AIC = 4358.1
1 - (glm_cop_1$deviance/glm_cop_1$null.deviance) # = 0.217

glm_cop_2 = glm(copepods ~ Depth_ctgry + 
                  DO + 
                  temp_C + 
                  salinity +
                  chlorophyll +
                  DOM + 
                  DO:temp_C + 
                  DO:salinity + 
                  DO:chlorophyll + 
                  DO:DOM + 
                  temp_C:salinity + 
                  temp_C:chlorophyll + 
                  temp_C:DOM + 
                  salinity:chlorophyll + 
                  salinity:DOM, # + 
                  #chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) #increase so model converges
summary(glm_cop_2) # AIC = 4356.2
1 - (glm_cop_2$deviance/glm_cop_2$null.deviance) # = 0.217

glm_cop_3 = glm(copepods ~ Depth_ctgry + 
                  DO + 
                  temp_C + 
                  salinity +
                  chlorophyll +
                  DOM + 
                  DO:temp_C + 
                  DO:salinity + 
                  DO:chlorophyll + 
                  DO:DOM + 
                  temp_C:salinity + 
                  temp_C:chlorophyll + 
                  #temp_C:DOM + 
                  salinity:chlorophyll + 
                  salinity:DOM, # + 
                #chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) #increase so model converges
summary(glm_cop_3) # AIC = 4354.3
1 - (glm_cop_3$deviance/glm_cop_3$null.deviance) # = 0.216

glm_cop_4 = glm(copepods ~ Depth_ctgry + 
                  DO + 
                  temp_C + 
                  salinity +
                  chlorophyll +
                  DOM + 
                  DO:temp_C + 
                  #DO:salinity + 
                  DO:chlorophyll + 
                  DO:DOM + 
                  temp_C:salinity + 
                  temp_C:chlorophyll + 
                  #temp_C:DOM + 
                  salinity:chlorophyll + 
                  salinity:DOM, # + 
                #chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) #increase so model converges
summary(glm_cop_4) # AIC = 4353.1
1 - (glm_cop_4$deviance/glm_cop_4$null.deviance) # = 0.214

glm_cop_5 = glm(copepods ~ Depth_ctgry + 
                  DO + 
                  temp_C + 
                  salinity +
                  chlorophyll +
                  DOM + 
                  DO:temp_C + 
                  #DO:salinity + 
                  DO:chlorophyll + 
                  DO:DOM + 
                  temp_C:salinity + 
                  temp_C:chlorophyll + 
                  #temp_C:DOM + 
                  salinity:chlorophyll, # + 
                  #salinity:DOM, # + 
                #chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) #increase so model converges
summary(glm_cop_5) # AIC = 4351.4
1 - (glm_cop_5$deviance/glm_cop_5$null.deviance) # = 0.213

glm_cop_6 = glm(copepods ~ Depth_ctgry + 
                  DO + 
                  temp_C + 
                  salinity +
                  chlorophyll +
                  DOM + 
                  DO:temp_C + 
                  #DO:salinity + 
                  DO:chlorophyll + 
                  #DO:DOM + 
                  temp_C:salinity + 
                  temp_C:chlorophyll + 
                  #temp_C:DOM + 
                  salinity:chlorophyll, # + 
                #salinity:DOM, # + 
                #chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) #increase so model converges
summary(glm_cop_6) # AIC = 4350.2
1 - (glm_cop_6$deviance/glm_cop_6$null.deviance) # = 0.211

glm_cop_7 = glm(copepods ~ Depth_ctgry + 
                  DO + 
                  temp_C + 
                  salinity +
                  chlorophyll +
                  #DOM + 
                  DO:temp_C + 
                  #DO:salinity + 
                  DO:chlorophyll + 
                  #DO:DOM + 
                  temp_C:salinity + 
                  temp_C:chlorophyll + 
                  #temp_C:DOM + 
                  salinity:chlorophyll, # + 
                #salinity:DOM, # + 
                #chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) #increase so model converges
summary(glm_cop_7) # AIC = 4348.4
1 - (glm_cop_7$deviance/glm_cop_7$null.deviance) # = 0.211

glm_cop_8 = glm(copepods ~ Depth_ctgry + 
                  DO + 
                  temp_C + 
                  salinity +
                  chlorophyll +
                  #DOM + 
                  DO:temp_C + 
                  #DO:salinity + 
                  DO:chlorophyll + 
                  #DO:DOM + 
                  temp_C:salinity + 
                  temp_C:chlorophyll, # + 
                  #temp_C:DOM + 
                  #salinity:chlorophyll, # + 
                #salinity:DOM, # + 
                #chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) #increase so model converges
summary(glm_cop_8) # AIC = 4348.9
1 - (glm_cop_8$deviance/glm_cop_8$null.deviance) # = 0.203

glm_cop_9 = glm(copepods ~ Depth_ctgry + 
                  DO + 
                  temp_C + 
                  salinity +
                  chlorophyll +
                  #DOM + 
                  DO:temp_C + 
                  #DO:salinity + 
                  #DO:chlorophyll + 
                  #DO:DOM + 
                  temp_C:salinity + 
                  temp_C:chlorophyll, # + 
                #temp_C:DOM + 
                #salinity:chlorophyll, # + 
                #salinity:DOM, # + 
                #chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) #increase so model converges
summary(glm_cop_9) # AIC = 4350.4
1 - (glm_cop_9$deviance/glm_cop_9$null.deviance) # = 0.193

plot(residuals(glm_cop_9)); abline(h=0)
######

# BIVALVES
######
glm_biv_1 = glm.nb(bivalves ~ Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM + 
                     chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_1) # AIC = 3645.8
1 - (glm_biv_1$deviance/glm_biv_1$null.deviance) # = 0.236

glm_biv_2 = glm.nb(bivalves ~ Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     #DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM + 
                     chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_2) # AIC = 3643.8
1 - (glm_biv_2$deviance/glm_biv_2$null.deviance) # = 0.236

glm_biv_3 = glm.nb(bivalves ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     #DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM + 
                     chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_3) # AIC = 3641.9
1 - (glm_biv_3$deviance/glm_biv_3$null.deviance) # = 0.235

glm_biv_4 = glm.nb(bivalves ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     #DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM, # + 
                     #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_4) # AIC = 3640.4
1 - (glm_biv_4$deviance/glm_biv_4$null.deviance) # = 0.234

glm_biv_5 = glm.nb(bivalves ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     #DO:salinity + 
                     #DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_5) # AIC = 3640.7
1 - (glm_biv_5$deviance/glm_biv_5$null.deviance) # = 0.229

glm_biv_6 = glm.nb(bivalves ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     #DO:salinity + 
                     #DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     #salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_6) # AIC = 3640
1 - (glm_biv_6$deviance/glm_biv_6$null.deviance) # = 0.226

glm_biv_7 = glm.nb(bivalves ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     #DO:salinity + 
                     #DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     #temp_C:chlorophyll + 
                     temp_C:DOM + 
                     #salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_7) # AIC = 3639.9
1 - (glm_biv_7$deviance/glm_biv_7$null.deviance) # = 0.221

glm_biv_8 = glm.nb(bivalves ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     #chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     #DO:salinity + 
                     #DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     #temp_C:chlorophyll + 
                     temp_C:DOM + 
                     #salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_8) # AIC = 3639.6
1 - (glm_biv_8$deviance/glm_biv_8$null.deviance) # = 0.217

glm_biv_9 = glm.nb(bivalves ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     #chlorophyll +
                     DOM + 
                     #DO:temp_C + 
                     #DO:salinity + 
                     #DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     #temp_C:chlorophyll + 
                     temp_C:DOM + 
                     #salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_9) # AIC = 3640.4
1 - (glm_biv_9$deviance/glm_biv_9$null.deviance) # = 0.210

glm_biv_10 = glm.nb(bivalves ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     #chlorophyll +
                     DOM + 
                     #DO:temp_C + 
                     #DO:salinity + 
                     #DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     #temp_C:chlorophyll + 
                     temp_C:DOM, # + 
                     #salinity:chlorophyll + 
                     #salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_biv_10) # AIC = 3640.6
1 - (glm_biv_10$deviance/glm_biv_10$null.deviance) # = 0.205

plot(residuals(glm_biv_10)); abline(h=0)
######

# GASTROPODS
######
glm_gas_1 = glm.nb(gastropods ~ Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM + 
                     chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_gas_1) # AIC = 2846.2
1 - (glm_gas_1$deviance/glm_gas_1$null.deviance) # = 0.259

glm_gas_2 = glm.nb(gastropods ~ Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM, # + 
                     #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_gas_2) # AIC = 2844.2
1 - (glm_gas_2$deviance/glm_gas_2$null.deviance) # = 0.259

glm_gas_3 = glm.nb(gastropods ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_gas_3) # AIC = 2843.3
1 - (glm_gas_3$deviance/glm_gas_3$null.deviance) # = 0.256

glm_gas_4 = glm.nb(gastropods ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     #DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_gas_4) # AIC = 2842.4
1 - (glm_gas_4$deviance/glm_gas_4$null.deviance) # = 0.254

glm_gas_5 = glm.nb(gastropods ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     #DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     #temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_gas_5) # AIC = 2843.7
1 - (glm_gas_5$deviance/glm_gas_5$null.deviance) # = 0.246

glm_gas_6 = glm.nb(gastropods ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     #DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     #temp_C:chlorophyll + 
                     temp_C:DOM + 
                     #salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_gas_6) # AIC = 2842.3
1 - (glm_gas_6$deviance/glm_gas_6$null.deviance) # = 0.245

plot(residuals(glm_gas_6)); abline(h=0)
######

# PLUTEI
######
glm_plut_1 = glm.nb(plutei ~ Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM + 
                     chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_plut_1) # AIC = 1893
1 - (glm_plut_1$deviance/glm_plut_1$null.deviance) # = 0.269

glm_plut_2 = glm.nb(plutei ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      #DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_plut_2) # AIC = 1891
1 - (glm_plut_2$deviance/glm_plut_2$null.deviance) # = 0.269

glm_plut_3 = glm.nb(plutei ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      #DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM, # + 
                      #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_plut_3) # AIC = 1889
1 - (glm_plut_3$deviance/glm_plut_3$null.deviance) # = 0.269

glm_plut_4 = glm.nb(plutei ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      #DO:salinity + 
                      DO:chlorophyll + 
                      #DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_plut_4) # AIC = 1887.8
1 - (glm_plut_4$deviance/glm_plut_4$null.deviance) # = 0.267

glm_plut_5 = glm.nb(plutei ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      #DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_plut_5) # AIC = 1887.3
1 - (glm_plut_5$deviance/glm_plut_5$null.deviance) # = 0.263

glm_plut_6 = glm.nb(plutei ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      #DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_plut_6) # AIC = 1887
1 - (glm_plut_6$deviance/glm_plut_6$null.deviance) # = 0.259

glm_plut_7 = glm.nb(plutei ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      #DO:DOM + 
                      temp_C:salinity + 
                      #temp_C:chlorophyll + 
                      temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_plut_7) # AIC = 1885.2
1 - (glm_plut_7$deviance/glm_plut_7$null.deviance) # = 0.258

glm_plut_8 = glm.nb(plutei ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      #chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      #DO:DOM + 
                      temp_C:salinity + 
                      #temp_C:chlorophyll + 
                      temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_plut_8) # AIC = 1883.4
1 - (glm_plut_8$deviance/glm_plut_8$null.deviance) # = 0.258

plot(residuals(glm_plut_8)); abline(h=0)
######

# BARNACLE NAUPLII
######
glm_naup_1 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_1) # AIC = 2238.2
1 - (glm_naup_1$deviance/glm_naup_1$null.deviance) # = 0.247

glm_naup_2 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM, # + 
                      #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_2) # AIC = 2236.2
1 - (glm_naup_2$deviance/glm_naup_2$null.deviance) # = 0.246

glm_naup_3 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_3) # AIC = 2234.2
1 - (glm_naup_3$deviance/glm_naup_3$null.deviance) # = 0.246

glm_naup_4 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_4) # AIC = 2232.2
1 - (glm_naup_4$deviance/glm_naup_4$null.deviance) # = 0.246

glm_naup_5 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      #temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_5) # AIC = 2230.3
1 - (glm_naup_5$deviance/glm_naup_5$null.deviance) # = 0.246

glm_naup_6 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      temp_C:chlorophyll + 
                      #temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_6) # AIC = 2228.5
1 - (glm_naup_6$deviance/glm_naup_6$null.deviance) # = 0.246

glm_naup_7 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      #temp_C:salinity + 
                      temp_C:chlorophyll + 
                      #temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_7) # AIC = 2226.6
1 - (glm_naup_7$deviance/glm_naup_7$null.deviance) # = 0.245

glm_naup_8 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      #temp_C:salinity + 
                      temp_C:chlorophyll, # + 
                      #temp_C:DOM + 
                      #salinity:chlorophyll + 
                      #salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_8) # AIC = 2225.5
1 - (glm_naup_8$deviance/glm_naup_8$null.deviance) # = 0.243

glm_naup_9 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      #salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM + 
                      #temp_C:salinity + 
                      temp_C:chlorophyll, # + 
                    #temp_C:DOM + 
                    #salinity:chlorophyll + 
                    #salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_9) # AIC = 2224.4
1 - (glm_naup_9$deviance/glm_naup_9$null.deviance) # = 0.241

glm_naup_10 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      #salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      DO:chlorophyll + 
                      DO:DOM, # + 
                      #temp_C:salinity + 
                      #temp_C:chlorophyll, # + 
                    #temp_C:DOM + 
                    #salinity:chlorophyll + 
                    #salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_10) # AIC = 2223.5
1 - (glm_naup_10$deviance/glm_naup_10$null.deviance) # = 0.239

glm_naup_11 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                       DO + 
                       #temp_C + 
                       #salinity +
                       chlorophyll +
                       DOM + 
                       #DO:temp_C + 
                       #DO:salinity + 
                       DO:chlorophyll + 
                       DO:DOM, # + 
                     #temp_C:salinity + 
                     #temp_C:chlorophyll, # + 
                     #temp_C:DOM + 
                     #salinity:chlorophyll + 
                     #salinity:DOM, # + 
                     #chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase so model converges
summary(glm_naup_11) # AIC = 2221.8
1 - (glm_naup_11$deviance/glm_naup_11$null.deviance) # = 0.238

plot(residuals(glm_naup_11)); abline(h=0)
######

# CYPRIDS
######
glm_cyp_1 = glm.nb(barnacle_cyprids ~ Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       DO:salinity + 
                       DO:chlorophyll + 
                       DO:DOM + 
                       temp_C:salinity + 
                       temp_C:chlorophyll + 
                       temp_C:DOM + 
                       salinity:chlorophyll + 
                       salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100)) #increase so model converges
summary(glm_cyp_1) # AIC = 1802.2
1 - (glm_cyp_1$deviance/glm_cyp_1$null.deviance) # = 0.174

glm_cyp_2 = glm.nb(barnacle_cyprids ~ Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     #temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM + 
                     chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_cyp_2) # AIC = 1800.6
1 - (glm_cyp_2$deviance/glm_cyp_2$null.deviance) # = 0.173

glm_cyp_3 = glm.nb(barnacle_cyprids ~ Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     #temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM, # + 
                     #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_cyp_3) # AIC = 1800.3
1 - (glm_cyp_3$deviance/glm_cyp_3$null.deviance) # = 0.168

glm_cyp_4 = glm.nb(barnacle_cyprids ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     #temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) #increase so model converges
summary(glm_cyp_4) # AIC = 1801.5
1 - (glm_cyp_4$deviance/glm_cyp_4$null.deviance) # = 0.160

plot(residuals(glm_cyp_4)); abline(h=0)
#######

#######
## COLLECTION OF BEST MODELS
glm_pter_5 = glm.nb(pteropods ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      #temp_C:salinity + 
                      temp_C:chlorophyll + 
                      #temp_C:DOM + 
                      salinity:chlorophyll + 
                      #salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=50)) #increase to 50 so model converges
summary(glm_pter_5)
1 - (glm_pter_5$deviance/glm_pter_5$null.deviance) # = 0.2584441


glm_chaet_8 = glm.nb(chaetognaths ~ #Depth_ctgry + 
                       DO + 
                       temp_C + 
                       salinity +
                       chlorophyll +
                       DOM + 
                       DO:temp_C + 
                       #DO:salinity + 
                       #DO:chlorophyll + 
                       DO:DOM + 
                       #temp_C:salinity + 
                       #temp_C:chlorophyll + 
                       #temp_C:DOM + 
                       salinity:chlorophyll + 
                       #salinity:DOM + 
                       chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100))
summary(glm_chaet_8)
1 - (glm_chaet_8$deviance/glm_chaet_8$null.deviance)

glm_larv_7 = glm.nb(larvaceans ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      #temp_C:chlorophyll + 
                      temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM + 
                      chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100)) 
summary(glm_larv_7)
1 - (glm_larv_7$deviance/glm_larv_7$null.deviance)

glm_cop_9 = glm(copepods ~ Depth_ctgry + 
                  DO + 
                  temp_C + 
                  salinity +
                  chlorophyll +
                  #DOM + 
                  DO:temp_C + 
                  #DO:salinity + 
                  #DO:chlorophyll + 
                  #DO:DOM + 
                  temp_C:salinity + 
                  temp_C:chlorophyll, # + 
                #temp_C:DOM + 
                #salinity:chlorophyll, # + 
                #salinity:DOM, # + 
                #chlorophyll:DOM,
                data = data,
                link = log,
                control=glm.control(maxit=1000)) 
summary(glm_cop_9)
1 - (glm_cop_9$deviance/glm_cop_9$null.deviance)

glm_biv_10 = glm.nb(bivalves ~ #Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      #chlorophyll +
                      DOM + 
                      #DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      DO:DOM + 
                      temp_C:salinity + 
                      #temp_C:chlorophyll + 
                      temp_C:DOM, # + 
                    #salinity:chlorophyll + 
                    #salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100))
summary(glm_biv_10)
1 - (glm_biv_10$deviance/glm_biv_10$null.deviance)

glm_gas_6 = glm.nb(gastropods ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     #DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     temp_C:salinity + 
                     #temp_C:chlorophyll + 
                     temp_C:DOM + 
                     #salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100))
summary(glm_gas_6)
1 - (glm_gas_6$deviance/glm_gas_6$null.deviance)

glm_plut_8 = glm.nb(plutei ~ Depth_ctgry + 
                      DO + 
                      temp_C + 
                      salinity +
                      #chlorophyll +
                      DOM + 
                      DO:temp_C + 
                      #DO:salinity + 
                      #DO:chlorophyll + 
                      #DO:DOM + 
                      temp_C:salinity + 
                      #temp_C:chlorophyll + 
                      temp_C:DOM + 
                      #salinity:chlorophyll + 
                      salinity:DOM, # + 
                    #chlorophyll:DOM,
                    data = data,
                    link = log,
                    control=glm.control(maxit=100))
summary(glm_plut_8)
1 - (glm_plut_8$deviance/glm_plut_8$null.deviance)

glm_naup_11 = glm.nb(barnacle_nauplii ~ Depth_ctgry + 
                       DO + 
                       #temp_C + 
                       #salinity +
                       chlorophyll +
                       DOM + 
                       #DO:temp_C + 
                       #DO:salinity + 
                       DO:chlorophyll + 
                       DO:DOM, # + 
                     #temp_C:salinity + 
                     #temp_C:chlorophyll, # + 
                     #temp_C:DOM + 
                     #salinity:chlorophyll + 
                     #salinity:DOM, # + 
                     #chlorophyll:DOM,
                     data = data,
                     link = log,
                     control=glm.control(maxit=100))
summary(glm_naup_11)
1 - (glm_naup_11$deviance/glm_naup_11$null.deviance)

glm_cyp_4 = glm.nb(barnacle_cyprids ~ #Depth_ctgry + 
                     DO + 
                     temp_C + 
                     salinity +
                     chlorophyll +
                     DOM + 
                     DO:temp_C + 
                     DO:salinity + 
                     DO:chlorophyll + 
                     DO:DOM + 
                     #temp_C:salinity + 
                     temp_C:chlorophyll + 
                     temp_C:DOM + 
                     salinity:chlorophyll + 
                     salinity:DOM, # + 
                   #chlorophyll:DOM,
                   data = data,
                   link = log,
                   control=glm.control(maxit=100)) 
summary(glm_cyp_4)
1 - (glm_cyp_4$deviance/glm_cyp_4$null.deviance)

