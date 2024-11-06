# Package names
packages <- c("etwfe","gtsummary", "knitr","kableExtra", "foreign", "haven", "dplyr", "tidyverse", "stringr", "MatchIt", "cobalt", "sandwich", "data.table", "ggplot2", "did", "fixest","plyr","todor")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

############################################# DATA URBAN REGION ################################################################

### 'Personas' Questionnaire

UPersonas_2010 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2010/Urbano/Bases/UPersonas.dta")
UPersonas_2013 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2013/Urbano/Bases/UPersonas.dta")
UPersonas_2016 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2016/Urbano/Bases/UPersonas.dta")

### 'Hogares' Questionnaire

UHogar_2010 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2010/Urbano/Bases/UHogar.dta")
UHogar_2013 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2013/Urbano/Bases/UHogar.dta")
UHogar_2016 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2016/Urbano/Bases/UHogar.dta")

############################################# DATA RURAL REGION ################################################################

### 'Personas' Questionnaire

RPersonas_2010 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2010/Rural/Bases/RPersonas.dta")
RPersonas_2013 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2013/Rural/Bases/RPersonas.dta")
RPersonas_2016 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2016/Rural/Bases/RPersonas.dta")

### 'Hogares' Questionnaire

RHogar_2010 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2010/Rural/Bases/RHogar.dta")
RHogar_2013 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2013/Rural/Bases/RHogar.dta")
RHogar_2016 <- read_dta("/Users/isabel/Documents/Master Material/Thesis/Colombia (FA)/DATA/Urban & Rural/ELCA 2016/Rural/Bases/RHogar.dta")

############################################# DATA CLEANING & PREPARATION ################################################################

############################################# URBAN REGION ################################################################

### PERSONAS QUESTIONNAIRE 

### YEAR 2010
# Select relevant variables from 'Personas' Questionnaire

UPersonas_2010 <- UPersonas_2010 %>% select(consecutivo, llave_ID_lb, edad,sexo,horas_normal,horas_2, parentesco, afiliacion,tam_empresa,vr_salario,hijos_hombres_unionm)

# Change hijos_hombres_unionm to hijos

UPersonas_2010 <- UPersonas_2010 %>% dplyr::rename(hijos = hijos_hombres_unionm)

# Replace NA with 0 for hijos

UPersonas_2010 <- UPersonas_2010 %>% 
  mutate_at(c('hijos'), ~replace_na(.,0))

# Replace NA with 0 for horas_normal and horas_2

UPersonas_2010 <- UPersonas_2010 %>% 
  mutate_at(c('horas_normal','horas_2'), ~replace_na(.,0))

# Add up horas_normal and horas_2 and create a new column 'horas_normal'. Remove horas_2 from dataframe. 

UPersonas_2010 <- UPersonas_2010 %>% rowwise() %>%
  dplyr::mutate(horas_normal = sum(c_across(horas_normal:horas_2))) %>%
  select(-horas_2)

## NOTE:
# horas2:  This variables refers to the amount of hours worked (second job).
# Check ELCA_Manual_Hogar_Urbano2010 

# Drop observations with variables llave_ID_lb & consecutivo --> NA

UPersonas_2010 <- UPersonas_2010 %>% 
  drop_na(c("llave_ID_lb", "consecutivo"))

# Change 'sexo' variable to take 1 = female and 0 = male.
# Originally: 1 = male and 2 = female. 

UPersonas_2010$sexo <- ifelse(UPersonas_2010$sexo == 2, 1, 0)

# Convert edad, parentesco, tam_empresa, vr_salario and afiliacion to numeric type 

UPersonas_2010$edad <- as.numeric(as.character(UPersonas_2010$edad))
UPersonas_2010$parentesco <- as.numeric(as.character(UPersonas_2010$parentesco))
UPersonas_2010$tam_empresa <- as.numeric(as.character(UPersonas_2010$tam_empresa))
UPersonas_2010$afiliacion <- as.numeric(as.character(UPersonas_2010$afiliacion))
UPersonas_2010$vr_salario <- as.numeric(as.character(UPersonas_2010$vr_salario))
UPersonas_2010$hijos <- as.numeric(as.character(UPersonas_2010$hijos))

# Change variable name 'tam_empresa' to 'n_empleados'

names(UPersonas_2010)[names(UPersonas_2010) == "tam_empresa"] <- "n_empleados"

# Add Year Column to identify time (Here, Year = 2010)

UPersonas_2010 <- UPersonas_2010 %>%
  mutate(Year = 2010)

### YEAR 2013
# Select relevant variables from 'Personas' Questionnaire

UPersonas_2013 <- UPersonas_2013 %>% select(consecutivo, llave_ID_lb, edad,sexo,horas_normal,parentesco,afiliacion,n_empleados,vr_salario,hijos_hombres,hijas_mujeres)

# Add up hijos & hijas 

UPersonas_2013 <- UPersonas_2013 %>%
  mutate(hijos = rowSums(select(., hijas_mujeres:hijos_hombres)),
         hijos_hombres = NULL,
         hijas_mujeres = NULL)

# Replace NA with 0 for hijos

UPersonas_2013 <- UPersonas_2013 %>% 
  mutate_at(c('hijos'), ~replace_na(.,0))

# Replace NA with 0 for horas_normal

UPersonas_2013 <- UPersonas_2013 %>% mutate_at(c('horas_normal'), ~replace_na(.,0))

# Drop observations with variables llave_ID_lb & consecutivo --> NA

UPersonas_2013 <- UPersonas_2013 %>% 
  drop_na(c("llave_ID_lb", "consecutivo"))

# Change 'sexo' variable to take 1 = female and 0 = male.
# Originally: 1 = male and 2 = female. 

UPersonas_2013$sexo <- ifelse(UPersonas_2013$sexo == 2, 1, 0)

# Convert edad, parentesco, tam_empresa and afiliacion to numeric type 

UPersonas_2013$edad <- as.numeric(as.character(UPersonas_2013$edad))
UPersonas_2013$parentesco <- as.numeric(as.character(UPersonas_2013$parentesco))
UPersonas_2013$n_empleados <- as.numeric(as.character(UPersonas_2013$n_empleados))
UPersonas_2013$afiliacion <- as.numeric(as.character(UPersonas_2013$afiliacion))
UPersonas_2013$vr_salario <- as.numeric(as.character(UPersonas_2013$vr_salario))
UPersonas_2013$hijos <- as.numeric(as.character(UPersonas_2013$hijos))

# Add Year Column to identify time (Here, Year = 2013)

UPersonas_2013 <- UPersonas_2013 %>%
  mutate(Year = 2013)

### YEAR 2016
# Select relevant variables from 'Personas' Questionnaire

UPersonas_2016 <- UPersonas_2016 %>% select(consecutivo, llave_ID_lb, edad,sexo,horas_normal, parentesco, afiliacion,n_empleados,vr_salario,hijos_hombres,hijas_mujeres)

# Replace NA with 0 for horas_normal

UPersonas_2016 <- UPersonas_2016 %>% 
 mutate_at(c('horas_normal'), ~replace_na(.,0))

# Add up hijos & hijas 

UPersonas_2016 <- UPersonas_2016 %>%
  mutate(hijos = rowSums(select(., hijas_mujeres:hijos_hombres)),
         hijos_hombres = NULL,
         hijas_mujeres = NULL)

# Replace NA with 0 for hijos

UPersonas_2016 <- UPersonas_2016 %>% 
  mutate_at(c('hijos'), ~replace_na(.,0))

# Drop observations with variables llave_ID_lb & consecutivo --> NA

UPersonas_2016 <- UPersonas_2016 %>% 
  drop_na(c("llave_ID_lb", "consecutivo"))

# Change 'sexo' variable to take 1 = female and 0 = male.
# Originally: 1 = male and 2 = female. 

UPersonas_2016$sexo <- ifelse(UPersonas_2016$sexo == 2, 1, 0)

# Convert edad, parentesco, tam_empresa and afiliacion to numeric type 

UPersonas_2016$edad <- as.numeric(as.character(UPersonas_2016$edad))
UPersonas_2016$parentesco <- as.numeric(as.character(UPersonas_2016$parentesco))
UPersonas_2016$n_empleados <- as.numeric(as.character(UPersonas_2016$n_empleados))
UPersonas_2016$afiliacion <- as.numeric(as.character(UPersonas_2016$afiliacion))
UPersonas_2016$vr_salario <- as.numeric(as.character(UPersonas_2016$vr_salario))
UPersonas_2016$hijos <- as.numeric(as.character(UPersonas_2016$hijos))

# Add Year Column to identify time (Here, Year = 2016)

UPersonas_2016 <- UPersonas_2016 %>%
  mutate(Year = 2016)

# Merge Personas Questionnaire

UPersonas_All <- rbind(UPersonas_2010,UPersonas_2013,UPersonas_2016)

### HOGARES QUESTIONNAIRE 

### YEAR 2010

UHogar_2010 <- UHogar_2010 %>% select(consecutivo, familias_accion,tenencia_vivienda,
                                      sp_energia,n_lavadoras,material_paredes,sp_recoleccion_basura, 
                                      obtencion_agua)

# Change 'familias_accion' values
# Originally: FA = 1 -> receives CCT ; FA = 2 --> doesn't receive CCT
# Now: FA = 0 --> doesn't receive CCT ; FA = 1 --> receives CCT

UHogar_2010$familias_accion[UHogar_2010$familias_accion==2]<-0

# Add Year Column to identify time (Here, Year = 2010)

UHogar_2010 <- UHogar_2010 %>%
  mutate(Year = 2010)

### YEAR 2013

# Select relevant variables from 'Hogares' Questionnaire

UHogar_2013 <- UHogar_2013 %>% select(consecutivo, familias_accion,tenencia_vivienda,
                                      sp_energia,n_lavadoras,material_paredes,sp_recoleccion_basura, 
                                      obtencion_agua)

# Change 'familias_accion' values
# Originally: FA = 1 -> receives CCT ; FA = 2 --> doesn't receive CCT
# Now: FA = 0 --> doesn't receive CCT ; FA = 1 --> receives CCT

UHogar_2013$familias_accion[UHogar_2013$familias_accion==2]<-0

# Add Year Column to identify time (Here, Year = 2013)

UHogar_2013 <- UHogar_2013 %>%
  mutate(Year = 2013)

### YEAR 2016

# Select relevant variables from 'Hogares' Questionnaire

UHogar_2016 <- UHogar_2016 %>% select(consecutivo, familias_accion,tenencia_vivienda,
                                      sp_energia,n_lavadoras,material_paredes,sp_recoleccion_basura, 
                                      obtencion_agua)

# Change 'familias_accion' values
# Originally: FA = 1 -> receives CCT ; FA = 2 --> doesn't receive CCT
# Now: FA = 0 --> doesn't receive CCT ; FA = 1 --> receives CCT

UHogar_2016$familias_accion[UHogar_2016$familias_accion==2]<-0

# Add Year Column to identify time (Here, Year = 2016)

UHogar_2016 <- UHogar_2016 %>%
  mutate(Year = 2016)

# Merge Hogares Questionnaire by 'consecutivo'

UHogares_All <- rbind(UHogar_2010,UHogar_2013,UHogar_2016)

# Convert the following variables to numeric 

UHogares_All <- mutate_at(UHogares_All, vars(familias_accion, tenencia_vivienda, sp_energia, material_paredes,sp_recoleccion_basura,obtencion_agua), as.numeric)

# Merge Hogares and Personas Questionnaires by 'consecutivo' and 'year'

UHogares_UPersonas_All <- merge(UPersonas_All, UHogares_All, by = c("consecutivo", "Year"))

# Create identifier for Community --> Area = Urban

UHogares_UPersonas_All <- UHogares_UPersonas_All %>%
  mutate(Area = "Urban")

sapply(UHogares_UPersonas_All,class)

############################################# RURAL REGION ################################################################

### DATA CLEANING & PREPARATION

### PERSONAS QUESTIONNAIRE 

### YEAR 2010
# Select relevant variables from 'Personas' Questionnaire

RPersonas_2010 <- RPersonas_2010 %>% select(consecutivo, llave_ID_lb, edad,sexo,horas_normal, parentesco,afiliacion,hijos_hombres_unionm)

# Change hijos_hombres_unionm to hijos

RPersonas_2010 <- RPersonas_2010 %>% dplyr::rename(hijos = hijos_hombres_unionm)

# Replace NA with 0 for hijos

RPersonas_2010 <- RPersonas_2010 %>% 
  mutate_at(c('hijos'), ~replace_na(.,0))

# Replace NA with 0 for horas_normal

RPersonas_2010 <- RPersonas_2010 %>% 
  mutate_at(c('horas_normal'), ~replace_na(.,0))

# Drop observations with variables llave_ID_lb & consecutivo --> NA

RPersonas_2010 <- RPersonas_2010 %>% 
  drop_na(c("llave_ID_lb", "consecutivo"))

# Change 'sexo' variable to take 1 = female and 0 = male.
# Originally: 1 = male and 2 = female. 

RPersonas_2010$sexo <- ifelse(RPersonas_2010$sexo == 2, 1, 0)

# Convert edad, parentesco and afiliacion to numeric type 

RPersonas_2010$edad <- as.numeric(as.character(RPersonas_2010$edad))
RPersonas_2010$parentesco <- as.numeric(as.character(RPersonas_2010$parentesco))
RPersonas_2010$afiliacion <- as.numeric(as.character(RPersonas_2010$afiliacion))
RPersonas_2010$hijos <- as.numeric(as.character(RPersonas_2010$hijos))

# Add Year Column to identify time (Here, Year = 2010)

RPersonas_2010 <- RPersonas_2010 %>%
  mutate(Year = 2010)

### YEAR 2013
# Select relevant variables from 'Personas' Questionnaire

RPersonas_2013 <- RPersonas_2013 %>% select(consecutivo, llave_ID_lb, edad,sexo,horas_normal,horas_normal1,horas_normal2,horas_normal3,
                                            horas_normal4,horas_normal5,horas_normal6, parentesco, afiliacion,hijos_hombres,hijas_mujeres)

## NOTE: horas_normal# (from 1 to 6) shows amount of hours worked by person in her/his different jobs during the week. 
# Add up all those hours together for every individual

# Replace NA with 0 for horas_normal (from 1 to 6) 

RPersonas_2013 <- RPersonas_2013 %>% 
  mutate_at(c('horas_normal','horas_normal','horas_normal1','horas_normal2','horas_normal3',
              'horas_normal4','horas_normal5','horas_normal6'), ~replace_na(.,0))

# Add up horas_normal (from 1 to 6)  for every individual and create new column with result called 'horas_normal'

RPersonas_2013$horas_normal <- rowSums(RPersonas_2013[, c('horas_normal','horas_normal','horas_normal1','horas_normal2','horas_normal3',
                                                          'horas_normal4','horas_normal5','horas_normal6')], na.rm = TRUE)
                                                      
RPersonas_2013 = subset(RPersonas_2013, select = -c(horas_normal1,horas_normal2,horas_normal3,
                                                    horas_normal4,horas_normal5,horas_normal6))

# Add up hijos & hijas 

RPersonas_2013 <- RPersonas_2013 %>%
  mutate(hijos = rowSums(select(., hijas_mujeres:hijos_hombres)),
         hijos_hombres = NULL,
         hijas_mujeres = NULL)

# Replace NA with 0 for hijos

RPersonas_2013 <- RPersonas_2013 %>% 
  mutate_at(c('hijos'), ~replace_na(.,0))

# Drop observations with variables llave_ID_lb & consecutivo --> NA

RPersonas_2013 <- RPersonas_2013 %>% 
  drop_na(c("llave_ID_lb", "consecutivo"))

# Change 'sexo' variable to take 1 = female and 0 = male.
# Originally: 1 = male and 2 = female. 

RPersonas_2013$sexo <- ifelse(RPersonas_2013$sexo == 2, 1, 0)

# Convert edad, parentesco and afiliacion to numeric type 

RPersonas_2013$edad <- as.numeric(as.character(RPersonas_2013$edad))
RPersonas_2013$parentesco <- as.numeric(as.character(RPersonas_2013$parentesco))
RPersonas_2013$afiliacion <- as.numeric(as.character(RPersonas_2013$afiliacion))
RPersonas_2013$hijos <- as.numeric(as.character(RPersonas_2013$hijos))

# Add Year Column to identify time (Here, Year = 2013)

RPersonas_2013 <- RPersonas_2013 %>%
  mutate(Year = 2013)

### YEAR 2016
# Select relevant variables from 'Personas' Questionnaire

RPersonas_2016 <- RPersonas_2016 %>% select(consecutivo, llave_ID_lb, edad,sexo,horas_normal,horas_normal1,horas_normal2,
                                            horas_normal3,horas_normal4,horas_normal5,parentesco, afiliacion,hijos_hombres,hijas_mujeres)

## NOTE: horas_normal# (from 1 to 5) seems to show amount of hours worked by person in her/his different jobs during the week. 
# Will need to add up all those hours together for every individual

# Replace NA with 0 for horas_normal (from 1 to 5) 

RPersonas_2016 <- RPersonas_2016 %>% 
  mutate_at(c('horas_normal','horas_normal','horas_normal1','horas_normal2','horas_normal3',
              'horas_normal4','horas_normal5'), ~replace_na(.,0))

# Add up horas_normal (from 1 to 5)  for every individual and create new column with result called 'horas_normal'

RPersonas_2016$horas_normal <- rowSums(RPersonas_2016[, c('horas_normal','horas_normal','horas_normal1','horas_normal2','horas_normal3',
                                                          'horas_normal4','horas_normal5')], na.rm = TRUE)

RPersonas_2016 = subset(RPersonas_2016, select = -c(horas_normal1,horas_normal2,horas_normal3,
                                                    horas_normal4,horas_normal5))

# Add up hijos & hijas 

RPersonas_2016 <- RPersonas_2016 %>%
  mutate(hijos = rowSums(select(., hijas_mujeres:hijos_hombres)),
         hijos_hombres = NULL,
         hijas_mujeres = NULL)

# Replace NA with 0 for hijos

RPersonas_2016 <- RPersonas_2016 %>% 
  mutate_at(c('hijos'), ~replace_na(.,0))

# Drop observations with variables llave_ID_lb & consecutivo --> NA

RPersonas_2016 <- RPersonas_2016 %>% 
  drop_na(c("llave_ID_lb", "consecutivo"))

# Change 'sexo' variable to take 1 = female and 0 = male.
# Originally: 1 = male and 2 = female. 

RPersonas_2016$sexo <- ifelse(RPersonas_2016$sexo == 2, 1, 0)

# Convert edad, parentesco and afiliacion to numeric type 

RPersonas_2016$edad <- as.numeric(as.character(RPersonas_2016$edad))
RPersonas_2016$parentesco <- as.numeric(as.character(RPersonas_2016$parentesco))
RPersonas_2016$afiliacion <- as.numeric(as.character(RPersonas_2016$afiliacion))
RPersonas_2016$hijos <- as.numeric(as.character(RPersonas_2016$hijos))

# Add Year Column to identify time (Here, Year = 2016)

RPersonas_2016 <- RPersonas_2016 %>%
  mutate(Year = 2016)

# Merge Personas Questionnaire

RPersonas_All <- rbind(RPersonas_2010,RPersonas_2013,RPersonas_2016)

# Convert horas_normal to numeric type

RPersonas_All$horas_normal <- as.numeric(as.character(RPersonas_All$horas_normal))

### HOGARES QUESTIONNAIRE 

### YEAR 2010

# Select relevant variables from 'Hogares' Questionnaire

RHogar_2010 <- RHogar_2010 %>% select(consecutivo, familias_accion,tenencia_vivienda,
                                      sp_energia,n_lavadoras,material_paredes,sp_recoleccion_basura, 
                                      obtencion_agua)

# Change 'familias_accion' values
# Originally: FA = 1 -> receives CCT ; FA = 2 --> doesn't receive CCT
# Now: FA = 0 --> doesn't receive CCT ; FA = 1 --> receives CCT

RHogar_2010$familias_accion[RHogar_2010$familias_accion==2]<-0

# Add Year Column to identify time (Here, Year = 2010)

RHogar_2010 <- RHogar_2010 %>%
  mutate(Year = 2010)

### YEAR 2013

# Select relevant variables from 'Hogares' Questionnaire

RHogar_2013 <- RHogar_2013 %>% select(consecutivo, familias_accion,tenencia_vivienda,
                                      sp_energia,n_lavadoras,material_paredes,sp_recoleccion_basura, 
                                      obtencion_agua)

# Change 'familias_accion' values
# Originally: FA = 1 -> receives CCT ; FA = 2 --> doesn't receive CCT
# Now: FA = 0 --> doesn't receive CCT ; FA = 1 --> receives CCT

RHogar_2013$familias_accion[RHogar_2013$familias_accion==2]<-0

# Add Year Column to identify time (Here, Year = 2013)

RHogar_2013 <- RHogar_2013 %>%
  mutate(Year = 2013)

### YEAR 2016

# Select relevant variables from 'Hogares' Questionnaire

RHogar_2016 <- RHogar_2016 %>% select(consecutivo, familias_accion,tenencia_vivienda,
                                      sp_energia,n_lavadoras,material_paredes,sp_recoleccion_basura, 
                                      obtencion_agua)

# Change 'familias_accion' values
# Originally: FA = 1 -> receives CCT ; FA = 2 --> doesn't receive CCT
# Now: FA = 0 --> doesn't receive CCT ; FA = 1 --> receives CCT

RHogar_2016$familias_accion[RHogar_2016$familias_accion==2]<-0

# Add Year Column to identify time (Here, Year = 2016)

RHogar_2016 <- RHogar_2016 %>%
  mutate(Year = 2016)

# Merge Hogares Questionnaire by 'consecutivo'

RHogares_All <- rbind(RHogar_2010,RHogar_2013,RHogar_2016)

# Convert the following variables to numeric 

RHogares_All <- mutate_at(RHogares_All, vars(familias_accion, tenencia_vivienda, sp_energia, material_paredes,sp_recoleccion_basura,obtencion_agua), as.numeric)

# Merge Hogares and Personas Questionnaires by 'consecutivo' and 'year'

RHogares_RPersonas_All <- merge(RPersonas_All, RHogares_All, by = c("consecutivo", "Year"))

# Create identifier for Rural Community --> Area = Rural

RHogares_RPersonas_All <- RHogares_RPersonas_All %>%
  mutate(Area = "Rural")

sapply(RHogares_RPersonas_All, class)

# Merge Rural and Urban Surveys together. 

Urban_Rural <- rbind.fill(UHogares_UPersonas_All,RHogares_RPersonas_All)

# Remove duplicates

Urban_Rural <- Urban_Rural %>%
  group_by(llave_ID_lb, Year) %>%
  slice(1) %>%
  ungroup()

# Change 'afiliacion' variable to take 1 = contributing to SS and 0 = not contributing
# Originally: 1 = contributing and 2 = not contributing 

Urban_Rural$afiliacion[Urban_Rural$afiliacion==2]<-0

# Eliminate individuals with hours worked exceeding 150 (measurement error)

Urban_Rural <- Urban_Rural[Urban_Rural$horas_normal < 150, ]

# Eliminate individuals older than 90 years 

Urban_Rural <- Urban_Rural[!(Urban_Rural$edad > 90), ]

# Eliminate individuals with a 'wrong' value under 'afiliacion'

Urban_Rural <- Urban_Rural[!(Urban_Rural$afiliacion == 8), ]

# Eliminate rows filled with NAs (duplicates from the merge)

Urban_Rural <- Urban_Rural[rowSums(is.na(Urban_Rural)) != ncol(Urban_Rural), ]

# Change name of relevant variables 

Urban_Rural <- dplyr::rename(Urban_Rural, Age = edad, House_Ownership = tenencia_vivienda, 
                             Washing_Machine = n_lavadoras, Wall_Materials = material_paredes,
                             Waste_Services = sp_recoleccion_basura, Energy_Access = sp_energia,
                             Water_Source = obtencion_agua)
