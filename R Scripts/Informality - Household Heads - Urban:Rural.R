############################################################################################################################ 
############################################################################################################################ 
##################### Master Thesis: Impact of Familias en Acción on HH Labor Market Decisions ##################### 

## ANALYSIS 3: 
## Impact of Familias en Acción on informality 
## Definition:

# DANE: 
# The National Administrative Department of Statistics (DANE) considers informal workers to be 
# “those people employed in companies with a size equal to or less than 5 people.”

## Sample: urban population & household heads. We exclude the rural sample, as it does not contain the
## variable of interest to measure informality. 
# For this analysis, we use our Household_Heads data set. 
# Here we are interested in individuals who answered the question:
# How many employees, including yourself does the company where you work employ?
# This information is given by the variable 'n_empleados'. Thus, we drop individuals with NAs under
# this variable. 

Informality <- subset(Household_Heads, !is.na(n_empleados))

# As before, we want to make sure we have only complete cases. Since we dropped some obs., now we need to run
# the complete cases exercise once more. 

Informality <- Informality %>% 
  group_by(llave_ID_lb) %>%
  dplyr::mutate(cnt = n()) %>%
  filter(cnt == 3) %>%
  select(-cnt)

# In the survey, 'n_empleados' is coded as follows: 
# 1 = Work alone
# 2 = From 2 to 5 people
# 3 = From 6 to 10 people
# 4 = From 11 to 19 people
# 5 = From 20 to 49 people
# 6 = 50 people and more

# Next, we create a new variable called 'informal' based on 'n_empleados' as follows:

## DANE Definition: 

# Informal = 1 if n_empleados =< 2  and Informal = 0 otherwise

Informality$Informal <- ifelse(Informality$n_empleados <= 2, 1, 0)

######################### IMPACT FAMILIAS EN ACCION ON INFORMALITY BY HOUSEHOLD HEADS - WHOLE

# Approach: Callaway and Sant'Anna (2021) / Group-Time Average Treatment Effects ---------------------------------

ATT_Informality <- att_gt(yname = "Informal",
                          tname = "Year",
                          idname = "llave_ID_lb",
                          gname = "Year_Transfer",
                          xformla = ~1,
                          data = Informality,
                          allow_unbalanced_panel = TRUE,
                          est_method = "reg")

summary(ATT_Informality, cluster = c("Area"))
ggdid(ATT_Informality, ylim = c(-15,10))

# Simple Aggregation:

ATT_Informality_Simple <- aggte(ATT_Informality, type = "simple")
summary(ATT_Informality_Simple,cluster = c("Area"))
save(ATT_Informality_Simple, file="models/main_informality_simple.Rdata")

# Dynamic ATT (Event-Study):

Dynamic_Informality <- aggte(ATT_Informality, type = "dynamic")
summary(Dynamic_Informality,cluster = c("Area"))
ggdid(Dynamic_Informality)
save(Dynamic_Informality, file="models/main_informality_dynamic.Rdata")

# ATT for each group:

Group_Effects_Informality <- aggte(ATT_Informality, type = "group")
summary(Group_Effects_Informality,cluster = c("Area"))
save(Group_Effects_Informality, file="models/main_informality_group.Rdata")

################################################ BY GENDER

## FEMALE

Women_Informal <- Informality %>%
  filter(sexo == 1)

## MALE

Men_Informal <- Informality %>%
  filter(sexo == 0)

# a) For women:

ATT_Women_Informal <- att_gt(yname = "Informal",
                             gname = "Year_Transfer",
                             idname = "llave_ID_lb",
                             tname = "Year",
                             xformla = ~1,
                             data = Women_Informal,
                             allow_unbalanced_panel = TRUE,
                             est_method = "reg")

summary(ATT_Women_Informal,cluster = c("Area"))
ggdid(ATT_Women_Informal, ylim = c(-15,10))

# Simple Aggregation:

ATT_Women_Informal_Simple <- aggte(ATT_Women_Informal, type = "simple")
summary(ATT_Women_Informal_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Women_Informal <- aggte(ATT_Women_Informal, type = "dynamic")
summary(Dynamic_Women_Informal,cluster = c("Area"))
ggdid(Dynamic_Women_Informal)

# ATT for each group:

Group_Effects_Women_Informal <- aggte(ATT_Women_Informal, type = "group")
summary(Group_Effects_Women_Informal,cluster = c("Area"))

# b) For men:

ATT_Men_Informal <- att_gt(yname = "Informal",
                           gname = "Year_Transfer",
                           idname = "llave_ID_lb",
                           tname = "Year",
                           xformla = ~1,
                           data = Men_Informal,
                           allow_unbalanced_panel = TRUE,
                           est_method = "reg")

summary(ATT_Men_Informal,cluster = c("Area"))
ggdid(ATT_Men_Informal, ylim = c(-25,10))

# Simple Aggregation:

ATT_Men_Informal_Simple <- aggte(ATT_Men_Informal, type = "simple")
summary(ATT_Men_Informal_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Men_Informal <- aggte(ATT_Men_Informal, type = "dynamic")
summary(Dynamic_Men_Informal,cluster = c("Area"))
ggdid(Dynamic_Men_Informal)

# ATT for each group:

Group_Effects_Men_Informal <- aggte(ATT_Men_Informal, type = "group")
summary(Group_Effects_Men_Informal,cluster = c("Area"))

Informality %>%
  group_by(llave_ID_lb) %>%
  summarise(count_positive = sum(Year_Transfer > 0),
            count_negative = sum(Year_Transfer == 0))

Informality %>%
  group_by(llave_ID_lb) %>%
  summarise(
    count_positive = sum(Year_Transfer > 0),
    count_negative = sum(Year_Transfer == 0),
    count_informality_gt_0 = sum(Informal == 1),
    count_informality_gt_0 = sum(Informal == 0)
  )

######################### IMPACT FAMILIAS EN ACCION ON INFORMALITY BY HOUSEHOLD HEADS - SINGLE

Informality_Single <- Informality %>%
  group_by(consecutivo) %>%
  filter(all(parentesco != 2)) %>%
  ungroup()

# Approach: Callaway and Sant'Anna (2021) / Group-Time Average Treatment Effects ---------------------------------

## 1. DANE Definition:

ATT_Informality_Single <- att_gt(yname = "Informal",
                               tname = "Year",
                               idname = "llave_ID_lb",
                               gname = "Year_Transfer",
                               xformla = ~1,
                               data = Informality_Single,
                               allow_unbalanced_panel = TRUE,
                               est_method = "reg")

summary(ATT_Informality_Single, cluster = c("Area"))
ggdid(ATT_Informality_Single, ylim = c(-15,10))

# Simple Aggregation:

ATT_Informality_Single_Simple <- aggte(ATT_Informality_Single, type = "simple")
summary(ATT_Informality_Single_Simple,cluster = c("Area"))
save(ATT_Informality_Single_Simple, file="models/main_informality_simple_single.Rdata")

# Dynamic ATT (Event-Study):

Dynamic_Informality_Single <- aggte(ATT_Informality_Single, type = "dynamic")
summary(Dynamic_Informality_Single,cluster = c("Area"))
ggdid(Dynamic_Informality_Single)
save(Dynamic_Informality_Single, file="models/main_informality_dynamic_single.Rdata")

# ATT for each group:

Group_Effects_Informality_Single <- aggte(ATT_Informality_Single, type = "group")
summary(Group_Effects_Informality_Single,cluster = c("Area"))
save(Group_Effects_Informality_Single, file="models/main_informality_group_single.Rdata")

################################################ BY GENDER

## FEMALE

Women_Informal_Single <- Informality_Single %>%
  filter(sexo == 1)

## MALE

Men_Informal_Single <- Informality_Single %>%
  filter(sexo == 0)

# a) For women:

ATT_Women_Informal_Single <- att_gt(yname = "Informal",
                                  gname = "Year_Transfer",
                                  idname = "llave_ID_lb",
                                  tname = "Year",
                                  xformla = ~1,
                                  data = Women_Informal_Single,
                                  allow_unbalanced_panel = TRUE,
                                  est_method = "reg")

summary(ATT_Women_Informal_Single,cluster = c("Area"))
ggdid(ATT_Women_Informal_Single, ylim = c(-15,10))

# Simple Aggregation:

ATT_Women_Informal_Simple_Single <- aggte(ATT_Women_Informal_Single, type = "simple")
summary(ATT_Women_Informal_Simple_Single,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Women_Informal_Single <- aggte(ATT_Women_Informal_Single, type = "dynamic")
summary(Dynamic_Women_Informal_Single,cluster = c("Area"))
ggdid(Dynamic_Women_Informal_Single)

# ATT for each group:

Group_Effects_Women_Informal_Single <- aggte(ATT_Women_Informal_Single, type = "group")
summary(Group_Effects_Women_Informal_Single,cluster = c("Area"))

# b) For men:

ATT_Men_Informal_Single <- att_gt(yname = "Informal",
                                gname = "Year_Transfer",
                                idname = "llave_ID_lb",
                                tname = "Year",
                                xformla = ~1,
                                data = Men_Informal_Single,
                                allow_unbalanced_panel = TRUE,
                                est_method = "reg")

summary(ATT_Men_Informal_Single,cluster = c("Area"))
ggdid(ATT_Men_Informal_Single, ylim = c(-25,10))

# Simple Aggregation:

ATT_Men_Informal_Simple_Single <- aggte(ATT_Men_Informal_Single, type = "simple")
summary(ATT_Men_Informal_Simple_Single,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Men_Informal_Single <- aggte(ATT_Men_Informal_Single, type = "dynamic")
summary(Dynamic_Men_Informal_Single,cluster = c("Area"))
ggdid(Dynamic_Men_Informal_Single)

# ATT for each group:

Group_Effects_Men_Informal_Single <- aggte(ATT_Men_Informal_Single, type = "group")
summary(Group_Effects_Men_Informal_Single,cluster = c("Area"))
