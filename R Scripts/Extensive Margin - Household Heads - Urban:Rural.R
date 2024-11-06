############################################################################################################################ 
############################################################################################################################ 
##################### Master Thesis: Impact of Familias en Acción on HH Labor Market Decisions ##################### 

## ANALYSIS 2: 
## Impact of Familias en Acción on decision to work (extensive margin)

## Sample:
# For this analysis, we employ the dataframe "Household_Heads" that was used for the Intensive Margin
# analysis. This sample already contains a "matched" Control Group and includes all relevant variables
# (e.g.: Treatment, Year_Treatment, etc.) 

# In order to identify whether an individual is working or not, we consider the variable
# "horas_normal", that specifies the avg. (weekly) amount of hours worked. 
# Based on "horas_normal", we create a variable called "employed", that takes a value of 1 if "horas_normal" > 0
# and 0 otherwise. 

Household_Heads$employed <- as.integer(Household_Heads$horas_normal > 0)

######################### IMPACT FAMILIAS EN ACCION ON DECISION TO WORK BY HOUSEHOLD HEADS - WHOLE

# Approach: Callaway and Sant'Anna (2021) / Group-Time Average Treatment Effects ---------------------------------

ATT_Extensive <- att_gt(yname = "employed",
                              tname = "Year",
                              idname = "llave_ID_lb",
                              gname = "Year_Transfer",
                              xformla = ~1,
                              data = Household_Heads,
                              allow_unbalanced_panel = TRUE,
                              est_method = "reg")

summary(ATT_Extensive, cluster = c("Area"))
ggdid(ATT_Extensive, ylim = c(-15,10))

# Simple Aggregation:

ATT_Extensive_Simple <- aggte(ATT_Extensive, type = "simple")
summary(ATT_Extensive_Simple,cluster = c("Area"))
save(ATT_Extensive_Simple, file="models/main_extensive_simple.Rdata")

# Dynamic ATT (Event-Study):

Dynamic_Extensive <- aggte(ATT_Extensive, type = "dynamic")
summary(Dynamic_Extensive,cluster = c("Area"))
ggdid(Dynamic_Extensive)
save(Dynamic_Extensive, file="models/main_extensive_dynamic.Rdata")

# ATT for each group:

Group_Effects_Extensive <- aggte(ATT_Extensive, type = "group")
summary(Group_Effects_Extensive,cluster = c("Area"))
save(Group_Effects_Extensive, file="models/main_extensive_group.Rdata")

################################################ BY GENDER

## FEMALE

Women_Extensive <- Household_Heads %>%
  filter(sexo == 1)

## MALE

Men_Extensive <- Household_Heads %>%
  filter(sexo == 0)

# a) For women:

ATT_Women_Extensive <- att_gt(yname = "employed",
                                    gname = "Year_Transfer",
                                    idname = "llave_ID_lb",
                                    tname = "Year",
                                    xformla = ~1,
                                    data = Women_Extensive,
                                    allow_unbalanced_panel = TRUE,
                                    est_method = "reg")

summary(ATT_Women_Extensive,cluster = c("Area"))
ggdid(ATT_Women_Extensive, ylim = c(-15,10))

# Simple Aggregation:

ATT_Extensive_Women_Simple <- aggte(ATT_Women_Extensive, type = "simple")
summary(ATT_Extensive_Women_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Women_Extensive <- aggte(ATT_Women_Extensive, type = "dynamic")
summary(Dynamic_Women_Extensive,cluster = c("Area"))
ggdid(Dynamic_Women_Extensive)

# ATT for each group:

Group_Effects_Women_Extensive <- aggte(ATT_Women_Extensive, type = "group")
summary(Group_Effects_Women_Extensive,cluster = c("Area"))

# b) For men:

ATT_Men_Extensive <- att_gt(yname = "employed",
                                  gname = "Year_Transfer",
                                  idname = "llave_ID_lb",
                                  tname = "Year",
                                  xformla = ~1,
                                  data = Men_Extensive,
                                  allow_unbalanced_panel = TRUE,
                                  est_method = "reg")

summary(ATT_Men_Extensive,cluster = c("Area"))
ggdid(ATT_Men_Extensive, ylim = c(-15,10))

# Simple Aggregation:

ATT_Extensive_Men_Simple <- aggte(ATT_Men_Extensive, type = "simple")
summary(ATT_Extensive_Men_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Men_Extensive<- aggte(ATT_Men_Extensive, type = "dynamic")
summary(Dynamic_Men_Extensive,cluster = c("Area"))
ggdid(Dynamic_Men_Extensive)

# ATT for each group:

Group_Effects_Men_Extensive <- aggte(ATT_Men_Extensive, type = "group")
summary(Group_Effects_Men_Extensive,cluster = c("Area"))

######################### IMPACT FAMILIAS EN ACCION ON DECISION TO WORK BY HOUSEHOLD HEADS - SINGLE

# For this additional analysis, we will use the "Household_Single" dataframe. As we did with 
# Household_Heads, we will add a new variable called employed in order to determine whether
# individuals were part of the labor market. 

Household_Single$employed <- as.integer(Household_Single$horas_normal > 0)

# Approach: Callaway and Sant'Anna (2021) / Group-Time Average Treatment Effects ---------------------------------

ATT_Household_Single_Extensive <- att_gt(yname = "employed",
                               tname = "Year",
                               idname = "llave_ID_lb",
                               gname = "Year_Transfer",
                               xformla = ~1,
                               data = Household_Single,
                               allow_unbalanced_panel = TRUE,
                               est_method = "reg")

summary(ATT_Household_Single_Extensive,cluster = c("Area"))
ggdid(ATT_Household_Single_Extensive, ylim = c(-15,10))

# Simple Aggregation:

ATT_Household_Single_Extensive_Simple <- aggte(ATT_Household_Single_Extensive, type = "simple")
summary(ATT_Household_Single_Extensive_Simple,cluster = c("Area"))
save(ATT_Household_Single_Extensive_Simple, file="models/main_extensive_simple_single.Rdata")

# Dynamic ATT (Event-Study):

Dynamic_Household_Extensive_Single <- aggte(ATT_Household_Single_Extensive, type = "dynamic")
summary(Dynamic_Household_Extensive_Single,cluster = c("Area"))
ggdid(Dynamic_Household_Extensive_Single)
save(Dynamic_Household_Extensive_Single, file="models/main_extensive_dynamic_single.Rdata")

# ATT for each group:

Group_Effects_Household_Extensive_Single <- aggte(ATT_Household_Single_Extensive, type = "group")
summary(Group_Effects_Household_Extensive_Single,cluster = c("Area"))
save(Group_Effects_Household_Extensive_Single, file="models/main_extensive_group_single.Rdata")

################################################ BY GENDER

## FEMALE

Women_Household_Single_Extensive <- Household_Single %>%
  filter(sexo == 1)

## MALE

Men_Household_Single_Extensive <- Household_Single %>%
  filter(sexo == 0)

# a) For women:

ATT_Women_Household_Extensive_Single <- att_gt(yname = "employed",
                                     gname = "Year_Transfer",
                                     idname = "llave_ID_lb",
                                     tname = "Year",
                                     xformla = ~1,
                                     data = Women_Household_Single_Extensive,
                                     allow_unbalanced_panel = TRUE,
                                     est_method = "reg")

summary(ATT_Women_Household_Extensive_Single,cluster = c("Area"))
ggdid(ATT_Women_Household_Extensive_Single, ylim = c(-15,10))

# Simple Aggregation:

ATT_Household_Single_Women_Extensive_Simple <- aggte(ATT_Women_Household_Extensive_Single, type = "simple")
summary(ATT_Household_Single_Women_Extensive_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Women_Household_Extensive_Single <- aggte(ATT_Women_Household_Extensive_Single, type = "dynamic")
summary(Dynamic_Women_Household_Extensive_Single,cluster = c("Area"))
ggdid(Dynamic_Women_Household_Extensive_Single)

# ATT for each group:

Group_Effects_Women_Household_Extensive_Single <- aggte(ATT_Women_Household_Extensive_Single, type = "group")
summary(Group_Effects_Women_Household_Extensive_Single,cluster = c("Area"))

# b) For men:

ATT_Men_Household_Extensive_Single <- att_gt(yname = "employed",
                                   gname = "Year_Transfer",
                                   idname = "llave_ID_lb",
                                   tname = "Year",
                                   xformla = ~1,
                                   data = Men_Household_Single_Extensive,
                                   allow_unbalanced_panel = TRUE,
                                   est_method = "reg")

summary(ATT_Men_Household_Extensive_Single,cluster = c("Area"))
ggdid(ATT_Men_Household_Extensive_Single, ylim = c(-15,10))

# Simple Aggregation:

ATT_Household_Single_Men_Extensive_Simple <- aggte(ATT_Men_Household_Extensive_Single, type = "simple")
summary(ATT_Household_Single_Men_Extensive_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Men_Household_Heads_Extensive_Single <- aggte(ATT_Men_Household_Extensive_Single, type = "dynamic")
summary(Dynamic_Men_Household_Heads_Extensive_Single,cluster = c("Area"))
ggdid(Dynamic_Men_Household_Heads_Extensive_Single)

# ATT for each group:

Group_Effects_Men_Household_Heads_Extensive_Single <- aggte(ATT_Men_Household_Extensive_Single, type = "group")
summary(Group_Effects_Men_Household_Heads_Extensive_Single,cluster = c("Area"))

 
