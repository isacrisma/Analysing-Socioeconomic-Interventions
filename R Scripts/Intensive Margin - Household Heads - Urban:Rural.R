############################################################################################################################ 
############################################################################################################################ 
##################### Master Thesis: Impact of Familias en Acción on HH Labor Market Decisions ##################### 

## ANALYSIS 1: 
## Impact of Familias en Acción on hours worked (intensive margin) 
## This analysis considers both the Urban and the Rural samples.

##################################### ANALYSIS 1 - Household Heads & Hours Worked ##################################### 
## Sample:
# For this analysis, we consider only household heads, identified by the variable 'parentesco'.
# 'Parentesco' takes a value of 1 if the individual is the head of the household and 2 if he/she is the spouse. 

Household_Heads <- subset(Urban_Rural,parentesco %in% c(1, 2))

# For the analysis below, we will consider the sample Household_Heads

# We exclude individuals who were receiving treatment in 2010.
# We also exclude individuals who received treatment in 2013 but stopped receiving it in 2016

ids.treat.2013 <- unique(subset(Household_Heads, Year == 2013 & familias_accion == 1)$llave_ID_lb)
ids.not.2016 <- unique(subset(Household_Heads, Year == 2016 & familias_accion == 0)$llave_ID_lb)
ids.out <- intersect(ids.treat.2013, ids.not.2016)

Household_Heads$leavetreat <- Household_Heads$llave_ID_lb %in% ids.out

Household_Heads <- Household_Heads %>%
  group_by(llave_ID_lb) %>%
  filter(all(leavetreat == "FALSE")) %>%
  select(-leavetreat)

# Second, we create a Treated variable. This dummy variable identifies treated individuals.

Household_Heads$Treated <- ifelse(Household_Heads$llave_ID_lb %in% Household_Heads$llave_ID_lb[Household_Heads$Year == 2013 & Household_Heads$familias_accion == 1] |
                                    Household_Heads$llave_ID_lb %in% Household_Heads$llave_ID_lb[Household_Heads$Year == 2016 & Household_Heads$familias_accion == 1], 1, 0)

# Create another variable called Treatment. This variable takes a value of 1 if the individual
# is receiving treatment in a given year (will be useful for later computations).

Household_Heads <- Household_Heads %>%
  mutate(Treatment = ifelse(familias_accion == 1, 1, 0))

# Now we want to consider only those individuals from the Control Group who are similar to the individuals in the
# Treatment Group. To achieve this, we use Propensity Score Matching to 'build' a comparable Control Group. 

# Using the matchit function from MatchIt to match each treated with a non-treated (1 to 1 matching) based on
# on a number of relevant covariates. 

Match <- matchit(Treated ~ Age + Age^2  + House_Ownership  +
                     Washing_Machine  + Wall_Materials + Waste_Services +
                     Energy_Access + Water_Source,
                   data = Household_Heads, method = "nearest", distance ="glm",
                   ratio = 1,
                   replace = FALSE)

summary(Match)

# Plotting the standardized mean differences between treated and non-treated before and 
# after matching.

love.plot(Match, binary = "std", thresholds = c(m = .1))
plot(Match, type = "hist")

# Extract the matched data and save the data

Household_Heads <- match.data(Match)

# We want only complete cases. Include only individuals who are tracked all years 

Household_Heads <- Household_Heads %>% 
  group_by(llave_ID_lb) %>%
  dplyr::mutate(cnt = n()) %>%
  filter(cnt == 3) %>%
  select(-cnt)

######################### IMPACT FAMILIAS EN ACCION ON HOURS WORKED BY HOUSEHOLD HEADS - WHOLE

# Approach: Callaway and Sant'Anna (2021) / Group-Time Average Treatment Effects ---------------------------------

# We start by creating a new variable 'Year_Transfer' initialized as empty and 
# set its value  based on the Treatment variable. Here we want to make sure that 'Year_Transfer'
# displays the "true" year when an individual started receiving Treatment (i.e.: if Treatment 
# started in 2016, variable = 2016)

ids.treated2013 <- subset(Household_Heads, Treatment == 1 & Year == 2013)$llave_ID_lb
ids.treated2016 <- subset(Household_Heads, Treatment == 1 & Year == 2016)$llave_ID_lb
ids.treated2016 <- setdiff(ids.treated2016, ids.treated2013)
Household_Heads$Year_Transfer <- 0
Household_Heads$Year_Transfer[Household_Heads$llave_ID_lb %in% ids.treated2013] <- 2013
Household_Heads$Year_Transfer[Household_Heads$llave_ID_lb %in% ids.treated2016] <- 2016

ATT_Intensive <- att_gt(yname = "horas_normal",
                              tname = "Year",
                              idname = "llave_ID_lb",
                              gname = "Year_Transfer",
                              xformla = ~1,
                              data = Household_Heads,
                              est_method = "reg")

summary(ATT_Intensive,cluster = c("Area"))
ggdid(ATT_Intensive, ylim = c(-15,10))

# Simple Aggregation:

ATT_Simple <- aggte(ATT_Intensive, type = "simple")
summary(ATT_Simple,cluster = c("Area"))
save(ATT_Simple, file="models/main_intensive_simple.Rdata")

# Dynamic ATT (Event-Study):

Dynamic_Intensive <- aggte(ATT_Intensive, type = "dynamic")
summary(Dynamic_Intensive,cluster = c("Area"))
ggdid(Dynamic_Intensive)
save(Dynamic_Intensive, file="models/main_intensive_dynamic.Rdata")

# ATT for each group:

Group_Effects_Intensive <- aggte(ATT_Intensive, type = "group")
summary(Group_Effects_Intensive,cluster = c("Area"))
save(Group_Effects_Intensive, file="models/main_intensive_group.Rdata")

################################################ BY GENDER

## FEMALE

Women_Household_Heads <- Household_Heads %>%
  filter(sexo == 1)

## MALE

Men_Household_Heads <- Household_Heads %>%
  filter(sexo == 0)

# a) For women:

ATT_Women_Intensive <- att_gt(yname = "horas_normal",
                                    gname = "Year_Transfer",
                                    idname = "llave_ID_lb",
                                    tname = "Year",
                                    xformla = ~1,
                                    data = Women_Household_Heads,
                                    allow_unbalanced_panel = TRUE,
                                    est_method = "reg")

summary(ATT_Women_Intensive,cluster = c("Area"))
ggdid(ATT_Women_Intensive, ylim = c(-15,10))

# Simple Aggregation:

ATT_Women_Simple <- aggte(ATT_Women_Intensive, type = "simple")
summary(ATT_Women_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Women_Intensive <- aggte(ATT_Women_Intensive, type = "dynamic")
summary(Dynamic_Women_Intensive,cluster = c("Area"))
ggdid(Dynamic_Women_Intensive)

# ATT for each group:

Group_Effects_Women_Intensive <- aggte(ATT_Women_Intensive, type = "group")
summary(Group_Effects_Women_Intensive,cluster = c("Area"))

# b) For men:

ATT_Men_Intensive <- att_gt(yname = "horas_normal",
                                  gname = "Year_Transfer",
                                  idname = "llave_ID_lb",
                                  tname = "Year",
                                  xformla = ~1,
                                  data = Men_Household_Heads,
                                  allow_unbalanced_panel = TRUE,
                                  est_method = "reg")

summary(ATT_Men_Intensive,cluster = c("Area"))
ggdid(ATT_Men_Intensive, ylim = c(-15,10))

# Simple Aggregation:

ATT_Men_Simple <- aggte(ATT_Men_Intensive, type = "simple")
summary(ATT_Men_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Men_Intensive <- aggte(ATT_Men_Intensive, type = "dynamic")
summary(Dynamic_Men_Intensive,cluster = c("Area"))
ggdid(Dynamic_Men_Intensive)

# ATT for each group:

Group_Effects_Men_Intensive <- aggte(ATT_Men_Intensive, type = "group")
summary(Group_Effects_Men_Intensive,cluster = c("Area"))

######################### IMPACT FAMILIAS EN ACCION ON HOURS WORKED BY HOUSEHOLD HEADS - SINGLE

# For this additional analysis, we will consider single household heads. That is, heads of households
# with no partner. We will directly use our dataframe "Household_Heads" with all relevant variables and the 
# "matched" Control Group.

Household_Single <- Household_Heads %>%
  group_by(consecutivo) %>%
  filter(all(parentesco != 2)) %>%
  ungroup()

# Approach: Callaway and Sant'Anna (2021) / Group-Time Average Treatment Effects ---------------------------------

# We start by creating a new variable 'Year_Transfer' initialized as empty and 
# set its value  based on the Treated variable

ids.treated2013 <- subset(Household_Single, Treatment == 1 & Year == 2013)$llave_ID_lb
ids.treated2016 <- subset(Household_Single, Treatment == 1 & Year == 2016)$llave_ID_lb
ids.treated2016 <- setdiff(ids.treated2016, ids.treated2013)
Household_Single$Year_Transfer <- 0
Household_Single$Year_Transfer[Household_Single$llave_ID_lb %in% ids.treated2013] <- 2013
Household_Single$Year_Transfer[Household_Single$llave_ID_lb %in% ids.treated2016] <- 2016

ATT_Household_Single <- att_gt(yname = "horas_normal",
                              tname = "Year",
                              idname = "llave_ID_lb",
                              gname = "Year_Transfer",
                              xformla = ~1,
                              data = Household_Single,
                              allow_unbalanced_panel = TRUE,
                              est_method = "reg")

summary(ATT_Household_Single, cluster = c("Area"))
ggdid(ATT_Household_Single, ylim = c(-15,10))

# Simple Aggregation:

ATT_Single_Simple <- aggte(ATT_Household_Single, type = "simple")
summary(ATT_Single_Simple,cluster = c("Area"))
save(ATT_Single_Simple, file="models/main_intensive_simple_single.Rdata")

# Dynamic ATT (Event-Study):

Dynamic_Household_Single <- aggte(ATT_Household_Single, type = "dynamic")
summary(Dynamic_Household_Single, cluster = c("Area"))
ggdid(Dynamic_Household_Single)
save(Dynamic_Household_Single, file="models/main_intensive_dynamic_single.Rdata")

# ATT for each group:

Group_Effects_Household_Single <- aggte(ATT_Household_Single, type = "group")
summary(Group_Effects_Household_Single, cluster = c("Area"))
save(Group_Effects_Household_Single, file="models/main_intensive_group_single.Rdata")

################################################ BY GENDER

## FEMALE

Women_Household_Single <- Household_Single %>%
  filter(sexo == 1)

## MALE

Men_Household_Single <- Household_Single %>%
  filter(sexo == 0)

# a) For women:

ATT_Women_Household_Single <- att_gt(yname = "horas_normal",
                                    gname = "Year_Transfer",
                                    idname = "llave_ID_lb",
                                    tname = "Year",
                                    xformla = ~1,
                                    data = Women_Household_Single,
                                    allow_unbalanced_panel = TRUE,
                                    est_method = "reg")

summary(ATT_Women_Household_Single,cluster = c("Area"))
ggdid(ATT_Women_Household_Single, ylim = c(-15,10))


# Simple Aggregation:

ATT_Women_Household_Single_Simple <- aggte(ATT_Women_Household_Single, type = "simple")
summary(ATT_Women_Household_Single_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Women_Household_Single <- aggte(ATT_Women_Household_Single, type = "dynamic")
summary(Dynamic_Women_Household_Single,cluster = c("Area"))
ggdid(Dynamic_Women_Household_Single)

# ATT for each group:

Group_Effects_Women_Household_Single <- aggte(ATT_Women_Household_Single, type = "group")
summary(Group_Effects_Women_Household_Single,cluster = c("Area"))

# b) For men:

ATT_Men_Household_Single <- att_gt(yname = "horas_normal",
                                  gname = "Year_Transfer",
                                  idname = "llave_ID_lb",
                                  tname = "Year",
                                  xformla = ~1,
                                  data = Men_Household_Single,
                                  allow_unbalanced_panel = TRUE,
                                  est_method = "reg")

summary(ATT_Men_Household_Single,cluster = c("Area"))
ggdid(ATT_Men_Household_Single, ylim = c(-15,10))

# Simple Aggregation:

ATT_Men_Household_Single_Simple <- aggte(ATT_Men_Household_Single, type = "simple")
summary(ATT_Men_Household_Single_Simple,cluster = c("Area"))

# Dynamic ATT (Event-Study):

Dynamic_Men_Household_Single <- aggte(ATT_Men_Household_Single, type = "dynamic")
summary(Dynamic_Men_Household_Single,cluster = c("Area"))
ggdid(Dynamic_Men_Household_Single)

# ATT for each group:

Group_Effects_Men_Household_Single <- aggte(ATT_Men_Household_Single, type = "group")
summary(Group_Effects_Men_Household_Single,cluster = c("Area"))
