## Robustness Check

### INTENSIVE MARGIN 

## 1. Household Heads - Full Sample 

robust_full =
  etwfe(
    fml  = horas_normal ~ llave_ID_lb, 
    tvar = Year,      
    gvar = Year_Transfer, 
    data = Household_Heads,       
  )

summary(robust_full)
emfx(robust_full) #simple
emfx(robust_full, type = "event")
emfx(robust_full, type = "group")

## 2. Household Heads - Single Sample 

robust_single =
  etwfe(
    fml  = horas_normal ~ llave_ID_lb, 
    tvar = Year,      
    gvar = Year_Transfer, 
    data = Household_Single,       
  )

summary(robust_single)
emfx(robust_single) #simple
emfx(robust_single, type = "event")
emfx(robust_single, type = "group")

### EXTENSIVE MARGIN 

## 1. Household Heads - Full Sample 

robust_ext =
  etwfe(
    fml  = employed ~ llave_ID_lb, 
    tvar = Year,      
    gvar = Year_Transfer, 
    data = Household_Heads,       
  )

summary(robust_ext)
emfx(robust_ext)
emfx(robust_ext, type = "event")
emfx(robust_ext, type = "group")

## 2. Household Heads - Single Sample 

robust_ext_single =
  etwfe(
    fml  = employed ~ llave_ID_lb, 
    tvar = Year,      
    gvar = Year_Transfer, 
    data = Household_Single,       
  )

summary(robust_ext_single)
emfx(robust_ext_single)
emfx(robust_ext_single, type = "event")
emfx(robust_ext_single, type = "group")


### INFORMALITY 

## 1. Household Heads - Full Sample 

robust_infor =
  etwfe(
    fml  = Informal ~ llave_ID_lb, 
    tvar = Year,      
    gvar = Year_Transfer, 
    data = Informality,       
  )

summary(robust_infor)
emfx(robust_infor)
emfx(robust_infor, type = "event")
emfx(robust_infor, type = "group") 

## 2. Household Heads - Single Sample 

robust_infor_single =
  etwfe(
    fml  = Informal ~ llave_ID_lb, 
    tvar = Year,      
    gvar = Year_Transfer, 
    data = Informality_Single,       
  )

summary(robust_infor_single)
emfx(robust_infor_single)
emfx(robust_infor_single, type = "event")
emfx(robust_infor_single, type = "group") 
