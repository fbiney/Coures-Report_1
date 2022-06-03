# Data


# Load Library----
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(skimr)
library(funModeling)


# Load  original Data

setwd("C:/Users/fbiney/Desktop/WaterProj/Coures-Report_1/Code_Report")

dat <- read_csv("communities_03_30_003.csv")
names(dat) # or  colnames(dat) 



# Clean columns and Rename Variables


library('janitor')
dat1 <-dat %>%
  clean_names()%>%
  rename(
    id = census_tract_id,
    county=county_name,                                                                                                                                                                     
    state=state_territory,
    T_threshld =total_threshold_criteria_exceeded,
    T_cat_exceded=total_categories_exceeded,
    is_disAdvan=identified_as_disadvantaged,
    LILpHE=is_low_income_and_has_a_low_percent_of_higher_ed_students,
    Expt.AgricLoss.gde90_LILpHE=greater_than_or_equal_to_the_90th_percentile_for_expected_agriculture_loss_rate_is_low_income_and_has_a_low_percent_of_higher_ed_students,
    Expt.AgricLoss_Natu.Haz.Risk.Pec=expected_agricultural_loss_rate_natural_hazards_risk_index_percentile,
    Expt.AgricLoss_Natu.Haz.Risk.Indx=expected_agricultural_loss_rate_natural_hazards_risk_index,
    Expt.BldLoss.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_expected_building_loss_rate_is_low_income_and_has_a_low_percent_of_higher_ed_students,
    Expt.BldLoss_Natu.Haz.Risk.Pec=expected_building_loss_rate_natural_hazards_risk_index_percentile,
    Expt.BldLoss_Natu.Haz.Risk.Indx=expected_building_loss_rate_natural_hazards_risk_index,
    Expt.PopLoss.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_expected_population_loss_rate_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                     
    Expt.PopLoss_Natu.Haz.Risk.Pec=expected_population_loss_rate_natural_hazards_risk_index_percentile,                    
    Expt.PopLoss_Natu.Haz.Risk.Indx=expected_population_loss_rate_natural_hazards_risk_index,
    Energy.burd.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_energy_burden_is_low_income_and_has_a_low_percent_of_higher_ed_students,
    Energy_burd.pect=energy_burden_percentile,                                                                                                                                                         
    Ennergy_burd=energy_burden,
    pm2.5_expo.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_pm2_5_exposure_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                                    
    pm2.5_air.pect=pm2_5_in_the_air_percentile,                                                                                                                                                      
    pm2.5_air=pm2_5_in_the_air,
    Expt.diesel.p.Matter.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_diesel_particulate_matter_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                        
    Diesel.p.matter.expo.pect=diesel_particulate_matter_exposure_percentile,                                                                                                                                
    Diesel.p.matter.expo=diesel_particulate_matter_exposure,
    traffic.prox.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_traffic_proximity_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                                 
    traffi.prox.pect=traffic_proximity_and_volume_percentile,                                                                                                                                        
    traffi.prox=traffic_proximity_and_volume,                                                                                                                                                     
    House.burd.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_housing_burden_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                                    
    Housing.burd.p.ptil=housing_burden_percent_percentile,                                                                                                                                                
    Housing.burd.p=housing_burden_percent,                                                                                                                                                           
    Ld.Paint.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_lead_paint_the_median_house_value_is_less_than_90th_percentile_is_low_income_and_has_a_low_percent_of_higher_ed_students,        
    Ld.paint.1960.ptil=percent_pre_1960s_housing_lead_paint_indicator_percentile,                                                                                                                    
    Ld.paint.1960.indx=percent_pre_1960s_housing_lead_paint_indicator,                                                                                                                               
    median.value_owner.ptil=median_value_of_owner_occupied_housing_units_percentile,                                                                                                                      
    median.value_owner=median_value_of_owner_occupied_housing_units,                                                                                                                                  
    prox.hazard.waste.facilit_gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_proximity_to_hazardous_waste_facilities_is_low_income_and_has_a_low_percent_of_higher_ed_students,                           
    prox.hazard.waste.site.ptil=proximity_to_hazardous_waste_sites_percentile,                                                                                                                                
    prox.hazard.waste.site=proximity_to_hazardous_waste_sites,                                                                                                                                            
    prox.superfund.sites.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_proximity_to_superfund_sites_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                      
    prox.npl.sites.ptil=proximity_to_npl_sites_percentile,                                                                                                                                            
    prox.npl.sites=proximity_to_npl_sites,                                                                                                                                                           
    prox.rmp.sites.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_proximity_to_rmp_sites_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                            
    prox.risk.mngt.rmp_facilit.ptil=proximity_to_risk_management_plan_rmp_facilities_percentile,                                                                                                                  
    prox.risk.mngt.rmp_facilit=proximity_to_risk_management_plan_rmp_facilities,                                                                                                                             
    wastewater.dischg.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_wastewater_discharge_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                              
    wastewater.dischg.ptil=wastewater_discharge_percentile,                                                                                                                                               
    wastewater.dischg=wastewater_discharge,                                                                                                                                                             
    Asthmag.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_asthma_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                                             
    Asthma.Adults.age18.ptil=current_asthma_among_adults_aged_greater_than_or_equal_to_18_years_percentile,                                                                                                 
    Asthma.Adults.age18=current_asthma_among_adults_aged_greater_than_or_equal_to_18_years,                                                                                                           
    Diabetes.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_diabetes_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                                          
    Diabetes.Adults.age18.diag.ptil=diagnosed_diabetes_among_adults_aged_greater_than_or_equal_to_18_years_percentile,                                                                                            
    Diabetes.Adults.age18.diag=diagnosed_diabetes_among_adults_aged_greater_than_or_equal_to_18_years,                                                                                                       
    Heart.dis.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_heart_disease_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                                         
    Heart.dis.Adults.age18.ptil=coronary_heart_disease_among_adults_aged_greater_than_or_equal_to_18_years_percentile,                                                                                        
    Heart.dis.Adults.age18=coronary_heart_disease_among_adults_aged_greater_than_or_equal_to_18_years,                                                                                                    
    Low.life.expectcy.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_low_life_expectancy_is_low_income_and_has_a_low_percent_of_higher_ed_students,                                               
    Low.life.expectcy.ptil=low_life_expectancy_percentile,                                                                                                                                                
    life.expectcy_yrs=life_expectancy_years,                                                                                                                                                            
    Low.Median.household.income.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_low_median_household_income_as_a_percent_of_area_median_income_has_low_hs_attainment_and_has_a_low_percent_of_higher_ed_students,
    Low.Median.household.income.ptil=low_median_household_income_as_a_percent_of_area_median_income_percentile,                                                                                                    
    median.house.income_Area.incom=median_household_income_as_a_percent_of_area_median_income,                                                                                                                   
    HouseHd.linguistic.isolation.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_households_in_linguistic_isolation_has_low_hs_attainment_and_has_a_low_percent_of_higher_ed_students,                        
    linguistic.isolation.p.ptil=linguistic_isolation_percent_percentile,                                                                                                                                      
    linguistic.isolation.p=linguistic_isolation_percent,                                                                                                                                                  
    unemplt.low.hs.attainment.gde90_LILE=greater_than_or_equal_to_the_90th_percentile_for_unemployment_has_low_hs_attainment_and_has_a_low_percent_of_higher_ed_students,                                              
    unemplt.p.ptil=unemployment_percent_percentile,                                                                                                                                                  
    unemplt.p=unemployment_percent,                                                                                                                                                             
    below.100pct.fed.povty.lvl.gtd90_LILE=greater_than_or_equal_to_the_90th_percentile_for_households_at_or_below_100_percent_federal_poverty_level_has_low_hs_attainment_and_has_a_low_percent_of_higher_ed_students,      
    below.200pct.fed.povty.Ln=percent_of_individuals_below_200_percent_federal_poverty_line_percentile,                                                                                                         
    i00pct.fed.povty=percent_of_individuals_100_percent_federal_poverty_line_percentile,                                                                                                               
    below.200pct.fed.povty.ln=percent_of_individuals_below_200_percent_federal_poverty_line,                                                                                                                    
    i00pct.fed.povty.ln=percent_of_individuals_100_percent_federal_poverty_line,                                                                                                                          
    Age.25.less.HS.deg.ptil=percent_individuals_age_25_or_over_with_less_than_high_school_degree_percentile,                                                                                                  
    Age.25.less.HS.deg=percent_individuals_age_25_or_over_with_less_than_high_school_degree,                                                                                                             
    unempt.pct.2009=unemployment_percent_in_2009_island_areas_and_2010_states_and_pr,                                                                                                                 
    houshd.pct.blow.100.pct.fed.povty.Ln.2009=percentage_households_below_100_percent_of_federal_poverty_line_in_2009_island_areas_and_2010_states_and_pr,                                                                      
    unemplt.gde90_low.hs2009.islandAreas=greater_than_or_equal_to_the_90th_percentile_for_unemployment_and_has_low_hs_education_in_2009_island_areas,                                                                      
    houshd.blow.100.pct.fed.povty.lvl.gde900_low.hs2009.islandAreas=greater_than_or_equal_to_the_90th_percentile_for_households_at_or_below_100_percent_federal_poverty_level_and_has_low_hs_education_in_2009_island_areas, 
    low.med.househd.income_Area.incom=greater_than_or_equal_to_the_90th_percentile_for_low_median_household_income_as_a_percent_of_area_median_income_and_has_low_hs_education_in_2009_island_areas
  ) 






# Water quality related variables and Demographic  x'tics s.a Income,Educ.,soco. Economic

dat2 <- dat1 %>% 
  select (id                                                       
          ,county                                                         
          ,state
          ,is_disAdvan 
          ,T_threshld
          ,T_cat_exceded
          ,LILpHE
          ,Ld.Paint.gde90_LILE                      
          ,Ld.paint.1960.ptil                                 
          ,Ld.paint.1960.indx 
          ,prox.hazard.waste.facilit_gde90_LILE                           
          ,prox.hazard.waste.site.ptil                                    
          ,prox.hazard.waste.site 
          ,wastewater.dischg.gde90_LILE                                   
          ,wastewater.dischg.ptil                                         
          ,wastewater.dischg
          ,total_population   
          ,median.value_owner.ptil                                        
          ,median.value_owner                                             
          ,Asthmag.gde90_LILE                                             
          ,Asthma.Adults.age18.ptil                                       
          ,Asthma.Adults.age18                                            
          ,Diabetes.gde90_LILE                                            
          ,Diabetes.Adults.age18.diag.ptil                                
          ,Diabetes.Adults.age18.diag                                     
          ,Heart.dis.gde90_LILE                                           
          ,Heart.dis.Adults.age18.ptil                                    
          ,Heart.dis.Adults.age18                                         
          ,Low.life.expectcy.gde90_LILE                                   
          ,Low.life.expectcy.ptil                                         
          ,life.expectcy_yrs                                              
          ,Low.Median.household.income.gde90_LILE                         
          ,Low.Median.household.income.ptil                               
          ,median.house.income_Area.incom                                 
          ,HouseHd.linguistic.isolation.gde90_LILE                        
          ,linguistic.isolation.p.ptil                                    
          ,linguistic.isolation.p                                         
          ,unemplt.low.hs.attainment.gde90_LILE                           
          ,unemplt.p.ptil                                                 
          ,unemplt.p                                                      
          ,below.100pct.fed.povty.lvl.gtd90_LILE                          
          ,i00pct.fed.povty                                               
          ,i00pct.fed.povty.ln                                            
          ,Age.25.less.HS.deg.ptil                                        
          ,Age.25.less.HS.deg                                             
          ,unempt.pct.2009                                                
          ,houshd.pct.blow.100.pct.fed.povty.Ln.2009 
          
  )




# El Paso Water quatlity data


EP_WQ_df <- dat1 %>% 
  filter (state == "Texas",county =="El Paso County") %>% 
  select (id,                                                       
          county                                                         
          ,state                                                          
          ,T_threshld
          ,T_cat_exceded
          ,is_disAdvan                                                    
          ,total_population                                               
          #,LILpHE
          ,Ld.Paint.gde90_LILE                      
          ,Ld.paint.1960.ptil                                 
          #,Ld.paint.1960.indx 
          ,prox.hazard.waste.facilit_gde90_LILE                           
          ,prox.hazard.waste.site.ptil                                    
          ,prox.hazard.waste.site 
          ,wastewater.dischg.gde90_LILE                                   
          ,wastewater.dischg.ptil                                         
          ,wastewater.dischg                                              
          
  )%>% 
  mutate_if(is_logical, as_factor)
#%>% drop_na()




## Basic Demographic  x'tics s.a Income,Educ.,soco. Economic


Demo_df  <- dat1 %>% 
  filter (state == "Texas",county =="El Paso County") %>% 
  select(
    id,                                                             
    county,                                                         
    state,                                                          
    T_threshld,
    T_cat_exceded,
    is_disAdvan,                                                    
    total_population,                                               
    LILpHE,                                                         
    #Ld.Paint.gde90_LILE,                                            
    #Ld.paint.1960.ptil,                                             
    #Ld.paint.1960.indx,                                             
    median.value_owner.ptil,                                        
    median.value_owner,                                             
    Asthmag.gde90_LILE,                                             
    Asthma.Adults.age18.ptil,                                       
    Asthma.Adults.age18,                                            
    Diabetes.gde90_LILE,                                            
    Diabetes.Adults.age18.diag.ptil,                                
    Diabetes.Adults.age18.diag,                                     
    Heart.dis.gde90_LILE,                                           
    Heart.dis.Adults.age18.ptil,                                    
    Heart.dis.Adults.age18,                                         
    Low.life.expectcy.gde90_LILE,                                   
    Low.life.expectcy.ptil,                                         
    life.expectcy_yrs,                                              
    Low.Median.household.income.gde90_LILE,                         
    Low.Median.household.income.ptil,                               
    median.house.income_Area.incom,                                 
    HouseHd.linguistic.isolation.gde90_LILE,                        
    linguistic.isolation.p.ptil,                                    
    linguistic.isolation.p,                                         
    unemplt.low.hs.attainment.gde90_LILE,                           
    unemplt.p.ptil,                                                 
    unemplt.p,                                                      
    below.100pct.fed.povty.lvl.gtd90_LILE,                          
    i00pct.fed.povty,                                               
    i00pct.fed.povty.ln,                                            
    Age.25.less.HS.deg.ptil,                                        
    Age.25.less.HS.deg,                                             
    unempt.pct.2009,                                                
    houshd.pct.blow.100.pct.fed.povty.Ln.2009,  
  ) %>% 
  mutate_if(is_logical, as_factor) 
#%>% 
#  drop_na()






## Basic Demographic x'tics s.a Income,Educ.,soco. Economic health related
#measures




#dat1 %>%names()
EP_Demo_df  <- dat1 %>% 
  filter (state == "Texas",county =="El Paso County") %>% 
  select(
    id,                                                             
    county,                                                         
    state,
    is_disAdvan, 
    T_cat_exceded, 
    T_threshld,                                                     
    total_population,                                               
    LILpHE,                                                         
    median.value_owner.ptil,                                        
    median.value_owner,                                             
    Asthmag.gde90_LILE,                                             
    Asthma.Adults.age18.ptil,                                       
    Asthma.Adults.age18,                                            
    Diabetes.gde90_LILE,                                            
    Diabetes.Adults.age18.diag.ptil,                                
    Diabetes.Adults.age18.diag,                                     
    Heart.dis.gde90_LILE,                                           
    Heart.dis.Adults.age18.ptil,                                    
    Heart.dis.Adults.age18,                                         
    Low.life.expectcy.gde90_LILE,                                   
    Low.life.expectcy.ptil,                                         
    life.expectcy_yrs,                                              
    Low.Median.household.income.gde90_LILE,                         
    Low.Median.household.income.ptil,                               
    median.house.income_Area.incom,                                 
    HouseHd.linguistic.isolation.gde90_LILE,                        
    linguistic.isolation.p.ptil,                                    
    linguistic.isolation.p,                                         
    unemplt.low.hs.attainment.gde90_LILE,                           
    unemplt.p.ptil,                                                 
    unemplt.p,                                                      
    below.100pct.fed.povty.lvl.gtd90_LILE,                          
    i00pct.fed.povty,                                               
    i00pct.fed.povty.ln,                                            
    Age.25.less.HS.deg.ptil,                                        
    Age.25.less.HS.deg,                                             
    unempt.pct.2009,                                                
    houshd.pct.blow.100.pct.fed.povty.Ln.2009,  
  ) %>% 
  mutate_if(is_logical, as_factor) 
#%>% 
#drop_na()




# Tigris package 



library('tigris') # Census data manip.
options(tigris_use_cache = TRUE) # make download faster






### Create Texas Map


tx_county <- counties(state = "TX", year = 2010)

tx_county %>% glimpse()






### Create El Paso Map


tx_tract <- tracts(state = "TX", county = "El Paso",year = 2010)

tx_tract %>%  as_tibble()

tx_tract %>% glimpse()







# Extract the census tract from the tigris package
tx_tract_2 <- tx_tract %>% 
  select(4:13) %>% 
  rename(id = GEOID10)




### Combining water quality and censu tract data


WQ_tract_tbl <- tx_tract_2 %>%
  left_join(EP_WQ_df, by = c("id" = "id")) #%>% 
#drop_na()

































































