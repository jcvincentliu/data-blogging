
# This function takes in a data frame and a grouping variable representing the anti-violence 
# method used by the schools and returns a summary table that showcase the percentage of 
# schools adopting this strategy by the number of violent incidents occurred at the school. 

library(tidyverse)

get_strategy_by_violent_incident <- function(data, method_var, incident_var) { 
    
    method_name <- expss::var_lab(data %>% select({{method_var}}))  # subtract variable label as string
  
    df1 <- data %>% 
    mutate(`Violent Incident Group` =
             case_when(
               {{incident_var}} == 0 ~ "No \nviolent\n incidents",
               {{incident_var}} >0 & {{incident_var}} <=5 ~ 
                                        "Between \n1 and 5\n violent\n incidents",
               {{incident_var}} >5 & {{incident_var}} <=20 ~ 
                                        "Between \n5 and 20\n violent\n incidents",
               {{incident_var}} >20 ~ 
                 "Over 20\n violent\n incidents"))  %>%
    mutate(`Violent Incident Group` = factor(`Violent Incident Group`,  # factorize to keep order
                                             levels = c( 
                                               "No \nviolent\n incidents",
                                               "Between \n1 and 5\n violent\n incidents",
                                               "Between \n5 and 20\n violent\n incidents",
                                               "Over 20\n violent\n incidents"))) %>%
    select({{method_var}}, `Violent Incident Group`) %>%
    filter({{method_var}} == 1) # only include schools who adopt this method
    
  df <- df1 %>%
    group_by(`Violent Incident Group`) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    mutate(Percent = round(Count/nrow(df1),
                           4)) %>%
    mutate(Strategy = method_name,
           .before = `Violent Incident Group`)

    
  return(df) 
}

# Note:
# incident_var is the variable that represents the number of violent incidents at the school
# incident_var should be in the format of VIOINC+xx (xx is the last two digits of the year)

## To use the function, the codes should look like:
##    source(path+function_name)  # path should be relative path, ideally
##    get_strategy_by_violent_incident(ssocs18, C0110, VIOINC18)


