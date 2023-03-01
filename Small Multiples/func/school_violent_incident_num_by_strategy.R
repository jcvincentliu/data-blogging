


 library(tidyverse)
 
 # helper function to get a table that ....
 table_given_violent_inci_and_strategy <- function(data, method_var, incident_var, cutoff_range) { 
   
   low_num <- cutoff_range[1]
   higher_num <- cutoff_range[2]
   
   method_name <- expss::var_lab(data %>% select({{method_var}}))  # subtract variable label as string
   
   df1 <- data %>% 
     mutate(`Violent Incident Group` =
              case_when(
                {{incident_var}} == 0 ~ "No Violent Incidents",
                {{incident_var}} >0 & {{incident_var}} <=5 ~ 
                  "Between 1 and 5 violent incidents",
                {{incident_var}} >5 & {{incident_var}} <=20 ~ 
                  "Between 5 and 20 violent incidents",
                {{incident_var}} >20 ~ 
                  "Over 20 violent incidents"))  %>%
     mutate(`Violent Incident Group` = factor(`Violent Incident Group`,  # factorize to keep order
                                              levels = c( 
                                                "No violent incidents",
                                                "Between 1 and 5 violent incidents",
                                                "Between 5 and 20 violent incidents",
                                                "Over 20 violent incidents"))) %>%
     select({{method_var}}, `Violent Incident Group`, {{incident_var}}) %>%
     mutate(Strategy = method_name,
            .before = `Violent Incident Group`)
   
   if (higher_num == 0) {
     df1 <- df1 %>%
       filter({{incident_var}} == 0) %>%
       select(!{{incident_var}})
   } else {
     df1 <- df1 %>%
       filter({{incident_var}} > low_num,
              {{incident_var}} <= higher_num) %>% # filter by incident_number group
       select(!{{incident_var}})
   }
   
   df <- df1 %>%
     group_by({{method_var}}) %>%
     summarise(Strategy = Strategy,
               `Violent Incident Group` = `Violent Incident Group`,
               Count = n(),
               Percent = round(Count/nrow(df1),
                               4)) %>%
     ungroup() %>%
     filter({{method_var}} ==1) %>%
     select(!{{method_var}}) %>%
     head(1)
   
   if (higher_num == 0) { 
     df <- df %>%
       mutate(`Violent Incident Group` = "No violent incidents")
   }
   
   return(df)
 }

  # Note:
  
  # cutoff_range should be in the format of a vector and must take one of the following: 
  #                   (0,0), (0,5), (5,20), or (20, 298), 
  # in which the first number is the lower bound and the second number is the upper bound 
 
 
 get_violent_incident_by_strategy <- function(data, method_var, incident_var) {
   t1 <- table_given_violent_inci_and_strategy(data, {{method_var}}, 
                                               {{incident_var}},
                                               c(0,0))
   t2 <- table_given_violent_inci_and_strategy(data, {{method_var}}, 
                                               {{incident_var}},
                                               c(0,5))
   t3 <- table_given_violent_inci_and_strategy(data, {{method_var}},
                                               {{incident_var}},
                                               c(5,20))
   t4 <- table_given_violent_inci_and_strategy(data, {{method_var}},
                                               {{incident_var}},
                                               c(20,298))
   
   t <- rbind(t1,t2,t3,t4)
   
   return(t)
 }

