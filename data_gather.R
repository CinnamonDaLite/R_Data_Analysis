# This program takes the data, cleans it up, and sends it to the shiny R directory

# This is large data, so it takes a while to run
#setwd('./Statistical_Analysis')
carcrash = read.csv('https://data.cityofnewyork.us/api/views/h9gi-nx95/rows.csv?accessType=DOWNLOAD')
library(tidyverse)
library(lubridate)

#The big cleanup
carcrash = carcrash %>%
  pivot_longer(cols = starts_with('CONTRIBUTING.FACTOR'), 
               names_to = 'Vnum', values_to = 'Factors') %>%
  filter(Factors != '', Factors != 'Unspecified', Factors != '1', Factors != '80') %>%
  # getting list of factors categorized for clear graph reasons
  mutate('Drugs & Alcohol' = Factors == 'Drugs (illegal)' | 
           Factors == 'Drugs (Illegal)' | Factors == 'Alcohol Involvement',
         
         'Distracted Driving' = Factors == 'Driver Inattention/Distraction' |
           Factors == 'Cell Phone (hand-held)' | 
           Factors == 'Cell Phone (hand-Held)' |
           Factors == 'Cell Phone (hands-free)'|
           Factors == 'Texting' | Factors == 'Passenger Distraction' |
           Factors == 'Using On Board Navigation Device'| 
           Factors == 'Eating or Drinking' | 
           Factors == 'Listening/Using Headphones' |
           Factors == 'Other Electronic Device' |
           Factors == 'Outside Car Distraction',
         
         'Poor Driving behavior' = Factors == 'Aggressive Driving/Road Rage' |
           Factors == 'Backing Unsafely' | 
           Factors == 'Failure to Keep Right' | 
           Factors == 'Failure to Yield Right-of-Way' | 
           Factors == 'Following Too Closely' |
           Factors == 'Driver Inexperience' | 
           Factors == 'Passing or Lane Usage Improper' |
           Factors == 'Passing Too Closely' | 
           Factors == 'Traffic Control Disregarded' |
           Factors == 'Turning Improperly' | 
           Factors == 'Unsafe Lane Changing' | 
           Factors == 'Unsafe Speed',
         
         'Vehicle Problems' = Factors == 'Accelerator Defective' | 
           Factors == 'Brakes Defective' |
           Factors == 'Driverless/Runaway Vehicle'| 
           Factors == 'Headlights Defective' |
           Factors == 'Other Lighting Defects' | 
           Factors == 'Steering Failure' | 
           Factors == 'Tinted Windows' |
           Factors == 'Tire Failure/Inadequate' | 
           Factors == 'Vehicle Vandalism' | 
           Factors == 'Windshield Inadequate',
         
         'Health Problems' = Factors == 'Lost Consciousness' | 
           Factors == 'Fatigued/Drowsy'|
           Factors == 'Fell Asleep' |
           Factors == 'Illnes'|
           Factors == 'Illness'|
           Factors == 'Physical Disability'|
           Factors == 'Prescription Medication',
         
         'Road Problems/Road Confusion' = Factors == 'Lane Marking Improper/Inadequate'|
           Factors == 'Pavement Defective'|
           Factors == 'Pavement Slippery'|
           Factors == 'Shoulders Defective/Improper',
         
         'Other' = Factors == 'Pedestrian/Bicyclist/Other Pedestrian Error/Confusion'|
           Factors == 'Obstruction/Debris'|
           Factors == 'Glare'|
           Factors == 'Animals Action'|
           Factors == 'Other Vehicular'|
           Factors == 'Oversized Vehicle'|
           Factors == 'Reaction to Other Uninvolved Vehicle'|
           Factors == 'Reaction to Uninvolved Vehicle'|
           Factors == 'Tow Hitch Defective'|
           Factors == 'Traffic Control Device Improper/Non-Working'|
           Factors == 'View Obstructed/Limited') %>% #Categorizing factors
  pivot_longer(cols = 27:33, names_to = 'Categories') %>%
  filter(value == TRUE) %>% #saving factors as categories for quicker graphs and general concepts
  pivot_longer(cols = starts_with('VEHICLE.TYPE'), 
               names_to = 'Vtype', values_to = 'vehicle_type') %>%
  filter(vehicle_type == 'Sedan' | 
           vehicle_type == 'Station Wagon/Sport Utility Vehicle' |
           vehicle_type == 'Box Truck' |
           vehicle_type == 'Taxi' |
           vehicle_type == 'Pick-up Truck' |
           vehicle_type == 'Van' |
           vehicle_type == 'Ambulance' |
           vehicle_type == 'Bus' |
           vehicle_type == 'Garbage or Refuse' |
           vehicle_type == 'Motorcycle') %>%
  mutate(date = as_datetime(CRASH.DATE, format = '%m/%d/%Y'),
         time = as_datetime(CRASH.TIME, format = '%H:%M')) %>%
  mutate(week = weekdays(date), hour = hour(time))
  
carcrash = carcrash %>%
  mutate(injured = rowSums(carcrash[,c('NUMBER.OF.PERSONS.INJURED',
                                   'NUMBER.OF.PEDESTRIANS.INJURED',
                                   'NUMBER.OF.CYCLIST.INJURED',
                                   'NUMBER.OF.MOTORIST.INJURED')]),
         killed = rowSums(carcrash[,c('NUMBER.OF.PERSONS.KILLED',
                                  'NUMBER.OF.PEDESTRIANS.KILLED',
                                  'NUMBER.OF.CYCLIST.KILLED',
                                  'NUMBER.OF.MOTORIST.KILLED')])) %>%
  select(-Vnum, -Vtype, -value, -NUMBER.OF.PERSONS.INJURED,
         -NUMBER.OF.PEDESTRIANS.INJURED, -NUMBER.OF.CYCLIST.INJURED, -NUMBER.OF.MOTORIST.INJURED,
         -NUMBER.OF.PERSONS.KILLED, -NUMBER.OF.PEDESTRIANS.KILLED, -NUMBER.OF.CYCLIST.KILLED,
         -NUMBER.OF.MOTORIST.KILLED, -CRASH.DATE, -CRASH.TIME, -date, -time)

save.image('carcrash')

#Notes:
# 1. Think about the date and what time of day will accidents are likely to occur
# 2. map to show which locations accidents are likely to occur
# 3. What factors lead to the deadliest accidents
# 4. What kind of cars are likely to get into car accidents
# 5. Which car is at fault and which is not?

# Think about the probability of death in car accidents

# Extract day of week

# Can extract time of day


