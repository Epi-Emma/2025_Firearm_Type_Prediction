
#################################################
## Firearm Suicide NVDRS Investigate Geography ##
#################################################

#Written by: Emma Gause
#Date: 11/10/21


#Load in libraries
library("tidyverse")
library("dplyr")
library("ggthemes")

#Create path to directory
datadir <- "[insert path to directory]"

#Read in data (missingness included)
dat <- readRDS(paste0(datadir, "NVDRS_Firearm_Suicide_2005_2018.rds"))

##------------------------------------------------------------------------##

#Let's start looking at geography
table(is.na(dat$InjuryFIPS))
#281 records are missing an injury FIPS

table(dat$IncidentYear, dat$SiteID)

#Plot state graphs with all states (non-reporting years will simply not be depicted)

table(dat$InjuryState, useNA = "ifany")
#125 records missing state of injury

#assign reporting state when injury state is missing
dat <- dat %>% mutate(InjuryState_imp = if_else(is.na(dat$InjuryState), dat$SiteID, dat$InjuryState))
table(dat$InjuryState_imp, useNA = "ifany")

#look at state/year patterns
table(dat$InjuryState_imp, useNA = "ifany")
table(dat$IncidentYear, dat$InjuryState_imp, useNA = "ifany")

#Summarize by Injury State
state <- dat %>% group_by(InjuryState_imp, longgun, IncidentYear) %>% 
  summarise(deaths = n())
state <- ungroup(state)

#convert to wide
state2 <- spread(state, key = "longgun", value = "deaths") %>% 
  select("state" = "InjuryState_imp", "year" = "IncidentYear",
         "handgun" = "0", "longgun"="1")
state2 <- ungroup(state2)

#Puerto rico has no long gun deaths for reported years 
#Need to convert the NA to true zeros
state2$longgun[state2$state=="Puerto Rico"] <- 0

#DC has no long gun deaths for reporting years
#Need to convert the NA to true zeros
state2$longgun[state2$state=="District of Columbia"&(state2$year>2016)] <- 0

#set DC non-reporting years to NA
state2$handgun[state2$state=="District of Columbia"&(state2$year<2016)] <- NA
state2$longgun[state2$state=="District of Columbia"&(state2$year<2016)] <- NA

#We need to remove deaths for non-reporting state/years (i.e. occurred in a state that wasn't reporting state)
#This is fewer than 10, EXCEPT for Rhode Island, Puerto Rico, and DC  
  #which report but have few deaths
state2$handgun[state2$handgun<10&state2$state!="District of Columbia"&
                    state2$state!="Puerto Rico"&
                    state2$state!="Rhode Island"] <- NA
state2$longgun[state2$longgun<10&state2$state!="District of Columbia"&
                                    state2$state!="Puerto Rico"&
                                    state2$state!="Rhode Island"] <- NA

#flag rows with all NAs and remove
statex <- state2 %>% mutate(flag = if_else(is.na(handgun)&is.na(longgun), 1, 0)) %>%
    filter(flag==0) %>% select(-flag)

#create proportion
statex$l_to_h <- statex$longgun/statex$handgun
statex$total_deaths <- statex$longgun+statex$handgun

#Graph state-specific props over time
xbrks = seq(2005, 2018, 1)
s <- statex %>% ggplot(aes(x=year, y=l_to_h, group=state, color=state)) +
  geom_line() + geom_point(aes(size=total_deaths)) + 
  scale_y_continuous(name="Proportion of Long Guns to Handguns",
                     breaks=c(0, 0.25, 0.5, 0.75, 1.0), limits=c(0,1)) +
  scale_x_continuous(name="Death Year",
                     breaks=xbrks, limits=c(2005, 2018)) +
  scale_size(breaks = c(50, 300, 800)) + 
  theme_calc()
s + facet_wrap("state")+guides(color="none") + theme(axis.text.x = element_text(angle = 90))


##------------------------------------------------------------------------##
