library(googlesheets4)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ggeffects)

#early-growth measurement data upload
growth_nov17 <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=1444597239", sheet = "leaf_counts_nov17")
growth_dec1 <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=1444597239", sheet = "leaf_counts_dec1")
growth_dec15 <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=2127344940", sheet = "leaf_counts_dec15")


#combining trt into 1 column
growth_nov17$trt <- with(growth_nov17, ifelse(trt_temp == "cool" & trt_length == "short", "short_cool",
                                              ifelse(trt_temp == "cool" & trt_length == "long", "long_cool",
                                                     ifelse(trt_temp == "warm" & trt_length == "short", "short_warm", "long_warm"))))

growth_dec1$trt <- with(growth_dec1, ifelse(trt_temp == "cool" & trt_length == "short", "short_cool",
                                            ifelse(trt_temp == "cool" & trt_length == "long", "long_cool",
                                                   ifelse(trt_temp == "warm" & trt_length == "short", "short_warm", "long_warm"))))

growth_dec15$trt <- with(growth_dec15, ifelse(trt_temp == "cool" & trt_length == "short", "short_cool",
                                              ifelse(trt_temp == "cool" & trt_length == "long", "long_cool",
                                                     ifelse(trt_temp == "warm" & trt_length == "short", "short_warm", "long_warm"))))


#early-growth measurements & stats
#sum prim & sec leaf counts

# Checking where NAs are in dataframes
table(growth_nov17$sec_lf_nov17, growth_nov17$prim_lf_nov17, useNA = "ifany")
table(growth_dec1$sec_lf_dec1, growth_dec1$prim_lf_dec1, useNA = "ifany") 
# Dec 1 has more NAs than expected in the secondary leaves column
table(growth_dec15$sec_lf_dec15, growth_dec15$prim_lf_dec15, useNA = "ifany")

summary(growth_nov17)
growth_nov17$total_lf <- growth_nov17$prim_lf_nov17 + na.rm(growth_nov17$sec_lf_nov17)
growth_dec1$total_lf <- growth_dec1$prim_lf_dec1 + growth_dec1$sec_lf_dec1
growth_dec15$total_lf <- growth_dec15$prim_lf_dec15 + growth_dec15$sec_lf_dec15

ggplot(data=growth_nov17, aes(x=trt,y=prim_lf_nov17))+
  geom_boxplot(alpha=0.3, colour = "khaki4")+
  geom_jitter(colour="darkslategrey", width = 0.1, height = 0.1, alpha = 0.3)

ggplot(data=growth_dec1, aes(x=trt,y=prim_lf_dec1))+
  geom_boxplot(alpha=0.3, colour = "khaki4")+
  # geom_point(colour="darkslategrey")+
  geom_jitter(colour="darkslategrey", width = 0.1, height = 0.1, alpha = 0.3) #+
  # geom_point(data=growth_dec1, aes(x=trt, y=sec_lf_dec1), colour="pink")

ggplot(data=growth_dec1, aes(x=trt,y=total_lf))+
  geom_boxplot(alpha=0.3, colour = "khaki4")+
  geom_jitter(colour="darkslategrey", width = 0.1, height = 0.1, alpha = 0.3) 
#get v diff plot when using TOTAL_LF (sum of prim & sec)

ggplot(data=growth_dec15, aes(x=trt,y=total_lf))+
  geom_boxplot(alpha=0.3, colour = "khaki4")+
  geom_jitter(colour="darkslategrey", width = 0.1, height = 0.1, alpha = 0.3) +
  geom_point(data=growth_dec15, aes(x=trt, y=sec_lf_dec15), colour="pink")+
  scale_colour_manual(values = c("darkslategrey" = "Primary Leaves", "pink" = "Secondary Leaves"))+
  # MB: the reason the legend isn't appearing is because the leaf values aren't in the same columns. It's kind of a technical thing but the legend only generates when you have a column with multiple factor levels in it. I don't think that displaying the primary and secondary leaves on top of each other like this is the best way to visualize them. Just doing total leaves is probably good, or doing prim and sec on separate plots.
  labs(colour = "Leaf Type")
ggsave("dec15 growth.pdf", width=15, height=15)












#flwr survey & post-flwr measurement data upload
flwr_survey <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=2127344940", sheet = "flower_survey")
petal <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=2127344940", sheet = "petal_imageJ")
leaf <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=2127344940", sheet = "leaf_imageJ")
biomass <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=2127344940", sheet = "soil dry processing") %>%
  mutate(across(
    where(is.list),
    # list conversion to char leaves "NULL" entries > TRICKY: NULLs convert to NA
    ~ as.character(.x) %>% 
      na_if("NULL") %>% 
      as.numeric))
germ_date_key <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=2127344940", sheet = "pick_colour_key")
pick_survey <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=2127344940", sheet = "pick_colour_survey")


all_data <- left_join(flwr_survey, leaf) %>%
  left_join(biomass) %>%
  left_join(petal) %>%
  left_join(pick_survey) %>%
  left_join(germ_date_key) %>%
  mutate(total_biomass = above_veg + below + rep) %>%
  mutate(total_above = above_veg + rep)
  


all_data$flwr_date_rel = difftime(all_data$flwr_date, as.Date(all_data$Germination_date))

#how to tell diff bw germ date & flwr date for each indiv
#flwr_survey$flwr_date_rel_germ = difftime(flwr_survey$flwr_date, flwr_survey$germ_date)
#flwr_survey$flwr_date_rel

#combining trt into 1 variable
all_data$trt <- with(all_data, ifelse(trt_temp == "cool" & trt_length == "short", "short_cool",
                                ifelse(trt_temp == "cool" & trt_length == "long", "long_cool",
                                       ifelse(trt_temp == "warm" & trt_length == "short", "short_warm", "long_warm"))))



#checking data upload
c1 <- all_data %>%
  filter(!is.na(flwr_date)&is.na(area_lf))


summary(all_data)

#histograms
ggplot(data = all_data, aes(x=trt, y=flwr_date_rel)) +
  geom_boxplot()
#cool trt flowered latest, long trt flwr later than short

ggplot(data = all_data, aes(x=trt, y=height_to_fr_flwr)) +
  geom_boxplot()
#height is almost the same for all trt groups

ggplot(data = all_data, aes(x=trt, y=nodes_to_fr_flwr)) +
  geom_boxplot()
#short warm have more nodes??

ggplot(data = all_data, aes(x=trt, y=area_lf)) +
  geom_boxplot()

ggplot(data = all_data, aes(x=trt, y=flwr_date_rel, fill=trt)) +
  geom_boxplot()+
  facet_wrap(.~pop)
ggsave("flwr date by trt.pdf", width=5, height=5)

ggplot(data = all_data, aes(x=trt, y=below, fill=trt)) +
  geom_boxplot()+
  facet_wrap(.~pop, scales = "free_y")
#ggsave("flwr date by trt.pdf", width=5, height=5)

ggplot(data=all_data, aes(x=flwr_date_rel, y=below, fill=trt, colour=trt))+
  geom_point()+
  geom_smooth(method = "lm")
  #facet_wrap(.~pop, scales = "free_y")

ggplot(data=all_data, aes(x=trt, y=below))+
  geom_boxplot()

gggeom_boxplot()ggplot(data = all_data, aes(x=pop, y=area_pt, fill=trt)) +
  geom_boxplot()

#continous plot

ggplot(data = all_data, aes(x=flwr_date_rel, y=height_to_fr_flwr, colour=trt))+
  geom_point()

all_data$flwr_date_num <- as.numeric(all_data$flwr_date_rel)




#models

#m1 <- lmer(data = all_data, flwr_date_num ~ trt +(1|pop))
#summary(m1)

m2 <- lmer(data = all_data, flwr_date_num ~ trt_length*trt_temp +(1|pop))
summary(m2)

plot(ggpredict(m2, terms = c("trt_length", "trt_temp")))

m4 <- lmer(data = all_data, above_veg ~ flwr_date_rel +(1|pop))
summary(m4)

plot(ggpredict(m4))
#shows model confidence in the relationship 

#m3 <- lm(data = all_data, flwr_date_num ~ trt_length*trt_temp*pop)
#summary(m3)
#no sig 3 way interaction - not diff depending on pop

tools::package_dependencies("Matrix", which = "LinkingTo", reverse = TRUE)[[1L]]
install.packages("lme4", type = "source")



