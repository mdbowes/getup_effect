# 'Measuring the GetUp effect in the 2019 election' Analysis Script
# Matt Bowes, 28/01/2020

library(tidyverse)
library(eechidna)
library(microsynth)
library(ggthemes)
library(scales)
library(formattable)
library(here)

colours <- c("#21618c", "#2e86c1", "#5dade2", "#aed6f1")

data("tpp19")
data('tpp16')
data('tpp13')
data('tpp10')
data('tpp07')
data('tpp04')

## Data setup ##

# First need to select only those seats that exist across all years (QLD added one seat in 2010)

tpp19 <- tpp19 %>%
  mutate(year = 2019) %>%
  select(-DivisionID)

tpp16 <- tpp16 %>%
  mutate(year = 2016)

tpp13 <- tpp13 %>%
  mutate(year = 2013)

tpp10 <- tpp10 %>%
  mutate(year = 2010)

tpp07 <- tpp07 %>%
  mutate(year = 2007)

tpp04 <- tpp04 %>%
  mutate(year = 2004)

tpp <- bind_rows(tpp04, tpp07, tpp10, tpp13, tpp16, tpp19)
rm(tpp04, tpp07, tpp10, tpp13, tpp16, tpp19)

tpp_unique <- tpp %>%
  group_by(UniqueID) %>%
  summarise(n_unique = n())

tpp <- left_join(tpp, tpp_unique, by = "UniqueID")

tpp <- tpp %>%
  filter(n_unique == 6)

unique(tpp$DivisionNm)

# Now filter just the QLD electorates 

tpp <- tpp %>%
  filter(StateAb == "QLD")

# Now to add a special redistribution adjustment variable, as queensland had redistributions in 2018 and 2009
# This variable comes from Antony Green pendulum estimates from 2010 (https://www.abc.net.au/elections/federal/2010/guide/stateindex.htm#QLD)
# And Antony Green estimates from 2019 (https://www.abc.net.au/news/elections/federal-redistribution-2018/qld/)
# Looking at the swing net estimated redistribution should help account for the adjustment in vote due to the redistribution

tpp <- tpp %>%
  mutate(redist_lab = if_else(year == 2019, case_when(
    DivisionNm == "BLAIR" ~ -0.8,
    DivisionNm == "BONNER" ~ 0,
    DivisionNm == "BOWMAN" ~ 0,
    DivisionNm == "BRISBANE" ~ -0.1,
    DivisionNm == "CAPRICORNIA" ~ 0,
    DivisionNm == "DAWSON" ~ -0.1,
    DivisionNm == "DICKSON" ~ -0.1,
    DivisionNm == "FADDEN" ~ -0.1,
    DivisionNm == "FAIRFAX" ~ 0,
    DivisionNm == "FISHER" ~ -0.1,
    DivisionNm == "FLYNN" ~ 0,
    DivisionNm == "FORDE" ~ 0,
    DivisionNm == "GRIFFITH" ~ -0.2,
    DivisionNm == "GROOM" ~ 0,
    DivisionNm == "HERBERT" ~ 0,
    DivisionNm == "HINKLER" ~ 0,
    DivisionNm == "KENNEDY" ~ 0.2,
    DivisionNm == "LEICHHARDT" ~ 0,
    DivisionNm == "LILLEY" ~ 0.4,
    DivisionNm == "LONGMAN" ~ 0,
    DivisionNm == "MARANOA" ~ 0,
    DivisionNm == "MCPHERSON" ~ 0,
    DivisionNm == "MONCRIEFF" ~ -0.3,
    DivisionNm == "MORETON" ~ 0,
    DivisionNm == "OXLEY" ~ -0.1,
    DivisionNm == "PETRIE" ~ 0,
    DivisionNm == "RANKIN" ~ 0,
    DivisionNm == "RYAN" ~ 0.1,
    DivisionNm == "WIDE BAY" ~ -0.1,
    DivisionNm == "WRIGHT" ~ 0), 
    0)) %>%
  mutate(redist_lab = if_else(year == 2010, case_when(
    DivisionNm == "BLAIR" ~ 7-4.5,
    DivisionNm == "BONNER" ~ 0,
    DivisionNm == "BOWMAN" ~ 0,
    DivisionNm == "BRISBANE" ~ 4.5 - 6.8,
    DivisionNm == "CAPRICORNIA" ~ 12.1-12.7,
    DivisionNm == "DAWSON" ~ 2.6-3.2,
    DivisionNm == "DICKSON" ~ 0.9 ,
    DivisionNm == "FADDEN" ~ -0.2,
    DivisionNm == "FAIRFAX" ~ 0,
    DivisionNm == "FISHER" ~ -0.4,
    DivisionNm == "FLYNN" ~ 2.2-0.16,
    DivisionNm == "FORDE" ~ 3.4-2.9,
    DivisionNm == "GRIFFITH" ~ 0,
    DivisionNm == "GROOM" ~ 0,
    DivisionNm == "HERBERT" ~ 0.2,
    DivisionNm == "HINKLER" ~ 0.2,
    DivisionNm == "KENNEDY" ~ 0.2,
    DivisionNm == "LEICHHARDT" ~ 0.1,
    DivisionNm == "LILLEY" ~ -0.6,
    DivisionNm == "LONGMAN" ~ 1.9-3.6,
    DivisionNm == "MARANOA" ~ 0.3,
    DivisionNm == "MCPHERSON" ~ 0.1,
    DivisionNm == "MONCRIEFF" ~ 0.2,
    DivisionNm == "MORETON" ~ 1.25,
    DivisionNm == "OXLEY" ~ 11.3-14.1,
    DivisionNm == "PETRIE" ~ 2.1,
    DivisionNm == "RANKIN" ~ 0,
    DivisionNm == "RYAN" ~ 2.6,
    DivisionNm == "WIDE BAY" ~ 0,
    DivisionNm == "WRIGHT" ~ 0), 
    redist_lab)) %>%
  mutate(redist_lnp = -redist_lab)

# Now we need to pool all the census data we have for control variables 
# For controls I'll use NoReligion, Renting, MedianAge, MedianHouseholdIncome and BachelorAbv

data("abs2010", "abs2013", "abs2016", "abs2019", "abs2007", "abs2004")

abs2004 <- abs2004 %>%
  select(NoReligion, UniqueID, Renting, MedianAge, MedianHouseholdIncome, BachelorAbv, 
         Extractive, LFParticipation) %>%
  mutate(year = 2004)

abs2007 <- abs2007 %>%
  select(NoReligion, UniqueID, Renting, MedianAge, MedianHouseholdIncome, BachelorAbv, 
         Extractive, LFParticipation) %>%
  mutate(year = 2007)

abs2010 <- abs2010 %>%
  select(NoReligion, UniqueID, Renting, MedianAge, MedianHouseholdIncome, BachelorAbv, 
         Extractive, LFParticipation) %>%
  mutate(year = 2010)

abs2013 <- abs2013 %>%
  select(NoReligion, UniqueID, Renting, MedianAge, MedianHouseholdIncome, BachelorAbv, 
         Extractive, LFParticipation) %>%
  mutate(year = 2013)

abs2016 <- abs2016 %>%
  select(NoReligion, UniqueID, Renting, MedianAge, MedianHouseholdIncome, BachelorAbv, 
         Extractive, LFParticipation) %>%
  mutate(year = 2016)

abs2019 <- abs2019 %>%
  select(NoReligion, UniqueID, Renting, MedianAge, MedianHouseholdIncome, BachelorAbv, 
         Extractive, LFParticipation) %>%
  mutate(year = 2019)

abs <- bind_rows(abs2004, abs2007, abs2010, abs2013, abs2016, abs2019)
rm(abs2004, abs2007, abs2010, abs2013, abs2016, abs2019)

# Now we can merge these control variables by uniqueID and year with the tpp

data <- left_join(tpp, abs, by = c("UniqueID", "year"))

# We'll then average the control variables across all years 
# (this helps create a simpler synthetic control model in cases where adding time variant controls 
# doesn't work because of our relatively small sample size)

av_data <- data %>%
  group_by(UniqueID) %>%
  summarise(norel_av = mean(NoReligion),
            renting_av = mean(Renting),
            medage_av = mean(MedianAge),
            medhhi_av = mean(MedianHouseholdIncome),
            bach_av = mean(BachelorAbv),
            extract_av = mean(Extractive),
            lfp_av =mean(LFParticipation))

data <- left_join(data, av_data, by = "UniqueID")

# Now we'll create our variable of interest: the LNP swing, adjusting for redistributions
# The 'Swing' in eechidna is defined in terms of governing party, so we'll adjust

data <- data %>%
  mutate(lnp_sw = if_else(year %in% c(2010, 2013), -Swing - redist_lnp, Swing - redist_lnp))

## Synthetic Control Model ##

micro_2 <- microsynth(as.data.frame(data), idvar = "UniqueID", timevar = "year", intvar = "treatment",
                      start.pre = 2004, end.pre = 2013, end.post = 2019,
                      match.out = c("lnp_sw"),
                      match.out.min = c("Renting", "NoReligion",
                                        "MedianAge", "MedianHouseholdIncome"),
                      # Note that I include some variables as time invariant controls because otherwise the model doesn't run, due to limited sample size
                      match.covar = c("bach_av", "extract_av"),
                      use.backup = TRUE,
                      test = "lower", 
                      # Note also that I unfortunately can't turn on jacknife simulations because of limited sample size
                      result.var = "lnp_sw", omnibus.var = "lnp_sw")

summary(micro_2)

plot_microsynth(micro_2, start.pre = 2004, end.pre = 2013, end.post = 2019)

# Plot of weights in synthetic control model

wts_2 <- micro_2$w$Weights
wts_2_names <- rownames(wts_2)
wts_2_names <- as.numeric(wts_2_names)
wts_2 <- as.data.frame(wts_2)
wts_2 <- as_tibble(wts_2)
wts_2$UniqueID <- wts_2_names
wts_2 <- wts_2 %>% rename(wts_2 = Main, )

divisions <- data %>%
  filter(year == 2019) %>%
  group_by(UniqueID) %>%
  summarise(division = unique(DivisionNm)) %>%
  mutate(division = str_to_title(division))

wts_2 <- left_join(wts_2, divisions, by = "UniqueID")

wts_2_plot <- wts_2 %>%
  filter(division != "Dickson" &
           wts_2 > 0.001) %>%
  ggplot(aes(y = wts_2, x = reorder(division, wts_2))) +
  geom_col() +
  coord_flip() +
  labs(y = "Comparative Weights",
       subtitle = "Synthetic Dickson Composition",
       x = "") +
  theme(plot.title.position = "plot",
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_hc()

wts_2_plot

# Comparison of Demographic Indicators in Synthetic vs Real Dickson

con_comp_2 <- data %>%
  filter(DivisionNm != "DICKSON" & 
           year == 2019) %>%
  select(Renting, MedianHouseholdIncome, 
         MedianAge, BachelorAbv, Extractive, LFParticipation, lnp_sw, NoReligion, wts_2) %>%
  summarise_all(list(~weighted.mean(x = ., w = wts_2))) %>%
  select(-wts_2) %>%
  pivot_longer(cols = Renting:NoReligion, 
               names_to = "stat",
               values_to = "value") %>%
  mutate(stat = case_when(stat == "LFParticipation" ~ "Percent in Labour Force",
                          stat == "Renting" ~ "Percent Renting",
                          stat == "MedianHouseholdIncome" ~ "Median Household Income ($)",
                          stat == "MedianAge" ~ "Median Age (Years)",
                          stat == "BachelorAbv" ~ "Percent with Bachelors Degree",
                          stat == "Extractive" ~ "Percent Working in Extractive Industries",
                          stat == "lnp_sw" ~ "Swing to LNP in 2019 Election",
                          stat == "NoReligion" ~ "Percent Non-Religious")) %>%
  rename(`Indicator (2016)` = stat,
         `Synthetic Dickson` = value)

synth_demo <- left_join(dickson_state_demo, con_comp_2, by = c("Indicator (2016)")) %>%
  select(-`Median of Rest of QLD`) %>%
  mutate_if(is.numeric, ~round(., digits = 1))

custom_color_tile_2 <- function (...) 
{
  formatter("span",
            style = function(x) style(display = "block", 
                                      padding = "0px 0px", 
                                      `color` = "white", 
                                      `border-radius` = "4px", 
                                      `background-color` = csscolor(...)))
}

synth_tab <- formattable(synth_demo,
                         align = c("l", "c", "c"),
                         list(`Indicator (2016)` = formatter( "span", style = ~ style(color = "grey", 
                                                                                      font.weight = "bold")),
                              area(col = 3) ~ custom_color_tile_2("#B1CBEB"),
                              `Dickson` = formatter("span", style = ~ style(display = "block", 
                                                                            padding = "0px 0px", 
                                                                            `color` = "white", 
                                                                            `border-radius` = "4px", 
                                                                            `background-color` = csscolor("#3E7DCC"))),
                              `Synthetic Dickson` = formatter("span", style = ~ style(display = "block", 
                                                                                      padding = "0px 0px", 
                                                                                      `color` = "white", 
                                                                                      `border-radius` = "4px",
                                                                                      `align-content` = "center",
                                                                                      `background-color` = csscolor("#B1CBEB")))))

synth_tab

# Comparison of LNP Swing in Synthetic vs Real Dickson

synth_dickson <- as.numeric(micro_2$Plot.Stats$Control)
dickson <- as.numeric(micro_2$Plot.Stats$Treatment)
year <- c(2004, 2007, 2010, 2013, 2016, 2019)

synth <- tibble(synth_dickson, dickson, year) %>%
  pivot_longer(cols = synth_dickson:dickson, names_to = "group", values_to = "swing") %>%
  mutate(group = if_else(group == "dickson", "Dickson", "Synthetic Dickson")) %>%
  mutate(group = factor(group, levels = c("Synthetic Dickson", "Dickson")))

synth_plot <- synth %>%
  ggplot(aes(x = year, y = swing, colour = group)) +
  geom_line(size = 1.5) +
  labs(subtitle = "Two Party Preferred Swing to LNP in Dickson vs Synthetic Dickson",
       colour = "") +
  theme_hc() +
  scale_x_continuous(breaks = c(2004, 2007, 2010, 2013, 2016, 2019)) +
  scale_y_continuous(labels = unit_format(unit = "%", sep = ""),
                     breaks = c(-9, -6, -3, 0, 3, 6)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        legend.position = "right") +
  scale_colour_manual(values = c(colours[4], colours[1]))

synth_plot
