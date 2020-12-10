PRESS\_analysis
================
Nicholas Baetge
12/4/2020

This document provides the analysis and figures for the PRESS
manuscript, specifically using data from experiment PN15-3.

# Prepare libraries and data

``` r
library(tidyverse)
library(readxl)
library(lmodel2)
library(blandr)
library(patchwork)
```

``` r
# data <- read_excel("~/GITHUB/PRESS_ms/Input_Data/PRESS_data.xlsx", sheet = "data") %>% 
#   filter(experiment == "PN15-3")

# write_rds(data, "~/GITHUB/PRESS_ms/Input_Data/PRESS_subset.rds")

subset <- read_rds("~/GITHUB/PRESS_ms/Input_Data/PRESS_subset.rds")

glimpse(subset)
```

    ## Rows: 48
    ## Columns: 14
    ## $ experiment  <chr> "PN15-3", "PN15-3", "PN15-3", "PN15-3", "PN15-3", "PN15-3…
    ## $ bottle      <chr> "A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "C…
    ## $ timepoint   <dbl> 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, …
    ## $ hours       <dbl> 0, 48, 27, 96, 0, 48, 27, 96, 0, 48, 27, 96, 0, 48, 27, 9…
    ## $ filter_type <chr> "none", "none", "none", "none", "none", "none", "none", "…
    ## $ method      <chr> "pour", "pour", "pour", "pour", "pour", "pour", "pour", "…
    ## $ bottle_type <chr> "custom.cap.switch", "custom.cap.switch", "custom.cap.swi…
    ## $ bottle_vol  <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, …
    ## $ toc         <dbl> 85.49484, 81.82101, 83.31384, 83.20631, 86.86581, 82.8030…
    ## $ sd_toc      <dbl> 0.80848410, 0.73245111, 0.40044040, 0.61333276, 1.3331117…
    ## $ doc         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ sd_doc      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
    ## $ ba          <dbl> 252140.89, 675581.14, 840807.04, 1032144.25, 247930.87, 7…
    ## $ sd_ba       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…

# Pour v. Pressure TOC

``` r
toc.data <- subset %>% 
  filter(filter_type == "none", method %in% c("pour", "press", "grav")) %>% 
  drop_na(toc) %>% 
  pivot_wider(id_cols = c(experiment, bottle, timepoint), names_from = method, values_from = c(toc, sd_toc))
```

    ## RMA was not requested: it will not be computed.

``` r
toc.lm.plot <- toc.data %>% 
  ggplot(aes(x = toc_pour, y = toc_press)) + 
   geom_abline(intercept = reg1$regression.results[3,2],
              slope = reg1$regression.results[3,3],colour = "black", linetype = 2, size = 1) +
   geom_abline(intercept = reg1$confidence.intervals[3,2],
              slope = reg1$confidence.intervals[3,4],colour = "grey", linetype = 3, size = 1) +
  geom_abline(intercept = reg1$confidence.intervals[3,3],
              slope = reg1$confidence.intervals[3,5],colour = "grey", linetype = 3, size = 1) +
  geom_errorbarh(aes(xmin = toc_pour - sd_toc_pour, xmax = toc_pour + sd_toc_pour), height = 0.5) +
  geom_errorbar(aes(ymin = toc_press - sd_toc_press, ymax = toc_press + sd_toc_press), width = 0.5) +
  geom_point(shape = 21, color = "black", fill = "white", size = 2, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = expression("TOC (Poured), µmol C L"^-1), y = expression("TOC (PRESS), µmol C L"^-1)) +
  xlim(80, 90) +
  ylim(80, 90) +
  annotate( geom = "text", label = expression(atop("y = 1.33x - 28.9", paste("r"^2," = 0.09, ", italic("p "), "= 0.24"))), x = 82.5, y = 89, size = 4) +
  theme_bw()
```

``` r
toc.blandr <- blandr.statistics(toc.data$toc_pour, toc.data$toc_press, sig.level = 0.95)

toc.blandr$bias
```

    ## [1] 1.114464

``` r
toc.lm.plot + toc.blandr.plot
```

![](PRESS_analysis_files/figure-gfm/combine%20TOC%20comparison%20plots-1.png)<!-- -->

\#Plot TOC and DOC curves

``` r
curve.data <- subset %>%
  select(timepoint:method, toc, doc) %>% 
  group_by(timepoint, filter_type, method) %>%
  mutate(ave_toc = mean(toc, na.rm = T),
         sd_toc = sd(toc, na.rm = T),
         ave_doc = mean(doc, na.rm = T),
         sd_doc = sd(doc, na.rm = T)) %>% 
  ungroup() %>% 
  select(-c(toc,doc)) %>% 
  distinct() %>% 
  mutate(filter_type = ifelse(filter_type == "none", "TOC", "DOC"),
         method = ifelse(method == "pour", "Pour", "PRESS"))

curve.ave.pivot <- curve.data %>% 
  select(-contains("sd")) %>% 
  pivot_longer(c(ave_toc, ave_doc), names_to = "oc", values_to = "ave", names_prefix = "ave_")

curve.sd.pivot <- curve.data %>% 
  select(-contains("ave")) %>% 
  pivot_longer(c(sd_toc, sd_doc), names_to = "oc", values_to = "sd", names_prefix = "sd_")

curve.pivot <- left_join(curve.ave.pivot, curve.sd.pivot) %>% 
  drop_na(sd) %>% 
  mutate(days = hours/24)
```

    ## Joining, by = c("timepoint", "hours", "filter_type", "method", "oc")

``` r
curve.pivot %>% 
  ggplot(aes(x = days, y = ave, group = interaction(filter_type, method))) +
  labs(x = "Days", y = expression("Organic Carbon, µmol C L"^-1), fill = "Method", linetype = "Sample Type") +
  geom_errorbar(aes(ymin = ave - sd, ymax = ave + sd, color = method), width = 0.1, alpha = 0.8) +
  geom_line(aes(color = method, linetype = filter_type), size = 0.75) + 
  geom_point(aes(fill = method), size = 2, shape = 21) +
  scale_color_manual(values = c("Pour" = "#4DAF4A", "PRESS" = "#377EB8")) +
  scale_fill_manual(values = c("Pour" = "#4DAF4A", "PRESS" = "#377EB8")) +
  theme_bw() +
  guides(color = F)
```

![](PRESS_analysis_files/figure-gfm/plot%20curve-1.png)<!-- -->
