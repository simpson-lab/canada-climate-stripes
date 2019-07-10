library('cowplot')
library('readr')
library('tibble')
library('rlang')
library('purrr')
library('tidyr')
library('dplyr')
library('stringr')
library('readxl')
library('here')
library('ggplot2')

## load our functions
source(here("functions", "functions.R"))

station_list <- read_xls(here("data", "Temperature_Stations.xls"), skip = 3)
station_list <- set_names(station_list,
                          c("province", "station_name", "station_id", "start_year", "start_month",
                            "end_year", "end_month", "latitude", "longitude", "elevation",
                            "joined"))

fldr <- here("data", "temperature")
fns <- list.files(fldr)

ahccd_temp <- map_dfr(fns, read_ahccd, folder = fldr)

ahccd_temp <- set_names(ahccd_temp, str_to_lower(names2(ahccd_temp)))

avg_temp <- ahccd_temp %>% group_by(station_id, year) %>%
    summarise(mean_temp = mean(value, na.rm = TRUE),
              n_recs    = sum(!is.na(value))) %>%
    mutate(mean_temp = case_when(n_recs < 335 ~ NA_real_,
                                 n_recs >= 335 ~ mean_temp)) %>%
    ungroup()

avg_temp <- left_join(avg_temp, station_list, by = "station_id")

prov_avg_temp <- avg_temp %>%
    group_by(province, year) %>%
    summarise(mean_temp = mean(mean_temp, na.rm = TRUE),
              n_stns = length(unique(station_id))) %>%
    mutate(mean_temp = case_when(province == "PEI" ~ mean_temp,
                                 province != "PEI" & n_stns < 5 ~ NA_real_,
                                 province != "PEI" & n_stns >= 5 ~ mean_temp)) %>%
    ungroup()

## plot number of stations
ggplot(prov_avg_temp, aes(x = year, y = n_stns)) +
    geom_line() + facet_wrap(~ province, scales = "free_y")

## look at implied trends resulting from averaging
ggplot(prov_avg_temp, aes(x = year, y = mean_temp)) + geom_line() + facet_wrap(~ province, scales = "free_y")

## climate stripes for each province and territory
## The daily AHCCD data uses non-standard abbreviations for provinces/territories
(ab_stripe <- ggstripe(filter(prov_avg_temp, province == "ALTA" & year >= 1901)))
(bc_stripe <- ggstripe(filter(prov_avg_temp, province == "BC"   & year >= 1901)))
(mb_stripe <- ggstripe(filter(prov_avg_temp, province == "MAN"  & year >= 1901)))
(nb_stripe <- ggstripe(filter(prov_avg_temp, province == "NB"   & year >= 1901)))
(nl_stripe <- ggstripe(filter(prov_avg_temp, province == "NFLD" & year >= 1901)))
(ns_stripe <- ggstripe(filter(prov_avg_temp, province == "NS"   & year >= 1901)))
(nu_stripe <- ggstripe(filter(prov_avg_temp, province == "NU"   & year >= 1901)))
(nt_stripe <- ggstripe(filter(prov_avg_temp, province == "NWT"  & year >= 1901)))
(on_stripe <- ggstripe(filter(prov_avg_temp, province == "ONT"  & year >= 1901)))
(pe_stripe <- ggstripe(filter(prov_avg_temp, province == "PEI"  & year >= 1901)))
(qu_stripe <- ggstripe(filter(prov_avg_temp, province == "QUE"  & year >= 1901)))
(sk_stripe <- ggstripe(filter(prov_avg_temp, province == "SASK" & year >= 1901)))
(yt_stripe <- ggstripe(filter(prov_avg_temp, province == "YT"   & year >= 1901)))

## arrange the plots in a list with province/territory names
plt_lst <- list(yt_stripe, nt_stripe, nu_stripe,
                 bc_stripe, ab_stripe, sk_stripe, mb_stripe,
                 on_stripe, qu_stripe, nb_stripe, pe_stripe, ns_stripe, nl_stripe)
plt_lst <- set_names(plt_lst,
                     c("YT","NT","NU","BC","AB","SK","MB","ON","QU","NB","PE","NS","NL"))

## plot stripes; stripes are in ~W-E order, N-S
plot_grid(plotlist = plt_lst, ncol = 1, align = "h", axis = "lr")

ggsave(here("plots", "climate-stripes-geographical.pdf"),
       width = 27, height = 40)

ggsave(here("plots", "climate-stripes-geographical.svg"),
       width = 27, height = 40)

## as above but with province / territory labels
plot_grid(plotlist = plt_lst, ncol = 1, align = "h", axis = "lr",
          labels = str_to_upper(names(plt_lst)))

ggsave(here("plots", "climate-stripes-geographical-with-labels.pdf"),
       width = 27, height = 40)

ggsave(here("plots", "climate-stripes-geographical-with-labels.svg"),
       width = 27, height = 40)

## stripes but in alphabetical order by province/territory abbreviation
ord <- order(names(plt_lst))
plot_grid(plotlist = plt_lst[ord], ncol = 1, align = "h", axis = "lr",
          labels = str_to_upper(names(plt_lst)[ord]))

ggsave(here("plots", "climate-stripes-alphabetical-with-labels.pdf"),
       width = 27, height = 40)

ggsave(here("plots", "climate-stripes-alphabetical-with-labels.svg"),
       width = 27, height = 40)

