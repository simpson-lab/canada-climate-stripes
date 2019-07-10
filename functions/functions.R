##' Read daily temperature AHCCD files into tibbles.
##'
##' @title Read daily AHCCD files
##' @param fn character; vector of file
##'
##' @return A tibble containing the read AHCCD data
##' 
##' @author Gavin L. Simpson
`read_ahccd` <- function(file, folder = "./") {
    col_names <- c('Year','Month', 1:31)
    df <- read_fwf(file.path(folder, file),
                   col_position = fwf_widths(c(5, 4, rep(8, 31)),
                                             col_names = col_names),
                   skip = 4, na = c('', 'NA', '-9999.9M'))
    df <- gather(df, 'Day', 'Value', - Year, - Month)
    df <- mutate(df,
                 Flag  = str_extract(Value, '[[:alpha:]?$]'),
                 Value = str_replace(Value, '[[:alpha:]?$]', ''),
                 Flag  = case_when(is.na(Value) ~ 'M'),
                 Value = as.numeric(Value),
                 Year  = as.integer(Year),
                 Month = as.integer(Month),
                 Day   = as.integer(Day))
    df <- add_column(df,
                     station_id = rep(str_replace(str_sub(file, start = 3L), "\\.txt$", ""),
                                      nrow(df)), .before = 1L)
    df
}

##' @title A vector of climate stripe colours
##'
##' @return A character vector of 11 colours.
##' 
##' @author Gavin L. Simpson
`stripe_palette` <- function() {
    colours <- c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7",
                 "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
    colours
}

##' Draws climate strips for processed AHCCD data sets
##' 
##' @title Draw climate stripes
##' 
##' @param data data frame; must contain variables `mean_temp` and `year`
##' @param legend character; legend placement
##' @param background character; the background colour for the climate stripe
##' @return A ggplot object.
##' @author Gavin L. Simpson & Kimberly Hinz
ggstripe <- function(data, legend = "none", background = c("white","black")) {
    background <- arg_match(background)
    col_strip <- stripe_palette()
    prov_avg <- filter(data, year >= 1901 & year <= 2000) %>%
        summarise(avg_temp = mean(mean_temp)) %>% pull(avg_temp)
    prov_sd <- filter(data, year >= 1901 & year <= 2000) %>%
        summarise(sd_temp = sd(mean_temp)) %>% pull(sd_temp)
    
    ggplot(data, aes(x = year, y = 1, fill = mean_temp)) +
        geom_tile() + theme_void() +
        theme(legend.position = legend,
              panel.background = element_rect(fill = background, colour = background, size = 0)) +
        scale_fill_gradientn(colors = rev(col_strip),
                             limits = prov_avg + (c(-1,1) * 2.6*prov_sd),
                             na.value = "grey90")
}
