#' =============================================================================
#' Project: GES Health Study (ENVIRONS)
#' Date Created: July 26, 2024
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description: helper functions for the Interactive Maps
#' =============================================================================

library(here)
library(readxl)
library(writexl)
library(sf)
library(tidycensus)
library(tigris)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggspatial)
library(ggthemes)
library(scales)
library(colorspace)
library(ggnewscale)
library(patchwork)
library(ggiraph)
library(tidygeocoder)

#' Load data (see code/03_final_data_sets.R)
load("ges_health_study_app/ges_app_data.rdata")
nbhd_data_no_geo <- st_set_geometry(nbhd_data_long, NULL)

neighborhood_names <- unique(acs_neighborhoods$NBHD_NAME)

#' Text for the app (English and Spanish in one document,
#' selection of language occurs in the code below)
text_dictionary <- readxl::read_xlsx("ges_health_study_app/text_dictionary.xlsx")

#' ===============================================
#' PUBLISHED DATE
#' When you make changes to the app, update the pub_date below

pub_date <- "2025-01-07"
#' ===============================================

#' ===============================================
#' LANGUAGE SWITCH: TRUE OR FALSE

spanish <- F

#' THIS SECTION CHANGES DEPENDING ON IF THE APP IS
#' IN ENGLISH OR SPANISH!!
#' These include:
#'      Data dictionary
#'      Text strings
#'      Unit definitions
#'      HTML Map source file names

if(spanish == F) {
  #' FOR ENGLIGH
  dictionary <- readxl::read_xlsx("ges_health_study_app/dictionary_english.xlsx") %>%
    as.data.frame() %>%
    mutate(note = ifelse(is.na(note), "", note))
  
  text <- text_dictionary$english
  names(text) <- text_dictionary$text_id
  
  units <- c("percent" = "%",
             "per_1000" = " per 1,000",
             "per_10000" = " per 10,000",
             "per_100000" = " per 100,000",
             "dollars" = "$",
             "ug_m3" = " \U00B5g/m\U00B3",
             "ppb" = " ppb",
             "ppm" = " ppm",
             "none" = "",
             "deg_F" = "\U2109",
             "dBA" = " dBA Leq",
             "years" = " years")
  
  comm_map_source <- "interactive_community_locations_map_02.15.23.html"
  health_map_source <- "interactive_nbn_health_map_02.15.23.html"
  nbn_map_source <- "interactive_nbn_demographics_map_02.15.23.html"
  tree_map_source <- "interactive_tree_equity_map_v2_02.15.23.html"
  
  geo_name <- "Neighborhoods"
  den_name <- "Denver Neighborhoods"
  ges_name <- "GES Neighborhoods"
  hist_y_lab <- "Number of neighborhoods"
  nbhd_missing <- "Neighborhood not available"
} else {
  #' FOR SPANISH
  dictionary <- readxl::read_xlsx("ges_health_study_app/dictionary_spanish.xlsx") %>%
    as.data.frame() %>%
    mutate(note = ifelse(is.na(note), "", note))
  
  text <- text_dictionary$spanish
  names(text) <- text_dictionary$text_id
  
  units <- c("percent" = "%",
             "per_1000" = " por 1.000",
             "per_10000" = " por 10.000",
             "per_100000" = " por 100.000",
             "dollars" = "$",
             "ug_m3" = " \U00B5g/m\U00B3",
             "ppb" = " ppb",
             "ppm" = " ppm",
             "none" = "",
             "deg_F" = "\U2109",
             "dBA" = " dBA Leq",
             "years" = " años")
  
  comm_map_source <-"interactive_community_locations_map_es_02.27.23.html"
  health_map_source <- "interactive_nbn_health_map_es_02.27.23.html"
  nbn_map_source <- "interactive_nbn_demographics_map_es_02.27.23.html"
  tree_map_source <- "interactive_tree_equity_map_es_v2_02.27.23.html"
  
  geo_name <- "Barrios"
  den_name <- "Barrios de Denver"
  ges_name <- "Barrios de GES"
  hist_y_lab <- "Número de barrios"
  nbhd_missing <- "Barrio no disponible"
}

#' END OF THE LANUGUAGE SPECIFIC SECTION OF CODE
#' ===============================================

#'Variable lists
#' population variables
pop_vars <- dictionary[which(dictionary$type == "pop"), "variable"]
pop_var_dict <- filter(dictionary, variable %in% pop_vars) %>%
  arrange(factor(variable, levels = pop_vars))
pop_var_list <- pop_var_dict$variable
names(pop_var_list) <- pop_var_dict$short_var_name

#' environmental variables
env_vars <- dictionary[which(dictionary$type == "env"), "variable"]
env_var_dict <- filter(dictionary, variable %in% env_vars) %>%
  arrange(factor(variable, levels = env_vars))
env_var_list <- env_var_dict$variable
names(env_var_list) <- env_var_dict$short_var_name

#' health variables
health_vars <- dictionary[which(dictionary$type == "health"), "variable"]
health_var_dict <- filter(dictionary, variable %in% health_vars) %>%
  arrange(factor(variable, levels = health_vars))
health_var_list <- health_var_dict$variable
names(health_var_list) <- health_var_dict$short_var_name

#' Predefined colors
den_col <- "#377eb8"
ges_col <- "#e41a1c"
oth_col <- "#4daf4a"

# color_ramp <- "Greens"

#' Theme for the maps
map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(size = 12, color = 'black'),
  #panel.spacing.y = unit(0,"cm"),
  #panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border = element_blank(),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  # plot.margin = grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank(),
  #legend.background = element_rect(fill='transparent'),
  plot.margin = unit(c(0, 0, 0, 0), "cm"),
  panel.spacing = unit(c(0, 0, 0, 0), "cm"),
  legend.position = "inside",
  legend.position.inside = c(0.86, 0.35),
  legend.title = element_text(size = 12),
  legend.background = element_rect(fill = "white"),
  legend.spacing.y = unit(0.2, 'cm'),
  legend.margin = margin(0.1,0,0,0, unit="cm")
)

#' Data set filter function
data_to_save <- function(dataset, var1, var2) {
  df <- st_set_geometry(filter(dataset, var %in% c(var1, var2)), NULL)
  df_source <- dictionary[which(dictionary$variable %in% c(var1, var2)),
                          c("variable", "short_var_name",
                            "full_var_name", "source", "note")]
  df_save <- left_join(df, df_source, by = c("var" = "variable"))
  return(df_save)
}

#' Map function
map_variable <- function(geo = "Neighborhoods", map_var, show_boundaries = T,
                         show_highways = T, color_ramp) {
  plot_title <- dictionary[which(dictionary$variable == map_var), "short_var_name"]
  plot_subtitle <- dictionary[which(dictionary$variable == map_var), "full_var_name"]
  legend_title <- dictionary[which(dictionary$variable == map_var), "legend_title"]
  show_legend <- dictionary[which(dictionary$variable == map_var), "show_legend"]
  label_function <- dictionary[which(dictionary$variable == map_var), "label_fn"]
  source <- dictionary[which(dictionary$variable == map_var), "source"]
  footnote <- dictionary[which(dictionary$variable == map_var), "note"]
  alt_text <- dictionary[which(dictionary$variable == map_var), "alt_text"]
  unit <- dictionary[which(dictionary$variable == map_var), "units"]

  map <- ggplot() +
    #' Layers if geography is neighborhoods:
    {if(geo == "Neighborhoods")
      geom_sf(data = nbhd_data,
              aes(fill = .data[[map_var]]), 
                  show.legend = show_legend,
              color = "black", inherit.aes = F)} +
    {if(show_boundaries & geo == "Neighborhoods")
      geom_sf(data = neighborhoods, aes(color = "denver"),
              show.legend = "polygon", fill = NA,
              inherit.aes = F, linewidth = 0.25)} +
    {if(show_boundaries & geo == "Neighborhoods")
      geom_sf(data = ges, aes(color = "ges"), fill = NA,
              show.legend = "polygon", inherit.aes = F, linewidth = 1.5) 
      } +
    {if(show_boundaries & geo == "Neighborhoods")
      scale_color_manual(name = geo_name,
                         values = c("denver" = "black",
                                    "ges" = ges_col),
                         labels = c("denver" = den_name,
                                    "ges" = ges_name))} +

    #' Layers if geography is census tracts:
    # {if(geo == "Census tracts")
    #   geom_sf(data = acs_tracts,
    #           aes(fill = .data[[map_var]]), show.legend = show_legend,
    #           color = "black", inherit.aes = F)} +
    # {if(show_boundaries & geo == "Census tracts")
    #   geom_sf(data = tracts20, aes(color = "denver"),
    #           show.legend = "polygon", fill = NA,
    #           inherit.aes = F, linewidth = 0.25)} +
    # {if(show_boundaries & geo == "Census tracts")
    #   geom_sf(data = ges_tracts20, aes(color = "ges"), fill = NA,
    #           show.legend = "polygon", inherit.aes = F, linewidth = 1)} +
    # {if(show_boundaries & geo == "Census tracts")
    #   scale_color_manual(name = geo,
    #                      values = c("denver" = "black",
    #                                 "ges" = ges_col),
    #                      labels = c("denver" = "All Denver tracts",
    #                                 "ges" = "GES Tracts"))} +
    
    #' Fill option when label_function == "percent", "currency", or "none"
    {if(label_function == "percent")
      scale_fill_binned_sequential(palette = color_ramp,
                                   rev = T,
                                   name = legend_title,
                                   labels = label_percent(scale = 1,
                                                          accuracy = 0.1),
                                   nice.breaks = F,
                                   breaks = function(x) 
                                     quantile(x, probs = c(0.25, 0.5, 0.75)),
                                   show.limits = T)} +
    {if(label_function == "currency")
      scale_fill_binned_sequential(palette = color_ramp,
                                   rev = T,
                                   name = legend_title,
                                   labels = label_currency(accuracy = 0),
                                   nice.breaks = F,
                                   breaks = function(x) 
                                     quantile(x, probs = c(0.25, 0.5, 0.75)),
                                   show.limits = T)} +
    {if(label_function == "none")
      scale_fill_binned_sequential(palette = color_ramp,
                                   rev = T,
                                   name = legend_title,
                                   labels = label_number(accuracy = 0.1),
                                   nice.breaks = F,
                                   breaks = function(x) 
                                     quantile(x, probs = c(0.25, 0.5, 0.75)),
                                   show.limits = T)} +

    #' Correcting the legend for the boundaries
    {if(show_boundaries)
      guides(color = guide_legend(override.aes = list(color = c("black", ges_col),
                                                      fill = c(NA, NA),
                                                      linewidth = c(0.25, 1)),
                                  order = 1))} +

    #' Including highways if requested
    {if(show_highways)
      geom_sf(data = den_highways, #show.legend = "line",
              color = "darkblue", inherit.aes = F, linewidth = 1)} +
    {if(show_highways)
      geom_sf_text(data = den_hw_labels, show.legend = F, color = "darkblue",
                   aes(label = name), inherit.aes = F, fontface = "bold")} +

    #' Transparent layer with the neighborhood names
    geom_sf_interactive(data = nbhd_data, 
                        aes(tooltip = paste0(NBHD_NAME, ": ",
                                             round(.data[[map_var]], 1),
                                             units[[unit]])),
                        fill = NA, color = NA) +
    
    #' Standard elements for all plots
    annotation_north_arrow(aes(location = "tl"),
                           height = unit(1, "cm"), width = unit(1, "cm")) +
    annotation_scale() +
    xlab("")  + ylab("") +
    labs(title = plot_title, 
         subtitle = str_wrap(plot_subtitle, 70),
         caption = paste(text["text_84"], source, "\n", str_wrap(footnote, 70)),
         alt = paste(text["text_82"], alt_text, text["text_83"])) +
    #guides(color = guide_legend(position = "inside")) +
    map_theme
  return(map)
}

#' Box plot function
box_variable <- function(plot_var, color_ramp) {
  var_title <- dictionary[which(dictionary$variable == plot_var), "short_var_name"]
  legend_title <- dictionary[which(dictionary$variable == plot_var), "legend_title"]
  label_function <- dictionary[which(dictionary$variable == plot_var), "label_fn"]
  source <- dictionary[which(dictionary$variable == plot_var), "source"]
  alt_text <- dictionary[which(dictionary$variable == plot_var), "alt_text"]

  box <- ggplot() +
    geom_boxplot(data  = filter(nbhd_data_long, var == plot_var),
                 aes(x = value, y = var),
                 fill = alpha(sequential_hcl(palette = color_ramp, n = 11)[6], 0.5),
                 color = sequential_hcl(palette = color_ramp, n = 11)[6]) +
    theme_test() +
    ylab("") + xlab(legend_title) +
    labs(alt = paste0("A boxplot of ", alt_text, " estimated at the neighborhood level")) +
    scale_y_discrete(labels = var_title) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12))
  return(box)
}

#' Histogram plot function
hist_variable <- function(plot_var, color_ramp) {
  plot_title <- dictionary[which(dictionary$variable == plot_var), "short_var_name"]
  plot_subtitle <- dictionary[which(dictionary$variable == plot_var), "full_var_name"]
  legend_title <- dictionary[which(dictionary$variable == plot_var), "legend_title"]
  show_legend <- dictionary[which(dictionary$variable == plot_var), "show_legend"]
  label_function <- dictionary[which(dictionary$variable == plot_var), "label_fn"]
  source <- dictionary[which(dictionary$variable == plot_var), "source"]
  footnote <- dictionary[which(dictionary$variable == plot_var), "note"]
  alt_text <- dictionary[which(dictionary$variable == plot_var), "alt_text"]
  unit1 <- dictionary[which(dictionary$variable == plot_var), "units"]
  
  hist <- ggplot() +
    geom_histogram(data  = filter(nbhd_data_long, var == plot_var),
                   aes(x = value),
                   bins = 30,
                   fill = alpha(sequential_hcl(palette = color_ramp, n = 11)[6], 0.5),
                   color = sequential_hcl(palette = color_ramp, n = 11)[6]) 
  max_count <- max(ggplot_build(hist)$data[[1]]$count)
  
  hist2 <- hist +
    geom_segment(data = filter(nbhd_data_long, var == plot_var & 
                                 NBHD_NAME == "Globeville - Elyria Swansea"),
                 aes(x = value, xend = value, y = 0, yend = max_count * 0.8)) +
    geom_point(data = filter(nbhd_data_long, var == plot_var & 
                               NBHD_NAME == "Globeville - Elyria Swansea"),
               aes(x = value, y = max_count * 0.8)) +
    geom_text(data = filter(nbhd_data_long, var == plot_var & 
                              NBHD_NAME == "Globeville - Elyria Swansea"),
              aes(x = value, y = max_count * 0.85, 
                  label = paste0("GES: ", round(value, 1), units[unit1])),
              size = 4.5, hjust = "inward", fontface = "bold") +
    theme_test() +
    labs(y = hist_y_lab, 
         x = paste0(plot_title, " (",legend_title, ")"),
         title = plot_title, 
         subtitle = str_wrap(plot_subtitle, 90),
         caption = paste(text["text_84"], source, "\n", str_wrap(footnote, 70)),
         alt = paste(text["text_107"], alt_text, text["text_83"])) +
    #scale_x_continuous(labels = var_title) +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12))
  return(hist2)
}

#' Neighborhood comparison map
compare_map <- function(plot_nbhd1, plot_nbhd2) {
  
  if(plot_nbhd1 == plot_nbhd2) {
    comp_nbhds <- filter(neighborhoods, NBHD_NAME == plot_nbhd1) %>%
      mutate(nbhd_fac = factor(NBHD_NAME, levels = c(plot_nbhd1),
                               labels = c(plot_nbhd1))) %>%
      mutate(nudge_y = 0.01)
    cols <- c(ges_col)
    labs <- c(plot_nbhd1)
      
  } else {
    comp_nbhds <- filter(neighborhoods, NBHD_NAME %in% c(plot_nbhd1, plot_nbhd2)) %>%
      mutate(nbhd_fac = factor(NBHD_NAME, levels = c(plot_nbhd1, plot_nbhd2),
                               labels = c(plot_nbhd1, plot_nbhd2))) %>%
      mutate(nudge_y = c(0.01, -0.01))
    cols <- c(ges_col, den_col)
    labs <- c(plot_nbhd1, plot_nbhd2)
  }
  
  comp_map <- ggplot() +
    geom_sf(data = neighborhoods, fill = "white", color = "black") +
    geom_sf(data = filter(nbhd_data, NBHD_NAME == plot_nbhd1),
            fill = ges_col, color = "black") +
    geom_sf(data = filter(nbhd_data, NBHD_NAME == plot_nbhd2),
            fill = den_col, color = "black") +
    geom_sf_label(data = comp_nbhds,
                        aes(label = nbhd_fac, color = nbhd_fac),
                  nudge_y = comp_nbhds$nudge_y,
                  hjust = "inward", fontface = "bold",
                  size = 4,
                  show.legend = F) +
    scale_color_manual(name = "Neighborhood",
                       values = cols,
                       labels = labs) +
    #' Standard elements for all plots
    annotation_north_arrow(aes(location = "tl"), 
                           height = unit(1, "cm"), width = unit(1, "cm")) +
    annotation_scale() +
    xlab("")  + ylab("") +
    labs(alt = paste(text["text_93"], plot_nbhd1, text["text_94"], 
                     plot_nbhd2, text["text_95"]),
         caption = text["text_89"]) +
    map_theme
  #comp_map
  return(comp_map)
}

#' Bar chart function
compare_variables <- function(plot_var1, plot_var2,
                              plot_nbhd1, plot_nbhd2) {

  #' What we need for the first variable
  plot_title1 <- dictionary[which(dictionary$variable == plot_var1), "short_var_name"]
  plot_subtitle1 <- dictionary[which(dictionary$variable == plot_var1), "full_var_name"]
  units1 <- dictionary[which(dictionary$variable == plot_var1), "units"]
  source1 <- dictionary[which(dictionary$variable == plot_var1), "source"]
  alt_text1 <- dictionary[which(dictionary$variable == plot_var1), "alt_text"]

  bar_df1 <- filter(nbhd_data_long, var == plot_var1) %>%
    st_set_geometry(NULL) %>%
    as.data.frame() %>%
    mutate(units = units[[units1]]) %>%
    mutate(nbhd_label = ifelse(units != "dollars",
                               paste0(NBHD_NAME, ": ", format(round(value, 1), nsmall = 1),
                                      units),
                               paste0(NBHD_NAME, ": ", units,
                                      format(round(value, 1), nsmall = 1))),
           value_scaled = (value - min(value, na.rm = T)) /
             (max(value, na.rm = T) - min(value, na.rm = T)),
           max = max(value_scaled, na.rm = T),
           min = min(value_scaled, na.rm = T))
  bar_min1 <- ifelse(units1 != "dollars",
                     paste0("Min: ", format(round(min(bar_df1$value, na.rm = T), 1),
                                            nsmall = 1), units[[units1]]),
                     paste0("Min: ", units[[units1]],
                            format(round(min(bar_df1$value, na.rm = T), 1), nsmall = 1)))
  bar_max1 <- ifelse(units1 != "dollars",
                     paste0("Max: ", format(round(max(bar_df1$value, na.rm = T), 1),
                                            nsmall = 1), units[[units1]]),
                     paste0("Max: ", units[[units1]],
                            format(round(max(bar_df1$value, na.rm = T), 1), nsmall = 1)))

  bar1 <- ggplot(data = filter(bar_df1, NBHD_NAME %in% c(plot_nbhd1, plot_nbhd2))) +
    geom_linerange(aes(xmin = min, xmax = max, y = var),
                   linewidth = 10, color = "gray50", alpha = 0.25) +
    geom_point(data = filter(bar_df1, NBHD_NAME == plot_nbhd1),
               aes(x = value_scaled, y = var),
               shape = "|", size = 10, color = ges_col) +
    geom_text(data = filter(bar_df1, NBHD_NAME == plot_nbhd1),
              aes(x = value_scaled, y = var, label = nbhd_label),
              color = ges_col, fontface = "bold", vjust = -2, hjust = "inward") +
    geom_point(data = filter(bar_df1, NBHD_NAME == plot_nbhd2),
               aes(x = value_scaled, y = var),
               shape = "|", size = 10, color = den_col) +
    geom_text(data = filter(bar_df1, NBHD_NAME == plot_nbhd2),
              aes(x = value_scaled, y = var, label = nbhd_label),
              color = den_col, fontface = "bold", vjust = 3, hjust = "inward") +
    annotate(geom = "text", x = c(-0.01, 1.01), y = plot_var1,
             label = c(str_wrap(bar_min1, width = 15), str_wrap(bar_max1, width = 15)), 
             hjust = c(1, 0)) +
    #scale_y_discrete(labels = dictionary[which(dictionary$variable == var1), "short_var_name"]) +
    xlim(c(-0.1, 1.1)) +
    xlab("")  + ylab("") +
    ggtitle(label = plot_title1,
            subtitle = paste0(plot_subtitle1, "\n", text["text_84"], " ", source1)) +
    theme_void()
  bar1

  #' What we need for the second variable
  plot_title2 <- dictionary[which(dictionary$variable == plot_var2), "short_var_name"]
  plot_subtitle2 <- dictionary[which(dictionary$variable == plot_var2), "full_var_name"]
  units2 <- dictionary[which(dictionary$variable == plot_var2), "units"]
  source2 <- dictionary[which(dictionary$variable == plot_var2), "source"]
  alt_text2 <- alt_text <- dictionary[which(dictionary$variable == plot_var2), "alt_text"]

  bar_df2 <- filter(nbhd_data_long, var == plot_var2) %>%
    st_set_geometry(NULL) %>%
    as.data.frame() %>%
    mutate(units = units[[units2]]) %>%
    mutate(nbhd_label = ifelse(units != "dollars",
                               paste0(NBHD_NAME, ": ", format(round(value, 1), nsmall = 1),
                                      units),
                               paste0(NBHD_NAME, ": ", units,
                                      format(round(value, 1), nsmall = 1))),
           value_scaled = (value - min(value, na.rm = T)) /
             (max(value, na.rm = T) - min(value, na.rm = T)),
           max = max(value_scaled, na.rm = T),
           min = min(value_scaled, na.rm = T))
  bar_min2 <- ifelse(units2 != "dollars",
                     paste0("Min: ", format(round(min(bar_df2$value, na.rm = T), 1),
                                            nsmall = 1), units[[units2]]),
                     paste0("Min: ", units[[units2]],
                            format(round(min(bar_df2$value, na.rm = T), 1), nsmall = 1)))
  bar_max2 <- ifelse(units2 != "dollars",
                     paste0("Max: ", format(round(max(bar_df2$value, na.rm = T), 1),
                                            nsmall = 1), units[[units2]]),
                     paste0("Max: ", units[[units1]],
                            format(round(max(bar_df2$value, na.rm = T), 1), nsmall = 1)))

  bar2 <- ggplot(data = filter(bar_df2, NBHD_NAME %in% c(plot_nbhd1, plot_nbhd2))) +
    geom_linerange(aes(xmin = min, xmax = max, y = var),
                   linewidth = 10, color = "gray50", alpha = 0.25) +
    geom_point(data = filter(bar_df2, NBHD_NAME == plot_nbhd1),
               aes(x = value_scaled, y = var),
               shape = "|", size = 10, color = ges_col) +
    geom_text(data = filter(bar_df2, NBHD_NAME == plot_nbhd1),
              aes(x = value_scaled, y = var, label = nbhd_label),
              color = ges_col, fontface = "bold", vjust = -2, hjust = "inward") +
    geom_point(data = filter(bar_df2, NBHD_NAME == plot_nbhd2),
               aes(x = value_scaled, y = var),
               shape = "|", size = 10, color = den_col) +
    geom_text(data = filter(bar_df2, NBHD_NAME == plot_nbhd2),
              aes(x = value_scaled, y = var, label = nbhd_label),
              color = den_col, fontface = "bold", vjust = 3, hjust = "inward") +
    annotate(geom = "text", x = c(-0.01, 1.01), y = plot_var2,
             label = c(str_wrap(bar_min2, width = 15), str_wrap(bar_max2, width = 15)), 
             hjust = c(1, 0)) +
    #scale_y_discrete(labels = dictionary[which(dictionary$variable == var1), "short_var_name"]) +
    xlim(c(-0.1, 1.1)) +
    xlab("")  + ylab("") +
    labs(alt = paste0("Chart comparing ", alt_text1, " and ", alt_text2,
                      " between ", plot_nbhd1, " and ", plot_nbhd2)) +
    ggtitle(label = plot_title2,
            subtitle = paste0(plot_subtitle2, "\n", text["text_84"], " ", source2)) +
    theme_void()
  bar2

  combined_plot <- bar1 / bar2
  return(combined_plot)
}

#' Geocoder function
#' Using US Census Geocoder via tidygeocoder
find_nbhd <- function(add) {
  tryCatch(
    {
      add_tibble <- tibble(address = add) %>%
        tidygeocoder::geocode(address, method = "census") %>%
        st_as_sf(coords = c("long", "lat"), crs = "EPSG:4269")
      add_nbhd <- st_transform(add_tibble, crs = st_crs(neighborhoods)) %>%
        st_join(neighborhoods, largest = T)
      return(add_nbhd)
    },
    error = function(e) {
      add_nbhd <- "Need new address"
      return(add_nbhd)
    }
  )
}

#' Map and label neighborhood (if address is in Denver)
map_nbhd <- function(add_nbhd) {
  if(!is.na(add_nbhd$NBHD_NAME[1])) {
    alt_text <- ifelse(spanish == TRUE, 
                       "Un mapa que muestra el vecindario asociado con la dirección ingresada arriba.",
                       "A map showing the neighborhood associated with the address entered above.")
  } else {
    alt_text <- ifelse(spanish == TRUE, 
                       "Un mapa que no muestra ningún vecindario asociado porque la dirección ingresada arriba está fuera de Denver.",
                       "A map showing no associated neighborhood because the address entered above is outside Denver.")
  }
                     
  add_nbhd_map <- ggplot() +
    geom_sf(data = neighborhoods,
            show.legend = "polygon", fill = NA, color = "black",
            inherit.aes = F, linewidth = 0.25) +
    {if(!is.na(add_nbhd$NBHD_NAME[1]))
      geom_sf(data = filter(neighborhoods, NBHD_NAME == add_nbhd$NBHD_NAME[1]), 
              fill = "darkgreen", color = "darkgreen", alpha = 0.2,
              show.legend = "polygon", inherit.aes = F, linewidth = 1)} +
    
    xlab("")  + ylab("") +
    
    {if(!is.na(add_nbhd$NBHD_NAME[1]))
      labs(title = add_nbhd$NBHD_NAME[1],
           alt = alt_text)} +
    
    {if(is.na(add_nbhd$NBHD_NAME[1]))
      labs(title = nbhd_missing,
           alt = alt_text)} +
    
    map_theme +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(add_nbhd_map)
}


