library(shiny)
library(bslib)
library(rsconnect)
library(shinyWidgets)

#' -----------------------------------------------------------------------------
#' App code starts here
#' -----------------------------------------------------------------------------

#' source helpers
source("ges_health_study_app/helpers.R")

# Define UI ----
ui <- page_navbar(title = text["text_1"],
  setBackgroundColor("#3c005a"),
                  
  #' Welcome tab
  nav_panel(title = text["text_2"],
            titlePanel(h1(text["text_2"], style = 'color:white; font-weight: bold;')),
            page_fluid(
              card(card_header(h6(HTML(text["text_switch"]))),
                   card_header(h6(HTML(text["text_access"])))),
              card(
                card_header(h3(text["text_3"])),
                p(text["text_4"]),
                p(text["text_5"]),
                p(HTML(text["text_6"])),
                p(text["text_7"])),
              card(
                card_header(text["text_8"]),
                p(HTML(text["text_9"]))
              ),
              card(
                card_header(text["text_10"]),
                p(HTML(text["text_11"])),
                p(text["text_12"])
              ),
              card(
                card_header(text["text_97"]),
                p(HTML(text["text_106"])),
                p(text["text_12"])
              ),
              card(
                card_header(text["text_13"]),
                p(HTML(text["text_14"]))
              ),
              card(
                card_header(text["text_15"]),
                p(HTML(text["text_16"]))
              ),
              card(
                card_header(text["text_17"]),
                p(HTML(text["text_18"])),
              ),
              card(
                card_header(text["text_19"]),
                p(HTML(text["text_20"])),
              ),
              card_footer(paste(text["text_21"], pub_date)),
              card_footer(HTML(text["text_access"]))
            )
  ),
  #' From the CC Tab
  nav_panel(title = text["text_8"],
            titlePanel(h1(text["text_8"], style = 'color:white; font-weight: bold;')),
            card(
              card_title(text["text_22"]),
              p(text["text_23"]),
              card_footer(paste(text["text_21"], pub_date)),
              card_footer(HTML(text["text_access"]))
            )
  ),
  # Interactive Maps: Environment Tab
  nav_panel(title = text["text_10"],
            page_sidebar(
              sidebar = sidebar(width = 400,
                                h5(text["text_24"],
                                   style = 'color:white; font-weight: bold;'),
                                card(text["text_25"],
                                  selectInput("var1",
                                              label = text["text_26"],
                                              choices = pop_var_list,
                                              selected = "pct_hl"),
                                  selectInput("var2",
                                              label = text["text_27"],
                                              choices = env_var_list,
                                              selected = "pm25")
                                ),
                                card(text["text_28"],
                                     downloadButton("download_data1",
                                                    text["text_29"])),
                                card(girafeOutput("neighborhood_map1"),
                                     em(text["text_108"]))
              ),
              titlePanel(h1(text["text_10"], style = 'color:white; font-weight: bold;')),
              #titlePanel(text["text_10"]),
              page_fluid(
                card(
                  p(text["text_30"]),
                  p(text["text_31"]),
                  p(text["text_32"]),
                  p(text["text_33"]),
                  p(text["text_34"]),
                ),
                layout_columns(
                  card(card_header(text["text_35"]),
                       p(text["text_100"]),
                       layout_column_wrap(girafeOutput("map_var1"), 
                                          girafeOutput("map_var2"), 
                                          htmlOutput("map_var1_cap"), 
                                          htmlOutput("map_var2_cap"),
                                          width = (1/2), 
                                          heights_equal = "row"),
                       downloadButton("download_maps1",
                                      text["text_85"])),
                  card(card_header(text["text_36"]),
                       p(text["text_101"]),
                       layout_column_wrap(plotOutput("hist_var1"),
                                          plotOutput("hist_var2"),
                                          htmlOutput("hist_var1_cap"),
                                          htmlOutput("hist_var2_cap"),
                                          width = (1/2),
                                          heights_equal = "row"
                       ),
                       downloadButton("download_hist1",
                                      text["text_98"])),
                  card(card_header(text["text_37"]),
                       layout_sidebar(
                         sidebar = sidebar(
                           width = 350,
                           text["text_38"],
                           selectInput("nbhd1",
                                       label = text["text_39"],
                                       choices = neighborhood_names,
                                       selected = "Globeville - Elyria Swansea"),
                           selectInput("nbhd2",
                                       label = text["text_40"],
                                       choices = neighborhood_names,
                                       selected = "Cherry Creek"),
                           plotOutput("compare_map1")
                         ),
                         p(text["text_102"]),
                         plotOutput("compare_plot1"),
                         htmlOutput("compare_plot1_cap"),
                         downloadButton("download_comp1",
                                        text["text_99"]),
                         )),
                  col_widths = c(12, 12, 12),
                  row_heights = c(1, 1, 1)),
                card(card_footer(paste(text["text_21"], pub_date)),
                     card_footer(HTML(text["text_access"])))
                )
              )
            ),
  # Interactive Maps: Health Tab
  nav_panel(title = text["text_97"],
            page_sidebar(
              sidebar = sidebar(width = 400,
                                h5(text["text_24"], 
                                   style = 'color:white; font-weight: bold;'),
                                card(text["text_25"],
                                     selectInput("var3",
                                                 label = text["text_26"],
                                                 choices = pop_var_list,
                                                 selected = "pct_hl"),
                                     selectInput("var4",
                                                 label = text["text_96"],
                                                 choices = health_var_list,
                                                 selected = "life_exp")
                                ),
                                card(text["text_28"],
                                     downloadButton("download_data2",
                                                    text["text_29"])),
                                card(girafeOutput("neighborhood_map2"),
                                     em(text["text_108"]))
              ),
              titlePanel(h1(text["text_97"], style = 'color:white; font-weight: bold;')),
              #titlePanel(text["text_97"]),
              page_fluid(
                card(
                  p(text["text_90"]),
                  p(text["text_91"]),
                  p(text["text_32"]),
                  p(text["text_33"]),
                  p(text["text_34"]),
                ),
                layout_columns(
                  card(card_header(text["text_35"]),
                       p(text["text_103"]),
                       layout_column_wrap(girafeOutput("map_var3"),
                                          girafeOutput("map_var4"),
                                          htmlOutput("map_var3_cap"),
                                          htmlOutput("map_var4_cap"),
                                          width = (1/2), 
                                          heights_equal = "row"),
                       downloadButton("download_maps2",
                                      text["text_85"])),
                  card(card_header(text["text_36"]),
                       p(text["text_104"]),
                       layout_column_wrap(plotOutput("hist_var3"),
                                      plotOutput("hist_var4"),
                                      htmlOutput("hist_var3_cap"),
                                      htmlOutput("hist_var4_cap"),
                                      width = (1/2),
                                      heights_equal = "row"),
                       downloadButton("download_hist2",
                                      text["text_98"])),
                  card(card_header(text["text_37"]),
                       layout_sidebar(
                         sidebar = sidebar(
                           width = 350,
                           text["text_38"],
                           selectInput("nbhd3",
                                       label = text["text_39"],
                                       choices = neighborhood_names,
                                       selected = "Globeville - Elyria Swansea"),
                           selectInput("nbhd4",
                                       label = text["text_40"],
                                       choices = neighborhood_names,
                                       selected = "Cherry Creek"),
                           plotOutput("compare_map2")
                         ),
                         p(text["text_105"]),
                         plotOutput("compare_plot2"),
                         htmlOutput("compare_plot2_cap"),
                         downloadButton("download_comp2",
                                        text["text_99"]),
                       )),
                  col_widths = c(12, 12, 12),
                  row_heights = c(1, 1, 1)),
                card(card_footer(paste(text["text_21"], pub_date)),
                     card_footer(HTML(text["text_access"])))
              )
            )
  ),
  #' Community Maps Tab
  nav_panel(title = text["text_13"],
            titlePanel(h1(text["text_13"], style = 'color:white; font-weight: bold;')),
            page_fluid(
              card(
                p(text["text_41"]),
                p(HTML(text["text_42"])) 
              ),
              card(card_header(text["text_43"]),
                   htmlOutput("comm_map"),
                   p(text["text_44"]),
                   full_screen = T),
              card(card_footer(paste(text["text_21"], pub_date)),
                   card_footer(HTML(text["text_access"])))
            )
            ),
  #' Other Health and Env Maps Tab
  nav_panel(title = text["text_15"],
            titlePanel(h1(text["text_15"], style = 'color:white; font-weight: bold;')),
            page_fluid(
              card(
                p(text["text_45"]),
                p(HTML(text["text_46"]))),
              navset_card_tab(
                nav_panel(title = text["text_47"],
                          card(card_header(text["text_48"]),
                               p(text["text_49"])),
                          htmlOutput("health_map"),
                          p(HTML(text["text_50"]))),
                nav_panel(title = text["text_51"],
                          card(card_header(text["text_52"]),
                               p(text["text_53"]),
                               p(text["text_54"])),
                          htmlOutput("nbn_map"),
                          p(HTML(text["text_55"]))),
                nav_panel(title = text["text_56"],
                          card(card_header(text["text_57"]),
                               p(HTML(text["text_58"])),
                               p(HTML(text["text_59"]))),
                          htmlOutput("tree_map"),
                          p(HTML(text["text_60"]))),
              ),
              card(card_footer(paste(text["text_21"], pub_date)),
                   card_footer(HTML(text["text_access"])))
            )
  ),
  #' Learn More Tab
  nav_panel(title = text["text_17"],
            titlePanel(h1(text["text_17"], style = 'color:white; font-weight: bold;')),
            card(
              card_title(text["text_61"]),
              p(text["text_62"]),
              p(HTML(text["text_63"])),
              p(HTML(text["text_64"])),
              p(HTML(text["text_65"])),
              p(HTML(text["text_66"])),
              card_footer(paste(text["text_21"], pub_date)),
              card_footer(HTML(text["text_access"]))
            )
  ),
  #' About Us Tab
  nav_panel(title = text["text_19"],
            titlePanel(h1(text["text_19"], style = 'color:white; font-weight: bold;')),
            layout_columns(
              card(p(text["text_67"]),
                   p(text["text_68"]),
                   p(text["text_69"]),
                   p(text["text_70"]),
                   p(HTML(text["text_71"])),
                   p(HTML(text["text_86"])),
                   card_footer(paste(text["text_21"], pub_date)),
                   card_footer(HTML(text["text_access"]))
              ),
              img(src = "environs_logo.png", align = "center",
                  alt = text["text_79"]),
              col_widths = c(8, 4)
              )
            )
)

# Define server logic ----
server <- function(input, output, session) {

  output$neighborhood_map1 <- renderGirafe({
    nbhd_map1 <- ggplot() +
      geom_sf(data = neighborhoods, aes(color = "denver"),
              show.legend = "polygon", fill = NA,
              inherit.aes = F, linewidth = 0.25) +
      geom_sf(data = ges, aes(color = "ges"), fill = NA,
              show.legend = "polygon", inherit.aes = F, linewidth = 1) +
      geom_sf_interactive(data = neighborhoods, aes(tooltip = NBHD_NAME),
                          fill = NA, color = NA) +
      scale_color_manual(name = geo_name,
                         values = c("denver" = "black",
                                    "ges" = ges_col),
                         labels = c("denver" = den_name,
                                    "ges" = ges_name)) +
      #' Standard elements for all plots
      annotation_north_arrow(aes(location = "tl"), 
                             height = unit(1, "cm"), width = unit(1, "cm")) +
      annotation_scale() +
      xlab("")  + ylab("") +
      labs(title = text["text_87"],
           subtitle = text["text_92"],
           alt = text["text_88"],
           caption = text["text_89"]) +
      map_theme +
      theme(legend.position = "inside",
            legend.position.inside = c(0.84, 0.55))
    
    girafe(ggobj = nbhd_map1,
           height_svg = 6, width_svg = 6)
  })
  #' Caption for nbhd map is static: text_108
  
  output$neighborhood_map2 <- renderGirafe({
    nbhd_map2 <- ggplot() +
      geom_sf(data = neighborhoods, aes(color = "denver"),
              show.legend = "polygon", fill = NA,
              inherit.aes = F, linewidth = 0.25) +
      geom_sf(data = ges, aes(color = "ges"), fill = NA,
              show.legend = "polygon", inherit.aes = F, linewidth = 1) +
      geom_sf_interactive(data = neighborhoods, aes(tooltip = NBHD_NAME),
                          fill = NA, color = NA) +
      scale_color_manual(name = geo_name,
                         values = c("denver" = "black",
                                    "ges" = ges_col),
                         labels = c("denver" = den_name,
                                    "ges" = ges_name)) +
      #' Standard elements for all plots
      annotation_north_arrow(aes(location = "tl"), 
                             height = unit(1, "cm"), width = unit(1, "cm")) +
      annotation_scale() +
      xlab("")  + ylab("") +
      labs(title = text["text_87"],
           subtitle = text["text_92"],
           alt = text["text_88"],
           caption = text["text_89"]) +
      map_theme +
      theme(legend.position = "inside",
            legend.position.inside = c(0.84, 0.55))
    
    girafe(ggobj = nbhd_map2,
           height_svg = 6, width_svg = 6)
  })
  #' Caption for nbhd map is static: text_108
  
  output$map_var1 <- renderGirafe({
    mapping_var1 <- map_variable(geo = "Neighborhoods", 
                                 map_var = input$var1, show_boundaries = T,
                                 show_highways = T, color_ramp = "Blues")
    girafe(ggobj = mapping_var1,
           height_svg = 7, width_svg = 7)
  })

  output$map_var1_cap <- renderUI({
    HTML(
    paste(text["text_109"], 
          text["text_82"],
          dictionary[which(dictionary$variable == input$var1), "alt_text"],
          text["text_83"])
    )
  })
  
  output$map_var2 <- renderGirafe({
    mapping_var2 <- map_variable(geo = "Neighborhoods", 
                                 map_var = input$var2, show_boundaries = T,
                                 show_highways = T, color_ramp = "Burg")
    girafe(ggobj = mapping_var2,
           height_svg = 7, width_svg = 7)
  })
  
  output$map_var2_cap <- renderUI({
    HTML(
      paste(text["text_110"], 
          text["text_82"],
          dictionary[which(dictionary$variable == input$var2), "alt_text"],
          text["text_83"])
    )
  })
  
  output$map_var3 <- renderGirafe({
    mapping_var3 <- map_variable(geo = "Neighborhoods", 
                                 map_var = input$var3, show_boundaries = T,
                                 show_highways = T, color_ramp = "Blues")
    girafe(ggobj = mapping_var3,
           height_svg = 7, width_svg = 7)
  })
  
  output$map_var3_cap <- renderUI({
    HTML(
      paste(text["text_109"], 
          text["text_82"],
          dictionary[which(dictionary$variable == input$var3), "alt_text"],
          text["text_83"])
    )
  })

  output$map_var4 <- renderGirafe({
    mapping_var4 <- map_variable(geo = "Neighborhoods", 
                                 map_var = input$var4, show_boundaries = T,
                                 show_highways = T, color_ramp = "Purples")
    girafe(ggobj = mapping_var4,
           height_svg = 7, width_svg = 7)
  })
  
  output$map_var4_cap <- renderUI({
    HTML(
    paste(text["text_110"], 
          text["text_82"],
          dictionary[which(dictionary$variable == input$var4), "alt_text"],
          text["text_83"])
    )
  })
  
  output$hist_var1 <- renderPlot({
    hist_variable(plot_var = input$var1, color_ramp = "Blues")
  },
  alt = reactive({
    paste(text["text_107"], 
          dictionary[which(dictionary$variable == input$var1), "alt_text"],
          text["text_83"])
  })
  )
  
  output$hist_var1_cap <- renderUI({
    HTML(
    paste(text["text_111"],
          text["text_107"],
          dictionary[which(dictionary$variable == input$var1), "alt_text"],
          text["text_83"])
    )
  })
  
  output$hist_var2 <- renderPlot({
    hist_variable(plot_var = input$var2, color_ramp = "Burg")
  },
  alt = reactive({
    paste(text["text_107"], 
          dictionary[which(dictionary$variable == input$var2), "alt_text"],
          text["text_83"])
  })
  )
  
  output$hist_var2_cap <- renderUI({
    HTML(
    paste(text["text_112"],
          text["text_107"],
          dictionary[which(dictionary$variable == input$var2), "alt_text"],
          text["text_83"])
    )
  })
  
  output$hist_var3 <- renderPlot({
    hist_variable(plot_var = input$var3, color_ramp = "Blues")
  },
  alt = reactive({
    paste(text["text_107"], 
          dictionary[which(dictionary$variable == input$var3), "alt_text"],
          text["text_83"])
  })
  )
  
  output$hist_var3_cap <- renderUI({
    HTML(
    paste(text["text_111"],
          text["text_107"],
          dictionary[which(dictionary$variable == input$var3), "alt_text"],
          text["text_83"])
    )
  })
  
  output$hist_var4 <- renderPlot({
    hist_variable(plot_var = input$var4, color_ramp = "Purples")
  },
  alt = reactive({
    paste(text["text_107"], 
          dictionary[which(dictionary$variable == input$var4), "alt_text"],
          text["text_83"])
  })
  )
  
  output$hist_var4_cap <- renderUI({
    HTML(
      paste(text["text_112"],
          text["text_107"],
          dictionary[which(dictionary$variable == input$var4), "alt_text"],
          text["text_83"])
    )
  })
  
  output$compare_map1 <- renderPlot({
    compare_map(plot_nbhd1 = input$nbhd1, plot_nbhd2 = input$nbhd2)
  },
  alt = reactive({
    paste(text["text_82"], input$nbhd1, text["text_77"], input$nbhd2)
  })
  )
  
  output$compare_map2 <- renderPlot({
    compare_map(plot_nbhd1 = input$nbhd3, plot_nbhd2 = input$nbhd4)
  },
  alt = reactive({
    paste(text["text_82"], input$nbhd3, text["text_77"], input$nbhd4)
  })
  )
  
  output$compare_plot1 <- renderPlot({
    compare_variables(plot_var1 = input$var1, plot_var2 = input$var2,
                      plot_nbhd1 = input$nbhd1, plot_nbhd2 = input$nbhd2)
  }, 
  alt = reactive({
    paste(text["text_76"], 
          dictionary[which(dictionary$variable == input$var1), "alt_text"], 
          text["text_77"],
          dictionary[which(dictionary$variable == input$var2), "alt_text"],
          text["text_78"], input$nbhd1, text["text_77"], input$nbhd2)
  })
  )
  
  output$compare_plot1_cap <- renderUI({
    HTML(
    paste(text["text_113"], 
          text["text_76"], 
          dictionary[which(dictionary$variable == input$var1), "alt_text"], 
          text["text_77"],
          dictionary[which(dictionary$variable == input$var2), "alt_text"],
          text["text_78"], input$nbhd1, text["text_77"], input$nbhd2)
    )
  })
  
  output$compare_plot2 <- renderPlot({
    compare_variables(plot_var1 = input$var3, plot_var2 = input$var4,
                      plot_nbhd1 = input$nbhd3, plot_nbhd2 = input$nbhd4)
  }, 
  alt = reactive({
    paste(text["text_76"], 
          dictionary[which(dictionary$variable == input$var3), "alt_text"], 
          text["text_77"],
          dictionary[which(dictionary$variable == input$var4), "alt_text"],
          text["text_78"], input$nbhd3, text["text_77"], input$nbhd4)
  })
  )
  
  output$compare_plot2_cap <- renderUI({
    HTML(
      paste(text["text_113"], 
          text["text_76"], 
          dictionary[which(dictionary$variable == input$var3), "alt_text"], 
          text["text_77"],
          dictionary[which(dictionary$variable == input$var4), "alt_text"],
          text["text_78"], input$nbhd1, text["text_77"], input$nbhd2)
    )
  })
  
  output$download_data1 <- downloadHandler(
    filename = function() {
      paste0(input$var1, "_", input$var2, ".csv")
    },
    content = function(filename) {
      write_csv(data_to_save(nbhd_data_long, input$var1, input$var2), filename)
    }
  )
  
  output$download_data2 <- downloadHandler(
    filename = function() {
      paste0(input$var3, "_", input$var4, ".csv")
    },
    content = function(filename) {
      write_csv(data_to_save(nbhd_data_long, input$var3, input$var4), filename)
    }
  )
  
  output$download_maps1 <- downloadHandler(
    filename = function() {
      paste0(input$var1, "_", input$var2, "_maps.jpeg")
    },
    content = function(filename) {
      jpeg(filename, width = 980, height = 400, units = "px", pointsize = 12,
           bg = "white")
      map1 <- map_variable(geo = "Neighborhoods", 
                           map_var = input$var1, show_boundaries = T,
                           show_highways = T, color_ramp = "Blues")
      map2 <- map_variable(geo = "Neighborhoods", 
                           map_var = input$var2, show_boundaries = T,
                           show_highways = T, color_ramp = "Burg")
      maps <- map1 + map2
      print(maps)
      dev.off()
    },
    contentType = 'image/jpeg'
  )
  
  output$download_maps2 <- downloadHandler(
    filename = function() {
      paste0(input$var3, "_", input$var4, "_maps.jpeg")
    },
    content = function(filename) {
      jpeg(filename, width = 980, height = 400, units = "px", pointsize = 12,
           bg = "white")
      map1 <- map_variable(geo = "Neighborhoods", 
                           map_var = input$var3, show_boundaries = T,
                           show_highways = T, color_ramp = "Blues")
      map2 <- map_variable(geo = "Neighborhoods", 
                           map_var = input$var4, show_boundaries = T,
                           show_highways = T, color_ramp = "Purples")
      maps <- map1 + map2
      print(maps)
      dev.off()
    },
    contentType = 'image/jpeg'
  )
  
  output$download_hist1 <- downloadHandler(
    filename = function() {
      paste0(input$var1, "_", input$var2, "_histograms.jpeg")
    },
    content = function(filename) {
      jpeg(filename, width = 980, height = 400, units = "px", pointsize = 12,
           bg = "white")
      
      hist1 <- hist_variable(plot_var = input$var1, color_ramp = "Blues")
      hist2 <- hist_variable(plot_var = input$var2, color_ramp = "Burg")
      
      hist <- hist1 + hist2
      print(hist)
      dev.off()
    },
    contentType = 'image/jpeg'
  )
  
  output$download_hist2 <- downloadHandler(
    filename = function() {
      paste0(input$var3, "_", input$var4, "_histograms.jpeg")
    },
    content = function(filename) {
      jpeg(filename, width = 980, height = 400, units = "px", pointsize = 12,
           bg = "white")
      
      hist1 <- hist_variable(plot_var = input$var3, color_ramp = "Blues")
      hist2 <- hist_variable(plot_var = input$var4, color_ramp = "Purples")
      
      hist <- hist1 + hist2
      print(hist)
      dev.off()
    },
    contentType = 'image/jpeg'
  )
  
  output$download_comp1 <- downloadHandler(
    filename = function() {
      paste0(input$var1, "_", input$var2, "_", 
             input$nbhd1, "_", input$nbhd2, "_comparisons.jpeg")
    },
    content = function(filename) {
      jpeg(filename, width = 980, height = 400, units = "px", pointsize = 12,
           bg = "white")
      
      comp <- compare_variables(plot_var1 = input$var1, plot_var2 = input$var2,
                                plot_nbhd1 = input$nbhd1, plot_nbhd2 = input$nbhd2)
      print(comp)
      dev.off()
    },
    contentType = 'image/jpeg'
  )
  
  output$download_comp2 <- downloadHandler(
    filename = function() {
      paste0(input$var3, "_", input$var4, "_", 
             input$nbhd3, "_", input$nbhd4, "_comparisons.jpeg")
    },
    content = function(filename) {
      jpeg(filename, width = 980, height = 400, units = "px", pointsize = 12,
           bg = "white")
      
      comp <- compare_variables(plot_var1 = input$var3, plot_var2 = input$var4,
                                plot_nbhd1 = input$nbhd3, plot_nbhd2 = input$nbhd4)
      print(comp)
      dev.off()
    },
    contentType = 'image/jpeg'
  )

  #addResourcePath("html_maps", "./html_maps")
  output$comm_map <- renderUI({
    tags$iframe(seamless = "seamless",
                src = comm_map_source,
                height = 800, width = 1000,
                alt = text["text_72"])
  })
  
  output$health_map <- renderUI({
    tags$iframe(seamless = "seamless",
                src = health_map_source,
                height = 800, width = 1000,
                alt = text["text_73"])
  })
  
  output$nbn_map <- renderUI({
    tags$iframe(seamless = "seamless",
                src = nbn_map_source,
                height = 800, width = 1000,
                alt = text["text_74"])
  })
  
  output$tree_map <- renderUI({
    tags$iframe(seamless = "seamless",
                src = tree_map_source,
                height = 800, width = 1000,
                alt = text["text_75"])
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
