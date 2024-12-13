library(shiny)
library(bslib)
library(rsconnect)

#' -----------------------------------------------------------------------------
#' App code starts here
#' -----------------------------------------------------------------------------

#' source helpers
source("ges_health_study_app/helpers.R")

# Define UI ----
ui <- page_navbar(title = text["text_1"],
  #' Welcome tab
  nav_panel(title = text["text_2"],
            page_fluid(
              card(card_header(h5(HTML(text["text_switch"]))),
                   card_header(h5(HTML(text["text_access"])))),
              card(            
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
            )
            
  ),
  #' From the CC Tab
  nav_panel(title = text["text_8"],
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
              sidebar = sidebar(width = 450,
                                h5(text["text_24"]),
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
                                card(girafeOutput("neighborhood_map1"))
              ),
              titlePanel(text["text_10"]),
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
                       p("Explain here"),
                       layout_columns(girafeOutput("map_var1"), girafeOutput("map_var2")),
                       downloadButton("download_maps1",
                                      text["text_85"])),
                  card(card_header(text["text_36"]),
                       p("Explain here"),
                       layout_columns(plotOutput("hist_var1"), plotOutput("hist_var2")),
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
                         p("Explain here"),
                         plotOutput("compare_plot1"),
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
              sidebar = sidebar(width = 450,
                                h5(text["text_24"]),
                                card(text["text_25"],
                                     selectInput("var3",
                                                 label = text["text_26"],
                                                 choices = pop_var_list,
                                                 selected = "pct_hl"),
                                     selectInput("var4",
                                                 label = text["text_96"],
                                                 choices = health_var_list,
                                                 selected = "ast_hosp")
                                ),
                                card(text["text_28"],
                                     downloadButton("download_data2",
                                                    text["text_29"])),
                                card(girafeOutput("neighborhood_map2"))
              ),
              titlePanel(text["text_97"]),
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
                       p("Explain here"),
                       layout_columns(girafeOutput("map_var3"), girafeOutput("map_var4")),
                       downloadButton("download_maps2",
                                      text["text_85"])),
                  card(card_header(text["text_36"]),
                       p("Explain here"),
                       layout_columns(plotOutput("hist_var3"), plotOutput("hist_var4")),
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
                         p("Explain here"),
                         plotOutput("compare_plot2"),
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
    nbhd_map <- ggplot() +
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
    
    girafe(ggobj = nbhd_map,
           height_svg = 6, width_svg = 6)
  })
  
  output$neighborhood_map2 <- renderGirafe({
    nbhd_map <- ggplot() +
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
    
    girafe(ggobj = nbhd_map,
           height_svg = 6, width_svg = 6)
  })
  
  output$map_var1 <- renderGirafe({
    mapping_var1 <- map_variable(geo = "Neighborhoods", 
                                 map_var = input$var1, show_boundaries = T,
                                 show_highways = T, color_ramp = "Blues")
    girafe(ggobj = mapping_var1,
           height_svg = 7, width_svg = 7)
  })
  
  output$map_var2 <- renderGirafe({
    mapping_var2 <- map_variable(geo = "Neighborhoods", 
                                 map_var = input$var2, show_boundaries = T,
                                 show_highways = T, color_ramp = "Greens")
    girafe(ggobj = mapping_var2,
           height_svg = 7, width_svg = 7)
  })
  
  output$map_var3 <- renderGirafe({
    mapping_var3 <- map_variable(geo = "Neighborhoods", 
                                 map_var = input$var3, show_boundaries = T,
                                 show_highways = T, color_ramp = "Blues")
    girafe(ggobj = mapping_var3,
           height_svg = 7, width_svg = 7)
  })
  
  output$map_var4 <- renderGirafe({
    mapping_var4 <- map_variable(geo = "Neighborhoods", 
                                 map_var = input$var4, show_boundaries = T,
                                 show_highways = T, color_ramp = "Greens")
    girafe(ggobj = mapping_var4,
           height_svg = 7, width_svg = 7)
  })
  
  output$hist_var1 <- renderPlot({
    hist_variable(plot_var = input$var1, color_ramp = "Blues")
  })
  
  output$hist_var2 <- renderPlot({
    hist_variable(plot_var = input$var2, color_ramp = "Greens")
  })
  
  output$hist_var3 <- renderPlot({
    hist_variable(plot_var = input$var3, color_ramp = "Blues")
  })
  
  output$hist_var4 <- renderPlot({
    hist_variable(plot_var = input$var4, color_ramp = "Purples")
  })
  
  output$compare_map1 <- renderPlot({
    compare_map(plot_nbhd1 = input$nbhd1, plot_nbhd2 = input$nbhd2)
  })
  
  output$compare_map2 <- renderPlot({
    compare_map(plot_nbhd1 = input$nbhd3, plot_nbhd2 = input$nbhd4)
  })
  
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
                           show_highways = T, color_ramp = "Greens")
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
      hist2 <- hist_variable(plot_var = input$var2, color_ramp = "Greens")
      
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
