# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)

shinyServer(function(input, output) { 

  online1 = reactive({input$rb1})
  online2 = reactive({input$rb2})
  online3 = reactive({input$rb3})
  
  # Begin Barchart1 Tab ------------------------------------------------------------------
  df1 <- eventReactive(input$click1, {
    if(online1() == "SQL") {
      #print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="conneyc/s-17-dv-project-5", type="sql",
        query="select State, Race, avg(Average_Birth_Weight) as avg_b
        from Natality
        where State in ('Georgia', 'Alabama', 'South Carolina', 'Mississippi', 'Louisiana')
        group by State, Race"
      )  #%>% View()
    }
    else {
      #print("Getting from csv")
      file_path = "www/Natality.csv"
      df <- readr::read_csv(file_path)
      tdf = df %>% dplyr::filter(State %in% c('Georgia', 'Alabama', 'South Carolina', 'Mississippi', 'Louisiana')) %>%
        dplyr::group_by(State, Race) %>% 
        dplyr::summarize(avg_b = mean(Average_Birth_Weight)) # %>% View()
    }
    tdf1 = tdf %>% group_by(State) %>% summarize(window_avg_b = mean(avg_b))
    dplyr::inner_join(tdf, tdf1, by = "State")
  })
  output$data1 <- renderDataTable({DT::datatable(df1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(df1(), aes(x=Race, y=avg_b)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      #scale_y_continuous(limits = c(0, 5000)) +
      #theme(panel.spacing.x = unit(30, "lines")) +
      #scale_y_continuous(breaks=c(15,30),minor_breaks=c(10,20,25)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~State, ncol=1) + 
      coord_flip() + 
      # Add sum_bw, and (sum_bw - window_avg_bw) label.
      geom_text(mapping=aes(x=Race, y=avg_b, label=round(avg_b)), size=5,colour="white", hjust=8) +
      geom_text(mapping=aes(x=Race, y=avg_b, label=round(avg_b - window_avg_b)), size=4,colour="blue", hjust=-.1) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(window_avg_b)), color="red") +
      geom_text(aes( -1, window_avg_b, label = window_avg_b, vjust = -3, hjust = 1), size=5,color="red")
  })
  
  # End Barchart1 Tab ___________________________________________________________
  
  
  # Begin Barchart2 Tab ------------------------------------------------------------------
  
  df2 <- eventReactive(input$click2, {
    if(online2() == "SQL") {
      #print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="conneyc/s-17-dv-project-5", type="sql",
        query="select State_Code as state_id, AreaName, avg(Median_Age) as avg_medage
        from Natality n join Census2015 c 
        on n.State = c.AreaName
        where Median_Age >= 40
        group by State_Code, AreaName
        order by State_Code, AreaName"
      ) # %>% View()
    }
    else {
      #print("Getting from csv")
      file_path = "www/Natality.csv"
      dfn <- readr::read_csv(file_path)
      file_path2 = "www/Census2015.csv"
      dfc <- readr::read_csv(file_path2)
      df <- dplyr::left_join(dfn, dfc, by = c("State"="AreaName"))
      df %>%
        dplyr::group_by(state_id = State_Code, State) %>% 
        dplyr::filter(Median_Age >= 40) %>%
        dplyr::summarize(avg_medage = mean(Median_Age)) # %>% View()
    }
  })
  output$data2 <- renderDataTable({DT::datatable(df2(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot2 <- renderPlot({ggplot(df2(), aes(x=state_id, y=avg_medage)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") +
      geom_text(mapping=aes(x=state_id, y=avg_medage, label=round(avg_medage)), size=5,colour="red", vjust=-1) +
      geom_text(mapping=aes(x=state_id, y=avg_medage, label=round(state_id)), size=5,colour="white", vjust=5) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = 40), color="red")
  })
  # End Barchart2 Tab ___________________________________________________________
  
  # Begin Barchart3 Tab ------------------------------------------------------------------
  
  df3 <- eventReactive(input$click3, {
    if(online3() == "SQL") {
      #print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="conneyc/s-17-dv-project-5", type="sql",
        query="select Births, Race, AreaName as state, avg(Median_Age) as avg_medage
        from Natality n join Census2015 c 
        on n.State = c.AreaName
        where Median_Age >= 40
        group by Births, Race, AreaName
        order by Births, Race, AreaName"
      ) # %>% View()
    }
    else {
      #print("Getting from csv")
      file_path = "www/Natality.csv"
      dfn <- readr::read_csv(file_path)
      file_path2 = "www/Census2015.csv"
      dfc <- readr::read_csv(file_path2)
      df <- dplyr::left_join(dfn, dfc, by = c("State"="AreaName"))
      df %>%
        dplyr::group_by(Births, Race, state = State) %>% 
        dplyr::filter(Median_Age >= 40) %>%
        dplyr::summarize(avg_medage = mean(Median_Age)) # %>% View()
    }
  })
  output$data3 <- renderDataTable({DT::datatable(df3(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot3 <- renderPlot({ggplot(df3(), aes(x=state, y=Births)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(aes(fill=Race), stat = "identity")
  })
  
  # End Barchart3 Tab ___________________________________________________________
  
  })
