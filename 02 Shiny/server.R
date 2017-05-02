# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(plotly)

shinyServer(function(input, output) { 
  
  #radiobuttons
  # These widgets are for the histograms tab.
  hgonline1 = reactive({input$hgrb1})
  output$races2 <- renderUI({selectInput("selectedRaces", "Choose Races:", race_list, multiple = TRUE) })
  # These widgets are for the scatterplots tab.
  sponline1 = reactive({input$sprb1})
  output$states2 <- renderUI({selectInput("selectedStates", "Choose States:", state_list, multiple = TRUE) })
  # Crosstab
  ctonline1 = reactive({input$ctrb1})
  KPI_Low1 = reactive({input$KPI1})     
  KPI_Medium1 = reactive({input$KPI2})
  ctonline2 = reactive({input$ctrb2})
  KPI_Low2 = reactive({input$KPI3})     
  KPI_Medium2 = reactive({input$KPI4})
  ctonline3 = reactive({input$ctrb3})
  KPI_Low3 = reactive({input$KPI5})     
  KPI_Medium3 = reactive({input$KPI6})
  # barchart
  bconline1 = reactive({input$bcrb1})
  bconline2 = reactive({input$bcrb2})
  bconline3 = reactive({input$bcrb3})
  
  # The following query is for the select list in the histogram tab.
  races = query(
    data.world(propsfile = "www/.data.world"),
    dataset="conneyc/s-17-dv-project-5", type="sql",
    query="select distinct Race as D, Race as R
    from Natality
    order by 1"
  ) # %>% View()
  if(races[1] == "Server error") {
    print("Getting Races from csv")
    file_path = "www/Natality.csv"
    df <- readr::read_csv(file_path) 
    tdf1 = df %>% dplyr::distinct(Race) %>% arrange(Race) %>% dplyr::rename(D = Race)
    tdf2 = df %>% dplyr::distinct(Race) %>% arrange(Race) %>% dplyr::rename(D = Race)
    races = bind_cols(tdf1, tdf2)
  }
  race_list <- as.list(races$D, races$R)
  race_list <- append(list("All" = "All"), race_list)
  
  
  # Begin Histogram Tab ------------------------------------------------------------------
  hgdf1 <- eventReactive(input$hgclick1, {
    if(input$selectedRaces == 'All') race_list <- input$selectedRaces
    else race_list <- append(list("Skip" = "Skip"), input$selectedRaces)
    
    if(hgonline1() == "SQL") {
      print("Getting from data.world")
      df = query(
        data.world(propsfile = "www/.data.world"),
        dataset="conneyc/s-17-dv-project-5", type="sql",
        query="select Race, Average_Age_Mother
        from Natality
        where ? = 'All' or Race in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        queryParameters = race_list
      )  #%>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/Natality.csv"
      df <- readr::read_csv(file_path)
      df <- df %>% dplyr::select(Race, Average_Age_Mother) %>% 
        dplyr::filter(Race %in% input$selectedRaces | input$selectedRaces == "All") 
      #View(df)
    }
    tdf1 = df %>% group_by(Race) %>% summarize(avg_age_mother = mean(Average_Age_Mother))
    dplyr::inner_join(df, tdf1, by = "Race")
  })
  output$hgdata1 <- renderDataTable({DT::datatable(hgdf1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$hgplot1 <- renderPlotly({ p <- ggplot(hgdf1(), aes(x=Average_Age_Mother)) +
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_histogram(aes(x=Average_Age_Mother), binwidth=0.4) +
      geom_vline(aes(xintercept = avg_age_mother), color = "red")
      ggplotly(p)
  })
  
  # End Histogram Tab ___________________________________________________________
  
  
  # The following query is for the select list in the scatterplot tab.
  states = query(
    data.world(propsfile = "www/.data.world"),
    dataset="conneyc/s-17-dv-project-5", type="sql",
    query="select distinct State as D, State as R
    from Natality
    order by 1"
  ) # %>% View()
  if(states[1] == "Server error") {
    print("Getting States from csv")
    file_path = "www/Natality.csv"
    df <- readr::read_csv(file_path) 
    tdf1 = df %>% dplyr::distinct(State) %>% arrange(State) %>% dplyr::rename(D = State)
    tdf2 = df %>% dplyr::distinct(State) %>% arrange(State) %>% dplyr::rename(D = State)
    states = bind_cols(tdf1, tdf2)
  }
  state_list <- as.list(states$D, states$R)
  state_list <- append(list("All" = "All"), state_list)
  
  
  # Begin Scatterplot Tab ------------------------------------------------------------------
  spdf1 <- eventReactive(input$spclick1, {
    if(input$selectedStates == 'All') state_list <- input$selectedStates
    else state_list <- append(list("Skip" = "Skip"), input$selectedStates)
    
    if(sponline1() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="conneyc/s-17-dv-project-5", type="sql",
        query="select State as state, Births, Median_Age, Race
        from Natality n join Census2015 c 
        on n.State = c.AreaName
        where ? = 'All' or State in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        queryParameters = state_list
      )  #%>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/Natality.csv"
      dfn <- readr::read_csv(file_path)
      file_path2 = "www/Census2015.csv"
      dfc <- readr::read_csv(file_path2)
      df <- dplyr::left_join(dfn, dfc, by = c("State"="AreaName"))
      df %>% dplyr::select(state = State, Births, Median_Age, Race) %>%
        dplyr::filter(state %in% input$selectedStates | input$selectedStates == "All")
    }
  })
  output$spdata1 <- renderDataTable({DT::datatable(spdf1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$spplot1 <- renderPlotly({ p <- ggplot(spdf1(), aes(x=Median_Age, y=Births)) +
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_point(aes(x=Median_Age, y = Births, colour = state, shape = Race))
      ggplotly(p)
  })
  
  # End Scatterplot Tab ___________________________________________________________
  
  
  # Begin Crosstab1 Tab ------------------------------------------------------------------
  ctdf1 <- eventReactive(input$ctclick1, {
    if(ctonline1() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="conneyc/s-17-dv-project-5", type="sql",
        query="select Race, State, 
        avg(Average_Birth_Weight) as avg_bw, 
        avg(Average_Birth_Weight) / 4000 as ratio,
        
        case
        when avg(Average_Birth_Weight) / 4000 < ? then 'Low'
        when avg(Average_Birth_Weight) / 4000 < ? then 'Medium'
        else 'High'
        end AS kpi
        
        from Natality
        group by Race, State
        order by Race, State",
        queryParameters = list(KPI_Low1(), KPI_Medium1())
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/Natality.csv"
      df <- readr::read_csv(file_path)
      df %>% 
        dplyr::group_by(Race, State) %>% 
        dplyr::summarize(avg_bw = mean(Average_Birth_Weight),
                         ratio = mean(Average_Birth_Weight) / 4000,
                         kpi = if_else(ratio <= KPI_Low1(), 'Low',
                                       if_else(ratio <= KPI_Medium1(), 'Medium', 'High'))) # %>% View()
    }
  })
  output$ctdata1 <- renderDataTable({DT::datatable(ctdf1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$ctplot1 <- renderPlotly({p <- ggplot(ctdf1()) + 
      theme(axis.text.x=element_text(angle=90, size=11, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=11, hjust=0.5)) +
      geom_text(aes(x=Race, y=State, label=avg_bw), size=3) +
      geom_tile(aes(x=Race, y=State, fill=kpi), alpha=0.50)
      ggplotly(p)
  })
  
  # End Crosstab1 Tab ___________________________________________________________
  
  
  # Begin Crosstab2 Tab ------------------------------------------------------------------
  
  ctdf2 <- eventReactive(input$ctclick2, {
    if(ctonline2() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="conneyc/s-17-dv-project-5", type="sql",
        query="select Race, State, 
        avg(Average_Birth_Weight) as avg_bw, 
        avg(Average_Birth_Weight) / 4000 as ratio,
        
        case
        when avg(Average_Birth_Weight) / 4000 < ? then 'Low'
        when avg(Average_Birth_Weight) / 4000 < ? then 'Medium'
        else 'High'
        end AS kpi
        
        from Natality
        where
        State in ('Alaska', 'Maine', 'Minnesota', 'New Hampshire')
        group by Race, State
        order by Race, State",
        queryParameters = list(KPI_Low2(), KPI_Medium2())
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/Natality.csv"
      df <- readr::read_csv(file_path)
      df %>% 
        dplyr::filter(State %in% 
                        c('Alaska', 'Maine', 'Minnesota', 'New Hampshire')) %>%
        dplyr::group_by(Race, State) %>% 
        dplyr::summarize(avg_bw = mean(Average_Birth_Weight),
                         ratio = mean(Average_Birth_Weight) / 4000,
                         kpi = if_else(ratio <= KPI_Low2(), 'Low',
                                       if_else(ratio <= KPI_Medium2(), 'Medium', 'High'))) # %>% View()
    }
  })
  output$ctdata2 <- renderDataTable({DT::datatable(ctdf2(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$ctplot2 <- renderPlotly({p <- ggplot(ctdf2()) + 
      theme(axis.text.x=element_text(angle=90, size=11, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=11, hjust=0.5)) +
      geom_text(aes(x=Race, y=State, label=avg_bw), size=3) +
      geom_tile(aes(x=Race, y=State, fill=kpi), alpha=0.50)
      ggplotly(p)
  })
  
  # End Crosstab2 Tab ___________________________________________________________
  
  # Begin Crosstab3 Tab ------------------------------------------------------------------
  
  ctdf3 <- eventReactive(input$ctclick3, {
    if(ctonline3() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="conneyc/s-17-dv-project-5", type="sql",
        query="select Race, State, 
        sum(Births) as sum_b, Median_Age,
        
        case
        when avg(Median_Age) < ? then 'Low'
        when avg(Median_Age) < ? then 'Medium'
        else 'High'
        end AS kpi
        
        from Natality n join Census2015 c 
        on n.State = c.AreaName
        group by Race, State, Median_Age
        order by Race, State, Median_Age",
        queryParameters = list(KPI_Low3(), KPI_Medium3())
      ) # %>% View()
    }
    else {
      print("Getting from csv")
      file_path = "www/Natality.csv"
      dfn <- readr::read_csv(file_path)
      file_path2 = "www/Census2015.csv"
      dfc <- readr::read_csv(file_path2)
      df <- dplyr::left_join(dfn, dfc, by = c("State"="AreaName"))
      df %>%
        dplyr::group_by(Race, State, Median_Age) %>% 
        dplyr::summarize(sum_b = sum(Births),
                         ratio = mean(Median_Age),
                         kpi = if_else(ratio <= KPI_Low3(), 'Low',
                                       if_else(ratio <= KPI_Medium3(), 'Medium', 'High'))) # %>% View()
    }
  })
  output$ctdata3 <- renderDataTable({DT::datatable(ctdf3(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$ctplot3 <- renderPlotly({p <- ggplot(ctdf3()) + 
      theme(axis.text.x=element_text(angle=90, size=11, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=11, hjust=0.5)) +
      geom_text(aes(x=Race, y=State, label=sum_b), size=3) +
      geom_tile(aes(x=Race, y=State, fill=kpi), alpha=0.50)
      ggplotly(p)
  })
  
  # End Crosstab3 Tab ___________________________________________________________
  
  
  # Begin Barchart1 Tab ------------------------------------------------------------------
  bcdf1 <- eventReactive(input$bcclick1, {
    if(bconline1() == "SQL") {
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
  output$bcdata1 <- renderDataTable({DT::datatable(bcdf1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$bcplot1 <- renderPlot({ggplot(bcdf1(), aes(x=Race, y=avg_b)) +
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
  
  bcdf2 <- eventReactive(input$bcclick2, {
    if(bconline2() == "SQL") {
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
  output$bcdata2 <- renderDataTable({DT::datatable(bcdf2(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$bcplot2 <- renderPlotly({p <- ggplot(bcdf2(), aes(x=state_id, y=avg_medage)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") +
      geom_text(mapping=aes(x=state_id, y=avg_medage, label=round(avg_medage)), size=5,colour="blue", vjust=-1) +
      #geom_text(mapping=aes(x=state_id, y=avg_medage, label=round(state_id)), size=5,colour="white", vjust=5) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = 40), color="red")
      ggplotly(p)
  })
  # End Barchart2 Tab ___________________________________________________________
  
  # Begin Barchart3 Tab ------------------------------------------------------------------
  
  bcdf3 <- eventReactive(input$bcclick3, {
    if(bconline3() == "SQL") {
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
  output$bcdata3 <- renderDataTable({DT::datatable(bcdf3(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$bcplot3 <- renderPlotly({p <- ggplot(bcdf3(), aes(x=state, y=Births)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=8, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(aes(fill=Race), stat = "identity")
      ggplotly(p)
  })
  
  # End Barchart3 Tab ___________________________________________________________
  
  })
