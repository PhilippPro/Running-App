server <- function(input, output) {

  source("extra_functions.R")
  data = read_ods(path = "/media/philipp/Elements/Sport/Laufzeiten.ods", sheet = 1, range = "A1:H91")
  setDT(data)

  data$Index = 1:nrow(data)
  data$Datum = as.Date(data$Datum, format = "%d.%m.%Y")
  data$Zeit_pro_km = data$Zeit/data$km
  data$Minute_pro_km = round((data$Zeit_pro_km) / 60, 2)
  data$Kilometer_pro_h = round(data$km/as.numeric(((data$Zeit)/3600)), 2)
  data$Min_pro_km = round(1/(data$Kilometer_pro_h/60), 2)
  
  data$Jahr = paste0("20", format(data$Datum, format="%Y"))
  data$Monat = as.numeric(format(data$Datum, format="%m"))
  
  # Gesamtanzahl km
  gesamt_km = sum(data$km)
  # Gesamtzeit in h
  gesamt_h = round(sum(as.numeric(data$Zeit))/3600, 2)
  # Gesamt: km pro h
  gesamt_km_h = round(gesamt_km/gesamt_h, 2)
  # Gesamt: Zeit pro km
  gesamt_km_min = gesamt_km_h/60
  gesamt_min_km = round(1/gesamt_km_min, 2)
  # Zusammenfassung
  gesamt_table = data.frame(Kilometer = gesamt_km, Stunden = gesamt_h, KM_H = gesamt_km_h, MIN_KM = gesamt_min_km)
  
  # Subsets
  # km pro Jahr
  km_pro_jahr = data[, .(sum_km = sum(km)), by = list(Jahr)]
  # km pro Jahr/Monat
  km_pro_jahr_monat = data[, .(sum_km = sum(km), mean_km = round(mean(km), 2)), by = list(Jahr, Monat)]
  colnames(km_pro_jahr_monat)[3] = "sum_km"
  km_pro_jahr_monat$Jahr_Monat = factor(
    paste0(km_pro_jahr_monat$Monat, "/", km_pro_jahr_monat$Jahr), 
    levels = paste0(km_pro_jahr_monat$Monat, "/", km_pro_jahr_monat$Jahr)
  )
  # Zum Ergänzen
  # km/h pro Jahr
  # km/h pro Jahr/Monat
  
  # Aufteilung nach Schuhen
  km_pro_schuhe = data[, .(sum_km = round(sum(km))), by = list(Schuhe)]
  km_pro_schuhe = km_pro_schuhe[order(sum_km), ]
  km_pro_schuhe$Schuhe = factor(
    km_pro_schuhe$Schuhe, 
    levels = km_pro_schuhe$Schuhe
  )
  
  # Outputs
  # Summary table
  output$summary_table = renderTable(gesamt_table)

  # Plots
  # km pro Jahr
  output$plot_km_pro_jahr <- renderPlotly({
    fig <- plot_ly(
      x = km_pro_jahr$Jahr,
      y = km_pro_jahr$sum_km,
      name = "km_pro_jahr",
      type = "bar"
    )
    fig
  })
  # km pro Jahr/Monat
  output$plot_km_pro_jahr_monat <- renderPlotly({
    fig <- plot_ly(
      x = km_pro_jahr_monat$Jahr_Monat,
      y = km_pro_jahr_monat$sum_km,
      name = "km_pro_jahr",
      type = "bar"
    )
    fig
  })
  # Durchschnitts km pro Jahr/Monat
  output$plot_mean_km_pro_jahr_monat <- renderPlotly({
    fig <- plot_ly(
      x = km_pro_jahr_monat$Jahr_Monat,
      y = km_pro_jahr_monat$mean_km,
      name = "km_pro_jahr",
      type = "bar"
    )
    fig
  })
  # km pro Schuhe
  output$plot_km_pro_schuhe <- renderPlotly({
    fig <- plot_ly(
      x = km_pro_schuhe$Schuhe,
      y = km_pro_schuhe$sum_km,
      name = "km_pro_schuhe",
      type = "bar"
    )
    fig
  })

  # Zeitverlauf  
  output$plot1 <- renderPlotly({
    if (input$index == "Index") {
      fig <- plot_ly(data, x = ~Index, y = ~km, type = 'scatter', mode = 'lines')
      fig
    } else {
      fig <- plot_ly(data, x = ~Datum, y = ~km, type = 'scatter', mode = 'lines')
      fig
    }
  })
  
  output$plot2 <- renderPlotly({
    if (input$index == "Index") {
      fig <- plot_ly(data, x = ~Index, y = ~Kilometer_pro_h, type = 'scatter', mode = 'lines')
      fig
    } else {
      fig <- plot_ly(data, x = ~Datum, y = ~Kilometer_pro_h, type = 'scatter', mode = 'lines')
      fig
    }
  })
}
