server <- function(input, output) {

  library(stringr)
  source("extra_functions.R")
  
  #### Alte Daten ####
  data_old = read_ods(path = "/media/philipp/Elements/Sport/Laufzeiten.ods", sheet = 1, range = "A1:H2000")
  data_old = data_old[!is.na(data_old$Datum),]
  setDT(data_old)
  data_old$Index = 1:nrow(data_old)
  data_old$Datum = as.Date(data_old$Datum, format = "%d.%m.%Y")

  #### Garmin Daten ####
  activities = read.csv("/media/philipp/Elements/Sport/Running-App/data/Activities.csv")
  setDT(activities)
  activities = activities[order(Datum)]

  activities$Jahr = substr(as.character(activities$Datum), 1, 4)
  activities$Monat = substr(as.character(activities$Datum), 6, 7)
  activities$Tag = substr(as.character(activities$Datum), 9, 10)
  activities$Datum = as.Date(as.character(activities$Datum))
  activities = activities[Jahr >= 2022]
  #activities$Datum_alt = paste(activities$Tag, activities$Monat, activities$Jahr, sep = ".")
  #write.csv(activities, file = "/home/philipp/Downloads/Activities_adjusted.csv")
  head(activities)
  
  #### Mergen der Daten ####
  data_new = activities[, c("Datum", "Distanz", "Zeit", "Anstieg.gesamt")]
  data_new$Schuhe = sample(c("Brooks Adrenaline", "Sportiva Akasha"), nrow(data_new), replace = TRUE, prob = c(0.6, 0.4))
  data_new$Schuhe[(nrow(data_new) - 1): nrow(data_new)] = "Sportiva Akyra"
  standard_names = c("Datum", "km", "Zeit", "Höhenmeter", "Schuhe")
  colnames(data_new) = standard_names
  
  data_new$Zeit = as.numeric(as.difftime(as.character(data_new$Zeit), units = "secs"))
  
  data_new$km = as.character(data_new$km)
  data_new$km = str_replace_all(data_new$km, ",", ".")
  data_new$km = as.numeric(data_new$km)
  data_old$Zeit = as.numeric(data_old$Zeit)
  data_old$Datum = as.Date(paste0("20", data_old$Datum))
  
  data = rbind(data_old[, ..standard_names], data_new)
  
  # Kennzahlen berechnen
  data$Zeit_pro_km = data$Zeit/data$km
  data$Minute_pro_km = round((data$Zeit_pro_km) / 60, 2)
  data$Kilometer_pro_h = round(data$km/as.numeric(((data$Zeit)/3600)), 2)
  data$Min_pro_km = round(1/(data$Kilometer_pro_h/60), 2)
  
  data$Jahr = format(data$Datum, format="%Y")
  data$Monat = as.numeric(format(data$Datum, format="%m"))
  data$Index = 1:nrow(data)
  
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
    fig = fig %>% layout(yaxis = list(title = 'km'))
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
    fig = fig %>% layout(yaxis = list(title = 'km (Gesamt)'))
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
    fig = fig %>% layout(yaxis = list(title = 'km pro Lauf (Mittelwert)'))
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
    fig = fig %>% layout(yaxis = list(title = 'km'))
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
