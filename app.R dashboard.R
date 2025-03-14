library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)
library(lubridate)
library(readxl)

Rentalcar_tabla <- read_excel("RentalCar G2-G9.xlsx")

Rentalcar_tabla <- Rentalcar_tabla %>% mutate(Pickupd = as.POSIXct(Pickupd))
Rentalcar_tabla <- Rentalcar_tabla %>% mutate(Fecha_Pickup = as.Date(Pickupd))
Rentalcar_tabla <- Rentalcar_tabla %>% mutate(Hora_Pickup = format(Pickupd, "%H:%M:%S"))

Rentalcar_tabla <- Rentalcar_tabla %>% mutate(Returnd = as.POSIXct(Returnd))
Rentalcar_tabla <- Rentalcar_tabla %>% mutate(Fecha_Returnd = as.Date(Returnd))
Rentalcar_tabla <- Rentalcar_tabla %>% mutate(Hora_Returnd = format(Returnd, "%H:%M:%S"))

Rentalcar_tabla <- Rentalcar_tabla %>% mutate(Booked = as.POSIXct(Booked))
Rentalcar_tabla <- Rentalcar_tabla %>% mutate(Fecha_Booked = as.Date(Booked))
Rentalcar_tabla <- Rentalcar_tabla %>% mutate(Hora_Booked = format(Booked, "%H:%M:%S"))

#tabla agrupada de frecuencias por fecha
frecuencia_por_tiempo_Status <- Rentalcar_tabla %>%
  group_by(periodo = floor_date(Fecha_Booked, "month"), Status_) %>%
  summarise(frecuencia = n())

library(RColorBrewer)

# Tabla agrupada por frecuencia de Fuente (Source)
frecuencia_Source <- Rentalcar_tabla %>%
  group_by( Source) %>%
  summarise(frecuencia = n())

colores_azules_degradados <- c(
  "#E1F5FE",  # Azul muy claro
  "#B3E5FC",
  "#81D4FA",
  "#4FC3F7",
  "#29B6F6",
  "#03A9F4",
  "#039BE5",
  "#0288D1",
  "#0277BD",
  "#01579B",  # Azul oscuro
  "#0D47A1",  # Azul muy oscuro
  "#1A237E",  # Azul oscuro con un toque violeta
  "#1976D2",  # Azul intermedio
  "#1565C0"   # Azul aún más profundo
)

frecuencia_Source <- frecuencia_Source[order(frecuencia_Source$frecuencia), ]

tabla_1 <- summary(Rentalcar_tabla[c("TotalRate", "AvgRateDay", "TotalExtras","TotalBill")])

frecuencia_Aeropuerto <- Rentalcar_tabla %>%
  group_by(LocOut) %>%
  summarise(frecuencia = n())

colores_azules_degradados <- c(
  "#E1F5FE",  # Azul muy claro
  "#B3E5FC",
  "#81D4FA",
  "#4FC3F7",
  "#29B6F6",
  "#03A9F4",
  "#039BE5",
  "#0288D1",
  "#0277BD",
  "#01579B",  # Azul oscuro
  "#0D47A1",  # Azul muy oscuro
  "#1A237E",  # Azul oscuro con un toque violeta
  "#1976D2",  # Azul intermedio
  "#1565C0",
  "#3A9BD3",
  "#008B8B",
  "#00FFFF",
  "#87CEEB",
  "#00BFFF"
  
  # Azul aún más profundo
)

frecuencia_Aeropuerto <- frecuencia_Aeropuerto[order(frecuencia_Aeropuerto$frecuencia), ]

# Tabla agrupada por frecuencia de Fuente (Source)
frecuencia_Class <- Rentalcar_tabla %>%
  group_by(Class) %>%
  summarise(frecuencia = n())

colores_azules_degradados <- c(
  "#E1F5FE",  # Azul muy claro
  "#B3E5FC",
  "#81D4FA",
  "#4FC3F7",
  "#29B6F6",
  "#03A9F4",
  "#039BE5",
  "#0288D1",
  "#0277BD",
  "#01579B",  # Azul oscuro
  "#0D47A1",  # Azul muy oscuro
  "#1A237E",  # Azul oscuro con un toque violeta
  "#1976D2",  # Azul intermedio
  "#1565C0",
  "#3A9BD3",
  "#008B8B",
  "#00FFFF",
  "#87CEEB",
  "#00BFFF"
  
  # Azul aún más profundo
)

frecuencia_Class <- frecuencia_Class[order(frecuencia_Class$frecuencia), ]

# DASHBOARD SHINY

# Define la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Análisis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "home", icon = icon("dashboard")),
      menuItem("Reservas por mes", tabName = "plot", icon = icon("chart-bar")),
      menuItem("Fuentes de reserva", tabName = "plot_1", icon = icon("chart-bar")),
      menuItem("Tarifas y renta", tabName = "plot_2", icon = icon("chart-bar")),
      menuItem("Aeropuertos", tabName = "plot_3", icon = icon("chart-bar")),
      menuItem("Clase de Autos", tabName = "plot_4", icon = icon("chart-bar")),
      menuItem("Clase por Aeropuerto", tabName = "plot_5", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("Bienvenido a tu Dashboard"), "“Rental Cars” es una empresa cuyo servicio que ofrece es el alquiler de automóviles durante un período corto de tiempo (ya sea que la persona necesite alquilar un auto para un viaje al no tener uno propio, o que su automóvil no se encuentre disponible por fallas técnicas que presente el mismo).En el presente dashboard se presentan los análisis de las operaciones de esta empresa, a continuación se presenta la tabla de datos proporcionada por la empresa para su análisis.",fluidRow(
                box(title = "Tabla de Datos Interactiva", status = "primary", solidHeader = TRUE, width = 12,
                    DTOutput("dataTable"))
              )),
      
      ####### PAGINA 1
      
      tabItem(tabName = "plot", h2("Frecuencias de reservas por mes"), "Se realizó un análisis para establecer la cantidad de reservas realizadas durante el año 2024, tomando en cuenta su estatus.",
              fluidRow(
                box(title = "Filtro de Tiempo", status = "primary", solidHeader = TRUE, width = 12 ,
                    dateRangeInput("fecha_range", "Seleccionar rango de fechas:",
                                   start = min(frecuencia_por_tiempo_Status$periodo),
                                   end = max(frecuencia_por_tiempo_Status$periodo),
                                   min = min(frecuencia_por_tiempo_Status$periodo),
                                   max = max(frecuencia_por_tiempo_Status$periodo),
                                   separator = " hasta ")
                ),
                
                box(title = "Frecuencias de reservas por mes", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("Freq_Status_T")), h3("Conclusiones"), "Podemos observar que la fecha en la que se rentaron más autos fue entre junio y agosto del año 2024, el mes de julio fue el más activo con más de 200 reservas de autos donde el cliente se presentó para retirar y usar el auto, alrededor de 200 reservas donde el cliente no se presentó para retirar el auto, un poco menos de 175 reservas canceladas, y alrededor de 125 reservas cuyo estatus es desconocido."
              )),
      
      ###### PAGINA 2
      
      tabItem(tabName = "plot_1", h2("Frecuencia de reserva de autos por fuente"), "SPara establecer cuales son las fuentes en las que se realizaron la mayoría de las reservas con la intención de
 posiblemente invertir en publicidad",
              fluidRow(
                box(title = "Filtro de Fuentes", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("fuenteSeleccionada", 
                                "Selecciona una fuente de reserva:",
                                choices = unique(Rentalcar_tabla$Source), # Asegúrate de que 'datos$fuente' sea la columna correspondiente en tu dataset
                                selected = unique(Rentalcar_tabla$Source)[1],
                                multiple = TRUE)
                ),
                
                box(title = "Frecuencia de las fuentes de renta", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("Freq_Fuentes")), h3("Conclusiones"), "Podemos observar que las 3 mayores funtes de reserva son: SABRE,BookingGroup y Cartrawler.

Se podría considerar invertir en publicidad en estos servicios para aumentar la demanda a través de estas fuentes.

También vemos que las 3 menores fuentes de reserva son: CRX, CALL FREE 0800 y Telefono Estación"
              )),
      
      ###### PAGINA 3
      
      tabItem(tabName = "plot_2", h2("Estadísticas descriptivas de las Tarifas"), "Podemos ver mediante las estadísticas descriptivas de las tarifas y cobros extras, lo que por lo general cubre las necesidades de los clientes dado por la media, la mediana y los cuartiles 1 y 3.",
              fluidRow(
                
                box(title = "Tabla descriptiva de las tarifas", status = "primary", solidHeader = TRUE, width = 6,
                    DTOutput("dataTable_2")),
                
                box(title = "Histograma de días de reserva", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("Hist_1")),
                h3("Conclusiones"), p("Podemos observar que la tarifa promedio por día ronda alrededor de entre 20 y 26 dólares, también vemos que en general la tarifa total pagada por los clientes se encuentra alrededor de 80 dólares, lo que nos indica que los autos se rentan generalmente entre 3 y 4 días. Entonces, con el fin de atraer más clientes se pueden realizar ofertas y paquetes asociados a estos días y publicitarlos dado que se sabe que por lo general esto es lo que más necesitan los clientes."),br(),
                
                p("Vemos en los pagos totales, que el máximo que se ha pagado por la renta de un auto ha sido de 2615,87 dólares, mientras que la renta mínima ha sido de 10,27 dólares."), br(), 
                p("Y los cuartiles nos indican que el 25% de las reservas se han pagado por un costo inferior a 79,74 dólares, mientras que el 75% de las reservas se han pagado por un costo inferior a 240 dólares, esto nos dice que el 50% de las rentas de autos se han realizado entre 80 y 240 dólares, esto es, aproximadamente entre 2 y 10 días de renta.")
              )),
      
      ####### PAGINA 4
      
      tabItem(tabName = "plot_3", h2("Frecuencia de rentas por Aeropuerto"), "Mediante el estudio de la cantidad de rentas por aeropuerto podemos observar cuales son los aeropuertos que generan más ganancias para la empresa y aquellos en los que hay que realizar otro enfoque en el servicio.",
              fluidRow(
                
                box(title = "Filtro de Fuentes", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("Aeropuertoseleccionado", 
                                "Selecciona varios Aeropuertos:",
                                choices = unique(Rentalcar_tabla$LocOut), # Asegúrate de que 'datos$fuente' sea la columna correspondiente en tu dataset
                                selected = unique(Rentalcar_tabla$LocOut)[1],
                                multiple = TRUE)
                ),
                
                box(title = "Frecuencia de rentas por Aeropuerto", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("Freq_Aero")),
                h3("Conclusiones"), p("Podemos observar que los aeropuertos donde se rentó mayor cantidad de autos son:"),br(),
                
                p("DFW: Aeropuerto Internacional de Dallas/Fort Worth (Texas, EE.UU.)"), br(),
                p("MCO: Aeropuerto Internacional de Orlando (Florida, EE.UU.)"), br(),
                p("IAH: Aeropuerto Intercontinental George Bush (Houston, Texas, EE.UU."), br(),
                p("YYZ: Aeropuerto Internacional de Toronto Pearson (Toronto, Canadá)"), br(),
                
                p("En estos aeropuertos se rentaron entre 125 y 240 autos, de estos 4 aeropuertos podemos observar que en los aeropuertos de Dallas y Orlando se rentaron más de 225 autos."), br(),
                
                p("Los aeropuertos en donde se rentó la menor cantidad de autos son los de:"), br(),
                
                p("SAW: Aeropuerto Sabiha Gökçen, ubicado en Estambul, Turquía."), br(),
                
                p("MIAC01: Este código no parece corresponder a un aeropuerto, sino a un código de sistema para Miami, a menudo utilizado para referirse a ciertos servicios o lugares en el área de Miami, pero no es un código de aeropuerto estándar. En el caso de MIA, sería el Aeropuerto Internacional de Miami."), br(),
                
                p("CUN: Aeropuerto Internacional de Cancún, ubicado en Cancún, México."), br(),
                
                p("STI: Aeropuerto Internacional Cibao, ubicado en Santiago de los Caballeros, República Dominicana.")
              )),
      
      ###### PAGINA 5
      
      tabItem(tabName = "plot_4", h2("Frecuencia de rentas por Clase de Auto"), "Mediante el estudio de la cantidad de rentas por tipo de auto podemos observar cuales son las clases de autos que generan más ganancias para la empresa y aquellos en los que no vale la pena hacer inversiones",
              fluidRow(
                
                box(title = "Filtro de clase de autos", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("Autoseleccionado", 
                                "Selecciona varios Aeropuertos:",
                                choices = unique(Rentalcar_tabla$Class), # Asegúrate de que 'datos$fuente' sea la columna correspondiente en tu dataset
                                selected = unique(Rentalcar_tabla$Class)[1],
                                multiple = TRUE)
                ),
                
                box(title = "Frecuencia de las clases de autos", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("Freq_Class")),
                h3("Conclusiones"), p("Podemos observar que de todos los modelos ofertados por la empresa solo 3 son los más utilizados, estos son:"),br(),
                p("EDAR: (Economy, Deluxe, Automatic, Regular)"), br(),
                p("IDAR: (Intermediate, Deluxe, Automatic, Regular)"), br(),
                p("CDAR: (Compact, Deluxe, Automatic, Regular)"), br(),
                p("Mientras que los menos rentados son:"), br(), 
                p("FVAR: Full-size Vehicle (All-Round), (Vehículo de tamaño completo, todo uso)"), br(),
                p("STAR: Standard SUV (SUV estándar)"), br(),
                p("OVAR: Oversize Van (Furgoneta de gran tamaño)")
              )),
      
      ####### PAGINA 6
      
      tabItem(tabName = "plot_5", h2("Tarifa promedio por Clase de Auto por aeropuerto"),
              fluidRow(
                
                box(title = "Filtro de clase de autos", status = "primary", solidHeader = TRUE, width = 12,
                    selectInput("Autosseleccionados", 
                                "Selecciona el tipo de vehículo:",
                                choices = unique(Rentalcar_tabla$Class), # Asegúrate de que 'datos$fuente' sea la columna correspondiente en tu dataset
                                selected = unique(Rentalcar_tabla$Class)[1],
                                multiple = FALSE)
                ),
                
                box(title = "Tarifa promedio por Clase de Auto por aeropuerto", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("Freq_Class_Aero")),
                h3("Conclusiones"), p("Podemos observar que el aeropuerto que ha tenido la tarifa más alta promedio por día para los autos de tipo EDAR es el Aeropuerto internacional de Estambul (IST), y la que ha tenido la tarifa promedio más baja ha sido es Aeropuerto internacional de Miami (MIA)."),br(),
                
                p("El aeropuerto que posee la mediana más alta de la tarifa promedio es el Aeropuerto Internacional de Vancouver (YVR), mientras que el que posee la mediana más baja es el de Miami (MIA)."),br(),
                
                p("Y el que tiene mayor variación en su tarifa promedio diaria es el de Estambul (IST)."),br(),
                
                p("Esto nos permite determinar que Aeropuertos generan los mayores beneficios, y cuales son aquellos que tienen la tarifa más baja y podrían mejorar en su servicio para producir mayores ganancias para la empresa.")
              ))
      
    )
  )
)

# Define el servidor
server <- function(input, output) {
  
  ClassFiltrados_2 <- reactive({
    subset(Rentalcar_tabla, Class %in% input$Autosseleccionados)
  })
  
  ClassFiltrados <- reactive({
    subset(frecuencia_Class, Class %in% input$Autoseleccionado)
  })
  
  AeroFiltrados <- reactive({
    subset(frecuencia_Aeropuerto, LocOut %in% input$Aeropuertoseleccionado)
  })
  
  datosFiltrados <- reactive({
    subset(frecuencia_Source, Source %in% input$fuenteSeleccionada)
  })
  
  datos_filtrados <- reactive({
    req(input$fecha_range)  # Asegura que haya un rango seleccionado
    frecuencia_por_tiempo_Status %>%
      filter(periodo >= input$fecha_range[1] & periodo <= input$fecha_range[2])
  })
  
  output$Freq_Status_T <- renderPlot({
    # Crear un gráfico de ejemplo
    ggplot(datos_filtrados(), aes(x = periodo, y = frecuencia, color = Status_)) +
      geom_line() +
      labs(title = "Serie de Tiempo de Frecuencia por status",
           x = "Periodo",
           y = "Frecuencia") +
      theme_minimal()
  })
  
  output$dataTable <- renderDT({
    datatable(Rentalcar_tabla, options = list(pageLength = 5, scrollX = TRUE), filter = 'top')
  })
  
  output$dataTable_2 <- renderDT({
    datatable(tabla_1, options = list(pageLength = 5, scrollX = TRUE), filter = 'top')
  })
  
  output$Freq_Fuentes <- renderPlot({
    # Crear un gráfico de ejemplo
    ggplot(datosFiltrados(), aes(x = reorder(Source, -frecuencia), y = frecuencia, fill = Source)) +
      geom_bar(stat = "identity") +
      labs(title = "Frecuencia de las Fuentes de renta",
           x = "Fuentes",
           y = "Frecuencia") +
      scale_fill_manual(values = colores_azules_degradados) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$Hist_1 <- renderPlot({
    # Crear un gráfico de ejemplo
    ggplot(Rentalcar_tabla, aes(x = RDays)) + 
      geom_histogram(binwidth = 2, fill = "blue", color = "black") + 
      labs(title = "Histograma del número de días de renta", x = "Número de días de renta", y = "Frecuencia")
  })
  
  output$Freq_Aero <- renderPlot({
    # Crear un gráfico de ejemplo
    ggplot(AeroFiltrados(), aes(x = reorder(LocOut, -frecuencia), y = frecuencia, fill = LocOut)) +
      geom_bar(stat = "identity") +
      labs(title = "Frecuencia de los Aeropuertos donde se rentó el Auto",
           x = "Aeropuertos",
           y = "Frecuencia") +
      scale_fill_manual(values = colores_azules_degradados) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$Freq_Class <- renderPlot({
    # Crear un gráfico de ejemplo
    ggplot(ClassFiltrados(), aes(x = reorder(Class, -frecuencia), y = frecuencia, fill = Class)) +
      geom_bar(stat = "identity") +
      labs(title = "Frecuencia de los tipos de carros que fueron rentados",
           x = "Tipo de carro",
           y = "Frecuencia") +
      scale_fill_manual(values = colores_azules_degradados) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$Freq_Class_Aero <- renderPlot({
    # Crear un gráfico de ejemplo
    ggplot(ClassFiltrados_2(), aes(x = LocOut, y = AvgRateDay)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      labs(title = "Variación de la Tarifa Diaria de la clase de auto por Aeropuerto",
           x = "Aeropuerto",
           y = "Tarifa Diaria") +
      theme_minimal()
  })
  
}

# Ejecuta la aplicación
shinyApp(ui, server)


