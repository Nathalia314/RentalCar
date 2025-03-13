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
      
      