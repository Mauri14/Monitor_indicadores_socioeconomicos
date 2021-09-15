
##########
## ui.R ##
##########

#### Encabezado ####


encabezado <- dashboardHeader(title = "Trabajo final",
                              tags$li(
                                a(
                                  strong("Buscar ECH"),
                                  height = 40,
                                  href = "https://www.ine.gub.uy/encuesta-continua-de-hogares1",
                                  title = "",
                                  target = "_blank"
                                ),
                                class = "dropdown"
                              ))

#### Barra Lateral ####

barra_lateral <- dashboardSidebar(
  
  
  
  sidebarMenu(
    menuItem("Inicio", tabName = "inicio", icon = icon("home")),
    menuItem("Cargar ECH", tabName = "cargadatos", icon = icon("upload"),
             
             #Carga de datos      
             fileInput("datos", "Base de personas .sav", multiple = TRUE,
                       accept = c(".sav", ".RData")),
             fileInput("datosH", "Base de hogares .sav", multiple = TRUE,
                       accept = c(".sav", ".RData")),
             fileInput("datosGeo", "Estratos y UPM .sav", multiple = TRUE,
                       accept = c(".sav", ".RData")),
             selectInput("dis", "Información disponible", choices = c("pública", "estratos+UPM"))),
    
    menuItem("Indicadores", icon = icon("chart-line"),
             menuSubItem("Educación", tabName = "educ", icon = icon("mortar-board")),
             menuSubItem("Salud", tabName = "Salud", icon = icon("ambulance")),
             menuSubItem("Mercado laboral", tabName = "laboral", icon = icon("wrench")),
             menuSubItem("Ingresos y bienestar", tabName = "ingresos", icon = icon("dollar-sign")),
             menuSubItem("Tecnología y comunicación", tabName = "tec", icon = icon("laptop")),
             menuSubItem("Demografía", tabName = "demo", icon = icon("globe-americas")),
             menuSubItem("Viviendas y hogares", tabName = "hogares", icon = icon("home"))
             
    )
    
  )
)

#### Contenido ####

contenido <- dashboardBody(
  
  
  tabItems(
    tabItem(tabName = "inicio", h1("Monitor de indicadores socioeconómicos estimados a partir de la Encuesta Continua de Hogares"),
            fluidRow(
              tabBox(
                width = 12,
                tabPanel(
                  status= "primary",
                  title = "Resumen",
                  p("Esta aplicación fue realizada en el marco de un trabajo final de grado de la Licenciatura en Estadística de la Facultad de Ciencias Económicas y de Administración de la UDELAR.
                    En esta se trabaja con la estimación en dominios de indicadores socioeconómicos que se obtienen a partir de la Encuesta Continua de Hogares(ECH). El grupo de indicadores con los que se trabaja son calculados año a año por el Observatorio Territorio Uruguay (OTU) de la Oficina de Planeamiento y Presupuesto (OPP) y se encuentran disponibles en su página web.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size: 18px;"),
                  p(strong("Para el correcto funcionamiento de la app se deben seguir los siguientes pasos:", style= "font-size: 18px;")),
                  p("En primer lugar se deben cargar las bases de datos de la ECH con las opciones presentes en la barra lateral. En el primer espacio se debe cargar la base de personas, en el segundo la base de hogares y por último se debe cargar el archivo que contiene la información sobre estratos y unidades primarias de muestreo, en caso de contar con la disponibilidad del mismo. Si se cuenta con esta información, se debe seleccionar la opción estratos+UPM, de lo contrario se debe seleccionar la opción pública. Es importante remarcar que todas las bases de datos deben ser cargadas en formato .SAV.
                    En la esquina superior de la app se encuentra el link de acceso a la página web del Instituto Nacional de Estadística (INE), donde se encuentran disponibles las bases de datos de la encuesta.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size: 18px;"),
                  p("Una vez cargados los datos, se debe seleccionar la sección", em("indicadores"), " y dentro de esta el indicador que se desea visualizar, contando con distintos tipos de gráficos y con una tabla con los resultados de las estimaciones puntuales. En el caso del gráfico de mapa, en la mayoría de los indicadores, se debe seleccionar la categoría a visualizar.
                    
                    Además de las estimaciones puntuales para los indicadores, se encuentran los resultados de los intervalos de confianza al 95% para las estimaciones.", strong(" Cabe aclarar que, de no estar utilizando la información sobre los estratos y las unidades primarias de muestreo, no se debe tener en cuenta los resultados de los intervalos de confianza ya que son incorrectos."), " Esto se debe a que la información sobre el diseño muestral es fundamental para el cálculo de los errores estándar de las estimaciones y con ello de los intervalos de confianza. Todas las tablas pueden ser descargadas en formato csv con el botón de descarga que tienen abajo de cada una, mientras los gráficos se pueden descargar en formato png.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size: 18px;"),
                  p("En la siguiente pestaña de esta sección, se encuentra el listado de variables de la ECH utilizadas en la app. Estas variables son fundamentales para el correcto funcionamiento de la aplicación, por lo que en caso de utilizar los datos de la encuesta para un año distinto a 2019, se debe verificar que estas variables no hayan sufrido cambios. En caso de que alguna de las variables haya cambiado de nombre en la base, se debe modificar el código de la app, o en su defecto modificar el nombre de la variable en la base de datos.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px;font-size: 18px;"),
                  fluidRow(
                    
                    img(src = "udelar_logo.jpg"
                        , height = 200, width = 150),
                    box(
                      width = 4,
                      background = "green",
                      h2("Trabajo Final de Grado",style="text-align:center;"),
                      p("Licenciatura en Estadística",style="text-align:center;font-size: 22px;"),
                      p("Mauricio Pittamiglio",style="text-align:center;font-size: 20px;"),
                    ),
                    
                    img(src = "logo_fcea.png"
                        , height = 150, width = 250)
                    
                    
                  )),
                tabPanel(
                  status= "success",
                  title = "Listado de variables",
                  DTOutput("Variab")
                  
                )
              )
              
            )
            
    ),
    tabItem(tabName = "educ", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow( 
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre", "Indicador", choices= c("Población entre 25 y 65 años con estudios terciarios por sexo"="741",
                                                                                                                                                 "Tasa neta de asistencia de 3 a 5 años en educación preescolar por sexo"="751",
                                                                                                                                                 "Tasa neta de asistencia de 12 a 17 años en educación media por sexo" ="1807",
                                                                                                                                                 "Personas de 18 años o más que completaron segundo ciclo de educación media por sexo"="740", 
                                                                                                                                                 "Tasa neta de asistencia de 0 a 2 años en educación inicial por sexo"="748", 
                                                                                                                                                 "Adolescentes de 12 a 17 años que asisten a establecimientos educativos por sexo"="732",
                                                                                                                                                 "Tasa neta de asistencia de 6 a 11 años en educación escolar por sexo"="756", 
                                                                                                                                                 "Personas mayores de 15 años que completaron media básica general o técnica por sexo"="739", 
                                                                                                                                                 "Tasa de analfabetismo en población de 15 años o más por sexo"="689", 
                                                                                                                                                 "Jovenes de 14 a 24 años que no estudian ni trabajan por sexo"="690",
                                                                                                                                                 "Personas mayores a 25 años por máximo nivel educativo alcanzado"="696",
                                                                                                                                                 "Ocupados por máximo nivel educativo alcanzado"="725",
                                                                                                                                                 "Tasa de analfabetismo de la población de 15 años o más por tramos de edad"="1020",
                                                                                                                                                 "Promedio de años de educación de las personas de 25 años o más por sexo"="1380",
                                                                                                                                                 "Tasa bruta de asistencia de 6 a 11 años a educación primaria"="747",
                                                                                                                                                 "Tasa bruta de asistencia de 3 a 5 años a educación preescolar"="746",
                                                                                                                                                 "Tasa bruta de asistencia de 12 a 17 años a educación media"="1808"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , status = "primary",uiOutput("SeleccionEdu")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_edu",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_edu",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos1"), fluidRow(p(class = 'text-center', downloadButton('dd1', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfEdu"),
                             p(class = 'text-center', downloadButton('dinf1', 'Descargar'))
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupEdu"),
                             p(class = 'text-center', downloadButton('dsup1', 'Descargar'))
                           ),
                           tabPanel(
                             status= "success",
                             title = "Gráfico",
                             uiOutput("SeleccionEdu2"),
                             plotlyOutput("ic_edu", height = 740)
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "Salud", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(   
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre2", "Indicador", choices= c("Personas afiliadas a emergencias móviles por sexo"="501",
                                                                                                                                                  "Personas afiliadas a emergencias móviles por tramos de edad"="529",
                                                                                                                                                  "Personas por tipo de atención en salud"="517"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , status = "primary",uiOutput("SeleccionSalud")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_salud",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_salud",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos2"), fluidRow(p(class = 'text-center', downloadButton('dd2', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfSalud"),
                             p(class = 'text-center', downloadButton('dinf2', 'Descargar'))
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupSalud"),
                             p(class = 'text-center', downloadButton('dsup2', 'Descargar'))
                             
                           ),
                           tabPanel(
                             status= "success",
                             title = "Gráfico",
                             uiOutput("SeleccionSalud2"),
                             plotlyOutput("ic_salud", height = 740)
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "laboral", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre3", "Indicador", choices= c("Tasa de empleo por sexo"="533",
                                                                                                                                                  "Subempleo por sexo"="521",
                                                                                                                                                  "Informalidad por sexo"="526",
                                                                                                                                                  "Ocupados en establecimientos fuera del dpto por sexo"="618",
                                                                                                                                                  "Tasa de actividad por sexo"="608",
                                                                                                                                                  "Jovenes de 14 a 24 años que no estudian ni trabajan por sexo"="690",
                                                                                                                                                  "Tasa de desempleo por sexo"="502",
                                                                                                                                                  "Desocupados por última ocupación"="609",
                                                                                                                                                  "Ocupados por categoría de ocupación"="611",
                                                                                                                                                  "Tasa de desempleo por tramos de edad"="531",
                                                                                                                                                  "Tasa de empleo por tramos de edad"="534",
                                                                                                                                                  "Tasa de actividad por tramos de edad"="607",
                                                                                                                                                  "Población por condición de actividad por sexo"="610"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , status = "primary",uiOutput("SeleccionLab")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_lab",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_lab",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos3"), fluidRow(p(class = 'text-center', downloadButton('dd3', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfLab"),
                             p(class = 'text-center', downloadButton('dinf3', 'Descargar'))
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupLab"),
                             p(class = 'text-center', downloadButton('dsup3', 'Descargar'))
                             
                           ),
                           tabPanel(
                             status= "success",
                             title = "Gráfico",
                             uiOutput("SeleccionLab2"),
                             plotlyOutput("ic_lab", height = 740)
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "ingresos", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre4", "Indicador", choices= c("Informalidad por sexo"="526",
                                                                                                                                                  "Personas en hogares en situación de pobreza por sexo"="568",
                                                                                                                                                  "Jovenes entre 14 y 29 años en situación de pobreza por sexo"="1636",
                                                                                                                                                  "Tasa de desempleo por tramos de edad"="531",
                                                                                                                                                  "Tasa de empleo por tramos de edad"="534",
                                                                                                                                                  "Hogares con hacinamiento"="577",
                                                                                                                                                  "Hogares con al menos un auto o camioneta"="1929",
                                                                                                                                                  "Hogares en situación de pobreza"="553"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , status = "primary",uiOutput("SeleccionIng")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_ing",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_ing",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos4"), fluidRow(p(class = 'text-center', downloadButton('dd4', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfIng"),
                             p(class = 'text-center', downloadButton('dinf4', 'Descargar'))
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupIng"),
                             p(class = 'text-center', downloadButton('dsup4', 'Descargar'))
                             
                           ),
                           tabPanel(
                             status= "success",
                             title = "Gráfico",
                             uiOutput("SeleccionIng2"),
                             plotlyOutput("ic_ing", height = 740)
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "tec", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre5", "Indicador", choices= c("Personas que utilizan internet por sexo"="605",
                                                                                                                                                  "Personas con celular por sexo"="581",
                                                                                                                                                  "Utilización de computadora el último mes por sexo"="603",
                                                                                                                                                  "Frecuencia de utilización de internet por sexo"="582",
                                                                                                                                                  "Hogares con conexión a internet"="591",
                                                                                                                                                  "Hogares con computadora o laptop"="594",
                                                                                                                                                  "Hogares con al menos una computadora del Plan Ceibal"="584"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE , status = "primary",uiOutput("SeleccionTec")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_tec",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_tec",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800))),
              tabPanel("Tabla", DTOutput("datos5"), fluidRow(p(class = 'text-center', downloadButton('dd5', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfTec"),
                             p(class = 'text-center', downloadButton('dinf5', 'Descargar'))
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupTec"),
                             p(class = 'text-center', downloadButton('dsup5', 'Descargar'))
                             
                           ),
                           tabPanel(
                             status= "success",
                             title = "Gráfico",
                             uiOutput("SeleccionTec2"),
                             plotlyOutput("ic_tec", height = 740)
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "demo", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre6", "Indicador", choices= c("Población por lugar de residencia hace 5 años"="678",
                                                                                                                                                  "Población por lugar de nacimiento"="654",
                                                                                                                                                  "Población por lugar de residencia anterior"="655"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE ,status = "primary", uiOutput("SeleccionDemo")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_demo",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_demo",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800)
                       )),
              tabPanel("Tabla", DTOutput("datos6"), fluidRow(p(class = 'text-center', downloadButton('dd6', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfDemo"),
                             p(class = 'text-center', downloadButton('dinf6', 'Descargar'))
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupDemo"),
                             p(class = 'text-center', downloadButton('dsup6', 'Descargar'))
                             
                           ),
                           tabPanel(
                             status= "success",
                             title = "Gráfico",
                             uiOutput("SeleccionDemo2"),
                             plotlyOutput("ic_demo", height = 740)
                           )
                         )
                         
                       ))
            )
    ),
    tabItem(tabName = "hogares", 
            tabsetPanel(
              tabPanel("Visualización", 
                       fluidRow(    
                         box(title= "Indicador", width = 7, solidHeader = TRUE, status = "primary",selectInput("Nombre7", "Indicador", choices= c("Hogares con hacinamiento"="577",
                                                                                                                                                  "Hogares en situación de pobreza"="553",
                                                                                                                                                  "Hogares por tipo de evacuación del sistema sanitario"="764",
                                                                                                                                                  "Hogares por fuente de energía para iluminar"="782",
                                                                                                                                                  "Hogares por origen del agua para beber y cocinar"="783",
                                                                                                                                                  "Hogares por presencia de lugar para cocinar"="765",
                                                                                                                                                  "Hogares por presencia y uso de baño"="766",
                                                                                                                                                  "Hogares por tipo de relación con la vivienda"="774"))),
                         box(title = "Seleccionar categoría para el mapa", width = 5, solidHeader = TRUE ,status = "primary",uiOutput("SeleccionHog")),
                         box(title = "Mapa de Uruguay", status = "primary", solidHeader = TRUE, 
                             plotlyOutput("map_hog",height = 740), height = 800, width = 6), 
                         box(title= "Gráfico de barras", plotlyOutput("barras_hog",height = 740), status = "primary", solidHeader = TRUE,width = 6, height = 800)
                       )),
              tabPanel("Tabla", DTOutput("datos7"), fluidRow(p(class = 'text-center', downloadButton('dd7', 'Descargar')))),
              tabPanel("IC",
                       fluidRow(
                         tabBox(
                           width = 12,
                           tabPanel(
                             status= "primary",
                             title = "Inferior",
                             DTOutput("InfHog"),
                             p(class = 'text-center', downloadButton('dinf7', 'Descargar'))
                           ),
                           tabPanel(
                             status= "success",
                             title = "Superior",
                             DTOutput("SupHog"),
                             p(class = 'text-center', downloadButton('dsup7', 'Descargar'))
                             
                           ),
                           tabPanel(
                             status= "success",
                             title = "Gráfico",
                             uiOutput("SeleccionHog2"),
                             plotlyOutput("ic_hog", height = 740)
                           )
                         )
                         
                       ))
            )
    )
  )
)

## UI - Interfaz del Usuario ##

ui <- dashboardPage(skin = "green", encabezado, barra_lateral, contenido)

