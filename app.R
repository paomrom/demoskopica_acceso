
# ========== Seguridad y dependencias ==========
library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(bslib)
library(mxmaps)
library(tidyr)
library(lubridate)
library(tibble)
library(genero)
library(officer)
library(rmarkdown)
library(fuzzyjoin)
library(stringr)
library(shinyauthr)
library(shinymanager)
library(reactable)

registro<-read_excel("Registro de Datos Personal de Campo 1.xlsx")
evaluaciones<-read_excel("evaluaciones.xlsx")


  
  # Funciones auxiliares
  normalizar_nombres <- function(nombre) {
    str_to_lower(nombre) %>%
      str_trim() %>%
      str_replace_all("[áàä]", "a") %>%
      str_replace_all("[éèë]", "e") %>%
      str_replace_all("[íìï]", "i") %>%
      str_replace_all("[óòö]", "o") %>%
      str_replace_all("[úùü]", "u") %>%
      str_replace_all("[^a-z ]", "")
  }
  
  capitalizar_texto <- function(texto) {
    if (is.na(texto) | texto == "") return(NA)
    palabras_excluidas <- c("de", "del", "la", "los", "las", "y", "o", "en", "por", "con", "a", "el")
    palabras <- str_split(str_to_lower(texto), " ")[[1]]
    palabras <- sapply(palabras, function(palabra) {
      if (palabra %in% palabras_excluidas) return(palabra)
      else return(str_to_title(palabra))
    })
    paste(palabras, collapse = " ")
  }
  
  
  # ==================== Limpieza inicial ====================
  
  # Normalizar nombres de carreras
  registro <- registro %>%
    mutate(`Nombre de la carrera` = normalizar_nombres(`Nombre de la carrera`))
  
  # Clasificación usando grepl()
  registro <- registro %>%
    mutate(
      Nombre_carrera_limpia = case_when(
        grepl("administracion|ventas|empresa|mercadotecnia|contaduria|negocios|finanzas|comercio", `Nombre de la carrera`, ignore.case = T) ~ "Administración y Negocios",
        grepl("derecho|abogado|abogada|sociologia|trabajo social|filosofia|historia|geografia|geo|Estudios politicos y gobierno|gobierno|ciencias politicas|antropologia|economia|estudios politicos y gobienro", `Nombre de la carrera`, ignore.case = T) ~ "Ciencias Sociales y Humanidades",
        grepl("medicina|enfermeria|odontologia|psicologia|nutricion|salud|cirujano dentista|Puericultura", `Nombre de la carrera`, ignore.case = T) ~ "Ciencias de la Salud",
        grepl("quimica|biologia|fisica|matematicas|medio ambiente|ciencias naturales|bioquimica|farmacia", `Nombre de la carrera`,ignore.case = T) ~ "Ciencias Exactas y Naturales",
        grepl("mecatronica|electronica|industrial|tecnologias|informatica|comunicaciones|sistemas|ingenieria|ing|electromecanica", `Nombre de la carrera`,ignore.case = T) ~ "Ingeniería y Tecnología",
        grepl("arquitectura|urbanismo|planificacion urbana", `Nombre de la carrera`, ignore.case = T) ~ "Arquitectura y Urbanismo",
        grepl("docencia|pedagogia|educacion|formacion docente", `Nombre de la carrera`, ignore.case = T) ~ "Educación y Pedagogía",
        grepl("turismo|hoteleria|gastronomia|cocina", `Nombre de la carrera`, ignore.case = T) ~ "Turismo y Gastronomía",
        grepl("relaciones internacionales|comunicacion|letras|escritura creativa|periodismo|marketing|publicidad", `Nombre de la carrera`, ignore.case = T) ~ "Comunicación y Medios",
        grepl("arte|diseño|grafico|moda|fotografia", `Nombre de la carrera`, ignore.case = T) ~ "Arte y Diseño",
        grepl("criminologia|psicologia organizacional|servicio social|trabajo social|criminalistica", `Nombre de la carrera`, ignore.case = T) ~ "Ciencias Sociales y Jurídicas",
        grepl("ciencias aplicadas|tecnologia|bioquimica|farmacia|biofisica|biotecnologia|alimentos", `Nombre de la carrera`, ignore.case = T) ~ "Ciencias Aplicadas",
        grepl("Técnica|Tecnico|superior", `Nombre de la carrera`, ignore.case = T) ~ "Otras Carreras Técnicas",
        TRUE ~ "Sin especificar" 
      )
    )
  
  
  
  
  
  registro <- registro %>%
    rename(`Calle (domicilio)` = matches("Calle.*domicilio")) %>%
    mutate(
      Nombre_completo = paste(`Nombre(s)`, Apellidos),
      Nombre_normalizado = normalizar_nombres(Nombre_completo),
      Municipio_limpio = str_to_title(`Municipio (domicilio)`),
      Estado_limpio = str_to_title(`Entidad federativa de nacimiento`),
      Edad = year(`Hora de finalización`) - year(`Fecha de nacimiento`),
      CURP = toupper(CURP),
      RFC = toupper(RFC),
      Nombre_limpio = sapply(Nombre_completo, capitalizar_texto),
      Domicilio_limpio = sapply(`Calle (domicilio)`, capitalizar_texto)
    )
  
  # Convertir a mayúsculas
  registro$CURP <- toupper(registro$CURP)
  registro$RFC <- toupper(registro$RFC)
  
  registro <- registro %>%
    mutate(Municipio_limpio = case_when(
      Municipio_limpio == "Colotlan" ~ "Colotlán",
      Municipio_limpio == "Tlajomulco De Zúñiga" ~ "Tlajomulco de Zúñiga",
      TRUE ~ Municipio_limpio  # Mantiene los valores que no coinciden
    ))
  
  
  # ==================== Estimación de género ====================
  # 1. Extraer primer nombre
  registro <- registro %>%
    mutate(nombre_simple = word(Nombre_limpio, 1))
  
  registro <- registro %>%
    mutate(genero = genero(nombre_simple))
  
  registro <- registro %>%
    mutate(genero = case_when(
      genero == "female" ~ "Mujer",
      genero == "male" ~ "Hombre",
      TRUE ~ genero  # Mantiene los valores que no coinciden
    ))

  # 2. Corrección manual para nombres no clasificados automáticamente
  nombres_mujer <- c("Mayela", "Aymara", "Yoselin", "Ma.", "Eldaa", "Vianey", "Arandza", 
                     "Aneliz", "Fátima", "Renatta", "Kemberly", "Chelsy", "Kaxandra", 
                     "Mareli", "Anahi", "Eneidi", "Nahomy", "Nalleli")
  
  nombres_hombre <- c("Rito", "Jefte")
  
  registro <- registro %>%
    mutate(genero = case_when(
      nombre_simple %in% nombres_mujer ~ "Mujer",
      nombre_simple %in% nombres_hombre ~ "Hombre",
      TRUE ~ genero  # Mantener lo que ya está
    ))
  
    
  registro <- registro %>%
    mutate(
      # Año desde fecha de nacimiento
      anio_nacimiento_fecha = suppressWarnings(if_else(
        !is.na(`Fecha de nacimiento`), 
        year(`Fecha de nacimiento`), 
        NA_integer_
      )),
      
      edad_desde_fecha = 2024 - anio_nacimiento_fecha,
      
      # Extraer año desde RFC (posición 5 y 6)
      rfc_anio_texto = substr(RFC, 5, 6),
      anio_nacimiento_rfc = suppressWarnings(as.numeric(paste0("19", rfc_anio_texto))),
      
      # Edad desde RFC
      edad_desde_rfc = 2024 - anio_nacimiento_rfc,
      
      # Lógica final: usa edad de fecha si es válida (>12 años), si no usa RFC
      Edad = case_when(
        !is.na(edad_desde_fecha) & edad_desde_fecha > 12 ~ edad_desde_fecha,
        !is.na(edad_desde_rfc) ~ edad_desde_rfc,
        TRUE ~ NA_real_
      )
    )
  
  
  
  
  # ==================== Base final ====================
  registro_limpia <- registro %>%
    select(Nombre_limpio, Edad, genero, Estado_limpio,`¿Estudias actualmente?`, `Escolaridad (último grado de estudios)`,`Área de formación o conocimiento`,
           Nombre_carrera_limpia, Domicilio_limpio, Municipio_limpio, `Código Postal (domicilio)`,
           `Número de teléfono celular`, `¿Por cuál(es) medio(s) te enteraste de la convocatoria?`, `Disponibilidad de tiempo para participar en encuestas`,
           `Preferencia de actividades`, `¿Tienes experiencia en levantamiento de encuestas?`,`Menciona qué tipo de experiencia en levantamiento de encuestas tienes`,
           `¿Tienes disponibilidad para viajar?`,`¿Cuentas con licencia de conducir vigente?` , `Tipo de licencia`, `¿Tienes experiencia de manejo en...?`, `¿Qué tipo de vehículo sabes manejar?`)
  
  data("df_mxmunicipio_2020")
  
  library(fuzzyjoin)
  library(dplyr)
  library(stringr)
  
  # Crear columna "Nombre_normalizado" en ambas bases
  normalizar_nombre <- function(nombre) {
    nombre %>%
      str_to_lower() %>%
      str_replace_all("[áàä]", "a") %>%
      str_replace_all("[éèë]", "e") %>%
      str_replace_all("[íìï]", "i") %>%
      str_replace_all("[óòö]", "o") %>%
      str_replace_all("[úùü]", "u") %>%
      str_replace_all("[^a-z ]", "") %>%
      str_trim()
  }
  
  registro_limpia <- registro_limpia %>%
    mutate(Nombre_normalizado = normalizar_nombre(Nombre_limpio))
  
  evaluaciones <- evaluaciones %>%
    mutate(Nombre_normalizado = normalizar_nombre(`Nombre de encuestador`))
  
  # Join difuso
  datos_combinados <- stringdist_inner_join(
    evaluaciones, 
    registro_limpia, 
    by = "Nombre_normalizado", 
    method = "jw",    # Jaro-Winkler
    max_dist = 0.1    # Puedes afinar este valor
  )
  
  # ========== UI ==========
  # UI sin carga de datos, sólo estructura visual y autenticación
  ui_raw <- fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    title = "Panel Demoskópica",
    tags$head(
      # 🔹 Script de animación
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/countup.js/2.0.7/countUp.min.js"),
      tags$script(HTML("
      Shiny.addCustomMessageHandler('countup', function(message) {
        var el = document.getElementById(message.id);
        if (el) {
          var countUp = new CountUp(message.id, message.value);
          if (!countUp.error) {
            countUp.start();
          }
        }
      });
    ")),
      
      # 🔹 Estilos combinados
      tags$style(HTML("
      .navbar-default {
        background-color: #e3521e !important;
        border-color: #e3521e !important;
      }
      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: white !important;
      }
      .navbar-default .navbar-nav > li > a:hover {
        color: #f4f4f4 !important;
      }
      .well {
        padding: 10px;
        margin-bottom: 10px;
        background-color: transparent;
        border: none;
        box-shadow: none;
      }
      .well h3 {
        font-size: 24px;  
        border-bottom: 2px solid #e3521e;  
        padding-bottom: 5px;
      }
      .well h2 {
        font-size: 30px;  
        font-weight: bold;
      }

    
  .ficha-perfil {
    max-width: 800px;
    margin: 30px auto;
    padding: 30px;
    border-radius: 12px;
    background-color: #fff;
    box-shadow: 0 0 20px rgba(0,0,0,0.05);
    border-left: 6px solid #e3521e;
    font-family: 'Helvetica Neue', sans-serif;
  }

  .ficha-perfil h2 {
    text-align: center;
    color: #e3521e;
    font-size: 28px;
    margin-bottom: 10px;
  }

  .foto-icono {
    display: block;
    text-align: center;
    font-size: 70px;
    color: #e3521e;
    margin-bottom: 10px;
  }

  .bloque {
    margin-top: 25px;
    padding-top: 15px;
    border-top: 1px solid #e4e4e4;
  }

  .bloque h4 {
    color: #333;
    font-size: 18px;
    margin-bottom: 10px;
    border-bottom: 2px solid #e3521e;
    display: inline-block;
    padding-bottom: 5px;
  }

  .bloque p {
    font-size: 16px;
    margin: 6px 0;
  }

  .dato {
    font-weight: 600;
    color: #000;
  }
    "))
    ),
    navbarPage(
      title = tags$div(
        style = "position: absolute; top: 0px; right: 20px; z-index: 9999;",
        tags$img(
          src = "https://www.demoskopicamexico.com.mx/wp-content/uploads/2019/06/Logo-transparente-3.png",
          height = "60px"
        )
      ),
      tabPanel("Dashboard",
               fluidPage(
                 fluidRow(
                   column(3, wellPanel(
                     h3(icon("users", style = "color:#e3521e;"), " Total encuestadores"),
                     h2(textOutput("total_encuestadores"))
                   )),
                   column(3, wellPanel(
                     h3(icon("user-clock", style = "color:#e3521e;"), " Edad promedio"),
                     h2(textOutput("edad_promedio"))
                   )),
                   column(3, wellPanel(
                     h3(icon("female", style = "color:#e3521e;"), " Porcentaje de mujeres"),
                     h2(textOutput("porcentaje_mujeres"))
                   )),
                   column(3, wellPanel(
                     h3(icon("male", style = "color:#e3521e;"), " Porcentaje de hombres"),
                     h2(textOutput("porcentaje_hombres"))
                   )
                   )
                 ),
                 br(),
                 
                 # 🔹 Filtros en horizontal
                 fluidRow(
                   column(3,
                          sliderInput("edad", "Rango de Edad:",
                                      min = min(registro_limpia$Edad, na.rm = TRUE),
                                      max = max(registro_limpia$Edad, na.rm = TRUE),
                                      value = range(registro_limpia$Edad, na.rm = TRUE))
                   ),
                   column(3,
                          selectInput("Municipio", "Municipio:",
                                      choices = c("Todos", sort(unique(registro_limpia$Municipio_limpio))),
                                      selected = "Todos")
                   ),
                   column(3,
                          selectInput("Escolaridad", "Escolaridad:",
                                      choices = c("Todas", sort(unique(registro_limpia$`Escolaridad (último grado de estudios)`))),
                                      selected = "Todas")
                   ),
                   column(3,
                          selectInput("Experiencia", "¿Tiene experiencia en encuestas?",
                                      choices = c("Todas", sort(unique(registro_limpia$`¿Tienes experiencia en levantamiento de encuestas?`))),
                                      selected = "Todas")
                   )
                 ),
                 
                 br(),
                 
                 # 🔹 Visualizaciones
                 fluidRow(
                   column(12,
                          leafletOutput("mapa", height = 300)
                   )
                 ),
                 br(),
                 fluidRow(
                   column(12,
                          plotlyOutput("gg_escolaridad")
                   ),
                   column(12,
                          h3(icon("file", style = "color:#e3521e;"), " Datos filtrados"),
                          br(),
                          DTOutput("tabla_encuestadores")
                   )
                 )
               )
      ),
      tabPanel("Perfil",
               fluidPage(
                 h2("Ficha del perfil seleccionado", style = "color:#e3521e;"),
                 selectInput(
                   "persona",
                   "Selecciona un nombre con evaluación:",
                   choices = unique(datos_combinados$Nombre_limpio[!is.na(datos_combinados$Puntualidad)])
                 )
                 ,
                 br(),
                 uiOutput("ficha_perfil"),
                 br(),
                 fluidRow(
                   column(6, downloadButton("descargar_word", "📥 Descargar Word", class = "btn btn-success")),
                   column(6, downloadButton("descargar_pdf", "📥 Descargar PDF", class = "btn btn-danger"))
                 )
               )
      ),
      tabPanel("Evaluaciones",
               fluidPage(
                 h2("Resumen de Evaluaciones", style = "color:#e3521e"),
                 
                 fluidRow(
                   column(4,
                          selectInput("filtro_proyecto_eval", "Proyecto:",
                                      choices = c("Todos", unique(na.omit(evaluaciones$`Proyecto`))),
                                      selected = "Todos")
                   ),
                   column(4,
                          selectInput("filtro_encuestador", "Nombre de encuestador:",
                                      choices = c("Todos", unique(na.omit(evaluaciones$`Nombre de encuestador`))),
                                      selected = "Todos")
                   ),
                   column(4,
                          selectInput("filtro_coordinacion_eval", "Coordinación:",
                                      choices = c("Todas", unique(na.omit(evaluaciones$Coordinación))),
                                      selected = "Todas")
                   )
                 ),
                 
                 fluidRow(
                   column(3, plotlyOutput("gauge_puntualidad", height = "250px")),
                   column(3, plotlyOutput("gauge_habilidad", height = "250px")),
                   column(3, plotlyOutput("gauge_actitud", height = "250px")),
                   column(3, plotlyOutput("gauge_rigurosidad", height = "250px"))
                 ),
                 HTML('
<div style="margin-top: 10px;">
  <h4 style="color:#e3521e; text-align:center; margin-bottom: 10px;">Escala de Evaluación</h4>
  <div style="display: flex; flex-wrap: wrap; justify-content: center; gap: 30px;">

    <div style="text-align: center; min-width: 130px;">
      <div style="font-size: 26px;">🌟</div>
      <div style="color:#2e7d32; font-weight: bold;">4 – Excelente</div>
    </div>

    <div style="text-align: center; min-width: 130px;">
      <div style="font-size: 26px;">👍</div>
      <div style="color:#ef6c00; font-weight: bold;">3 – Buena</div>
    </div>

    <div style="text-align: center; min-width: 130px;">
      <div style="font-size: 26px;">👌</div>
      <div style="color:#fbc02d; font-weight: bold;">2 – Regular</div>
    </div>

    <div style="text-align: center; min-width: 130px;">
      <div style="font-size: 26px;">⚠️</div>
      <div style="color:#c62828; font-weight: bold;">1 – Mala</div>
    </div>

  </div>
</div>
')
                 
                 
                 
                 ,
                 
                 div(
                   style = "background-color:#fffaf0; 
           font-size:20px; font-weight:600; 
           padding:12px; margin:20px auto;
           text-align:center; width:80%; 
           color:##0a0a0a;",
                   textOutput("promedio_general")
                 )
                 
                 ,
                 
                 
                 h3("Semaforización de encuestadores", style = "color:#e3521e"),
                 DTOutput("tabla_semaforizada")
               )),
      tabPanel("Descarga masiva",
               fluidPage(
                 h2("Bases completas sin filtros", style = "color:#e3521e"),
                 tabsetPanel(
                   tabPanel("Registro limpio",
                            br(),
                            downloadButton("descargar_registro", "📥 Descargar registro limpio"),
                            br(), br(),
                            DTOutput("tabla_registro")
                   ),
                   tabPanel("Evaluaciones",
                            br(),
                            downloadButton("descargar_evaluaciones", "📥 Descargar evaluaciones"),
                            br(), br(),
                            DTOutput("tabla_evaluaciones")
                   )
                 )
               )
      )
      
    )
  )

  
  # Activar seguridad
  credentials <- data.frame(
    user = c("PVMR", "Demoskopica", "Usuario1"), 
    password = c("1234", "demoskopica*2025", "usuario*2025"), 
    admin = c(TRUE, FALSE, FALSE),
    comment = c("Admin principal", "Usuario 1", "Usuario 2"),
    stringsAsFactors = FALSE
  )
  
  ui <- secure_app(ui_raw, language = "es")
  

  
  # ========== SERVER ==========
  server <- function(input, output, session) {
    res_auth <- secure_server(
      check_credentials = check_credentials(credentials)
    )
    
     
  
    output$total_encuestadores <- renderText({
      nrow(datos_filtrados())
    })
    
    
    datos_filtrados <- reactive({
      df <- registro_limpia %>%
        filter(Edad >= input$edad[1], Edad <= input$edad[2])
      
      if (input$Municipio != "Todos") {
        df <- df %>% filter(Municipio_limpio == input$Municipio)
      }
      
      if (input$Escolaridad != "Todas") {
        df <- df %>% filter(`Escolaridad (último grado de estudios)` == input$Escolaridad)
      }
      
      if (input$Experiencia != "Todas") {
        df <- df %>% filter(`¿Tienes experiencia en levantamiento de encuestas?` == input$Experiencia)
      }
      
      df
    })
    
    
    
    # Porcentaje de mujeres reactivo
    output$porcentaje_mujeres <- renderText({
      df <- datos_filtrados()
      genero_tabla <- df %>%
        filter(!is.na(genero)) %>%
        count(genero) %>%
        mutate(pct = n / sum(n) * 100)
      
      mujeres_pct <- genero_tabla$pct[genero_tabla$genero == "Mujer"]
      
      if (length(mujeres_pct) == 0) {
        return("0%")
      } else {
        return(paste0(round(mujeres_pct, 0), "%"))
      }
    })
    
    
    # Porcentaje de hombres reactivo
    output$porcentaje_hombres <- renderText({
      df <- datos_filtrados()
      genero_tabla <- df %>%
        filter(!is.na(genero)) %>%
        count(genero) %>%
        mutate(pct = n / sum(n) * 100)
      
      hombres_pct <- genero_tabla$pct[genero_tabla$genero == "Hombre"]
      
      if (length(hombres_pct) == 0) {
        return("0%")
      } else {
        return(paste0(round(hombres_pct, 0), "%"))
      }
    })
    
    output$edad_promedio <- renderText({
      df <- datos_filtrados()
      promedio <- mean(df$Edad, na.rm = TRUE)
      
      if (is.nan(promedio) || is.na(promedio)) {
        return("N/A")
      } else {
        return(round(promedio, 0))
      }
    })
    
    
    

    
    datos_mapa_filtrado <- reactive({
      df_base <- datos_filtrados()
      if (nrow(df_base) == 0) return(NULL)
      
      df <- df_base %>%
        count(Estado_limpio, Municipio_limpio, name = "n") %>%
        mutate(porcentaje = n / sum(n) * 100)
      
      df_mapa <- merge(df_mxmunicipio_2020, df,
                       by.x = c("state_name", "municipio_name"),
                       by.y = c("Estado_limpio", "Municipio_limpio"),
                       all.x = FALSE)
      
      df_mapa$porcentaje[is.na(df_mapa$porcentaje)] <- 0
      df_mapa
    })
    
    output$mapa <- renderLeaflet({
      df <- datos_mapa_filtrado()
      
      if (is.null(df) || nrow(df) == 0) {
        return(leaflet() %>% addProviderTiles("CartoDB.Positron"))
      }
      
      paleta <- colorNumeric(c("yellow", "orange", "red"), domain = df$porcentaje)
      
      centro <- if (input$Municipio != "Todos" && nrow(df) == 1) {
        list(lng = df$long[1], lat = df$lat[1], zoom = 10)
      } else {
        list(lng = mean(df$long, na.rm = TRUE), lat = mean(df$lat, na.rm = TRUE), zoom = 6.5)
      }
      
      leaflet(df) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = centro$lng, lat = centro$lat, zoom = centro$zoom) %>%
        addCircleMarkers(
          lng = ~long, lat = ~lat,
          radius = ~sqrt(porcentaje) * 2,
          color = ~paleta(porcentaje),
          fillColor = ~paleta(porcentaje),
          fillOpacity = 0.6,
          popup = ~paste0("<strong>Municipio:</strong> ", municipio_name,
                          "<br><strong>Estado:</strong> ", state_name,
                          "<br><strong>Porcentaje:</strong> ", round(porcentaje, 2), "%"),
          clusterOptions = markerClusterOptions()
        )
    })
    
    output$gg_escolaridad <- renderPlotly({
      df <- datos_filtrados()
      escolaridad <- df %>%
        count(Escolaridad = `Escolaridad (último grado de estudios)`) %>%
        drop_na() %>%
        mutate(porcentaje = round(n / sum(n) * 100, 0))  # 🔹 Redondear
      
      gg <- ggplot(escolaridad, aes(x = str_wrap(Escolaridad,5), y = porcentaje, fill = porcentaje)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(porcentaje, "%")),   # 🔹 Mostrar % en etiquetas
                  vjust = -0.3, size = 4) +
        scale_fill_gradient(low = "#ffb86c", high = "#e3521e") +
        theme_minimal() +
        labs(x = "Escolaridad", y = "Porcentaje") +
        theme(axis.text.x = element_text(angle = 0, hjust = 1))  # 🔄 Mejora lectura
      
      ggplotly(gg)
    })
    
    
    
    output$tabla_encuestadores <- renderDT({
      df <- datos_filtrados() %>%
        select("Nombre completo"= Nombre_limpio, 
               Edad, 
               "Sexo"= genero, 
               "Municipio" = Municipio_limpio, 
               "Escolaridad"=`Escolaridad (último grado de estudios)`, 
               "Formación"=`Nombre_carrera_limpia`,
               "Experiencia"= `¿Tienes experiencia en levantamiento de encuestas?`,
               "Disponibilidad a viajar"=`¿Tienes disponibilidad para viajar?`,
               "Licenciacia de conducir"=`¿Cuentas con licencia de conducir vigente?` , 
               `Tipo de licencia`)
      
      datatable(df,
                extensions = 'Buttons',
                options = list(
                  scrollY=T,
                  paging=F,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'excel', 'print')
                ),
                class = 'display nowrap'
      )
    })
    
    
    output$ficha_perfil <- renderUI({
      req(input$persona)
      
      datos <- datos_combinados %>%
        filter(Nombre_limpio == input$persona) %>%
        slice(1)
      
      traducir_evaluacion <- function(x) {
        dplyr::recode(x,
                      "E" = "Excelente", "B" = "Buena", "R" = "Regular", "M" = "Mala", .default = x
        )
      }
      
      datos <- datos %>%
        mutate(
          Puntualidad = traducir_evaluacion(Puntualidad),
          `Habilidad para abordar` = traducir_evaluacion(`Habilidad para abordar`),
          `Actitud de cooperación con sus compañeros` = traducir_evaluacion(`Actitud de cooperación con sus compañeros`),
          `Rigurosidad metodológica` = traducir_evaluacion(`Rigurosidad metodológica`)
        )
      
      icono <- ifelse(datos$genero == "Mujer",
                      '<div class="foto-icono"><i class="fas fa-female"></i></div>',
                      '<div class="foto-icono"><i class="fas fa-male"></i></div>')
      
      HTML(paste0(
        '<div class="ficha-perfil">',
        icono,
        '<h2>', datos$Nombre_limpio, '</h2>',
        
        # 🔹 Datos generales
        '<div class="bloque">',
        '<h4><i class="fas fa-id-card"></i> Datos generales</h4>',
        '<p>Edad: <span class="dato">', datos$Edad, ' años</span></p>',
        '<p>Género: <span class="dato">', datos$genero, '</span></p>',
        '<p>Municipio: <span class="dato">', datos$Municipio_limpio, '</span></p>',
        '<p>Celular: <span class="dato">', datos$`Número de teléfono celular`, '</span></p>',
        '<p>Correo: <span class="dato">', datos$`Correo electrónico`, '</span></p>',
        '<p>Domicilio: <span class="dato">', datos$Domicilio_limpio, '</span></p>',
        '<p>Código Postal: <span class="dato">', datos$`Código Postal (domicilio)`, '</span></p>',
        '</div>',
        
        # 🔹 Formación
        '<div class="bloque">',
        '<h4><i class="fas fa-graduation-cap"></i> Formación académica</h4>',
        '<p>Escolaridad: <span class="dato">', datos$`Escolaridad (último grado de estudios)`, '</span></p>',
        '<p>Área de formación: <span class="dato">', datos$`Área de formación o conocimiento`, '</span></p>',
        '</div>',
        
        # 🔹 Experiencia
        '<div class="bloque">',
        '<h4><i class="fas fa-briefcase"></i> Experiencia y disponibilidad</h4>',
        '<p>Medio por el que se enteró: <span class="dato">', datos$`¿Por cuál(es) medio(s) te enteraste de la convocatoria?`, '</span></p>',
        '<p>Disponibilidad de tiempo: <span class="dato">', datos$`Disponibilidad de tiempo para participar en encuestas`, '</span></p>',
        '<p>Preferencia de actividades: <span class="dato">', datos$`Preferencia de actividades`, '</span></p>',
        '<p>¿Tiene experiencia en encuestas?: <span class="dato">', datos$`¿Tienes experiencia en levantamiento de encuestas?`, '</span></p>',
        '<p>Tipo de experiencia: <span class="dato">', datos$`Menciona qué tipo de experiencia en levantamiento de encuestas tienes`, '</span></p>',
        '<p>¿Disponibilidad para viajar?: <span class="dato">', datos$`¿Tienes disponibilidad para viajar?`, '</span></p>',
        '<p>¿Cuenta con licencia?: <span class="dato">', datos$`¿Cuentas con licencia de conducir vigente?`, '</span></p>',
        '<p>Tipo de licencia: <span class="dato">', datos$`Tipo de licencia`, '</span></p>',
        '<p>Experiencia de manejo: <span class="dato">', datos$`¿Tienes experiencia de manejo en...?`, '</span></p>',
        '<p>Tipo de vehículo: <span class="dato">', datos$`¿Qué tipo de vehículo sabes manejar?`, '</span></p>',
        '<p>Contactos recomendados: <span class="dato">', datos$`¿Tienes contactos con experiencia en campo, responsabilidad y profesionalidad que te gustaría invitar a participar con Demoskópica?`, '</span></p>',
        '</div>',
        
        # 🔹 Evaluación
        '<div class="bloque">',
        '<h4><i class="fas fa-star-half-alt"></i> Evaluación</h4>',
        '<p><strong>Nombre de encuestador:</strong> ', datos$`Nombre de encuestador`, '</p>',
        '<p><strong>Coordinación:</strong> ', datos$Coordinación, '</p>',
        '<p><strong>Hora de llegada:</strong> ', datos$`Hora de llegada`, '</p>',
        '<p><strong>Duración del cuestionario:</strong> ', datos$`Duración para concluir sus cuestionarios en cada punto`, '</p>',
        '<p><strong>Puntualidad:</strong> ', datos$Puntualidad, '</p>',
        '<p><strong>Habilidad para abordar:</strong> ', datos$`Habilidad para abordar`, '</p>',
        '<p><strong>Actitud con compañeros:</strong> ', datos$`Actitud de cooperación con sus compañeros`, '</p>',
        '<p><strong>Rigurosidad metodológica:</strong> ', datos$`Rigurosidad metodológica`, '</p>',
        '<p><strong>Fortalezas:</strong> ', datos$Fortalezas, '</p>',
        '<p><strong>Debilidades:</strong> ', datos$Debilidades, '</p>',
        '<p><strong>Observaciones:</strong> ', datos$Observaciones, '</p>',
        '</div>',
        
        '</div>'
      ))
    })
    
    output$promedio_general <- renderText({
      df <- evaluaciones_filtradas()
      
      df_prom <- df %>%
        mutate(
          P = recode(as.character(Puntualidad), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
          H = recode(as.character(`Habilidad para abordar`), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
          A = recode(as.character(`Actitud de cooperación con sus compañeros`), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
          R = recode(as.character(`Rigurosidad metodológica`), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_)
        ) %>%
        mutate(promedio = rowMeans(across(c(P, H, A, R)), na.rm = TRUE)) %>%
        summarise(promedio_general = round(mean(promedio, na.rm = TRUE), 2)) %>%
        pull(promedio_general)
      
      if (is.na(df_prom)) {
        "Promedio general: No hay datos disponibles."
      } else {
        paste0("Promedio general del proyecto seleccionado: ", df_prom)
      }
    })
    
    
    evaluaciones_filtradas <- reactive({
      data <- evaluaciones
      
      if (input$filtro_proyecto_eval != "Todos") {
        data <- data %>% filter(`Proyecto` == input$filtro_proyecto_eval)
      }
      if (input$filtro_encuestador != "Todos") {
        data <- data %>% filter(`Nombre de encuestador` == input$filtro_encuestador)
      }
      if (input$filtro_coordinacion_eval != "Todas") {
        data <- data %>% filter(Coordinación == input$filtro_coordinacion_eval)
      }
      
      data
    })
    
    crear_gauge <- function(valor, titulo) {
      plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = valor,
        type = "indicator",
        mode = "gauge+number",
        gauge = list(
          axis = list(range = list(0, 4)),
          bar = list(color = "#e3521e"),
          steps = list(
            list(range = c(0, 1), color = "#ff4d4d"),
            list(range = c(1, 2), color = "#ff944d"),
            list(range = c(2, 3), color = "#ffe34d"),
            list(range = c(3, 4), color = "#a4e352")
          )
        ),
        title = list(text = titulo)
      )
    }
    
    output$gauge_puntualidad <- renderPlotly({
      promedio <- evaluaciones_filtradas() %>%
        mutate(Puntualidad = recode(Puntualidad, "E"=4, "B"=3, "R"=2, "M"=1)) %>%
        summarise(valor = mean(Puntualidad, na.rm = TRUE)) %>%
        pull(valor)
      crear_gauge(promedio, "Puntualidad")
    })
    
    output$gauge_habilidad <- renderPlotly({
      promedio <- evaluaciones_filtradas() %>%
        mutate(Habilidad = recode(as.character(`Habilidad para abordar`),
                                  "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_)) %>%
        summarise(valor = mean(Habilidad, na.rm = TRUE)) %>%
        pull(valor)
      crear_gauge(promedio, "Habilidad")
    })
    
    output$gauge_actitud <- renderPlotly({
      promedio <- evaluaciones_filtradas() %>%
        mutate(Actitud = recode(as.character(`Actitud de cooperación con sus compañeros`),
                                "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_)) %>%
        summarise(valor = mean(Actitud, na.rm = TRUE)) %>%
        pull(valor)
      crear_gauge(promedio, "Actitud")
    })
    
    output$gauge_rigurosidad <- renderPlotly({
      promedio <- evaluaciones_filtradas() %>%
        mutate(Rigurosidad = recode(as.character(`Rigurosidad metodológica`),
                                    "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_)) %>%
        summarise(valor = mean(Rigurosidad, na.rm = TRUE)) %>%
        pull(valor)
      crear_gauge(promedio, "Rigurosidad")
    })
    
    
    output$tabla_semaforizada <- renderDT({
  df <- evaluaciones_filtradas()

  df_valores <- df %>%
    mutate(
      Puntualidad = recode(as.character(Puntualidad), "E" = 4, "B" = 3, "R" = 2, "M" = 1, .default = NA_real_),
      `Habilidad para abordar` = recode(as.character(`Habilidad para abordar`), "E" = 4, "B" = 3, "R" = 2, "M" = 1, .default = NA_real_),
      `Actitud de cooperación con sus compañeros` = recode(as.character(`Actitud de cooperación con sus compañeros`), "E" = 4, "B" = 3, "R" = 2, "M" = 1, .default = NA_real_),
      `Rigurosidad metodológica` = recode(as.character(`Rigurosidad metodológica`), "E" = 4, "B" = 3, "R" = 2, "M" = 1, .default = NA_real_)
    ) %>%
    mutate(
      Promedio = round(rowMeans(across(c(Puntualidad, `Habilidad para abordar`, `Actitud de cooperación con sus compañeros`, `Rigurosidad metodológica`)), na.rm = TRUE), 2)
    ) %>%
    arrange(desc(Promedio)) %>%
    select(
      Encuestador = `Nombre de encuestador`,
      Coordinación,
      Proyecto = `Proyecto`,
      Puntualidad,
      `Habilidad para abordar`,
      `Actitud de cooperación con sus compañeros`,
      `Rigurosidad metodológica`,
      Promedio
    )

  datatable(df_valores, extensions = 'Buttons',
            options = list(
              scrollY = TRUE,
              scrollX = TRUE,
              paging = FALSE,
              dom = 'Bfrtip',
              buttons = c('copy', 'excel', 'print')
            ),
            class = 'cell-border stripe') %>%
    formatStyle(
      "Promedio",
      backgroundColor = styleInterval(
        c(2.5, 3.5), c('#ff4d4d', '#ffe34d', '#a4e352')
      )
    )
})
    output$tabla_registro <- renderDT({
      df_mostrado <- registro_limpia %>%
        select(
          `Nombre completo` = Nombre_limpio,
          `Edad` = Edad,
          `Género` = genero,
          `Municipio` = Municipio_limpio,
          `Escolaridad` = `Escolaridad (último grado de estudios)`,
          `Carrera` = Nombre_carrera_limpia,
          `Experiencia en encuestas` = `¿Tienes experiencia en levantamiento de encuestas?`,
          `Disponibilidad para viajar` = `¿Tienes disponibilidad para viajar?`,
          `Licencia de conducir` = `¿Cuentas con licencia de conducir vigente?`,
          `Tipo de licencia` = `Tipo de licencia`
        )
      
      datatable(
        df_mostrado,
        extensions = c('Buttons', 'FixedHeader', 'ColReorder', 'Scroller'),
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'print'),
          fixedHeader = TRUE,
          scrollX = TRUE,
          scrollY = 400,
          colReorder = TRUE,
          scroller = TRUE,
          deferRender = TRUE
        ),
        class = 'stripe hover nowrap'
      )
    })
    # Mostrar tabla: Evaluaciones con promedio
    output$tabla_evaluaciones <- renderDT({
      datos_eval <- evaluaciones %>%
        mutate(
          P = recode(as.character(Puntualidad), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
          H = recode(as.character(`Habilidad para abordar`), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
          A = recode(as.character(`Actitud de cooperación con sus compañeros`), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
          R = recode(as.character(`Rigurosidad metodológica`), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
          Promedio = round(rowMeans(across(c(P, H, A, R)), na.rm = TRUE), 2)
        ) %>%
        select(-P, -H, -A, -R)
      
      datatable(datos_eval, options = list(scrollX = TRUE))
    })
    
    # Descargar evaluaciones con promedio
    output$descargar_evaluaciones <- downloadHandler(
      filename = function() {
        paste0("evaluaciones_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        datos_eval <- evaluaciones %>%
          mutate(
            P = recode(as.character(Puntualidad), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
            H = recode(as.character(`Habilidad para abordar`), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
            A = recode(as.character(`Actitud de cooperación con sus compañeros`), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
            R = recode(as.character(`Rigurosidad metodológica`), "E"=4, "B"=3, "R"=2, "M"=1, .default = NA_real_),
            Promedio = round(rowMeans(across(c(P, H, A, R)), na.rm = TRUE), 2)
          ) %>%
          select(-P, -H, -A, -R)
        
        writexl::write_xlsx(datos_eval, path = file)
      }
    )
   
  }


# ✅ Siempre retornar shinyApp
shinyApp(ui, server)

