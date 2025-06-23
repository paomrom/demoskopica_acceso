# Demoskópica

**Demoskópica** es una plataforma interactiva desarrollada en R con Shiny para la gestión, visualización y análisis de datos provenientes de encuestas en campo. Está diseñada para facilitar el monitoreo de encuestadores, generar dashboards dinámicos, analizar resultados por entidad y descargar fichas de perfil personalizadas.

---

##  Características principales

-  **Acceso seguro con login**
-  **Carga de archivos Excel validada automáticamente**
-  **Limpieza y normalización de datos**
-  **Clasificación automática por carrera y género**
- 🗺 **Visualizaciones interactivas con `plotly` y `leaflet`**
- **Fichas de perfil con opción de descarga en Word y PDF**
-  **Dashboards y tablas dinámicas con filtros avanzados**
-  **Descarga masiva de datos procesados**

---

##  Estructura del proyecto
demoskopica/
├── www/ # Archivos estáticos (CSS, logos, íconos)
├── data/ # Archivos .xlsx o .rds de entrada
├── modules/ # Módulos Shiny reutilizables
├── R/ # Scripts con funciones auxiliares
├── ficha.Rmd # Plantilla de ficha individual
├── server.R # Lógica del servidor
├── ui.R # Interfaz de usuario
├── global.R # Carga inicial de librerías y datos
├── README.md # Documentación general
└── run.R # Script de ejecución


---

##  Requisitos

- R >= 4.1
- Paquetes requeridos:

```r
install.packages(c(
  "shiny", "shinydashboard", "tidyverse", "plotly", "leaflet", 
  "readxl", "DT", "rmarkdown", "stringr", "lubridate", "janitor",
  "shinyWidgets", "shinyalert", "shinyFeedback", "openxlsx"
))
```
source("run.R")
Desarrollado por el equipo de análisis y desarrollo de [Demoskópica].

Correo: contacto@demoskopica.org
Versión: 1.0.0 – Última actualización: junio 2025
