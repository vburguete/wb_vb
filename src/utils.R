# Librerías

# if (!require("forcats")) { 
#   devtools::install_version("forcats", version = "0.5.0", repos = "http://cran.us.r-project.org")
#   library(forcats) 
# }
# 
# if (!require("plotly")) { 
#   devtools::install_version("plotly", version = "4.9.0", repos = "http://cran.us.r-project.org")
#   library(plotly) 
# }
# 
# if (!require("ggplot2")) { 
#   devtools::install_version("ggplot2", version = "3.3.1", repos = "http://cran.us.r-project.org")
#   library(ggplot2) 
# }
# 
# if (!require("tidyverse")) { 
#   devtools::install_version("tidyverse", version = "1.3.0", repos = "http://cran.us.r-project.org")
#   library(tidyverse) 
# }
# if (!require("DT")) { 
#   devtools::install_version("DT", version = "0.8", repos = "http://cran.us.r-project.org")
#   library(DT) 
# }
# if (!require("shiny")) { 
#   devtools::install_version("shiny", version = "1.2.0", repos = "http://cran.us.r-project.org")
#   library(shiny) 
# }
# if (!require("shinydashboard")) { 
#   devtools::install_version("shinydashboard", version = "0.6.1", repos = "http://cran.us.r-project.org")
#   library(shinydashboard) 
# }
# if (!require("shinyWidgets")) { 
#   devtools::install_version("shinyWidgets", version = "0.4.3", repos = "http://cran.us.r-project.org")
#   library(shinyWidgets) 
# }
# if (!require("shinycssloaders")) { 
#   install.packages("shinycssloaders")
#   library(shinycssloaders) 
# }
# if (!require("shinydashboardPlus")) { 
#   install.packages("shinydashboardPlus")
#   library(shinydashboardPlus) 
# }



library(plotly) 
library(ggplot2) 
library(tidyverse) 
library(DT) 
library(shiny) 
library(shinydashboard) 
library(shinyWidgets) 
library(shinycssloaders) 
library(shinydashboardPlus) 


# Se leen rds generados para la app en informe_wb_vb.Rmd
poblacion_edad_sexo <- read_rds(file.path('.', 'data', 'internas', 'poblacion_edad_sexo.rds'))
poblacion_residencia <- read_rds(file.path('.', 'data', 'internas', 'poblacion_residencia.rds'))
poblacion_tasas <- read_rds(file.path('.', 'data', 'internas', 'poblacion_tasas.rds'))
