# LIBRERÍAS
library(shiny)
library(DT)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(here)
library(readxl)
library(ggmosaic)
library(paletteer)
library(leaflet)
library(leaflet.extras)
library(sf)








# DATOS
#here()
# Datos limpios sobre precio_m2 de casas
here()
#datos <- read_excel(here("Datos","datos_limpios.xlsx"))
#datos


#datos <- read_excel(here("Avances","datos_2.xlsx"))

datos <- read_excel("datos_limpios.xlsx")
datos

#datos <- read.csv(here("houses.csv"))
#datos


#DATOS PARA EL MAPA CON LATITUD Y LONGITUD
datos_mapa <- read.csv("datoss.csv")
shape <- st_read("sig_municipios.shp")
shape <- st_transform(shape, "+init=epsg:4326")

#Crear tabla para añadir la media de precio por metros cuadrados de los alquileres y la cantidad de alquileres de cada municipio

tabla<- datos_mapa %>%
  group_by(Municipio)%>%
  summarise(total=n()) 

tabla_2<- datos_mapa %>%
  group_by(Municipio)%>%
  select(precio_m2) %>%
  summarise(medias=mean(precio_m2))

shape <- shape %>%
  left_join(tabla, by=c("MUNICIPIO"= "Municipio"))#Le añadimos la tabla al shapefile, para poder trabajar mas adelante con estos datos y lograr visualizarlos en un mapa de calor

shape<- shape %>%
  left_join(tabla_2, by= c("MUNICIPIO"= "Municipio"))

 #PALETA DE COLORES PARA LOS MAPAS

nam_municipios <- unique(datos_mapa$Municipio)

color_municipio <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf')
pal<- colorFactor(color_municipio, domain= nam_municipios)

paletaseq<- c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#990000')
color_pal <- colorNumeric(paletaseq, domain= tabla$total)


#FILTRAR

colnames(datos)




datos <- datos %>%
  mutate(precio_m2_rec = case_when(
    precio_m2 >= 13 ~ "Precio m2 mayor a 13",
    precio_m2 < 13 ~  "Precio m2 menor a 13"))


datos <- datos |>
  filter(!(zona == "Montevideo"))



datos <- datos |>
  filter(precio_m2 < 100)


datos <- datos %>%
  mutate(zona = case_when(
    zona == "Carrasco Barrios con Seguridad" ~ "Carrasco",
    zona == "Carrasco Este" ~ "Carrasco",
    zona == "Golf" ~ "Punta Carretas",
    zona == "Parque Miramar" ~ "Carrasco",
    zona == "Peñarol Lavalleja" ~ "Peñarol",
    zona == "Pocitos Nuevo" ~ "Pocitos",
    zona == "Prado Nueva Savona" ~ "Prado",
    zona == "Puerto Buceo" ~ "Buceo",
    zona == "Villa Biarritz" ~ "Punta Carretas",
    TRUE ~ zona  # Mantiene los valores originales para los que no cumplen las condiciones anteriores
  ))


datos
# Cambiando la zona por municipios

datos <- datos %>% # Datos
  mutate(Municipio = case_when( # Casos donde a cada barrio le vamos a asociar un municipio
    
    zona %in% c("Cerro","La Teja","Paso de la Arena","Belvedere","Nuevo París","Prado","Paso Molino")  ~ "Municipio_A",
    
    zona %in% c("Cordón","Parque Rodó","Palermo","Barrio Sur","Ciudad Vieja","Centro","Aguada","La Comercial") ~ "Municipio_B",
    
    zona %in% c("Aguada", "Arroyo Seco", "Atahualpa", "Bella Vista", "Brazo Oriental", "Capurro", "Goes", "Jacinto Vera", "La Comercial", "Mercado Modelo", "Reducto", "Villa Muñoz") ~ "Municipio_C",
    
    zona %in% c("Tres Cruces", "La Blanqueada", "Parque Batlle", "Villa Dolores", "Buceo", "Pocitos", "Punta Carretas") ~ "Municipio_CH",
    
    zona %in% c("Piedras Blancas", "Villa Española", "Unión","Bolivar","Cerrito") ~ "Municipio_D",
    
    zona %in% c("Malvín Norte","Malvín", "Carrasco Norte", "Carrasco", "Punta Gorda", "Buceo", "La Blanqueada") ~ "Municipio_E",
    
    zona %in% c("Maroñas", "Flor de Maroñas", "Villa Española", "Ituzaingó", "Jardines del Hipódromo", "Piedras Blancas","Punta Rieles") ~ "Municipio_F",
    
    zona %in% c("Lezica", "Peñarol", "Nuevo París", "Sayago", "Conciliación", "Colón") ~ "Municipio_G",
    
    
    TRUE ~ zona  
  ))






# Colores personalizados para el gráfico de municipios
colores_personalizados <- c(
  "Municipio_A" = "red", "Municipio_B" = "black", "Municipio_C" = "green",
  "Municipio_CH" = "purple", "Municipio_D" = "orange", "Municipio_E" = "pink",
  "Municipio_F" = "brown", "Municipio_G" = "cyan"
)


# UI
ui <- fluidPage( # Definimos una página en blanco
  titlePanel("Análisis de Alquileres"), # Título
  sidebarLayout(      # Definimos la estructura
    sidebarPanel(     # Colocar los selectInput
      
      # OUTPUTS DE GASTOS COMUNES POR MUNICIPIO
      conditionalPanel(   # Panel Condicional
        condition = "input.tabselected == 4", # Le asociamos el valor 4
        selectInput("x_axis__GC_municipio", "Seleccione el eje X:",
                    # Distintos ejes X para la reactividad
                    choices = list( # Opciones a elegir
                      "Zona" = "zona",
                      "Tipo de Propiedad" = "tipo_prop",
                      "Condición" = "Condicion",
                      "Disposicion" = "disposicion"
                    )),
        
        # Este input va a estar asociado al gráfico de un municipio especifico
        selectInput("single_plot_GC", "Seleccione el Municipio:",
        # Distintos ejes X para la reactividad, donde estaremos eligiendo un municipio de todos los que hay
                    choices = unique(datos$Municipio),
                    selected = unique(datos$Municipio)[1]),
        
        # Distintos ejes X para la reactividad, donde estaremos eligiendo un barrio de todos los que hay
        selectInput("zona_plot_GC", "Seleccione la Zona:",
                    choices = NULL),  # Inicio por defecto
      
        actionButton("update_municipio_GC", "Actualizar Gráficos")
        # Boton para hacer aparecer los gráficos.
      ),
      
      
      # OUTPUT DE MOSAICOS
      conditionalPanel( # Panel Condicional
        condition = "input.tabselected == 5", # Le asociamos el valor 5
        selectInput("x_axis_bernoulli", "Seleccione el eje X para los gráficos de Bernoulli:",
            # Distintos ejes X para la reactividad              
                    choices = list( # Opciones a elegir
                      "Conexión Gas" = "conexion_gas",
                      "WiFi" = "WiFi",
                      "Duplex" = "Duplex",
                      "Pet" = "Pet",
                      "Pool" = "Pool",
                      "Gym" = "Gym"
                    )),
        actionButton("update_bernoulli", "Actualizar Gráficos")
        # Boton para hacer aparecer los gráficos.
      ),
      
      
      # OUTPUT DE MUNICIPIO POR PRECIO_M2
      conditionalPanel( # Panel Condicional
        condition = "input.tabselected == 6", # Le asociamos el valor 6
        selectInput("x_axis_municipio", "Seleccione el eje X:",
                    # Distintos ejes X para la reactividad        
                    choices = list( # Opciones a elegir
                      "Zona" = "zona",
                      "Tipo de Propiedad" = "tipo_prop",
                      "Condición" = "Condicion",
                      "Disposicion"="disposicion"
                    )),
        
        # Este input va a estar asociado al gráfico de un municipio especifico
        selectInput("single_plot", "Seleccione el Municipio:",
         # Distintos ejes X para la reactividad, donde estaremos eligiendo un municipio de todos los que hay
                    choices = unique(datos$Municipio),
                    selected = unique(datos$Municipio)[1]),
        
        
        # Este input va a estar asociado al gráfico de un barrio especifico
        selectInput("zona_plot", "Seleccione la Zona:",
                    choices = NULL),  #  Inicio por defecto
        
        actionButton("update_municipio", "Actualizar Gráficos")
        # Boton para hacer aparecer los gráficos.
      ), 
      
      
      
      # OUTPUT DE MAPA
      conditionalPanel( # Panel Condicional
        condition = "input.tabselected== 3", # Le asociamos el valor 3
        selectInput("change_mapa", "Seleccionar mapa:", # Distintos ejes X para la reactividad
                    choices= list( # Opciones a elegir
                      "Ubicacion de alquileres en Montevideo"= "mapa1",
                      "Cantidad de alquileres por Municipio"= "mapa2"))
      )
      
    ),
    mainPanel( # Panel para las pestañas donde se veran los gráficos
      tabsetPanel(id = "tabselected", # Pestañas posibles (cuatro)
                  tabPanel("Mapa", # Pestaña del mapa
                           h2("Gráfico de Mapa"), # Título
                           leafletOutput("mapa", width="100%", height= "600px"),
                           # Parámetros relacionados con la imagen del mapa
                           value = 3 # Valor Asociado
                  ),
                  
                  tabPanel("Municipio GC", # Pestaña de Gastos Comunes por Municipio
                           h2("Gastos Comunes por Municipio"), # Título
                           plotOutput("municipioPlot1_GC"),  # Municipios
                           plotOutput("singleMunicipioPlot_GC"), # Municipio Especifico
                           plotOutput("zonaPlot_GC"), # Barrio especifico
                           value = 4 # Valor asociado
                  ),
                  tabPanel("Mosaicos", #  Pestaña de Mosaicos
                           h2("Gráfico de Mosaicos"), # Título
                           plotOutput("bernoulliPlot"), # Gráfico
                           value = 5 # Valor asociado
                  ),
                  tabPanel("Municipio", # Pestaña de Municipios por precio_m2
                           h2("Gráficos por Municipio"), # Título
                           plotOutput("municipioPlot1"), # Municipios
                           plotOutput("singleMunicipioPlot"), # Municipio especifico
                           plotOutput("zonaPlot"),   # Barrio especifico
                           value = 6     # Valor asociado
                  )
                  
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # GRÁFICO DE GASTOS COMÚNES
  observeEvent(input$single_plot_GC, {
    selected_municipio <- input$single_plot_GC
    zonas_filtradas <- unique(datos$zona[datos$Municipio == selected_municipio])
    
    updateSelectInput(session, "zona_plot_GC",
                      choices = zonas_filtradas,
                      selected = zonas_filtradas[1])
  })
  
  
  observeEvent(input$update_municipio_GC, {
    output$municipioPlot1_GC <- renderPlot({
      x_axis <- input$x_axis__GC_municipio
      ggplot(datos, aes_string(x = x_axis, y = "Gastos_Comunes", fill = "Municipio")) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        facet_grid(. ~ Municipio, scales = "free_x") +
        labs(x = x_axis, y = "Gastos_Comunes", fill = "Municipio") +
        ggtitle("Gastos Comúnes por Municipio") +
        scale_fill_manual(values = colores_personalizados) +  # Aplicar colores personalizados
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11))
    })
    
    output$singleMunicipioPlot_GC <- renderPlot({
      x_axis <- input$x_axis__GC_municipio
      selected_municipio <- input$single_plot_GC
      datos_filtrados <- datos[datos$Municipio == selected_municipio, ]
      ggplot(datos_filtrados, aes_string(x = x_axis, y = "Gastos_Comunes", fill = "Municipio")) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        labs(x = x_axis, y = "Gastos Comúnes", fill = "Municipio") +
        ggtitle(paste("Gastos Comúnes en", selected_municipio)) +
        scale_fill_manual(values = colores_personalizados) +  # Aplicar colores personalizados
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 0, size = 11))
    })
    
    output$zonaPlot_GC <- renderPlot({
      x_axis <- input$x_axis__GC_municipio
      selected_zona <- input$zona_plot_GC
      datos_filtrados_zona <- datos[datos$zona == selected_zona, ]
      ggplot(datos_filtrados_zona, aes_string(x = x_axis, y = "Gastos_Comunes", fill = "Municipio")) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        labs(x = x_axis, y = "Gastos Comúnes", fill = "Municipio") +
        ggtitle(paste("Gastos Comúnes en la Zona", selected_zona)) +
        scale_fill_manual(values = colores_personalizados) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 0, size=11))
    })
  })
  
  # GRÁFICO DE BERNOULLIS
  observeEvent(input$update_bernoulli, {
    output$bernoulliPlot <- renderPlot({
      x_axis <- input$x_axis_bernoulli
      
      ggplot(data = datos) +
        geom_mosaic(aes_string(x = paste0("product(precio_m2_rec, ", x_axis, ")"), fill= "precio_m2_rec")) +
        labs(title='Comparación del precio por metro cuadrado y el número de piso', x = x_axis, y = "Precio por metro cuadrado")+
        theme(legend.position = "bottom")
    })
  })
  
  
  
  
  
  # GRÁFICO DE MUNICIPIOS
  observeEvent(input$single_plot, {
    selected_municipio <- input$single_plot
    zonas_filtradas <- unique(datos$zona[datos$Municipio == selected_municipio])
    
    updateSelectInput(session, "zona_plot",
                      choices = zonas_filtradas,
                      selected = zonas_filtradas[1])
  })
  
  observeEvent(input$update_municipio, {
    output$municipioPlot1 <- renderPlot({
      x_axis <- input$x_axis_municipio
      ggplot(datos, aes_string(x = x_axis, y = "precio_m2", fill = "Municipio")) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        facet_grid(. ~ Municipio, scales = "free_x") +
        labs(x = x_axis, y = "precio_m2", fill = "Municipio") +
        scale_fill_manual(values = colores_personalizados) +
        ggtitle("precio_m2 por Municipio") +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11))
    })
    
    output$singleMunicipioPlot <- renderPlot({
      x_axis <- input$x_axis_municipio
      selected_municipio <- input$single_plot
      datos_filtrados <- datos[datos$Municipio == selected_municipio, ]
      ggplot(datos_filtrados, aes_string(x = x_axis, y = "precio_m2", fill = "Municipio")) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        labs(x = x_axis, y = "precio_m2", fill = "Municipio") +
        scale_fill_manual(values = colores_personalizados) +
        ggtitle(paste("precio_m2 en", selected_municipio)) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 0, size = 11))
    })
    
    output$zonaPlot <- renderPlot({
      x_axis <- input$x_axis_municipio
      selected_zona <- input$zona_plot
      datos_filtrados_zona <- datos[datos$zona == selected_zona, ]
      ggplot(datos_filtrados_zona, aes_string(x = x_axis, y = "precio_m2", fill = "Municipio")) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        labs(x = x_axis, y = "precio_m2", fill = "Municipio") +
        scale_fill_manual(values = colores_personalizados) +
        ggtitle(paste("precio_m2 en la Zona", selected_zona)) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 0, size = 11))
    })
  })
  
  
  # GRÁFICO DEL MAPA
  output$mapa <- renderLeaflet({
    if(input$change_mapa== "mapa1"){
      leaflet() %>% addTiles() %>% 
        setView(-56.18816, -34.90328, 10)%>%
        addCircles( data= datos_mapa, lat =~as.numeric(oc_lat),
                    lng= ~as.numeric(oc_lng), color= ~pal(Municipio), label= ~Municipio,
                    fillOpacity = 0.5) %>%
        addPolygons(data= shape, 
                    fillColor = ~pal(MUNICIPIO),
                    weight = 0.5,
                    label= ~MUNICIPIO) %>%
        addLegend(data= datos_mapa, "bottomleft", pal= pal,
                  values= ~Municipio, title= "Municipios", opacity= 1,
                  group= "Leyenda"
        ) %>%
        addLayersControl(overlayGroups = c("Leyenda"),
                         options= layersControlOptions(collapsed=TRUE))
    }
    else if(input$change_mapa== "mapa2"){
      leaflet() %>% addTiles() %>% 
        setView(-56.18816, -34.90328, 10)%>%
        addPolygons(data= shape, 
                    fillColor = ~color_pal(total),
                    fillOpacity = 1,
                    weight = 0.5,
                    label= ~medias,
                    popup= ~MUNICIPIO) %>%
        addLegend(data=shape, "bottomleft", pal=color_pal,
                  values= ~total, title="Cantidad", opacity=1,
                  group="Leyenda") %>%
        addLayersControl(overlayGroups = c("Leyenda"),
                         options=layersControlOptions(collapsed=TRUE))
    }
    
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)
