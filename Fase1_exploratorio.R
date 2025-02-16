#instlaicon delibrerias

library(readr)
library(tidyverse)

##Ejercicio 1. Fase exploraoria de la base de datos.

#cargue de base de datos.
Appendix_1_Database <- read_csv("bd/Appendix 1 - Database.csv")
View(Appendix_1_Database)

#buscando valores vacíos en columnas.
nas_por_columna <- colSums (is.na(Appendix_1_Database))
print("Valores NA por columna:")
print(nas_por_columna)

#base de datos limpia
Appendix_1_Database_limpio <- Appendix_1_Database %>%
  filter(!is.na(CustomerID))
nrow(Appendix_1_Database_limpio)

#busqueda de valores unicos en columna StockCode.

productos_duplicados <- Appendix_1_Database_limpio %>%
  group_by(Description) %>%
     summarise(
           cantidad_codigos = n_distinct(StockCode),
           codigos = paste(unique(StockCode), collapse = ", ")
       ) %>%
     filter(cantidad_codigos > 1) %>%
     arrange(desc(cantidad_codigos))
 # Mostrar los resultados
  print("Productos con múltiples códigos de stock:")
  print(productos_duplicados)
  
#posibles valores ceros y negativos en las columnas Quantity y UnitPrice.
  
valores_inusuales <- Appendix_1_Database_limpio %>%
    summarise(
      # Cantidades
      cantidad_negativa = sum(Quantity < 0),
      cantidad_cero = sum(Quantity == 0),
      # Precios
      precio_negativo = sum(UnitPrice < 0),
      precio_cero = sum(UnitPrice == 0)
    )
  
print("Valores inusuales en cantidades y precios:")
print(valores_inusuales)
  
  
# Resumen estadistico pra detecicon valores inusuales(outliers)

summary(Appendix_1_Database_limpio)


# Nota: aclaro que para los puntos 2,3,y 4 trabajo con la base de datos Appendix_1_Database

Appendix_1_Database <- Appendix_1_Database %>%
  mutate(preciototal= Quantity * UnitPrice)
head(Appendix_1_Database)


# Agrupamos por factura y sumamos
totales_factura <- Appendix_1_Database %>%
  mutate(preciototal= Quantity * UnitPrice)%>%
  group_by(InvoiceNo) %>%
  summarise(Total_Factura = sum(preciototal))
head(totales_factura)

#factura con el precio más alto.

factura_mas_alta <- Appendix_1_Database %>%
  mutate(TotalPrice = Quantity * UnitPrice) %>%
  group_by(InvoiceNo) %>%
  summarise(Total_Factura = sum(TotalPrice)) %>%
  arrange (desc(Total_Factura) ) %>%
  slice(1)
head (factura_mas_alta)

# Ejercicio 2 , graficos.
## Preparacion de los datos.
totales_por_factura <- Appendix_1_Database %>%
  mutate(TotalPrecio = Quantity * UnitPrice) %>%
  group_by(InvoiceNo) %>%
  summarise(Total_Factura = sum(TotalPrecio)) %>%
  
# Filtrar valores extremos o negativos si los hubiera
  filter(Total_Factura > 0)
ggplot(totales_por_factura, aes(x = Total_Factura)) +
  geom_histogram(fill = "blue", color = "black", bins = 50) +
  xlim(0, quantile(totales_por_factura$Total_Factura, 0.95)) +
  labs(
    title = "Distribución del Precio Total por Factura",
    x = "Precio Total",
    y = "Frecuencia"
  ) +
  theme_minimal()
  