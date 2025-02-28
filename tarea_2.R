
#Cargo base de datos.
library(readr)
datos_tarea2 <- read_csv("C:/Users/User/OneDrive/Desktop/UNAD SEGUNDO SEMESTRE/Predictive Analytics/TAREA 2/Appendix 1 - Database.csv")

#Muestro las primeras filas de la base de datos.
head(datos_tarea2)

#realizo la limpieza y preparacion de los datos.

datos_limpios <- datos_tarea2 %>%
  # Conviero la fecha a formato Date (usando formato mes/día/año)
  mutate(FechaFactura = mdy_hm(InvoiceDate)) %>%
  # Calculo el valor total de cada línea
  mutate(ValorTotal = Quantity * UnitPrice) %>%
  # Filtro registros con cantidades positivas y precios válidos
  filter(Quantity > 0, UnitPrice > 0, !is.na(CustomerID))

head(datos_limpios)

# Creo una tabla de facturas con su valor total
ordenes_por_factura <- datos_limpios %>%
  group_by(InvoiceNo, CustomerID, FechaFactura, Country) %>%
  summarise(
    ValorFactura = sum(ValorTotal),
    .groups = 'drop'
  )

head(ordenes_por_factura)


# Transformo a nivel de cliente
datos_clientes <- ordenes_por_factura %>%
  group_by(CustomerID) %>%
  summarise(
    # Fecha de compra más reciente
    FechaUltimaCompra = max(FechaFactura),
    
    # días desde la última compra hasta la fecha más reciente en los datos
    ultima_compra_dias = as.numeric(difftime(max(ordenes_por_factura$FechaFactura), 
                                   max(FechaFactura), 
                                   units = "days")),
    
    # Frecuencia (número de compras distintas)
    Frecuencia = n_distinct(InvoiceNo),
    
    # Total de compras (monto total gastado)
    ComprasTotal = sum(ValorFactura),
    
    # País desde donde se realizan las compras (el más frecuente)
    Pais = names(which.max(table(Country))),
    
    # Compra mínima
    CompraMinima = min(ValorFactura),
    
    # Compra promedio
    CompraPromedio = mean(ValorFactura)
  )

# Visualizo los primeros registros pra verificar que todo este bien..
head(datos_clientes)

# Estadísticas descriptivas
summary(datos_clientes)


# Histograma de Compras Totales
p3 <- ggplot(datos_clientes, aes(x = ComprasTotal)) +
  geom_histogram(fill = "salmon", bins = 30) +
  labs(title = "Distribución de Compras Totales", x = "Monto total gastado", y = "Número de clientes") +
  scale_x_log10() +
  theme_minimal()

