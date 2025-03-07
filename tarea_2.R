#cargo las librerias necesrias
library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
#Cargo base de datos.
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
  labs(title = "Distribución de Compras Totales", 
       x = "Monto total gastado", 
       y = "Número de clientes") +
  scale_x_log10() +
  theme_minimal()
  
# Mostrar el gráfico
print(p3)


#EJERCICIO 2
#Seleccione la letra B por ende hare el ejercicio con 4 clusters.

# Selecciono las variables para el clustering (RFM) justificado por el estudio hecho por Fader y Hardie.(2005),donde relaciona RFM con el valor del ciclo de vida del cliente (CLV).
datos_clustering <- datos_clientes %>%
  select(ultima_compra_dias, Frecuencia, ComprasTotal) %>%
  rename(Monetario = ComprasTotal)

# Aplico transformación logarítmica para manejar valores extremos
datos_clustering <- datos_clustering %>%
  mutate(
    Frecuencia = log1p(Frecuencia),
    Monetario = log1p(Monetario)
  )

# Escalo los datos para que todas las variables tengan el mismo peso
datos_escalados <- scale(datos_clustering)

# Aplico K-means con 4 clústeres
set.seed(123) # Para reproducibilidad
numero_clusters <- 4

# Ejecuto K-means
resultados_kmeans <- kmeans(datos_escalados, centers = numero_clusters, nstart = 25)

# Añado la asignación de clústeres a los datos originales
datos_clientes$Segmento <- as.factor(resultados_kmeans$cluster)

# Uso PCA para visualizar los clústeres en 2D
pca_result <- prcomp(datos_escalados)
pca_data <- as.data.frame(pca_result$x[, 1:2])
pca_data$Segmento <- as.factor(resultados_kmeans$cluster)

# Creo el gráfico con ggplot2
grafico_clusters <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Segmento)) +
  geom_point(alpha = 0.7) +
  stat_ellipse(type = "norm", level = 0.95, geom = "polygon", alpha = 0.1, aes(fill = Segmento)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Segmentación de Clientes en 4 Clústeres",
       subtitle = "Usando Días desde última compra, Frecuencia y Valor Monetario",
       x = "Componente Principal 1",
       y = "Componente Principal 2") +
  theme_minimal()

print(grafico_clusters)

# Analizo características de cada clúster
resumen_segmentos <- datos_clientes %>%
  group_by(Segmento) %>%
  summarise(
    NumeroClientes = n(),
    DiasUltimaCompra = mean(ultima_compra_dias),
    FrecuenciaMedia = mean(Frecuencia),
    GastoTotal = mean(ComprasTotal),
    ValorTicketPromedio = mean(CompraPromedio),
    CompraMinima = mean(CompraMinima)
  ) %>%
  mutate(
    PorcentajeClientes = NumeroClientes / sum(NumeroClientes) * 100
  )

# Muestro resumen de los clústeres
print(resumen_segmentos)


#EJERCICIO 3

# Creo el gráfico de dispersión con CustomerID y ComprasTotal
grafico_dispersion <- ggplot(datos_clientes, 
                             aes(x = as.numeric(as.factor(CustomerID)), 
                                 y = ComprasTotal, 
                                 color = Segmento)) +
  geom_point(alpha = 0.7) +
  scale_y_log10(labels = comma) +  # Uso escala logarítmica para mejor visualización
  scale_color_brewer(palette = "Set1") +  # Uso una paleta de colores distintiva
  labs(title = "Segmentación de Clientes por Gasto Total",
       subtitle = "Visualización de clusters(k=4)",
       x = "ID de Cliente (posición)",
       y = "Gasto Total (escala logarítmica)",
       color = "Segmento") +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),  # Oculto las etiquetas del eje x por ser muy numerosas
    axis.ticks.x = element_blank()
  )

# Muestro el gráfico
print(grafico_dispersion)


#EJERCICIO 4

# Selecciono solo las variables numéricas del dataset (excluyendo ID)
datos_numericos <- datos_clientes %>%
  select(ultima_compra_dias, Frecuencia, ComprasTotal, 
         CompraMinima, CompraPromedio) %>%
  # Aseguro que todas son numéricas
  mutate_all(as.numeric)

# Verifico la estructura de los datos
str(datos_numericos)

# CalculO la matriz de correlación de Spearman
matriz_correlacion <- cor(datos_numericos, method = "spearman", use = "complete.obs")

# Matriz de correlación con 3 decimales.
print(round(matriz_correlacion, 3))


# Uso distancia euclidiana ya que es la medida de distancia mas usanda entre las medidas de disimilitud ya aprovechando que los datos yalos escale anteriormente.
dist_matriz <- dist(datos_escalados, method = "euclidean")

# Aplico agrupamiento jerárquico con diferentes métodos de vinculación, seleccione este metodo por que se puede representar los puntos en un espacio euclidiano lo que lo vinvula con la metrica de distancia(euclidiana)
hc_ward <- hclust(dist_matriz, method = "ward.D2")

#Creo el dendrograma simple
plot(hc_ward, 
     main = "Dendrograma - Método Ward.D2", 
     sub = "", 
     xlab = "")

## como se nota el dendograma en este caso debido a la cantidad de datos queda sucio, por ende para mejorarlo 


# eliminare algunas etiquetas para limpiarlo, trabajaer con k 4 deacuerdo a mi eleccion
plot(hc_ward, main = "Dendrograma sin etiquetas", labels = FALSE)
rect.hclust(hc_ward, k = 4, border = "red")

# es dificil detectar los grupos de variables qe pueden estar realcionadas, por ende cambio estructura del dendograma para idetinficarlas.
#Convierto correlaciones en distancias

# Aplico clustering jerárquico a las VARIABLES
hc_variables <- hclust(dist_variables, method = "complete")

#Visualizo el dendrograma de VARIABLES
plot(hc_variables, 
     main = "Dendrograma de Variables Correlacionadas", 
     xlab = "")

# Añado líneas para identificar grupos
rect.hclust(hc_variables, k = 4, border = "blue")

# Identifico los grupos de variables
grupos_variables <- cutree(hc_variables, k = 3)
print("Agrupación de variables en clusters:")
print(grupos_variables)


#Ejercicio 5

# Verifico los países únicos y su frecuencia
tabla_frecuencia <- table(datos_clientes$Pais)
print("Frecuencia de países en el dataset:")
print(tabla_frecuencia)

# Codificación por orden alfabético
datos_clientes$Pais_numerico <- as.numeric(factor(datos_clientes$Pais))

# Muestro la correspondencia entre país y número
correspondencia <- data.frame(
  Pais = levels(factor(datos_clientes$Pais)),
  Codigo = 1:length(levels(factor(datos_clientes$Pais)))
)
print("Correspondencia entre países y códigos numéricos:")
print(correspondencia)

# Verifico que la conversión se haya realizado correctamente
print("Primeras filas con la nueva variable numérica:")
head(datos_clientes[, c("CustomerID", "Pais", "Pais_numerico")])


# pñara el segundo punto del ejercicio 5 tomo como parametro (3 meses = 90 días)
umbral_abandono <- 90


# Creo la variable Churn basada en el criterio de los 3 meses, si el cliente lleva mas de 3 meses se asignara 1,de lo contrario 0.
datos_clientes <- datos_clientes %>%
  mutate(Churn = ifelse(ultima_compra_dias > umbral_abandono, 1, 0))

# Convierto Churn a factor para que se considere categórica
datos_clientes$Churn <- as.factor(datos_clientes$Churn)

# Verifico cuántos clientes están en cada categoría
head((datos_clientes))


# Establezco una semilla para reproducibilidad
set.seed(123)

# Defino la proporción para el conjunto de entrenamiento (80%)
proporcion_entrenamiento <- 0.8

# Calculo cuántas observaciones irán al conjunto de entrenamiento
n_entrenamiento <- round(nrow(datos_clientes) * proporcion_entrenamiento)

# Genero índices aleatorios para seleccionar las observaciones de entrenamiento
indices_entrenamiento <- sample(1:nrow(datos_clientes), n_entrenamiento)

# Divido el dataset en entrenamiento y prueba
datos_entrenamiento <- datos_clientes[indices_entrenamiento, ]
datos_prueba <- datos_clientes[-indices_entrenamiento, ]


# Verifico las dimensiones de los conjuntos resultantes
cat("Dimensiones del conjunto de datos original:", dim(datos_clientes), "\n")
cat("Dimensiones del conjunto de entrenamiento:", dim(datos_entrenamiento), "\n")
cat("Dimensiones del conjunto de prueba:", dim(datos_prueba), "\n")
cat("Proporción de entrenamiento:", round(nrow(datos_entrenamiento)/nrow(datos_clientes)*100, 1), "%\n\n")

# Cargo la librería necesaria para SVM
library(e1071)

# Aseguro que la variable Churn sea factor
datos_train$Churn <- as.factor(datos_train$Churn)
datos_test$Churn <- as.factor(datos_test$Churn)

# Selecciono y escalo las variables predictoras
variables <- c("ultima_compra_dias", "Frecuencia", "ComprasTotal")

# Escalo las variables (importante para SVM)
datos_train_scale <- datos_train
datos_test_scale <- datos_test

for (var in variables) {
  # Obtengo media y desviación de entrenamiento
  media <- mean(datos_train[[var]], na.rm = TRUE)
  desv <- sd(datos_train[[var]], na.rm = TRUE)
  
  # Aplico el mismo escalado a ambos conjuntos
  datos_train_scale[[var]] <- (datos_train[[var]] - media) / desv
  datos_test_scale[[var]] <- (datos_test[[var]] - media) / desv
}

# Construyo el modelo SVM
modelo_svm <- svm(
  Churn ~ ultima_compra_dias + Frecuencia + ComprasTotal,
  data = datos_train_scale,
  kernel = "radial",     # Kernel radial (no lineal)
  cost = 1,              # Parámetro de regularización
  probability = TRUE     # Para obtener probabilidades
)

# Resumen del modelo
print(modelo_svm)

# Predicciones en conjunto de prueba
predicciones <- predict(modelo_svm, datos_test_scale)

# Evaluación simple: matriz de confusión y precisión
tabla <- table(Predicho = predicciones, Real = datos_test$Churn)
print(tabla)


