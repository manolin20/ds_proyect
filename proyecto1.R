

data1<-read.csv('proyect/archive/salary_data_cleaned.csv', 
                  header = T, sep = ',', stringsAsFactors = F,
                  check.names = F)
data3<- read.csv('proyect/archive/eda_data.csv', 
                header = T, sep = ',', stringsAsFactors = F,
                check.names = F)
data4<- read.csv('proyect/archive/glassdoor_jobs.csv', 
                header = T, sep = ',', stringsAsFactors = F,
                check.names = F)


# Cargar paquetes
#install.packages("skimr")
#install.packages(c("DataExplorer","corrplot"))

library(dplyr)
library(ggplot2)
library(skimr)
library(DataExplorer)
library(corrplot)

# 1. Resumen general del dataset
dim(data1)           # Dimensiones
str(data1)           # Estructura y tipos de datos
summary(data1)       # Resumen estadístico básico

# 2. Información más detallada
skim(data1)          # Estadísticas detalladas y valores faltantes

# 3. Valores faltantes
colSums(is.na(data1))          # Cantidad de NAs por columna
DataExplorer::plot_missing(data1)  # Visualización de NAs

# 4. Estadísticas descriptivas
numeric_cols <- sapply(data1, is.numeric)
data1_numeric <- data1[, numeric_cols]
summary(data1_numeric)

# 5. Distribuciones y gráficos
# Histograma para variables numéricas
for(col in names(data1_numeric)) {
  print(ggplot(data1, aes_string(x=col)) + 
          geom_histogram(fill="steelblue", color="black", bins=30) + 
          ggtitle(paste("Histograma de", col)))
}

# Barras para variables categóricas
categorical_cols <- sapply(data1, is.factor)
data1_categorical <- data1[, categorical_cols]
for(col in names(data1_categorical)) {
  print(ggplot(data1, aes_string(x=col)) +
          geom_bar(fill="salmon") +
          ggtitle(paste("Conteo de", col)))
}

# 6. Correlación entre variables numéricas
cor_matrix <- cor(data1_numeric, use="complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method="color", type="upper", tl.col="black")

# 7. Detección de outliers
boxplot(data1_numeric, main="Boxplot de variables numéricas", las=2, col="lightgreen")

# 8. Informe automático completo (opcional)
DataExplorer::create_report(data1, output_file = "reporte_data1.html")