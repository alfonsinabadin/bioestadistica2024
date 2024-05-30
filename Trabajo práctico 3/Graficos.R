library(readxl)
library(dplyr)
library(openxlsx)
library(kableExtra)
library(ggplot2)
par(family = "serif")

poblaciones2 <- read_excel("Trabajo práctico 3/Bases limpias/Poblaciones.xlsx")
poblaciones2 <- subset(poblaciones2,poblaciones2$PROVRES != 98)
poblaciones2 <- subset(poblaciones2,poblaciones2$PROVRES != 99)

edades <- c("0-14","15-29","30-44","45-59","60-74","75 y más")

# grafico edades x provincia

poblaciones2$Edad <- factor(poblaciones2$GRUPEDAD, levels = rev(edades))

poblaciones2 %>%
  ggplot(aes(x = factor(PROVRES), y = POBLACION/1000000, fill = Edad)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values = rev(c("#ffcdb2","#ffb4a2","#e5989b","#b5838d","#6d6875","#2b2d42"))) +
  labs(x = "Provincia",
       y = "Población (por millón)") +
  theme_grey() +  # Cambia "Times" por la fuente que uses en tu documento
  theme(
    text = element_text(family = "serif")
  )


# grafico metodo directo

metodo_directo <- read.delim2("Trabajo práctico 3/metodo_directo.txt")

codigos <- read_excel("Trabajo práctico 3/Bases limpias/Códigos de poblacion.xlsx")[,1:2]
colnames(codigos) <- c("Población","Provincia")

metodo_directo <- merge(metodo_directo,codigos,by ="Población",all.x = TRUE)

top10_md <- metodo_directo[order(-metodo_directo$Tasa.ajustada), ][1:10, ]
top10_md[1,7] <- "CABA"

ggplot(top10_md) +
  geom_point(aes(x = Provincia, y = LI), color = "#6d6875") +
  geom_point(aes(x = Provincia, y = LS), color = "#6d6875") +
  geom_segment(aes(x = Provincia, xend = Provincia, y = LI, yend = LS), color = "#6d6875") +
  geom_point(aes(x = Provincia, y = Tasa.ajustada), color = "#2b2d42", size = 3, shape = 15) +
  labs(y = "Tasa ajustada e intervalos de confianza del 95%", 
       x = "Provincia") +
  coord_flip() +
  theme_grey() +
  theme(
    text = element_text(family = "serif")
  )
  

# grafico metodo indirecto

metodo_indirecto <- read.delim2("Trabajo práctico 3/metodo_indirecto.txt")
metodo_indirecto <- merge(metodo_indirecto,codigos,by ="Población",all.x = TRUE)
metodo_indirecto[1,8] <- "CABA"

media_IME <- mean(metodo_indirecto$IME.)

# Crear el gráfico con fondo en las etiquetas
ggplot(metodo_indirecto, aes(x = IME., y = reorder(Provincia, IME.))) +
  geom_segment(aes(x = media_IME, xend = IME., y = Provincia, yend = Provincia), color = "#6d6875",
               linetype = "dashed") +
  geom_label(aes(label = Provincia), hjust = 0, nudge_x = 1.5, size = 3, fill = "#2b2d42", 
             color = "white", label.padding = unit(0.5, "lines")) +
  geom_point(size = 2.5, color = "#2b2d42") +
  scale_x_continuous(breaks = seq(40,180,15), limits = c(40,180)) +
  scale_y_discrete("", breaks = "") +
  geom_vline(xintercept = media_IME, linetype = "dashed", color = "#6d6875") +
  theme_grey() +
  labs(x = "Índice de mortalidad estandar", y = NULL) +
  theme(
    text = element_text(family = "serif")
  )
