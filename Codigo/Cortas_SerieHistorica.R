# Evolución das cortas de madeira en Galicia, por grupos de especies, 1960-2020
# Eduardo Corbelle, 8 de setembro de 2017 (modificado 30 xaneiro 2024)

library(data.table)
library(ggplot2)

# Carga de datos de cortas ----

cortas1 <- rbind(
  fread("Datos/Provincias/Cortas1.txt", skip = 6),
  fread("Datos/Provincias/Cortas2.txt", skip = 6),
  fread("Datos/Provincias/Cortas3.txt", skip = 6)
)

cortas2 <- dcast(cortas1, ano + provincia ~ grupo, value.var = "volume", fill = 0)
cortas2[ , `:=`(Outras = Cupulíferas + Outras + Frondosas - Eucalipto,
               Total = Cupulíferas + Outras + Frondosas + Coníferas)]
cortas3 <- melt(cortas2, 
                id.vars = c("ano", "provincia"), 
                measure.vars = c("Coníferas", "Eucalipto", "Outras", "Total"),
                variable.name = "grupo",
                value.name = "volume")
cortas <- cortas3[ , .(volume = sum(volume)), .(ano, grupo)]
cortas$grupo <- factor(cortas$grupo, levels = c("Total", "Coníferas", "Eucalipto", "Outras"))


# Gráficos ----

g.total <- ggplot(cortas, aes(x = ano, y = volume/1e6)) +
  geom_line(linewidth = .25) +
  facet_wrap(~grupo, scales = "free_y") +
  expand_limits(y = 0) +
  xlab("") +
  ylab("Volume con cortiza (millóns m³)") +
  labs(title = "Galicia. Cortas de madeira por grupos de especies, 1960-2020", 
          # subtitle = "Galicia, 1960-2020",
          caption = "Eduardo Corbelle, 2024. CC-BY 4.0\nDatos: Estadística Forestal de España (MITECO)") +
  # stat_smooth(se = FALSE, lwd = .5) +
  theme_light() +
  theme(plot.caption = element_text(size = 7, colour = "grey40"))


# Exportación ----
png("Cortas1.png", width = 15, height = 10, units = "cm", res = 300)
 print(g.total)
dev.off()

