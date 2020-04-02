#install.packages("factoextra")
library("factoextra")


df_plot <- left_join(nombres, resumen, by = "Nemo")


df_plot <- df_plot %>% mutate(MktCap = `N Acc`*Px_Hoy,
                         pxhoy_Fco = Px_Hoy*`N Acc`/(FCO))



group_aux <- df_plot %>% group_by(Industria) %>% summarise(Prom = mean(pxhoy_Fco))

df_plot <- left_join(df_plot, group_aux, by = "Industria") %>% mutate(prom_pxfco = round(pxhoy_Fco/Prom,2)
                                                              )

test <- df_plot

test_norm <- df_plot %>% filter(FCO > 0) %>% mutate(sc_prom_pxfco = scale(prom_pxfco, center = TRUE, scale = TRUE),
                                                    sc_rz_circ = scale(`Razon Circulante`, center = TRUE, scale = TRUE),
                                                    sc_prop = scale(prop, center = TRUE, scale = TRUE),
                                                    Rating = -0.2*(sc_prom_pxfco)+0.5*sc_rz_circ-0.3*sc_prop) %>% select(Nemo, Industria, sc_rz_circ, sc_prop, sc_prom_pxfco, Rating)

write.csv(test_norm, "test_norm.csv")

test <- df_plot %>% filter(FCO > 0) %>% mutate(Rating = 0.2*(1-prom_pxfco)+0.4*`Razon Circulante` + (0.4*(1-prop/100) ))  %>% select(Nemo, Industria, sc_rz_circ, sc_prop, sc_prom_pxfco, Rating) %>% arrange(Rating)



set.seed(20)
clusters <- kmeans(scale(cbind(df_plot$`Razon Circulante`, df_plot$prop)), 4)


df_plot$cluster <- as.factor(clusters$cluster)
str(clusters)

fviz_cluster(clusters, data = cbind(df_plot$`Razon Circulante`, df_plot$prop, df_plot$MktCap) , ellipse.type = "norm")



ggplot(df_plot, aes(`Razon Circulante`, prop, color = factor(cluster), label = Nemo)) +
  geom_hline(yintercept = (min(df_plot$prop)+max((df_plot$prop)) )/2) +
  geom_vline(xintercept = (min(df_plot$`Razon Circulante`)+max((df_plot$`Razon Circulante`)) )/2) +
  annotate("text", x = quantile(df_plot$`Razon Circulante`, 0.05), y = quantile(df_plot$prop, 0.05 ), alpha = 0.35, label = c("Caja Debil\nPrecio Subvalorado")) +
  annotate("text", x = quantile(df_plot$`Razon Circulante`, 0.05), y = quantile(df_plot$prop, 0.99), alpha = 0.35, label = c("Caja Debil\nPrecio cercano a Objetivo")) +
  annotate("text", x = quantile(df_plot$`Razon Circulante`, 0.95), y = quantile(df_plot$prop, 0.99), alpha = 0.35, label = c("Caja Fuerte\nPrecio cercano a Objetivo")) +
  annotate("text", x = quantile(df_plot$`Razon Circulante`, 0.95), y = quantile(df_plot$prop, 0.05), alpha = 0.35, label = c("Caja Fuerte\nPrecio subvalorado")) +
  labs(title = "Clusterizacion principales acciones Bolsa Stgo", y = "Px Hoy/Prom Px 2019", color = "Cluster", caption = "Data actualizada diariamente, extraida desde Yahoo Finance") +
  geom_text()


ggplot(df_plot, aes(`Razon Circulante`, prop, color = factor(Industria), label = Nemo)) +
  geom_hline(yintercept = (min(df_plot$prop)+max((df_plot$prop)) )/2) +
  geom_vline(xintercept = (min(df_plot$`Razon Circulante`)+max((df_plot$`Razon Circulante`)) )/2) +
  annotate("text", x = quantile(df_plot$`Razon Circulante`, 0.05), y = quantile(df_plot$prop, 0.05 ), alpha = 0.35, label = c("Caja Debil\nPrecio Subvalorado")) +
  annotate("text", x = quantile(df_plot$`Razon Circulante`, 0.05), y = quantile(df_plot$prop, 0.99), alpha = 0.35, label = c("Caja Debil\nPrecio cercano a Objetivo")) +
  annotate("text", x = quantile(df_plot$`Razon Circulante`, 0.95), y = quantile(df_plot$prop, 0.99), alpha = 0.35, label = c("Caja Fuerte\nPrecio cercano a Objetivo")) +
  annotate("text", x = quantile(df_plot$`Razon Circulante`, 0.95), y = quantile(df_plot$prop, 0.05), alpha = 0.35, label = c("Caja Fuerte\nPrecio subvalorado")) +
  labs(title = "Clusterizacion principales acciones Bolsa Stgo", y = "Px Hoy/Prom Px 2019", color = "Cluster", caption = "Data actualizada diariamente, extraida desde Yahoo Finance") +
  geom_text()




ggplot((subset(df_plot, Nemo != "EISA")) , aes(prom_pxfco, prop, color = factor(Industria), label = Nemo)) +
  geom_hline(yintercept = (min(df_plot$prop)+max((df_plot$prop)) )/2) +
  geom_vline(xintercept = (min(subset(df_plot, Nemo != "EISA")$prom_pxfco)+max((subset(df_plot, Nemo != "EISA")$prom_pxfco)) )/2) +
  annotate("text", x = quantile(subset(df_plot, Nemo != "EISA")$prom_pxfco, 0.05), y = quantile(df_plot$prop, 0.05 ), alpha = 0.35, label = c("Caja Debil\nPrecio Subvalorado")) +
  annotate("text", x = quantile(subset(df_plot, Nemo != "EISA")$prom_pxfco, 0.05), y = quantile(df_plot$prop, 0.99), alpha = 0.35, label = c("Caja Debil\nPrecio cercano a Objetivo")) +
  annotate("text", x = quantile(subset(df_plot, Nemo != "EISA")$prom_pxfco, 0.75), y = quantile(df_plot$prop, 0.99), alpha = 0.35, label = c("Caja Fuerte\nPrecio cercano a Objetivo")) +
  annotate("text", x = quantile(subset(df_plot, Nemo != "EISA")$prom_pxfco, 0.75), y = quantile(df_plot$prop, 0.05), alpha = 0.35, label = c("Caja Fuerte\nPrecio subvalorado")) +
  labs(title = "Clusterizacion principales acciones Bolsa Stgo", y = "Px Hoy/Prom Px 2019", color = "Cluster", caption = "Data actualizada diariamente, extraida desde Yahoo Finance") +
  geom_text()


ggplot(df_plot, aes(`Razon Circulante`, prop, color = factor(cluster), label = Nemo)) +
  facet_wrap(~Industria, scales = "free") +
  labs(title = "Clusterizacion principales acciones Bolsa Stgo", y = "Px Hoy/Prom Px 2019", color = "Cluster", caption = "Data actualizada diariamente, extraida desde Yahoo Finance") +
  geom_text()

ggplot(df_plot, aes(prom_pxfco, prop, color = factor(cluster), label = Nemo)) +
  facet_wrap(~Industria, scales = "free") +
  labs(title = "Clusterizacion principales acciones Bolsa Stgo", y = "Px Hoy/Prom Px 2019", color = "Cluster", caption = "Data actualizada diariamente, extraida desde Yahoo Finance") +
  geom_text()


ggplot(df_plot, aes(`Razon Circulante`, prop, color = factor(Industria), label = Nemo)) +
  facet_wrap(~cluster, scales = "free") +
  labs(title = "Clusterizacion principales acciones Bolsa Stgo", y = "Px Hoy/Prom Px 2019", color = "Cluster", caption = "Data actualizada diariamente, extraida desde Yahoo Finance") +
  geom_text()


