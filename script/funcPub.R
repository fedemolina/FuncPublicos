paquetes <- c("data.table", "ggplot2", "magrittr", "extrafont", "ggthemes", "grid", "cowplot",
              "ggrepel")
sapply(paquetes, require, character.only=TRUE)
# font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)
extrafont::loadfonts(device="win")
# theme_set(theme_minimal(base_family = "Helvetica Neue"))
theme_set(theme_minimal())

# Carga de datos ----------------------------------------------------------

repo <- here::here("Datos")
dt <- readxl::read_excel(here::here("Datos", "Datos-ONSC", "Evolucion_funcionarios_publicos_2018.xls"), 
            skip = 3, range = "B4:Z69") %>%  # Arreglar nombres, sacar acentos
    as.data.table(.)
dt_i <- readxl::read_excel(paste(repo, "GobMunicipales.xlsx", sep = "/"), range = "A1:H20", 
                            col_types = "text") %>% as.data.table(.)
df <- readxl::read_excel(here::here("Datos", "Datos-ONSC", "Evolucion_NO_funcionarios_publicos_2018.xls"), 
                         skip = 3, range = "B4:Z69") %>% as.data.table(.)
setnames(dt, old = "INCISO", new = "inciso")
setnames(dt_i, old = "Departamento", new = "inciso")
setnames(df, old = "INCISO", new = "inciso")
# Limpieza ----------------------------------------------------------------

# Tabla de partido político en el poder según intendencia
dt_il1 <- melt(dt_i, id.vars = "inciso", variable.name = "fecha", value.name = "partido") 
# Necesito tener los datos por separado, los de geom_rect, geom_lines...
dt_il1[, `:=`(f_ini = as.Date(paste(substr(fecha,1,4),"12","31",sep="-")),
              f_fin = as.Date(paste(substr(fecha,6,9),"12","31",sep="-")))]
fech <- seq.Date(as.Date("1995-12-31"), as.Date("2015-12-31"), "5 years")
dt_il1 <- dt_il1[f_ini %in% fech]
dt_il1[, ano := as.integer(substr(fecha,1,4))]
dt_il1[f_fin == as.Date("2020-12-31"), f_fin := as.Date("2018-12-31")]
inciso <- unique(dt_il1$inciso)
f_ini <- as.Date(rep("2018-12-31",19))
f_fin <- as.Date(rep("2018-12-31",19))
ano <- rep(2018,19)
partido <- dt_il1[fecha == "2015-2020", partido]
fecha <- rep("2015-2020", 19)
temp <- data.table(inciso, fecha, partido, f_ini, f_fin, ano)
dt_il1 <- rbind(dt_il1, temp)

dt_il  <- data.table::melt(dt_i, id.vars = "inciso", variable.name = "fecha", value.name = "partido") 
dt_il[, fecha := substr(fecha, 1, 4) %>% as.integer]
setkey(dt_il, "inciso", "fecha")
dt_il[rep(seq_len(nrow(dt_il)), 5), ][, .N, by = inciso]
dt_il <- dt_il[rep(seq_len(nrow(dt_il)), 5), ]
setkey(dt_il, "inciso", "fecha")
for(i in c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) {
    dt_il[fecha == i, fecha := seq(from = i, to = i+4), by = inciso]
}
dt_il[fecha %in% c(1985, 1990, 1995, 2000, 20005, 2010, 2015), .N, by = .(fecha, partido)][order(partido)]
setkey(dt_il ,"fecha", "partido")
dt_il
# cortamos porque solo tenemos datos de 1995 a 2018.
dt_il <- dt_il[fecha %between% c(1995,2018)]
# Luego matchear por año con las otras bases



# Funcionarios Públicos por Intendencia -----------------------------------


dt <- melt(dt, id.vars = "inciso", variable.name = "fecha", value.name = "cantidad", variable.factor = FALSE)
dt[, fecha := janitor::excel_numeric_to_date(as.numeric(fecha))][, ano := year(fecha)]
dt[grepl(pattern = "Gobierno Departamental", inciso), ]
# Nos quedamos con el subconjunto de intendencias
sdt <- dt[grepl(pattern = "Gobierno Departamental", inciso), ]
sdt[, inciso := gsub(inciso, pattern = "Gobierno Departamental de ", replacement = "")]

# # Ahora le agrego los partidos o antes lo puedo hacer. DEJAR ESTO PARA DESPUÉS, AL FINAL DE TODO.
setkey(sdt, ano, inciso)
setkey(dt_il, fecha, inciso)
sdt <- sdt[dt_il]
sdt[partido=="Colorado", color := "red"][partido == "Frentista", color := "blue"][partido=="Nacionalista", color := "skyblue"]
setkey(dt_il1, ano, inciso)
dt_il1 <- dt_il1[sdt[ano %in% c(1995,2000,2005,2010,2015, 2018), .(cantidad ,color, ano, inciso)]]


ggplot(sdt, aes(x = fecha, y = cantidad, color = inciso)) + 
    geom_line() +
    theme() +
    labs() +
    facet_wrap(~inciso) + 
    geom_vline(xintercept = as.Date(paste(seq(1995,2015,5),"01","01",sep="-")),
           colour = "#aa000077", size = .7, linetype = 2) 

sdt[, f_ini := fecha]
sdt[, f_fin := as.Date(paste(ano+1,"12","31", sep = "-"))]
sdt[ano == 2018, f_fin := as.Date("2018-12-31")]

ggplot(sdt, aes(x = fecha, y = cantidad)) + 
    geom_line() +
    facet_wrap(~inciso) + 
    geom_rect(data= sdt , aes(xmin = f_ini, ymin = -Inf, 
        xmax = f_fin, ymax = Inf), fill = sdt$color, alpha = 0.5) +    
    # geom_path(size = 0.2) +
    geom_vline(xintercept = as.Date(paste(seq(1995,2015,5),"12","31",sep="-")),
               colour = "black", size = .1, linetype = 3) +
    scale_y_continuous(name = "Cantidad funcionarios Públicos") +
    scale_x_date(name = "Fecha", date_labels = "%Y", position = "bottom", 
                 limits = as.Date(c("1995-12-31","2018-12-31")), 
                 breaks = seq.Date(from = as.Date("1995-12-31"), to = as.Date("2018-12-31"), by = "5 year")) +
    labs(title = "Funcionarios Públicos por Intendencias",
         subtitle = "Periodo 1995-2018",
         caption = "Color azul FA, color celeste PN, color rojo PC. Elaboración Fede Molina. Datos ONSC") +
    theme(#legend.position = "none", 
          axis.text.x = element_text(angle=45, hjust=1, size = 6),
          plot.caption = element_text(color = "black", face = "italic", size = 8),
          panel.spacing =  unit(0, "lines"))
ggsave(here::here("output", "funcPubIntendenciasPartido.png"))

ggplot(sdt, aes(x = fecha, y = cantidad)) + 
    geom_line() +
    facet_wrap(~inciso, scales = "free") + 
    geom_rect(data= sdt , aes(xmin = f_ini, ymin = -Inf, 
                              xmax = f_fin, ymax = Inf), fill = sdt$color, alpha = 0.5) +    
    # geom_path(size = 0.2) +
    geom_vline(xintercept = as.Date(paste(seq(1995,2015,5),"12","31",sep="-")),
               colour = "black", size = .1, linetype = 3) +
    scale_y_continuous(name = "Cantidad funcionarios Públicos") +
    scale_x_date(name = "Fecha", date_labels = "%Y", position = "bottom", 
                 limits = as.Date(c("1995-12-31","2018-12-31")), 
                 breaks = seq.Date(from = as.Date("1995-12-31"), to = as.Date("2018-12-31"), by = "5 year")) +
    labs(title = "Funcionarios Públicos por Intendencias",
         subtitle = "Periodo 1995-2018",
         caption = "Color azul FA, color celeste PN, color rojo PC. Elaboración Fede Molina. Datos ONSC") +
    theme(#legend.position = "none", 
        axis.text.x = element_text(angle=45, hjust=1, size = 6),
        plot.caption = element_text(color = "black", face = "italic", size = 8),
        panel.spacing =  unit(0, "lines"))
ggsave(here::here("output", "funcPubIntendenciasPartidoFree.png"))

# ggplot(dt_il1, aes(x = f_ini, y = cantidad)) +
#     geom_line() +
#     # geom_line(data = dt_il1, aes(x = f_fin, y = cantidad)) +
#     facet_wrap(~inciso, scales = "free") +
#     geom_rect(data= dt_il1 , aes(xmin = f_ini, ymin = -Inf, 
#                                  xmax = f_fin, ymax = Inf), fill = dt_il1$color) +
#     geom_path(size = 0.1) +
#     theme()


# Funcionarios no públicos ------------------------------------------------


df <- melt(df, id.vars = "inciso", variable.name = "fecha", value.name = "funcionarios", 
           variable.factor = FALSE) 
df[, fecha := janitor::excel_numeric_to_date(as.numeric(df$fecha))]
df[, ano := year(fecha)]
sdf <- df[grepl(pattern = "Gobierno Departamental", inciso), ]
sdf[, inciso := gsub(inciso, pattern = "Gobierno Departamental de ", replacement = "")]
sdf[, f_ini := fecha][, f_fin := as.Date(paste(ano+1, "12", "31", sep="-"))]

setkey(sdf, ano, inciso)
setkey(dt_il1, ano, inciso)
dt_il1[, `:=`(f_ini = NULL,
              f_fin = NULL,
              fecha = NULL)]
sdf <- dt_il1[sdf]


ggplot(sdf, aes(x = fecha, y = funcionarios)) + 
    geom_line() +
    labs(y = "Cantidad de funcionarios públicos",
         title = "Evolución funcionarios no públicos por intendencia",
         subtitle = "Periodo 1995-2018",
         caption = "Color azul FA, color celeste PN, color rojo PC. Elaboración Fede Molina. Datos ONSC") +
    facet_wrap(~inciso) + 
    geom_rect(data= sdf , aes(xmin = f_ini, ymin = -Inf, 
                              xmax = f_fin, ymax = Inf), fill = sdt$color, alpha = 0.5) +
    geom_vline(xintercept = as.Date(paste(seq(1995,2015,5),"12","31",sep="-")),
               colour = "brown", size = .1, linetype = 3) +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 6),
        plot.caption = element_text(color = "black", face = "italic", size = 8),
        panel.spacing =  unit(0, "lines")) +
    scale_y_continuous(name = "Cantidad funcionarios Públicos") +
    scale_x_date(name = "Fecha", date_labels = "%Y", position = "bottom", 
                 limits = as.Date(c("1995-12-31","2018-12-31")), 
                 breaks = seq.Date(from = as.Date("1995-12-31"), 
                                   to = as.Date("2018-12-31"), by = "5 year"))
ggsave(here::here("output", "funcNOPubIntendencia.png"))

ggplot(sdf, aes(x = fecha, y = funcionarios)) + 
    geom_line() +
    labs(y = "Cantidad de funcionarios públicos",
         title = "Evolución funcionarios no públicos por intendencia",
         subtitle = "Periodo 1995-2018",
         caption = "Color azul FA, color celeste PN, color rojo PC. Elaboración Fede Molina. Datos ONSC") +
    facet_wrap(~inciso, scales = "free") + 
    geom_rect(data= sdf , aes(xmin = f_ini, ymin = -Inf, 
                              xmax = f_fin, ymax = Inf), fill = sdt$color, alpha = 0.5) +
    geom_vline(xintercept = as.Date(paste(seq(1995,2015,5),"12","31",sep="-")),
               colour = "brown", size = .1, linetype = 3) +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 6),
          plot.caption = element_text(color = "black", face = "italic", size = 8),
          panel.spacing =  unit(0, "lines")) +
    scale_y_continuous(name = "Cantidad funcionarios Públicos") +
    scale_x_date(name = "Fecha", date_labels = "%Y", position = "bottom", 
                 limits = as.Date(c("1995-12-31","2018-12-31")), 
                 breaks = seq.Date(from = as.Date("1995-12-31"), 
                                   to = as.Date("2018-12-31"), by = "5 year"))
ggsave(here::here("output", "funcNOPubIntendenciaFree.png"))


# Efecto neto entre públicos y no públicos --------------------------------


setkey(sdf, ano, inciso)
setkey(sdt, ano, inciso)
neto <- sdf[sdt[,.(ano,inciso,cantidad, partido)]]

neto[, neto := sum(funcionarios+cantidad, na.rm = TRUE)]
neto[, neto := sum(funcionarios, na.rm = TRUE) + sum(cantidad, na.rm = TRUE), by = .(inciso, ano)]


ggplot(neto, aes(x = fecha, y = neto, color = inciso)) + 
    geom_line() +
    theme(legend.position = "none") +
    labs(y = "Cantidad de funcionarios públicos",
         title = "Evolución funcionarios públicos y no públicos por intendencia",
         subtitle = "Periodo 1995-2018", 
         caption = "Fuente de datos: ONSC") +
    facet_wrap(~inciso, scales = "free") +
    geom_vline(xintercept = as.Date(paste(seq(1995,2015,5),"01","01",sep="-")),
               colour = "#aa000077", size = .7, linetype = 2)

ggplot(neto, aes(x = fecha, y = neto)) + 
    geom_line() +
    labs(y = "Cantidad de funcionarios",
         title = "Evolución funcionarios públicos y no públicos por intendencia",
         subtitle = "Periodo 1995-2018",
         caption = "Color azul FA, color celeste PN, color rojo PC. Elaboración Fede Molina. Datos ONSC") +
    facet_wrap(~inciso, scales = "free") + 
    geom_rect(data= neto , aes(xmin = f_ini, ymin = -Inf, 
                              xmax = f_fin, ymax = Inf), fill = sdt$color, alpha = 0.5) +
    geom_vline(xintercept = as.Date(paste(seq(1995,2015,5),"12","31",sep="-")),
               colour = "brown", size = .1, linetype = 3) +
    theme(axis.text.x = element_text(angle=45, hjust=1, size = 6),
          plot.caption = element_text(color = "black", face = "italic", size = 8),
          panel.spacing =  unit(0, "lines")) +
    scale_y_continuous(name = "Cantidad funcionarios Públicos") +
    scale_x_date(name = "Fecha", date_labels = "%Y", position = "bottom", 
                 limits = as.Date(c("1995-12-31","2018-12-31")), 
                 breaks = seq.Date(from = as.Date("1995-12-31"), 
                                   to = as.Date("2018-12-31"), by = "5 year"))
ggsave(here::here("output", "funcTotalIntendenciaFree.png"))
