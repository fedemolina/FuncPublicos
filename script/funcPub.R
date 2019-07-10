paquetes <- c("data.table", "ggplot2", "magrittr", "extrafont", "ggthemes", "grid", "cowplot",
              "ggrepel")
sapply(paquetes, require, character.only=TRUE)
# font_import(paths = NULL, recursive = TRUE, prompt = TRUE,pattern = NULL)
# extrafont::loadfonts(device="win")
# theme_set(theme_minimal(base_family = "Helvetica Neue"))
theme_set(theme_minimal())

# Carga de datos ----------------------------------------------------------

repo <- here::here("Datos")
pub_wide <- readxl::read_excel(here::here("Datos", "Datos-ONSC", "Evolucion_funcionarios_publicos_2018.xls"), 
                               skip = 3, range = "B4:Z68") %>%  # Arreglar nombres, sacar acentos
    as.data.table(.)
gob_dep_wide <- readxl::read_excel(paste(repo, "GobMunicipales.xlsx", sep = "/"), range = "A1:H20", 
                                   col_types = "text") %>% as.data.table(.)
no_pub_wide <- readxl::read_excel(here::here("Datos", "Datos-ONSC", "Evolucion_NO_funcionarios_publicos_2018.xls"), 
                                  skip = 3, range = "B4:Z68") %>% as.data.table(.)
setnames(pub_wide, old = "INCISO", new = "inciso")
setnames(gob_dep_wide, old = "Departamento", new = "inciso")
setnames(no_pub_wide, old = "INCISO", new = "inciso")
#no_pub_wide viene con una fila vacia inicial porque usan 2 filas en los nombres...

# Limpieza y cambio de formato --------------------------------------------


# Funcionarios públicos
pub_long <- melt(pub_wide, id.vars = "inciso", variable.name = "fecha", value.name = "publicos", variable.factor = FALSE)
pub_long[, fecha := janitor::excel_numeric_to_date(as.numeric(fecha))][, ano := year(fecha)]

pub_long_gobdep <- pub_long[grepl(pattern = "Gobierno Departamental", inciso), ]
pub_long_gobdep[, inciso := gsub(inciso, pattern = "Gobierno Departamental de ", replacement = "")][, fecha := NULL]

# Funcionarios no públicos
no_pub_wide <- no_pub_wide[2:.N]
no_pub_long <- melt(no_pub_wide, id.vars = "inciso", variable.name = "fecha", value.name = "noPublicos", variable.factor = FALSE)
no_pub_long[, fecha := janitor::excel_numeric_to_date(as.numeric(fecha))][, ano := year(fecha)] # Usando {} debería hacerlo en 1 línea.
no_pub_long_gobdep <- no_pub_long[grepl(pattern = "Gobierno Departamental", inciso)
                                  ][, inciso := gsub(inciso, pattern = "Gobierno Departamental de ", replacement = "")
                                    ][,`:=`(f_ini = fecha,
                                            f_fin = as.Date(paste(ano + 1, "12", "31", sep = "-"))
                                    )
                                    ][, fecha := NULL]

# Partidos políticos en el poder
gob_dep_long <- melt(gob_dep_wide, id.vars = "inciso", variable.name = "fecha", variable.factor = FALSE, value.name = "partido")
gob_dep_long[, ano := substr(fecha, 1, 4) %>% as.integer][, fecha := NULL]
setkey(gob_dep_long, "inciso", "ano")
gob_dep_long[rep(seq_len(nrow(gob_dep_long)), 5), ][, .N, by = inciso]
gob_dep_long <- gob_dep_long[rep(seq_len(nrow(gob_dep_long)), 5), ]
setkey(gob_dep_long, "inciso", "ano")
for(i in c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) {
    gob_dep_long[ano == i, ano := seq(from = i, to = i+4), by = inciso]
}
gob_dep_long[ano %in% c(1985, 1990, 1995, 2000, 20005, 2010, 2015), .N, by = .(ano, partido)][order(ano, partido)]
setkey(gob_dep_long ,"ano", "partido")
gob_dep_long
# cortamos porque solo tenemos datos de 1995 a 2018.
gob_dep_long <- gob_dep_long[ano %between% c(1995,2018)]


# Inner entre las tablas --------------------------------------------------
# Por seguridad fijemos las key nuevamente.
keycols <- c("ano", "inciso")
setkeyv(gob_dep_long, keycols)
setkeyv(no_pub_long_gobdep, keycols)
setkeyv(pub_long_gobdep, keycols)

dt <- no_pub_long_gobdep[gob_dep_long][pub_long_gobdep][
    partido == "Colorado", color := "red"
    ][  partido == "Frentista", color := "blue"
        ][  partido == "Nacionalista", color := "skyblue"
            ][, neto := sum(noPublicos, na.rm = TRUE) + sum(publicos, na.rm = TRUE), by = .(inciso, ano)
              ]

# Función para graficar
func_plot <- function(data, x = "x",y = "y", title, scala = "free", color = "color") {
    # f_ini <- data[, f_ini]
    # f_fin <- data[, f_fin]
    fill  <- data[, color]
    ggplot(data, aes_string(x = x, y = y)) + 
        geom_line() +
        labs(y = "Cantidad de funcionarios",
             title = paste("Evolución funcionarios", title, sep = " "),
             subtitle = "Periodo 1995-2018. Por Gobierno Departamental",
             caption = "Color azul FA, color celeste PN, color rojo PC. Elaboración Fede Molina. Datos ONSC") +
        facet_wrap(~inciso, scales = scala) + 
        geom_rect(data = data , aes(xmin = f_ini, ymin = -Inf, 
                                    xmax = f_fin, ymax = Inf), fill = fill, alpha = 0.5) +
        geom_vline(xintercept = as.Date(paste(seq(1995,2015,5),"12","31",sep="-")),
                   colour = "brown", size = .1, linetype = 3) +
        theme(axis.text.x = element_text(angle=45, hjust=1, size = 6),
              plot.caption = element_text(color = "black", face = "italic", size = 8),
              panel.spacing =  unit(0, "lines"),
              aspect.ratio = 1) +
        scale_y_continuous(name = paste("Cantidad funcionarios", title, sep = " ")) +
        scale_x_date(name = "Fecha", date_labels = "%Y", position = "bottom", 
                     limits = as.Date(c("1995-12-31","2018-12-31")), 
                     breaks = seq.Date(from = as.Date("1995-12-31"), 
                                       to = as.Date("2018-12-31"), by = "5 year"))
}

func_plot(dt, x = "f_ini", y = "neto", title = "publicos y no públicos")
func_plot(dt, x = "f_ini", y = "noPublicos", title = "no públicos")
func_plot(dt, x = "f_ini", y = "publicos", title = "públicos")

ggsave(here::here("output", "funcTotalIntendenciaFree.png"))
