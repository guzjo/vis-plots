# Plot to visualize some % of certain groups of a pop / 100% of a total pop 

col_f_p <- dataset %>%                                     
  mutate( fraccion = total_pop_reported / certain_pop_group_reported,
          perc = (total_pop_reported / certain_pop_group_reported) * 100)


marcasy <- seq( from = 0,           # creamos una secuencia desde el valor 0
                to = 0.35,          # hasta el valor n  (equivalente al n % )
                by = 0.1 )          # con incrementos de 0.1


# preparamos el fondo del plot
espiral1 <- ggplot( data = col_f_p ) +  # con el dataframe de los cambios
  geom_hline( yintercept = marcasy,    # creamos lineas horizontales en cada valor de n a n, con incrementos de n
              linetype = "dashed",     # el tipo de linea es "guion"
              color = "gray" ) +       # color gris la linea
  geom_hline( yintercept = 0 )         # trazamos una linea negra en el valot Y = 0



# Dibujamos las esferas
espiral2 <- espiral1 +
  geom_segment( mapping = aes( x = Isocode,                # nombres de los ejes X
                               xend = Isocode,             
                               y = 0,                     # y = a 0 (todos empiezan en la linea negra del valor Y = 0 )
                               yend = perc ) ) +  # yend = valor final porcentual/fraccion 
  geom_point( shape = 21,                                 # forma de circulo relleno
              size = 4,                                   # tamanio de punto 4
              mapping = aes( x = Isocode,                  # los puntos van en x = nombre de ejes x
                             y = perc,            # los puntos van en y = el cambio de porcentaje
                             fill = perc ) )      # el relleno colorido de los puntos depende del cambio en porcentaje/fraccion

# aniadir etiquetas de porcentajes relevantes
# Este paso se puede omitir,
# si se aniade, hay que jugar con los valores de X & Y para que aparezcan
#  de forma correcta
espiral3 <- espiral2 +
  geom_text( x = 0.5, y = -2.2, label = "0.1%" ) +
  geom_text( x = 1.5, y = -1.7, label = "0.2%" ) +
  geom_text( x = 0.5, y = -1, label = "0.3%" ) 


# creamos un vector ordenado
# este servira para reordenar los paises con porcentajes
# en el grafico final
ordenados <- col_f_p %>%  # a partir del dataframe con los cambios 
  arrange( desc( perc ) ) %>%   # ordenamos de acuerdo a la columna fracc, en orden DESCendente
  pull( Isocode )                        # extraemos la columna "pais"; esto la extrae como vector



# Cambiamos relleno de color y ajustamos ejes
espiral4 <- espiral3 +
  scale_fill_gradient( low = "#990066",     # los valores bajos van en un color
                       high = "#ff9900" ) +  # el valor alto va en otro color
  scale_x_discrete( limits = ordenados )    # los valores de pais se reordenan de acuerdo a nuestro vector de arriba


# polarizamos el grafico
# y agregamos titulos
espiral5 <- espiral4 +
  coord_polar( ) +
  scale_y_continuous(trans = "log")  + # si los valores difieren mucho entre si, se puede poner un log
  labs( title = "Porcentaje de individuos NatAm estudiados \n respecto a la población NatAm total reportada",       # ponemos titulo
        subtitle = "Individuos NatAm contemporáneos",
        caption = "Valores transformados con logaritmo") +                        # ponemos pie de figura
  theme_void( ) +                                             # usamos un tema Vacio
  theme( legend.position = "none",                            # eliminamos la leyenda
         axis.text.x = element_text( face = "bold" ),         # recuperamos los nombres de los estados en Negritas ("bold")
         plot.background = element_rect( fill = "white",      # recuperamos el fondo blanco del grafico
                                         colour = "white"),
         plot.title = element_text(hjust = 0.5),
         plot.subtitle = element_text(hjust = 0.5),
         plot.caption = element_text(hjust = 0.5),
         text = element_text(size = 18)
  ) 
espiral5



# guardamos el plot
ggsave( filename = "espiral_natam_ind.png",  # el nombre del archivo de salida
        plot = espiral5,           # guardamos el ultimo grafico que hicimos
        width = 7,                # ancho de 7 pulgadas
        height = 7,               # alto 7 de pulgadas
        dpi = 600 )               # resolucion de 600 puntos por pulgada


