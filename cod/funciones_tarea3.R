# Funcion: alerta.movil
# Recibe : un data frame de transacciones, dos parametros.
# Devuelve: un nuevo data frame con fecha, cantidad de transacciones en el dia, media, desviacion y una alerta.


alerta.movil <-  function(df,ventana,k.alerta){
  datos.diarios <- df %>%
    group_by(fecha) %>%
    summarise(
      n.tx = n(),
      .groups = "drop"
      ) %>% 
    arrange(fecha)


  

  n <- nrow(datos.diarios)
  
  
  datos.diarios$media <- NA
  datos.diarios$desv <- NA
  datos.diarios$alerta <- NA

  for (t in (ventana+1):n) {
      anteriores <- datos.diarios$n.tx[(t-ventana):(t-1)]
      datos.diarios$media[t] <- mean(anteriores)
      datos.diarios$desv[t] <- sd(anteriores)
  
  
      datos.diarios$alerta[t] <- datos.diarios$n.tx[t] > datos.diarios$media[t] +k.alerta *datos.diarios$desv[t]
    }
    return(datos.diarios)
  }