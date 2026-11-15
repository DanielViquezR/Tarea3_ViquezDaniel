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



# Función: benford.mad
# Recibe : vector de montos
# Devuelve la desv abs media usando la ley de bendford.


benford.mad <-  function(x){
  digitos <- floor(x / 10^floor(log10(x)))
  frecuencia <-  tabulate(digitos ,nbins=9)
  proporciones <- frecuencia /length(digitos)
  
  d <-  1:9
  benford <-  log10(1+1/d)
  mad <- mean(abs(proporciones - benford)) 
  return(mad)
}


# Funcion : Benford cantón
# Recibe: transacciones, canal y tabla de cantones
# Devuelve: tabla por cantón con n, MAD y clasificación


benford.canton <- function(df, canal, cantones) {
  resultado <- df %>%
    filter(.data$canal == .env$canal) %>%
    group_by(cod_canton) %>%
    summarise(
      n = n(),
      mad = benford.mad(monto_crc),
      .groups = "drop"
    ) %>%
    left_join(cantones, by = "cod_canton") %>%
    mutate(
      clase = case_when(
        mad <= 0.006 ~ "conformidad cercana",
        mad <= 0.012 ~ "conformidad aceptable",
        mad <= 0.015 ~ "conformidad marginal",
        TRUE ~ "no conformidad"
      )
    ) %>%
    select(cod_canton, canton, n, mad, clase) %>%
    arrange(desc(mad))
  
  return(resultado)
}



# Funcion : Valor acumulado

valor.acumulado <-  function(df, tasa.anual, canal){
  tasa.mensual <- (1+tasa.anual)^(1/12) - 1
  
  datos.canal <-  df[df$canal == canal,]
  
  
  tabla <- datos.canal %>% 
    mutate(mes = as.integer(format(as.Date(fecha), "%m"))) %>%
    group_by(mes) %>% 
    summarise(
      monto = sum(monto_crc),
      .groups = "drop"
    ) %>% 
    mutate( factor = (1+tasa.mensual)^(12 - mes))


va <- sum(tabla$monto *tabla$factor)

interes <-  va - sum(tabla$monto)

return(list(
  tabla= tabla, tasa.mensual= tasa.mensual, va= va, interes= interes)
)

}