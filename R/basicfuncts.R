
#' Cálculo del monto generado a una tasa simple.
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param interes Tasa de interés simple a la que se dejará la inversión inicial
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details La función calcula el monto que va a generar nuestra inversión a una tasa de interés dada, con un periodo establecido a una frecuencia de capitalización dada. Genera una cadena de texto explicando el valor del monto final.
#' @return
#' @export
#'
#' @examples monto_simple(100,12,0.05,12)
monto_simple <- function(capital,tiempo,interes, periodicidad){

  Monto <- capital*interes*(tiempo/periodicidad)

  return( paste0("El monto generado con tu inversión será de $", round(digits = 3,Monto)))

}

#' Cálculo del monto generado a una tasa simple.
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param interes Tasa de interés simple a la que se dejará la inversión inicial
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details La función calcula el monto que va a generar nuestra inversión a una tasa de interés dada, con un periodo establecido a una frecuencia de capitalización dada. Unicamente devuelve el valor numérico. Útil cuando se requieren hacer cálculos adicionales con ese valor.
#'
#' @return
#' @export
#'
#' @examples monto_simple_n(100,12,0.05,12)
monto_simple_n <- function(capital,tiempo,interes, periodicidad){

  Monto <- capital*interes*(tiempo/periodicidad)

  return(Monto)

}

#' Cálculo del valor futuro generado a una tasa simple.
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param interes Tasa de interés simple a la que se dejará la inversión inicial
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details La función calcula el monto que va a generar nuestra inversión a una tasa de interés dada, con un periodo establecido a una frecuencia de capitalización dada. Genera una cadena de texto explicando el valor futuro total.
#'
#' @return
#' @export
#'
#' @examples valor_futuro_simple(100,12,0.05,12)
valor_futuro_simple <- function(capital,tiempo,interes, periodicidad){

  Monto <- (capital*interes*(tiempo/periodicidad))+capital

  return( paste0("El valor futuro con tu inversión será de $", round(digits = 3,Monto)))

}

#' Cálculo del valor futuro generado a una tasa simple.
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param interes Tasa de interés simple a la que se dejará la inversión inicial
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details Unicamente devuelve el valor numérico. Útil cuando se requieren hacer cálculos adicionales con ese valor.
#'
#' @return
#' @export
#'
#' @examples valor_futuro_simple_n(100,12,0.05,12)
valor_futuro_simple_n <- function(capital,tiempo,interes, periodicidad){

  Monto <- (capital*interes*(tiempo/periodicidad))+capital

  return(Monto)

}



#' Cálculo de la tasa de interés.
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details Se calcula la tasa de interés necesaria para obtener un monto a un cierto periodo, con una inversión inicial dada. Devuelve una cadena de texto con la tasa de interés necesaria.
#'
#' @return
#' @export
#'
#' @examples interes_simple(100,12,50,12)
interes_simple <- function(capital,tiempo,monto, periodicidad){

  interes <- (monto*periodicidad)/(capital*tiempo)

  return(paste0("La tasa de interés en este caso es de ",round(digits = 2,interes)))

}

#' Cálculo de la tasa de interés
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details Se calcula la tasa de interés necesaria para obtener un monto a un cierto periodo, con una inversión inicial dada. Unicamente devuelve el valor numérico. Útil cuando se requieren hacer cálculos adicionales con ese valor.
#'
#' @return
#' @export
#'
#' @examples interes_simple_n(100,12,50,12)
interes_simple_n <- function(capital,tiempo,monto, periodicidad){

  interes <- (monto*periodicidad)/(capital*tiempo)

  return(interes)

}

#' Cálculo del capital
#'
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#' @param interes Tasa de interés simple a la que se dejará la inversión inicial
#'
#' @details Función para calcular el capital necesario para obtener un monto dado a un cierto tiempo, con una tasa de interés y capitalización dadas. Regresa una cadena de texto explicando el valor necesario.
#'
#' @return
#' @export
#'
#' @examples capital_simple(12,150,12,0.12)
capital_simple <- function(tiempo,monto, periodicidad,interes){

  capital <- (monto*periodicidad)/(interes*tiempo)

  return(paste0("El capital necesario para obtener un monto de ",monto," debe ser de ",round(digits = 2,capital)))

}

#' Cálculo del capital
#'
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#' @param interes Tasa de interés simple a la que se dejará la inversión inicial
#'
#' @details Función para calcular el capital necesario para obtener un monto dado a un cierto tiempo, con una tasa de interés y capitalización dadas. Únicamente devuelve el valor numérico. Útil cuando harás más cálculos con ese valor.
#'
#' @return
#' @export
#'
#' @examples capital_simple(12,150,12,0.12)
capital_simple_n <- function(tiempo,monto, periodicidad,interes){

  capital <- (monto*periodicidad)/(interes*tiempo)

  return(capital)

}

#' Cálculo del tiempo
#'
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#' @param capital Valor de la inversión inicial
#' @param interes Tasa de interés simple a la que se dejará la inversión inicial
#'
#' @details Función para calcular el tiempo necesario para obtener un monto dado a un cierto tiempo, con una tasa de interés y capitalización dadas, considerando un capital inicial dado. Regresa una cadena de texto explicando el tiempo necesario.
#'
#' @return
#' @export
#'
#' @examples tiempo_simple(150,12,200,12)
tiempo_simple <- function(monto, periodicidad,capital,interes ){

  tiempo <- (monto*periodicidad)/(capital*interes)

  return({

    if (periodicidad ==12){

      paste0("Se requeriran ", round(tiempo, digits = 1), " meses para esta inversión")

    }else if (periodicidad == 1){

      paste0("Se requeriran ", round(tiempo, digits = 1), " años para esta inversión")

    }else if (periodicidad == 365){

      paste0("Se requeriran ", round(tiempo, digits = 1), " días para esta inversión")

    }else{

      paste0("Se requeriran ", round(tiempo, digits = 1), " unidades de tiempo para esta inversión")

    }

  })

}


#' Cálculo del tiempo
#'
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#' @param capital Valor de la inversión inicial
#' @param interes Tasa de interés simple a la que se dejará la inversión inicial
#'
#' @details Función para calcular el tiempo necesario para obtener un monto dado a un cierto tiempo, con una tasa de interés y capitalización dadas, considerando un capital inicial dado. Únicamente devuelve el valor numérico. Útil cuando harás más cálculos con ese valor.
#'
#' @return
#' @export
#'
#' @examples tiempo_simple_n(150,12,200,12)
tiempo_simple_n <- function(monto, periodicidad,capital,interes ){

  tiempo <- (monto*periodicidad)/(capital*interes)

  return(tiempo)

}



#' Cálculo del monto generado a una tasa compuesta.
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param interes Tasa de interés compuesta a la que se dejará la inversión inicial
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details La función calcula el monto que va a generar nuestra inversión a una tasa de interés compuesta dada, con un periodo establecido a una frecuencia de capitalización dada. Genera una cadena de texto explicando el valor del monto final.
#' @return
#' @export
#'
#' @examples monto_compuesto(100,12,0.05,12)
monto_compuesto <- function(capital,tiempo,interes, periodicidad){

  Monto = (capital*(1 + interes)^(tiempo/periodicidad))-capital

  return( paste0("El monto generado con tu inversión será de $", round(digits = 3,Monto)))

}


#' Cálculo del monto generado a una tasa compuesta.
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param interes Tasa de interés compuesta a la que se dejará la inversión inicial
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details La función calcula el monto que va a generar nuestra inversión a una tasa de interés compuesta dada, con un periodo establecido a una frecuencia de capitalización dada. Unicamente devuelve el valor numérico. Útil cuando se requieren hacer cálculos adicionales con ese valor.
#' @return
#' @export
#'
#' @examples monto_compuesto_n(100,12,0.05,12)
monto_compuesto_n <- function(capital,tiempo,interes, periodicidad){

  Monto = (capital*(1 + interes)^(tiempo/periodicidad))-capital

  return(Monto)

}


#' Cálculo del valor futuro generado a una tasa compuesta
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param interes Tasa de interés compuesto a la que se dejará la inversión inicial
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details La función calcula el monto que va a generar nuestra inversión a una tasa de interés compuesta dada, con un periodo establecido a una frecuencia de capitalización dada. Genera una cadena de texto explicando el valor futuro total.
#'
#' @return
#' @export
#'
#' @examples valor_futuro_compuesto(100,12,0.05,12)
valor_futuro_compuesto <- function(capital,tiempo,interes, periodicidad){

  Monto = (capital*(1 + interes)^(tiempo/periodicidad))

  return( paste0("El monto generado con tu inversión será de $", round(digits = 3,Monto)))

}

#' Cálculo del valor futuro generado a una tasa compuesta
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param interes Tasa de interés compuesto a la que se dejará la inversión inicial
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details La función calcula el monto que va a generar nuestra inversión a una tasa de interés compuesta dada, con un periodo establecido a una frecuencia de capitalización dada. Unicamente devuelve el valor numérico. Útil cuando se requieren hacer cálculos adicionales con ese valor.
#'
#' @return
#' @export
#'
#' @examples valor_futuro_compuesto_n(100,12,0.05,12)
valor_futuro_compuesto_n <- function(capital,tiempo,interes, periodicidad){

  Monto = (capital*(1 + interes)^(tiempo/periodicidad))

  return(Monto)

}

#' Cálculo de la tasa de interés compuesta.
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details Se calcula la tasa de interés compuesta necesaria para obtener un monto a un cierto periodo, con una inversión inicial dada. Devuelve una cadena de texto con la tasa de interés necesaria.
#'
#' @return
#' @export
#'
#' @examples interes_compuesto(100,12,50,12)
interes_compuesto <- function(capital,tiempo,monto, periodicidad){

  interes = ((capital+monto)/capital)^(1/(tiempo/periodicidad))-1

  return(paste0("La tasa de interés en este caso es de ",round(digits = 2,interes)))

}

#' Cálculo de la tasa de interés compuesta.
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details Se calcula la tasa de interés compuesta necesaria para obtener un monto a un cierto periodo, con una inversión inicial dada. Unicamente devuelve el valor numérico. Útil cuando se requieren hacer cálculos adicionales con ese valor.
#'
#' @return
#' @export
#'
#' @examples interes_compuesto_n(100,12,50,12)
interes_compuesto_n <- function(capital,tiempo,monto, periodicidad){

  interes = ((capital+monto)/capital)^(1/(tiempo/periodicidad))-1

  return(interes)

}


#' Cálculo del tiempo
#'
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#' @param capital Valor de la inversión inicial
#' @param interes Tasa de interés compuesto a la que se dejará la inversión inicial
#'
#' @details Función para calcular el tiempo necesario para obtener un monto dado a un cierto tiempo, con una tasa de interés compuesta y capitalización dadas, considerando un capital inicial dado. Regresa una cadena de texto explicando el tiempo necesario.
#'
#' @return
#' @export
#'
#' @examples tiempo_compuesto(150,12,200,12)
tiempo_compuesto<- function(monto, periodicidad,capital,interes ){

  tiempo = log((capital+monto)/capital)/log(1+interes)

  return({

    if (periodicidad ==12){

      paste0("Se requeriran ", round(tiempo, digits = 1), " meses para esta inversión")

    }else if (periodicidad == 1){

      paste0("Se requeriran ", round(tiempo, digits = 1), " años para esta inversión")

    }else if (periodicidad == 365){

      paste0("Se requeriran ", round(tiempo, digits = 1), " días para esta inversión")

    }else{

      paste0("Se requeriran ", round(tiempo, digits = 1), " unidades de tiempo para esta inversión")

    }

  })

}

#' Cálculo del tiempo
#'
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#' @param capital Valor de la inversión inicial
#' @param interes Tasa de interés compuesto a la que se dejará la inversión inicial
#'
#' @details Función para calcular el tiempo necesario para obtener un monto dado a un cierto tiempo, con una tasa de interés compuesta y capitalización dadas, considerando un capital inicial dado. Unicamente devuelve el valor numérico. Útil cuando se requieren hacer cálculos adicionales con ese valor.
#'
#' @return
#' @export
#'
#' @examples tiempo_compuesto_n(150,12,200,12)
tiempo_compuesto_n <- function(monto, periodicidad,capital,interes ){

  tiempo = log((capital+monto)/capital)/log(1+interes)

  return(tiempo)

}

#' Cálculo del capital
#'
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#' @param interes Tasa de interés compuesta a la que se dejará la inversión inicial
#'
#' @details Función para calcular el capital necesario para obtener un monto dado a un cierto tiempo, con una tasa de interés compuesta y capitalización dadas. Regresa una cadena de texto explicando el valor necesario.
#'
#' @return
#' @export
#'
#' @examples capital_compuesto(12,150,12,0.12)
capital_compuesto <- function(tiempo,valor_final, periodicidad,interes){

  capital <- valor_final/((1+interes)^(tiempo/periodicidad))

  return(paste0("El capital necesario para obtener un valor final de ",valor_final," debe ser de ",round(digits = 2,capital)))

}


#' Cálculo del capital
#'
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param monto Monto esperado a recibir al final del plazo
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#' @param interes Tasa de interés compuesta a la que se dejará la inversión inicial
#'
#' @details Función para calcular el capital necesario para obtener un monto dado a un cierto tiempo, con una tasa de interés compuesta y capitalización dadas. Unicamente devuelve el valor numérico. Útil cuando se requieren hacer cálculos adicionales con ese valor.
#'
#' @return
#' @export
#'
#' @examples capital_compuesto_n(12,150,12,0.12)
capital_compuesto_n <- function(tiempo,valor_final, periodicidad,interes){

  capital <- valor_final/((1+interes)^(tiempo/periodicidad))

  return(round(capital))

}



#' Capitalización anual
#'
#' @param deposito Valor de los depósitos constantes que se deben hacer en el periodo dado
#' @param interes Valor de la tasa de interés a lo largo del periodo
#' @param periodicidad Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param tiempo_anual Número de periodos distribuidos con amplitud anual
#'
#' @details La capitalización consiste en depositar de manera constante un capital, el cuál irá aumentando y generando un monto final con una ganancia por una tasa de interés dada. La función regresa una cadena de texto explicando ese valor.
#'
#' @return
#' @export
#'
#' @examples anualidad(20000,0.12,2,4.5)
anualidad <- function(deposito,interes,periodicidad,tiempo_anual){

  monto = deposito*((1+(interes/periodicidad))^(periodicidad*tiempo_anual)-1)/(interes/periodicidad)

  return(paste0("El monto total al final del periodo será de $",round(monto, digits = 2) ))

}

#' Capitalización anual
#'
#' @param deposito Valor de los depósitos constantes que se deben hacer en el periodo dado
#' @param interes Valor de la tasa de interés a lo largo del periodo
#' @param periodicidad Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param tiempo_anual Número de periodos distribuidos con amplitud anual
#'
#' @details La capitalización consiste en depositar de manera constante un capital, el cuál irá aumentando y generando un monto final con una ganancia por una tasa de interés dada. La función regresa únicamente el valor numérico. Útil cuando se quieren hacer cálculos con ese valor.
#'
#' @return
#' @export
#'
#' @examples anualidad_n(20000,0.12,2,4.5)
anualidad_n <- function(deposito,interes,periodicidad,tiempo_anual){

  monto = deposito*((1+(interes/periodicidad))^(periodicidad*tiempo_anual)-1)/(interes/periodicidad)

  return(monto)

}


#' Gráfico de interés simple
#'
#' @param capital Valor de la inversión inicial
#' @param tiempo Unidades de tiempo (días, meses, años) en las que se dejará la inversión actual
#' @param interes Tasa de interés simple a la que se dejará la inversión inicial
#' @param periodicidad Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.
#'
#' @details Generación automática de un gráfico mediante Ggplot2, con el monto obtenido al final del periodo, más el capital inicial. La función además devuelve un dataframe con los montos totales para cada periodo (interés simple).
#'
#' @return
#' @export
#'
#' @examples
vfsimple_plot <- function(capital,tiempo,interes, periodicidad){


  Monto <- capital*interes*(tiempo/periodicidad)

  x <- dplyr::tibble(
    value = c(capital),
    periodo = c("t_0")
  )


  for (i in 1:tiempo){


    Monto <- capital*interes*(1/periodicidad)


    y <- dplyr::tibble(
      value = c(capital+Monto*i),
      periodo = c(paste0("t_",i))
    )


    x <- x |>
      dplyr::bind_rows(y)

  }

  int <<- x[-c(i+1),]


  p <-

    int |>
    ggplot2::ggplot() + ggplot2::aes(x = forcats::fct_inorder(periodo),
                                     y = value, group = 1) +
    ggplot2::geom_line()

  return(p)


}
