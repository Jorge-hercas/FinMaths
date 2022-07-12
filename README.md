# FinMaths

![logo_small](https://user-images.githubusercontent.com/70007745/178418733-8c1a9240-8fd2-4723-996a-09f1636d5a69.png)



La finalidad de este paquete es poder simplificar en la medida de lo posible los cálculos realizados para las áreas financieras, poder generarlos en masa y aumentar la eficiencia de éstos.

Instalación de la versión de desarrollador:

```
devtools::install_github("Jorge-hercas/FinMaths")
```

Ejemplo de uso para un caso práctico utilizando cadenas de texto:

```
library(FinMaths)
library(dplyr)
tibble(
  capital = sample(100:200,20,replace = TRUE),
  tiempo = sample(1:24,20,replace = TRUE),
  interes = sample((1:8)/32,20,replace = TRUE),
  periodicidad = sample(1:365,20,replace = TRUE)
) |> 
  mutate(
    inversion_tasa_simple = valor_futuro_simple(capital,tiempo,interes,periodicidad),
    inversion_tasa_comp = valor_futuro_compuesto(capital,tiempo,interes,periodicidad)
  )

# A tibble: 20 x 5
   capital tiempo interes periodicidad inversion_tasa_simple                            
     <int>  <int>   <dbl>        <int> <chr>                                            
 1     181      9  0.219            72 El valor futuro con tu inversión será de $185.949
 2     149     20  0.125           254 El valor futuro con tu inversión será de $150.467
 3     110      6  0.0938           69 El valor futuro con tu inversión será de $110.897
 4     135     14  0.188            83 El valor futuro con tu inversión será de $139.27 
 5     171     24  0.0625          282 El valor futuro con tu inversión será de $171.91 
 6     124     22  0.219           198 El valor futuro con tu inversión será de $127.014
 7     156      4  0.0312          319 El valor futuro con tu inversión será de $156.061
 8     107     13  0.0312          244 El valor futuro con tu inversión será de $107.178
 9     189      7  0.0625          105 El valor futuro con tu inversión será de $189.787
10     151      2  0.0938          259 El valor futuro con tu inversión será de $151.109
11     195     12  0.0312          257 El valor futuro con tu inversión será de $195.285
12     164     21  0.0938          261 El valor futuro con tu inversión será de $165.237
13     118     21  0.156            82 El valor futuro con tu inversión será de $122.722
14     187     12  0.156           120 El valor futuro con tu inversión será de $189.922
15     110      2  0.0625           18 El valor futuro con tu inversión será de $110.764
16     164      9  0.125           118 El valor futuro con tu inversión será de $165.564
17     101     22  0.219           345 El valor futuro con tu inversión será de $102.409
18     164     15  0.188           245 El valor futuro con tu inversión será de $165.883
19     179     17  0.219           233 El valor futuro con tu inversión será de $181.857
20     155      7  0.219           223 El valor futuro con tu inversión será de $156.064
```

Ejemplo de uso para un caso práctico utilizando únicamente el resultado numérico:

```
library(FinMaths)
library(dplyr)
tibble(
  capital = sample(100:200,20,replace = TRUE),
  tiempo = sample(1:24,20,replace = TRUE),
  interes = sample((1:8)/32,20,replace = TRUE),
  periodicidad = sample(1:365,20,replace = TRUE)
) |> 
  mutate(
    inversion_tasa_simple = valor_futuro_simple_n(capital,tiempo,interes,periodicidad),
    inversion_tasa_comp = valor_futuro_compuesto_n(capital,tiempo,interes,periodicidad)
  )

# A tibble: 20 x 6
   capital tiempo interes periodicidad inversion_tasa_simple inversion_tasa_comp
     <int>  <int>   <dbl>        <int>                 <dbl>               <dbl>
 1     173     23  0.0938          174                  175.                175.
 2     130     23  0.0938           15                  149.                149.
 3     185      5  0.125            51                  187.                187.
 4     133     22  0.156           187                  135.                135.
 5     123     10  0.25            297                  124.                124.
 6     179     18  0.219            79                  188.                187.
 7     175     15  0.219           206                  178.                178.
 8     180     12  0.0625          204                  181.                181.
 9     141      8  0.156           200                  142.                142.
10     166     16  0.188           184                  169.                168.
11     195      2  0.0625          104                  195.                195.
12     183      9  0.125           338                  184.                184.
13     186     13  0.125           358                  187.                187.
14     145      2  0.0312          291                  145.                145.
15     184     24  0.0625          275                  185.                185.
16     198     21  0.219           119                  206.                205.
17     152     14  0.188           240                  154.                154.
18     173      5  0.188           361                  173.                173.
19     145      8  0.219           307                  146.                146.
20     163      7  0.188           242                  164.                164.
```



