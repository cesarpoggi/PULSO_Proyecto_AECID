library(dplyr)
library(readxl)
library(labelled)
library(haven)

ForLabels <- read_excel("ASE_OG.xls", sheet = "Base")
ASE <- read_excel("ASE_OG.xls", sheet = "Base", skip = 2)

ForLabels <- ForLabels[c(1,2),]

first_row_vector = as.character(ForLabels[1, ])



#Crear variable combinada de preguntas 40 y 43 sobre NIVEL EDUCATIVO

ASE$premergeNEusted <- ASE$P189_1 #41 nivel educativo ENTREVISTADO
ASE$premergeNEusted[ASE$P191_1 == 3 | ASE$P191_1 == 8 | ASE$P191_1 == 9 ] <- NA

ASE$premergeNEotro <- ASE$P192_1 #44 nivel educativo de la persona que mas aporta (OTRO)
ASE$premergeNEotro[ASE$P191_1 == 1 | ASE$P191_1 == 2] <- NA

ASE$NE_1 <- rowSums(ASE[,c("premergeNEusted", "premergeNEotro")], na.rm=TRUE) #nivel educativo MERGE



#Crear variable combinada de preguntas 41 y 44 sobre SEGUROS DE SALUD

ASE$premergeSSusted <- ASE$P190_1 #41 seguro salud usted
ASE$premergeSSusted[ASE$P191_1 == 3 | ASE$P191_1 == 8 | ASE$P191_1 == 9 ] <- NA

ASE$premergeSSotro <- ASE$P193_1 #44 seguro de salud de la persona que mas aporta (otro)
ASE$premergeSSotro[ASE$P191_1 == 1 | ASE$P191_1 == 2] <- NA

ASE$SS_1 <- rowSums(ASE[,c("premergeSSusted", "premergeSSotro")], na.rm=TRUE) #seguro de salud MERGE

#Eliminar variables mergeadas creadas (4 en total, nos quedamos con las dos generales)

ASE <- ASE[-c(232,233,235,236)]



ASE_1<-
  ASE %>% 
  mutate(
    #Se selecciona las variables de cruce 
    #1.SEXO
    sexo = case_when(
      P10_1 == 1 ~ "Hombre", 
      TRUE ~ "Mujer"),
    
    #2.EDAD
    edad = case_when(
      P11_1 < 30 ~ "18-29", 
      P11_1 >= 30 & P11_1 < 45 ~ "30-44", 
      TRUE ~ "45-70"),
    
    #3.Lima y Regiones
    # #2 casos vacíos
    #norte:CAJAMARCA,LAMBAYEQUE,PIURA,LA LIBERTAD, NORTE =5
    #centro: ANCASH, JUNIN, HUANUCO, HUANCAVELICA, AYACUCHO, ICA =6 
    #sur: AREQUIPA, TACNA, CUSCO, SUR, PUNO = 4
    #oriente: LORETO, UCAYALI, SAN MARTIN =3
    #Lima metropolitana: LIMA, CALLAO = 2
    region = P215_1,
    aregion = factor(case_when(
      region == "CAJAMARCA" | region == "LAMBAYEQUE" | region == "PIURA" | region == "LA LIBERTAD" | region == "NORTE" ~ "Norte", 
      
      region == "AREQUIPA" | region == "TACNA" | region == "CUSCO" | region == "SUR" | region == "PUNO" ~ "Sur", 
      
      region == "ANCASH" | region == "JUNIN" | region == "HUANUCO" | region == "HUANCAVELICA" | region == "AYACUCHO" | region == "ICA" ~ "Centro", 
      
      region == "LORETO" | region == "UCAYALI" | region == "SAN MARTIN" ~ "Oriente", 
      
      region == "LIMA" | region == "CALLAO" ~ "Lima/ Callao", 
      
      TRUE ~ "NS/NR"), 
      
      levels = c ("Norte", "Sur", "Centro", "Oriente", "Lima/ Callao")),
    
    #4.Grado educativo
    #4.1. 189 -> encuestado
    educa1 = factor(case_when(
      P189_1 == 1 | P189_1 == 2 | P189_1 == 3 | P189_1 == 4 ~ "Primaria o menor",
      P189_1 == 5 | P189_1 == 6 ~ "Secundaria",
      P189_1 == 7 | P189_1 == 8 | P189_1 == 9 | P189_1 == 10 ~ "Superior técnica/ Universitaria",
      P189_1 == 11 ~ "Posgrado", 
      TRUE ~ "NS/NR"), 
      levels = c("Primaria o menor", "Secundaria", "Superior técnica/ Universitaria", "Posgrado", "NS/NR")),
    
    
    #4.2. 192 -> educa2 persona que oporta más ingresos (pasará a ser NE_1que se creó arriba - preguntas 40 y 43)
    
    #educa2a -> etiquetas agrupadas sobre NE
    
    #educa2 = as.numeric(P192_1), 
    educa2a = case_when(
      NE_1== 1 ~ "Sin nivel",
      NE_1== 2 ~ "Inicial",
      NE_1== 3 | NE_1== 4 ~ "Primaria",
      NE_1== 5 | NE_1== 6 ~ "Secundaria",
      NE_1== 7 | NE_1== 8 ~ "Superior técnica",
      NE_1== 9 | NE_1== 10 ~ "Superior universitaria",
      NE_1== 11 ~ "Posgrado", 
      TRUE ~ "NS/NR"
    ),
    
    n1 = case_when(
      NE_1== 1 ~ 0,
      NE_1>1 & NE_1<= 5 ~ 2,
      NE_1== 6 ~ 3,
      NE_1== 7 ~ 4, 
      NE_1== 8 ~ 5,
      NE_1== 9 ~ 7,
      NE_1== 10 ~ 9,
      NE_1== 11 ~ 10,
      TRUE ~ 0
    ),
    
    
    #5.Mención de acoso político
    #99
    map = as.numeric(ASE$P99_1),
    mape = factor(case_when(
      map == 1 ~ "Un político hombre",
      map == 2 ~ "Un político mujer",
      map == 3 ~ "Ambos por igual",
      map == 4 ~ "A ninguno",
      TRUE ~ "NS/NR"
    ),
    levels = c("Un político hombre", "Un político mujer", "Ambos por igual", "A ninguno", "NS/NR")),
    
    #NSE
    #N1: educa2p persona que oporta + ingresos #educa2p
    #N2: Para obtener el puntaje de N2 se deben sumar los puntajes asignados a 
    #los artefactos o servicios con los que cuenta el hogar la persona encuestada. #n2
    #P194_1	P195_1	P196_1	P197_1	P198_1	P199_1
    
    art1 = case_when(as.numeric(P194_1) == 1~ 3.5,TRUE~0),
    art2 = case_when(as.numeric(P195_1) == 1~ 3.5,TRUE~0),
    art3 = case_when(as.numeric(P196_1) == 1~ 3.5,TRUE~0),
    art4 = case_when(as.numeric(P197_1) == 1~ 3.5,TRUE~0),
    art5 = case_when(as.numeric(P198_1) == 1~ 3.5,TRUE~0),
    art6 = case_when(as.numeric(P199_1) == 1~ 3.5,TRUE~0),
    
    n2 = rowSums(across(c(art1, art2, art3, art4, art5, art6))),
    
    
    #N3: material predominante de los pisos #n3
    mpp = as.numeric(P188_1),
    
    #aca antes habia un error, mezclaban con educa2
    n3 = case_when(mpp == 1 ~ 9, 
                   mpp == 2 ~ 7, 
                   mpp == 3 ~ 6, 
                   mpp == 4 ~ 3, 
                   mpp == 5 ~ 0, 
                   TRUE ~ 0),
    
    #N4: n°habitaciones / n°personas que viven en el hogar
    #n4 -> 35/#36
    
    # hxm = P35_1/P36_1, #por que dividía estas variables?
    hxm = P184_1/P185_1, 
    
    n4 = case_when(
      hxm >= 1 ~ 5, 
      hxm < 1 & hxm >= 0.75 ~ 4, 
      hxm < 0.75 & hxm >=0.5 ~ 3,
      hxm < 0.5 & hxm >=0.25 ~ 2,
      hxm < 0.25 & hxm > 0 ~ 1,
      TRUE ~ 0),
    
    #n5: seguros de salud del jh (sea el entrevistado u otro)
    
    #ss= as.numeric(P193_1), <- pasará a ser SS_1que se creó arriba - preguntas 41 y 44
    n5 = case_when(
      SS_1== 1 ~ 1, 
      SS_1== 2 ~ 2, 
      SS_1== 3 ~ 2, 
      SS_1== 4 ~ 3, 
      SS_1== 5 ~ 3, 
      SS_1== 6 ~ 4, 
      SS_1== 7 ~ 5, 
      TRUE ~ 0),
    
    ###NSE##
    nsep = rowSums(across(c(n1, n2, n3, n4, n5))),
    
    nse = case_when(
      nsep < 10 ~ "NSE E", 
      nsep >= 10 & nsep < 22 ~ "NSE D", 
      nsep >= 22 & nsep < 26 ~ "NSE C2", 
      nsep >= 26 & nsep < 36 ~ "NSE C1", 
      nsep >= 36 & nsep < 41 ~ "NSE B2", 
      nsep >= 41 & nsep < 46 ~ "NSE B1", 
      nsep >= 46 & nsep < 49 ~ "NSE A2", 
      nsep >= 49 & nsep <= 50 ~ "NSE A1", 
      TRUE ~ "NS/NR"),
    
    nse2 = case_when(
      nsep < 22 ~ "D/E", 
      nsep >= 22 & nsep < 36 ~ "C", 
      nsep >= 36 & nsep <= 50 ~ "A/B", 
      TRUE ~ "NS/NR"),
  ) %>%  
  
  #RENOMBRANDO VARIABLES
  sjlabelled::var_labels(
    sexo = "Sexo",
    edad = "Edad",
    aregion = "Lima y regiones",
    educa1 = "Grado educativo de Encuestado",
    mape = "Mención de acoso político",
    nse2 = "Nivel socio-económico"
  ) %>%  
  
  mutate(
    #ETIQUETAMIENTO Y ORDENAMIENTO DE NIVELES DE VARIABLES
    across(c(P12_1),
           ~ factor(case_when(
             . == 1 ~ "Muy interesado/a", 
             . == 2 ~ "Interesado/a", 
             . == 3 ~ "Poco interesado/a", 
             . == 4 ~ "Nada interesado/a", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Muy interesado/a", "Interesado/a", "Poco interesado/a", "Nada interesado/a","NS/NR"))),
    
    across(c(P13_1),
           ~ factor(case_when(
             . == 1 ~ "Muy informado/a", 
             . == 2 ~ "Informado/a", 
             . == 3 ~ "Poco informado/a", 
             . == 4 ~ "Nada informado/a", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Muy informado/a", "Informado/a", "Poco informado/a", "Nada informado/a", "NS/NR"))),
    
    across(c(P14_1, P15_1, P16_1, P17_1, P18_1, P19_1, P20_1, P21_1, P22_1), 
           ~ factor(case_when(
             . == 1 ~ "Muy de acuerdo", 
             . == 2 ~ "De acuerdo", 
             . == 3 ~ "En desacuerdo", 
             . == 4 ~ "Muy en desacuerdo", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Muy de acuerdo", "De acuerdo", "En desacuerdo", "Muy en desacuerdo","NS/NR"))),
    
    #p23_1 en sintaxis original tenia problemas
    across(c(P23_1), 
           ~ factor(case_when(
             . == 1 ~ "Se puede confiar en la mayoría de las personas", 
             . == 2 ~ "Es muy difícil confiar en los demás", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Se puede confiar en la mayoría de las personas", "Es muy difícil confiar en los demás", "NS/NR"))),
    
    #24 a 34
    across(c(P24_1, P25_1, P26_1, P27_1, P28_1, P29_1, P30_1, P31_1, P32_1, P33_1, P34_1), 
           ~ factor(case_when(
             . == 1 ~ "Mucha", 
             . == 2 ~ "Alguna", 
             . == 3 ~ "Poca", 
             . == 4 ~ "Ninguna", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Mucha", "Alguna", "Poca", "Ninguna","NS/NR"))),
    
    #35-46
    across(c(P35_1, P36_1, P37_1, P38_1, P39_1, P40_1, P41_1, P42_1, P43_1, P44_1, P45_1, P46_1), 
           ~ factor(case_when(
             . == 1 ~ "Sí pertenece", 
             . == 2 ~ "No pertenece", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Sí pertenece", "No pertenece", "NS/NR"))),
    
    #47-56
    across(c(P47_1, P48_1, P49_1, P50_1, P51_1, P52_1, P53_1, P54_1, P55_1, P56_1), 
           ~ factor(case_when(
             . == 1 ~ "Ha hecho", 
             . == 2 ~ "Podría hacer", 
             . == 3 ~ "Nunca lo haría", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Ha hecho", "Podría hacer", "Nunca lo haría","NS/NR"))),
    
    across(c(P57_1), 
           ~ factor(case_when(
             . == 1 ~ "La democracia es preferible a cualquier otra forma de gobierno", 
             . == 2 ~ "A veces un gobierno autoritario o una dictadura puede ser preferible a un gobierno democrático", 
             . == 3 ~ "Me da lo mismo un tipo de gobierno u otro", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("La democracia es preferible a cualquier otra forma de gobierno", 
                        "A veces un gobierno autoritario o una dictadura puede ser preferible a un gobierno democrático", 
                        "Me da lo mismo un tipo de gobierno u otro",
                        "NS/NR"))),
    
    across(c(P58_1), 
           ~ factor(case_when(
             . == 1 ~ "Muy satisfecho", 
             . == 2 ~ "Satisfecho", 
             . == 3 ~ "Insatisfecho", 
             . == 4 ~ "Muy insatisfecho", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Muy satisfecho", "Satisfecho", "Insatisfecho", "Muy insatisfecho", "NS/NR"))),
    
    across(c(P59_1), 
           ~ factor(case_when(
             . == 1 ~ "Las listas de candidatos deben presentar intercaladamente candidatos hombres y candidatas mujeres", 
             . == 2 ~ "Las listas de candidatos deben presentar 30% de candidatas mujeres", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Las listas de candidatos deben presentar intercaladamente candidatos hombres y candidatas mujeres", 
                        "Las listas de candidatos deben presentar 30% de candidatas mujeres",
                        "NS/NR"))),
    
    #60-99
    across(c(P60_1, P61_1, P62_1, P63_1, P64_1, P65_1, P66_1, P67_1, P68_1, P69_1, 
             P70_1, P71_1, P72_1, P73_1, P74_1, P75_1, P76_1, P77_1, P78_1, P79_1, 
             P80_1, P81_1, P82_1, P83_1, P84_1, P85_1, P86_1, P87_1, P88_1, P89_1, 
             P90_1, P91_1, P92_1, P93_1, P94_1, P95_1, P96_1, P97_1, P98_1, P99_1), 
           ~ factor(case_when(
             . == 1 ~ "Un político hombre", 
             . == 2 ~ "Una política mujer", 
             . == 3 ~ "Ambos por igual", 
             . == 4 ~ "Ninguno", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Un político hombre", "Una política mujer", "Ambos por igual", "Ninguno","NS/NR"))),
    
    #100-103
    across(c(P100_1, P101_1, P102_1, P103_1), 
           ~ factor(case_when(
             . == 1 ~ "Un hombre", 
             . == 2 ~ "Una mujer", 
             . == 3 ~ "Ambos por igual", 
             . == 4 ~ "Ninguno", 
             . == 8 | . == 9 | . == 99 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Un hombre", "Una mujer", "Ambos por igual", "Ninguno","NS/NR"))),
    
    #104-106 // 143-145 // 160-169 // 172-181 // 187 // 194-199
    across(c(P104_1, P105_1, P106_1,
             
             P143_1, P144_1, P145_1,
             
             P160_1, P161_1, P162_1, P163_1, P164_1, P165_1, P166_1, P167_1, P168_1, P169_1,
             
             P172_1, P173_1, P174_1, P175_1, P176_1, P177_1, P178_1, P179_1, P180_1, P181_1,
             
             P187_1, P194_1, P195_1, P196_1, P197_1, P198_1, P199_1
    ), 
    ~ factor(case_when(
      . == 1 ~ "Sí", 
      . == 2 ~ "No", 
      . == 8 | . == 9 | . == 99 ~ "NS/NR", 
      TRUE ~ "NS/NR"), 
      
      levels = c("Sí", "No", "NS/NR"))),
    
    #107 -> respuesta multiple
    P107.1 = case_when(P107_1 == 1| P107_2 == 1 ~ "No concordaban con mi posición e ideas políticas",
                       TRUE ~ NA_character_),
    P107.2 = case_when(P107_1 == 2| P107_2 == 2 ~ "No tenían buenas propuestas frente a los problemas que me interesan",
                       TRUE ~ NA_character_),
    P107.3 = case_when(P107_1 == 3| P107_2 == 3 ~ "Por el partido político al que pertenecían",
                       TRUE ~ NA_character_),
    P107.4 = case_when(P107_1 == 4| P107_2 == 4 ~ "No estaban preparadas para ejercer esos cargos",
                       TRUE ~ NA_character_),
    P107.5 = case_when(P107_1 == 5| P107_2 == 5 ~ "No me generaban confianza",
                       TRUE ~ NA_character_),
    P107.6 = case_when(P107_1 == 6| P107_2 == 6 ~ "No tenían posibilidades de ganar",
                       TRUE ~ NA_character_),
    P107.7 = case_when(P107_1 == 7| P107_2 == 7 ~ "No había candidatas mujeres",
                       TRUE ~ NA_character_),
    P107.8 = case_when(P107_1 == 8| P107_2 == 8 ~ "Otra razón",
                       TRUE ~ NA_character_),
    P107.9 = case_when(P107_1 == 88| P107_2 == 99 ~ "NS/NR",
                       TRUE ~ NA_character_),
    P107.10 = case_when(P107_1 == 999| P107_2 == 999 ~ "No Aplica",
                        TRUE ~ NA_character_),
    
    
    #108 -> Otra razón: pregunta abierta
    
    #109
    across(c(P109_1), 
           ~ factor(case_when(
             . == 1 ~ "Por la falta de interés de las mujeres en la política", 
             . == 2 ~ "Por la falta de respaldo de los partidos y agrupaciones políticas", 
             . == 3 ~ "Por la falta de preparación de las mujeres para esos cargos", 
             . == 4 ~ "Porque el electorado prefiere votar por hombres que por mujeres", 
             . == 5 ~ "Otra razón", 
             . == 8 | . == 9 | . == 99 ~ "NS/NR", 
             TRUE ~ NA_character_ ),
             
             levels = c("Por la falta de interés de las mujeres en la política", 
                        "Por la falta de respaldo de los partidos y agrupaciones políticas", 
                        "Por la falta de preparación de las mujeres para esos cargos", 
                        "Porque el electorado prefiere votar por hombres que por mujeres", 
                        "Otra razón", 
                        "NS/NR"))),
    
    #110 -> Otra razón: pregunta abierta
    
    #111-121 // 130-142 // P146_1 // 149-155
    across(c(P111_1, P112_1, P113_1, P114_1, P115_1, P116_1, P117_1, P118_1, P119_1, P120_1, P121_1,
             
             P130_1, P131_1, P132_1, P133_1, P134_1, P135_1, P136_1, P137_1, P138_1, P139_1, P140_1,
             P141_1, P142_1,
             
             P146_1,
             
             P149_1, P150_1, P151_1, P152_1, P153_1, P154_1, P155_1
    ), 
    ~ factor(case_when(
      . == 1 ~ "Muy de acuerdo", 
      . == 2 ~ "De acuerdo", 
      . == 3 ~ "En desacuerdo", 
      . == 4 ~ "Muy en desacuerdo", 
      . == 8 | . == 9 | . == 99 ~ "NS/NR", 
      TRUE ~ "NS/NR"), 
      
      levels = c("Muy de acuerdo", "De acuerdo", "En desacuerdo", "Muy en desacuerdo", "NS/NR"))),
    
    #122
    across(c(P122_1), 
           ~ factor(case_when(
             . == 1 ~ "Mucho más importante para el hombre", 
             . == 2 ~ "Algo más importante para el hombre", 
             . == 3 ~ "Algo más importante para la mujer", 
             . == 4 ~ "Mucho más importante para la mujer",
             . == 5 ~ "Para ambos igual",
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Mucho más importante para el hombre", 
                        "Algo más importante para el hombre", 
                        "Algo más importante para la mujer", 
                        "Mucho más importante para la mujer", 
                        "Para ambos igual", 
                        "NS/NR"))),
    
    #123-129
    across(c(P123_1, P124_1, P125_1, P126_1, P127_1, P128_1, P129_1), 
           ~ factor(case_when(
             . == 1 ~ "Las mujeres son mucho más apropiadas", 
             . == 2 ~ "Las mujeres son algo más apropiadas", 
             . == 3 ~ "Por igual las mujeres y los hombres", 
             . == 4 ~ "Los hombres son algo más apropiados",
             . == 5 ~ "Los hombres son mucho más apropiados",
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Las mujeres son mucho más apropiadas", "Las mujeres son algo más apropiadas", "Por igual las mujeres y los hombres", "Los hombres son algo más apropiados", "Los hombres son mucho más apropiados", "NS/NR"))),
    
    #147-148
    across(c(P147_1, P148_1), 
           ~ factor(case_when(
             . == 1 ~ "Mucho", 
             . == 2 ~ "Algo", 
             . == 3 ~ "Poco", 
             . == 4 ~ "Nada",
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Mucho", "Algo", "Poco", "Nada", "NS/NR"))),
    
    #156-159
    across(c(P156_1, P157_1, P158_1, P159_1), 
           ~ factor(case_when(
             . == 1 ~ "Sí se justifica", 
             . == 2 ~ "No se justifica", 
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Sí se justifica", "No se justifica", "NS/NR"))),
    
    #170 -> otros: pregunta abierta
    
    #171
    across(c(P171_1), 
           ~ factor(case_when(
             . == 1 ~ "Diario", 
             . == 2 ~ "Varias veces por semana", 
             . == 3 ~ "Semanal", 
             . == 4 ~ "Varias veces al mes",
             . == 5 ~ "Mensual",
             . == 6 ~ "Pocas veces al año",
             . == 8 | . == 9 ~ "NS/NR", 
             TRUE ~ "NS/NR"), 
             
             levels = c("Diario", "Varias veces por semana", "Semanal", "Varias veces al mes", "Mensual", "Pocas veces al año", "NS/NR"))),
    
    #182 -> Otros: pregunta abierta
    
    
    # CREACION DE MÁS VARIABLES
    
    #183 -> respuesta multiple
    
    P183.1 = case_when(P183_1 == 1 |
                         P183_2 == 1 |
                         P183_3 == 1 ~ "Persona y/o familiar",
                       TRUE ~ NA_character_),
    P183.2 = case_when(P183_1 == 2 |
                         P183_2 == 2 |
                         P183_3 == 2 ~ "Laboral/ trabajo", 
                       TRUE ~ NA_character_),
    P183.3 = case_when(P183_1 == 3 |
                         P183_2 == 3 |
                         P183_3 == 3 ~ "Estudios", 
                       TRUE ~ NA_character_),
    P183.4 = case_when(P183_1 == 4 |
                         P183_2 == 4 |
                         P183_3 == 4 ~ "Actividad política/ opiniones políticas",
                       TRUE ~ NA_character_),
    P183.5 = case_when(P183_1 == 5 |
                         P183_2 == 5 |
                         P183_3 == 5 ~ "Actividad pública", 
                       TRUE ~ NA_character_),
    P183.6 = case_when(P183_1 == 6 |
                         P183_2 == 6 |
                         P183_3 == 6 ~ "Activismo social", 
                       TRUE ~ NA_character_),
    P183.7 = case_when(P183_1 == 7 |
                         P183_2 == 7 |
                         P183_3 == 7 ~ "Otros", 
                       TRUE ~ NA_character_),
    P183.8 = case_when(P183_1 == 8 |
                         P183_2 == 8 |
                         P183_3 == 8 |
                         P183_1 == 9 |
                         P183_2 == 9 |
                         P183_3 == 9 ~ "NS/NR", 
                       TRUE ~ NA_character_),
    P183.8 = case_when(P183_1 == 99 |
                         P183_2 == 99 |
                         P183_3 == 99 ~ "No Aplica", 
                       TRUE ~ NA_character_),
    
    
    #184-186: preguntas numéricas
    
    #184, 186
    across(c(P184_1, P186_1), 
           ~ factor(case_when(
             . < 4 ~ "0 a 3", 
             . < 8 ~ "4 a 7", 
             . < 12 ~ "8 a 11", 
             TRUE ~ NA_character_), 
             
             levels = c("0 a 3", "4 a 7", "8 a 11"))),
    
    #185
    across(c(P185_1), 
           ~ factor(case_when(
             . < 6 ~ "0 a 5", 
             . < 11 ~ "6 a 10", 
             . < 16 ~ "11 a 15", 
             TRUE ~ NA_character_), 
             
             levels = c("0 a 5", "6 a 10", "11 a 15"))),
    
    #188
    across(c(P188_1), 
           ~ factor(case_when(
             . == 1 ~ "Parquet o madera pulida y similares; porcelanato, alfombra, mármol", 
             . == 2 ~ "Laminado tipo madera, láminas asfálticas o similares", 
             . == 3 ~ "Losetas / terrazos, mayólicas, cerámicos, vinílicos, mosaico o similares",
             . == 4 ~ "Cemento sin pulir o pulido / madera (entablados)/ tapizón",
             . == 4 ~ "Tierra / otro material (arena y tablones sin pulir)",
             . == 8 | . == 9 ~ "NS/NR",
             TRUE ~ "NS/NR"), 
             
             levels = c("Parquet o madera pulida y similares; porcelanato, alfombra, mármol", 
                        "Laminado tipo madera, láminas asfálticas o similares", 
                        "Losetas / terrazos, mayólicas, cerámicos, vinílicos, mosaico o similares", 
                        "Cemento sin pulir o pulido / madera (entablados)/ tapizón", 
                        "Tierra / otro material (arena y tablones sin pulir)", 
                        "NS/NR"))),
    
    #189 -> variable control (?)
    
    #190,192
    across(c(P190_1, P193_1), 
           ~ factor(case_when(
             . == 1 ~ "Seguro integral de salud (SIS)", 
             . == 2 ~ "Seguro universitario", 
             . == 3 ~ "Seguro escolar privado",
             . == 4 ~ "Essalud",
             . == 5 ~ "Seguro FF.AA./ Policiales",
             . == 6 ~ "Entidad prestadora de salud - EPS",
             . == 7 ~ "Seguro privado de salud",
             . == 8 | . == 88 |. == 99 ~ "NS/NR",
             TRUE ~ "NS/NR"), 
             
             levels = c("Seguro integral de salud (SIS)", 
                        "Seguro universitario", 
                        "Seguro escolar privado", 
                        "Essalud", 
                        "Seguro FF.AA./ Policiales", 
                        "Entidad prestadora de salud - EPS", 
                        "Seguro privado de salud", 
                        "NS/NR"))),
    
    #201, 205
    
    across(c(P201_1, P205_1), 
           ~ factor(case_when(
             . == 1 ~ "Sí",
             . == 2 ~ "No"),
             
             levels = c("Sí", "No"))),
    
    #191: variable irrelevante
    
    across(c(P191_1), 
           ~ factor(case_when(
             . == 1 ~ "Usted Mismo",
             . == 2 ~ "Usted y otra persona casi en partes iguales",
             . == 3 ~ "Otra persona",
             . >= 8 ~ "NS/NR"),
             
             levels = c("Usted Mismo",
                        "Usted y otra persona casi en partes iguales",
                        "Otra persona",
                        "NS/NR"))),
    
    #192
    across(c(P192_1), 
           ~ factor(case_when(
             . == 1 | . == 2 ~ "Sin nivel/Inicial",
             . == 3 | . == 4 ~ "Primaria",
             . == 5 | . == 6 ~ "Secundaria",
             . == 7 | . == 8 ~ "Superior técnica",
             . == 9 | . == 10 ~ "Superior universitaria",
             . == 11 ~ "Posgrado",
             . == 88 | . == 99 ~ "NS/NR",
             TRUE ~ "NS/NR"), 
             
             levels = c("Sin nivel/Inicial", 
                        "Primaria", 
                        "Secundaria", 
                        "Superior técnica", 
                        "Superior universitaria", 
                        "Posgrado", 
                        "NS/NR"))),
  )


#Eliminar las respuestas de SÍ la variable P104_1 para casos que nos son del CALLAO

ASE_1$P104_1[ASE_1$P215_1 == "LIMA"] <- NA

#Crear Variable VotoxMujer para eliminar los casos colados en la preguta
#15 o simplemente hacer el código para eliminar (NA) con conectores lógicos
#Elimnar la varaible creada (si se creó)

ASE_1$P107_1[ASE_1$P104_1 == "Sí" | ASE_1$P105_1 == "Sí" | ASE_1$P106_1 == "Sí" ] <- NA
ASE_1$P107_2[ASE_1$P104_1 == "Sí" | ASE_1$P105_1 == "Sí" | ASE_1$P106_1 == "Sí" ] <- NA


#Modificado más arriba la mezcla de No Aplica con No sabe o no responde,
#para las preguntas salto

#Labeling de Variables

DataP1 <- ASE_1[c(1:231)]
DataP2 <- ASE_1[c(232:274)]


ASE_2_Incompleta <- labelled::set_variable_labels(DataP1, .labels = first_row_vector)

ASE_2_Incompleta$row_names <- row.names(ASE_2_Incompleta)
DataP2$row_names <- row.names(DataP2)

Data_2_Completa <- left_join(x = ASE_2_Incompleta, y = DataP2, by = "row_names")
Data_2_Completa <- Data_2_Completa[-c(232)]


write_sav(Data_2_Completa, "ASE_Intento_5.sav")