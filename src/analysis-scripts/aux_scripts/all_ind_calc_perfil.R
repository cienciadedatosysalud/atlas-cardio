database_path <- '../../inputs/data.duckdb'

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

# INDICADORES
## ENFERMEDAD ISQUÉMICA CORONARIA


# Cuidados ambulatorios

### - Enfermedad isquémica cardiaca en la población mayor de 18 años.

### Total

df_eic_global <- data.frame()

df <- dbGetQuery(conn = con, '
  select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL
  ')

list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select año_cd,ccaa_cd,zbs_residencia_cd,sexo_cd, grupo_edad_cd + 1 as grupo_edad_cd, nivel_copago_cd,poblacion_nm from main.poblacion 
  where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL
	')
df_poblacion$sexo_cd <- as.character(df_poblacion$sexo_cd)
df_poblacion$grupo_edad_cd[df_poblacion$grupo_edad_cd > 18] <- 18


df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd,grupo_edad_cd,sexo_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

df_poblacion_ccaa <- df_poblacion %>% group_by(ccaa_cd,sexo_cd,grupo_edad_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs,na.rm = TRUE))

data_csv <- df %>% group_by(ccaa_cd,sexo_cd,grupo_edad_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))

data_csv <- left_join(df_poblacion_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0

data_csv <- left_join(df_poblacion,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))


df_eic_global <- rbind(df_eic_global,data_csv)


df_eic_global$ind <- 'eic'

dbDisconnect(con, shutdown=TRUE)
rm(df_poblacion,df_poblacion_ccaa)

### - Población con EIC vacunada de la gripe el año de estudio.
### - Ecocardiograma el año de estudio. 
### - Determinación/registro de la fracción de eyección el año de estudio. 

df_eic_exams <- data.frame()


df_ind_exams <- data.frame(ind = c('vacuna_gripe','ecocardio'),
                           descr = c('Vacunación antigripal','Ecocardiograma'),
                           cod = c('eic_gripe','eic_eco'))

eic_indicadores_exam <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * FROM main.examenes WHERE examen_cd == '",i,"' and year(examen_dt) == 2023"))
  pobla <- dbGetQuery(conn = con,
                      paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_eic %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  df_ind_exams_ <- df_ind_exams %>% filter(ind %in% i)
  
  df_eic_exams <- rbind(df_eic_exams,data_csv)
  df_eic_exams$ind <- df_ind_exams_$cod
  
  dbDisconnect(con, shutdown=TRUE)            
  return(df_eic_exams)
}


df_eic_exams_all <- lapply(df_ind_exams$ind,eic_indicadores_exam)

df_eic_exams_all <- rbind(df_eic_exams_all[[1]],df_eic_exams_all[[2]])

### - Población con EIC e infarto de miocardio previo.

df_eic_iam_all <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con,
                 paste0("SELECT * FROM main.comorbilidad WHERE comorbilidad_cd == 1"))

pobla <- dbGetQuery(conn = con,
                    paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))

df <- df[!duplicated(df$paciente_id),]

df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  

df_eic <- left_join(df_eic,df,by='paciente_id')

df_eic <- df_eic %>% filter(fecha_diag_comorbilidad_dt <= fecha_enfermedad_dt)

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))

pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))

data_csv <- df_eic %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))

data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0

data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

df_eic_iam_all <- rbind(df_eic_iam_all,data_csv)


df_eic_iam_all$ind <- 'eic_iam'

dbDisconnect(con, shutdown=TRUE)  




### - Registro de tensión arterial (TA).
### - Determinación de colesterol LDL.      


df_eic_test <- data.frame()


df_ind_test <- data.frame(ind = c('pas','ldl'),
                          descr = c('Tensión arterial','Colesterol LDL (mg/dL)'),
                          cod = c('eic_ta','eic_ldl'))

eic_indicadores_test <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  
  if(i == 'pas'){
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd IN ('pas','pad') and year(test_dt) == 2023"))
    df <- df %>%
      group_by(paciente_id) %>%
      filter(all(c("pas", "pad") %in% test_cd)) %>%
      ungroup()
  }else{
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd == '",i,"' and year(test_dt) == 2023"))
  }
  pobla <- dbGetQuery(conn = con,
                      paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_eic %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  df_eic_test <- rbind(df_eic_test,data_csv)
  
  df_ind_test_ <- df_ind_test %>% filter(ind %in% i)
  
  df_eic_test$ind <- df_ind_test_$cod
  
  dbDisconnect(con, shutdown=TRUE)   
  return(df_eic_test)
}


df_eic_test_all <-lapply(df_ind_test$ind,eic_indicadores_test)  

df_eic_test_all <- rbind(df_eic_test_all[[1]],df_eic_test_all[[2]])

### - TA <140/90 mmHg (en la última determinación).
### - Determinación de colesterol LDL menor de 70 mg/dl 
### - Determinación de colesterol LDL menor de 55 mg/dl

df_eic_test_nm <- data.frame()

df_ind_test <- data.frame(ind = c('pas','ldl','ldl'),
                          descr = c('Tensión arterial','Colesterol LDL menor de 70 (mg/dL)','Colesterol LDL menor de 55 (mg/dL)'),
                          cod = c('eic_ta_ctrl','eic_ldl70','eic_ldl55'),
                          nm=c(140,70,55))

eic_indicadores_test_nm <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_ind_test_ <- df_ind_test %>% filter(cod %in% i)
  if(df_ind_test_$cod == 'eic_ta_ctrl'){
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd IN ('pas','pad') and year(test_dt) == 2023"))
    df <- df %>%
      group_by(paciente_id) %>%
      filter(all(c("pas", "pad") %in% test_cd)) %>%
      ungroup()
    df <- df %>% 
      group_by(paciente_id, test_cd) %>%
      filter(test_dt == max(test_dt)) %>%
      ungroup()
    df1 <- df %>% filter(test_cd == 'pas' & test_resultado_nm < 140)
    df2 <- df %>% filter(paciente_id %in% df1$paciente_id) %>% filter(test_cd == 'pad' & test_resultado_nm < 90)
    df <- df %>% filter(paciente_id %in% df2$paciente_id)
  }else{
    # df <- dbGetQuery(conn = con,
    #                  paste0("SELECT * FROM main.test WHERE test_cd == '",df_ind_test_$ind,"' and test_resultado_nm < '",df_ind_test_$nm,"' and year(test_dt) == 2023"))
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd == '",df_ind_test_$ind,"' and year(test_dt) == 2023"))
    df <- df %>%
      group_by(paciente_id, test_cd) %>%
      filter(test_dt == max(test_dt)) %>%
      ungroup()
    df <- df %>% filter(test_resultado_nm < df_ind_test_$nm)
  }
  pobla <- dbGetQuery(conn = con,
                      paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_eic %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  
  df_eic_test_nm <- rbind(df_eic_test_nm,data_csv)
  
  df_eic_test_nm$ind <- df_ind_test_$cod
  
  dbDisconnect(con, shutdown=TRUE)   
  return(df_eic_test_nm)
}




df_eic_test_all_nm <- lapply(df_ind_test$cod,eic_indicadores_test_nm) 

df_eic_test_all_nm <- rbind(df_eic_test_all_nm[[1]],df_eic_test_all_nm[[2]],df_eic_test_all_nm[[3]])


# Prescripción farmacéutica
### - Antagonistas de la aldosterona.
### - Betabloqueantes.
### - Antiagregantes.
### - Ranolazina.
### - Trimetazidina.
### - Nitratos.
### - Bloqueantes del canal de calcio.
### - Agentes activos sobre el sistema renina-angiotensina.
### - iSGLT2 
### - arGLP-1


df_eic_atc <- data.frame()

df_atc_tratamiento <- data.frame(ind = c('C03DA','C07','B01AC','C01EB18','C01EB15','C01DA','C08','C09',
                                         'A10B','A10BJ'),
                                 filter = c('C03DA','C07','B01AC','C01EB18','C01EB15','C01DA','C08','C09',
                                            'A10BK02|A10BD16|A10BK03|A10BD20|A10BD19','A10BJ02|A10BJ06|A10BJ05'),
                                 cod = c('eic_antag_aldo','eic_bbloc','eic_antiagreg','eic_ranol','eic_trimet',
                                         'eic_nitra','eic_bloc_ca','eic_sra','eic_isglt2','eic_glp1'))

eic_indicadores_tratamiento <- function(i){
  
  
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento %>% filter(ind %in% i)
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",i,"%'"))
  if(df_atc_tratamiento_$ind == 'A10B' | df_atc_tratamiento_$ind == 'A10BJ'){
    df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  }
  pobla <- dbGetQuery(conn = con,
                      paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_eic %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  
  df_eic_atc <- rbind(df_eic_atc,data_csv)
  df_eic_atc$ind <- df_atc_tratamiento_$cod
  
  dbDisconnect(con, shutdown=TRUE)            
  return(df_eic_atc)
}

df_eic_atc_all <- lapply(df_atc_tratamiento$ind,eic_indicadores_tratamiento)


df_eic_atc_all <- rbind(df_eic_atc_all[[1]],df_eic_atc_all[[2]],df_eic_atc_all[[3]],
                        df_eic_atc_all[[4]],df_eic_atc_all[[5]],df_eic_atc_all[[6]],
                        df_eic_atc_all[[7]],df_eic_atc_all[[8]],
                        df_eic_atc_all[[9]],df_eic_atc_all[[10]])

### - Población con EIC y diabetes 

df_eic_dm2_all <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con,
                 paste0("SELECT * FROM main.comorbilidad WHERE comorbilidad_cd == 2"))

pobla <- dbGetQuery(conn = con,
                    paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))

df <- df[!duplicated(df$paciente_id),]
df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))

data_csv <- df_eic %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))

data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0

data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

df_eic_dm2_all <- rbind(df_eic_dm2_all,data_csv)

df_eic_dm2_all$ind <- 'eic_dm2'
dbDisconnect(con, shutdown=TRUE) 


### - iSGLT2 (en población con diabetes)
### - arGLP-1 (en población con diabetes).

df_eic_atc_dm <- data.frame()

df_atc_tratamiento_dm <- data.frame(ind = c('A10B','A10BJ'),
                                    filter = c('A10BK02|A10BD16|A10BK03|A10BD20|A10BD19','A10BJ02|A10BJ06|A10BJ05'),
                                    cod = c('eic_dm2_isglt2','eic_dm2_glp1'))

eic_indicadores_tratamiento_diabetes <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento_dm %>% filter(ind %in% i)
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",i,"%'"))
  if(df_atc_tratamiento_$ind == 'A10B' | df_atc_tratamiento_$ind == 'A10BJ'){
    df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  }
  pobla <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.paciente_id,paciente.zbs_residencia_cd,paciente.ccaa_cd,paciente.enfermedad_cd,paciente.sexo_cd,
                             paciente.fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))
  
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_eic %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

  df_eic_atc_dm <- rbind(df_eic_atc_dm,data_csv)
  
  
  
  df_eic_atc_dm$ind <- df_atc_tratamiento_$cod
  
  
  dbDisconnect(con, shutdown=TRUE)     
  return(df_eic_atc_dm)
}


df_eic_atc_dm_all <- lapply(df_atc_tratamiento_dm$ind,eic_indicadores_tratamiento_diabetes)

df_eic_atc_dm_all <- rbind(df_eic_atc_dm_all[[1]],df_eic_atc_dm_all[[2]])

### - iSGLT2 (en población sin diabetes)
### - arGLP-1 (en población sin diabetes).

df_eic_atc_sin_dm <- data.frame()

df_atc_tratamiento_sin_dm <- data.frame(ind = c('A10B','A10BJ'),
                                    filter = c('A10BK02|A10BD16|A10BK03|A10BD20|A10BD19','A10BJ02|A10BJ06|A10BJ05'),
                                    cod = c('eic_sin_dm2_isglt2','eic_sin_dm2_glp1'))

eic_indicadores_tratamiento_sin_diabetes <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento_sin_dm %>% filter(ind %in% i)
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",i,"%'"))
  df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  pobla <- dbGetQuery(conn = con,
                      paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 1 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  
  pobla_dm <- dbGetQuery(conn = con,
                      paste0("SELECT paciente.paciente_id,paciente.zbs_residencia_cd,paciente.ccaa_cd,paciente.enfermedad_cd,paciente.sexo_cd,
                             paciente.fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente
                          LEFT JOIN main.comorbilidad ON paciente.paciente_id = comorbilidad.paciente_id
                          WHERE enfermedad_cd = 1
                          AND edad_nm >= 18
                          AND zbs_residencia_cd IS NOT NULL
                          AND comorbilidad.comorbilidad_cd = 2"))
  
  
  pobla <- pobla %>% filter(paciente_id %nin% pobla_dm$paciente_id)
  
  df_eic <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_eic %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  df_eic_atc_sin_dm <- rbind(df_eic_atc_sin_dm,data_csv)
  

  
  df_eic_atc_sin_dm$ind <- df_atc_tratamiento_$cod
  
  
  dbDisconnect(con, shutdown=TRUE)     
  return(df_eic_atc_sin_dm)
}

df_eic_atc_sin_dm_all <- lapply(df_atc_tratamiento_sin_dm$ind,eic_indicadores_tratamiento_sin_diabetes)

df_eic_atc_sin_dm_all <- rbind(df_eic_atc_sin_dm_all[[1]],df_eic_atc_sin_dm_all[[2]])

df_all_eic <- rbind(df_eic_global,df_eic_exams_all,df_eic_iam_all,df_eic_test_all,df_eic_test_all_nm,df_eic_atc_all,df_eic_dm2_all,df_eic_atc_dm_all,df_eic_atc_sin_dm_all)

write.table(df_all_eic,'../../outputs/eic_ind_tablas_perfil.csv',sep='|',row.names = FALSE)

rm(list=ls())

## FIBRILACIÓN AURICULAR

# Cuidados ambulatorios

### - Fibrilación Auricular en la población mayor de 18 años.

### Total

database_path <- '../../inputs/data.duckdb'

df_fa_global <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con, 'select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL
  ')

list_ccaa <- unique(df$ccaa_cd)

df_poblacion <- dbGetQuery(conn = con, '
  select año_cd,ccaa_cd,zbs_residencia_cd,sexo_cd, grupo_edad_cd + 1 as grupo_edad_cd, nivel_copago_cd,poblacion_nm from main.poblacion 
  where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL
	')

df_poblacion$sexo_cd <- as.character(df_poblacion$sexo_cd)
df_poblacion$grupo_edad_cd[df_poblacion$grupo_edad_cd > 18] <- 18

df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd,grupo_edad_cd,sexo_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

df_poblacion_ccaa <- df_poblacion %>% group_by(ccaa_cd,sexo_cd,grupo_edad_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs,na.rm = TRUE))

data_csv <- df %>% group_by(ccaa_cd,sexo_cd,grupo_edad_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))

data_csv <- left_join(df_poblacion_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0

data_csv <- left_join(df_poblacion,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))


df_fa_global <- rbind(df_fa_global,data_csv)

df_fa_global$ind <- 'fa'

dbDisconnect(con, shutdown=TRUE)

rm(df_poblacion,df_poblacion_ccaa)



### - Ecocardiograma pacientes diagnosticados 2023
### - Evaluación CHA₂DS₂-VASc (CHADS2) al diagnóstico.
### - Evaluación HASBLED al diagnóstico
### - Determinación de TSH al diagnóstico

df_fa_exams_tsh <- data.frame()

df_ind_exams_tsh <- data.frame(ind = c('ecocardio','chads','HASBLED','tsh'),
                               descr = c('Ecocardiograma','Evaluación escala CHADS2Vasc','Evaluación escala HASBLED','Hormona estimulante de la tiroides'),
                               cod = c('fa_eco','fa_chads2','fa_hasbled','fa_tsh'))

fa_indicadores_exam_tsh <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  
  df_ind_exams_ <- df_ind_exams_tsh %>% filter(ind %in% i)
  if(i == 'tsh'){
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd == '",i,"' and year(test_dt) == 2023")) 
  }else{
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.examenes WHERE examen_cd == '",i,"' and year(examen_dt) == 2023")) 
  }
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and zbs_residencia_cd is not NULL"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% 
    summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_fa %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  df_fa_exams_tsh <- rbind(df_fa_exams_tsh,data_csv)
  
  df_fa_exams_tsh$ind <- df_ind_exams_$cod
  
  
  dbDisconnect(con, shutdown=TRUE)      
  return(df_fa_exams_tsh)
}


df_fa_exams_tsh_all <- lapply(df_ind_exams_tsh$ind,fa_indicadores_exam_tsh)

df_fa_exams_tsh_all <- rbind(df_fa_exams_tsh_all[[1]],df_fa_exams_tsh_all[[2]],
                             df_fa_exams_tsh_all[[3]],df_fa_exams_tsh_all[[4]])




### - Determinación de función renal el año de estudio

df_fa_fg_all <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con,
                 paste0("SELECT * FROM main.test WHERE test_cd == 'fg' and year(test_dt) == 2023"))
df <- df %>% group_by(paciente_id) %>% summarise(count=n()) %>% filter(count >= 2)

pobla <- dbGetQuery(conn = con,
                    paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 3 and 
                             edad_nm >= 18 and zbs_residencia_cd is not NULL"))

df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))

data_csv <- df_fa %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))

data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0

data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

df_fa_fg_all <- rbind(df_fa_fg_all,data_csv)

df_fa_fg_all$ind <- 'fa_fr'

dbDisconnect(con, shutdown=TRUE)  



### Prescripción farmacéutica
### - Anticoagulantes orales.
### - Anticoagulantes orales de acción directa
### - Antagonistas de la vitamina K.
### - Antiagregantes.

df_fa_atc <- data.frame()


df_atc_tratamiento <- data.frame(ind = c('B01A','B01A','B01A','B01AC'),
                                 filter = c('B01AA|B01AE|B01AF','B01AE07|B01AF01|B01AF02|B01AF03','B01AA','B01AC'),
                                 cod = c('fa_aco','fa_acoad','fa_antagk','fa_antiagreg'))

fa_indicadores_tratamiento <- function(i){
  
  
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento %>% filter(cod %in% i)
  df <- dbGetQuery(conn = con,
                   paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",df_atc_tratamiento_$ind,"%'"))
  
  
  df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 3 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_fa <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_fa %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  df_fa_atc <- rbind(df_fa_atc,data_csv)
  
  
  df_fa_atc$ind <- df_atc_tratamiento_$cod
  
  dbDisconnect(con, shutdown=TRUE)      
  return(df_fa_atc)           
}

df_fa_atc_all <- lapply(df_atc_tratamiento$cod,fa_indicadores_tratamiento)  


df_fa_atc_all <- rbind(df_fa_atc_all[[1]],df_fa_atc_all[[2]],df_fa_atc_all[[3]],df_fa_atc_all[[4]])


df_all_fa <- rbind(df_fa_global,df_fa_exams_tsh_all,df_fa_fg_all,df_fa_atc_all)

write.table(df_all_fa,'../../outputs/fa_ind_tablas_perfil.csv',sep='|',row.names = FALSE)

rm(list=ls())




# INSUFICIENCIA CARDIACA CONGESTIVA

# - Insuficiencia cardíaca congestiva en la población mayor de 18 años.

### Total

database_path <- '../../inputs/data.duckdb'

df_icc_global <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)


df <- dbGetQuery(conn = con, 'select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL
  ')
list_ccaa <- unique(df$ccaa_cd)


df_poblacion <- dbGetQuery(conn = con, '
  select año_cd,ccaa_cd,zbs_residencia_cd,sexo_cd, grupo_edad_cd + 1 as grupo_edad_cd, nivel_copago_cd,poblacion_nm from main.poblacion 
  where grupo_edad_cd > 4 and zbs_residencia_cd is not NULL
	')
df_poblacion$sexo_cd <- as.character(df_poblacion$sexo_cd)
df_poblacion$grupo_edad_cd[df_poblacion$grupo_edad_cd > 18] <- 18


df_poblacion <- df_poblacion %>% group_by(ccaa_cd,zbs_residencia_cd,grupo_edad_cd,sexo_cd) %>% 
  filter(ccaa_cd %in% list_ccaa) %>% 
  summarise(n_pobla_zbs = sum(poblacion_nm,na.rm = TRUE))

df_poblacion_ccaa <- df_poblacion %>% group_by(ccaa_cd,sexo_cd,grupo_edad_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs,na.rm = TRUE))

data_csv <- df %>% group_by(ccaa_cd,sexo_cd,grupo_edad_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))

data_csv <- left_join(df_poblacion_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0

data_csv <- left_join(df_poblacion,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))


df_icc_global <- rbind(df_icc_global,data_csv)
df_icc_global$ind <- 'icc'

dbDisconnect(con, shutdown=TRUE)



### - Determinación/registro de péptido natriurético el año de estudio
### - Determinación/registro de fracción de eyección el año de estudio
### - Vacuna de la gripe el año de estudio.


df_icc_exams_pn <- data.frame()


df_ind_exams_pn <- data.frame(ind = c('pn','vacuna_gripe'),
                              descr = c('Pépitido natriurético (BNP y NT-proBNP)','Vacunación antigripal'),
                              cod = c('icc_pep_nat','icc_gripe'))

icc_indicadores_exam <- function(i){
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  
  df_ind_exams_ <- df_ind_exams_pn %>% filter(ind %in% i)
  if(i == 'pn'){
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.test WHERE test_cd == '",i,"' and year(test_dt) == 2023")) 
  }else{
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * FROM main.examenes WHERE examen_cd == '",i,"' and year(examen_dt) == 2023"))
  }
  
  pobla <- dbGetQuery(conn = con,
                      paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_icc %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  df_icc_exams_pn <- rbind(df_icc_exams_pn,data_csv)
  df_icc_exams_pn$ind <- df_ind_exams_$cod
  
  dbDisconnect(con, shutdown=TRUE)            
  return(df_icc_exams_pn)
}

df_icc_exams_pn_all <- lapply(df_ind_exams_pn$ind,icc_indicadores_exam)


df_icc_exams_pn_all <- rbind(df_icc_exams_pn_all[[1]],df_icc_exams_pn_all[[2]])




### - Ecocardiograma al diagnóstico.



df_icc_eco_all <- data.frame()

con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)

df <- dbGetQuery(conn = con,
                 paste0("SELECT * FROM main.examenes WHERE examen_cd == 'ecocardio' and year(examen_dt) == 2023"))
pobla <- dbGetQuery(conn = con,
                    paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 2 and 
                             edad_nm >= 18 and year(fecha_enfermedad_dt) == 2023 and  zbs_residencia_cd is not NULL"))

df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  

pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))

data_csv <- df_icc %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))

data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))

data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0

data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))


df_icc_eco_all <- rbind(df_icc_eco_all,data_csv)



df_icc_eco_all$ind <- 'icc_eco'

dbDisconnect(con, shutdown=TRUE)  


# - Inhibidores de la Enzima Convertidora de Angiotensina (IECA) o ARA-II.
# - Sacubritilo /valsartán. 
# - Sacubritilo /valsartán como primer tratamiento.
# - IECA o ARA-II o sacubitrilo-valsartán
# - Betabloqueantes.
# - Antagonistas de la aldosterona.
# - iSGLT2 (dapagliflozina o empagliflozina).
# - IECA o ARA-II o sacubitrilo-valsartán Y betabloqueante Y antagonista de aldosterona y iSGLT2. 
# - Vericiguat.



df_icc_atc <- data.frame()


df_atc_tratamiento <- data.frame(ind = c('C09','C09DX04','C07','C03DA','A10B','','C01DX22','C09DX04'),
                                 filter = c('C09C|C09D|C09A|C09B','C09DX04','C07','C03DA',
                                            'A10BK01|A10BD15|A10BD21|A10BK03|A10BD20|A10BD19',
                                            '','C01DX22','C09DX04'),
                                 cod = c('icc_ieca_ara','icc_sac_val','icc_bbloc','icc_antag_aldo','icc_isglt2',
                                         'icc_ieca-sac_bbloc_aa_isglt2','icc_ver','icc_prim_sac_val'))

icc_indicadores_tratamiento <- function(i){
  
  
  
  con = dbConnect(duckdb::duckdb(), dbdir=database_path, read_only=FALSE)
  df_atc_tratamiento_ <- df_atc_tratamiento %>% filter(cod %in% i)
  if(i == 'icc_prim_sac_val'){
    
    
    df <- dbGetQuery(conn=con,"SELECT paciente_id, atc_tratamiento_cd, fecha_inicio_prescripcion_dt
                                  FROM main.tratamiento")
    
    pobla <- dbGetQuery(conn = con,
                        paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 2 and year(fecha_enfermedad_dt) == 2023 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
    
    df_ <- left_join(df,pobla[c('paciente_id','fecha_enfermedad_dt')],by='paciente_id') 
    
    df_ <- df_ %>% filter(fecha_inicio_prescripcion_dt >= fecha_enfermedad_dt)
    df_ <- df_ %>% group_by(paciente_id) %>% mutate(min_date = min(fecha_inicio_prescripcion_dt)) %>% ungroup()
    df_ <- df_ %>% filter(fecha_inicio_prescripcion_dt == min_date)
    df_ <- df_ %>% filter(atc_tratamiento_cd %in% 'C09DX04')
    
    df <- df_
    rm(df_)
  }else{
    df <- dbGetQuery(conn = con,
                     paste0("SELECT * from main.tratamiento where atc_tratamiento_cd LIKE '",df_atc_tratamiento_$ind,"%'"))
    pobla <- dbGetQuery(conn = con,
                        paste0("select paciente_id,zbs_residencia_cd,ccaa_cd,enfermedad_cd,sexo_cd,fecha_enfermedad_dt,CASE
WHEN edad_nm >= 15
and edad_nm <= 19 THEN 4
WHEN edad_nm >= 20
and edad_nm <= 24 THEN 5
WHEN edad_nm >= 25
and edad_nm <= 29 THEN 6
WHEN edad_nm >= 30
and edad_nm <= 34 THEN 7
WHEN edad_nm >= 35
and edad_nm <= 39 THEN 8
WHEN edad_nm >= 40
and edad_nm <= 44 THEN 9
WHEN edad_nm >= 45
and edad_nm <= 49 THEN 10
WHEN edad_nm >= 50
and edad_nm <= 54 THEN 11
WHEN edad_nm >= 55
and edad_nm <= 59 THEN 12
WHEN edad_nm >= 60
and edad_nm <= 64 THEN 13
WHEN edad_nm >= 65
and edad_nm <= 69 THEN 14
WHEN edad_nm >= 70
and edad_nm <= 74 THEN 15
WHEN edad_nm >= 75
and edad_nm <= 79 THEN 16
WHEN edad_nm >= 80
and edad_nm <= 84 THEN 17
WHEN edad_nm >= 85 THEN 18
ELSE NULL 
END as grupo_edad_cd from main.paciente where enfermedad_cd == 2 and edad_nm >= 18 and zbs_residencia_cd is not NULL"))
  }
  
  if(i=='icc_ieca-sac_bbloc_aa_isglt2'){
    
    df_beta <- df  %>% filter(str_starts(atc_tratamiento_cd,'C07'))
    df_aa <- df  %>% filter(str_starts(atc_tratamiento_cd,'C03DA'))
    df_isglt <- df  %>% filter(str_starts(atc_tratamiento_cd,'A10BK01|A10BD15|A10BD21|A10BK03|A10BD20|A10BD19'))
    df_ <- df  %>% filter(str_starts(atc_tratamiento_cd,'C09C|C09D|C09A|C09B|C09DX04'))
    df__ <- df_beta %>% filter(paciente_id %in% df_aa$paciente_id)
    df__ <- df__ %>% filter(paciente_id %in% df_isglt$paciente_id)
    df__ <- df__ %>% filter(paciente_id %in% df_$paciente_id)
    df <- df__ 
    rm(df_beta,df_aa,df_isglt,df_,df__)
  }else{
    df <- df %>% filter(str_starts(atc_tratamiento_cd,df_atc_tratamiento_$filter))
  }
  
  df_icc <- pobla %>% filter(paciente_id %in% df$paciente_id)  
  
  pobla <- pobla %>% group_by(zbs_residencia_cd,ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_zbs = n_distinct(paciente_id))
  pobla_ccaa <- pobla %>% group_by(ccaa_cd,grupo_edad_cd,sexo_cd) %>% summarise(n_pobla_ccaa = sum(n_pobla_zbs))
  
  data_csv <- df_icc %>% group_by(grupo_edad_cd,sexo_cd,ccaa_cd) %>% summarise(n_casos_ccaa = n_distinct(paciente_id))
  
  data_csv <- left_join(pobla_ccaa,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  data_csv$n_casos_ccaa[is.na(data_csv$n_casos_ccaa)] <- 0
  
  data_csv <- left_join(pobla,data_csv,by=c('sexo_cd','grupo_edad_cd','ccaa_cd'))
  
  df_icc_atc <- rbind(df_icc_atc,data_csv)
  
  
  df_icc_atc$ind <- df_atc_tratamiento_$cod
  
  dbDisconnect(con, shutdown=TRUE)      
  return(df_icc_atc)           
}

df_icc_atc_all <- lapply(df_atc_tratamiento$cod,icc_indicadores_tratamiento)  

df_icc_atc_all <- rbind(df_icc_atc_all[[1]],df_icc_atc_all[[2]],df_icc_atc_all[[3]],
                        df_icc_atc_all[[4]],df_icc_atc_all[[5]],df_icc_atc_all[[6]],
                        df_icc_atc_all[[7]],df_icc_atc_all[[8]])


df_all_icc <- rbind(df_icc_global,df_icc_exams_pn_all,df_icc_eco_all,df_icc_atc_all)

write.table(df_all_icc,'../../outputs/icc_ind_tablas_perfil.csv',sep='|',row.names = FALSE)


rm(list=ls())
