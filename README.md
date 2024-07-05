![Logo of the project](https://cienciadedatosysalud.org/wp-content/uploads/logo-Data-Science-VPM.png)

# ATLASVPM: Atlas de cuidados ambulatorios en la enfermedad cardiaca

## Objetivos
Estudio de la variación intra- comunidad autónoma (CCAA) de los cuidados y tratamiento prestados en atención primaria (AP) y consultas externas hospitalarias (CEX) a los pacientes con enfermedades cardíacas a nivel de zona básica de salud (ZBS).

### Objetivos específicos:
1. Definir operativamente el listado de indicadores que describan cuidados, resultados intermedios y tratamiento farmacéutico ambulatorio en la atención a personas con enfermedades cardíacas (i.e., cardiopatía isquémica, insuficiencia cardíaca congestiva y fibrilación auricular) a nivel de atención primaria y consultas externas hospitalarias.
2. Definir el esquema de datos y determinar su conformidad con los datos de salud disponibles en los sistemas de información sanitaria de las CCAA participantes en el estudio.
3. Describir y cartografiar la atención al paciente cardíaco por ZBS analizando la variabilidad del conjunto y estratificando por sexo y nivel socioeconómico.
4. Representar aquellos resultados fiables a través de un cuadro de mandos interactivo. 


## Metodología:
### Diseño del estudio: 
Estudio observacional, retrospectivo y ecológico a partir de la reutilización de datos sanitarios mediante análisis federado. 

### Definición de las cohortes: 
La población objetivo se corresponde con la población usuaria del Sistema Nacional de Salud (SNS), definida como la población igual o mayor de 18 años con tarjeta sanitaria activa a 31 de diciembre del año anterior al año de estudio en las CCAA participantes. 
A partir de la población objetivo se seleccionará los grupos de población con algún diagnóstico de las enfermedades cardíacas a estudio durante los 5 años previos al año de estudio (2022).
- **Enfermedad isquémica coronaria (EIC)** (también conocida como cardiopatía isquémica): Población con episodio de enfermedad isquémica coronaria (_i.e., isquemia cardíaca con/sin angina o infarto agudo de miocardio (K74, K75, K76) o códigos icd-10: I20  Angina de pecho; I21  Infarto agudo de miocardio, I22  Infarto agudo de miocardio subsiguiente con elevación de ST (IAMCEST) (IMEST) (STEMI) y sin elevación de ST (IAMSEST) (IMNEST) (NSTEMI), I24 Otras enfermedades isquémicas agudas cardiacas, I25  Enfermedad isquémica crónica cardiaca ó códigos icd-9: 410, 411, 412, 413, 414_) abierto antes del 1 enero del año de estudio. Pacientes admitidos en urgencias u hospitalizados en los 5 años previos con diagnóstico primario o secundario de EIC. 
- **Fibrilación auricular (FA):** Población con episodio de fibrilación auricular abierto (_i.e., K78 o códigos icd-10: I49.01 Fibrilación ventricular; I49.02 Flutter ventricular, o códigos icd-9: 427.31 Fibrilación auricular; 427.3 Flutter auricular; 427.41  Fibrilación ventricular; 427.42 Flutter ventricular_) antes del 1 de enero del año de estudio. Pacientes admitidos a urgencias u hospitalizados en los 5 años previos con diagnóstico primario o secundario de FA. 
- **Insuficiencia cardíaca congestiva (ICC):** Población con episodio de insuficiencia cardíaca congestiva (_i.e., K77 o códigos icd-10: I50  Insuficiencia cardiaca, I11.0 Enfermedad cardíaca hipertensiva con insuficiencia cardiaca; I13.0 Enfermedad cardiaca y renal crónica hipertensiva con insuficiencia cardiaca y enfermedad renal crónica estadios 1 a 4 ó enfermedad renal crónica no especificada, I13.2 Enfermedad cardiaca y renal crónica hipertensiva con insuficiencia cardiaca y con enfermedad renal crónica en estadio 5 ó con enfermedad renal en estadio terminal, o códigos icd-9: 398.91, 402.01, 402.11, 402.91, 404.01, 404.03, 404.11, 404.13, 404.91, 404.93, 428.0, 428.1, 428.20, 428.22, 428.23, 428.30, 428.32, 428.33, 428.40, 428.42, 428.43, 428.9_) abierto antes del 1 de enero del año de estudio. Pacientes admitidos a urgencias u hospitalizados en los 5 años previos con diagnóstico primario o secundario de ICC. 

### Periodo de estudio: 
2023-01-01 hasta 2023-12-31

### Unidad de análisis:
ZBS y área sanitaria (AS) de las CCAA participantes


## CÓMO DESPLEGAR EL CONTENEDOR CON LOS ANÁLISIS (EN)
## HOW TO RUN THE DOCKER
Use the following code snippet to create the container.
```bash
docker pull ghcr.io/cienciadedatosysalud/atlas-cardio:latest

docker run -d -p 127.0.0.1:3000:3000 --name atlas-cardio-aspire ghcr.io/cienciadedatosysalud/atlas-cardio:latest

```
Open your web browser at http://localhost:3000.

## Run the analysis.
Follow the steps below.
  1. Map your data in the "MAP DATA" tab.
  2. If everything has worked well, in the "RUN ANALYSIS" tab, select the project **cardio_report.qmd**"
  3. Go to the "OUTPUTS" tab and download the results.


## MINIMUM REQUIREMENTS FOR DEPLOYMENT

<a href="https://creativecommons.org/licenses/by/4.0/" target="_blank" ><img src="https://img.shields.io/badge/license-CC--BY%204.0-lightgrey" alt="License: CC-BY 4.0"></a>
