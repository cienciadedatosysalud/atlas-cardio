![Logo of the project](https://cienciadedatosysalud.org/wp-content/uploads/logo-Data-Science-VPM.png)

# ATLAS CARDIO


## Aims
### General aim: 

### Main specific aim:


### Study Design: 


### Cohort definition: 

#### Inclusion criteria: 
#### Exclusion criteria: 


### Study period: 


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
  2. If everything has worked well, in the "RUN ANALYSIS" tab, select the project "Variaciones en el consumo de antibióticos por ZBS (y CCAA)" and select the script "**cardio_report.qmd**"
  3. Go to the "OUTPUTS" tab and download the results.


## MINIMUM REQUIREMENTS FOR DEPLOYMENT

<a href="https://creativecommons.org/licenses/by/4.0/" target="_blank" ><img src="https://img.shields.io/badge/license-CC--BY%204.0-lightgrey" alt="License: CC-BY 4.0"></a>
