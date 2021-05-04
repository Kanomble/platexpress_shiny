# platexpress_shiny
Transforming platexpress data for visualization in shiny dashboards.

## starting rstudio for developing
Clone the git-repo into an desired directory. Edit the PASSWORD environment variable. Start docker desktop on windows or the docker client on ubuntu. Open a shell (e.g. the windows powershell) and change your current directory to the downloaded git-repo directory with the command `cd path-to-git-repo`. Start the docker container by using the `docker-compose up` command. Visit the URL `localhost:8787` in an browser of your choice, google chrome or firefox are recommended. Use rstudio as user and the variable PASSWORD as your password. Load the `required_packages.R` script into your working directory (WD) and submit the code. This will install all necessary packages. 

## some notes on docker
Sometimes you need to remove docker containers or images. If you do this, keep in mind, that everything that has been installed inside the container is also removed by removing the container. Thus, all files that are inside the container and not on a shared directory will be removed to. The shared directory of this docker container is the whole directory in which you place the `docker-compose.yml` (default: the git-repo), which is set with the dot (".") in the `volume` section of the `docker-compose.yml`. 

If you need to re-install the docker container, first remove it. Use following commands to identify the container-id: `docker ps` (if the container has started) or `docker ps --all`. Identify the container-id of the rocker/rstudio container and use the command: `docker rm <id>` to remove the container. If you realy want to clean up everything you can also delete the image. This can be done with the `docker rmi <image-id>` cmd, identify the image-id with `docker images`.

## INFO
- use the `required_packages.R` script for installing additional packages
- change the PASSWORD environment variable in the `docker-compose.yml` file before you submit the `docker-compose up` cmd
 - Folder description:
  - data:
    - biolector and synergy test datasets
  - src:
    - platexpress_module: functions for interactions with platexpress
    - shiny: shiny R file with ui and server specifications

## TODO
- add boxData functionality and refactor functions
  1.read into the topic [x]
  
- documentation
- change and refactor code in the platexpress_interactions.R tryCatch blocks: 
  - edit the warning block
  
  - add error handling
  ***
    -displore alternatives to validate need [x]
    -purr::possibly,purr::safely
    -Common Lisp
    -withCallingsHandlers(),try()
  ***  
    -started working on TryCatch Blocks []
  ***
    -reading into topic [x]
  ***
  - add final statements
  
- change ui of groups tab
  1.reading into the topic [x]
- develop a plan where to go with this application
  

## implementation of correctBlanks and skipWells plateXpress functions
implemented this functionality:
```R
raw <- skipWells(raw, skip = "A9")
plate <- skipWells(plate, skip = "A9")
data <- correctBlanks(data=raw, plate=plate)
viewPlate(data, rows = c("A","B","C"), cols = 1:9)
```
