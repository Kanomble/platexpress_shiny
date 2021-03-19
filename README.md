# platexpress_shiny
Transforming platexpress data visualization into shiny dashboards.

## INFO
- use the `required_packages.R` script for installing additional packages
- change the PASSWORD environment variable in the docker-compose.yml file before you submit the `docker-compose.yml`
- Folder description:
  - data: biolector and synergy test datasets
  - src:
    - platexpress_module: functions for interactions with platexpress
    - shiny: shiny R file with ui and server specifications 

## TODO
- change and refactor code in the platexpress_interactions.R tryCatch blocks: 
  - edit the warning block
  - add error handling
  - add final statements
- change ui of groups tab
- develop a plan where to go with this application

## starting rstudio for developing
Clone the git-repo into an desired directory. Start the docker container by using the `docker-compose up` command.
