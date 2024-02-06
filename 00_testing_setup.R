## #######################################################################################
##
## TUTORIAL PRELUDE: TESTING THAT ALL PACKAGES ARE SET UP AND READY TO USE
##
## #######################################################################################


# Check 1: The following packages should all load, with messages but no errors
library(caret)
library(ggplot2)
library(mapview)
library(viridis)
library(sf)
library(tidycensus)


# Check 2: The r5r package is expected to give a warning about memory
options(java.parameters = "-Xmx1G")
library(r5r)


# Check 3: inspect the JAVA_HOME environment variable - does it match the directory where
#  you installed OpenJDK 21?
message("JAVA_HOME: ", Sys.getenv('JAVA_HOME'))
# If check 3 fails: add the following line to the top of the script and re-run
# Sys.setenv(JAVA_HOME = '/my/jdk21/install/path/')


# Check 4: Check that sf is connected to a folder that has the PROJ database in it
proj_search_paths <- sf::sf_proj_search_paths()
if(any('proj.db' %in% list.files(proj_search_paths))){
  message("SF is connected to proj")
} else {
  stop("SF could not find the proj database")
}
# If check 4 fails: add the following line to the top of the script and re-run
# sf::sf_proj_search_paths(path = "/my/proj/install/path/")


# Check 5: confirm that rJava can start up successfully and links to OpenJDK 21
rjava_startup_code <- rJava::.jinit()
if(rjava_startup_code != 0L) stop("rJava failed to start")
linked_java_version <- rJava::.jcall("java.lang.System", "S", "getProperty", "java.version")
if(grepl('^21', linked_java_version)){
  message("Linked Java version is ", linked_java_version, ", as expected.")
} else {
  stop("Linked Java version is ", linked_java_version, ", does not start with 21.")
}
