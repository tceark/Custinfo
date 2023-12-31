#!/bin/sh
##############################################################################################
##
##  This sample shell script is provided as an easy way to invoke the Cust Application
##  sample build from the command line.
##
##  usage: build.sh [options] <buildfile>
##
##  buildFile:  Relative path (from sourceDir) of the file to build. If file
##  is *.txt then is assumed to be a buildlist file containing a list of relative
##  path files to build. The build list file can be absolute or relative (from
##  sourceDir) path.  If omitted then CustApplication/build/files.txt is built.
##
##  **NOTE - Assumes CustApplication/build/build.properties has been configured with
##           default properties.  Default properties can be overridden using command options
##           described in CustApplication/build/build.groovy
##
##  Examples:
##  build.sh
##  build.sh CustApplication/cobol/epsnbrvl.cbl
##  build.sh --sourceDir /u/usr1/repo /u/usr/buildFiles.txt
##
###############################################################################################

# Set the DBB HOME directory
DBB_HOME=/usr/lpp/IBM/dbb

# $DBB_HOME/bin/groovyz automatically sets the env variables and classpath required for DBB
CMD="$DBB_HOME/bin/groovyz build.groovy $@"

echo $CMD
$CMD
