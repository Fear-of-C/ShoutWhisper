#!/bin/bash
# Proper header for a Bash script.

export JETTY_HOME=/usr/share/java
export APP_HOME=/home/nick/workspace/jettytests
export JAVA_OPTIONS="-Xms16m -Xmx64m -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=64M -Drun.mode=Production -Djetty.logs=$APP_HOME/logs -Dapp.home=$APP_HOME"
cd $JETTY_HOME
java $JAVA_OPTIONS -jar jetty-start-6.1.24.jar $APP_HOME/jettyconfig.xml >& $APP_HOME/logs/start.log &
