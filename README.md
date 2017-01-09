[![Bountysource](https://www.bountysource.com/badge/tracker?tracker_id=239449)](https://www.bountysource.com/trackers/239449-ensime?utm_source=239449&utm_medium=shield&utm_campaign=TRACKER_BADGE)

# ENSIME Maven Plugin

This [maven](https://maven.apache.org/) plugin generates a `.ensime` file for use with an [ENSIME server](http://github.com/ensime/ensime-server).

## Installation

Configure your `~/.m2/settings.xml` file so that maven is aware of the plugin group `org.ensime.maven.plugins`:

``` xml
  <pluginGroups>
    <pluginGroup>org.ensime.maven.plugins</pluginGroup>
  </pluginGroups>
```

## Generate `.ensime` file

<!-- ### (Optional) Download project sources and javadocs -->
<!--  -->
<!-- The ensime-maven plugin will tell ensime about the location of source jars, but won't automatically download them for you. You can get maven to do this by running: -->
<!--  -->
<!-- ``` shell -->
<!-- mvn dependency:sources && mvn dependency:resolve -Dclassifier=javadoc -->
<!-- ``` -->

### Generate the `.ensime` file

To actually generate the `.ensime` file from your pom, run:

``` shell
mvn ensime:generate
```

### (Optional) Initial project compilation

To prevent some surprises when working with a new project in ensime, do a full compile before starting up ensime for the first time:

``` shell
mvn compile test-compile
```
