package org.ensime.maven.plugins.ensime;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.ensime.maven.plugins.ensime.model.EnsimeConfig;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * Uses maven project metadata to run the ensime server in index-only mode to pre-index the
 * codebase.
 *
 * @author Aaditya Ramesh
 */
@Mojo(
    name = "serverIndex",
    requiresDependencyResolution = ResolutionScope.TEST,
    requiresProject = true, aggregator = true)
final public class ServerIndexMojo extends AbstractMojo {

  /**
   * The project whose project files to create.
   */
  @Component
  private MavenProject project;
  /**
   * ENSIME server
   */
  @Parameter(property = "ensime.server.version",
      defaultValue = "2.0.0-M4")
  private String ensimeServerVersion;
  /**
   * Ensime Scala version
   * <p>
   * If set (e.g. with -Densime.scala.version), the plugin will use this value instead of trying to
   * dynamically determine project scala version.
   */
  @Parameter(property = "ensime.scala.version")
  private String ensimeScalaVersion;
  @Parameter(defaultValue = "${repositorySystemSession}", readonly = true)
  private RepositorySystemSession session;
  @Component
  private RepositorySystem repoSystem;

  /**
   * Maintains the same behavior as the sbt ensime maven plugin's "orderFiles" method.
   */
  private static List<File> orderJars(List<File> jars) {
    Map<Boolean, List<File>> partitioned = jars.stream()
        .sorted(Comparator.comparing(file -> file.getName() + file.getPath()))
        .collect(Collectors.partitioningBy(o -> o.getName().contains("monkey")));
    List<File> orderedJars = new ArrayList<>();
    List<File> monkeyJars = partitioned.get(true);
    if (monkeyJars != null) {
      orderedJars.addAll(monkeyJars);
    }
    List<File> humanJars = partitioned.get(false);
    if (humanJars != null) {
      orderedJars.addAll(humanJars);
    }
    return orderedJars;
  }

  private static void launchEnsimeServer(
      String javaCommand,
      List<File> orderedClasspath,
      List<String> jvmFlags) throws IOException, InterruptedException {
    ProcessBuilder builder = new ProcessBuilder();
    List<String> fullCommand = new ArrayList<>();
    fullCommand.add(javaCommand);
    // JVM flags
    fullCommand.addAll(jvmFlags);
    // Class path
    fullCommand.add("-cp");
    fullCommand.add(orderedClasspath
        .stream()
        .map(File::getPath)
        .collect(Collectors.joining(":"))
    );
    // Main class
    fullCommand.add("org.ensime.server.Server");
    // Start the server and wait for it to finish.
    builder.inheritIO()
        .command(fullCommand)
        .start()
        .waitFor();
  }

  @Override
  public void execute() {
    // Preferably we would read the .ensime config but that would mean we would either write our own
    // S-Expression parser or we would have to use a library to do it. Instead, we will regenerate
    // the maven project metadata and an EnsimeConfig object from it, which will then be used for
    // starting the ensime server for indexing.

    Properties properties = new Properties();
    EnsimeConfigGenerator generator = new EnsimeConfigGenerator(project,
        repoSystem, session, properties, ensimeServerVersion, ensimeScalaVersion,
        getLog());
    EnsimeConfig ensimeConfig = generator.generateConfig();

    String javaCommand = String.format("%s/bin/java", ensimeConfig.getJavaHome());
    List<File> classPathJars = new ArrayList<>();
    classPathJars.addAll(ensimeConfig.getEnsimeServerJars());
    classPathJars.addAll(ensimeConfig.getScalaCompilerJars());
    classPathJars.add(new File(String.format("%s/lib/tools.jar", ensimeConfig.getJavaHome())));
    List<File> orderedClasspath = orderJars(classPathJars);
    List<String> jvmFlags = new ArrayList<>();
    jvmFlags.add("-Xms4g");
    jvmFlags.add("-Xmx4g");
    jvmFlags.add("-XX:StringTableSize=1000003");
    jvmFlags.add("-XX:+UnlockExperimentalVMOptions");
    jvmFlags.add("-XX:SymbolTableSize=1000003");
    jvmFlags.add("-Densime.config=.ensime");
    jvmFlags.add("-Densime.exitAfterIndex=true");
    jvmFlags.addAll(ensimeConfig.getJavaFlags());
    File cacheDir = ensimeConfig.getCacheDir();
    if (!cacheDir.exists() && !cacheDir.mkdirs()) {
      throw new RuntimeException(
          "Unable to create ensime cache directory. Please check permissions.");
    }
    try {
      launchEnsimeServer(javaCommand, orderedClasspath, jvmFlags);
    } catch (IOException e) {
      getLog().error("Unable to start ensime server for indexing.", e);
    } catch (InterruptedException e) {
      getLog().error("Interrupted while indexing.", e);
    }
  }
}
