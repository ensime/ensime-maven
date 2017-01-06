/*
 * Copyright 2012 Happy-Camper Street.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */
package org.ensime.maven.plugins.ensime;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.RepositorySystem;

/**
 * Generates ENSIME configuration files.
 *
 * @author ueshin
 */
@Mojo(name = "generate", requiresProject = true, requiresDependencyResolution = ResolutionScope.TEST, aggregator = true)
public class GenerateMojo extends AbstractMojo {

    public static final String DOT_ENSIME = ".ensime";

    /**
     * The project whose project files to create.
     */
    @Component
    protected MavenProject project;

    @Parameter(defaultValue = "${repositorySystemSession}")
    private RepositorySystemSession session;

    @Component
    private RepositorySystem repoSystem;

    /**
     * The formatter preferences
     */
    @Parameter(property = "ensime.formatter.preferences", defaultValue = "${basedir}/src/ensime/formatter.properties")
    protected File formatterPreferences;

    /**
     * Skip the operation when true.
     */
    @Parameter(property = "ensime.skip", defaultValue = "false")
    protected boolean skip;

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        if(skip) {
            return;
        }
        Properties properties = new Properties();
        InputStream in = null;
        try {
            in = new FileInputStream(formatterPreferences);
            properties.load(in);
        }
        catch(IOException e) {
        }
        finally {
            if(in != null) {
                try {
                    in.close();
                }
                catch(IOException e) {
                }
            }
        }

        ConfigGenerator generator = new ConfigGenerator(project,
            repoSystem, session, properties);
        generator.generate(new File(project.getBasedir(), DOT_ENSIME));
    }
}
