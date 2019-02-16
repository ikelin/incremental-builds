package com.ikelin;

import com.google.common.annotations.VisibleForTesting;

import org.apache.maven.AbstractMavenLifecycleParticipant;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.component.annotations.Component;
import org.codehaus.plexus.component.annotations.Requirement;
import org.codehaus.plexus.logging.Logger;

import java.util.List;
import java.util.Properties;

/**
 * An extension that resets the project list to only projects that changed or impacted by the
 * changes.
 */
@Component(role = AbstractMavenLifecycleParticipant.class)
public class IncrementalBuildsExtension extends AbstractMavenLifecycleParticipant {

  public static final String PROPERTY_PREFIX = "incremental";

  @Requirement
  private Logger logger;

  @Override
  public void afterProjectsRead(final MavenSession session) {
    logger.info("------------------------------------------------------------------------");
    if (!isEnabled(session.getUserProperties())) {
      logger.debug("Incremental Builds extension is not enabled");
      return;
    }

    logger.debug("Incremental Builds extension is enabled");

    try {
      RevInfo revInfo = RevInfo.create(session.getUserProperties());
      IncrementalBuilds incrementalBuilds = getIncrementalBuilds(session);
      List<MavenProject> changedProjects = incrementalBuilds.getChangedProjects(revInfo);

      // set session projects to changed projects
      // otherwise, set goals to validate
      if (changedProjects.isEmpty()) {
        List<String> goals = session.getGoals();
        goals.clear();
        goals.add("validate");
        logger.info("");
        logger.info("No project has changed since " + revInfo.getType() + " " + revInfo.getValue());
      } else {
        session.setProjects(changedProjects);
        logger.info("");
        logger.info("Projects changed since " + revInfo.getType() + " " + revInfo.getValue() + ":");
        logger.info("");
        for (MavenProject project : changedProjects) {
          logger.info(project.getName());
        }
      }
      logger.info("");
    } catch (Exception ex) {
      logger.error("Failed to perform incremental builds", ex);
    }
  }

  @VisibleForTesting
  IncrementalBuilds getIncrementalBuilds(final MavenSession session) {
    return new IncrementalBuilds(session);
  }

  private boolean isEnabled(final Properties properties) {
    String enable = properties.getProperty(PROPERTY_PREFIX + ".enable");
    return Boolean.parseBoolean(enable);
  }

}
