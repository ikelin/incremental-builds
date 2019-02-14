package com.ikelin;

import com.google.common.annotations.VisibleForTesting;
import org.apache.maven.AbstractMavenLifecycleParticipant;
import org.apache.maven.execution.MavenSession;
import org.codehaus.plexus.component.annotations.Component;
import org.codehaus.plexus.component.annotations.Requirement;
import org.codehaus.plexus.logging.Logger;
import org.eclipse.jgit.api.errors.GitAPIException;

import java.io.IOException;
import java.util.Properties;

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
      getIncrementalBuilds(logger, session).build();
    } catch (IOException | GitAPIException ex) {
      logger.error("Failed to perform incremental builds", ex);
    }
  }

  @VisibleForTesting
  IncrementalBuilds getIncrementalBuilds(final Logger logger, final MavenSession session) {
    return new IncrementalBuilds(logger, session);
  }

  private boolean isEnabled(final Properties properties) {
    String enable = properties.getProperty(PROPERTY_PREFIX + ".enable");
    return Boolean.parseBoolean(enable);
  }

}
