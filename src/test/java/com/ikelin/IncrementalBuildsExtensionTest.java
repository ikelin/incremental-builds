package com.ikelin;

import static com.ikelin.IncrementalBuildsExtension.PROPERTY_PREFIX;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.logging.Logger;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

@ExtendWith(MockitoExtension.class)
public class IncrementalBuildsExtensionTest {

  @Mock
  private Logger logger;

  @Mock
  private IncrementalBuilds incrementalBuilds;

  @Mock
  private MavenSession session;

  @Mock
  private Properties properties;

  @Spy
  private IncrementalBuildsExtension incrementalBuildsExtension;

  @BeforeEach
  public void beforeEach() throws NoSuchFieldException, IllegalAccessException {
    Field loggerField = IncrementalBuildsExtension.class.getDeclaredField("logger");
    loggerField.setAccessible(true);
    loggerField.set(incrementalBuildsExtension, logger);
  }

  @Test
  public void testExtensionNotEnabled() {
    when(properties.getProperty(PROPERTY_PREFIX + ".enable")).thenReturn(Boolean.FALSE.toString());
    when(session.getUserProperties()).thenReturn(properties);

    incrementalBuildsExtension.afterProjectsRead(session);

    verify(incrementalBuilds, never()).getChangedProjects(any(RevInfo.class));
  }

  @Test
  public void testExtensionEnabledNoChangedProjects() {
    when(properties.getProperty(PROPERTY_PREFIX + ".enable")).thenReturn(Boolean.TRUE.toString());
    when(session.getUserProperties()).thenReturn(properties);
    doReturn(incrementalBuilds).when(incrementalBuildsExtension).getIncrementalBuilds(session);

    incrementalBuildsExtension.afterProjectsRead(session);

    verify(session, times(1)).getGoals();
  }

  @Test
  public void testExtensionEnabledWithChangedProjects() {
    when(properties.getProperty(PROPERTY_PREFIX + ".enable")).thenReturn(Boolean.TRUE.toString());
    when(session.getUserProperties()).thenReturn(properties);
    doReturn(incrementalBuilds).when(incrementalBuildsExtension).getIncrementalBuilds(session);
    List<MavenProject> changedProjects = Arrays.asList(mock(MavenProject.class));
    when(incrementalBuilds.getChangedProjects(any(RevInfo.class))).thenReturn(changedProjects);

    incrementalBuildsExtension.afterProjectsRead(session);

    verify(incrementalBuilds).getChangedProjects(any(RevInfo.class));
  }

  @Test
  public void testIncrementBuildException() {
    when(properties.getProperty(PROPERTY_PREFIX + ".enable")).thenReturn(Boolean.TRUE.toString());
    when(session.getUserProperties()).thenReturn(properties);
    doReturn(incrementalBuilds).when(incrementalBuildsExtension).getIncrementalBuilds(session);
    RuntimeException exception = mock(RuntimeException.class);
    doThrow(exception).when(incrementalBuilds).getChangedProjects(any(RevInfo.class));

    incrementalBuildsExtension.afterProjectsRead(session);

    verify(logger).error(anyString(), eq(exception));
  }
}