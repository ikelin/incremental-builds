package com.ikelin;

import static com.ikelin.IncrementalBuildsExtension.PROPERTY_PREFIX;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.maven.execution.MavenSession;
import org.codehaus.plexus.logging.Logger;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Properties;

@ExtendWith(MockitoExtension.class)
class IncrementalBuildsExtensionTest {

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
  void beforeEach() throws NoSuchFieldException, IllegalAccessException {
    Field loggerField = IncrementalBuildsExtension.class.getDeclaredField("logger");
    loggerField.setAccessible(true);
    loggerField.set(incrementalBuildsExtension, logger);
  }

  @Test
  void testExtensionNotEnabled() throws IOException, GitAPIException {
    when(properties.getProperty(PROPERTY_PREFIX + ".enable")).thenReturn(Boolean.FALSE.toString());
    when(session.getUserProperties()).thenReturn(properties);

    incrementalBuildsExtension.afterProjectsRead(session);

    verify(incrementalBuilds, never()).build();
  }

  @Test
  void testExtensionEnabled() throws IOException, GitAPIException {
    when(properties.getProperty(PROPERTY_PREFIX + ".enable")).thenReturn(Boolean.TRUE.toString());
    when(session.getUserProperties()).thenReturn(properties);
    doReturn(incrementalBuilds).when(incrementalBuildsExtension).getIncrementalBuilds(logger, session);

    incrementalBuildsExtension.afterProjectsRead(session);

    verify(incrementalBuilds).build();
  }

  @Test
  void testIncrementBuildThrowsGitAPIException() throws IOException, GitAPIException {
    when(properties.getProperty(PROPERTY_PREFIX + ".enable")).thenReturn(Boolean.TRUE.toString());
    when(session.getUserProperties()).thenReturn(properties);
    doReturn(incrementalBuilds).when(incrementalBuildsExtension).getIncrementalBuilds(logger, session);
    GitAPIException exception = mock(GitAPIException.class);
    doThrow(exception).when(incrementalBuilds).build();

    incrementalBuildsExtension.afterProjectsRead(session);

    assertThrows(GitAPIException.class, () -> incrementalBuilds.build());
    verify(logger).error(anyString(), eq(exception));
  }
}