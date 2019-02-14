package com.ikelin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.logging.Logger;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
class IncrementalBuildsTest {

  @Mock
  private Logger logger;

  @Mock
  private MavenSession session;

  @Mock
  private Properties properties;

  @Mock
  private Git git;

  @Mock
  private Path basePath;

  @Mock
  private GitService gitService;

  @Mock
  private MavenService mavenService;

  private IncrementalBuilds incrementalBuilds;

  @Captor
  private ArgumentCaptor<List<MavenProject>> projectsCaptor;

  @BeforeEach
  void beforeEach() throws IOException {
    incrementalBuilds = spy(new IncrementalBuilds(logger, session));

    doReturn(git).when(incrementalBuilds).getGit(session);
    doReturn(basePath).when(incrementalBuilds).gitBasePath(git);
    doReturn(gitService).when(incrementalBuilds).getGitService(git, basePath);
    doReturn(mavenService).when(incrementalBuilds).getMavenService(session);

    when(session.getUserProperties()).thenReturn(properties);
    when(properties.getProperty(IncrementalBuildsExtension.PROPERTY_PREFIX + ".commit"))
        .thenReturn("12345");
  }

  @Test
  void testNoChangedProjects() throws IOException, GitAPIException {
    Set<Path> changedPaths = new HashSet<>();
    when(gitService.getChangedFilePaths(any(RevInfo.class))).thenReturn(changedPaths);
    when(mavenService.getChangedProjects(changedPaths)).thenReturn(new HashSet<>());

    List<String> goals = new ArrayList<>();
    goals.add("compile");
    goals.add("test");
    when(session.getGoals()).thenReturn(goals);

    incrementalBuilds.build();

    verify(session, never()).setProjects(anyList());

    assertEquals(1, goals.size());
    assertTrue(goals.contains("validate"));
  }

  @Test
  void testChangedProjects() throws IOException, GitAPIException {
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(mock(Path.class));
    when(gitService.getChangedFilePaths(any(RevInfo.class))).thenReturn(changedPaths);
    Set<MavenProject> changedProjects = new HashSet<>();
    MavenProject changedProject = mock(MavenProject.class);
    changedProjects.add(changedProject);
    when(mavenService.getChangedProjects(changedPaths)).thenReturn(changedProjects);

    incrementalBuilds.build();

    verify(session, never()).getGoals();

    verify(session).setProjects(projectsCaptor.capture());
    List<MavenProject> actualChangedProjects = projectsCaptor.getValue();
    assertEquals(1, actualChangedProjects.size());
    assertTrue(actualChangedProjects.contains(changedProject));
  }
}