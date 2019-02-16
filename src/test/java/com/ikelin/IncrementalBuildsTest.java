package com.ikelin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.eclipse.jgit.api.Git;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
public class IncrementalBuildsTest {

  @Mock
  private MavenSession session;

  @Mock
  private Git git;

  @Mock
  private Path basePath;

  @Mock
  private GitService gitService;

  @Mock
  private MavenService mavenService;

  @Mock
  private RevInfo revInfo;

  private IncrementalBuilds incrementalBuilds;

  @BeforeEach
  public void beforeEach() throws IOException {
    incrementalBuilds = spy(new IncrementalBuilds(session));

    doReturn(git).when(incrementalBuilds).getGit(session);
    doReturn(basePath).when(incrementalBuilds).gitBasePath(git);
    doReturn(gitService).when(incrementalBuilds).getGitService(git, basePath);
    doReturn(mavenService).when(incrementalBuilds).getMavenService(session);
  }

  @Test
  public void testNoChangedProjects() {
    Set<Path> changedPaths = new HashSet<>();
    when(gitService.getChangedFilePaths(any(RevInfo.class))).thenReturn(changedPaths);
    when(mavenService.getChangedProjects(changedPaths)).thenReturn(new HashSet<>());

    List<MavenProject> changedProjects = incrementalBuilds.getChangedProjects(revInfo);

    assertTrue(changedProjects.isEmpty());
  }

  @Test
  public void testChangedProjects() {
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(mock(Path.class));
    when(gitService.getChangedFilePaths(any(RevInfo.class))).thenReturn(changedPaths);
    Set<MavenProject> changedProjects = new HashSet<>();
    MavenProject changedProject = mock(MavenProject.class);
    changedProjects.add(changedProject);
    when(mavenService.getChangedProjects(changedPaths)).thenReturn(changedProjects);

    List<MavenProject> actualChangedProjects = incrementalBuilds.getChangedProjects(revInfo);

    verify(session, never()).getGoals();

    assertEquals(1, actualChangedProjects.size());
    assertTrue(actualChangedProjects.contains(changedProject));
  }
}