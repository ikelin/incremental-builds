package com.ikelin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.project.MavenProject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Answers;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
public class MavenServiceTest {

  @Mock(answer = Answers.RETURNS_DEEP_STUBS)
  private MavenSession session;

  private List<MavenProject> projects;
  private MavenService mavenService;

  @BeforeEach
  public void beforeEach() {
    projects = new ArrayList<>();
  }

  @Test
  public void testChangedProjects() {
    // changed project
    MavenProject project = createMavenProject("project", "/modules/project", "org.organization1",
        "project1", "1.0");
    projects.add(project);
    doReturn(projects).when(session).getProjects();

    // changed project dependency
    when(session.getProjectDependencyGraph().getDownstreamProjects(project, false))
        .thenReturn(Arrays.asList());

    // changed path
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(
        Paths.get("/modules", "/project", "/src", "/main", "/org", "/organization1", "/foo.java"));

    mavenService = new MavenService(session);
    Set<MavenProject> changedProjects = mavenService.getChangedProjects(changedPaths);

    assertEquals(1, changedProjects.size());
    assertTrue(changedProjects.contains(project));
  }

  @Test
  public void testChangedProjectsWithDifferentProjects() {
    // changed project
    MavenProject project1 = createMavenProject("project1", "/modules/project1", "org.organization1",
        "project1", "1.0");
    projects.add(project1);
    MavenProject project2 = createMavenProject("project2", "/modules/project2", "org.organization2",
        "project2", "2.0");
    projects.add(project2);
    doReturn(projects).when(session).getProjects();

    // changed project dependency
    when(session.getProjectDependencyGraph().getDownstreamProjects(project2, false))
        .thenReturn(Arrays.asList());

    // changed path
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(
        Paths.get("/modules", "/project2", "/src", "/main", "/org", "/organization1", "/foo.java"));

    mavenService = new MavenService(session);
    Set<MavenProject> changedProjects = mavenService.getChangedProjects(changedPaths);

    assertEquals(1, changedProjects.size());
    assertTrue(changedProjects.contains(project2));
  }

  @Test
  public void testChangedProjectsWithParentChildProjects() {
    // changed project
    MavenProject parentProject = createMavenProject("parent-project", "/modules/parent-project",
        "org.organization", "parent-project", "1.0");
    projects.add(parentProject);
    MavenProject childProject = createMavenProject(
        "child-project",
        "/modules/parent-project/child-project",
        "org.organization",
        "child-project",
        "1.0"
    );
    projects.add(childProject);
    doReturn(projects).when(session).getProjects();

    // changed project dependency
    when(session.getProjectDependencyGraph().getDownstreamProjects(childProject, false))
        .thenReturn(Arrays.asList());

    // changed path
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(Paths
        .get("/modules", "/parent-project", "/child-project", "/src", "/main", "/org",
            "/organization", "/foo.java"));

    mavenService = new MavenService(session);
    Set<MavenProject> changedProjects = mavenService.getChangedProjects(changedPaths);

    assertEquals(1, changedProjects.size());
    assertTrue(changedProjects.contains(childProject));
  }

  @Test
  public void testChangedProjectsWithDependentProjectsImpacted() {
    // changed project
    MavenProject project1 = createMavenProject("project1", "/modules/project1", "org.organization",
        "project1", "1.0");
    projects.add(project1);
    MavenProject project2 = createMavenProject("project2", "/modules/project2", "org.organization",
        "project2", "2.0");
    addDependency(project2, project1);
    projects.add(project2);
    doReturn(projects).when(session).getProjects();

    // changed project dependency
    when(session.getProjectDependencyGraph().getDownstreamProjects(project1, false))
        .thenReturn(Arrays.asList(project2));
    when(session.getProjectDependencyGraph().getDownstreamProjects(project2, false))
        .thenReturn(Arrays.asList());

    // changed path
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(
        Paths.get("/modules", "/project1", "/src", "/main", "/org", "/organization", "/foo.java"));

    mavenService = new MavenService(session);
    Set<MavenProject> changedProjects = mavenService.getChangedProjects(changedPaths);

    assertEquals(2, changedProjects.size());
    assertTrue(changedProjects.contains(project1));
    assertTrue(changedProjects.contains(project2));
  }

  @Test
  public void testChangedProjectsWithDependentProjectsNotImpacted() {
    // changed project
    MavenProject project1 = createMavenProject("project1", "/modules/project1", "org.organization",
        "project1", "1.0");
    projects.add(project1);
    MavenProject project2 = createMavenProject("project2", "/modules/project2", "org.organization",
        "project2", "1.0");
    addDependency(project2, project1);
    projects.add(project2);
    doReturn(projects).when(session).getProjects();

    // changed project dependency
    when(session.getProjectDependencyGraph().getDownstreamProjects(project1, false))
        .thenReturn(Arrays.asList(project2));
    when(session.getProjectDependencyGraph().getDownstreamProjects(project2, false))
        .thenReturn(Arrays.asList());

    // changed path
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(
        Paths.get("/modules", "/project2", "/src", "/main", "/org", "/organization", "/foo.java"));

    mavenService = new MavenService(session);
    Set<MavenProject> changedProjects = mavenService.getChangedProjects(changedPaths);

    assertEquals(1, changedProjects.size());
    assertTrue(changedProjects.contains(project2));
  }

  @Test
  public void testChangedProjectsWithDependentProjectsNotImpactedDifferentVersion() {
    // changed project
    MavenProject project1v2 = createMavenProject("project1", "/modules/project1",
        "org.organization", "project1", "2.0");
    projects.add(project1v2);
    MavenProject project2 = createMavenProject("project2", "/modules/project2", "org.organization",
        "project2", "1.0");
    addDependency(project2,
        createMavenProject("project1", "/modules/project1", "org.organization", "project1", "1.0"));
    projects.add(project2);
    doReturn(projects).when(session).getProjects();

    // changed project dependency
    when(session.getProjectDependencyGraph().getDownstreamProjects(project1v2, false))
        .thenReturn(Arrays.asList(project2));
    when(session.getProjectDependencyGraph().getDownstreamProjects(project2, false))
        .thenReturn(Arrays.asList());

    // changed path
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(
        Paths.get("/modules", "/project1", "/src", "/main", "/org", "/organization", "/foo.java"));

    mavenService = new MavenService(session);
    Set<MavenProject> changedProjects = mavenService.getChangedProjects(changedPaths);

    assertEquals(1, changedProjects.size());
    assertTrue(changedProjects.contains(project1v2));
  }

  @Test
  public void testChangedProjectsWithTransitiveDependentProjectsImpacted() {
    // changed project
    MavenProject project1 = createMavenProject("project1", "/modules/project1", "org.organization1",
        "project1", "1.0");
    projects.add(project1);
    MavenProject project2 = createMavenProject("project2", "/modules/project2", "org.organization2",
        "project2", "2.0");
    projects.add(project2);
    MavenProject project3 = createMavenProject("project3", "/modules/project3", "org.organization3",
        "project3", "3.0");
    addDependency(project3, project2);
    projects.add(project3);
    MavenProject project4 = createMavenProject("project4", "/modules/project4", "org.organization4",
        "project4", "4.0");
    addDependency(project4, project1);
    addDependency(project4, project3);
    projects.add(project4);
    doReturn(projects).when(session).getProjects();

    // changed project dependency
    when(session.getProjectDependencyGraph().getDownstreamProjects(project1, false))
        .thenReturn(Arrays.asList(project4));
    when(session.getProjectDependencyGraph().getDownstreamProjects(project2, false))
        .thenReturn(Arrays.asList(project3));
    when(session.getProjectDependencyGraph().getDownstreamProjects(project3, false))
        .thenReturn(Arrays.asList(project4));
    when(session.getProjectDependencyGraph().getDownstreamProjects(project4, false))
        .thenReturn(Arrays.asList());

    // changed path
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(
        Paths.get("/modules", "/project2", "/src", "/main", "/org", "/organization2", "/foo.java"));
    changedPaths.add(Paths
        .get("/modules", "/project2", "/src", "/main", "/org", "/organization2", "/bar",
            "/bar.java"));

    mavenService = new MavenService(session);
    Set<MavenProject> changedProjects = mavenService.getChangedProjects(changedPaths);

    assertEquals(3, changedProjects.size());
    assertTrue(changedProjects.contains(project2));
    assertTrue(changedProjects.contains(project3));
    assertTrue(changedProjects.contains(project4));
  }

  @Test
  public void testNoChangedProjectsDifferentProject() {
    // changed project
    MavenProject project = createMavenProject("project", "/modules/project", "org.organization1",
        "project1", "1.0");
    projects.add(project);
    doReturn(projects).when(session).getProjects();

    // changed project dependency
    when(session.getProjectDependencyGraph().getDownstreamProjects(project, false))
        .thenReturn(Arrays.asList());

    // changed path
    Set<Path> changedPaths = new HashSet<>();
    changedPaths.add(Paths
        .get("/modules", "/project-baz", "/src", "/main", "/org", "/organization1", "/baz.java"));

    mavenService = new MavenService(session);
    Set<MavenProject> changedProjects = mavenService.getChangedProjects(changedPaths);

    assertTrue(changedProjects.isEmpty());
  }

  private MavenProject createMavenProject(String name, String path, String groupId,
      String artifactId, String version) {
    MavenProject project = new MavenProject();
    project.setName(name);
    project.setFile(new File(path, "/pom.xml"));
    project.setGroupId(groupId);
    project.setArtifactId(artifactId);
    project.setVersion(version);
    return project;
  }

  private MavenProject addDependency(MavenProject project, MavenProject dependentProject) {
    Dependency dependency = new Dependency();
    dependency.setGroupId(dependentProject.getGroupId());
    dependency.setArtifactId(dependentProject.getArtifactId());
    dependency.setVersion(dependentProject.getVersion());
    project.getDependencies().add(dependency);
    return project;
  }

}