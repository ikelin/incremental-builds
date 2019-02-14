package com.ikelin;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.project.MavenProject;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class MavenService {

  private final MavenSession session;
  private final Map<Path, MavenProject> projectsMap;

  /**
   * Creates a new {@code MavenService} based on the given {@code MavenSession}.
   *
   * @param session the {@code MavenSession} to build this {@code MavenService} with
   */
  public MavenService(final MavenSession session) {
    this.session = session;

    projectsMap = new HashMap<>();
    for (MavenProject project : session.getProjects()) {
      Path path = project.getBasedir().toPath().normalize();
      projectsMap.put(path, project);
    }
  }

  /**
   * Based on a set of changed file {@code Path}, returns a set of {@code MavenProject} that have
   * changed or impacted by the changes.
   *
   * @param changedFilePaths a set of file {@code Path} that have changed
   * @return a set of {@code MavenProject} that have changed or impacted by the changes
   */
  public Set<MavenProject> getChangedProjects(final Set<Path> changedFilePaths) {
    Set<MavenProject> changedProjects = new HashSet<>();
    for (Path changedFilePath : changedFilePaths) {
      MavenProject changedProject = getProject(changedFilePath);
      if (changedProject == null) {
        continue;
      }

      changedProjects.add(changedProject);
      changedProjects.addAll(getDependentProjects(changedProject));
    }
    return changedProjects;
  }

  private MavenProject getProject(final Path changedFilePath) {
    // find all projects with paths that matches the beginning of the changed file path
    Set<Path> projectPaths = new HashSet<>();
    for (Path projectPath : projectsMap.keySet()) {
      if (changedFilePath.startsWith(projectPath)) {
        projectPaths.add(projectPath);
      }
    }

    // project path with longest length is the project
    Path projectPath = null;
    int max = 0;
    for (Path path : projectPaths) {
      int length = path.toString().length();
      if (max == 0 || length > max) {
        projectPath = path;
        max = length;
      }
    }

    if (projectPath == null) {
      return null;
    }

    return projectsMap.get(projectPath);
  }

  private Set<MavenProject> getDependentProjects(final MavenProject changedProject) {
    Set<MavenProject> dependentProjects = new HashSet<>();
    for (MavenProject project : session.getProjectDependencyGraph()
        .getDownstreamProjects(changedProject, false)) {
      if (dependsOnVersion(project, changedProject)) {
        dependentProjects.add(project);
        dependentProjects.addAll(getDependentProjects(project));
      }
    }
    return dependentProjects;
  }

  private boolean dependsOnVersion(final MavenProject project,
      final MavenProject dependsOnProject) {
    for (Dependency dependency : project.getDependencies()) {
      if (dependency.getGroupId().equals(dependsOnProject.getGroupId()) &&
          dependency.getArtifactId().equals(dependsOnProject.getArtifactId()) &&
          dependency.getVersion().equals(dependsOnProject.getVersion())) {
        return true;
      }
    }
    return false;
  }

}
