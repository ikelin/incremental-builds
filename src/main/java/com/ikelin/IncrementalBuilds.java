package com.ikelin;

import com.google.common.annotations.VisibleForTesting;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.storage.file.FileRepositoryBuilder;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Detects projects that changed or impacted by the changes.
 */
public class IncrementalBuilds {

  private final MavenSession session;

  /**
   * Creates a new instance of {@code IncrementalBuilds}.
   */
  public IncrementalBuilds(final MavenSession session) {
    this.session = session;
  }

  /**
   * Returns a list of projects that changed or impacted by the changes between current working
   * branch and the provided {@RevInfo}.
   *
   * @param revInfo the revision info to detect the changes against
   * @return list of changed projects
   */
  public List<MavenProject> getChangedProjects(RevInfo revInfo) {
    List<MavenProject> changedProjects = new ArrayList<>();
    try (Git git = getGit(session)) {
      Path basePath = gitBasePath(git);
      GitService gitService = getGitService(git, basePath);
      MavenService mavenService = getMavenService(session);

      Set<Path> paths = gitService.getChangedFilePaths(revInfo);
      Set<MavenProject> projects = mavenService.getChangedProjects(paths);
      changedProjects.addAll(projects);
    } catch (IOException ex) {
      throw new RuntimeException(ex);
    }
    return changedProjects;
  }

  @VisibleForTesting
  Git getGit(final MavenSession session) throws IOException {
    File baseDir = session.getCurrentProject().getBasedir();
    Repository repository = new FileRepositoryBuilder().setMustExist(true).readEnvironment()
        .findGitDir(baseDir).build();
    return new Git(repository);
  }

  @VisibleForTesting
  Path gitBasePath(final Git git) throws IOException {
    return Paths.get(git.getRepository().getDirectory().getCanonicalPath()).getParent();
  }

  @VisibleForTesting
  GitService getGitService(final Git git, final Path basePath) {
    return new GitService(git, basePath);
  }

  @VisibleForTesting
  MavenService getMavenService(final MavenSession session) {
    return new MavenService(session);
  }

}
