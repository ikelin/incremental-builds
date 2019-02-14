package com.ikelin;

import com.google.common.annotations.VisibleForTesting;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.project.MavenProject;
import org.codehaus.plexus.logging.Logger;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.storage.file.FileRepositoryBuilder;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class IncrementalBuilds {

  private final Logger logger;
  private final MavenSession session;

  public IncrementalBuilds(final Logger logger, final MavenSession session) {
    this.logger = logger;
    this.session = session;
  }

  /**
   * Builds only projects that changed or impacted by the changes.  If no projects have changed,
   * just run Maven goal {@code validate}.
   */
  public void build() throws IOException, GitAPIException {
    // get properties
    RevInfo revInfo = RevInfo.create(session.getUserProperties());

    // get changed projects
    List<MavenProject> changedProjects = new ArrayList<>();
    try (Git git = getGit(session)) {
      Path basePath = gitBasePath(git);
      GitService gitService = getGitService(git, basePath);
      MavenService mavenService = getMavenService(session);

      Set<Path> paths = gitService.getChangedFilePaths(revInfo);
      Set<MavenProject> projects = mavenService.getChangedProjects(paths);
      changedProjects.addAll(projects);
    }

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
      logger.info("Projects changed since " + revInfo.getType() + " " + revInfo.getValue() + ":");
      logger.info("");
      for (MavenProject project : changedProjects) {
        logger.info(project.getName());
      }
    }
    logger.info("");
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
