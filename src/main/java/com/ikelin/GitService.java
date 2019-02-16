package com.ikelin;

import com.google.common.annotations.VisibleForTesting;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.InvalidRefNameException;
import org.eclipse.jgit.diff.DiffEntry;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.lib.ObjectReader;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevTree;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.treewalk.AbstractTreeIterator;
import org.eclipse.jgit.treewalk.CanonicalTreeParser;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Provides GIT operations.
 */
public class GitService {

  private final Git git;
  private final Path basePath;

  /**
   * Creates a new {@code GitService}.
   *
   * @param git Git APIs that interact with a repository
   * @param basePath base path to the Git repository
   */
  public GitService(final Git git, final Path basePath) {
    this.git = git;
    this.basePath = basePath;
  }

  /**
   * Returns a set of {@code Path} that have changed between current branch and the provided {@code
   * RevInfo}.
   *
   * @param revInfo the revision info to compare against
   * @return set of {@code Path} that have changed
   */
  public Set<Path> getChangedFilePaths(final RevInfo revInfo) {
    Set<Path> changedPaths = new HashSet<>();
    try {
      changedPaths.addAll(getDiff(revInfo));
      changedPaths.addAll(getUncommitted());
    } catch (IOException | GitAPIException ex) {
      throw new RuntimeException(ex);
    }
    return changedPaths;
  }

  @VisibleForTesting
  AbstractTreeIterator getTreeIterator(final ObjectId objectId) throws IOException {
    try (RevWalk walk = new RevWalk(git.getRepository())) {
      RevCommit commit = walk.parseCommit(objectId);
      RevTree tree = walk.parseTree(commit.getTree().getId());

      CanonicalTreeParser treeParser = new CanonicalTreeParser();
      try (ObjectReader reader = git.getRepository().newObjectReader()) {
        treeParser.reset(reader, tree.getId());
      }
      walk.dispose();
      return treeParser;
    }
  }

  private Set<Path> getDiff(final RevInfo revInfo) throws IOException, GitAPIException {
    // old tree
    String revInfoVal = revInfo.getValue();
    if (revInfo.getType() == RevInfo.Type.BRANCH
        && git.getRepository().resolve(revInfoVal) == null) {
      git.branchCreate().setName(revInfoVal).setStartPoint("origin/" + revInfoVal).call();
    }

    ObjectId oldObjectId = git.getRepository().resolve(revInfoVal);
    if (oldObjectId == null) {
      throw new InvalidRefNameException(revInfo.getType() + " " + revInfoVal + " does not exist");
    }

    AbstractTreeIterator oldTree = getTreeIterator(oldObjectId);

    // new tree
    ObjectId newObjectId = git.getRepository().resolve("HEAD");
    AbstractTreeIterator newTree = getTreeIterator(newObjectId);

    // diff entries
    List<DiffEntry> diffEntries = git.diff().setOldTree(oldTree).setNewTree(newTree).call();

    // changed paths
    return getDiff(diffEntries);
  }

  private Set<Path> getDiff(List<DiffEntry> diffEntries) {
    Set<Path> changedPaths = new HashSet<>();
    for (DiffEntry entry : diffEntries) {
      switch (entry.getChangeType()) {
        case ADD:
          changedPaths.add(getPath(entry.getNewPath()));
          break;
        case DELETE:
        case MODIFY:
          changedPaths.add(getPath(entry.getOldPath()));
          break;
        case COPY:
        case RENAME:
          changedPaths.add(getPath(entry.getNewPath()));
          changedPaths.add(getPath(entry.getOldPath()));
          break;
        default:
          break;
      }
    }
    return changedPaths;
  }

  private Set<Path> getUncommitted() throws GitAPIException {
    return getPaths(git.status().call().getUncommittedChanges());
  }

  private Path getPath(final String file) {
    return basePath.resolve(file).normalize();
  }

  private Set<Path> getPaths(final Set<String> files) {
    Set<Path> changedPaths = new HashSet<>();
    for (String changedFile : files) {
      changedPaths.add(getPath(changedFile));
    }
    return changedPaths;
  }
}
