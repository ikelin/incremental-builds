package com.ikelin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.ikelin.RevInfo.Type;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.api.errors.InvalidRefNameException;
import org.eclipse.jgit.diff.DiffEntry;
import org.eclipse.jgit.diff.DiffEntry.ChangeType;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.treewalk.AbstractTreeIterator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Answers;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@ExtendWith(MockitoExtension.class)
class GitServiceTest {

  @Mock(answer = Answers.RETURNS_DEEP_STUBS)
  private Git git;

  private Path basePath;

  @Mock
  private AbstractTreeIterator treeIterator;

  private GitService gitService;

  @BeforeEach
  void beforeEach() {
    basePath = Paths.get("/modules");
    gitService = Mockito.spy(new GitService(git, basePath));
  }

  @Test
  void testDiffInvalidRevInfoValue() throws IOException {
    when(git.getRepository().resolve(anyString())).thenReturn(null);

    RevInfo revInfo = new RevInfo(Type.COMMIT, "12345");
    assertThrows(InvalidRefNameException.class, () -> gitService.getChangedFilePaths(revInfo));
  }

  @Test
  void testDiffAdd() throws IOException, GitAPIException {
    when(git.getRepository().resolve(anyString())).thenReturn(mock(ObjectId.class));
    doReturn(treeIterator).when(gitService).getTreeIterator(any(ObjectId.class));

    List<DiffEntry> diffEntries = new ArrayList<>();
    DiffEntry add = mock(DiffEntry.class);
    when(add.getChangeType()).thenReturn(ChangeType.ADD);
    String addNewPath = "/modules/project/add";
    when(add.getNewPath()).thenReturn(addNewPath);
    diffEntries.add(add);
    when(git.diff().setOldTree(any(AbstractTreeIterator.class))
        .setNewTree(any(AbstractTreeIterator.class)).call()).thenReturn(diffEntries);

    RevInfo revInfo = new RevInfo(Type.COMMIT, "12345");
    Set<Path> changedPaths = gitService.getChangedFilePaths(revInfo);

    verify(add, never()).getOldPath();
    assertEquals(1, changedPaths.size());
    assertEquals(addNewPath, changedPaths.stream().findFirst().get().toString());
  }

  @Test
  void testDiffDelete() throws IOException, GitAPIException {
    when(git.getRepository().resolve(anyString())).thenReturn(mock(ObjectId.class));
    doReturn(treeIterator).when(gitService).getTreeIterator(any(ObjectId.class));

    List<DiffEntry> diffEntries = new ArrayList<>();
    DiffEntry delete = mock(DiffEntry.class);
    when(delete.getChangeType()).thenReturn(ChangeType.DELETE);
    String deleteOldPath = "/modules/project/delete";
    when(delete.getOldPath()).thenReturn(deleteOldPath);
    diffEntries.add(delete);
    when(git.diff().setOldTree(any(AbstractTreeIterator.class))
        .setNewTree(any(AbstractTreeIterator.class)).call()).thenReturn(diffEntries);

    RevInfo revInfo = new RevInfo(Type.COMMIT, "12345");
    Set<Path> changedPaths = gitService.getChangedFilePaths(revInfo);

    verify(delete, never()).getNewPath();
    assertEquals(1, changedPaths.size());
    assertEquals(deleteOldPath, changedPaths.stream().findFirst().get().toString());
  }

  @Test
  void testDiffDeleteModify() throws IOException, GitAPIException {
    when(git.getRepository().resolve(anyString())).thenReturn(mock(ObjectId.class));
    doReturn(treeIterator).when(gitService).getTreeIterator(any(ObjectId.class));

    List<DiffEntry> diffEntries = new ArrayList<>();
    DiffEntry modify = mock(DiffEntry.class);
    when(modify.getChangeType()).thenReturn(ChangeType.MODIFY);
    String modifyOldPath = "/modules/project/modify";
    when(modify.getOldPath()).thenReturn(modifyOldPath);
    diffEntries.add(modify);
    when(git.diff().setOldTree(any(AbstractTreeIterator.class))
        .setNewTree(any(AbstractTreeIterator.class)).call()).thenReturn(diffEntries);

    RevInfo revInfo = new RevInfo(Type.COMMIT, "12345");
    Set<Path> changedPaths = gitService.getChangedFilePaths(revInfo);

    verify(modify, never()).getNewPath();
    assertEquals(1, changedPaths.size());
    assertEquals(modifyOldPath, changedPaths.stream().findFirst().get().toString());
  }

  @Test
  void testDiffDeleteCopy() throws IOException, GitAPIException {
    when(git.getRepository().resolve(anyString())).thenReturn(mock(ObjectId.class));
    doReturn(treeIterator).when(gitService).getTreeIterator(any(ObjectId.class));

    List<DiffEntry> diffEntries = new ArrayList<>();
    DiffEntry copy = mock(DiffEntry.class);
    when(copy.getChangeType()).thenReturn(ChangeType.COPY);
    String copyOldPath = "/modules/project/copy/from";
    String copyNewPath = "/modules/project/copy/to";
    when(copy.getOldPath()).thenReturn(copyOldPath);
    when(copy.getNewPath()).thenReturn(copyNewPath);
    diffEntries.add(copy);
    when(git.diff().setOldTree(any(AbstractTreeIterator.class))
        .setNewTree(any(AbstractTreeIterator.class)).call()).thenReturn(diffEntries);

    RevInfo revInfo = new RevInfo(Type.COMMIT, "12345");
    Set<Path> changedPaths = gitService.getChangedFilePaths(revInfo);

    assertEquals(2, changedPaths.size());
    for (Path changedPath : changedPaths) {
      assertTrue(
          changedPath.toString().equals(copyOldPath) || changedPath.toString().equals(copyNewPath));
    }
  }

  @Test
  void testDiffDeleteRename() throws IOException, GitAPIException {
    when(git.getRepository().resolve(anyString())).thenReturn(mock(ObjectId.class));
    doReturn(treeIterator).when(gitService).getTreeIterator(any(ObjectId.class));

    List<DiffEntry> diffEntries = new ArrayList<>();
    DiffEntry rename = mock(DiffEntry.class);
    when(rename.getChangeType()).thenReturn(ChangeType.COPY);
    String renameOldPath = "/modules/project/rename/from";
    String renameNewPath = "/modules/project/rename/to";
    when(rename.getOldPath()).thenReturn(renameOldPath);
    when(rename.getNewPath()).thenReturn(renameNewPath);
    diffEntries.add(rename);
    when(git.diff().setOldTree(any(AbstractTreeIterator.class))
        .setNewTree(any(AbstractTreeIterator.class)).call()).thenReturn(diffEntries);

    RevInfo revInfo = new RevInfo(Type.COMMIT, "12345");
    Set<Path> changedPaths = gitService.getChangedFilePaths(revInfo);

    assertEquals(2, changedPaths.size());
    for (Path changedPath : changedPaths) {
      assertTrue(changedPath.toString().equals(renameOldPath) || changedPath.toString()
          .equals(renameNewPath));
    }
  }

  @Test
  void testDiffByBranch() throws IOException, GitAPIException {
    when(git.getRepository().resolve(anyString())).thenReturn(null, mock(ObjectId.class));
    doReturn(treeIterator).when(gitService).getTreeIterator(any(ObjectId.class));
    when(git.diff().setOldTree(any(AbstractTreeIterator.class))
        .setNewTree(any(AbstractTreeIterator.class)).call()).thenReturn(new ArrayList<>());

    RevInfo revInfo = new RevInfo(Type.BRANCH, "12345");
    Set<Path> changedPaths = gitService.getChangedFilePaths(revInfo);

    assertEquals(0, changedPaths.size());
    verify(git.branchCreate().setName(revInfo.getValue())
        .setStartPoint("origin/" + revInfo.getValue())).call();
  }

  @Test
  void testUncommitted() throws IOException, GitAPIException {
    when(git.getRepository().resolve(anyString())).thenReturn(mock(ObjectId.class));
    doReturn(treeIterator).when(gitService).getTreeIterator(any(ObjectId.class));
    when(git.diff().setOldTree(any(AbstractTreeIterator.class))
        .setNewTree(any(AbstractTreeIterator.class)).call()).thenReturn(new ArrayList<>());
    Set<String> uncommittedPaths = new HashSet<>();
    String uncommittedPath = "/modules/projects/uncomitted";
    uncommittedPaths.add(uncommittedPath);
    when(git.status().call().getUncommittedChanges()).thenReturn(uncommittedPaths);

    RevInfo revInfo = new RevInfo(Type.COMMIT, "12345");
    Set<Path> changedPaths = gitService.getChangedFilePaths(revInfo);

    assertEquals(1, changedPaths.size());
    assertEquals(uncommittedPath, changedPaths.stream().findFirst().get().toString());
  }

}