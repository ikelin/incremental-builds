package com.ikelin;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;

import com.ikelin.RevInfo.Type;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Properties;

class RevInfoTest {

  @Test
  void testCommit() {
    Properties properties = mock(Properties.class);
    String commit = "commit";
    doReturn(commit).when(properties)
        .getProperty(IncrementalBuildsExtension.PROPERTY_PREFIX + ".commit");

    RevInfo revInfo = RevInfo.create(properties);

    Assertions.assertEquals(Type.COMMIT, revInfo.getType());
    assertEquals(commit, revInfo.getValue());
  }

  @Test
  void testBranch() {
    Properties properties = mock(Properties.class);
    String branch = "branch";
    doReturn(branch).when(properties)
        .getProperty(IncrementalBuildsExtension.PROPERTY_PREFIX + ".branch");

    RevInfo revInfo = RevInfo.create(properties);

    assertEquals(Type.BRANCH, revInfo.getType());
    assertEquals(branch, revInfo.getValue());
  }

  @Test
  void testTag() {
    Properties properties = mock(Properties.class);
    String tag = "tag";
    doReturn(tag).when(properties).getProperty(IncrementalBuildsExtension.PROPERTY_PREFIX + ".tag");

    RevInfo revInfo = RevInfo.create(properties);

    assertEquals(Type.TAG, revInfo.getType());
    assertEquals(tag, revInfo.getValue());
  }

  @Test
  void testNone() {
    Properties properties = mock(Properties.class);

    RevInfo revInfo = RevInfo.create(properties);

    assertEquals(Type.BRANCH, revInfo.getType());
    assertEquals("master", revInfo.getValue());
  }
}