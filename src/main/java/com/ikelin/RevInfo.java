package com.ikelin;

import static com.ikelin.IncrementalBuildsExtension.PROPERTY_PREFIX;

import java.util.Properties;

/**
 * Model for revision info.
 */
public class RevInfo {

  public enum Type {
    COMMIT,
    BRANCH,
    TAG
  }

  private final Type type;
  private final String value;

  /**
   * Creates a new {@code RevInfo}.
   *
   * @param type the type of revision info
   * @param value the value of the revision info, such as git commit sha, git branch name, or
   *     git tag name
   */
  public RevInfo(final Type type, final String value) {
    this.type = type;
    this.value = value;
  }

  public Type getType() {
    return type;
  }

  public String getValue() {
    return value;
  }

  /**
   * Creates a new {@code RevInfo} based on the provided properties.  If no properties are provided,
   * default to {@code master} branch.
   */
  public static RevInfo create(final Properties properties) {
    String commit = properties.getProperty(PROPERTY_PREFIX + ".commit");
    if (commit != null) {
      return new RevInfo(Type.COMMIT, commit);
    }

    String branch = properties.getProperty(PROPERTY_PREFIX + ".branch");
    if (branch != null) {
      return new RevInfo(Type.BRANCH, branch);
    }

    String tag = properties.getProperty(PROPERTY_PREFIX + ".tag");
    if (tag != null) {
      return new RevInfo(Type.TAG, tag);
    }

    return new RevInfo(Type.BRANCH, "master");
  }
}
