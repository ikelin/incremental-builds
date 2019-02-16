# Maven Incremental Builds Extension for GIT

In a Maven multi-module project using GIT, this extension only builds modules that changed or impacted by the changes.

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.ikelin/incremental-builds/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.ikelin/incremental-builds)
[![Build Status](https://travis-ci.org/ikelin/incremental-builds.svg?branch=master)](https://travis-ci.org/ikelin/incremental-builds)
[![Coverage Status](https://coveralls.io/repos/github/ikelin/incremental-builds/badge.svg?branch=master)](https://coveralls.io/github/ikelin/incremental-builds?branch=master)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/ad259a2e16374ab9a6e584d07b5cc541)](https://www.codacy.com/app/ikelin/incremental-builds?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=ikelin/incremental-builds&amp;utm_campaign=Badge_Grade)

Inside a multi-module Maven project:

  * `module A` is at version `1.1-SNAPSHOT`
  * `module B` depends on `module A` version `1.0`
  * `module C` depends on `module A` version `1.1-SNAPSHOT`

When `module A` is changed, both `module A` and `module C` will be built.

## Usage

Maven `pom.xml`:

```xml
<build>
  <extensions>
    <extension>
      <groupId>com.ikelin</groupId>
      <artifactId>incremental-builds</artifactId>
      <version>{VERSION}</version>
    </extension>
  </extensions>
</build>

```

Maven command:

```
$ mvn package -Dincremental.enable=true 
```

This enables the extension and builds modules that changed between the current branch, including uncommitted files and untraced files, and the master branch.

To compare current branch against another commit, branch, or tag:

```
$ mvn package -Dincremental.enable=true -Dincremental.commit=5a6e149
$ mvn package -Dincremental.enable=true -Dincremental.branch=staging
$ mvn package -Dincremental.enable=true -Dincremental.tag=1.0.2
```

Note that Maven options `-pl` or `--projects` will override this extension's changed modules list.
