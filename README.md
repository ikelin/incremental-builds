# Maven Incremental Build Extension for GIT

In a Maven multi-module project using GIT, this extension only builds modules that changed or impacted by the changes.
 
## Example Scenario

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
      <version>[VERSION]</version>
    </extension>
  </extensions>
</build>

```

Maven command:

```sh
$ mvn package -Dincremental.enable=true 
```

This enables the extension and builds modules that changed between the current branch, including uncommitted files and untraced files, and the master branch.

To compare current branch against another commits, branches, or tags:

```sh
$ mvn package -Dincremental.enable=true -Dincremental.commit=5a6e149
$ mvn package -Dincremental.enable=true -Dincremental.branch=staging
$ mvn package -Dincremental.enable=true -Dincremental.tag=1.0.2
```
