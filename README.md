# haven
## Recursively retrieve maven dependencies

A haskell project that uses Maven's `dependency:tree` command to get a
list of Maven dependencies. The primary output format is a list of
[nix sets](http://nixos.org/nix/manual/#idm140737318096432) describing
the maven packages. Maven's local repo is redirected to a tmp
directory, so haven won't pollute your home directory.

### Example

#### `pom.xml`

```xml
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
     xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
  <modelVersion>4.0.0</modelVersion>

  <groupId>com.dummy</groupId>
  <artifactId>dummy</artifactId>
  <version>1.0-SNAPSHOT</version>

  <repositories>
    <repository>
      <id>maven</id>
      <url>https://repo1.maven.org/maven2/</url>
    </repository>
    <repository>
      <id>central</id>
      <url>https://central.maven.org/maven2/</url>
    </repository>
    <repository>
      <id>jcenter</id>
      <url>https://jcenter.bintray.com/</url>
    </repository>
  </repositories>

  <dependencies>
    <dependency>
      <groupId>com.android.tools.build</groupId>
      <artifactId>gradle</artifactId>
      <version>2.3.1</version>
    </dependency>
    <dependency>
      <groupId>com.google.gms</groupId>
      <artifactId>google-services</artifactId>
      <version>3.0.0</version>
    </dependency>
    <dependency>
      <groupId>com.firebase</groupId>
      <artifactId>firebase-jobdispatcher</artifactId>
      <version>0.5.2</version>
      <type>aar</type>
      <exclusions>
        <exclusion>
          <groupId>com.android.support</groupId>
          <artifactId>support-v4</artifactId>
        </exclusion>
      </exclusions>
    </dependency>
  </dependencies>
</project>
```

#### Output

```bash
$ haven com.google.guava:guava:17.0 com.google.code.gson:gson:2.2.4
$ haven ./pom.xml
... # Maven logs on stderr

[
  { artifactId = "gradle";
    groupId = "com.android.tools.build";
    version = "2.3.1";
    repo = "https://jcenter.bintray.com";
    jarSha256 = "c621e827cf3b1dd6607d6c3f2dacdbea81cc4000ef3d7b91d7e967f368b9d9c3";
    pomSha256 = "9a240dcae4b27de87af1cfb7164cb2097785e32dc1bca39cdaa1f5504c50cae4";
    aarSha256 = null; }

  { artifactId = "gradle-core";
    groupId = "com.android.tools.build";
    version = "2.3.1";
    repo = "https://jcenter.bintray.com";
    jarSha256 = "6584bba738af0e1435521258ad20352e4f22e1ad9d6e3e10b566f9e316674804";
    pomSha256 = "d393f858b613952f08d01ba6515a91f910be30768d6e05729ac254d632c87e3b";
    aarSha256 = null; }

  { artifactId = "builder";
    groupId = "com.android.tools.build";
    version = "2.3.1";
    repo = "https://jcenter.bintray.com";
    jarSha256 = "b1a948c366e98061d840f2333d1467fbca7a53ff6d81f7a46cd15aea0a1272cc";
    pomSha256 = "883026a359536e2d05a01cb8cea06e076bb013eccf1fb0ce35d4d96beda60521";
    aarSha256 = null; }

  { artifactId = "builder-model";
    groupId = "com.android.tools.build";
    version = "2.3.1";
    repo = "https://jcenter.bintray.com";
    jarSha256 = "37ee6a2cbabc5ff9968dfbb58025aa53aaac9795d4d9a50f2a0883076e7398fb";
    pomSha256 = "34245bc246fe93b3c3bf6f933a51efbb36d300e8fcee6cec4cee51653a1469b3";
    aarSha256 = null; }

...

]
```
