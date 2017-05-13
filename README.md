# haven
## Recursively retrieve maven dependencies

A haskell project that retrieves maven package dependencies recursively given a starting set of packages. The primary output format is a list of [nix sets](http://nixos.org/nix/manual/#idm140737318096432) describing the maven packages.

### Example
```bash
$ haven com.google.guava:guava:17.0 com.google.code.gson:gson:2.2.4
[  { artifactId = "gson";
    groupId = "com.google.code.gson";
    version = "2.2.4";
    jarSha256 = "c0328cd07ca9e363a5acd00c1cf4afe8cf554bd6d373834981ba05cebec687fb";
    pomSha256 = "ae984d5d19894ce6dc4689866eaa12f8fc31409113e60ee3b44853b8ac4fd380"; }
  { artifactId = "guava";
    groupId = "com.google.guava";
    version = "17.0";
    jarSha256 = "8c36a80ea613d0b6b8040a17cf837c5bbe3677bc1b06a058a6c174fdb787ebbc";
    pomSha256 = "2a0d16010d3825c732aeab1d25f181074945063d8f76c28004e7d423d66cb75b"; }
  { artifactId = "junit";
    groupId = "junit";
    version = "3.8.2";
    jarSha256 = "ecdcc08183708ea3f7b0ddc96f19678a0db8af1fb397791d484aed63200558b0";
    pomSha256 = "aede67999f02ac851c2a2ae8cec58f9d801f826ba20994df23a1d9fbecc47f0f"; }
]
```
