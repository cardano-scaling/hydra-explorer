Secrets for the NixOS machine configuration.

Store a new github token (with private key in clipboard buffer):

``` shell
nix run github:ryantm/agenix -- -e github-runner-token.age -i <(xsel -b)
```

See https://github.com/ryantm/agenix for more instructions how to use.
