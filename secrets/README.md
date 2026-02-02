Secrets for the NixOS machine configuration.

Store a new github token (with private key in clipboard buffer):

Steps:

1. Copy your private key into your clipboard by some scheme (i.e. open in vim,
   select all, press `y`).
2. Run this command

``` shell
nix run github:ryantm/agenix -- -e github-runner-token.age -i <(xsel -b)
```

See https://github.com/ryantm/agenix for more instructions how to use.

#### FAQs

Q: Some kind of `fd` error?
A: Your private key is not in the clipboard.
