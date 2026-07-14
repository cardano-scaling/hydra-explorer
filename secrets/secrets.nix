let
  users = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBoR/OGSvjN2PCi2+OGeFTSTWR0aacCwK41j1gUu5UGr" # v0d1ch
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIDDeluqrRI+soqfLSupug/dp8AxZ5S+gplFC+8YPzbM" # ch1bo (hydra-explorer)
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBJd9BiDoUNl0pCVDeIKnlwJu6oOmLIz7l3Ct7xoYjBS" # noonio
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF/nkV4og13MWwILyhxQ2n3NWb2QQ4HqTuKgE9YmbIOx" # vrom911
  ];
  hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAu90XurvqxizckbEAViD/X1VMLS7U5FsZ0nbx1KRAes";
in
{
  "github-runner-token.age".publicKeys = users ++ [ hostKey ];
}
