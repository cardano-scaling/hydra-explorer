let
  users = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEMuBv9vXsKsOsjS7B6zMOpuLw+gGGHR6hADuTeiNfKO" # locallycompact
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBoR/OGSvjN2PCi2+OGeFTSTWR0aacCwK41j1gUu5UGr" # v0d1ch
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBtamRlrHLKLzr8Pcm3qEgdbJh7vCjMO4tm0wbW3REYL" # ffakenz
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIDDeluqrRI+soqfLSupug/dp8AxZ5S+gplFC+8YPzbM" # ch1bo (hydra-explorer)
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBJd9BiDoUNl0pCVDeIKnlwJu6oOmLIz7l3Ct7xoYjBS" # noonio
  ];
  hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICSZnoCbesoGNLpB3U5JPJrXE/WYpKI2ae4JPLrLKlHV";
in
{
  "github-runner-token.age".publicKeys = users ++ [ hostKey ];
}
