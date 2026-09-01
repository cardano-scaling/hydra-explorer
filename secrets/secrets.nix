let
  users = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBJd9BiDoUNl0pCVDeIKnlwJu6oOmLIz7l3Ct7xoYjBS" # noonio
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIATz/Jv+AnBft+9Q01UF07OydvgTTaTdCa+nMqabkUNl" # noonio
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIDDeluqrRI+soqfLSupug/dp8AxZ5S+gplFC+8YPzbM" # ch1bo (hydra-explorer)
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDHRjFKHOS4lOw907VWvDMrx/XawRMV2wyc+VSbA4YHnG2ecv6y/JT3gBjmdNw0bgltgQqeBBG/iTciio+Zax8I36rPWMEomDvpgq8B7i1L23eWoK9cKMqYNAUpIAfManhJKvZfBjJ9dRLz4hfUGo2Gah5reuweFrkzWGb2zqILNXoM2KowlkqMOFrd09SgP52sUuwNmaCJaPba7IdqzLqxotWaY420Msd5c8B2l/0E/hNgRu6m5qbZpidmQQJsTk2tq4CWP5xB2SbgEwAuZZ6AUOn2IqGfF8bkLfwHb5qdtss0jxZm47s5Fag9T9MzzbXCAHEdyO01+q83FKIxkiW/" # ch1bo
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM3SnBvHpMuwthuNJO0ROrn24lXgGkVtyrHLQuMz1WGc" # vrom911
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF/nkV4og13MWwILyhxQ2n3NWb2QQ4HqTuKgE9YmbIOx" # vrom911
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBoR/OGSvjN2PCi2+OGeFTSTWR0aacCwK41j1gUu5UGr" # v0d1ch
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP6QsbNUswjE36Mf5WcFIMHyGtOCB+rOrskJ1BmZmPJI" # v0d1ch
  ];
  hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKdMk+5kAbPYHfY3Ww7Bd16Ds8X4VzCdtFv7aPyxMsZe";
in
{
  "github-runner-token.age".publicKeys = users ++ [ hostKey ];
}
