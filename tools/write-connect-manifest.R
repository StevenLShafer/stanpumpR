# Generate the dependency manifest required by Posit Connect Cloud.
# Run from the repository root after restoring the project dependencies.
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Install the rsconnect package before generating manifest.json.")
}

rsconnect::writeManifest(
  appDir = ".",
  appPrimaryDoc = "app.R"
)

manifest <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
stanpumpMetadata <- manifest$packages$stanpumpR
if (is.null(stanpumpMetadata)) {
  stop("manifest.json does not include stanpumpR as an application dependency.")
}
remoteType <- stanpumpMetadata$RemoteType
if (is.null(remoteType)) remoteType <- stanpumpMetadata$Source
if (is.null(remoteType)) remoteType <- ""
if (!tolower(remoteType) %in% c("github", "git")) {
  stop(
    "stanpumpR is not recorded as a GitHub dependency. Install the exact GitHub ",
    "revision, regenerate the manifest, and inspect its stanpumpR entry."
  )
}

message("Wrote manifest.json. Verify its stanpumpR commit, then commit the manifest.")
