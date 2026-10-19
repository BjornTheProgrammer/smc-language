//! Updating to the newest release, for copies installed by the Velopack installers. Anything
//! else, like a `cargo install`, never offers an update.

use velopack::sources::GithubSource;
use velopack::{UpdateCheck, UpdateInfo, UpdateManager};

const REPOSITORY: &str = "https://github.com/BjornTheProgrammer/smc-language";

/// A release newer than this one
#[derive(Debug, Clone)]
pub struct Update(Box<UpdateInfo>);

impl Update {
    pub fn version(&self) -> &str {
        &self.0.TargetFullRelease.Version
    }
}

/// This copy's updater, if Velopack installed it
fn manager() -> Option<UpdateManager> {
    UpdateManager::new(GithubSource::new(REPOSITORY, None, false), None, None).ok()
}

/// The newest release, when it's newer than this one. It blocks on the network.
pub fn check() -> Option<Update> {
    match manager()?.check_for_updates().ok()? {
        UpdateCheck::UpdateAvailable(info) => Some(Update(info)),
        UpdateCheck::NoUpdateAvailable | UpdateCheck::RemoteIsEmpty => None,
    }
}

/// Downloads the update, then quits and restarts into it, so it only returns if that fails
pub fn install(update: Update) -> String {
    let result = manager()
        .ok_or_else(|| "this copy wasn't installed by the installer".to_string())
        .and_then(|manager| {
            manager
                .download_updates(&update.0, None)
                .map_err(|error| error.to_string())?;
            manager
                .apply_updates_and_restart(&*update.0)
                .map_err(|error| error.to_string())
        });
    match result {
        Ok(()) => "Restarting into the update".to_string(),
        Err(error) => format!("Update failed: {error}"),
    }
}
