//! Updating to the newest release. Only copies installed by the release installers can
//! update themselves, since those leave an install receipt saying where they went.

use axoupdater::{AxoUpdater, Version};

const APP: &str = "smc-emu";

/// The installed copy's updater, if this is it
fn updater() -> Option<AxoUpdater> {
    let mut updater = AxoUpdater::new_for(APP);
    updater.load_receipt().ok()?;
    if !updater.check_receipt_is_for_this_executable().ok()? {
        return None;
    }
    // The running build knows its version better than a receipt left by an older install
    let version: Version = env!("CARGO_PKG_VERSION").parse().ok()?;
    updater.set_current_version(version).ok()?;
    Some(updater)
}

/// The newest release's version, when it's newer than this one and this copy can update
pub async fn check() -> Option<String> {
    let mut updater = updater()?;
    if !updater.is_update_needed().await.ok()? {
        return None;
    }
    let version = updater.query_new_version().await.ok()??;
    Some(version.to_string())
}

/// Installs the newest release over this copy, returning its version. It takes effect the
/// next time the emulator starts.
pub async fn install() -> Result<String, String> {
    let mut updater = updater().ok_or("This copy wasn't installed by the installer")?;
    updater.disable_installer_output();
    match updater.run().await {
        Ok(Some(result)) => Ok(result.new_version.to_string()),
        Ok(None) => Err("Already up to date".to_string()),
        Err(error) => Err(error.to_string()),
    }
}
