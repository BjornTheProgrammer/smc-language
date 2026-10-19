//! Updating the SMC tools to their newest release. Only copies installed by the release
//! installers can update themselves, since those leave an install receipt saying where they
//! went; anything else, like a `cargo install`, is left alone.

use std::path::PathBuf;
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use axoupdater::{AxoUpdater, Version};
use serde::{Deserialize, Serialize};

/// How often [`App::daily_notice`] asks for the newest release
const CHECK_EVERY: Duration = Duration::from_secs(24 * 60 * 60);
/// How long [`App::daily_notice`] waits for an answer
const CHECK_TIMEOUT: Duration = Duration::from_secs(3);
/// Set to anything to never check
const OPT_OUT: &str = "SMC_NO_UPDATE_CHECK";

/// A running SMC tool: its release name and its own version
#[derive(Debug, Clone, Copy)]
pub struct App {
    pub name: &'static str,
    pub version: &'static str,
}

impl App {
    /// The installed copy's updater, if this is it
    fn updater(self) -> Option<AxoUpdater> {
        let mut updater = AxoUpdater::new_for(self.name);
        updater.load_receipt().ok()?;
        if !updater.check_receipt_is_for_this_executable().ok()? {
            return None;
        }
        // The running build knows its version better than a receipt left by an older install
        updater
            .set_current_version(self.version.parse().ok()?)
            .ok()?;
        Some(updater)
    }

    /// The newest release's version, when it's newer than this one and this copy can update
    pub async fn check(self) -> Option<String> {
        let mut updater = self.updater()?;
        if !updater.is_update_needed().await.ok()? {
            return None;
        }
        let version = updater.query_new_version().await.ok()??;
        Some(version.to_string())
    }

    /// Installs the newest release over this copy, returning its version. It takes effect the
    /// next time the tool starts.
    pub async fn install(self) -> Result<String, String> {
        let mut updater = self
            .updater()
            .ok_or("this copy wasn't installed by the release installer")?;
        updater.disable_installer_output();
        match updater.run().await {
            Ok(Some(result)) => Ok(result.new_version.to_string()),
            Ok(None) => Err("already up to date".to_string()),
            Err(error) => Err(error.to_string()),
        }
    }

    /// [`App::install`], for tools that aren't async
    pub fn install_blocking(self) -> Result<String, String> {
        block_on(self.install())?
    }

    /// The newest release's version, if it's newer, for command line tools that shouldn't
    /// wait on the network every run. It asks at most once a day, remembering the answer,
    /// and gives up after a few seconds.
    pub fn daily_notice(self) -> Option<String> {
        if std::env::var_os(OPT_OUT).is_some() || self.updater().is_none() {
            return None;
        }

        let path = self.cache_path()?;
        let now = SystemTime::now().duration_since(UNIX_EPOCH).ok()?.as_secs();
        let cached = std::fs::read_to_string(&path)
            .ok()
            .and_then(|json| serde_json::from_str::<LastCheck>(&json).ok())
            .filter(|last| now.saturating_sub(last.checked_at) < CHECK_EVERY.as_secs());

        let latest = match cached {
            Some(last) => last.latest,
            None => {
                let latest = self.check_with_timeout();
                let last = LastCheck {
                    checked_at: now,
                    latest: latest.clone(),
                };
                if let Ok(json) = serde_json::to_string(&last) {
                    let _ = std::fs::write(&path, json);
                }
                latest
            }
        };

        // A remembered release may be the one already running by now
        let newer = |latest: &String| {
            matches!(
                (latest.parse::<Version>(), self.version.parse::<Version>()),
                (Ok(latest), Ok(current)) if latest > current
            )
        };
        latest.filter(newer)
    }

    fn check_with_timeout(self) -> Option<String> {
        let (sender, receiver) = mpsc::channel();
        thread::spawn(move || {
            let _ = sender.send(block_on(self.check()).ok().flatten());
        });
        receiver.recv_timeout(CHECK_TIMEOUT).ok().flatten()
    }

    fn cache_path(self) -> Option<PathBuf> {
        let dir = dirs::config_dir()?.join(self.name);
        std::fs::create_dir_all(&dir).ok()?;
        Some(dir.join("update-check.json"))
    }
}

/// When [`App::daily_notice`] last asked, and what it heard
#[derive(Serialize, Deserialize)]
struct LastCheck {
    checked_at: u64,
    latest: Option<String>,
}

fn block_on<F: Future>(future: F) -> Result<F::Output, String> {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|error| error.to_string())?;
    Ok(runtime.block_on(future))
}
