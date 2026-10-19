#!/usr/bin/env bash
# Puts the emulator's install instructions at the top of a release's notes, since dist only
# writes notes for the apps it builds. Usage: velopack-notes.sh <tag>
set -euo pipefail

tag="$1"
version="${tag#v}"
repo="${GITHUB_REPOSITORY:-BjornTheProgrammer/smc-language}"
base="https://github.com/$repo/releases/download/$tag"

current="$(gh release view "$tag" --repo "$repo" --json body --jq .body)"
if grep -q "^# smc-emu " <<<"$current"; then
  echo "The notes already cover smc-emu"
  exit 0
fi

notes="$(mktemp)"
cat >"$notes" <<NOTES
# smc-emu $version

## Install smc-emu $version

| Platform | Installer |
|----------|-----------|
| Windows | [smc-emu-win-Setup.exe]($base/smc-emu-win-Setup.exe) |
| macOS (Apple Silicon and Intel) | [smc-emu-osx-Setup.pkg]($base/smc-emu-osx-Setup.pkg) |
| Linux x64 | [smc-emu.AppImage]($base/smc-emu.AppImage) |
| Linux ARM64 | [smc-emu-linux-arm64.AppImage]($base/smc-emu-linux-arm64.AppImage) |

To run it without installing, use [smc-emu-win-Portable.zip]($base/smc-emu-win-Portable.zip) or [smc-emu-osx-Portable.zip]($base/smc-emu-osx-Portable.zip). On Linux, mark the AppImage executable (\`chmod +x\`) and run it.

The macOS installer isn't signed, so macOS refuses to open it at first. Right-click it, choose **Open**, and confirm.

Installed copies update themselves: click **UPDATE** in the status bar when a newer release is out. The \`.nupkg\` and \`releases.*.json\` files are what the updater downloads.

NOTES
printf '%s\n' "$current" >>"$notes"
gh release edit "$tag" --repo "$repo" --notes-file "$notes"
