# GitHub Workflows

This directory contains automated workflows for the OpenHPC project.

## Package Update Checker

**File:** `package-update-checker.yml`

This workflow automatically monitors OpenHPC packages for newer releases
available on GitHub and manages GitHub issues to track update status.

### How it works

1. **Schedule**: Manual triggering only (hourly schedule disabled for now)
2. **Container**: Uses Red Hat UBI 10 container
   (`registry.access.redhat.com/ubi10:latest`)
3. **Dependencies**: Installs required tools
   (rpm-build, rpmdevtools, curl, jq, git)
4. **RPM Environment**: Sets up RPM build directories for proper spec file parsing
5. **Package Check**: Executes `misc/check_for_package_updates.sh` with
   markdown output
6. **Issue Management**: Creates, updates, or closes GitHub issues based on results

### Issue Management Logic

- **Exit Code 0 (No Updates)**:
  - If an open "Package Upgrades Necessary" issue exists → Close it
  - If no open issue exists → No action needed

- **Exit Code 1 (Updates Available)**:
  - If an open "Package Upgrades Necessary" issue exists → Update it with
    latest info
  - If no open issue exists → Create a new one
  - If a closed issue exists → Create a new issue (don't reopen the old one)

### Issue Format

Issues are created with:

- **Title**: `📦 Package Upgrades Necessary - YYYY-MM-DD`
- **Labels**: `package-updates`, `automation`
- **Body**: Contains the markdown output from the package checker script
- **Status**: Shows last check time and current status

### Manual Execution

The workflow can be triggered manually from the GitHub Actions tab using the
"workflow_dispatch" trigger.

**Note**: The hourly schedule is currently disabled. To re-enable automatic
hourly runs, uncomment the `schedule` section in the workflow file:

```yaml
on:
  schedule:
    # Run every hour at minute 0
    - cron: '0 * * * *'
  workflow_dispatch: # Allow manual triggering
```

### Requirements

- The `misc/check_for_package_updates.sh` script must be executable
- The workflow requires `issues: write` permission
- GitHub token is automatically provided via `secrets.GITHUB_TOKEN`
- Uses Red Hat UBI 10 container for consistent RPM tooling environment

### Configuration

The workflow is configured to:

- Run on Ubuntu latest with Red Hat UBI 10 container
- Use the repository's default GitHub token
- Search for issues with the `package-updates` label
- Handle multiple edge cases and error conditions
- Provide native RPM environment for optimal OpenHPC spec file processing

This automation ensures that package updates are tracked consistently and the
development team is notified when newer versions of dependencies become
available.
