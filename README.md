# haskell-security-action

Scan for vulnerabilities and create advisories on GitHub.

## Usage

```yaml
name: security scan

name: Scan for vulnerabilities
on:
  # Scan for each push event on your protected branch
  # push:
    # branches: [ "main" ]
  # Scan for pull requests
  # pull_request:
    # branches: [ "main" ]
  # Schedule a daily scan at midnight
  schedule:
    - cron: '0 0 * * *'
  workflow_dispatch:

jobs:
  haskell-security:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Run Haskell Security Action
        uses: blackheaven/haskell-security-action
        with:
          token: ${{ secrets.GITHUB_TOKEN }}
```
