# haskell-security-action

Scan for vulnerabilities and create advisories on GitHub.

## Usage

```yaml
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

      - uses: haskell-actions/setup@v2 # GHC is neneded in $PATH
        with:
          ghc-version: '9.8'

      - name: Log in to the Container registry
        uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Run Haskell Security Action
        uses: blackheaven/haskell-security-action@master
        with:
          token: ${{ secrets.GITHUB_TOKEN }}
```
