# sun-burst

## Develop
ghcid -c="stack ghci sun-burst:lib sun-burst:test:sun-burst-test" -T=main
ghcid -c="stack ghci sun-burst:lib sun-burst:sun-burst-exe" -r="main"
