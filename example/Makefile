.PHONY: nix-backend
nix-backend:
	nix-build -o ~/dist/example/backend-result -A ghc.backend

.PHONY: nix-frontend-ghcjs
nix-frontend-ghcjs:
	nix-build -o ~/dist/example/frontend-result -A ghcjs.frontend

.PHONY: cabal-ghc
cabal-ghc:
	nix-shell -A shells.ghc --run "cabal new-build all"

.PHONY: cabal-ghcjs
cabal-ghcjs:
	nix-shell -A shells.ghcjs --run "cabal --project-file=cabal.ghcjs.project --builddir=~/dist/example/dist-ghcjs new-build all"

.PHONY: warp-frontend
warp-frontend:
	nix-build --arg warp true -o ~/dist/example/ghc-frontend-result -A ghc.frontend

.PHONY: nix-frontend
nix-frontend:
	nix-build -o ~/dist/example/ghc-frontend-result -A ghc.frontend

.PHONY: cabal-frontend
cabal-frontend:
	nix-shell -A shells.ghc --run "cabal new-build frontend"
