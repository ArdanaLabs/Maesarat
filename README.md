<div align="center">

# Maesarat

</div>

# Usage

Run `nix flake check` To check the functionality of `cardano-node`. Nothing
impressive yet. Be prepared to compile everything. Eventually a cache will be
setup which can be used.

# Haskell

## Prerequisites

* `cardano-node`
* `cardano-wallet`

## Building

Enter a shell via `nix develop`. From there you can `cd haskell` and `cabal build`.

## Running

### Testnet

A cardano testnet must be running. This is most easily accomplished by running the `start-cluster` script after entering `nix-shell` in the `cardano-node` repo. The `Maesarat` executable verifies the testnet is running by the following criteria:

* Node socket file is available at `<testnet-dir>/node-0/node.socket`.
* EKG is available at `127.0.0.1:30100`.
* Prometheus is available at `127.0.0.1:30200/metrics`.

If _any_ of these criteria fail, then you will see a relevant error upon running the application.

### Maesarat

`maesarat` has the following usage:

```
Usage: maesarat (-t|--testnet-path PATH) [--wallet-exe PATH] [--wallet-host IP]
                [--wallet-port PORT] [--timeout NUM]

Available options:
  -t,--testnet-path PATH   Path to testnet directory, e.g.,
                           cardano-node/run/current
  --wallet-exe PATH        The path to the cardano-wallet exe. Defaults to
                           `cardano-wallet`
  --wallet-host IP         The IP address for the cardano-wallet service.
                           Defaults to 127.0.0.1
  --wallet-port PORT       The port for the cardano-wallet service. Defaults to
                           8090.
  --timeout NUM            The amount of time we wait for cardano-wallet to
                           become responsive, in seconds. Defaults to 30.
  -h,--help                Show this help text
```

The only mandatory argument is the `--testnet-path`. This is dependent on where you are running the testnet. If you are running from a checked out version of the `cardano-node` repository (i.e. after entering a `nix-shell`), then this may look like `.../path/to/cardano-node/run/current`.

Futhermore, you will have to provide the path to the `cardano-wallet` executable if it is not on the `PATH`. Running `maesarat` thus may look something like:

```
cabal run maesarat -- -t /path/to/cardano-node/run/current --wallet-exe ~/path/to/cardano-wallet
```

For now, we assume the genesis file necessary to run `cardano-wallet` can be found at `<testnet-dir>/genesis/byron/genesis.json`.

Right now, running the application will:

* Verify the testnet is up.
* Start `cardano-wallet` and verify it is responsive.
* Terminate `cardano-wallet` and exit.

## Development

This nix shell provided by `nix develop` gives several tools:

* `haskell-language-server`
* `hlint`
* `ghcid`
* `ormolu`

Additionally, helper scripts for formatting are located in the `scripts` folder.

### Documentation

A hoogle database can be generated with `cabal build --haddock-hoogle`. From there it can be served with `hoogle server --local`.

To see haddock for `maesarat` specifically, `script/gen_haddock` will generate docs and copy them into the `haskell/local-haddock` directory. These can then be viewed in the browser, e.g. `firefox haskell/local-haddock/index.html`.