# staking-pool-sc

## Introduction

This staking pool contract allows users to stake *MyToken* tokens and claim rewards
as time goes by. The pool has two APR levels depending on the duration of the deposits.
Users get paid a level 1 APR of 15% for deposits with a duration of less
than 90 days, and a level 2 APR of 20% for older deposits.

Pools can be launched with an initial supply of rewards. Users can then register to the
pool and make deposits and withdraws. These operations pay fees in *MyToken* that can
be configured in the inital settings of the pool. The fees are distributed among three
wallets, also configurable, that can serve different purposes to the pool operator.

After some time, users can claim their rewards and retrieve them, or compound them to
automatically stake back the rewards.

### Modules Design

The staking pool contract includes:
* Haskell off-chain code providing functions for each staking action.
* Haskell on-chain code for validating each transaction.
* Business logic code with abstract data types for the contract state
and functions to operate with them.
* Traces running on the EmulatorTrace for testing off-chain code, and test cases
for each one of these traces.
* Malicious off-chain code and traces running on the EmulatorTrace for testing
the on-chain code, and test cases for each one of these traces.

## How to build

Here you can find information about how to build this project and run the on-chain
and off-chain tests.

#### Plutus dependencies

Install the development environment following the official [documentation](https://github.com/input-output-hk/plutus/tree/36e2c8bdbb6e70d25a31331e5cd23f26dc3162d5#how-to-build-the-projects-artifacts).
Once you have all installed, you will start the environment with `nix-shell`
from the plutus repository folder and change into the project folder.

#### Compile

```
$> cabal build
```

#### Run tests

```
$> cabal run tests
```

#### More

You also can load the library or the tests with the repl with `cabal repl` or
`cabal repl test`.

## Supported Actions

### Admin actions

#### Start

Initializes the smart contract and creates the main staking pool UTxO with an
initial supply of *MyToken* rewards tokens.

#### Feed

Given an amount of *MyToken* tokens, adds them to the rewards pool.

### User actions

#### Register

Registers the user into the staking pool, creating a pool user UTxO with no deposits in it.
The public key hash of the user is stored in the main staking pool UTxO.

#### Unregister

Unregisters the user from the staking pool. The pool user UTxO is removed.

#### Deposit

Given an amount of *MyToken* tokens, makes a new deposit to the user current staking.
All deposits are stored with a timestamp to be able to keep their duration and APR level.

#### Withdraw

Given an amount of *MyToken* tokens, withdraws that from the user current staking.
Withdraws are always applied to the most recent deposits so older deposits can keep their APR levels.

#### Claim

The user claims all the earned rewards from the last claim until now.

#### Compound

The user claims all the earned rewards and automatically stakes them back instead of
collecting them.


## Relevant Issues

1. This contract is implemented and tested using the Plutus simulated blockchain.
It must still be adapted to work with the recently published PAB and a testnet.

2. The claim and compound operations are subject to congestion as they must both consume
the main pool UTxO. Only one user per block can perform one of these operations.
Fortunately, the pool UTxO is only consumed to access rewards, so the congestion can
be solved by producing several UTxOs with rewards.

3. When a user registers, an NFT is minted to identify his pool user UTxO.
When they unregister the NFT is not burned but paid to the user wallet.
The NFT should be burned or, even better, subscription tokens could be used
instead of NFTs. These are not burned on unregistration but paid back to the
main pool UTxO to be used for future registrations.

4. The APR levels are currently not configurable at pool launch in the contract settings.
They are instead hardcoded into the business logic, so the code should be edited and
recompiled to modify them. Fortunately, the code is modularized so only a couple of lines
must be changed. The APR configuration could be moved to the settings, at the cost of a more
expensive and complex on-chain validation.
