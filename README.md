# DFCT - Decentralized Fact-Checking Toolkit

DFCT (Decentralized Fact-Checking Toolkit) is a platform that redefines fact-checking by structuring and supporting collaborative verification processes. The platform leverages the Cardano ecosystem to promote transparency, scalability, and active community participation, incentivizing contributions with the DFCT Cardano Native Token (CNT).

This project contains the following main components:
1. Plutus Smart Contract for the Cardano blockchain
2. Python FastAPI Backend


## Prerequisites

- [Docker](https://www.docker.com/) and Docker Compose (28.0.4)
- [GHC](https://www.haskell.org/ghc/) (9.6.4)
- [Cabal](https://www.haskell.org/cabal/) (3.10.3.0) for Plutus development
- [Python](https://www.python.org/) (3.11.6)
- [Poetry](https://python-poetry.org/) (2.1.1)

## Environment Setup

### Setting Up Ubuntu/Debian

1. Update your system:
   ```bash
   sudo apt update && sudo apt upgrade -y
   ```

2. Install basic development tools:
   ```bash
   sudo apt install -y git build-essential curl wget libgmp-dev libssl-dev libncurses-dev
   ```

3. Install Docker and Docker Compose:
   ```bash
   # Install Docker
   sudo apt install -y docker.io docker-compose
   sudo usermod -aG docker $USER
   # Log out and log back in
   ```

4. Install Haskell and Cabal (for Plutus development):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   source ~/.ghcup/env
   ghcup install ghc 9.6.4
   ghcup set ghc 9.6.4
   ghcup install cabal 3.10.3.0
   ghcup set cabal 3.10.3.0
   ```

5. Install Python 3.11 and Poetry:
   ```bash
   sudo apt install -y python3.9 python3.9-dev python3-pip
   curl -sSL https://install.python-poetry.org | python3 -
   echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
   source ~/.bashrc
   ```

### Setting Up macOS

1. Install Homebrew (if not already installed):
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```

2. Install dependencies:
   ```bash
   brew install git curl wget gmp openssl
   ```

3. Install Docker Desktop:
   ```bash
   brew install --cask docker
   ```
   After installation, launch Docker Desktop from your Applications folder.

4. Install Haskell and Cabal (for Plutus development):
   ```bash
   # Install GHCup (Haskell toolchain installer)
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   
   # Update your PATH
   source ~/.ghcup/env

   # Install GHC and Cabal
   ghcup install ghc 9.6.4
   ghcup set ghc 9.6.4
   ghcup install cabal 3.10.3.0
   ghcup set cabal 3.10.3.0

   # Verify installations
   ghc --version
   cabal --version
   ```

5. Install Python 3.11 and Poetry:
   ```bash
   brew install python@3.11
   
   # Install Poetry
   curl -sSL https://install.python-poetry.org | python3 -
   
   # Add Poetry to your PATH
   echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
   source ~/.zshrc
   ```


### Setting Up Windows

> **DISCLAIMER:** While DFCT may work on WSL2, it is **not officially supported**. Some features, especially those relying on networking and Docker, may require additional configuration or may not work as expected. Use at your own discretion.

1. Install WSL2 (Windows Subsystem for Linux):
   - Open PowerShell as Administrator and run:
     ```powershell
     wsl --install
     ```
   - Restart your computer
   - After restart, a new terminal will open to set up your Linux username and password
   - Once that's done, you can follow the Ubuntu/Debian instructions above

2. Alternative - Install dependencies directly on Windows:
   - Install [Git for Windows](https://git-scm.com/download/win)
   - Install [Docker Desktop for Windows](https://www.docker.com/products/docker-desktop)
   - Install [GHCup](https://www.haskell.org/ghcup/) for Haskell toolchain management
   - Install GHC 9.6.4 and Cabal 3.10.3.0 using GHCup
   - Install [Python 3.11](https://www.python.org/downloads/windows/)
   - Install Poetry:
     ```powershell
     (Invoke-WebRequest -Uri https://install.python-poetry.org -UseBasicParsing).Content | python -
     ```

## DFCT Project

### Deploying dfct smart contract on testnet

1. Create a directory for your Cardano output files (keys, contracts, etc.):

```bash
#on project root
mkdir -p backend/assets
```

2. Run a cardano-node on testnet

```bash
#run script to fetch necessary cardano-node config files
#if necessary, make the script executable: chmod +x generate-keys.sh
./fetch_config_files.sh

#run
#WARNING: remove --platform linux/amd64 if you are *NOT* using and amd64 platform (e.g. Macbook with M1, M2, M3, M4 chips)
docker run --platform linux/amd64 -d --name cardano-node-preview \
  -v $HOME/cardano/preview-config:/config \
  -v $(pwd)/backend/assets:/assets \
  -v $HOME/cardano/db:/data \
  -p 3001:3001 \
  ghcr.io/intersectmbo/cardano-node:10.4.0 \
  run \
  --config /config/config.json \
  --topology /config/topology.json \
  --database-path /data \
  --socket-path /data/node.socket \
  --host-addr 0.0.0.0 \
  --port 3001


#OR check if it is running if you had it before
docker ps --filter "name=cardano-node-preview"
```

3. Use the cardano node container to generate keys:

```bash
#Go to smart-contract folder
cd smart-contract
```

```bash
#call script to automatically generate necessary keys
#if necessary, make the script executable: chmod +x generate-keys.sh
./generate-keys.sh



#ALTERNATIVELLY, you can generate the key manually by executing the following:



#contract owner and minting authority
#verification and sigining keys
docker exec -it cardano-node-preview cardano-cli address key-gen --verification-key-file /assets/owner.vkey --signing-key-file /assets/owner.skey

#owner address
docker exec -it cardano-node-preview cardano-cli address build --payment-verification-key-file /assets/owner.vkey --out-file /assets/owner.addr --testnet-magic 2

#owner key hash
docker exec -it cardano-node-preview cardano-cli address key-hash --payment-verification-key-file /assets/owner.vkey --out-file /assets/owner.pkh

#key hash for proposer
docker exec -it cardano-node-preview cardano-cli address key-gen --verification-key-file /assets/proposer.vkey --signing-key-file /assets/proposer.skey
docker exec -it cardano-node-preview cardano-cli address key-hash --payment-verification-key-file /assets/proposer.vkey --out-file /assets/proposer.pkh

#proposer address
docker exec -it cardano-node-preview cardano-cli address build --payment-verification-key-file /assets/proposer.vkey --out-file /assets/proposer.addr --testnet-magic 2

#key hash for reviewer1
docker exec -it cardano-node-preview cardano-cli address key-gen --verification-key-file /assets/reviewer1.vkey --signing-key-file /assets/reviewer1.skey
docker exec -it cardano-node-preview cardano-cli address key-hash --payment-verification-key-file /assets/reviewer1.vkey --out-file /assets/reviewer1.pkh

#reviewer1 address
docker exec -it cardano-node-preview cardano-cli address build --payment-verification-key-file /assets/reviewer1.vkey --out-file /assets/reviewer1.addr --testnet-magic 2

#key hash for reviewer2
docker exec -it cardano-node-preview cardano-cli address key-gen --verification-key-file /assets/reviewer2.vkey --signing-key-file /assets/reviewer2.skey
docker exec -it cardano-node-preview cardano-cli address key-hash --payment-verification-key-file /assets/reviewer2.vkey --out-file /assets/reviewer2.pkh

#reviewer2 address
docker exec -it cardano-node-preview cardano-cli address build --payment-verification-key-file /assets/reviewer2.vkey --out-file /assets/reviewer2.addr --testnet-magic 2
```

Now fund owner with test ADA using cardano faucet
https://docs.cardano.org/cardano-testnets/tools/faucet
```bash
cat ../backend/assets/owner.addr
```

4. Build and create minting policy id:

**ATTENTION**: Update app/mint/CompileMP.hs with owner.pkh so the owner becomes the DFC token minting authority

```bash
#still in the dfct-cardano/smart-contract folder
#update cabal dependencies
cabal update

#build the minting smart contract
cabal build all

#run the minting executable to generate the minting plutus scripts
cabal run dfct-mint

#move dfct-minting-policy.plutus to the assets folder
mv dfct-minting-policy.plutus ../backend/assets

# create DFCT Minting Policy ID
docker exec -it cardano-node-preview cardano-cli latest transaction policyid --script-file /assets/dfct-minting-policy.plutus > ../backend/assets/dfct-minting-policy.id
```

5. Create provenance and governance plutus files:

**ATTENTION**: Update the dfcSymbol of app/provenance/CompilePV.hs with the dfct-minting-policy.id value

```bash
#build the provenance validator smart contract
cabal run dfct-provenance

#move validator plutus to the assets folder
mv dfct-provenance.plutus ../backend/assets

#calculate the provenance validator script testnet address:
docker exec -it cardano-node-preview cardano-cli address build --payment-script-file /assets/dfct-provenance.plutus --testnet-magic 2 --out-file /assets/dfct-provenance.addr
```

```bash
#build the governance validator smart contract
cabal run dfct-governance

#move validator plutus to the assets folder
mv dfct-governance.plutus ../backend/assets

#calculate the governance validator script testnet address:
docker exec -it cardano-node-preview cardano-cli address build --payment-script-file /assets/dfct-governance.plutus --testnet-magic 2 --out-file /assets/dfct-governance.addr
```


6. Deploy on testnet

```bash
#first, wait until cardano-node is synced (check progress and block number: https://preview.cardanoscan.io/)
docker exec -it cardano-node-preview cardano-cli query tip --testnet-magic 2 --socket-path /data/node.socket

#create protocol.json
docker exec -it cardano-node-preview cardano-cli query protocol-parameters \
  --testnet-magic 2 \
  --socket-path /data/node.socket \
  --out-file /assets/protocol.json

#check if owner.addr has funds and get the UTxOs at the owner’s address
docker exec -it cardano-node-preview cardano-cli query utxo \
  --address $(cat ../backend/assets/owner.addr) \
  --testnet-magic 2 \
  --socket-path /data/node.socket

#create a utxo to use as collateral if you do not have one yet"
docker exec -it cardano-node-preview cardano-cli latest transaction build \
   --testnet-magic 2 \
   --socket-path /data/node.socket \
   --change-address $(cat ../backend/assets/owner.addr) \
   --tx-in <replace-with-existing-utxo-from-faucet> \
   --tx-out "$(cat ../backend/assets/owner.addr) + 5000000 lovelace" \
   --out-file /assets/tx-collateral.body

docker exec -it cardano-node-preview cardano-cli latest transaction sign \
   --tx-body-file /assets/tx-collateral.body \
   --signing-key-file "/assets/owner.skey" \
   --out-file /assets/create-collateral.signed \
   --testnet-magic 2

docker exec -it cardano-node-preview cardano-cli latest transaction submit \
   --tx-file /assets/create-collateral.signed \
   --testnet-magic 2 \
   --socket-path /data/node.socket

#execute the generate-deploy-files.sh
#if necessary, make the script executable: chmod +x generate-deploy-files.sh
./generate-deploy-files.sh

#execute generated ref script to deploy DFC minting policy as a reference script utxo
./deploy-dfct-mp.sh

#check if collateral and reference appears as utxos in owner's account
docker exec -it cardano-node-preview cardano-cli query utxo \
  --address $(cat ../backend/assets/owner.addr) \
  --testnet-magic 2 \
  --socket-path /data/node.socket

#execute the generated script to mint 1000000 dfc
./mint-dfc.sh ../backend/assets/owner.addr

# wait for the transaction to be confirmed and 
# visit cardanoscan to see DFC tokens in the owner account
# https://preview.cardanoscan.io/address/<<onwer.addr>>>>

#send topic datum, ADA and DFC tokens from proposer to provenance validator script created at
#fund proposer (faucet) and create collateral utxo before execution this example.
./submit-topic-utxo.sh

#submit topic using utxo created at ./submit-topic-utxo.sh
./submit-topic.sh
```

## Testing

### Smart contract tests

#### Testing locally:

```bash
#still in the smart-contract folder call cabal test:
cabal test --test-show-details=streaming --test-option=--color=always
```

## Backend

### Features

- Submit new fact-checking topics to the blockchain
- Review and approve/reject topics by designated reviewers
- Wallet management for different user roles (proposer, reviewer)
- Transaction building and submission to Cardano blockchain
- Health check and configuration endpoints

The system expects account files in the specified assets directory:
- `owner.addr`, `owner.vkey`, `owner.skey`, `owner.pkh`
- `proposer.addr`, `proposer.vkey`, `proposer.skey`, `proposer.pkh`
- `reviewer1.addr`, `reviewer1.vkey`, `reviewer1.skey`, `reviewer1.pkh`
- `reviewer2.addr`, `reviewer2.vkey`, `reviewer2.skey`, `reviewer2.pkh`

The system requires the following contract files in the assets directory:
- `dfct-provenance.plutus`: The main validator script
- `dfct-minting-policy.plutus`: The token minting policy script
- `dfct-provenance.addr`: The validator script address
- `dfct-minting-policy.id`: The policy ID for the DFC tokens

You can generate these files using the scripts described in the smart contract section.

Make sure to fund owner, proposer, reviewer1 and reviewer2 (addr) using cardano faucet.

### Install, config and run the backend

Requirement:

Ogmios docker
```bash
docker run --platform linux/amd64 -d --name ogmios \
  -v $HOME/cardano/preview-config:/config \
  -v $HOME/cardano/db:/data \
  -v ./ipc:/ipc \
  -p 1337:1337 \
  cardanosolutions/ogmios:latest \
  --node-config /config/config.json \
  --node-socket /data/node.socket \
  --host 0.0.0.0


#OR check if it is running if you had it before
docker ps --filter "name=ogmios"
```

1. Create and set a virtualenv and install dependencies with Poetry:

```bash
#from smart-contract folder to backend
cd ../backend

#create virtualenv
virtualenv venv

#use it
source venv/bin/activate

#install dependencies
poetry install
```

2. Start the server:

```bash
uvicorn dfctbackend.main:app --host 0.0.0.0 --port 8000 --reload
```

### API Endpoints

#### Health and Configuration
- `GET /health`: Health check endpoint
- `GET /config`: Get system configuration

#### Topics
- `POST /api/v1/topic`: Submit a new topic
- `POST /api/v1/topic/review`: Review a topic (using reviewer1 wallet)
- `POST /api/v1/topic/{wallet_name}/review`: Review a topic with a specific wallet
- `POST /api/v1/topic/activate`: Activate a reviewed topic (not implemented yet)

#### Contributions
- `POST /api/v1/contribution`: Submit a contribution (not implemented yet)
- `POST /api/v1/contribution/review`: Review a contribution (not implemented yet)

#### Transactions
- `GET /api/v1/status/{tx_hash}`: Check transaction status (not implemented yet)

### API Documentation

The API documentation is available at `/docs` or `/redoc` when the server is running.

Before using the API, make sure to fund the owner, proposer, reviewer1, and reviewer2 addr using cardano preview faucet
https://docs.cardano.org/cardano-testnets/tools/faucet

Mint dfc tokens to these accounts using the mint-dfc script:
```bash
cd ../smart-contract
./mint-dfc.sh ../backend/assets/owner.addr
./mint-dfc.sh ../backend/assets/proposer.addr
./mint-dfc.sh ../backend/assets/reviewer1.addr
./mint-dfc.sh ../backend/assets/reviewer2.addr
cd -
```

### Testing
The project includes a comprehensive test suite covering all major components. The tests use pytest and are organized by module.
To run the full test suite:
```bash
poetry run pytest
```

To run specific test categories:
```bash
# Run only unit tests
poetry run pytest -m unit

# Run tests for a specific module
poetry run pytest tests/test_governance.py
```

Integration test using provenance on testnet
```bash
# ATTENTION:
# Owner, proposer, reviewer1, and reviewer2 must have:
# funds from faucet
# DFC tokens from mint script

# to run the provenance integration test
poetry run python integration_test_provenance.py

# to run the governance integration test
poetry run python integration_test_governance.py
```

## Running with Docker Compose

It is also possible to launch a full test environment with a docker-based setup using `docker-compose`. This includes:

* Cardano preview node
* Ogmios (for chain indexing and querying)
* Python FastAPI provenance backend

### Prerequisites

* [Docker](https://www.docker.com/)
* [Docker Compose](https://docs.docker.com/compose/)
* Required config files: `config.json`, `topology.json` in `cardano/preview-config`
  *(fetch with `./fetch_config_files.sh` if not already present)*

### Directory Structure Required

Ensure the following folders exist before running:

```bash
mkdir -p backend/assets cardano/preview-config cardano/db ipc
```

Also, populate `cardano/preview-config` using:

```bash
./fetch_config_files.sh
```

### Launch Services

```bash
docker-compose up --build
```

This will:

* Run the Cardano node on `localhost:3001`
* Start Ogmios on `localhost:1337`
* Launch the DFCT backend on `localhost:8000`

### Rebuilding

If you make changes to the backend code or configuration:

```bash
docker-compose up --build --force-recreate
```

### Stopping and Cleaning Up

```bash
docker-compose down
```

This will stop and remove all services but leave volumes intact.

## Project Structure

```
dfct/
│
├── smart-contract/        # Plutus smart contracts
│   ├── app/               # Plutus generators
│   ├── src/               # Smart contract source code
│   └── test/              # Smart contract tests
└── backend/               # Python FastAPI backend
    ├── dfctbackend/       # Backend source code
    ├── tests/             # Backend tests
    └── assets/            # Compiled Plutus scripts, keys, json

```


## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
=======
# d-FCT on Cardano
d-FCT (Decentralized Fact-Checking Toolkit), pronounced _de facto_, is a platform aiming at redefining fact-checking by structuring and supporting collaborative verification processes. The platform will leverage the Cardano ecosystem to promote transparency, scalability, and active community participation, while incentivizing contributions with its own Cardano Native Token (CNT). d-FCT’s goal is to build a high-trust environment for addressing the search for structured evidence, by combining AI processing and collective validation with tokenized rewards.

