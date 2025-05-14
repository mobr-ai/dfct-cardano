#!/bin/bash

# Script to generate all necessary keys for DFCT contract
# The script will create keys for owner, proposer, and reviewers
# This script should be executed from the smart-contract directory

# Define colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
WHITE='\033[0m'

# Define the assets directory
ASSETS_DIR="$(pwd)/../backend/assets"

# Create the assets directory if it doesn't exist
mkdir -p $ASSETS_DIR

echo -e "${YELLOW}Generating keys for DFCT contract...${WHITE}"

# Check if Docker is running
if ! docker ps | grep -q cardano-node-preview; then
  echo -e "${RED}Error: cardano-node-preview container is not running!${WHITE}"
  echo "Please start the cardano-node-preview container first."
  exit 1
fi

# Function to check if a file exists
check_file_exists() {
  if [ -f "$ASSETS_DIR/$1" ]; then
    return 1
  fi
  return 0
}

# Generate owner keys
echo -e "${BLUE}Checking and generating owner keys...${WHITE}"
if check_file_exists "owner.vkey" && check_file_exists "owner.skey"; then
  docker exec -it cardano-node-preview cardano-cli address key-gen \
    --verification-key-file /assets/owner.vkey \
    --signing-key-file /assets/owner.skey
  echo -e "${GREEN}Owner keys generated.${WHITE}"
fi

# Generate owner address
echo -e "${BLUE}Checking and generating owner address...${WHITE}"
if check_file_exists "owner.addr"; then
  docker exec -it cardano-node-preview cardano-cli address build \
    --payment-verification-key-file /assets/owner.vkey \
    --out-file /assets/owner.addr \
    --testnet-magic 2
  echo -e "${GREEN}Owner address generated.${WHITE}"
fi

# Generate owner key hash
echo -e "${BLUE}Checking and generating owner key hash...${WHITE}"
if check_file_exists "owner.pkh"; then
  docker exec -it cardano-node-preview cardano-cli address key-hash \
    --payment-verification-key-file /assets/owner.vkey \
    --out-file /assets/owner.pkh
  echo -e "${GREEN}Owner key hash generated.${WHITE}"
fi

# Generate proposer keys
echo -e "${BLUE}Checking and generating proposer keys...${WHITE}"
if check_file_exists "proposer.vkey" && check_file_exists "proposer.skey"; then
  docker exec -it cardano-node-preview cardano-cli address key-gen \
    --verification-key-file /assets/proposer.vkey \
    --signing-key-file /assets/proposer.skey
  echo -e "${GREEN}Proposer keys generated.${WHITE}"
fi

# Generate proposer key hash
echo -e "${BLUE}Checking and generating proposer key hash...${WHITE}"
if check_file_exists "proposer.pkh"; then
  docker exec -it cardano-node-preview cardano-cli address key-hash \
    --payment-verification-key-file /assets/proposer.vkey \
    --out-file /assets/proposer.pkh
  echo -e "${GREEN}Proposer key hash generated.${WHITE}"
fi

# Generate proposer address for funding
echo -e "${BLUE}Checking and generating proposer address...${WHITE}"
if check_file_exists "proposer.addr"; then
  docker exec -it cardano-node-preview cardano-cli address build \
    --payment-verification-key-file /assets/proposer.vkey \
    --out-file /assets/proposer.addr \
    --testnet-magic 2
  echo -e "${GREEN}Proposer address generated.${WHITE}"
fi

# Generate reviewer1 keys
echo -e "${BLUE}Checking and generating reviewer1 keys...${WHITE}"
if check_file_exists "reviewer1.vkey" && check_file_exists "reviewer1.skey"; then
  docker exec -it cardano-node-preview cardano-cli address key-gen \
    --verification-key-file /assets/reviewer1.vkey \
    --signing-key-file /assets/reviewer1.skey
  echo -e "${GREEN}Reviewer1 keys generated.${WHITE}"
fi

# Generate reviewer1 key hash
echo -e "${BLUE}Checking and generating reviewer1 key hash...${WHITE}"
if check_file_exists "reviewer1.pkh"; then
  docker exec -it cardano-node-preview cardano-cli address key-hash \
    --payment-verification-key-file /assets/reviewer1.vkey \
    --out-file /assets/reviewer1.pkh
  echo -e "${GREEN}Reviewer1 key hash generated.${WHITE}"
fi

# Generate reviewer1 address for funding
echo -e "${BLUE}Checking and generating reviewer1 address...${WHITE}"
if check_file_exists "reviewer1.addr"; then
  docker exec -it cardano-node-preview cardano-cli address build \
    --payment-verification-key-file /assets/reviewer1.vkey \
    --out-file /assets/reviewer1.addr \
    --testnet-magic 2
  echo -e "${GREEN}Reviewer1 address generated.${WHITE}"
fi

# Generate reviewer2 keys
echo -e "${BLUE}Checking and generating reviewer2 keys...${WHITE}"
if check_file_exists "reviewer2.vkey" && check_file_exists "reviewer2.skey"; then
  docker exec -it cardano-node-preview cardano-cli address key-gen \
    --verification-key-file /assets/reviewer2.vkey \
    --signing-key-file /assets/reviewer2.skey
  echo -e "${GREEN}Reviewer2 keys generated.${WHITE}"
fi

# Generate reviewer2 key hash
echo -e "${BLUE}Checking and generating reviewer2 key hash...${WHITE}"
if check_file_exists "reviewer2.pkh"; then
  docker exec -it cardano-node-preview cardano-cli address key-hash \
    --payment-verification-key-file /assets/reviewer2.vkey \
    --out-file /assets/reviewer2.pkh
  echo -e "${GREEN}Reviewer2 key hash generated.${WHITE}"
fi

# Generate reviewer2 address for funding
echo -e "${BLUE}Checking and generating reviewer2 address...${WHITE}"
if check_file_exists "reviewer2.addr"; then
  docker exec -it cardano-node-preview cardano-cli address build \
    --payment-verification-key-file /assets/reviewer2.vkey \
    --out-file /assets/reviewer2.addr \
    --testnet-magic 2
  echo -e "${GREEN}Reviewer2 address generated.${WHITE}"
fi

echo -e "${GREEN}All keys have been generated successfully!${WHITE}"

# Output all generated file paths
echo -e "${YELLOW}Generated files:${WHITE}"
echo -e "${BLUE}Owner files:${WHITE}"
echo -e "  - $ASSETS_DIR/owner.vkey (Verification key)"
echo -e "  - $ASSETS_DIR/owner.skey (Signing key)"
echo -e "  - $ASSETS_DIR/owner.addr (Address)"
echo -e "  - $ASSETS_DIR/owner.pkh  (Public key hash)"

echo -e "${BLUE}Proposer files:${WHITE}"
echo -e "  - $ASSETS_DIR/proposer.vkey (Verification key)"
echo -e "  - $ASSETS_DIR/proposer.skey (Signing key)"
echo -e "  - $ASSETS_DIR/proposer.addr (Address)"
echo -e "  - $ASSETS_DIR/proposer.pkh  (Public key hash)"

echo -e "${BLUE}Reviewer 1 files:${WHITE}"
echo -e "  - $ASSETS_DIR/reviewer1.vkey (Verification key)"
echo -e "  - $ASSETS_DIR/reviewer1.skey (Signing key)"
echo -e "  - $ASSETS_DIR/reviewer1.addr (Address)"
echo -e "  - $ASSETS_DIR/reviewer1.pkh  (Public key hash)"

echo -e "${BLUE}Reviewer 2 files:${WHITE}"
echo -e "  - $ASSETS_DIR/reviewer2.vkey (Verification key)"
echo -e "  - $ASSETS_DIR/reviewer2.skey (Signing key)"
echo -e "  - $ASSETS_DIR/reviewer2.addr (Address)"
echo -e "  - $ASSETS_DIR/reviewer2.pkh  (Public key hash)"

# Provide instructions for next steps
echo -e "\n${YELLOW}Check README file for the next steps:${WHITE}"