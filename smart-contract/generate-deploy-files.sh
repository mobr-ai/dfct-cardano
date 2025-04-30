#!/bin/bash
# Script to generate necessary files for deployment. Specifically:
#   json
#      topic-datum.json
#      new-topic-datum.json
#      submit-topic-redeemer.json
#      unit-redeemer.json
#   sh
#      deploy-dfct-mp.sh
#      mint-dfc.sh
#      deploy-dfct-pv.sh
# This script should be executed from the smart-contract directory

# Define paths
ASSETS_PATH="$(pwd)/../backend/assets"

echo "Generating JSON files for DFCT contract..."

# Check if required files exist
echo "Checking required files..."
REQUIRED_FILES=("owner.pkh" "proposer.pkh" "reviewer1.pkh" "reviewer2.pkh" "dfct-minting-policy.id")
for file in "${REQUIRED_FILES[@]}"; do
  if [ ! -f "$ASSETS_PATH/$file" ]; then
    echo "Error: Required file $ASSETS_PATH/$file not found!"
    exit 1
  fi
done

echo "All required files found."

# Read key hashes and policy ID - using tr to remove any newlines
OWNER_PKH=$(cat "$ASSETS_PATH/owner.pkh" | tr -d '\n\r')
PROPOSER_PKH=$(cat "$ASSETS_PATH/proposer.pkh" | tr -d '\n\r')
REVIEWER1_PKH=$(cat "$ASSETS_PATH/reviewer1.pkh" | tr -d '\n\r')
REVIEWER2_PKH=$(cat "$ASSETS_PATH/reviewer2.pkh" | tr -d '\n\r')
POLICY_ID=$(cat "$ASSETS_PATH/dfct-minting-policy.id" | tr -d '\n\r')

# Generate a unique topic ID
TOPIC_ID="topic-$(date +%Y%m%d%H%M%S)"
TOPIC_ID_HEX=$(echo -n "${TOPIC_ID}" | xxd -p | tr -d '\n')
TOPIC_TITLE="First DFCT Topic"
TOPIC_TITLE_HEX=$(echo -n "${TOPIC_TITLE}" | xxd -p | tr -d '\n')
TOPIC_DESC="This is a test topic for the DFCT contract"
TOPIC_DESC_HEX=$(echo -n "${TOPIC_DESC}" | xxd -p | tr -d '\n')
TOKEN_NAME="DFC"
TOKEN_NAME_HEX=$(echo -n "${TOKEN_NAME}" | xxd -p | tr -d '\n')

# Create topic-datum.json
echo "Creating topic-datum.json..."
cat > "$ASSETS_PATH/topic-datum.json" <<EOF
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "${TOPIC_ID_HEX}"
        },
        {
          "bytes": "${TOPIC_TITLE_HEX}"
        },
        {
          "bytes": "${TOPIC_DESC_HEX}"
        },
        {
          "bytes": "${PROPOSER_PKH}"
        },
        {
          "int": 0
        }
      ]
    },
    {
      "constructor": 0,
      "fields": []
    },
    {
      "constructor": 0,
      "fields": [
        {
          "int": 1000
        },
        {
          "int": 0
        },
        {
          "bytes": "${TOKEN_NAME_HEX}"
        },
        {
          "int": 0
        }
      ]
    },
    {
      "map": [
        {
          "k": { "bytes": "${REVIEWER1_PKH}" },
          "v": { "int": 1 }
        },
        {
          "k": { "bytes": "${REVIEWER2_PKH}" },
          "v": { "int": 1 }
        }
      ]
    }
  ]
}
EOF

# Create unit-redeemer.json
echo "Creating unit-redeemer.json..."
cat > "$ASSETS_PATH/unit-redeemer.json" <<EOF
{
  "constructor": 0,
  "fields": []
}
EOF

# Create submit-topic-redeemer.json
echo "Creating submit-topic-redeemer.json..."
cat > "$ASSETS_PATH/submit-topic-redeemer.json" <<EOF
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "${TOPIC_ID_HEX}"
            },
            {
              "bytes": "${TOPIC_TITLE_HEX}"
            },
            {
              "bytes": "${TOPIC_DESC_HEX}"
            },
            {
              "bytes": "${PROPOSER_PKH}"
            },
            {
              "int": 0
            }
          ]
        },
        {
          "constructor": 0,
          "fields": [
            {
              "int": 1000
            },
            {
              "int": 0
            },
            {
              "bytes": "${TOKEN_NAME_HEX}"
            },
            {
              "int": 0
            }
          ]
        }
      ]
    }
  ]
}
EOF

# Create new-topic-datum.json (for step 3 in deployment)
echo "Creating new-topic-datum.json..."
cat > "$ASSETS_PATH/new-topic-datum.json" <<EOF
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "${TOPIC_ID_HEX}"
        },
        {
          "bytes": "${TOPIC_TITLE_HEX}"
        },
        {
          "bytes": "${TOPIC_DESC_HEX}"
        },
        {
          "bytes": "${PROPOSER_PKH}"
        },
        {
          "int": 0
        }
      ]
    },
    {
      "constructor": 1,
      "fields": []
    },
    {
      "constructor": 0,
      "fields": [
        {
          "int": 1000
        },
        {
          "int": 0
        },
        {
          "bytes": "${TOKEN_NAME_HEX}"
        },
        {
          "int": 0
        }
      ]
    },
    {
      "map": [
        {
          "k": { "bytes": "${REVIEWER1_PKH}" },
          "v": { "bool": true }
        },
        {
          "k": { "bytes": "${REVIEWER2_PKH}" },
          "v": { "bool": true }
        }
      ]
    }
  ]
}
EOF

echo "JSON files generated successfully!"
echo "Files created:"
echo "- $ASSETS_PATH/topic-datum.json"
echo "- $ASSETS_PATH/unit-redeemer.json"
echo "- $ASSETS_PATH/submit-topic-redeemer.json"
echo "- $ASSETS_PATH/new-topic-datum.json"


###################################
########## mint-dfc.sh ############
###################################
# script to mint 100k DFC to the address in the file param
echo "Creating deployment helper script..."
cat > "./mint-dfc.sh" <<EOF
#!/bin/bash

# Automatically generated by generate-deplyoy-files.sh
# script to mint 100k DFC to the address in the file param

# Define paths
ASSETS_PATH="\$(pwd)/../backend/assets"

# Define colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
WHITE='\033[0m'

# Required files check
echo -e "\${YELLOW}Checking required files...\${WHITE}"
for file in "protocol.json" "owner.skey" "owner.addr" "proposer.skey" "dfct-provenance.addr" "dfct-minting-policy.id" "dfct-minting-policy.plutus" "dfct-provenance.plutus" "topic-datum.json" "unit-redeemer.json" "submit-topic-redeemer.json" "new-topic-datum.json"; do
  if [ ! -f "$ASSETS_PATH/\$file" ]; then
    echo -e "\${RED}Error: Required file $ASSETS_PATH/\$file not found!\${WHITE}"
    echo "Please run the generate-json-files.sh script first and ensure all files are in place."
    if [ "\$file" = "protocol.json" ]; then
      echo "You need to create protocol.json with:"
      echo "docker exec -it cardano-node-preview cardano-cli query protocol-parameters --testnet-magic 2 --socket-path /data/node.socket --out-file $ASSETS_PATH/protocol.json"
    fi
    exit 1
  fi
done

# Check if address file is provided
if [ \$# -ne 1 ]; then
  echo "Usage: \$0 <address_file>"
  echo "Example: \$0 ../backend/assets/owner.addr"
  exit 1
fi

# Check if provided file exists
ADDRESS_FILE="\$1"
if [ ! -f "\$ADDRESS_FILE" ]; then
  echo "Error: Address file \$ADDRESS_FILE not found!"
  exit 1
fi

# Read recipient address from file
RECIPIENT_ADDR=\$(cat "\$ADDRESS_FILE" | tr -d '\n\r')
echo "Will mint tokens to address: \$RECIPIENT_ADDR"

# Step 1: Query UTxOs to find inputs
echo -e "\${YELLOW}Querying UTxOs at owner address...\${WHITE}"
OWNER_ADDR=\$(cat "$ASSETS_PATH/owner.addr" | tr -d '\n\r')
docker exec -it cardano-node-preview cardano-cli query utxo \
  --address \$OWNER_ADDR \
  --testnet-magic 2 \
  --socket-path /data/node.socket

echo -e "\${YELLOW}Please enter the TxHash for the input:\${WHITE}"
read TX_HASH
echo -e "\${YELLOW}Please enter the TxIx for the input:\${WHITE}"
read TX_IX
TXIN="\$TX_HASH#\$TX_IX"

echo -e "\${YELLOW}Please enter the TxHash for the collateral UTxO:\${WHITE}"
read COL_TX_HASH
echo -e "\${YELLOW}Please enter the TxIx for the collateral UTxO:\${WHITE}"
read COL_TX_IX
TXINCOLLATERAL="\$COL_TX_HASH#\$COL_TX_IX"

echo -e "\${YELLOW}Please enter the TxHash for the reference UTxO:\${WHITE}"
read REF_TX_HASH
echo -e "\${YELLOW}Please enter the TxIx for the reference UTxO:\${WHITE}"
read REF_TX_IX
REF_TXIN="\$REF_TX_HASH#\$REF_TX_IX"


# Step 2: Mint DFC tokens
echo -e "\${YELLOW}Building transaction to mint DFC tokens...\${WHITE}"
TOKEN_NAME="DFC" 
TOKEN_NAME_HEX=\$(echo -n "\$TOKEN_NAME" | xxd -p)
POLICY_ID=\$(cat "\$ASSETS_PATH/dfct-minting-policy.id" | tr -d '\n\r')
CURRENT_SLOT=\$(docker exec -it cardano-node-preview cardano-cli query tip --testnet-magic 2 --socket-path /data/node.socket | jq .slot)
INVALID_HEREAFTER=\$((CURRENT_SLOT + 1000))

docker exec -it cardano-node-preview cardano-cli latest transaction build \
  --testnet-magic 2 \
  --socket-path /data/node.socket \
  --tx-in \$TXIN \
  --tx-in-collateral \$TXINCOLLATERAL \
  --tx-out \$RECIPIENT_ADDR+80000000+"100000 \$POLICY_ID.\$TOKEN_NAME_HEX" \
  --change-address \$OWNER_ADDR \
  --mint="100000 \$POLICY_ID.\$TOKEN_NAME_HEX" \
  --mint-tx-in-reference \$REF_TXIN \
  --mint-plutus-script-v3 \
  --mint-reference-tx-in-redeemer-file /assets/unit-redeemer.json \
  --policy-id \$POLICY_ID \
  --required-signer /assets/owner.skey \
  --invalid-before \$CURRENT_SLOT \
  --invalid-hereafter \$INVALID_HEREAFTER \
  --out-file /assets/mint-dfct-tokens.raw

if [ \$? -ne 0 ]; then
  echo -e "\${RED}Failed to build minting transaction\${WHITE}"
  exit 1
fi

echo -e "\${YELLOW}Signing transaction...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction sign \
  --tx-body-file "/assets/mint-dfct-tokens.raw" \
  --signing-key-file "/assets/owner.skey" \
  --out-file "/assets/mint-dfct-tokens.signed" \
  --testnet-magic 2

echo -e "\${YELLOW}Submitting transaction...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction submit \
  --tx-file "/assets/mint-dfct-tokens.signed" \
  --testnet-magic 2 \
  --socket-path /data/node.socket

if [ \$? -ne 0 ]; then
  echo -e "\${RED}Failed to submit minting transaction\${WHITE}"
  exit 1
fi

echo -e "\${GREEN}Tokens minted successfully!\${WHITE}"
echo -e "\${YELLOW}Waiting 30 seconds for transaction to be confirmed...\${WHITE}"
sleep 30
EOF

###################################
####### deploy-dfct-pv.sh #########
###################################
# Script to deploy provenance validator contract
echo "Creating validator deployment script..."
cat > "./deploy-dfct-pv.sh" <<EOF
#!/bin/bash

# Automatically generated by generate-deplyoy-files.sh
# Script to deploy provenance validator contract

# Required files check
echo -e "\${YELLOW}Checking required files...\${WHITE}"
for file in "protocol.json" "owner.skey" "owner.addr" "proposer.skey" "dfct-provenance.addr" "dfct-minting-policy.id" "dfct-minting-policy.plutus" "dfct-provenance.plutus" "topic-datum.json" "unit-redeemer.json" "submit-topic-redeemer.json" "new-topic-datum.json"; do
  if [ ! -f "$ASSETS_PATH/\$file" ]; then
    echo -e "\${RED}Error: Required file $ASSETS_PATH/\$file not found!\${WHITE}"
    echo "Please run the generate-json-files.sh script first and ensure all files are in place."
    if [ "\$file" = "protocol.json" ]; then
      echo "You need to create protocol.json with:"
      echo "docker exec -it cardano-node-preview cardano-cli query protocol-parameters --testnet-magic 2 --socket-path /data/node.socket --out-file $ASSETS_PATH/protocol.json"
    fi
    exit 1
  fi
done

# Define paths
ASSETS_PATH="\$(pwd)/../backend/assets"
OWNER_ADDR=\$(cat "$ASSETS_PATH/owner.addr" | tr -d '\n\r')
VALIDATOR_ADDR=\$(cat "$ASSETS_PATH/dfct-provenance.addr" | tr -d '\n\r')

# Define colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
WHITE='\033[0m'

# Step 1: Query UTxOs to get txin and create UTxO ref to the provenance validator script
echo -e "\${YELLOW}Querying UTxOs...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli query utxo \
  --address \$OWNER_ADDR \
  --testnet-magic 2 \
  --socket-path /data/node.socket

echo -e "\${YELLOW}Please enter the TxHash for the input:\${WHITE}"
read TX_HASH
echo -e "\${YELLOW}Please enter the TxIx for the input:\${WHITE}"
read TX_IX
TXIN="\$TX_HASH#\$TX_IX"

echo -e "\${YELLOW}Creating reference UTxO to provenance validator...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction build \
  --tx-in \$TXIN \
  --tx-out \$OWNER_ADDR+90000000 \
  --tx-out-reference-script-file /assets/dfct-provenance.plutus \
  --change-address \$OWNER_ADDR \
  --out-file "/assets/create-provenance-reference.raw" \
  --testnet-magic 2 \
  --socket-path /data/node.socket

if [ \$? -ne 0 ]; then
  echo -e "\${RED}Failed to build provenance reference script transaction\${WHITE}"
  exit 1
fi

echo -e "\${YELLOW}Signing reference script transaction...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction sign \
  --tx-body-file "/assets/create-provenance-reference.raw" \
  --signing-key-file "/assets/owner.skey" \
  --out-file "/assets/create-provenance-reference.signed" \
  --testnet-magic 2

echo -e "\${YELLOW}Submitting transaction...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction submit \
  --tx-file "/assets/create-provenance-reference.signed" \
  --testnet-magic 2 \
  --socket-path /data/node.socket

if [ \$? -ne 0 ]; then
  echo -e "\${RED}Failed to submit reference provenance script transaction\${WHITE}"
  exit 1
fi

echo -e "\${GREEN}Reference provenance script UTxO created successfully!\${WHITE}"
echo -e "\${YELLOW}Waiting 30 seconds for transaction to be confirmed...\${WHITE}"
sleep 30

EOF

####################################
####### dfct-prov-first.sh #########
####################################
# Script to send first datum to provenance validator contract
echo "Creating script to send first datum to provenance validator contract..."
cat > "./dfct-prov-first.sh" <<EOF
#!/bin/bash

# Automatically generated by generate-deplyoy-files.sh
# Script to send first datum to provenance validator contract

# Required files check
echo -e "\${YELLOW}Checking required files...\${WHITE}"
for file in "protocol.json" "owner.skey" "owner.addr" "proposer.skey" "dfct-provenance.addr" "dfct-minting-policy.id" "dfct-minting-policy.plutus" "dfct-provenance.plutus" "topic-datum.json" "unit-redeemer.json" "submit-topic-redeemer.json" "new-topic-datum.json"; do
  if [ ! -f "$ASSETS_PATH/\$file" ]; then
    echo -e "\${RED}Error: Required file $ASSETS_PATH/\$file not found!\${WHITE}"
    echo "Please run the generate-json-files.sh script first and ensure all files are in place."
    if [ "\$file" = "protocol.json" ]; then
      echo "You need to create protocol.json with:"
      echo "docker exec -it cardano-node-preview cardano-cli query protocol-parameters --testnet-magic 2 --socket-path /data/node.socket --out-file $ASSETS_PATH/protocol.json"
    fi
    exit 1
  fi
done

# Define paths
ASSETS_PATH="\$(pwd)/../backend/assets"
OWNER_ADDR=\$(cat "$ASSETS_PATH/owner.addr" | tr -d '\n\r')
VALIDATOR_ADDR=\$(cat "$ASSETS_PATH/dfct-provenance.addr" | tr -d '\n\r')
TOKEN_NAME="DFC" 
TOKEN_NAME_HEX=\$(echo -n "\$TOKEN_NAME" | xxd -p)
POLICY_ID=\$(cat "\$ASSETS_PATH/dfct-minting-policy.id" | tr -d '\n\r')

# Define colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
WHITE='\033[0m'

# Step 1: Query UTxOs to get script reference and collateral
echo -e "\${YELLOW}Querying UTxOs...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli query utxo \
  --address \$OWNER_ADDR \
  --testnet-magic 2 \
  --socket-path /data/node.socket

echo -e "\${YELLOW}Please enter the TxHash for the input with DFC tokens:\${WHITE}"
read TOKEN_TX_HASH
echo -e "\${YELLOW}Please enter the TxIx for the input with DFC tokens:\${WHITE}"
read TOKEN_TX_IX
TXIN_WITH_TOKENS="\$TOKEN_TX_HASH#\$TOKEN_TX_IX"
echo -e "\${YELLOW}Please enter the TxHash for the reference provenance script UTxO:\${WHITE}"
read REF_VALIDATOR_TX_HASH
echo -e "\${YELLOW}Please enter the TxIx for the validator UTxO:\${WHITE}"
read REF_VALIDATOR_TX_IX
REF_VALIDATOR_UTXO="\$REF_VALIDATOR_TX_HASH#\$REF_VALIDATOR_TX_IX"
echo -e "\${YELLOW}Please enter the TxHash for the collateral UTxO:\${WHITE}"
read COL_TX_HASH
echo -e "\${YELLOW}Please enter the TxIx for the collateral UTxO:\${WHITE}"
read COL_TX_IX
TXIN_COLLATERAL="$COL_TX_HASH#$COL_TX_IX"


# Step 2: Transactions to send ADA and DFC tokens to provenance validator

echo -e "\${YELLOW}Building transactions to send topic datum, ADA and DFC tokens to provenance validator...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction build \
  --tx-in \$TXIN_WITH_TOKENS \
  --tx-out \$VALIDATOR_ADDR+40000000+"100000 \$POLICY_ID.\$TOKEN_NAME_HEX" \
  --change-address \$OWNER_ADDR \
  --tx-out-datum-embed-file "/assets/topic-datum.json" \
  --out-file "/assets/send-dfc-to-provenance.raw" \
  --testnet-magic 2 \
  --socket-path /data/node.socket

if [ \$? -ne 0 ]; then
  echo -e "\${RED}Failed to build transaction to send dfc to provenance script\${WHITE}"
  exit 1
fi

echo -e "\${YELLOW}Signing reference script transaction...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction sign \
  --tx-body-file "/assets/send-dfc-to-provenance.raw" \
  --signing-key-file "/assets/owner.skey" \
  --out-file "/assets/send-dfc-to-provenance.signed" \
  --testnet-magic 2

echo -e "\${YELLOW}Submitting transaction...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction submit \
  --tx-file "/assets/send-dfc-to-provenance.signed" \
  --testnet-magic 2 \
  --socket-path /data/node.socket

if [ \$? -ne 0 ]; then
  echo -e "\${RED}Failed to submit dfc and datum transaction\${WHITE}"
  exit 1
fi

echo -e "\${GREEN}DFC and topic datum sent successfully!\${WHITE}"
echo -e "\${YELLOW}Waiting 30 seconds for transaction to be confirmed...\${WHITE}"
sleep 30

EOF

###################################
####### deploy-dfct-mp.sh #########
###################################
# Script to deploy dfct minting policy script in a reference utxo

echo "Creating deploy-dfct-mp script..."
cat > "./deploy-dfct-mp.sh" <<EOF
#!/bin/bash

# Automatically generated by generate-deplyoy-files.sh
# Creates reference utxo for minting DFC
# This script assumes your node is fully synced

# Define paths
ASSETS_PATH="\$(pwd)/../backend/assets"
OWNER_ADDR=\$(cat "$ASSETS_PATH/owner.addr" | tr -d '\n\r')

# Define colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
WHITE='\033[0m'

echo -e "\${YELLOW}Querying UTxOs at owner address...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli query utxo \
  --address \$OWNER_ADDR \
  --testnet-magic 2 \
  --socket-path /data/node.socket

echo -e "\${YELLOW}Please enter the TxHash for the input:\${WHITE}"
read TX_HASH
echo -e "\${YELLOW}Please enter the TxIx for the input:\${WHITE}"
read TX_IX
TXIN="\$TX_HASH#\$TX_IX"

echo -e "\${YELLOW}Creating reference UTxO for minting policy...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction build \
  --tx-in \$TXIN \
  --tx-out \$OWNER_ADDR+30000000 \
  --tx-out-reference-script-file /assets/dfct-minting-policy.plutus \
  --change-address \$OWNER_ADDR \
  --out-file "/assets/create-reference.raw" \
  --testnet-magic 2 \
  --socket-path /data/node.socket

echo -e "\${YELLOW}Signing reference transaction...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction sign \
  --tx-body-file "/assets/create-reference.raw" \
  --signing-key-file "/assets/owner.skey" \
  --out-file "/assets/create-reference.signed" \
  --testnet-magic 2

echo -e "\${YELLOW}Submitting reference transaction...\${WHITE}"
docker exec -it cardano-node-preview cardano-cli latest transaction submit \
  --tx-file "/assets/create-reference.signed" \
  --testnet-magic 2 \
  --socket-path /data/node.socket

echo -e "\${YELLOW}Waiting 30 seconds for transaction to confirm...\${WHITE}"
sleep 30
echo -e "\${GREEN}All done!\${WHITE}"
echo -e "\${YELLOW}Wait until reference utxo appears in the owner account:"
echo -e "docker exec -it cardano-node-preview cardano-cli query utxo \
  --address $(cat ../backend/assets/owner.addr) \
  --testnet-magic 2 \
  --socket-path /data/node.socket"
EOF

# Making the deployment script executable
chmod +x "./deploy-dfct-mp.sh"
chmod +x "./mint-dfc.sh"
chmod +x "./deploy-dfct-pv.sh"
chmod +x "./dfct-prov-first.sh"

echo "Use ./deploy-dfct-mp.sh to deploy DFC minting policy as a reference script utxo"
echo "Minting script created at ./mint-dfc.sh"
echo "Provenance validator deploy script created at ./deploy-dfct-pv.sh"
echo "Script to send first topic datum, ADA and DFC tokens created at ./dfct-prov-first.sh"