import json
import binascii
import uuid
import logging
from pathlib import Path
from typing import Any

from pycardano import PlutusV3Script

logger = logging.getLogger(__name__)

def hex_to_bytes(hex_str: str) -> bytes:
    """Convert hex string to bytes."""
    try:
        return binascii.unhexlify(hex_str)
    except binascii.Error as e:
        raise ValueError(f"Invalid hex string: {str(e)}")

def bytes_to_hex(bytes_data: bytes) -> str:
    """Convert bytes to hex string."""
    return binascii.hexlify(bytes_data).decode('utf-8')

def str_to_hex(text: str) -> str:
    """Convert string to hex."""
    if text == "": return ""
    return bytes_to_hex(text.encode('utf-8'))

def hex_to_str(hex_str: str) -> str:
    """Convert hex string to string."""
    return hex_to_bytes(hex_str).decode('utf-8')

def load_plutus_script(file_path: Path) -> PlutusV3Script:
    """
    Load a Plutus script from a file.
    """
    try:
        with open(file_path, 'r') as f:
            content = f.read().strip()
            try:
                data = json.loads(content)
                if 'cborHex' not in data:
                    raise ValueError("JSON script missing 'cborHex' field")
                cbor_bytes = hex_to_bytes(data['cborHex'])
                return PlutusV3Script(cbor_bytes)
            except json.JSONDecodeError:
                cbor_bytes = hex_to_bytes(content)
                return PlutusV3Script(cbor_bytes)
    except Exception as e:
        logger.error(f"Failed to load Plutus script from {file_path}: {str(e)}")
        raise ValueError(f"Failed to load Plutus script: {str(e)}")

def load_json_file(file_path: Path) -> dict[str, Any]:
    """
    Load JSON from a file.
    """
    try:
        with open(file_path, 'r') as f:
            return json.load(f)
    except Exception as e:
        logger.error(f"Failed to load JSON from {file_path}: {str(e)}")
        raise ValueError(f"Failed to load JSON: {str(e)}")

def load_raw_file(file_path: Path) -> str:
    """
    Load raw data from a file.
    """
    try:
        with open(file_path, 'r') as f:
            return f.read().strip()
    except Exception as e:
        logger.error(f"Failed to load raw data from {file_path}: {str(e)}")
        raise ValueError(f"Failed to load raw data: {str(e)}")

def save_json_file(data: dict[str, Any], file_path: Path) -> None:
    """
    Save JSON data to a file.
    """
    try:
        with open(file_path, 'w') as f:
            json.dump(data, f, indent=2)
        logger.debug(f"Saved JSON to {file_path}")
    except Exception as e:
        logger.error(f"Failed to save JSON to {file_path}: {str(e)}")
        raise ValueError(f"Failed to save JSON: {str(e)}")

def generate_topic_id() -> str:
    """
    Generate a unique topic ID.
    """
    return f"t{uuid.uuid4().hex[:8]}"

def generate_contribution_id() -> str:
    """
    Generate a unique contribution ID.
    """
    return f"c{uuid.uuid4().hex[:8]}"