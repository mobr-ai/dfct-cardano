import os
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

def write_temp_json(data: dict, filename: str, assets_path: str) -> str:
    """Write a dictionary to a temporary JSON file in assets directory."""
    file_path = os.path.join(assets_path, filename)
    with open(file_path, 'w') as f:
        json.dump(data, f, indent=2)
    return file_path