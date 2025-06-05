from typing import Any
import logging
import cbor2
from pycardano import UTxO

from dfctbackend.config import settings
from dfctbackend.cardano.utils import str_to_hex

logger = logging.getLogger(__name__)

class DatumProcessor:
    """
    Base class for datum processing in DFCT contracts.
    """
    def __init__(self):
        """Initialize the contract with chain context and transaction handling."""
        self.token_name = settings.TOKEN_NAME
        self.token_name_hex = str_to_hex(settings.TOKEN_NAME)

    def _decode_value(self, value: Any) -> Any:
        if isinstance(value, bytes):
            datum = value
            if "\\" in str(value):
                return value.hex()
            return bytes(datum).decode('utf-8')

        if type(value) is str and str(value).startswith("b'"):
            return self._decode_value(bytes(value))

        return value

    def _decode_datum_list(self, datum_list: list) -> list:
        """
        Decode a datum's list.
        """
        assert (type(datum_list) is list)

        decoded = []
        for item in datum_list:
            new_item = item
            if isinstance(item, cbor2.CBORTag):
                new_item = self._decode_cbor_tag(item)
            elif isinstance(item, dict):
                new_item = self._decode_datum_dict(item)
            elif isinstance(item, list):
                new_item = self._decode_datum_list(item)
            else:
                new_item = self._decode_value(item)
            decoded.append(new_item)
        return decoded

    def _decode_datum_dict(self, datum_dict: dict) -> dict:
        """
        Decode a datum's dict.
        """
        assert (type(datum_dict) is dict)

        decoded = {}
        for key, value in datum_dict.items():
            d_key = self._decode_value(key)
            new_value = value
            if isinstance(value, cbor2.CBORTag):
                new_value = self._decode_cbor_tag(value)
            elif isinstance(value, list):
                new_value = self._decode_datum_list(value)
            elif isinstance(value, dict):
                new_value = self._decode_datum_dict(value)
            else:
                new_value = self._decode_value(value)
            decoded[d_key] = new_value
        return decoded

    def _decode_cbor_tag(self, datum: cbor2.CBORTag):
        """
        Decode a CBORTag object.
        """
        assert (isinstance(datum, cbor2.CBORTag))

        if not datum.value:
            decoded = (datum.tag - 121)
        else:
            decoded = self._decode_datum_bytes(datum.value)
        return decoded

    def _decode_datum_bytes(self, datum: list | dict):
        """
        Decode a datum's bytes.
        """
        if isinstance(datum, cbor2.CBORTag):
            return self._decode_cbor_tag(datum)
        if type(datum) is list:
            return self._decode_datum_list(datum)
        if type(datum) is dict:
            return self._decode_datum_dict(datum)
        return self._decode_value(datum)

    def decode_utxo_datum(self, topic_utxo: UTxO) -> list[Any]:
        """
        Extract and decode datum from a UTXO.
        """
        decoded = []
        if topic_utxo and hasattr(topic_utxo.output, 'datum') and topic_utxo.output.datum:
            datum = topic_utxo.output.datum
            if hasattr(datum, "cbor") and datum.cbor:
                datum = cbor2.loads(datum.cbor)
            if datum:
                decoded = self._decode_datum_bytes(datum)
        return decoded

    def find_utxo_with_datum_id(self, utxos: list[UTxO], id_str: str, x: int = 0, y: int = 0, z: int = -1) -> UTxO:
        """
        Find the UTxO with datum containing a specific ID in the id datum field.
        """
        for utxo in utxos:
            decoded = self.decode_utxo_datum(utxo)
            if decoded:
                if z >= 0:
                    datum_id = decoded[x][y][z]
                else:
                    datum_id = decoded[x][y]

                hex_str_value = str_to_hex(id_str)
                if (datum_id == id_str or 
                    datum_id == hex_str_value
                ):
                    return utxo, decoded
        return None, None

    def extract_int_from_datum(self, datum, ix):
        extracted_int = datum[ix]
        if not extracted_int:
            extracted_int = 0
        elif type(extracted_int) is list:
            extracted_int = extracted_int[0]
        elif type(extracted_int) is dict:
            extracted_int = list(extracted_int.keys())[0]
        return extracted_int