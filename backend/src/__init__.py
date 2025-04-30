import sys

# Make the package available as dfctbackend
sys.modules['dfctbackend'] = sys.modules[__name__]