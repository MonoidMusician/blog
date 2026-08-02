import os, sys

if os.path.isfile("cert/localhost.crt") and os.path.isfile("cert/localhost.key"):
    sys.exit(0)

from cryptography import x509
from cryptography.x509.oid import NameOID
from cryptography.hazmat.primitives.asymmetric import rsa, ec
from cryptography.hazmat.primitives import serialization, hashes
import datetime

try: os.mkdir("cert")
except FileExistsError: pass

key = ec.generate_private_key(
    ec.SECP384R1()
)
# key = rsa.generate_private_key(
#     public_exponent=65537,
#     key_size=4096,
# )

subject = issuer = x509.Name([])

now = datetime.datetime.now(datetime.timezone.utc)

cert = x509.CertificateBuilder(
).subject_name(
    subject
).issuer_name(
    issuer
).public_key(
    key.public_key()
).serial_number(
    x509.random_serial_number()
).not_valid_before(
    now
).not_valid_after(
    now + datetime.timedelta(days=365)
).add_extension(
    x509.SubjectAlternativeName([x509.DNSName("localhost")]),
    critical=False,
# Sign our certificate with our private key
).sign(key, hashes.SHA256())

with open("cert/localhost.key", "wb") as f:
    f.write(key.private_bytes(
        encoding=serialization.Encoding.PEM,
        format=serialization.PrivateFormat.PKCS8,
        encryption_algorithm=serialization.NoEncryption(),
    ))
with open("cert/localhost.crt", "wb") as f:
    f.write(cert.public_bytes(serialization.Encoding.PEM))

# openssl x509 -inform pem -in cert/localhost.crt -noout -text
