if [ ! -f "key" ]; then
	echo "RSA key does not exist, generating..."
	ssh-keygen -t rsa -f key -N ""
	RES=$?
	if [ $RES != 0 ]; then
		echo "Key generation failed with error $RES!"
		exit $RES
	fi
fi
	
if [ ! -f "openacd.csr" ]; then
	echo "Certificate Signing Request does not exist, generating..."
	openssl req -new -key key -out openacd.csr
	RES=$?
	if [ $RES != 0 ]; then
		echo "CSR generation failed with error $RES"
		exit $RES
	fi
fi

if [ ! -f "openacd.crt" ]; then
	echo "Certificate does not exists, generating self-signed for a year..."
	openssl x509 -req -days 365 -in openacd.csr -signkey key -out openacd.crt
	RES=$?
	if [ $RES != 0 ]; then
		echo "Certificate generation failed with error $RES"
		exit $RES
	fi
fi
