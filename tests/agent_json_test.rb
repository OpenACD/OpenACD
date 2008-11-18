
require "rubygems"
require "json"

JSON.parse(`curl -d "username=test" -d "password=yes" -c cookiejar -b cookiejar "http://localhost:5050/login"`)
JSON.parse(`curl -d "username=test" -d "password=yes" -c cookiejar -b cookiejar "http://localhost:5050/state/idle"`)
JSON.parse(`curl -d "username=test" -d "password=yes" -c cookiejar -b cookiejar "http://localhost:5050/poll"`)

