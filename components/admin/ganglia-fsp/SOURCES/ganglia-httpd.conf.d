#
# Ganglia monitoring system php web frontend
#

Alias /ganglia /usr/share/ganglia

<Location /ganglia>
  Require all granted
  Order deny,allow
  Deny from all
  Allow from 192.168.0.1
  Allow from 127.0.0.1
  Allow from ::1
  # Allow from .example.com
</Location>
