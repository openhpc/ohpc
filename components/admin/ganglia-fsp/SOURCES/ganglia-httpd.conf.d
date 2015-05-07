#
# Ganglia monitoring system php web frontend
#

Alias /ganglia /usr/share/ganglia

<Location /ganglia>
  Order deny,allow
  Deny from all
  Allow from 127.0.0.1
  Allow from ::1
  # Allow from .example.com
</Location>
