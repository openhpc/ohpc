#
# Ganglia monitoring system php web frontend
#

Alias /ganglia /usr/share/ganglia-ohpc

<Location /ganglia>
  Require all granted
  # Require ip 10.1.2.3
  # Require host example.org
</Location>
