#
# Ganglia monitoring system php web frontend
#

Alias /ganglia /usr/share/ganglia

<Location /ganglia>
  Require local
  # Require ip 10.1.2.3
  # Require host example.org
</Location>
