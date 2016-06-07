/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */

#include "adios_socket.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <string.h>

#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>


/* PUBLIC FUNCTIONS */


int adios_create_socket(int *socketid)
{
  *socketid = socket( AF_INET, SOCK_STREAM, 0 );
  if ( *socketid == -1 ) return 1;

  return 0;
}

int adios_close_socket(int socketid)
{
  return close(socketid);
}


int adios_set_socket_address(char *hostname,int port,struct sockaddr_in *address)
{
  struct hostent *hp;
  unsigned int addr;
  
  if (isalpha(hostname[0]))
  {   /* host address is a name */
      hp = gethostbyname(hostname);
  }
  else  
  { /* Convert nnn.nnn address to a usable one */
      addr = inet_addr(hostname);
      hp = gethostbyaddr((char *)&addr,4,AF_INET);
  }
  if (hp == NULL ) 
  {
      return 1;
  }

  memset(address,0,sizeof(*address));
  memcpy(&(address->sin_addr),hp->h_addr,hp->h_length);
  address->sin_family = hp->h_addrtype;
  address->sin_port = htons(port);

  return 0;
}

int adios_bind_socket(int socketid,struct sockaddr_in *address)
{
  if(bind(socketid, (struct sockaddr *) address, sizeof(*address))!=0)
    return 1;
  return 0;
}

int adios_connect_socket(int socketid,struct sockaddr_in *address)
{
  if (connect(socketid,(struct sockaddr*)address,sizeof(*address))== -1) 
    return 1;
  
  return 0;
}

int adios_socket_start_listen(int socketid)
{
  if(listen(socketid,5)!=0)
  {
      return 1;
  }
  return 0;
}

int adios_socket_accept(int socketid,int *connected)
{
  *connected=accept(socketid,NULL,0);
  if(*connected==-1)
  {
      return 1;
  }
  return 0;
}
   

int adios_blocking_read_request(int socketid,char *buffer,int maxlength)
{
  char cChar;
  int num = 0,ret;
  
  ret = recv( socketid, &cChar, 1 , 0 );
  if(ret<=0) return ret;
  buffer[num] = cChar;
  num++;
  
  while(cChar!='\0'&&num<=maxlength)
  {
      ret = recv( socketid, &cChar, 1 , 0 );
      buffer[num] = cChar;
      num++;
      if(ret<=0) return ret;
  }

  return num;
}

int adios_read_block(int socketid,char *buffer,int maxlength)
{
  int ret;
  
  ret = recv( socketid, buffer, maxlength , 0 );
  return ret;
}


int adios_blocking_write_request(int socketid,char *buffer,int length)
{
  if(send( socketid, buffer , length , 0 )==-1)
    return 1;
  
  return 0;
}

int adios_get_own_hostname(char *host)
{
  struct hostent *pMyHost;
  char buffer[255];
  
  if(gethostname ( buffer , 255 )!=0 ) return 1;
  pMyHost = gethostbyname ( buffer );
  if(pMyHost ==  NULL) return 1;
  strcpy( host , pMyHost->h_name );
  
  return 0;
}

int adios_get_remote_hostname(int socket,char *remotehost)
{
  struct sockaddr_in name;
  socklen_t namelen = sizeof (name);
  struct hostent *pHost;
  char buffer[255];
  unsigned int addr;

  getpeername(socket, (struct sockaddr *)&name, &namelen);
  strcpy(buffer,inet_ntoa(name.sin_addr));
  addr = inet_addr(buffer);
  pHost = gethostbyaddr((char *)&addr,4,AF_INET);
  strcpy(remotehost,pHost->h_name);
  
  return 0;
}


