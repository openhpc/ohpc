/* 
 * ADIOS is freely available under the terms of the BSD license described
 * in the COPYING file in the top level directory of this source distribution.
 *
 * Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
 */


#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

/** Get host:port info from some file.
    Input: 
      env_var_name    environment variable name that contains the path to a file to be checked
      filename        name of file to be looked for if not named in the env var.
      path,           first path to look for 'filename'
      alternate_path  alternate path to look for 'filename'
      default_port    value to be returned in 'port' if port info is not found

    Output: 
      host            host name, string will be allocated
      port            port value

    Host will be NULL if information is not found 
    port will be default_port if that part of info is not found
 */


/// Socket functions

int adios_create_socket(int *socketid);
int adios_close_socket(int socketid);
int adios_set_socket_address(char *hostname,int port,struct sockaddr_in *address);
int adios_bind_socket(int socketid,struct sockaddr_in *address);
int adios_connect_socket(int socketid,struct sockaddr_in *address);
int adios_socket_start_listen(int socketid);
int adios_socket_accept(int socketid,int *connected);
int adios_blocking_read_request(int socketid,char *buffer,int maxlength);

/** Read message in one block that fits into buffer. 
Much faster than adios_BlockingReadRequest that is used for short message exchange only.
*/
int adios_read_block(int socketid,char *buffer,int maxlength);

int adios_blocking_write_request(int socketid,char *buffer,int length);
int adios_get_own_hostname(char *host);
int adios_get_remote_hostname(int socket,char *remotehost);
