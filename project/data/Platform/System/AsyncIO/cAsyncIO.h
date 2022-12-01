#ifndef __CASYNCIO__
#define __CASYNCIO__

#include "hashtable.h"

typedef struct io_data_s io_data_t;

// Functions called through Clean Foreign Function Interface.
int ioInitC();
int ioGetEventsC(int main_fd, int timeout, CleanIntArray p_fd_list, CleanIntArray p_ev_kinds);
void windowsAcceptC (int main_fd, int listen_fd, int *p_err_code, int *p_client_fd);
void acceptCAsyncIO (int main_fd, int listen_fd, int *p_err_code, int *p_client_fd);
void tcplistenC(int main_fd, int port, int *p_err_code, int *p_listen_fd);
void connectC (int main_fd, int ip_addr, int port, int *p_err_code, int *p_client_fd);
int queueWriteSockC(int fd, char *p_send_data, int size);
int queueWritePipeC(int fd, char* p_send_data, int size);
int signalSendSockC(int main_fd, int fd);
int signalSendPipeC(int main_fd, int fd);
int getpeernameC(int client_fd, int listen_fd, int *p_host);
void retrieveDataC(int main_fd, int fd, int *p_err_code, CleanString *p_received);
int cleanupFdC(int main_fd, int fd, int isASocket);
int ioMonitorPipeC(int main_fd, int pipe_fd);
int windowsCreatePipeC(int* p_read_handle, int* p_write_handle);
int windowsIncPacketsToSendC(int fd, int num_packets);
int windowsAnyPacketsC (int fd, int* anyPackets);
void windowsReadSockC(int fd, int *p_err_code);
void windowsReadPipeC(int fd, int *p_err_code);

// Internal helper functions.
int ioMonitorFd(int main_fd, int fd, int op);
io_data_t* initIOData(int fd, int op);
table_entry_t* setSendBuf(char* p_fd_str, char* p_send_data, int size);
int initFdBuffers(int client_fd);
int fdToFdStr(int fd, char* p_fd_str);
#endif
