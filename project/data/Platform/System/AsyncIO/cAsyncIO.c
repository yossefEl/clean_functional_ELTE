#include <sys/event.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include "Clean.h"
#include "cAsyncIO.h"
#include "queue.h"
#include "hashtable.h"

#define MAX_SUPPORTED_FDS 2500

// Events returned to Clean.
#define InConnectionEvent 0
#define OutConnectionEvent 1
#define ReadEventSock 2
#define ReadEventPipe 3
#define SendEventSock 4
#define SendEventPipe 5
#define ReadAndSendEventSock 6
#define ReadAndSendEventPipe 7
#define DisconnectEventSock 8
#define DisconnectEventPipe 9
#define SendEventNop 10

// Stores file descriptor and context it is being monitored for (e.g InConnectionEvent).
typedef struct io_data_s {
	int fd;
	int op;
} io_data_t;

CleanString rcv_buf;
int rcv_buf_size;
// Hashtable storing the data that is to be sent for every client/pipe.
hashtable_t* ht;

int ioInitC () {
	// Init rcv_buf_size.
	int s = socket(PF_INET, SOCK_STREAM, 0);
	socklen_t idc = sizeof(rcv_buf_size);
	getsockopt(s, SOL_SOCKET, SO_RCVBUF, (void*) &rcv_buf_size, &idc);
	close(s);

	rcv_buf = (CleanString) malloc(rcv_buf_size + sizeof(long));
	if (rcv_buf == NULL) {
		return -1;
	}

	// Create hash table for writequeues.
	ht = initHT(MAX_SUPPORTED_FDS);
	if (ht == NULL) {
		return -1;
	}
	// Creates kqueue fd.
	return kqueue();
}

int ioMonitorFd (int main_fd, int fd, int op) {
	// io_data_t is retrieved and used by ioGetEventsC to determine which event should be returned to Clean.
	io_data_t* io_data = initIOData(fd, op);
	if (io_data == NULL) {
		return -1;
	}

	struct kevent e;

	// Watch for readability.
	if (op == InConnectionEvent || op == ReadEventSock || op == ReadEventPipe) {
		EV_SET(&e, fd, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, (void*) io_data);
	}
	else { // Watch for readability and writability (ReadAndSendEvent)
		EV_SET(&e, fd, EVFILT_WRITE, EV_ADD | EV_ENABLE, 0, 0, (void*) io_data);
		kevent(main_fd, &e, 1, NULL, 0, NULL);
		EV_SET(&e, fd, EVFILT_READ, EV_ADD  | EV_ENABLE, 0, 0, (void*) io_data);
	}

	return kevent(main_fd, &e, 1, NULL, 0, NULL);
}

int ioGetEventsC (int main_fd, int timeout, CleanIntArray p_fd_list, CleanIntArray p_ev_kinds) {
	struct timespec time = {.tv_sec = timeout/1000, .tv_nsec = (timeout % 1000) * 1000000};
	struct kevent events[MAX_SUPPORTED_FDS];
	int num_fds = CleanIntArraySize(p_fd_list);

	if (num_fds == 0) {
		return 0;
	}

	// Retrieve events.
	int num_events = kevent(main_fd, NULL, 0, events, num_fds, &time);
	if(num_events == -1) {
		printf("ioGetEventsC: Error retrieving events.\n");
		return -1;
	}

	for (int i = 0; i < num_events; i++) {
		io_data_t* io_data = (io_data_t*) events[i].udata;
		p_fd_list[i] = io_data->fd;

		int read_ev, send_ev, disconnect_ev;
		// Monitoring pipe.
		if (io_data->op == ReadAndSendEventPipe || io_data->op == ReadEventPipe) {
			read_ev = ReadEventPipe;
			send_ev = SendEventPipe;
			disconnect_ev = DisconnectEventPipe;
		} else { // Monitoring socket.
			read_ev = ReadEventSock;
			send_ev = SendEventSock;
			disconnect_ev = DisconnectEventSock;
		}

		if (events[i].flags & EV_ERROR){
			printf("ioGetEventsC: EV_ERROR %d.\n", errno);
			p_ev_kinds[i] = disconnect_ev;
			continue;
		}

		if (events[i].flags & EV_EOF){
			printf("ioGetEventsC: EV_EOF.\n");
			p_ev_kinds[i] = disconnect_ev;
			continue;
		}

		// If both read and write are monitored it is determined whether this particular event is a read or write event.
		// Unlike for epoll (Linux), it is not possible for a read and write event to be returned at the same time.
		if (io_data->op == ReadAndSendEventSock || io_data->op == ReadAndSendEventPipe) {
			if (events[i].filter == EVFILT_READ) {
				p_ev_kinds[i] = read_ev;
			} else {
				p_ev_kinds[i] = send_ev;
			}
		} else {
			p_ev_kinds[i] = io_data->op;
		}

		if(io_data->op == OutConnectionEvent) {
			// Stop monitoring for writability.
			struct kevent e;
			EV_SET(&e, events[i].ident, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
			if (kevent(main_fd, &e, 1, NULL, 0, NULL) == -1) {
				return -1;
			}

			// Check if connection attempt succeeded.
			struct sockaddr_in addr;
			socklen_t addr_len = sizeof(addr);
			int err = getpeername(io_data->fd, (struct sockaddr*) &addr, &addr_len);
			if (err) {
				printf("Connection attempt for fd %d failed.", io_data->fd);
				continue;
			}
			// Monitor for readability after connecting.
			ioMonitorFd(main_fd, io_data->fd, ReadEventSock);
		}

		// Send data if necessary.
		if ((io_data->op == ReadAndSendEventSock || io_data->op == ReadAndSendEventPipe) && events[i].filter == EVFILT_WRITE) {
			// Retrieve write queue hashtable entry for fd.
			int length = snprintf(NULL, 0, "%d", io_data->fd);
			char* fd_str = malloc(length + 1);
			if(fd_str == NULL) {
				return -1;
			}
			snprintf(fd_str, length + 1, "%d", io_data->fd);
			queue_t* write_queue = getHT(ht, fd_str);

			if (write_queue == NULL) {
				return -1;
			}

			// Writequeue was found, send data in writequeue.
			while (!isEmpty(write_queue)) {
				int total_bytes_sent = 0;
				packet_t* packet = dequeue(write_queue);
				while (total_bytes_sent < packet->size) {
					int bytes_sent = write(io_data->fd, packet->data+total_bytes_sent, packet->size-total_bytes_sent);
					if(bytes_sent == -1) {
						printf("send failed, errno: %d\n", errno);
						// Broken pipe (ungraceful disconnect by peer).
						if (errno == EPIPE || errno == ECONNRESET) {
							p_ev_kinds[i] = disconnect_ev;
							goto endSend;
						} else if (errno == EWOULDBLOCK || errno == EAGAIN) {
							/* The fd remains monitored for writability but the send attempt is aborted
							 * until the fd is writable again.
							 * goto is used to easily break out of the writing loop
							 * as the sendqueue might not be empty.
							 * Because of the location of endsend, the file descriptor remains monitored for writability.
							 * The data remaining in the packet that was not completely sent is requeued.
							 */
							int err = enqueueFront(write_queue, packet->data+total_bytes_sent, packet->size-total_bytes_sent);
							if (err) {
								return -1;
							}

							err = putHT(ht, fd_str, write_queue);
							if (err) {
								return -1;
							}

							p_ev_kinds[i] = SendEventNop;
							goto endSend;
						} else {
							return -1;
						}
					}
					total_bytes_sent += bytes_sent;
				}
			}
			// All data in the write queue has been sent, stop monitoring for writability (until there is data to be sent again).
			struct kevent e;
			EV_SET(&e, events[i].ident, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
			if(kevent(main_fd, &e, 1, NULL, 0, NULL) == -1) {
				return -1;
			}
			endSend: ;
		}

	}
	return num_events;
}

void windowsAcceptC (int main_fd, int server, int *p_err_code, int *p_client_fd) {
	*p_err_code = 0;
	*p_client_fd = 0;
}

int allocWriteQueue(int fd) {
	char key[9];
	int err = fdToFdStr(fd, key);

	if (err) {
		return -1;
	}

	queue_t* queue = initQueue();
	if (queue == NULL) {
		return -1;
	}
	return putHT(ht, key, queue);
}

int fdToFdStr(int fd, char* fd_str_buf) {
	int err = sprintf(fd_str_buf, "%d", fd);
	if (err < 0) {
		printf("cAsyncIO.c, sprintf failed.\n");
		return -1;
	}
	return 0;
}

void acceptCAsyncIO (int main_fd, int server, int *p_err_code, int *p_client_fd) {
	// Accept connection request.
	int client_fd = accept(server, (struct sockaddr*) 0, (int*) 0);
	if (client_fd == -1) {
		if (errno == EAGAIN || errno == EWOULDBLOCK || errno == ECONNABORTED || errno == EPROTO) {
			*p_err_code = -2;
			close(client_fd);
			return;
		} else {
			*p_err_code = -1;
			close(client_fd);
			return;
		}
	}

	// Set the client socket to be non-blocking.
	int flags = fcntl(client_fd, F_GETFL, 0);
	fcntl(client_fd, F_SETFL, flags | O_NONBLOCK);

	// Allocate a write queue for the client, in which the data to be written is stored.
	// The data is read from the queue by ioGetEventsC if the client is in a writable state.
	int err = allocWriteQueue(client_fd);
	if (err) {
		*p_err_code = -1;
		return;
	}

	// Monitor client socket for readability.
	*p_err_code = ioMonitorFd(main_fd, client_fd, ReadEventSock);
	*p_client_fd = client_fd;
}


void tcplistenC (int main_fd, int port, int *p_err_code, int *p_listen_fd)
{
	struct sockaddr_in srv_adr;
	*p_err_code = -1;

	// Create socket that listens on any available port/address.
	int listen_fd = socket (PF_INET, SOCK_STREAM, 0);
	if (listen_fd==-1) {
		return;
	}

	srv_adr.sin_family = AF_INET;
	srv_adr.sin_addr.s_addr = INADDR_ANY;
	srv_adr.sin_port = htons((short int)port);

	int so_reuseaddr = 1;
	*p_err_code = setsockopt (listen_fd, SOL_SOCKET, SO_REUSEADDR, &so_reuseaddr, sizeof so_reuseaddr);
	if (*p_err_code){
		close(listen_fd);
		return;
	}

	// Make listening socket non-blocking.
	*p_err_code = fcntl(listen_fd, F_SETFL, fcntl(listen_fd, F_GETFL, 0) | O_NONBLOCK);
	if (*p_err_code) {
		close(listen_fd);
		return;
	}

	*p_err_code = bind (listen_fd, (struct sockaddr*) &srv_adr, sizeof(srv_adr));
	if (*p_err_code){
		close(listen_fd);
		return;
	}

	*p_err_code = listen (listen_fd, 5);
	if (*p_err_code){
		close(listen_fd);
		return;
	}

	// Monitor listen socket for connection requests.
	if (ioMonitorFd(main_fd, listen_fd, InConnectionEvent)) {
		*p_err_code = -1;
	}

	*p_listen_fd = listen_fd;
}

void connectC (int main_fd, int ipAddr, int port, int *p_err_code, int *p_client_fd)
{
	*p_err_code = -1;

	struct sockaddr_in srv_adr,client_adr;
	int client_fd = socket (PF_INET, SOCK_STREAM, 0);

	if (client_fd==-1) {
		return;
	}

	// Make client socket non-blocking.
	int err = fcntl(client_fd, F_SETFL, fcntl(client_fd, F_GETFL, 0) | O_NONBLOCK);
	if (err == -1) {
		return;
	}

	client_adr.sin_family = AF_INET;
	client_adr.sin_addr.s_addr = INADDR_ANY;
	client_adr.sin_port = 0;

	err = bind (client_fd, (struct sockaddr*) &client_adr, sizeof(client_adr));

	if (err){
		close (client_fd);
		return;
	}

	srv_adr.sin_family = AF_INET;
	srv_adr.sin_addr.s_addr = htonl (ipAddr);
	srv_adr.sin_port = htons ((short int)port);
	err = connect (client_fd, (struct sockaddr*) &srv_adr, sizeof(srv_adr));
	if (err && !(errno==EINPROGRESS)){
		close (client_fd);
		return;
	}
	err = allocWriteQueue(client_fd);
	if (err) {
		return;
	}

	// Monitor socket for writability to return event indicating successful connection attempt.
	*p_err_code = ioMonitorFd(main_fd, client_fd, OutConnectionEvent);
	*p_client_fd = client_fd;
}

// Add data to write queue, which is emptied when data is sent in ioGetEventsC.
int queueWriteSockC(int fd, char* p_send_data, int size) {
	char key[9];
	int err = fdToFdStr(fd, key);
	if (err) {
		return -1;
	}
	queue_t* queue = getHT(ht, key);
	if (queue == NULL) {
		return -1;
	}

	err = enqueue(queue, p_send_data, size);
	if (err) {
		return -1;
	}
	return putHT(ht, key, queue);
}

int queueWritePipeC(int fd, char* p_send_data, int size) {
	return queueWriteSockC(fd, p_send_data, size);
}

int signalSendSockC(int main_fd, int fd) {
	return ioMonitorFd(main_fd, fd, ReadAndSendEventSock);
}

int signalSendPipeC(int main_fd, int fd) {
	return ioMonitorFd(main_fd, fd, ReadAndSendEventPipe);
}

int getpeernameC(int client_fd, int listen_fd, int* p_host) {
	struct sockaddr_in addr;
	socklen_t addr_len = sizeof(addr);
	int err = getpeername(client_fd, (struct sockaddr*) &addr, &addr_len);
	if(!err) {
		*p_host = ntohl(addr.sin_addr.s_addr);
	}
	return err;
}

void retrieveDataC(int main_fd, int fd, int* p_err_code, CleanString *p_received) {
	int bytes_received = read(fd, CleanStringCharacters(rcv_buf), rcv_buf_size);
	// Data was received.
	if (bytes_received > 0) {
		CleanStringLength(rcv_buf) = bytes_received;
		*p_received = rcv_buf;
		*p_err_code = 1;
		return;
	}
	// Graceful peer disconnect.
	if (bytes_received == 0) {
		CleanStringLength(rcv_buf) = 0;
		*p_received = rcv_buf;
		*p_err_code = 0;
		return;
	}
	CleanStringLength(rcv_buf) = 0;
	*p_received = rcv_buf;
	if (errno == 5) {
		*p_err_code = 0;
		return;
	}
	*p_err_code = -1;
}

int cleanupFdC(int main_fd, int fd, int isASocket) {
	char key[9];
	int err = fdToFdStr(fd,key);
	if (err) {
		return -1;
	}
	err = removeHT(ht, key);
	if (err) {
		return -1;
	}

	// In case of using pipes the file descriptor is duplicated (e.g stdout is redirected to a pipe, which involves dup).
	// This results in the pipe fd still being monitored by kqueue on close,
	// since this might not be the last close (see kqueue manual).
	// Therefore, in case of cleaning up a pipe it is deregistered from kqueue explicitly.
	if (!isASocket) {
		struct kevent e;
		EV_SET(&e, fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
		if(kevent(main_fd, &e, 1, NULL, 0, NULL) == -1) {
			return -1;
		}
	}

	return close(fd);
}

int ioMonitorPipeC(int main_fd, int pipe_fd) {
	// Make the pipe non-blocking.
	int flags = fcntl(pipe_fd, F_GETFL, 0);
	fcntl(pipe_fd, F_SETFL, flags | O_NONBLOCK);

	int err = allocWriteQueue(pipe_fd);
	if (err) {
		return -1;
	}
	return ioMonitorFd(main_fd, pipe_fd, ReadEventPipe);
}

int windowsCreatePipeC(int* p_read_handle, int* p_write_handle) {
	*p_read_handle = 0;
	*p_write_handle = 0;
	return 0;
}

int windowsIncPacketsToSendC(int fd, int num_packets) {
	return 0;
}

int windowsAnyPacketsC (int fd, int* anyPackets) {
	*anyPackets = 1;
	return 0;
}

void windowsReadSockC(int fd, int *p_err_code) {
	*p_err_code = 0;
}

void windowsReadPipeC(int fd, int *p_err_code) {
	*p_err_code = 0;
}

io_data_t* initIOData(int fd, int op) {
	io_data_t *io_data = (io_data_t*) malloc(sizeof(io_data_t));
	if (io_data == NULL) {
		return NULL;
	}
	io_data->fd = fd;
	io_data->op = op;
	return io_data;
}
