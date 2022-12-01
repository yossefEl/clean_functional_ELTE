
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "Clean.h"
#include "cTCP_121.h"

/*
THE FUNCTION PROTOTYPES

Naming convention: functions, which are called from Clean end with a "C". Function's that

GLOBAL VARIABLES

The inetEventQueue: To append easily a new item to the end of the list, "toLastNext" points
to the "next" field of the last list element (or to &inetEventQueue)
*/

dictitem	*endpointDict = NULL;

int			tcpStartedUp = 0;
int			rcvBuffSize;	/* contains the size of the sockets internal buffer for receiving data. */
CleanString	pRcvBuff;

#if 0
extern	void (*exit_tcpip_function)();	/* the function will be called, when the Clean
										   program terminates */
#endif

/* FUNCTION IMPLEMENTATION */

int tcpPossibleC (void)
{
	StartUp (0);
	return tcpStartedUp;
}

int os_eom (SOCKET endpointRef)
{
	int err;
	char dummyBuffer[1];
	dictitem *pDictitem;

	err = recv (endpointRef, dummyBuffer, 1, MSG_PEEK);
	if (err>0)
		return 0;
	if (err<0 && (errno==EWOULDBLOCK || errno==EAGAIN))
		return 0;
	pDictitem = lookup (endpointRef);
	if (pDictitem->availByteValid)
		return 0;
	if (err==0)
		return 1;
	return 1;
}

int os_disconnected (SOCKET endpointRef)
{
	int err;
	char string[1];
	dictitem *pDictitem;

	pDictitem = lookup (endpointRef);
	if (pDictitem->disconnected)
		return 1;
	err	= send (endpointRef, string, 0, 0);
	if (err!=-1)
		return 0;
	else
		return errno!=EWOULDBLOCK;
				/* only this error can happen with sockets that can still send */
}

int os_connectrequestavailable (SOCKET endpointRef)
{
	fd_set readSet;
	struct timeval timeout;
	int		nr;

	FD_ZERO (&readSet);
	FD_SET (endpointRef,&readSet);

	timeout.tv_sec	= 0;			/* Timeout in sec's */
	timeout.tv_usec	= 0;			/* Timeout in microsec's */
	nr = select ((int)endpointRef+1,&readSet,NULL,NULL,&timeout);
	return nr>0 && FD_ISSET(endpointRef,&readSet);
}

void StartUp (int abort)
{
	if (!tcpStartedUp){
		socklen_t four=4;
		SOCKET s;

		/* initialize rcvBuffSize */
		s = socket (PF_INET, SOCK_STREAM, 0);
		if (s==-1){
			fprintf (stderr,"ERROR: can't create a socket, program aborts\n");
			exit (1);
		}
		if (getsockopt (s, SOL_SOCKET, SO_RCVBUF, (void*) &rcvBuffSize, &four)){
			fprintf (stderr,"ERROR: can't call getsockopt, program aborts\n");
			exit (1);
		}

		pRcvBuff = (CleanString) malloc (rcvBuffSize+sizeof (long));
		if (pRcvBuff==NULL) {
			fprintf (stderr,"ERROR: out of memory, program aborts\n");
			exit (1);
		}
#if 0
		exit_tcpip_function	= CleanUp;
#endif
		tcpStartedUp	= 1;
	}
}

void lookupHost_syncC (CleanString inetAddr, int *errCode, int *ipAddrP)
	/* error code: 0 ok, 1 error (also: addr doesn't exist) */
{
	struct hostent *hostentP;
	in_addr_t ipAddr;
	
	StartUp (1);
	ipAddr	= inet_addr (CleanStringCharacters(inetAddr));
	if (ipAddr!=INADDR_NONE){
		*errCode	= 0;
		*ipAddrP	= ntohl(ipAddr);
		return;
	}
	*errCode = 1;
	hostentP = gethostbyname (CleanStringCharacters (inetAddr));	/* string is alphanumerical */
	if (hostentP!=NULL){
		*ipAddrP = ntohl (*((uint32_t*)*hostentP->h_addr_list));	
		if (*ipAddrP!=0)
		    *errCode = 0;
	}
}

void openTCP_ListenerC (int portNum, int *pErrCode, SOCKET *pEndpointRef)
	/* errCode: 0:ok;	otherwise:not ok */
{
	SOCKET s;
	struct sockaddr_in srvAdr;

	StartUp (1);

	*pErrCode = 1;
	
	s = socket (PF_INET, SOCK_STREAM, 0);
	if (s==-1)
	  return;

	srvAdr.sin_family = AF_INET;			/* of course internet adress family */
	srvAdr.sin_addr.s_addr = INADDR_ANY;	/* internet address will be given after "accept" */
	srvAdr.sin_port = htons((short int)portNum);
	
    int so_reuseaddr = 1;
	*pErrCode = setsockopt (s, SOL_SOCKET, SO_REUSEADDR, &so_reuseaddr, sizeof so_reuseaddr);
	if (*pErrCode){
		close (s);
		return;
	}

	*pErrCode = bind (s, (struct sockaddr*) &srvAdr, sizeof(srvAdr));
	if (*pErrCode){
		close (s);
		return;
	}

	*pErrCode = listen (s,5);
	if (*pErrCode){
		close (s);
		return;
	}
	*pEndpointRef = s;

	*pErrCode	= insertNewDictionaryItem (s);
	if (*pErrCode)
		return;
	
	setEndpointData_no_new_notifiersC (s, 1,0,0,0);
}

void acceptC (SOCKET listener, int *pErrCode, int *pInetHost, SOCKET *pEndpointRef)
	/* errCode: 0:ok;	otherwise:not ok */
{
	SOCKET		endpointRef;
	struct sockaddr_in clientAdr;
	socklen_t clientAdrSize;
	int tru;
	
	clientAdrSize	= sizeof(clientAdr);
	endpointRef		= accept (listener,(struct sockaddr*) &clientAdr, &clientAdrSize);
	tru				= 1;
	ioctl (endpointRef, FIONBIO, &tru);	/* set mode to non blocking */
	*pErrCode		= endpointRef==-1;
	if (*pErrCode)
		return;

	*pInetHost		= ntohl (clientAdr.sin_addr.s_addr);
	*pEndpointRef	= endpointRef;

	*pErrCode		= insertNewDictionaryItem (endpointRef);
	if (*pErrCode)
		return;

	setEndpointData_no_new_notifiersC (endpointRef,2,0,0,0);
}

void os_connectTCP_syncC (int onlyForMac, int doTimeout, unsigned int stopTime,
					 int ipAddr, int portnum,
					 int *errCodeP, int *timeoutExpiredP, size_t *endpointRefP)
	/* errCode: 0 ok;	1 not ok */
{
	SOCKET client;
	struct sockaddr_in srvAdr,clientAdr;
	int err, tru;

	*errCodeP = 1;
	*timeoutExpiredP	= 0;
		
	client = socket (PF_INET, SOCK_STREAM, 0);
	if (client==-1)
		return;

	clientAdr.sin_family = AF_INET;			/* of course internet adress family */
	clientAdr.sin_addr.s_addr = INADDR_ANY;	/* internet adress will be given after "connect" */
	clientAdr.sin_port = 0;					/* the winsock library will choose a free number between 1024 and 5000 */
	
	err = bind (client, (struct sockaddr*) &clientAdr, sizeof(clientAdr));
	if (err){
		close (client);
		return;
	}

	srvAdr.sin_family = AF_INET;			/* of course internet adress family */
	srvAdr.sin_addr.s_addr = htonl (ipAddr);
	srvAdr.sin_port = htons ((short int)portnum);	
	
	tru				= 1;

	if (doTimeout){
		ioctl (client, FIONBIO, &tru);		/* set mode to non blocking */
		err = connect (client, (struct sockaddr*) &srvAdr, sizeof(srvAdr));
		if (!err){
			*errCodeP = 0;
			*timeoutExpiredP	= 0;
			*endpointRefP = client;
		} else if (!(errno==EINPROGRESS || errno==EWOULDBLOCK)){
			close (client);
			return;
		} else {
			fd_set writeSet, exptnSet;
			struct timeval timeout;
			unsigned int	now;
			int				noOfWritableSockets, timeoutTicks;

			FD_ZERO (&writeSet);
			FD_SET (client,&writeSet);
			FD_ZERO (&exptnSet);
			FD_SET (client,&exptnSet);

			{
				struct timeval tv;

				gettimeofday (&tv,NULL);
				now = tv.tv_sec*1000 + tv.tv_usec/1000;
			}

			timeoutTicks	= ((int)stopTime) - ((int)now);
			if (timeoutTicks<=0){							/* timeout expired */
				close (client);
				*timeoutExpiredP	= 1;
				return;
			}
			timeout.tv_sec	= timeoutTicks / 1000;			/* Timeout in sec's */
			timeout.tv_usec	= (timeoutTicks % 1000)*1000;	/* Timeout in microsec's */
			noOfWritableSockets = select ((int)client+1,NULL,&writeSet,&exptnSet,&timeout);

			int so_error;
			socklen_t len = sizeof so_error;
			getsockopt(client, SOL_SOCKET, SO_ERROR, &so_error, &len);

			if (so_error != 0) {
				*errCodeP = 1;
				*timeoutExpiredP = 0;
				close(client);
				return;
			}

			*errCodeP =		noOfWritableSockets<0
						||	(noOfWritableSockets>0 && FD_ISSET(client,&exptnSet));
			*timeoutExpiredP	= noOfWritableSockets==0;
			*endpointRefP		= client;
			if (*errCodeP || *timeoutExpiredP){
				close (client);
				return;
			}
		}
	}

	if (!doTimeout){
		err = connect (client, (struct sockaddr*) &srvAdr, sizeof(srvAdr));
		if (err){
			close (client);
			return;
		}

		ioctl (client, FIONBIO, &tru);		/* set mode to non blocking */

		*errCodeP = 0;
		*timeoutExpiredP	= 0;
		*endpointRefP = client;
	}

	*errCodeP	= insertNewDictionaryItem (client);
	if (*errCodeP){
		close (client);
		return;
	}

	setEndpointData_no_new_notifiersC (client,2,0,0,0);
}

void sendC (SOCKET endpointRef, CleanString data, int begin, int nBytes,
			int *pErrCode, int *pSentBytes)
{
	int sentBytes;

	*pErrCode	= 0;
	sentBytes = send (endpointRef, CleanStringCharacters(data)+begin,nBytes, 0);
	if (sentBytes==-1){
		sentBytes	= 0;
		if (errno!=EWOULDBLOCK){
			dictitem	*pDictitem;
			pDictitem	= lookup(endpointRef);
			pDictitem->disconnected	=1;
			*pErrCode	= 1;
		}
	}
	*pSentBytes	= sentBytes;
}

void receiveC (SOCKET endpointRef, int maxSize, CleanString *pReceived)
{
	int			size, received;
	dictitem	*pDictitem;

	*pReceived	= (CleanString) pRcvBuff;
	size		= maxSize<=0 ? rcvBuffSize : maxSize;

	received	= recv (endpointRef, CleanStringCharacters(pRcvBuff), size, 0);

	pDictitem	= lookup (endpointRef);
	if (received>0){
		pDictitem->availByteValid = 0;
		CleanStringLength(pRcvBuff)	= received;
	} else if (pDictitem->availByteValid){
		CleanStringCharacters(pRcvBuff)[0]	= pDictitem->availByte;
		pDictitem->availByteValid = 0;
		CleanStringLength(pRcvBuff) = 1;
	} else
		CleanStringLength(pRcvBuff) = 0;
}

int data_availableC (SOCKET endpointRef)
{
	dictitem	*pDictitem;
	int			err;

	pDictitem	= lookup(endpointRef);
	if (pDictitem->availByteValid)
		return 1;
	err	= recv (endpointRef, &pDictitem->availByte, 1, MSG_PEEK);
	if (err>0){
		pDictitem->availByteValid = 1;
		return 1;
	}
	return 0;
}

void disconnectGracefulC (SOCKET endpointRef)
{
	shutdown (endpointRef,1);	/* 1: graceful */
}

void disconnectBrutalC (SOCKET endpointRef)
{
	struct linger linger;

	linger.l_onoff	= 1;
	linger.l_linger	= 0;
	setsockopt (endpointRef, SOL_SOCKET, SO_LINGER, (char*) &linger, sizeof(linger));
}

void garbageCollectEndpointC (SOCKET endpointRef)
{
	dictitem	*pDictitem;

	pDictitem	= lookup (endpointRef);
	if (pDictitem!=NULL && pDictitem->referenceCount==0){
		close (endpointRef);
		removeDictionaryItem(endpointRef);
	}
}

void initFD_SET (fd_set *pSet, long sockets[], int n)
{
	int	i;

	FD_ZERO (pSet);

	for (i=0; i<n; i++)
		FD_SET(sockets[i], pSet);
}

void selectChC (int justForMac, int nonBlocking, int doTimeout, unsigned int stopTime, 
			    long *pRChannels, int *justForMac2, long *pSChannels,
				int *pErrCode)
	/* error code: 0=ok; 1=timeout expired, 3=other errors */
{
	int				nRChannels, nSChannels, i;
	fd_set readSet, writeSet;
	struct timeval timeout;
	unsigned int	now;
	int	n,timeoutTicks,max_fd;

	nRChannels	= CleanIntArraySize (pRChannels);
	nSChannels	= CleanIntArraySize (pSChannels);
	
	max_fd = -1;
	for (i=0; i<nRChannels; ++i){
		int fd;

		fd=pRChannels[i];
		max_fd = fd>max_fd ? fd : max_fd;
	}
	for (i=0; i<nSChannels; ++i){
		int fd;

		fd=pSChannels[i];
		max_fd = fd>max_fd ? fd : max_fd;
	}

	if (doTimeout){
		{
			struct timeval tv;

			gettimeofday (&tv,NULL);
			now = tv.tv_sec*1000 + tv.tv_usec/1000;
		}

		timeoutTicks	= nonBlocking ? 0 : ((int)stopTime) - ((int)now);
		if (timeoutTicks<0){
			*pErrCode	= 1;
			return;
		}
		timeout.tv_sec	= timeoutTicks / 1000;			/* Timeout in sec's */
		timeout.tv_usec	= (timeoutTicks % 1000)*1000;	/* Timeout in microsec's */
	}
	initFD_SET (&readSet,  pRChannels, nRChannels);
	initFD_SET (&writeSet, pSChannels, nSChannels);
	n = select (max_fd+1,&readSet,&writeSet,NULL, doTimeout ? &timeout : NULL);
	if (n==0){
		/* timeout expired */
		*pErrCode	= 1;
		return;
	}
	if (n<0){
		*pErrCode	= 3;
		return;
	}
	for(i=0; i<nRChannels; i++)
		if (FD_ISSET(pRChannels[i], &readSet))
			pRChannels[i]	= 0;
	for(i=0; i<nSChannels; i++)
		if (FD_ISSET(pSChannels[i], &writeSet))
			pSChannels[i]	= 0;
	*pErrCode	= 0;
}

/* this function is called from Cleans runtime system via *exit_tcpip_function */
void CleanUp (void)
{
	dictitem	*pDictitem, *pTemp;
	int			referenceCount;
	pDictitem	= endpointDict;
	while (pDictitem){
		referenceCount	= pDictitem->referenceCount;
		if (referenceCount!=0){
			if (referenceCount==1 && pDictitem->aborted){
				disconnectBrutalC(pDictitem->endpointRef);
			} else
				disconnectGracefulC(pDictitem->endpointRef);
			close (pDictitem->endpointRef);
		}
		pTemp	= pDictitem;
		pDictitem	= pDictitem->next;
		free ((char*) pTemp);
	}
}

/* FUNCTION IMPLEMENTATIONS FOR THE ENDPOINT DICTIONARY */

int insertNewDictionaryItem(SOCKET endpointRef)
{
	dictitem	*newItem			= (dictitem*) malloc (sizeof(dictitem));
	if (newItem==NULL)
		return 1;
		
	newItem->endpointRef			= endpointRef;
	newItem->next					= endpointDict;
	newItem->availByteValid			= 0;
	newItem->aborted				= 0;
	newItem->disconnected			= 0;
	endpointDict					= newItem;

	return 0;
}

dictitem* lookup(SOCKET endpointRef)
{
	dictitem	*ptr=endpointDict;
	while (ptr!=NULL && (ptr->endpointRef!=endpointRef))
		ptr	= ptr->next;
	
	return ptr;
}

void setEndpointData_no_new_notifiersC
	(SOCKET endpointRef, int referenceCount, int hasReceiveNotifier, int hasSendableNotifier, int aborted)
{
	/*
	use only if no notifiers are added, because ghMainWindow_p_for_tcp
	and PM_SOCKET_EVENT_for_tcp may not be initialized
	*/
	dictitem	*ptr			= lookup (endpointRef);
	
	if (ptr!=NULL){
		ptr->referenceCount			= referenceCount;
		ptr->hasReceiveNotifier		= hasReceiveNotifier ? 1 : 0;
		ptr->hasSendableNotifier	= hasSendableNotifier ? 1 : 0;
		ptr->aborted				= aborted ? 1 : 0;
	}
}

void getEndpointDataC (	SOCKET endpointRef, int *referenceCount,
						int *hasReceiveNotifier, int *hasSendableNotifier, int *aborted)
{
	dictitem	*ptr			= lookup (endpointRef);
	
	if (ptr!=NULL){
		*referenceCount			= ptr->referenceCount;
		*hasReceiveNotifier		= ptr->hasReceiveNotifier!=0;
		*hasSendableNotifier	= ptr->hasSendableNotifier!=0;
		*aborted				= ptr->aborted!=0;
	}
}

void removeDictionaryItem (SOCKET endpointRef)
	/* the dictionary MUST contain a valid item with the endpointRef */
{
	dictitem	**ptr, *temp;
	int			notRemoved;

	ptr	= &endpointDict;
	notRemoved	= 1;
	while (notRemoved)
		if ((*ptr)->endpointRef==endpointRef){
			temp	= *ptr;
			*ptr	= (*ptr)->next;
			free ((char*) temp);
			notRemoved	= 0;
		} else
			ptr	= &((*ptr)->next);
}

long GetTickCount (void)
{
	struct timeval tv;

	gettimeofday (&tv,NULL);
	return tv.tv_sec*1000l + tv.tv_usec/1000;
}
