#ifndef __CTCP__
#define __CTCP__

typedef int SOCKET;

/* the dictionary items */
struct dictitem
	{	SOCKET endpointRef;
		struct dictitem	*next;
		char			availByte;
		unsigned		availByteValid		: 1;
		unsigned		referenceCount		: 2;
		unsigned		hasReceiveNotifier	: 1;
			/* three kinds of receivers: receivers for established connections, */
			/* receivers for dns requests, receivers for asynchronous connect */
		unsigned		hasSendableNotifier	: 1;
		unsigned		aborted				: 1;
		unsigned		disconnected		: 1;
	};
typedef struct dictitem dictitem;

#define IE_CONNECTREQUEST		0x0001
#define IE_RECEIVED				0x0004
#define IE_EOM					0x0010
#define IE_SENDABLE				0x0100
#define IE_DISCONNECTED			0x0011
#define IE_IPADDRESSFOUND		0x2000000F
#define IE_IPADDRESSNOTFOUND	0x20000010
#define IE_ASYNCCONNECTCOMPLETE	0x0002
#define IE_ASYNCCONNECTFAILED	0x0003 

#define ListenerReceiver	0
#define RChanReceiver		1
#define SChanReceiver		2
#define DNSReceiver			3
#define ConnectReceiver		4

extern dictitem* lookup(SOCKET endpointRef);

/* functions, which are called from Clean (semantic is explained in tcp.icl or ostcp.icl) */

int os_eom (SOCKET endpointRef);
int os_disconnected (SOCKET endpointRef);
int os_connectrequestavailable (SOCKET endpointRef);
void abortedHost_syncC(CleanString inetAddr, int *errCode, int *ipAddr);
void abortedHost_asyncC(CleanString inetAddr, int *errCode, void *endpointRef);
void openTCP_ListenerC(int portNum, int *pErrCode, SOCKET *pEndpointRef);
void acceptC (SOCKET endpointRef, int *pErrCode, int *pInetHost, SOCKET *pEndpointRef);
void os_connectTCP_syncC (int onlyForMac, int doTimeout, unsigned int stopTime,
					 int ipAddr, int portnum,
					 int *errCodeP, int *timeoutExpiredP, size_t *endpointRefP);

void sendC (SOCKET endpointRef, CleanString data, int begin, int nBytes, int *pErrCode, int *pSentBytes);
void receiveC(SOCKET endpointRef, int maxSize, CleanString *data);
int getRcvBuffSizeC();
void resizeStringC(CleanString string, int newSize);
int data_availableC(SOCKET endpointRef);
void disconnectGracefulC (SOCKET endpointRef);
void disconnectBrutalC (SOCKET endpointRef);
void garbageCollectEndpointC (SOCKET endpointRef);
void os_select_inetevents (SOCKET endpointRef, int receiverCategory,
								 int referenceCount, int getReceiveEvents, int getSendEvents,
								 int aborted
								);
void selectChC (int justForMac, int nonBlocking, int doTimeout, unsigned int stopTime, 
					  long *pRChannels, int *justForMac2, long *pSChannels,
					  int *pErrCode
					 );
int tcpPossibleC (void);

/* other functions */

void StartUp(int abort);
/*
	initialize winsock (if not started yet. uses global "tcpStartedUp")
	if succesful: tcpStartedUp==TRUE afterwards
	if not succesful && abort: aborts
	if not succesful && !abort: tcpStartedUp==FALSE afterwards
*/
void CleanUp(void);

/* functions to deal with the endpoint dictionary: */

int insertNewDictionaryItem(SOCKET endpointRef);
	/* allocates memory for new dictionary item, initializes it as far as possible and */
	/* adds it to the dictionary. returns error code: 0==ok, 1==not ok */
dictitem* lookup(SOCKET endpointRef);
	/* lookup entry (CFN) */
void setEndpointData_no_new_notifiersC (SOCKET endpointRef, int referenceCount,
							 int hasReceiveNotifier, int hasSendableNotifier, int aborted);
	/* set the corresponding fields of the entry */
void getEndpointDataC (SOCKET endpointRef, int *referenceCount,
							 int *hasReceiveNotifier, int *hasSendableNotifier, int *aborted);
	/* returns the corresponding fields of the entry */
void removeDictionaryItem(SOCKET endpointRef);
	/* remove one item via pointer manipulations (must not be called from notifier) */

#endif
