#include <mqueue.h>

#define DATA_QUEUE "/chat_overlay_data"
#define PERMS (S_IRUSR | S_IWUSR)

#define VIDEO_FPS 25
#define VIDEO_WIDTH 1280
#define VIDEO_HEIGHT 720
#define BITRATE 10000000
#define MAX_SEGMENTS 10

static struct mq_attr data_attr;

typedef enum {CALLBACK, FPS} MESSAGE_TYPE;

typedef struct {
    MESSAGE_TYPE type;
    void (*callback)(void*);
    void* args;
} message_t;

#define MSGSIZE (sizeof(message_t)>128 ? sizeof(message_t) : 128)

typedef struct {
    unsigned char alpha;
    unsigned short int x;
    unsigned short int y;
    unsigned char yuv[3];
} pixel_t;

#define DATALEN (VIDEO_WIDTH*VIDEO_HEIGHT)
#define DATASIZE (VIDEO_WIDTH*VIDEO_HEIGHT*sizeof(pixel_t))