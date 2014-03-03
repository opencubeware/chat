#include <stdio.h>
#include <string.h>
#include <cairo/cairo.h>
#include <sys/stat.h>

#include "chat.h"
#include "erl_nif.h"

typedef struct {
    unsigned int x;
    unsigned int y;
    unsigned char alpha;
    cairo_surface_t* surface;
} segment_t;

// structs for carrying arguments for worker thread callbacks
typedef struct {
    ErlNifPid pid;
    int width;
    int height;
} new_args;

typedef struct {
    ErlNifPid pid;
    char file[256];
    int x;
    int y;
    double alpha;
} add_logo_args;

typedef struct {
    ErlNifPid pid;
    int segment;
} delete_segment_args;

// "global" variables
static ERL_NIF_TERM OK;
static ERL_NIF_TERM ERROR;
static ErlNifTid tid;
static mqd_t writer;
static mqd_t reader;
static mqd_t data_writer;
static struct mq_attr attr;
static ErlNifEnv *local_env;
static segment_t* segments[MAX_SEGMENTS];
static int next_segment;
static pixel_t *pixels;

// NIF callbacks
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

// NIFs
static ERL_NIF_TERM add_logo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM delete_segment(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ErlNifFunc nif_funcs[] =
{
    {"add_logo", 4, add_logo},
    {"delete_segment", 1, delete_segment}
};

// functions that are called in the context of a worker thread
// only these are supposed to modify state fields
static void* worker_loop(void*);
static void do_add_logo(void*);
static void do_delete_segment(void*);
static void send_data(ErlNifEnv*, ErlNifPid*, ERL_NIF_TERM);