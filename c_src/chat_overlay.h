#include <stdio.h>
#include <string.h>
#include <cairo/cairo.h>
#include <sys/stat.h>
#include <time.h>

#include "chat.h"
#include "erl_nif.h"

typedef struct {
    char id[128];
    unsigned int x;
    unsigned int y;
    unsigned char alpha;
    cairo_surface_t* surface;
} segment_t;

// structs for carrying arguments for worker thread callbacks
typedef struct {
    ErlNifPid pid;
} flush_buffer_args;

typedef struct {
    ErlNifPid pid;
    char id[128];
    char file[256];
    int x;
    int y;
    double alpha;
} add_logo_args;

typedef struct {
    ErlNifPid pid;
    char event[64];
    char round[64];
    char competition[64];
    char year[64];
    char info[128];
} add_info_args;

typedef struct {
    ErlNifPid pid;
    int x;
    int y;
} add_time_args;

typedef struct {
    ErlNifPid pid;
    char id[128];
} delete_segment_args;

// "global" variables
static ERL_NIF_TERM OK;
static ERL_NIF_TERM ERROR;

static ErlNifTid tid;
static mqd_t writer;
static mqd_t reader;
static mqd_t data_writer;
static struct mq_attr attr, data_attr;
static ErlNifEnv *local_env;
static segment_t* segments[MAX_SEGMENTS];
static int next_segment;
static pixel_t *pixels;
static ErlNifPid owner;

// NIF callbacks
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

// NIFs
static ERL_NIF_TERM get_segments(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM flush_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM add_logo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM add_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM add_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM delete_segment(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"get_segments_nif", 0, get_segments},
    {"flush_buffer_nif", 0, flush_buffer},
    {"add_logo_nif", 5, add_logo},
    {"add_info_nif", 3, add_info},
    {"add_time_nif", 2, add_time},
    {"delete_segment_nif", 1, delete_segment}
};

// functions that are called in the context of a worker thread
// only these are supposed to modify state fields
static void* worker_loop(void*);
static void do_flush_buffer(void*);
static void do_add_logo(void*);
static void do_add_info(void*);
static void do_add_time(void*);
static void do_delete_segment(void*);
static void add_segment(segment_t*, ErlNifPid, int);
static void send_data(ErlNifEnv*, ErlNifPid*, ERL_NIF_TERM);
