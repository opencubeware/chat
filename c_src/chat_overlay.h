#include <stdio.h>
#include <string.h>
#include <cairo/cairo.h>
#include <mqueue.h>
#include <sys/stat.h>

#include "erl_nif.h"

#define PERMS (S_IRUSR | S_IWUSR)

typedef struct {
    cairo_surface_t* surface;
    cairo_t* context;
} state;

typedef struct {
    void (*callback)(void*);
    void* args;
} message;

#define MSGSIZE (sizeof(message)>128 ? sizeof(message) : 128)

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
    char file[256];
} save_args;

// "global" variables
static state s;
static ERL_NIF_TERM OK;
static ERL_NIF_TERM ERROR;
static ErlNifTid tid;
static mqd_t writer;
static mqd_t reader;
static struct mq_attr attr;
static ErlNifEnv *local_env;

// NIF callbacks
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

// NIFs
static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM add_logo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM save(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ErlNifFunc nif_funcs[] =
{
    {"new", 2, new},
    {"add_logo", 4, add_logo},
    {"save", 1, save}
};

// functions that are called in the context of a worker thread
// only these are supposed to modify state fields
static void* worker_loop(void*);
static void do_new(void*);
static void do_add_logo(void*);
static void do_save(void*);

