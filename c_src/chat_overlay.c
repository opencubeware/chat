#include "chat_overlay.h"

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    // build local environment
    local_env = enif_alloc_env();
    
    //open POSIX message queue to interoperate with the worker thread
    const char* queue_name = "/chat_overlay_worker";
    mq_unlink(queue_name);

    attr.mq_msgsize = MSGSIZE;
    attr.mq_flags = 0;
    attr.mq_maxmsg = 10;
    attr.mq_curmsgs = 0;
    
    writer = mq_open(queue_name, O_CREAT | O_WRONLY, PERMS, &attr);
    reader = mq_open(queue_name, O_RDONLY);
    if(writer == -1 || reader == -1) {
        return 1;
    }
    
    //start worker thread
    enif_thread_create("chat_overlay_worker", &tid, worker_loop, &reader, NULL);
    
    OK = enif_make_atom(env, "ok");
    ERROR = enif_make_atom(env, "error");
    return 0;
}

static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifPid pid;
    int width;
    int height;
    
    if(!enif_get_local_pid(env, argv[0], &pid) ||
       !enif_get_int(env, argv[1], &width) ||
       !enif_get_int(env, argv[2], &height)) {
        return enif_make_badarg(env);
    }
    
    new_args* args = (new_args*)enif_alloc(sizeof(new_args));
    args->env = env;
    args->pid = pid;
    args->width = width;
    args->height = height;
    message msg = {do_new, args};
    if(mq_send(writer, (char*)&msg, sizeof(message), 0)) {
        return ERROR;
    }
    else {
        return OK;
    }
}

static ERL_NIF_TERM add_logo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char file[256];
    int x;
    int y;
    double alpha;
    
    if(s.context == NULL || cairo_status(s.context) != CAIRO_STATUS_SUCCESS) {
        return ERROR;
    }
    
    if(enif_get_string(env, argv[0], file, 256, ERL_NIF_LATIN1) <= 0 ||
       !enif_get_int(env, argv[1], &x) ||
       !enif_get_int(env, argv[2], &y) ||
       !enif_get_double(env, argv[3], &alpha)) {
        return enif_make_badarg(env);
    }
    
    add_logo_args* args = (add_logo_args*)enif_alloc(sizeof(add_logo_args));
    strcpy(args->file, file);
    args->x = x;
    args->y = y;
    args->alpha = alpha;
    message msg = {do_add_logo, args};
    if(mq_send(writer, (char*)&msg, sizeof(message), 0)) {
        return ERROR;
    }
    else {
        return OK;
    }
}

static ERL_NIF_TERM save(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char file[256];
    
    if(s.context == NULL || cairo_status(s.context) != CAIRO_STATUS_SUCCESS) {
        return ERROR;
    }
    
    if(enif_get_string(env, argv[0], file, 256, ERL_NIF_LATIN1) <= 0) {
        return enif_make_badarg(env);
    }
    
    save_args* args = (save_args*)enif_alloc(sizeof(save_args));
    strcpy(args->file, file);
    message msg = {do_save, args};
    if(mq_send(writer, (char*)&msg, sizeof(message), 0)) {
        return ERROR;
    }
    else {
        return OK;
    }
}

static void* worker_loop(void* args) {
    mqd_t reader = *((mqd_t*)args);
    message* msg = (message*)enif_alloc(MSGSIZE);
    while(1) {
        if(mq_receive(reader, (char*)msg, MSGSIZE, NULL) > 0) {
            (*(msg->callback))(msg->args);
            enif_free(msg->args);
        }
    }
    enif_free(msg);
    return NULL;
}

static void do_new(void* args) {
    new_args* a = (new_args*)args;
    s.surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, a->width, a->height);
    s.context = cairo_create(s.surface);
    enif_send(a->env, &a->pid, local_env, OK);
}

static void do_add_logo(void* args) {
    add_logo_args* a = (add_logo_args*)args;
    cairo_surface_t *logo = cairo_image_surface_create_from_png(a->file);
    if(cairo_surface_status(logo) == CAIRO_STATUS_SUCCESS) {
        cairo_set_source_surface(s.context, logo, a->x, a->y);
        cairo_paint_with_alpha(s.context, a->alpha);
        cairo_surface_destroy(logo);
    }
}

static void do_save(void* args) {
    save_args* a = (save_args*)args;
    cairo_surface_write_to_png(s.surface, a->file);
}

ERL_NIF_INIT(chat_overlay, nif_funcs, load, NULL, NULL, NULL);
