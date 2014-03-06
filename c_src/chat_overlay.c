#include "chat_overlay.h"

#include <errno.h>

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    // build local environment
    local_env = enif_alloc_env();
    
    // clear segments counter
    next_segment = 0;
    
    // alloc pixel buffer (it has to be on the heap)
    pixels = (pixel_t*)(enif_alloc(sizeof(pixel_t)*DATALEN));
    
    //open POSIX message queue to interoperate with the worker thread
    attr.mq_msgsize = MSGSIZE;
    attr.mq_flags = 0;
    attr.mq_maxmsg = 10;
    attr.mq_curmsgs = 0;
    
    data_attr.mq_msgsize = DATASIZE;
    data_attr.mq_flags = 0;
    data_attr.mq_maxmsg = 2;
    data_attr.mq_curmsgs = 0;
    
    writer = mq_open(WORKER_QUEUE, O_CREAT | O_WRONLY | O_NONBLOCK, PERMS, &attr);
    reader = mq_open(WORKER_QUEUE, O_RDONLY);
    data_writer = mq_open(DATA_QUEUE, O_CREAT | O_WRONLY | O_NONBLOCK, PERMS, &data_attr);
    
    if(writer == -1 || reader == -1 || data_writer == -1) {
        perror("chat_overlay NIF loading error: ");
        return 1;
    }
    
    //start worker thread
    enif_thread_create("chat_overlay_worker", &tid, worker_loop, &reader, NULL);
    
    OK = enif_make_atom(env, "ok");
    ERROR = enif_make_atom(env, "error");
              
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
    // terminate worker thread
    message_t msg = {CALLBACK, enif_thread_exit, NULL};
    mq_send(writer, (char*)&msg, sizeof(message_t), 0);
    enif_thread_join(tid, NULL);
    
    enif_free_env(local_env);
    enif_free(pixels);
    mq_close(writer);
    mq_close(reader);
    mq_close(data_writer);
}

static ERL_NIF_TERM get_segments(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int n=0, i;
    ERL_NIF_TERM term_segments[MAX_SEGMENTS];
    enif_self(env, &owner);
    for(i=0; i<MAX_SEGMENTS; i++) {
        if(segments[i] != NULL) {
            ERL_NIF_TERM atom = enif_make_atom(env, segments[i]->id);
            term_segments[n++] = atom;
        }
    }
    return enif_make_list_from_array(env, term_segments, n);
}

static ERL_NIF_TERM flush_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifPid pid;
    enif_self(env, &pid);

    flush_buffer_args* args = (flush_buffer_args*)enif_alloc(sizeof(flush_buffer_args));
    args->pid = pid;
    message_t msg = {CALLBACK, do_flush_buffer, args};
    if(mq_send(writer, (char*)&msg, sizeof(message_t), 0)) {
        return ERROR;
    }
    else {
        return OK;
    }
}

static ERL_NIF_TERM add_logo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifPid pid;
    char id[256];
    char file[256];
    int x;
    int y;
    double alpha;
    
    enif_self(env, &pid);
    if(!enif_get_atom(env, argv[0], id, 256, ERL_NIF_LATIN1) ||
       enif_get_string(env, argv[1], file, 256, ERL_NIF_LATIN1) <= 0 ||
       !enif_get_int(env, argv[2], &x) ||
       !enif_get_int(env, argv[3], &y) ||
       !enif_get_double(env, argv[4], &alpha)) {
        return enif_make_badarg(env);
    }
    
    add_logo_args* args = (add_logo_args*)enif_alloc(sizeof(add_logo_args));
    args->pid = pid;
    strcpy(args->id, id);
    strcpy(args->file, file);
    args->x = x;
    args->y = y;
    args->alpha = alpha;
    message_t msg = {CALLBACK, do_add_logo, args};
    if(mq_send(writer, (char*)&msg, sizeof(message_t), 0)) {
        return ERROR;
    }
    else {
        return OK;
    }
}

static ERL_NIF_TERM add_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifPid pid;
    int x;
    int y;

    enif_self(env, &pid);
    if(!enif_get_int(env, argv[0], &x) ||
       !enif_get_int(env, argv[1], &y)) {
        return enif_make_badarg(env);
    }

    add_time_args* args = (add_time_args*)enif_alloc(sizeof(add_time_args));
    args->pid = pid;
    args->x = x;
    args->y = y;
    message_t msg = {CALLBACK, do_add_time, args};
    if(mq_send(writer, (char*)&msg, sizeof(message_t), 0)) {
        return ERROR;
    }
    else {
        return OK;
    }

}

static ERL_NIF_TERM delete_segment(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifPid pid;
    char id[256];
    enif_self(env, &pid);
    
    if(!enif_get_atom(env, argv[0], id, 256, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    delete_segment_args* args = (delete_segment_args*)enif_alloc(sizeof(delete_segment_args));
    args->pid = pid;
    strcpy(args->id, id);
    message_t msg = {CALLBACK, do_delete_segment, args};
    if(mq_send(writer, (char*)&msg, sizeof(message_t), 0)) {
        return ERROR;
    }
    else {
        return OK;
    }
}

static void* worker_loop(void* args) {
    mqd_t reader = *((mqd_t*)args);
    message_t* msg = (message_t*)enif_alloc(MSGSIZE);
    while(1) {
        if(mq_receive(reader, (char*)msg, MSGSIZE, NULL) > 0) {
            if(msg->type == CALLBACK) {
                (*(msg->callback))(msg->args);
                enif_free(msg->args);
            }
            else if(msg->type == FPS) {
                ERL_NIF_TERM fps = enif_make_double(local_env, (double)msg->fps);
                ERL_NIF_TERM atom = enif_make_atom(local_env, "fps");
                ERL_NIF_TERM tuple = enif_make_tuple(local_env, 2, atom, fps);
                enif_send(NULL, &owner, local_env, tuple);
                enif_clear_env(local_env);
            }
        }
    }
    enif_free(msg);
    return NULL;
}

static void do_flush_buffer(void* args) {
    flush_buffer_args* a = (flush_buffer_args*)args;
    send_data(local_env, &a->pid, OK);
}

static void do_add_logo(void* args) {
    add_logo_args* a = (add_logo_args*)args;
    cairo_surface_t *logo = cairo_image_surface_create_from_png(a->file);
    
    if(cairo_surface_status(logo) == CAIRO_STATUS_SUCCESS) {
        segment_t* new_segment = enif_alloc(sizeof(segment_t));
        strcpy(new_segment->id, a->id);
        new_segment->x = a->x;
        new_segment->y = a->y;
        new_segment->alpha = a->alpha*255;
        new_segment->surface = logo;
        add_segment(new_segment, a->pid, 1);
    }
    else {
        enif_send(NULL, &a->pid, local_env, ERROR);
        enif_clear_env(local_env);
    }
}

static void do_add_time(void* args) {
    add_time_args* a = (add_time_args*)args;

    time_t rawtime;
    time(&rawtime);
    char* timestr = ctime(&rawtime);
    char* newline = strchr(timestr, '\n');
    if(newline != NULL) {
        strncpy(newline, "", 1);
    }

    cairo_surface_t* surface;
    cairo_t* context;

    surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 500, 50);
    context = cairo_create(surface);

    // add datetime caption
    cairo_move_to(context, 20, 35);
    cairo_set_source_rgba(context, 1.0, 0, 0, 1.0);
    cairo_select_font_face(context, "Sans", CAIRO_FONT_SLANT_NORMAL,
                           CAIRO_FONT_WEIGHT_BOLD);
    cairo_set_font_size(context, 30.0);
    cairo_text_path(context, timestr);
    cairo_fill_preserve(context);
    // add stroke
    cairo_set_source_rgba(context, 0, 0, 0, 1.0);
    cairo_set_line_width(context, 1);
    cairo_stroke(context);

    // create segment
    segment_t* segment = (segment_t*)enif_alloc(sizeof(segment_t));
    strcpy(segment->id, "time");
    segment->x = a->x;
    segment->y = a->y;
    segment->alpha = 255;
    segment->surface = surface;

    // add segment, actually
    add_segment(segment, a->pid, 0);

    // cleanup cairo context
    cairo_destroy(context);
}

static void add_segment(segment_t* segment, ErlNifPid pid, int unique) {
    int segment_idx = next_segment, i;

    // check if segment doesn't exist already and have to be explicitly removed
    // or replaced by the given segment
    for(i=0; i<MAX_SEGMENTS; i++) {
        if(segments[i] != NULL && !strcmp(segments[i]->id, segment->id)) {
            if(unique) {
                ERL_NIF_TERM atom = enif_make_atom(local_env, "already_exists");
                ERL_NIF_TERM tuple = enif_make_tuple(local_env, 2, ERROR, atom);
                enif_send(NULL, &pid, local_env, tuple);
                enif_clear_env(local_env);
                return;
            }
            else {
                cairo_surface_destroy(segments[i]->surface);
                enif_free(segments[i]);
                segment_idx = i;
            }
        }
    }

    // if there are no free segments
    if(segment_idx == -1) {
        ERL_NIF_TERM atom = enif_make_atom(local_env, "no_free_segments");
        ERL_NIF_TERM tuple = enif_make_tuple(local_env, 2, ERROR, atom);
        enif_send(NULL, &pid, local_env, tuple);
        enif_clear_env(local_env);
        return;
    }

    segments[segment_idx] = segment;
    next_segment = -1;
    for(i=0; i<MAX_SEGMENTS; i++) {
        if(segments[i] == NULL) {
            next_segment = i;
            break;
        }
    }
    ERL_NIF_TERM atom = enif_make_atom(local_env, segment->id);
    ERL_NIF_TERM tuple = enif_make_tuple(local_env, 2, OK, atom);
    cairo_surface_flush(segment->surface);
    send_data(local_env, &pid, tuple);
}


static void do_delete_segment(void* args) {
    delete_segment_args* a = (delete_segment_args*)args;

    segment_t* segment = NULL;
    int i, index; 
    for(i=0; i<MAX_SEGMENTS; i++) {
        if(segments[i] != NULL && !strcmp(segments[i]->id, a->id)) {
            segment = segments[i];
            index = i;
            break;
        }
    }
    
    if(segment == NULL) {
        ERL_NIF_TERM atom = enif_make_atom(local_env, "no_such_segment");
        ERL_NIF_TERM tuple = enif_make_tuple(local_env, 2, ERROR, atom);
        enif_send(NULL, &a->pid, local_env, tuple);
        enif_clear_env(local_env);
    }
    else {
        segments[index] = NULL;
        cairo_surface_destroy(segment->surface);
        enif_free(segment);
        send_data(local_env, &a->pid, OK);
    }
}

static void send_data(ErlNifEnv *env, ErlNifPid *pid, ERL_NIF_TERM term) {
    unsigned int x,y,p=0,i;

    for(i=0; i<MAX_SEGMENTS; i++) {
        segment_t* segment = segments[i];
        if(segment != NULL && p < DATALEN) {
            int width = cairo_image_surface_get_width(segment->surface);
            int height = cairo_image_surface_get_height(segment->surface);
            unsigned char* data = cairo_image_surface_get_data(segment->surface);
            for(x=0; x<width; x++) {
                for(y=0; y<height; y++) {
                    unsigned int start = (y*width+x)*4;
                    // armv6 is little-endian!
                    if(data[start+3] > 0) {
                        float root_alpha = data[start+3]/255.;
                        unsigned char r = data[start+2];
                        unsigned char g = data[start+1];
                        unsigned char b = data[start];
                        unsigned char ch_y = (unsigned char)(0.299*r+0.587*g+0.114*b);
                        unsigned char ch_v = (unsigned char)(0.500*r-0.419*g-0.081*b)+128;
                        unsigned char ch_u = (unsigned char)(-0.169*r-0.331*g-0.500*b)+128;
                        pixel_t pixel;
                        pixel.alpha = segment->alpha*root_alpha;
                        pixel.x = x+segment->x;
                        pixel.y = y+segment->y;
                        pixel.yuv[0] = ch_y;
                        pixel.yuv[1] = ch_u;
                        pixel.yuv[2] = ch_v;
                        pixels[p++] = pixel;
                    }
                }
            }
        }
    }
    if(mq_send(data_writer, (char*)pixels, p*sizeof(pixel_t), 0) == 0) {
        enif_send(NULL, pid, env, term);
    }
    else {
        enif_send(NULL, pid, env, ERROR);
    }
    enif_clear_env(env);
}

ERL_NIF_INIT(chat_overlay, nif_funcs, load, NULL, NULL, unload);
