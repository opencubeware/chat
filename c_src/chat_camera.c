#include <stdio.h>
#include <stdlib.h>
#include <sys/select.h>

#include "bcm_host.h"
#include "interface/vcos/vcos.h"

#include "interface/mmal/mmal.h"
#include "interface/mmal/util/mmal_util.h"
#include "interface/mmal/util/mmal_util_params.h"
#include "interface/mmal/util/mmal_default_components.h"
#include "interface/mmal/util/mmal_connection.h"

#include "chat.h"


#define MMAL_CAMERA_PREVIEW_PORT 0
#define MMAL_CAMERA_VIDEO_PORT 1
#define MMAL_CAMERA_CAPTURE_PORT 2

typedef struct {
    int width;
    int height;
    MMAL_COMPONENT_T *camera;
    MMAL_COMPONENT_T *encoder;
    MMAL_COMPONENT_T *preview;
    MMAL_PORT_T *camera_preview_port;
    MMAL_PORT_T *camera_video_port;
    MMAL_PORT_T *camera_still_port;
    MMAL_POOL_T *camera_video_port_pool;
    MMAL_PORT_T *encoder_input_port;
    MMAL_POOL_T *encoder_input_pool;
    MMAL_PORT_T *encoder_output_port;
    MMAL_POOL_T *encoder_output_pool;
} PORT_USERDATA;

static struct mq_attr attr, data_attr;
static mqd_t reader, writer;
static unsigned int pixels;
static pixel_t* pixel_buffer;

static void camera_video_buffer_callback(MMAL_PORT_T *port, MMAL_BUFFER_HEADER_T *buffer) {
    static unsigned char frames = 0;
    static struct timespec t1, t2;
    
    if(frames == 0) {
        clock_gettime(CLOCK_MONOTONIC, &t1);
    }

    MMAL_BUFFER_HEADER_T *new_buffer;
    MMAL_BUFFER_HEADER_T *output_buffer = 0;
    PORT_USERDATA *userdata = (PORT_USERDATA *) port->userdata;

    MMAL_POOL_T *pool = userdata->camera_video_port_pool;

    output_buffer = mmal_queue_get(userdata->encoder_input_pool->queue);

    int chrominance_offset = userdata->width * userdata->height;
    int offset = chrominance_offset / 4;

    if (output_buffer) {
        mmal_buffer_header_mem_lock(buffer);
        memcpy(output_buffer->data, buffer->data, buffer->length);
        
        int i;
        for(i=0;i<pixels;i++) {
            pixel_t pixel = pixel_buffer[i];
            int y = pixel.y * userdata->width + pixel.x;
            int u = (pixel.y/2) * (userdata->width/2) + (pixel.x/2) + chrominance_offset;
            int v = u + offset;
            
            // just in case we tried to overlay something outside the frame, ignore it
            if(y < buffer->length && u < buffer->length && v < buffer->length) {
                if(pixel.alpha == 255) {
                    output_buffer->data[y] = pixel.yuv[0];
                    output_buffer->data[u] = pixel.yuv[1];
                    output_buffer->data[v] = pixel.yuv[2];
                }
                else {
                    float alpha = pixel.alpha/255.;
                    output_buffer->data[y] = alpha*pixel.yuv[0]+(1-alpha)*output_buffer->data[y];
                    output_buffer->data[u] = alpha*pixel.yuv[1]+(1-alpha)*output_buffer->data[u];
                    output_buffer->data[v] = alpha*pixel.yuv[2]+(1-alpha)*output_buffer->data[v];
                }
            }
        }

        output_buffer->length = buffer->length;
        mmal_buffer_header_mem_unlock(buffer);
        if (mmal_port_send_buffer(userdata->encoder_input_port, output_buffer) != MMAL_SUCCESS) {
            //fprintf(stderr, "ERROR: Unable to send buffer \n");
        }
    } else {
        //fprintf(stderr, "ERROR: mmal_queue_get (%d)\n", output_buffer);
    }

    mmal_buffer_header_release(buffer);

    // and send one back to the port (if still open)
    if (port->is_enabled) {
        MMAL_STATUS_T status;

        new_buffer = mmal_queue_get(pool->queue);

        if (new_buffer) {
            status = mmal_port_send_buffer(port, new_buffer);
        }

        if (!new_buffer || status != MMAL_SUCCESS) {
            //fprintf(stderr, "Error: Unable to return a buffer to the video port\n");
        }
    }
    
    frames++;
    if(frames == 100) {
        clock_gettime(CLOCK_MONOTONIC, &t2);
        long ms = (t2.tv_sec*1000 + t2.tv_nsec/1000000) - (t1.tv_sec*1000 + t1.tv_nsec/1000000);
        float fpms = 100.0/ms;

        message_t msg;
        msg.type = FPS;
        msg.fps = fpms*1000;
        mq_send(writer, (char*)&msg, sizeof(message_t), 0);

        frames = 0;
        clock_gettime(CLOCK_MONOTONIC, &t1);
    }
}

static void encoder_input_buffer_callback(MMAL_PORT_T *port, MMAL_BUFFER_HEADER_T *buffer) {
    //fprintf(stderr, "INFO:%s\n", __func__);    
    mmal_buffer_header_release(buffer);
}

static void encoder_output_buffer_callback(MMAL_PORT_T *port, MMAL_BUFFER_HEADER_T *buffer) {
    MMAL_BUFFER_HEADER_T *new_buffer;
    PORT_USERDATA *userdata = (PORT_USERDATA *) port->userdata;
    MMAL_POOL_T *pool = userdata->encoder_output_pool;
    //fprintf(stderr, "INFO:%s\n", __func__);

    mmal_buffer_header_mem_lock(buffer);
    fwrite(buffer->data, 1, buffer->length, stdout);
    mmal_buffer_header_mem_unlock(buffer);

    mmal_buffer_header_release(buffer);
    if (port->is_enabled) {
        MMAL_STATUS_T status;

        new_buffer = mmal_queue_get(pool->queue);

        if (new_buffer) {
            status = mmal_port_send_buffer(port, new_buffer);
        }

        if (!new_buffer || status != MMAL_SUCCESS) {
            //fprintf(stderr, "Unable to return a buffer to the video port\n");
        }
    }
}

void fill_port_buffer(MMAL_PORT_T *port, MMAL_POOL_T *pool) {
    int q;
    int num = mmal_queue_length(pool->queue);

    for (q = 0; q < num; q++) {
        MMAL_BUFFER_HEADER_T *buffer = mmal_queue_get(pool->queue);
        if (!buffer) {
            //fprintf(stderr, "Unable to get a required buffer %d from pool queue\n", q);
        }

        if (mmal_port_send_buffer(port, buffer) != MMAL_SUCCESS) {
            //fprintf(stderr, "Unable to send a buffer to port (%d)\n", q);
        }
    }
}

int setup_camera(PORT_USERDATA *userdata) {
    MMAL_STATUS_T status;
    MMAL_COMPONENT_T *camera = 0;
    MMAL_ES_FORMAT_T *format;
    MMAL_PORT_T * camera_preview_port;
    MMAL_PORT_T * camera_video_port;
    //MMAL_PORT_T * camera_still_port;
    MMAL_POOL_T * camera_video_port_pool;

    status = mmal_component_create(MMAL_COMPONENT_DEFAULT_CAMERA, &camera);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: create camera %x\n", status);
        return -1;
    }
    userdata->camera = camera;
    userdata->camera_preview_port = camera->output[MMAL_CAMERA_PREVIEW_PORT];
    userdata->camera_video_port = camera->output[MMAL_CAMERA_VIDEO_PORT];
    userdata->camera_still_port = camera->output[MMAL_CAMERA_CAPTURE_PORT];

    camera_preview_port = camera->output[MMAL_CAMERA_PREVIEW_PORT];
    camera_video_port = camera->output[MMAL_CAMERA_VIDEO_PORT];
    //camera_still_port = camera->output[MMAL_CAMERA_CAPTURE_PORT];

    {
        MMAL_PARAMETER_CAMERA_CONFIG_T cam_config = {
            { MMAL_PARAMETER_CAMERA_CONFIG, sizeof (cam_config)},
            .max_stills_w = 1280,
            .max_stills_h = 720,
            .stills_yuv422 = 0,
            .one_shot_stills = 1,
            .max_preview_video_w = VIDEO_WIDTH,
            .max_preview_video_h = VIDEO_HEIGHT,
            .num_preview_video_frames = 3,
            .stills_capture_circular_buffer_height = 0,
            .fast_preview_resume = 0,
            .use_stc_timestamp = MMAL_PARAM_TIMESTAMP_MODE_RESET_STC
        };
        mmal_port_parameter_set(camera->control, &cam_config.hdr);
    }

    // Setup camera preview port format 
    format = camera_preview_port->format;
    format->encoding = MMAL_ENCODING_OPAQUE;
    format->encoding_variant = MMAL_ENCODING_I420;
    format->es->video.width = VIDEO_WIDTH;
    format->es->video.height = VIDEO_HEIGHT;
    format->es->video.crop.x = 0;
    format->es->video.crop.y = 0;
    format->es->video.crop.width = VIDEO_WIDTH;
    format->es->video.crop.height = VIDEO_HEIGHT;

    status = mmal_port_format_commit(camera_preview_port);

    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: camera viewfinder format couldn't be set\n");
        return -1;
    }

    // Setup camera video port format
    mmal_format_copy(camera_video_port->format, camera_preview_port->format);

    format = camera_video_port->format;
    format->encoding = MMAL_ENCODING_I420;
    format->encoding_variant = MMAL_ENCODING_I420;
    format->es->video.width = VIDEO_WIDTH;
    format->es->video.height = VIDEO_HEIGHT;
    format->es->video.crop.x = 0;
    format->es->video.crop.y = 0;
    format->es->video.crop.width = VIDEO_WIDTH;
    format->es->video.crop.height = VIDEO_HEIGHT;
    format->es->video.frame_rate.num = VIDEO_FPS;
    format->es->video.frame_rate.den = 1;

    camera_video_port->buffer_size = format->es->video.width * format->es->video.height * 12 / 8;
    camera_video_port->buffer_num = 2;

    //fprintf(stderr, "INFO:camera video buffer_size = %d\n", camera_video_port->buffer_size);
    //fprintf(stderr, "INFO:camera video buffer_num = %d\n", camera_video_port->buffer_num);

    status = mmal_port_format_commit(camera_video_port);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to commit camera video port format (%u)\n", status);
        return -1;
    }

    camera_video_port_pool = (MMAL_POOL_T *) mmal_port_pool_create(camera_video_port, camera_video_port->buffer_num, camera_video_port->buffer_size);
    userdata->camera_video_port_pool = camera_video_port_pool;
    camera_video_port->userdata = (struct MMAL_PORT_USERDATA_T *) userdata;


    status = mmal_port_enable(camera_video_port, camera_video_buffer_callback);

    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to enable camera video port (%u)\n", status);
        return -1;
    }

    status = mmal_component_enable(camera);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to enable camera (%u)\n", status);
        return -1;
    }


    fill_port_buffer(userdata->camera_video_port, userdata->camera_video_port_pool);


    MMAL_PARAMETER_MIRROR_T mirror = {{MMAL_PARAMETER_MIRROR, sizeof(MMAL_PARAMETER_MIRROR_T)}, MMAL_PARAM_MIRROR_NONE};
    mirror.value = MMAL_PARAM_MIRROR_BOTH;
    mmal_port_parameter_set(camera->output[0], &mirror.hdr);
    mmal_port_parameter_set(camera->output[1], &mirror.hdr);
    mmal_port_parameter_set(camera->output[2], &mirror.hdr);

    if (mmal_port_parameter_set_boolean(camera_video_port, MMAL_PARAMETER_CAPTURE, 1) != MMAL_SUCCESS) {
        //printf("%s: Failed to start capture\n", __func__);
    }

    //fprintf(stderr, "INFO: camera created\n");
    return 0;
}

int setup_encoder(PORT_USERDATA *userdata) {
    MMAL_STATUS_T status;
    MMAL_COMPONENT_T *encoder = 0;
    //MMAL_PORT_T *preview_input_port = NULL;

    MMAL_PORT_T *encoder_input_port = NULL, *encoder_output_port = NULL;
    MMAL_POOL_T *encoder_input_port_pool;
    MMAL_POOL_T *encoder_output_port_pool;

    status = mmal_component_create(MMAL_COMPONENT_DEFAULT_VIDEO_ENCODER, &encoder);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to create preview (%u)\n", status);
        return -1;
    }

    encoder_input_port = encoder->input[0];
    encoder_output_port = encoder->output[0];
    userdata->encoder_input_port = encoder_input_port;
    userdata->encoder_output_port = encoder_input_port;

    mmal_format_copy(encoder_input_port->format, userdata->camera_video_port->format);
    encoder_input_port->buffer_size = encoder_input_port->buffer_size_recommended;
    encoder_input_port->buffer_num = 2;


    mmal_format_copy(encoder_output_port->format, encoder_input_port->format);

    encoder_output_port->buffer_size = encoder_output_port->buffer_size_recommended;
    encoder_output_port->buffer_num = 2;
    // Commit the port changes to the input port 
    status = mmal_port_format_commit(encoder_input_port);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to commit encoder input port format (%u)\n", status);
        return -1;
    }

    // Only supporting H264 at the moment
    encoder_output_port->format->encoding = MMAL_ENCODING_H264;
    encoder_output_port->format->bitrate = BITRATE; 

    encoder_output_port->buffer_size = encoder_output_port->buffer_size_recommended;

    if (encoder_output_port->buffer_size < encoder_output_port->buffer_size_min) {
        encoder_output_port->buffer_size = encoder_output_port->buffer_size_min;
    }

    encoder_output_port->buffer_num = encoder_output_port->buffer_num_recommended;

    if (encoder_output_port->buffer_num < encoder_output_port->buffer_num_min) {
        encoder_output_port->buffer_num = encoder_output_port->buffer_num_min;
    }


    // Commit the port changes to the output port    
    status = mmal_port_format_commit(encoder_output_port);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to commit encoder output port format (%u)\n", status);
        return -1;
    }

    //fprintf(stderr, " encoder input buffer_size = %d\n", encoder_input_port->buffer_size);
    //fprintf(stderr, " encoder input buffer_num = %d\n", encoder_input_port->buffer_num);

    //fprintf(stderr, " encoder output buffer_size = %d\n", encoder_output_port->buffer_size);
    //fprintf(stderr, " encoder output buffer_num = %d\n", encoder_output_port->buffer_num);

    encoder_input_port_pool = (MMAL_POOL_T *) mmal_port_pool_create(encoder_input_port, encoder_input_port->buffer_num, encoder_input_port->buffer_size);
    userdata->encoder_input_pool = encoder_input_port_pool;
    encoder_input_port->userdata = (struct MMAL_PORT_USERDATA_T *) userdata;
    status = mmal_port_enable(encoder_input_port, encoder_input_buffer_callback);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to enable encoder input port (%u)\n", status);
        return -1;
    }
    //fprintf(stderr, "INFO:Encoder input pool has been created\n");


    encoder_output_port_pool = (MMAL_POOL_T *) mmal_port_pool_create(encoder_output_port, encoder_output_port->buffer_num, encoder_output_port->buffer_size);
    userdata->encoder_output_pool = encoder_output_port_pool;
    encoder_output_port->userdata = (struct MMAL_PORT_USERDATA_T *) userdata;

    status = mmal_port_enable(encoder_output_port, encoder_output_buffer_callback);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to enable encoder output port (%u)\n", status);
        return -1;
    }
    //fprintf(stderr, "INFO:Encoder output pool has been created\n");

    fill_port_buffer(encoder_output_port, encoder_output_port_pool);

    //fprintf(stderr, "INFO:Encoder has been created\n");
    return 0;
}

int setup_preview(PORT_USERDATA *userdata) {
    MMAL_STATUS_T status;
    MMAL_COMPONENT_T *preview = 0;
    MMAL_CONNECTION_T *camera_preview_connection = 0;
    MMAL_PORT_T *preview_input_port;

    status = mmal_component_create(MMAL_COMPONENT_DEFAULT_VIDEO_RENDERER, &preview);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to create preview (%u)\n", status);
        return -1;
    }
    userdata->preview = preview;
    preview_input_port = preview->input[0];

    {
        MMAL_DISPLAYREGION_T param;
        param.hdr.id = MMAL_PARAMETER_DISPLAYREGION;
        param.hdr.size = sizeof (MMAL_DISPLAYREGION_T);
        param.set = MMAL_DISPLAY_SET_LAYER;
        param.layer = 0;
        param.set |= MMAL_DISPLAY_SET_FULLSCREEN;
        param.fullscreen = 1;
        status = mmal_port_parameter_set(preview_input_port, &param.hdr);
        if (status != MMAL_SUCCESS && status != MMAL_ENOSYS) {
            //fprintf(stderr, "Error: unable to set preview port parameters (%u)\n", status);
            return -1;
        }
    }


    status = mmal_connection_create(&camera_preview_connection, userdata->camera_preview_port, preview_input_port, MMAL_CONNECTION_FLAG_TUNNELLING | MMAL_CONNECTION_FLAG_ALLOCATION_ON_INPUT);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to create connection (%u)\n", status);
        return -1;
    }

    status = mmal_connection_enable(camera_preview_connection);
    if (status != MMAL_SUCCESS) {
        //fprintf(stderr, "Error: unable to enable connection (%u)\n", status);
        return -1;
    }
    //fprintf(stderr, "INFO: preview created\n");
    return 0;
}

int setup_video_overlay(PORT_USERDATA *userdata) {
    return 0;
}

int main(int argc, char** argv) {
    PORT_USERDATA userdata;

    memset(&userdata, 0, sizeof (PORT_USERDATA));
    
    userdata.width = VIDEO_WIDTH;
    userdata.height = VIDEO_HEIGHT;

    pixel_buffer = (pixel_t*)(malloc(sizeof(pixel_t)*DATALEN));
    
    //fprintf(stderr, "VIDEO_WIDTH : %i\n", userdata.width );
    //fprintf(stderr, "VIDEO_HEIGHT: %i\n", userdata.height );
    //fprintf(stderr, "VIDEO_FPS   : %i\n",  VIDEO_FPS);
    //fprintf(stderr, "Running...\n");

    bcm_host_init();

    if (1 && setup_camera(&userdata) != 0) {
        //fprintf(stderr, "Error: setup camera\n");
        return -1;
    }
    if (1 && setup_encoder(&userdata) != 0) {
        //fprintf(stderr, "Error: setup encoder\n");
        return -1;
    }
    
    if (1 && setup_preview(&userdata) != 0) {
        //fprintf(stderr, "Error: setup preview\n");
        return -1;
    }

    data_attr.mq_msgsize = DATASIZE;
    data_attr.mq_flags = 0;
    data_attr.mq_maxmsg = 10;
    data_attr.mq_curmsgs = 0;
    reader = mq_open(DATA_QUEUE, O_RDONLY | O_CREAT | O_NONBLOCK, PERMS, &data_attr);
    
    attr.mq_msgsize = MSGSIZE;
    attr.mq_flags = 0;
    attr.mq_maxmsg = 10;
    attr.mq_curmsgs = 0;
    writer = mq_open(WORKER_QUEUE, O_CREAT | O_WRONLY | O_NONBLOCK, PERMS, &attr);
    
    if(reader == -1) {
        return -1;
    }
    
    char buf[128];
    fd_set fds, master;
    FD_ZERO(&master);
    FD_SET(0, &master);
    FD_SET(reader, &master);
    
    while (1) {
        memcpy(&fds, &master, sizeof(master));
        select(reader+1, &fds, NULL, NULL, NULL);
        
        if(FD_ISSET(reader, &fds)) {
            int received = mq_receive(reader, (char*)(pixel_buffer), DATASIZE, NULL);
            pixels = received / sizeof(pixel_t);
            //fprintf(stderr, "Received %d bytes of data!\n", received);
        }
        else {
            if(read(0, buf, 128) == 0) {
                return 0;
            }
        }
    }

    return 0;
}
