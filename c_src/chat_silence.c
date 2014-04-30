#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <stdio.h>

#define FIFO "/tmp/silence.aac"
#define PERMS (S_IRUSR | S_IWUSR)

// program generates PCM 16bit silence stream
// and saves it to a named pipe
// (882 zeros every 1/100 s = 44100 Hz mono)
int main(int argc, char** argv) {
    int i = 0;
    unsigned char chunk[882];
    for(i=0; i<882; i++) {
        chunk[i] = 0;
    }
    
    unlink(FIFO);
    if(mkfifo(FIFO, PERMS)) {
       perror("Error while creating FIFO");
       return 1;
    } 

    int fifo = open(FIFO, O_WRONLY);
    for(;;) {
        write(fifo, chunk, sizeof(chunk));
        usleep(10000);
    }

    return 0;
}
