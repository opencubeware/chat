#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <string.h>
#include <fcntl.h>
#include <stdio.h>

#define FIFO "/tmp/silence.aac"
#define PERMS (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)

// program generates PCM 16bit silence stream
// and saves it to a named pipe
// (882 zeros every 1/100 s = 44100 Hz mono)
int main(int argc, char** argv) {
    int i = 0;
    unsigned char chunk[8820 + 180];
    for(i=0; i<8820 + 180; i++) {
        chunk[i] = 0;
    }
    
    if(mkfifo(FIFO, PERMS) == -1 && errno != EEXIST) {
       perror("Error while creating FIFO");
       return 1;
    } 

    int fifo = open(FIFO, O_WRONLY);
    for(;;) {
        write(fifo, chunk, sizeof(chunk));
        usleep(100000);
    }

    return 0;
}
