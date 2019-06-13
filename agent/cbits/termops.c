#include <stdio.h>
#include <termios.h>
#include <string.h>
#include "termops.h"

void set_direct(int fd, struct termios* ptr) {
   struct termios new;
   tcgetattr(0, ptr);
   memcpy(&new, ptr, sizeof(new));
   new.c_iflag &= ~(BRKINT | IGNBRK | PARMRK | ISTRIP | INLCR | IGNCR | IXON);
   new.c_iflag |= (ICRNL);
   new.c_oflag |= OPOST;
   new.c_lflag &= ~(ICANON | ECHO | ECHOE | ECHONL);
   new.c_lflag |= (NOFLSH);
   tcsetattr(fd, TCSANOW, &new);
}

void reset_terminal(int fd, struct termios* ptr) {
   tcsetattr(fd, TCSANOW, ptr);
}
