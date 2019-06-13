#include <unistd.h>
#include <termios.h>

void set_direct(int fd, struct termios* ptr);
void reset_terminal(int fd, struct termios* ptr);
